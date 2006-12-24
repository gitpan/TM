package TM::Tau;

use TM::Tau::Filter;
use base qw(TM::Tau::Filter);

use Data::Dumper;

=pod

=head1 NAME

TM::Tau - Topic Maps, Tau Expressions

=head1 SYNOPSIS

  use TM::Tau;
  # read a map from an XTM file
  $tm = new TM::Tau ('test.xtm');        # or
  $tm = new TM::Tau ('file:test.xtm');   # or
  $tm = new TM::Tau ('file:test.xtm >'); # or
  $tm = new TM::Tau ('file:test.xtm > null:');

  # read it now and write it back to the file when object goes out of scope
  $tm = new TM::Tau ('test.xtm > test.xtm');

  # create empty map at start and then let it automatically flush onto file
  $tm = new TM::Tau ('null: > test.xtm');
  $tm = new TM::Tau ('> test.xtm');

  # read-in at the start (=constructor time) and then flush it back
  $tm = new TM::Tau ('> test.xtm >');

  # load and merge maps at constructor time
  $tm = new TM::Tau ('file:test.xtm + http://..../test.atm');

  # load map and filter it with a constraint at constructor time
  $tm = new TM::Tau ('mymap.atm * myontology.ont');

  # convert between different formats
  $tm = new TM::Tau ('test.xtm > test.atm'); # if there were an output driver for AsTMa=

=head1 DESCRIPTION

This package allows you to implement a I<breath-in-breath-out> paradigm when using topic maps coming
from different provenances. This, for instance, allows you to create a topic map which is read from
an AsTMa= file when it is instantiated at constructor time and then is saved into an XTM file when
the topic map object goes out of scope. When, how, or whether at all the in-memory representation of
the map is synchronized with these external resources, you can control with a simple language,
presented below.

This language also provides two binary operators to combine maps in this process. With C<+> maps can
be merged, with C<*> a filter can be applied to a map.

=head1 TAU EXPRESSIONS

=head2 Introduction

I<tau> expressions serve several purposes:

=over

=item

First, they allow to address whole maps via a URI.

=item

Then they allow to connect (real or virtual) topic maps together forming bigger maps. In

   # merging two map, one in AsTMa format, another in XTM
   file:tm.atm + http://topicmaps/some/map.xtm

we use the C<+> operator to I<merge> two maps into one.

B<NOTE>: Future versions of this package will use the C<+> operator also between maps and ontologies.

=item

Tau expressions are then generalized to filter maps.  Ontologies serve as filters as they are
interpreted as constraints. Only those parts of the original map which conform to the ontology
survive the filter process.

   # filter out according to an ontology
   product_data.atm * file:customer_view.onto

The C<*> operator symbolizes the filtering operation.

=item

Tau expressions are also used to completely transform maps into other maps (ontological
transformation).

   file:music.atm * beatles.atq
   file:music.atm * elvis.atq
 
Here C<*> symbolizes the transformation.

=back

B<NOTE>: Some of this filter functionality may not be yet implemented.

=head2 Map Source URLs

To address maps we use URIs. A map stored in the file system might be addressed as

  file:mydir/somemap.xtm

for a relative URL (relative to an application's current working directory), or via an
absolute URI such as

  http://myserver/somemap.atm

The package supports all those access methods (file:, http:, ...) which L<LWP::Simple> supports.

=head2 Drivers

Obviously a different deserializer package has to be used for an XTM file than for an AsTMa or LTM
file. Some topic map content may be in a TM backend database, some content may only exist virtually,
being emulated by a dedicated package.  While you may be mostly fine with system defaults, in some
cases you may want to have precise control on how files and other external sources are to be
interpreted. By their nature, drivers for sources must be subclasses of L<TM>.

A similar consideration applies to filters. Also here the specified URI determines which filter
actually has to be applied. It also can define where the content eventually is stored to. Drivers
for filters must be either subclasses of L<TM::Tau::Filter>, or alternatively must be a trait
providing a method C<sync_out>.

=head2 Binding by Schemes (implicit)

When the Tau expression is parsed, this package tries to identify which driver to use for which part
of that composite map denoted by the expression. For this purpose a pattern matching approach can be
used to map regular expression patterns to driver package names. If you would like to learn about
the current state of affairs do a

   use Data::Dumper;
   print Dumper \%TM::Tau::sources;
   print Dumper \%TM::Tau::filters;

Each entry contains as key a regular expression which is matched against the parsed URI and as value
the name of the driver to be used. There is a distinction made between the namespace of resources
(residing data) and filters (and transformers).

You can override values there:

   $TM::Tau::sources{'null:'}          = 'TM';
   $TM::Tau::sources{'tm:server\.com'} = 'My::Private::TopicMap::Driver';

At any time you can modify this hash, introduce new patterns, delete existing ones. The only
constraint is that the driver package must already be C<require>d into your Perl program.

During parsing of a Tau expression, two cases are distinguised:

=over

=item 

If the URI specifies a I<source>, then this URI will be matched against the regexps in the
C<TM::Tau::sources> hash. The value of that entry will be used as class name to instantiate an
object whereby one component (C<uri>) will be passed as parameter like this:

I<$this_class_name>->new (uri => I<$this_uri>, baseuri => I<$this_uri>)

This class should be a subclass of L<TM>.

=item

If te URI specifies a I<filter>, then you have two options: Either you use as entry the name of a
subclass of L<TM::Tau::Filter>. Then an object is create like above. Alternatively, the entry is a
list reference containing names of traits. Then a generic L<TM::Tau::Filter> node is generated first
and each of the traits are applied like this:

Class::Trait->apply ( $node => I<$trait> => {
                               exclude => [ 'mtime',
                                            'sync_out',
                                            'source_in' ]
                               } );

=back

If there is no match, this results in an exception.

=cut

our %sources = (
	        '^null:$'          	     => 'TM::Materialized::Null',

		'^(file|ftp|http):.*\.atm$'  => 'TM::Materialized::AsTMa',
		'^(file|ftp|http):.*\.ltm$'  => 'TM::Materialized::LTM',
                '^file:/tmp/.*'              => 'TM::Materialized::AsTMa',
#		'^(file|ftp|http):.*\.xtm$'  => 'TM::Materialized::XTM',
		'^inline:.*'       	     => 'TM::Materialized::AsTMa',

		'^io:stdin$'       	     => 'TM::Materialized::AsTMa',
		'^-$'                        => 'TM::Materialized::AsTMa',                         # in "- > whatever:xxx" the - is the map coming via STDIN
                );

our %filters = (                                                                                   # TM::Tau::Filter::* packages are supposed to register there
		'^null:$'                    => [ 'TM::Serializable::Dumper' ],

		'^(file|ftp|http):.*\.atm$'  => [ 'TM::Serializable::AsTMa' ],
		'^(file|ftp|http):.*\.ltm$'  => [ 'TM::Serializable::LTM' ],

		'^-$'                        => [ 'TM::Serializable::Dumper' ],                    # in "whatever > -" the - is an empty filter
		'^io:stdout$'      	     => [ 'TM::Serializable::Dumper' ],                    # stdout can be a URL for a filter
		);

# make sure all registered packages have been loaded
use TM;
use TM::Tau::Filter;

# to be removed?
#use TM::Materialized::AsTMa;
#use TM::Materialized::LTM;
#use TM::Materialized::Null;
#use TM::Materialized::XTM;

=pod

=head2 Binding by Package Pragmas (Explicit)

Another way to define which package should be used for a particular map
is to specify this directly in the I<tau> expression:

   http://.../map.xtm { My::BrokenXTM }

In this case the resource is loaded and is processed using
C<My::BrokenXTM> as package to parse it (see L<TM::Materialized::Stream> on how to write
such a driver).

=head2 Syntax

The Tau expression language supports two binary operators, C<+> and C<*>. The C<+> operator
intuitively puts things together, the C<*> applies the right-hand operand to the left-hand operand
and behave as a transformer or a filter. The exact semantics depends on the operands. In any case,
the C<*> binds stronger than the C<+>.

The parser understands the following syntax for Tau expression:

   tau_expr    -> mul_expr

   mul_expr    -> source { '*' filter }

   source      -> '(' add_expr ')' | primitive

   add_expr    -> mul_expr { '+' mul_expr }

   filter      -> '(' filter ')' | primitive

   primitive   -> uri [ module_spec ]

   module_spec -> '{' name '}'

Terms in quotes are terminals, terms inside {} can appear any number of times (also zero), terms
inside [] are optional. All other terms are non-terminals.

=cut

#== tau expressions =======================================================

$::RD_HINT = 1;

our $tau_grammar = q{

   {
       my $sources;
       my $filters;
       my $ms;
       use Data::Dumper;

       sub _mk_node {
	   my $uri   = shift;
	   my $spec  = shift;
	   my $first = shift || 0;
	   my $last  = shift || 0;

	   my $node;

	   $uri = ( $first ? 'io:stdin' : 'io:stdout' ) if $uri eq '-';             # decide what - actually should mean

	   if (ref ($spec)) {                                                       # if it is a list, then we have filter with traits
	       $node = new TM::Tau::Filter (url => $uri, baseuri => $uri );         # in any case this will be a filter
	       bless $node, 'TM::Tau' if $last;                                     # but if it is the last in the row, then a TM::Tau

	       foreach my $trait (@{ $spec }) {                                     # the rest of the list are traits
		   eval {
		       Class::Trait->apply ( $node => $trait => { exclude => [ 'mtime', 'sync_out', 'source_in' ] } ); # which we add now
                   }; die "cannot apply trait '$trait' for URI '$uri' ($@)" if $@;
	       }
	   } else {                                                                 # otherwise it is a simple module
	       my $module = $spec;                                                  # take that
	       eval "use $module";                                                  # try to load it on the fly
	       eval {                                                               # try to
		   $node = $module->new (url => $uri, baseuri => $uri );            # instantiate an object
	       }; 
	       die "cannot load '$module' for URI '$uri' ($@)"    if $@;
	       die "cannot instatiate object for '$module' ($@)"  unless $node;
	   }
	   return $node;
       }

       sub _mk_tree {
	   my $spec = shift;
	   my $top  = shift || 0;                                                     # are we at the top?
	   
#warn "mktree: ". Dumper $spec;
	   my $t;                                                                     # here we collect the tree
	   while (my $m = shift @$spec) {                                             # walk through the mul_expr's
	       my $c;                                                                 # find a new chain member
	       if (ref ($m) eq 'ARRAY') {                                             # this means that this operand (can only be the first) is an add_expr
		   my $d1 = _mk_tree (shift @{$m});                                   # take the first and make it a node
		   while (my $d2 = _mk_tree (shift @{$m})) {                          # if there are more things to add
		       use TM::Tau::Federate;
		       $d1 = new TM::Tau::Federate (left       => $d1,                # build a federation
						    right      => $d2,
						    url        => 'what:ever');
		   }
		   $c = $d1;                                                          # tuck it away for the end of the loop

	       } elsif (ref ($m) eq 'HASH') {                                         # this is just a primitive source/filter
		   $c = _mk_node (%$m,                                                # create a source/filter node
				  !defined $t,                                        # this is the first in a chain, so we have no $t yet
				  $top && ! @$spec);                                  # let it also know whether this is the top-top-top, so last, last, last
	       } else {
		   die "now this is bad";
	       }
	       
	       if ($t) {                                                              # we know there was something in the chain already and c is a filter
		   $c->left ($t);
		   $t = $c;
	       } else {
		   $t = $c;
	       }
	   }

	   return $t;
       }
    }

   startrule    : { $sources = $arg[0]; $filters = $arg[1]; }                                            # collect parameters
                  tau_expr

   tau_expr     : mul_expr                                { $return = _mk_tree ($item[1], 1); }         # a tau expr is a filter

   mul_expr     : source ( '*' filter )(s?)               { $return = [ $item[1], @{$item[2]} ]; }

   source       : '(' add_expr ')'                        { $return = $item[2]; }
                | primitive[$sources]

   add_expr     : <leftop: mul_expr '+' mul_expr>

   filter       : '(' filter ')'                          { $return = $item[2]; }                        # we allow arbitrary ()-nesting here, but
                | primitive[$filters]                                                                    # a filter cannot be composite (yet)

   primitive    : <rulevar: $schemes = $arg[0]>

   primitive    : /[^\s()\*\{\}]+/ module(?)
                {
#warn "using schemes ".Dumper ($schemes)." for $item[1]";
		    my $uri = $item[1];
		    if (@{$item[2]} && $item[2]->[0]) {                                                  # its defined and there is a module specification
			$return = { $uri, $item[2]->[0] };                                               # take that
		    } else {                                                                             # no module, so we have to guess via schemes
			$return = undef;
			foreach my $s (keys %$schemes) {                                                 # look at all of them
			    if ($uri =~ /$s/) {                                                          # if it matches
				$return = { $uri, $schemes->{$s} };
				last;                                                                    # if we found something, we stop
			    }
			}
			die "expression parser: undefined scheme '$uri' detected" unless $return;        # loop exhausted and nothing found => bad
		    }
		}

   module       : '{' /[\w:]*/ '}' { $return = $item[2]; }

};

my $parser; # will be compiled once when it is needed and then will be kept, this is faster

sub _parse {
    my $tau    = shift;
    my $ms     = shift;

    use Parse::RecDescent;
    $parser ||= new Parse::RecDescent ($tau_grammar)           or  $main::log->logdie (scalar __PACKAGE__ . ": problem in tau grammar");

    my $f = $parser->startrule (\$tau,  1, \%sources,                  # predefined sources
				           \%filters)                  # add the currently known filters
				           ;
    $main::log->logdie (scalar __PACKAGE__ . ": found unparseable '$tau'") if $tau =~ /\S/s ;
    return $f;
}

=pod

=head2 Examples

  # memory-only map
  null: > null:

  # read at startup, sync out on request of application
  file:test.atm > file:test.atm

  # copy AsTMa= to XTM
  file:test.atm > file:test.xtm
  # this only works if the XTM driver supports outputting

  # using a dedicated driver to load a map, store it onto a file
  dns:my.dns.server { My::DNS::Driver } > file:dns_snapshot.atm
  # this will only work if the My::DNS::Driver supports to materialize a map

  # read a map and compute the statistics
  file:test.atm * http://psi.tm.bond.edu.au/queries/1.0/statistics

=head1 INTERFACE

=head2 Constructor

The constructor accepts a I<tau-expression> parameter defining how the map is supposed to be built.
If that parameter is missing, C<< null: >> will be assumed which results in an empty map to be
created. That map, though, contains a memory component, so that things can be added to it after
that. Otherwise the I<Tau> expression must follow the tau algebra syntax given in L</Syntax>.  If
not, then an appropriate exception will be raised.

If - during the parsing process - no appropriate driver package for a particular resource can be
identified, an exception will be raised.

Examples:

   # map only existing in memory
   my $map = new TM::Tau;

   # map will be loaded as result of this tau expression
   my $map = new TM::Tau ('file:music.atm * file:beatles.tmql');


Apart from the Tau expression the constructor optionally interprets a hash with the following keys:

=over

=item C<sync_in> (default: C<1>)

If non-zero, in-synchronisation at constructor time will happen, otherwise it is suppressed. In that
case you can trigger in-synchronisation explicitly with the method C<sync_in>.

=item C<sync_out> (default: C<1>)

If non-zero, out-synchronisation at destruction time will happen, otherwise it is suppressed.

=back

Example:

   my $map = new TM::Tau ('test.xtm', sync_in => 0);

The (pre)parser supports the following shortcuts (I hate unnecessary typing):

=over

=item

"whatever" is interpreted as "(whatever) > -"

=item

"whatever >" is interpreted as "(whatever) > -"

=item

"> whatever" is interpreted as  "- > (whatever)"

=item

"< whatever >" is interpreted as "whatever > whatever", sync_in => 0

=item

"> whatever <" is interpreted as "whatever > whatever", sync_out => 0

=item

"> whatever >" is interpreted as "whatever > whatever"

=item

"< whatever <" is interpreted as "whatever > whatever", sync_in => 0, sync_out => 0

=item

The URI C<-> as source is interpreted as STDIN (via the L<TM::Serializable::AsTMa> trait).

=item

The URI C<-> as filter is interpreted as STDOUT (via the L<TM::Serializable::Dumper> trait).

=back

=cut

sub new {
    my $class = shift;
    my $tau   = shift || "null:";
    my %opts  = @_;

#warn "cano0 '$tau'";

    # providing defaults
    $opts{sync_in}  = 1 unless defined $opts{sync_in};
    $opts{sync_out} = 1 unless defined $opts{sync_out};

    $_ = $tau;                                                                          # we do a number of things now

    # canonicalization, phase 0: remove leading/trailing blanks
    s/^\s*//;
    s/\s*$//;

    # canonicalization, phase I: reduce the ><><>< crazyness to A > B
    if (/^<(.*)>$/) {
	$_ = "($1) > ($1)";
	$opts{sync_in} = 0; $opts{sync_out} = 1;
    } elsif (/^>(.*)<$/) {
	$_ = "($1) > ($1)";
	$opts{sync_in} = 1; $opts{sync_out} = 0;
    } elsif (/^>(.*)>$/) {
	$_ = "($1) > ($1)";
	$opts{sync_in} =    $opts{sync_out} = 1;
    } elsif (/^<(.*)<$/) {
	$_ = "($1) > ($1)";
	$opts{sync_in} =    $opts{sync_out} = 0;

    } elsif (/^(.*)>$/) {                                                               # > - default
	$_ = "($1) > -";
    } elsif (/^>(.*)$/) {                                                               # - > default
	$_ = "- > $1";
    } elsif (/>/) {                                                                     # there is a > somewhere in between
                                                                                        # we leave it as it is
    } else {                                                                            # no > to be see anywhere
	$_ = "($_) > -";
    }

#warn "cano2 '$_'";

    # canonicalization, phase III, removing >''s
    s/>/\*/g;                                                                           # * operator does now the trick

#warn "---- cano3 '$_'";

    my $self = _parse ($_);                                                             # DIRTY, but then not
#warn "============> ". ref ($self->left) . " <-- left -- " . ref ($self);

    $self->{sync_in}  = $opts{sync_in};                                                 # same here
    $self->{sync_out} = $opts{sync_out};

#warn "sync start? ".time;
    $self->sync_in if $self->{sync_in};                                                 # if user wants to sync at constructor time, lets do it
#warn "sync end ".time;

#     _dmp ($self->{_map}, 1);
#warn "cano3 after ".Dumper $self;
    return $self;
}

=pod

=head1 SEE ALSO

L<TM>, L<TM::Tau::Filter>

=head1 AUTHOR

Copyright 200[0-6], Robert Barta E<lt>drrho@cpan.orgE<gt>, All rights reserved.

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.  http://www.perl.com/perl/misc/Artistic.html


=cut

our $VERSION  = '1.13';
our $REVISION = '$Id: Tau.pm,v 1.13 2006/12/05 09:50:38 rho Exp $';

1;

__END__


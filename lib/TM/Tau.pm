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

Firstly, they allow to connect (real or virtual) topic maps together forming bigger maps. In

   # merging two map, one in AsTMa format, another in XTM
   file:tm.atm + http://topicmaps/some/map.xtm

we use the C<+> operator to I<merge> two maps into one.

B<NOTE>: Later versions of this package will use the C<+> operator also between maps and ontologies.

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

=head2 Map Source URLs

To address maps we use URIs. A map stored in the file system might be addressed as

  file:mydir/somemap.xtm

for a relative URL (relative to an applications current working directory), or

  http://myserver/somemap.atm

The package supports all those access methods (file:, http:, ...) the L<LWP::Simple> package
supports.

=head2 Drivers

Obviously a different deserializer package has to be used for an XTM file than for an AsTMa or LTM
file. Some topic map content may be in a TM backend database, some content may only exist virtually,
being emulated by a dedicated package.  While you may be mostly fine with system defaults, in some
cases you may want to have precise control on how files and other external sources are to be
interpreted.

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

Whenever during parsing of a tau expression a URI is matched against one of the scheme keys, an
instance of the driver will be created whereby one component (C<uri>) will be passed as parameter
like this:

   $this_schemes->new (uri => $this_uri)

At any time you can modify this hash, introduce new patterns, delete
existing ones. The only constraint is that the driver package
must already be C<require>d into your Perl program.

=cut

our %sources = (
		'^(file|ftp|http):.*\.atm$'  => 'TM::Materialized::AsTMa',
		'^(file|ftp|http):.*\.ltm$'  => 'TM::Materialized::LTM',
                '^file:/tmp/.*'              => 'TM::Materialized::AsTMa',
#		'^(file|ftp|http):.*\.xtm$'  => 'TM::Materialized::XTM',
		'^inline:.*'       	     => 'TM::Materialized::AsTMa',
	        '^null:$'          	     => 'TM::Materialized::Null',
#		'^tm:/.*'         	     => 'TM::MapSphere',
		'^io:stdin$'       	     => 'TM::Materialized::AsTMa',
		'^io:stdout$'      	     => 'TM::Serializable::Dumper', # ?????? check
		'^-$'                        => 'TM::Materialized::Null',                          # in "- > whatever:xxx" the - is an empty map
                );

our %filters = (                                                                                   # TM::Tau::Filter::* packages are supposed to register there
		'^null:$'                    => [ 'TM::Serializable::AsTMa' ],
		'^-$'                        => [ 'TM::Serializable::AsTMa' ],                     # in "whatever > -" the - is an empty filter
		'^(file|ftp|http):.*\.atm$'  => [ 'TM::Serializable::AsTMa' ],
		'^(file|ftp|http):.*\.ltm$'  => [ 'TM::Serializable::LTM' ],
		);



# make sure all registered packages have to be loaded
use TM;
use TM::Materialized::AsTMa;
use TM::Materialized::LTM;
use TM::Materialized::Null;
use TM::Tau::Filter;


#use TM::Materialized::XTM;
#use TM::MapSphere;

=pod

=head2 Binding by Package Pragmas (Explicit)

Another way to define which package should be used for a particular map
is to specify this directly in the I<tau> expression:

   http://.../map.xtm { My::BrokenXTM }

In this case the resource is loaded and is processed using
C<My::BrokenXTM> as package to parse it (see L<TM::Materialized::Stream> on how to write
such a driver).

=head2 Operator Semantics

The Tau expression language supports two operators, C<+> and C<*>. The C<+> operator intuitively
puts things together, the C<*> applies the right-hand operand to the left-hand operand and behave as
a transformer or a filter. The exact semantics depends on the operands. In any case, the C<*> binds
stronger than the C<+>.

NOTE: This package does not implement all variations yet. The following operations are implemented:

=over

=item C<+> as merging of maps

=item C<*> as applying a filter to a map

=back

=head2 Syntax

The parser understands the following syntax for Tau expression:

@@@@@@@

   tau_expr    -> add_expr

   add_expr    -> mul_expr { '+' mul_expr }

   mul_expr    -> expr { '*' expr }

   expr        -> '(' add_expr ')' | primitive

   primitive   -> uri [ module_spec ]

   module_spec -> '{' name '}'

=head1 Filters

@@@@@@@@@@@@@@@@@@@@@@@

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
		       Class::Trait->apply ( $node => $trait );                     # which we add now
                   }; die "cannot apply trait '$trait' for URI '$uri' ($@)" if $@;
	       }
	   } else {                                                                 # otherwise it is a simple module
	       my $module = $spec;                                                  # take that
	       eval {                                                               # try to
		   $node = $module->new (url => $uri, baseuri => $uri );            # instantiate an object
	       }; die "cannot instantiate driver '$module' for URI '$uri' ($@)" if $@;
	   }
	   return $node;
       }

       sub _mk_tree {
	   my $spec = shift;
	   my $top  = shift || 0;                                                     # are we at the top?
	   
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

   primitive    : /[^\s()\{\}]+/ module(?)
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

The constructor accepts a I<tau-expression> parameter defining how the
map is supposed to be built.  If that parameter is missing, C<< null:
>> will be assumed which results in an empty map to be created. That
map, though, contains a memory component, so that things can be added
to it after that.

Otherwise the I<tau> expression must follow the tau algebra syntax
given in L</Syntax>.  If not, then an appropriate exception will be
raised.

If - during the parsing process - no appropriate driver package for a
particular resource can be identified, an exception will be raised.
Also an exception will be raised if the tau expression is not
consistent with the capabilities of a driver. You cannot synchronize
content from most virtual drivers, for instance.

Examples:

   # map only existing in memory
   my $map = new TM::Tau;

   # map will be loaded as result of this tau expression
   my $map = new TM::Tau ('file:music.atm * file:beatles.tmql');

After the Tau expression any number of key/value pairs can be added. All
of these are added to the map without any checking; the only 

@@@@@@@@@

read methods for sync in/out

becomes that last filter class!!!

sync_in , sync_out 0, 1, 2 = no, start/end, immediate

Examples:

   my $map = new TM::Tau @@ (.....)

=cut

sub new {
    my $class = shift;
    my $tau   = shift || "null:";
    my %opts  = @_;

#warn "cano0 '$tau'";

    # providing defaults
    $opts{sync_in}  ||= 1;
    $opts{sync_out} ||= 1;

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
our $REVISION = '$Id: Tau.pm,v 1.6 2006/11/13 08:02:33 rho Exp $';

1;

__END__

x=head2 Methods

Methods directly supported by this class are:

x=over

x=item B<tau>

I<$string> = I<$tm>->tau

This simply gives access to the tau expression used. You cannot change this without creating a new
map, i.e. this is a read-only function.

x=cut

sub tau {
  return @_[0]->{tau};
}

x=pod

x=back



x=cut

#sub last_mod

# use vars qw($AUTOLOAD);
# sub AUTOLOAD {
#     my($method) = $AUTOLOAD =~ m/([^:]+)$/;
#     my $self = shift;

# warn "AUTOLOAD forwarding '$method' to map object";
#     no strict 'refs';

#     my $map = $self->{chain} or die "no chain to use as map found";
#     my $res = $map->$method (@_);
#     $self->{last_mod} = $map->{last_mod};
#     return $res;
# }

x=pod

sub _dmp {
    my $m = shift;
    my $i = shift || 1;

    my $bs = ' ' x $i;
    warn $bs.$m;
    warn $bs."map : $m->{map}".  " (in:".$m->{map}->{_in_url}.", url:".$m->{map}->{url}.", out:".$m->{map}->{_out_url}.")";
    _dmp ($m->{operand}, $i + 3) if $m->{operand};
    _dmp ($m->{left},    $i + 3) if $m->{left};
    _dmp ($m->{right},   $i + 3) if $m->{right};
}

# default is that left-most - is null map and so is right-most
our %STDIN  = (module => 'TM', url => 'null:'); # 'TM::Materialized::AsTMa'
our %STDOUT = (module => 'TM', url => 'null:');




# this wonderful piece of art walks through the tau tree and detects the first/last minus: ('-') in a chain
# the first is replaced by whatever the application has done with the above constant, same with the last
# the idea is that in some applications, a tau expression like - > - is supposed to read from STDIN and write to STDOUT
# for others this is the same as null: > null: = null: * null: = null:, meaning "do not do anything"

sub _canonicalize_urls {
    my $self = shift;
    my $nrm  = shift; # number of * seen from the top-level

#warn "_canon url ".ref($self);

    if (ref ($self) =~ /Federate$/) {               # these are the only which have two children
	$self->{left}  = _canonicalize_urls ($self->{left},  $nrm);
	$self->{right} = _canonicalize_urls ($self->{right}, $nrm);
	$self->{_in_url} = 'whatever:'; $self->{url} = undef;

    } elsif ($nrm == 0) {                           # we are at the end of a ... * ... * chain
	$self->{map} = $STDOUT{module}->new (url => $STDOUT{url}) if (ref($self->{map})   eq 'TM::Tau::Minus');

    } elsif (!$self->{operand}) {                   # we are at the begin of a ... * .... chain
	$self->{map} = $STDIN{module}->new  (url => $STDIN{url})  if (ref($self->{map})   eq 'TM::Tau::Minus');

    } else {                                        # somewhere in the middle
	$self->{map}->{url} = 'null:'                             if (ref($self->{map})   eq 'TM::Tau::Minus');
    }
    $self->{operand} = _canonicalize_urls ($self->{operand}, $nrm+1) if $self->{operand};
    return $self;
}



#warn "in parser $item[1] and sync $sync direction ".$direction;
#use Data::Dumper;
#warn "spec ".Dumper $item[3];

		  # aposteriori definition of the events, not overly elegant, but effective, changes also driver info!!!
		  # more checks here!!!
#		  if (@{$item[3]}) {
#		    @events = @{@{$item[3]}->[0]};
#		  } elsif ($direction == TM::Tau::Expression->INCOMING) {
#		    @events = (TM::Abstract::on_tie);
#		  } elsif ($direction == TM::Abstract::outgoing) {
#		    @events = (TM::Abstract::on_untie);
#		  } else {
#		  }
##use Data::Dumper;
##warn "events ".Dumper \@events;

#'on_tie'     { $return = TM::Abstract::on_tie; }
#                | 'on_untie'   { $return = TM::Abstract::on_untie; }
#                | 'on_sync'    { $return = TM::Abstract::on_sync; }
#                | 'on_change'  { $return = TM::Abstract::on_change; }
__END__


sub new {
  my $class   = shift;
  my $tau     = shift || '> null: >';
  my %options = @_;

  my $self = bless { %options }, $class;

  if (ref ($tau) && $tau->isa ('TM::Abstract')) {
      
  }

  use TM::Tau::Expression;
  


  if (ref($tau) && $tau->isa ('TM::Abstract')) {     # we already have something parsed here
      $self->{transit}  = $tau;

  } elsif ($tau =~ /^\s*>(.+)>\s*$/) {  # > something >
      $self->{transit}  = TM::Tau::parse ($1);

  } elsif ($tau =~ /^\s*>(.+)<\s*$/) {  # > something <
      $self->{incoming} = TM::Tau::parse ($1);
      $self->{incoming}->sync_in;
      $self->{transit}  = new TM::Materialized::Memory (store => $self->{incoming}->{store});
      $self->{outgoing} = $self->{incoming};

  } elsif ($tau =~ /^\s*<(.+)>\s*$/) {  # < something >
      $self->{incoming} = TM::Tau::parse ($1);
      $self->{transit}  = new TM::Materialized::Memory;
      $self->{outgoing} = $self->{incoming};

  } elsif ($tau =~ /^(.+)>\s*$/) {      # something >
      $self->{incoming} = TM::Tau::parse ($1);
      $self->{incoming}->sync_in;
      $self->{transit}  = new TM::Materialized::Memory (store => $self->{incoming}->{store});
      $self->{outgoing} = undef;

  } elsif ($tau =~ /^\s*>(.+)$/) {      # > something
      $self->{incoming} = undef;
      $self->{transit}  = new TM::Materialized::Memory;
      $self->{outgoing} = TM::Tau::parse ($1);

  } elsif ($tau =~ /^(.+?)>(.+)$/) {     # like something > something
      $self->{incoming} = TM::Tau::parse ($1);
      $self->{incoming}->sync_in;
      $self->{transit}  = new TM::Materialized::Memory (store => $self->{incoming}->{store});
      $self->{outgoing} = TM::Tau::parse ($2);

  } else {
      $self->{incoming} = TM::Tau::parse ($tau);
      $self->{incoming}->sync_in;
      $self->{transit}  = new TM::Materialized::Memory (store => $self->{incoming}->{store});
      $self->{outgoing} = undef;
  }

  return $self;

}

^-pod

^-head2 Methods

^-head3 Tau Expression and Synchronisation

^-over

^-item B<sync_in>

I<$tm>->sync_in ()

This method will try to copy content from the incoming Tau expression
into this map. The method will fail horribly if there is no incoming
Tau expression, or - if there is - if one of the sub maps in the Tau
expression is not materializable. Obviously.

Any prior content in the map will disappear when it will be
overwritten with the sync'ed in content.

^-cut

sub sync_in {
  my $self        = shift;
  
  $main::log->debug ("sync_in");

  die "nothing to synchronize in" unless $self->{incoming} and $self->{incoming}->can ('sync_in');

  $self->{incoming}->sync_in;                       # do the sync (may fail in case some parts are not materializable
  use TM::Materialized::Memory;
  $self->{transit}  = new TM::Materialized::Memory (store => $self->{incoming}->{store});
}

^-pod

^-item B<sync_out>

^-cut

sub sync_out {
    my $ self = shift;

    die "nothing to synchronize out" unless $self->{outgoing} and $self->{outgoing}->can ('sync_out');

    $self->{outgoing}->{store} = $self->{transit}->{store};
    $self->{outgoing}->sync_out;
}

^-pod

^-back

^-head3 Toplets and Maplets

All the toplet/maplet access methods are implemented as described in L<TM::Retrieve>.

^-cut

sub toplets {
  my $self = shift;
  return $self->{transit}->toplets (@_);
}

sub maplets {
  my $self = shift;
  return $self->{transit}->maplets (@_);
}

^-pod

^-head3 Adding, Removing and Transactions

Please consult L<TM::Update> for this.

sub assert_toplet {
  my $self = shift;
  return $self->{transit}->assert_toplet (@_);
}

sub assert_maplet {
  my $self = shift;
  return $self->{transit}->maplet (@_);
}




1;


__END__



sub add {
    my $self = shift;

    foreach my $o (@_) {
	if ($o->isa ('TM::Maplet')) {
	    $self->{store}->assert_maplet ($o);
	} else {
	    die "cannot add '".ref($o)."' yet";
	}
	$self->{tx_nr_modifies}++ if ($self->{in_transaction});
    }

    unless ($self->{in_transaction}) { # try to sync
      $self->_sync (TM::Driver::on_change, $self->{store});
    }
}

sub remove {
    my $self = shift;

    die "not implemented yet";

    if ($self->{in_transaction}) {
	$self->{tx_nr_modifies}++;
    } else {
      $self->_sync (TM::Driver::on_change, $self->{store});
    }
}

sub _fake_modify { # this is for testing mainly
    my $self = shift;

    if ($self->{in_transaction}) {
	$self->{tx_nr_modifies}++;
    } else {
      $self->_sync (TM::Driver::on_change, $self->{store});
    }
}

use vars qw($AUTOLOAD);
sub AUTOLOAD {
    my($method) = $AUTOLOAD =~ m/([^:]+)$/;
    my $self = shift;

    $log->debug ("AUTOLOAD forwarding '$method' to memory object");

    return if $method eq 'DESTROY';

    if ($method eq 'add' or $method eq 'remove') { # only these modify a map
	if ($self->{in_transaction}) {
	    $self->{tx_nr_modifies}++;
	} else {
	    foreach my $n (grep ($self->{ties}->{$_}->{sync}->{out}, keys %{$self->{ties}})) {
		$self->_sync ($n, TM::Driver::on_change);
	    }
	}
    }

    return;

#    no strict 'refs';
#    *$AUTOLOAD = sub { $self->{memory}->$method(@_) };
#    goto &$AUTOLOAD;
}


IGNORE THE REST !!!!!!!!!!!!!!!!!!!!!!!!


over




__END__

sub _eval {
  my $store = shift;
  my $mode  = shift;
  my $e     = shift;
  
  if (ref ($e) eq 'TM::Tau::AddNode') {
    
  } else {  

    my $l  = $e->left;
    my $r  = $e->right;

    if ($e->operator eq '*') {
	if ($l->{store}->{'maplets*'} && !$r->{store}->{'maplets*'}) { # left has constraints, right not
	    # now do some optimization: validate does not need the results and can terminate earlier
	    if ($mode eq 'filter') {                                   # indicates that there is an expectation for maplets in this map
		die "cannot do filter yet";
	    } else {                                                   # no map? then we only want to validate
		foreach my $c (@{$l->{store}->{'maplets*'}}) {
		    return $store                                      # return without adding any astma-validates maplet
			unless $r->{store}->match ($c, 'validate');
		}
		$store->assert_maplet (new TM::Maplet (scope   => 'pxtm-universal-scope',
							type    => 'astma-sum-ergo-sum',
							roles   => [ 'xtm-psi-topic' ],
							players => [ 'astma-validates' ]));
		return $store;
	    }
	} else {
	    $log->error_die ("cannot handle * between arbitrary maps/constraints yet");
	}
    } elsif ($e->{operator} eq '+') {
	$log->error_die ("cannot handle + yet");
    }
}




    foreach my $t (keys %{$self->{ties}}) {
      # check driver
      my $d = $self->{ties}->{$t};
      $log->error_die ("driver '$t' is invalid") unless $d->isa ('TM::Driver');

      # create schedule
      my $si = $d->sync_info;
      foreach my $e (@TM::Driver::events) {
	push @{$self->{schedule}->{$e}->{in}},  $d if grep ($e == $_, @{$si->{in}});
	push @{$self->{schedule}->{$e}->{out}}, $d if grep ($e == $_, @{$si->{out}});
      }
    }

    use TM::Maplet::Store;
    $self->{store} = new TM::Maplet::Store;
    $self->_sync ( TM::Driver::at_tie, $self->{store} ); # potentially reading/writing from/to resources
                                                         # (writing may not make a lot of sense now, though, but is possible)



__END__


sub _sync_out {
  my $self = shift;
  use UNIVERSAL;
  $self->{expr}->{out}->sync_out if $self->{expr} && 
                                    UNIVERSAL::isa ($self->{expr}->{out}, 'TM::Abstract') &&
				    $self->{expr}->{out}->can ('sync_out');
}




^-head1 WHERE DOES THIS BELONG TO??????

^-head2 Consistency

A consistent map is one which has gone through processing detailled in Annex F of the
XTM specification. By default an XTM object has consistency set to 'standard' which means
that all of the above mentioned processing will occur B<at every modification> of that
with following exception(s):

^-over

^-item external maps will not be followed automatically (implicit topic map merge, F.5.5).
This is to protect applications from unintentionally pulling in HUGE
ontologies from external maps only because of some topicRefs pointing to these
topics. So this is turned off by default.

^-back

Alternatively, the user can control the extent of 'consistency' by
providing a hash reference with the following components:

^-over

^-item I<merge>: The value is a list reference containing some of the following constants:

^-over

^-item C<Topic_Naming_Constraint>: see F.5.3

^-item C<Subject_based_Merging>: see F.5.2

^-item C<Id_based_Merging>: If set, then then two topics with the same id are merged. If not
set, then one topic will substitute the other. This was the old behaviour.

^-item C<Application_based_Merging>: not implemented yet.

^-item C<all>: includes all above, default

^-back

To achieve backward compatibility, you should set

  merge => $TM::backward_consistency

^-item I<duplicate_suppression>: The value is a list reference containing some of the following
constants:

^-over

^-item C<Subject_Indicator>: see F.6.1

^-item C<Topic_Name>: see F.6.2

^-item C<Association>: see F.6.3

^-item C<Role_Player>: see F.6.4

^-item C<all>: includes all above, default

^-back

^-back

^-item I<follow_maps>: The value is a list reference containing some of the following
constants:

^-over

^-item C<explicit>: see F.5.4,

^-item C<implicit>: see F.5.5

^-item C<all>: includes all above, default

^-back

The use of any other constant will raise an exception whenever the map is
modified for the first time (either by reading it from a tied resource or
when programmatically changing it).

The package provides the following constants

^-over

^-item C<default_consistency>: all but implicit follow-up of topic references to other maps

^-item C<max_consistency>: all

^-item C<backward_consistency>: backward compatible behavior

^-back

^-cut

our $default_consistency  = {merge                 => [ qw(Topic_Naming_Constraint Subject_based_Merging Id_based_Merging) ],
                             duplicate_suppression => [ qw(Subject_Indicator Topic_Name Association Role_Player) ],
                             follow_maps           => [ qw(explicit) ]};

our $max_consistency      = {merge                 => [ qw(Topic_Naming_Constraint Subject_based_Merging Id_based_Merging) ],
                             duplicate_suppression => [ qw(Subject_Indicator Topic_Name Association Role_Player) ],
                             follow_maps           => [ qw(explicit implicit) ]};

our $backward_consistency = {merge                 => [ ],
                             duplicate_suppression => [ ],
                             follow_maps           => [ qw(explicit) ]};

^-pod




#  $options{consistency} = $default_consistency unless $options{consistency};
#  foreach my $c (qw(merge duplicate_suppression follow_maps)) {
#    $options{consistency}->{$c} = $default_consistency->{$c} unless $options{consistency}->{$c};
#    $options{consistency}->{$c} = $max_consistency->{$c} if grep /^all$/, @{$options{consistency}->{$c}};
#  }
#  $self->{consistency} = $options{consistency};

  $self->{tau} ||= 'null:';



  $self->{'_incoming'}  = TM::Tau::Expression::parse ($in);
  $self->{'_outcoming'} = TM::Tau::Expression::parse ($out);


  # optimize later: build schedule of drivers which would need updating instead of walking down the tree
  $self->_sync (TM::Abstract::on_tie);

##  warn ref $self->{'_incoming'};
  # try to consolidate the whole tree, may involve merging at + operators and on store nodes
  $self->{'_incoming'}->consolidate ($self->{consistency});

  return $self;




  $self->_sync (TM::Abstract::on_untie) if $self->{_incoming} && $self->{_outgoing}; # do not do it on incomplete information


^-head2 Events

When discussing the exchange of information between the internal
representation of maps (and map expressions) and external resources
(such as Topic Map information in an XTM file) this package
distinguishes between specific events (object creation, destruction).

These events are not asynchronous in the sense of process
communication; instead they signal specific circumstances B<when> map
content has to be synchronized between internal and external
representations.

As such, a I<tau> expression (implicitely or explicitely) defines at
which events a synchronisation from the in-memory content onto the
external resource (C<sync_out>) or in the other direction (C<sync_in>)
occurs and which driver is to be used.

To separate the input from the output modalities, a I<tau> expression
is split into two parts separated with C<< > >>:

   file:test.atm [ on_tie ] > http://..../test.xtm [ on_untie ]

The left-hand side specifies the modalities for the synchronisation
(resource to memory), where the right-hand side does this for the
other direction.

The implementation supports the events provided in L<TM::Abstract>.
Every primitive (atomar submap) can specify a subset of these events
as events when a synchronisation should occur. Input and output synchronisation
can be specified separately.

It is errorenous to request synchronisations from a driver which it
cannot perform. Some drivers, for instance, will not be able to write
to the resource or may refuse to do so as it is infeasible.

sub _canonicalize_minus {
    my $self = shift;
    my $nrm  = shift; # number of minusses seen from the top-leve

#warn "_canon minus ".ref($self);
    if (ref ($self) =~ /Merge$/) {           # these are the only which have two children
	$self->{left}  = _canonicalize_minus ($self->{left},  $nrm);
	$self->{right} = _canonicalize_minus ($self->{right}, $nrm);
	return $self;
    } elsif ($self->{url} eq 'minus:') {     # must be Memory
	if ($self->{operand} && $nrm == 0) { # this was the first we have seen (= the last in the chain)
	    $self->{url} = $last_minus_in_chain;
	    $self->{operand} = _canonicalize_minus ($self->{operand}, $nrm+1);
	    return $self;
	} elsif ($self->{operand}) {         # not the first, but also not the last
	    $self->{url} = 'null:';
	    $self->{operand} = _canonicalize_minus ($self->{operand}, $nrm+1);
	    return $self;
	} else {                             # not the first, no operand => must be last
	    $self->{url} = 'null:';          # this make sure no harm is done when DESTROY kicks in
	    
	    return $first_minus_in_chain eq 'io:stdin' ? new TM::Materialized::AsTMa (url => $first_minus_in_chain) : new TM::Materialized::Memory (url => $first_minus_in_chain);
	}
    } elsif ($self->{operand}) {
	$self->{operand} = _canonicalize_minus ($self->{operand}, $nrm+1);
	return $self;
    } else {
	return $self;
    }
}

 =head2 Map Materialization

We distinguish between the following situations:

 =over

 =item B<Materialized Maps>:

Such maps exists as maps@@@@@. Maybe they are written in a notation like
AsTMa= or XTM and reside in a text file. They might also be a map stored
in some backend (which may or may not a XML database or a relational
database).

 =item B<Materializable Maps>:

In many case you will want to look at non-TM content through the eyes
of the TM paradigm. Data in a relational database can most certainly
be converted in a TM, simply by interpreting tables and columns and by
creating topics and associations from this. These maps are called
I<materializable>, because a conversion into a native map may happen.

It is to be considered, though, whether this is wise, in terms of
data consistency (where is the data updated?) and the conversion speed.

 =item B<Virtual (Non-Materializable) Maps>:

You can also manage so-called I<virtual maps>, i.e. those where a Perl
package abstracts away a given data source in such a way, so that it
only appears as map (see L<TM::Virtual>) while still being stored in
some other data source.

 =back


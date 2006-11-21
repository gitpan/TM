package TM::LTM::Parser;

our $ltm_grammar = q {
                      {
			  my $store;
			  my $log;
			  my $implicits;
			  use Data::Dumper;
			  use TM;
			  use TM::Literal;

			  my %prefixes;
		      }

                      # comment is handled outside

		      startrule : { $store = $arg[0]; $log = $arg[1]; $implicits = $arg[2] }  # set TM store and log once
                                  topic_map

		      topic_map : encoding(?) directive(s?) component(s)

		      component: topic | assoc | occur

		      encoding : '@' string  # not analyzed here, but capture in the calling program
		                             # no good here if we would have to translate the encoding

		      directive : version_directive    |
                                  topicmapid_directive |
		                  mergemap_directive   |
		                  prefix_directive     |
				  baseuri_directive
                      # INCLUDE is handled outside

                      prefix_directive : '#PREFIX' /\w+/ '@' uri
		      {
			  my $uri = $item[4];
#			  $uri =~ s/^\"//; $uri =~ s/\"$//;
			  $prefixes{$item[2]} = $uri;
#warn "prefixes ".Dumper \%prefixes;
		      }

		      version_directive : '#VERSION' string
		      {
			  my $version = $item[2];
			  $log->logdie (__PACKAGE__ . ": VERSION not supported '$version'") unless $version =~ /^1\.[23]$/;
		      }

		      topicmapid_directive : '#TOPICMAP' ( name | reify )
		      {
			  $log->logdie (__PACKAGE__ . ": TOPICMAP directive ignored (use proper means)");
		      }

		      mergemap_directive : '#MERGEMAP' uri tm_format(?)
		      {
			  my $uri = $item[2];
#warn "uri is $uri";
			  my $format = $item[3]->[0] ? $item[3]->[0] : 'ltm';
			  my $tm;
			  if ($format =~ /^ltm$/i) {
			      $tm = new TM::Materialized::LTM (url => $uri);
			  } elsif ($format =~ /^xtm$/i) {
			      $tm = new TM::Materialized::XTM (url => $uri);
			  } elsif ($format =~ /^astma$/i) {
			      $tm = new TM::Materialized::AsTMa (url => $uri);
			  } else {
			      $log->logdie (__PACKAGE__ . ": unsupported TM format '$format'");
			  }
			  $tm->sync_in;
			  $store->add ($tm);
#warn "after merged in".Dumper $store;
			  $return = $uri;
		      }

		      tm_format : string

		      baseuri_directive : '#BASEURI' uri

		      topic : '[' name types(?) topname(?) reify(?) subject(?) indicator(s?) ']'
		      {
#warn "topic ".Dumper \@item;
			  my $id = $store->internalize ($item[2] => $item[6]->[0]); # maybe there is a subject addr, maybe not

			  # add the subject indicators
			  map { $store->internalize ($id => $_ ) } @{$item[7]};


			  if ($item[3] and $item[3]->[0]) {
			      $store->assert ( map {
                                                       [ undef, 
							 undef, 
							 'isa', 
							 undef,
							 [ 'class', 'instance' ], 
							 [ $_,       $id ],
							 ] }  
						         @{$item[3]->[0]} );
			      map { $implicits->{'isa-thing'}->{$_}++ } @{$item[3]->[0]};   # the types and the ID are declared implicitely
			  }
#warn "item 4".Dumper $item[4];
			  if ($item[4] and @{$item[4]}) {
			      my $topnames = $item[4]->[0];
#warn "topnames ".Dumper $topnames;
			      my ($a) = $store->assert ( map {[ undef,                                            # LID
								   $topnames->{scope}->[0],                       # SCOPE
								   'name',                                        # TYPE
								   TM->NAME,                                      # KIND
								   [ 'thing', 'value' ],                          # ROLES
								   [ $id,     $_ ],                               # PLAYERS
								   undef ] }
							    @{$topnames->{names}}[0] );       # use the first for a name
			      $return = $a;
# TODO (2..3) for the variants

#warn "basename reify ".Dumper $item[5];
			      # reification of the basename
			      $store->internalize ($item[5]->[0], $a->[TM->LID]) if $item[5]->[0];

			      {
				  map { $implicits->{'isa-scope'}->{ $_ }++ } @{$topnames->{scope}};
			      }
			  }

			  $return = $id;
		      }

		      types : ':' name(s)      { $return = $item[2]; }

		      subject : '%' uri        { $return = $item[2]; }     # for subject addrs the encoding is 'no-reference'

		      indicator : '@' uri      { $return = \ $item[2]; }   # for indicators it is 'send as string reference'

		      topname : '=' basesortdispname scope(?)
		      {
#warn "basenames".Dumper \@item;
			  $return = {
			      scope    => $item[3],
			      names    => $item[2],
			  };
		      }

		      basesortdispname: <leftop: basename ';' basename>

		      basename : string { $return = new TM::Literal ($item[1], 'xsd:string'); }

		      scope : '/' name { $return = $item[2]; }

		      assoc : name '(' assocroles  ')' scope(?) reify(?)
		      {
#warn "assoc item " . Dumper \@item;
			  { # memorize that association type subclasses association
			      $implicits->{'isa-scope'}->{ $item[5]->[0] }++ if $item[5]->[0];
			  }
			  my ($a) = $store->assert ([ undef,                                            # LID
							 $item[5] && $item[5]->[0],                     # SCOPE
							 $item[1],                                      # TYPE
							 TM->ASSOC,                                     # KIND
							 [ map { $_->[1] } @{$item[3]} ],               # ROLES
							 [ map { $_->[0] } @{$item[3]} ],               # PLAYERS
							 undef ]);
			  $return = $a;
#warn "reify ".Dumper $item[6];
			  $store->internalize ($item[6]->[0], $a->[TM->LID]) if $item[6]->[0];
		      }

                      assocroles : assocrole(s /,/)

		      assocrole : ( topic | name ) role(?)
		      {
                          $return = [ $item[1], $item[2]->[0] || 'thing' ];
		      }

		      role : ':' name

		      occur : '{' occ_topic ',' occ_type ',' resource '}' scope(?) reify(?)
		      {
			  my $id = $store->internalize ($item[2]);
			  my ($a) = $store->assert ([ undef,                                         # LID
						      $item[8]->[0],                                 # SCOPE
						      $item[4],                                      # TYPE (MUST BE DEFINED!)
						      TM->OCC,                                       # KIND
						      [ 'thing', 'value' ],                          # ROLES
						      [ $id,     $item[6] ],                         # PLAYERS
						      undef ]);

			 { # memorize basename types and scopes as implicitely defined
			     $implicits->{'isa-scope'}-> { $item[8]->[0] }++ if $item[8]->[0];       # get the bloody scopes and tuck them away
			     $implicits->{'subclasses'}->{ 'occurrence' }->{ $item[4] }++;
			 }

#warn "reify ".Dumper $item[9];
			  $store->internalize ($item[9]->[0], $a->[TM->LID]) if $item[9]->[0];

			  $return = $a;
		      }

		      occ_topic: name

		      occ_type : name

                      reify    : '~' name

		      resource : uri  { $return = new TM::Literal ($item[1], 'xsd:uri') }
                                 |
                                 DATA { $return = new TM::Literal ($item[1], 'xsd:string') }

		      DATA     : '[[' /.*(?=\]\])/sx ']]' { $return = $item[2]; }

		      uri      : string

		      comment  : '/*' /.+?/s '*/'

		      string   : '"' /[^\"]*/ '"'       { $return = $item[2]; }

		      name     : /^\w[:\-\w]*/
		      {
			  my $name = $item[1];
			  if ($name =~ /^(\w+):/) {
			      my $prefix = $1;
			      if ($prefixes{$prefix}) {
				  $name =~ s/^$prefix:/$prefixes{$prefix}/;
				  $return = $name;
			      } else {
				  $return = undef;
			      }
			  } else {
			      $return = $name;
			  }
		      }
		      <reject: ! $return>
		            | /^\w[-\w]*/
		      {
			  $return = $item[1];
		      }

};

sub new {
  my $class = shift;
  my %options = @_;
  my $self = bless \%options, $class;

  $::RD_HINT = 1;
  eval {
    require TM::LTM::CParser;
    $self->{parser} = TM::LTM::CParser->new();
  }; if ($@) {
    warn "could not find CParser ($@)";
    use Parse::RecDescent;
    $self->{parser} = new Parse::RecDescent ($ltm_grammar) or $main::log->logdie (scalar __PACKAGE__ .": problem in grammar ($@)");
  };
  return $self;
}

sub parse {
    my $self = shift;
    my $text = shift;
    
    # we not only capture what is said EXPLICITELY in the map, we also collect implicit knowledge
    # we could add this immediately into the map at parsing, but it would slow the process down and
    # it would probably duplicate/complicate things
    my $implicits = {
	'isa-thing'  => undef,                                          # just let them spring into existence
	'isa-scope'  => undef,                                          # just let them spring into existence
	'subclasses' => undef
	};

    while ($text =~ /\#INCLUDE\s+\"(.+)\"/s) {          # find first
	my $src = $1;
	my $include; # we are trying to figure that one out
	if ($src =~ /^inline:(.*)/s) {
	    $include = $1;
	} else { # we try our luck with LWP
	    use LWP::Simple;
	    $include = get($1) || die "unable to load '$1'\n";
	}
#	use TM::Utils;
#	my $include = TM::Utils::get_content ($1);
	$text =~ s/\#INCLUDE\s+\"(.+)\"/\n$include\n/s; # replace first to find
    }

    # encoding
    # NOTE: currently ignored

    # remove comment
    # NOTE: LTM comments are extremely complex as they may appear anywhere
    # I ignored this and get rid of them on a syntactic level, even risking to throw away /* */ within string. So what.
    $text =~ s|/\*.*?\*/||sg; # global multiline


    $self->{parser}->startrule (\$text, 1, $self->{store}, $main::log, $implicits);
    $main::log->logdie ( scalar __PACKAGE__ . ": Found unparseable '".substr($text,0,40)."....'" ) unless $text =~ /^\s*$/s;

    { # resolving implicit stuff
	my $store     = $self->{store};

	{ # all super/subclasses
	    foreach my $superclass (keys %{$implicits->{'subclasses'}}) {
		$store->assert ( map {
		    [ undef, undef, 'is-subclass-of', TM->ASSOC, [ 'superclass', 'subclass' ], [ $superclass, $_ ] ] 
		    }  keys %{$implicits->{'subclasses'}->{$superclass}});
	    }
#warn "done with subclasses";
	}
	{ # all things in isa-things are THINGS, simply add them
##warn "isa things ".Dumper [keys %{$implicits->{'isa-thing'}}];
	    $store->internalize (map { $_ => undef } keys %{$implicits->{'isa-thing'}});
	}
	{ # establishing the scoping topics
	    $store->assert (map {
                                 [ undef, undef, 'isa', TM->ASSOC, [ 'class', 'instance' ], [ 'scope', $_ ] ] 
				 } keys %{$implicits->{'isa-scope'}});
	}
    }
}


=pod

=head1 SEE ALSO

L<TM>

=head1 AUTHOR INFORMATION

Copyright 200[1-6], Robert Barta <rho@bigpond.net.au>, All rights reserved.

This library is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.
http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION  = '0.4';
our $REVISION = '$Id: Parser.pm,v 1.7 2006/11/13 08:02:33 rho Exp $';

1;

__END__

use strict;
use utf8;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);

require Exporter;
require AutoLoader;

@ISA = qw(Exporter AutoLoader);
@EXPORT = qw( );
$VERSION = '0.03';

use Data::Dumper;
use Parse::RecDescent;
use URI;

use XTM;
use XTM::topic;
use XTM::association;


our $ltm_grammar = q {

		      startrule : topicmap[tm => $arg[0]]

		      topicmap : encoding(?) directive(s?) component(s)
		      {
			my $tm = $arg{tm};
			my @mentioned;

			foreach my $d (@{$item{directive}}) { # walk over directives, some contain components
			  push @{$item{component}}, $d->{components} if ($d->{components});
			}

			foreach my $cs (@{$item{component}}) {
			  foreach my $c (@{$cs}) {
			    if (ref($c) eq 'XTM::topic') {
			      $tm->add ($c);
			      push @mentioned, @{$c->connected};
			    } elsif (ref ($c) eq 'XTM::association') {
			      $tm->add ($c);
			      push @mentioned, @{$c->connected};
			    } elsif (ref($c) eq 'HASH') {     # is an occurrence with a topic
			      my $t;
			      eval {                          # try to find this topic
				$t = $tm->topic ($c->{topic});
			      }; if ($@) {                    # we have an occurrence but no topic
				$t = new XTM::topic (id => $c->{topic});
				$tm->add_topic ($t);
			      };
			      $t->add__s ($c->{occurrence});
			      push @mentioned, @{$t->connected};
			    } else {
			      die "XTM::LTM::Parser: Unexpected component '".ref($c)."'";
			    }
			  }
			}
			foreach my $href (@mentioned) {
#			  warn "testing:".$href;
			  use URI;
			  my $u = new URI ($href);
			  next if $u->scheme; # external
			  my $tid = $u->fragment;
			  next if $tm->is_topic ($tid);
			  $tm->add_topic (new XTM::topic (id => $tid));
			}

		      }

		      component: topic | assoc | occur

		      encoding : '@' string  # not used here, but capture in the calling program
		                             # no good here if we would have to translate the encoding

		      directive : topicmapid_directive |
		                  mergemap_directive   |
				  baseuri_directive

		      topicmapid_directive : '#TOPICMAP' name
		      {
			warn "XTM::LTM::Parser: TOPICMAP directive ignored";
		      }

		      mergemap_directive : '#MERGEMAP' tau_expr tm_format(?)
		      {
			warn "MERGEMAP: $item{tau_expr}, format $item{tm_format}";
#			use Data::Dumper;
#			print Dumper $item{tm_format};

			my $tm2; # will hold the new map
			if (scalar @{$item{tm_format}}) {
			  my $format = $item{tm_format}->[0];
			  use URI;
			  my $uri    = new URI ($item{tau_expr});
			  $uri->scheme ('file') unless $uri->scheme; # default is 'file:'

			  if ($format =~ /ltm/i) {
			    $tm2 = new XTM (tie => new XTM::LTM   (url => $uri->as_string));
			  } elsif ($format =~ /astma/i) {
			    $tm2 = new XTM (tie => new XTM::AsTMa (url => $uri->as_string));
			  } elsif ($format =~ /xtm/i) {
			    $tm2 = new XTM (tie => new XTM::XML   (url => $uri->as_string));
			  } else {
			    die "XTM::LTM::Parser: Format '$format' not implemented";
			  }
			} else {
			  $tm2 = new XTM (tie => new XTM::Virtual (expr => $item{tau_expr}));
			}
			my @components;
			push @components, map {$tm2->topic($_)}       @{$tm2->topics};
			push @components, map {$tm2->association($_)} @{$tm2->associations};
			$return = { components => \@components };
		      }

		      tau_expr  : string

		      tm_format : string

		      baseuri_directive : '#BASEURI' uri


		      topic : '[' name types(?) topname(s?) subject(?) indicator(s?) ']'
		      {
		       my @components; # here I collect all which I generate here

		       my $t = new XTM::topic (id => $item{name});

		       if (ref($item{types})) {
			 foreach (@{$item{types}->[0]}) {
			   $t->add__s (new XTM::instanceOf ( reference => new XTM::topicRef (href => "#$_")));
			 }
		       };
		       $t->add__s (new XTM::instanceOf ( reference => new XTM::topicRef (href => $XTM::PSI::xtm{topic})))
		          unless $t->instanceOfs && @{$t->instanceOfs};

		       if (ref($item{topname})) {
			 foreach my $bn (@{$item{topname}}) {
			   my $b = new XTM::baseName ();
			   $b->add_baseNameString (new XTM::baseNameString (string => $bn->{basename}));
			   $b->add_scope          (new XTM::scope());
			   if (ref ($bn->{scope}) && @{$bn->{scope}}) { # list of scopes defined
			     foreach my $s (@{$bn->{scope}}) {
			       $b->scope->add_reference_s (new XTM::topicRef (href => $s));
			     }
			   } else { # default
			     $b->scope->add_reference_s (new XTM::topicRef (href => $XTM::PSI::xtm{universal_scope}) );
			   }
			   $t->add__s ($b);
			 }
		       }


		       #use Data::Dumper;
		       #print Dumper $item{subject};

		       my $s = new XTM::subjectIdentity (); # maybe we need it
		       if (ref ($item{subject}) && @{$item{subject}}) {
			 $s->add_ ( $item{subject}->[0]);
		       }

		       if (ref($item{indicator})) {
			 foreach my $sin (@{$item{indicator}}) {
			   $s->add_reference_s ($sin);
			 }
		       }
		       $t->add_subjectIdentity ($s) if $s->references || $s->resourceRef; # only add it if we found at least one reference

#		       use Data::Dumper;
#		       print Dumper $t;

		       push @components, $t;
		       $return = \@components;
		      }

		      types : ':' name(s)      { $return = $item[2]; }

		      subject : '%' uri
		      {
			$return = new XTM::resourceRef (href => $item{uri});
		      }

		      indicator : '@' uri
		      {
			use URI;
			my $u = URI->new ($item{uri});
			$return = ref ($u) eq 'URI::_generic' ? 
			  new XTM::topicRef (href => $item{uri}) :
			    new XTM::subjectIndicatorRef (href => $item{uri});
		      }

		      topname : '=' basename scope(?)
		      {
			$return = { basename => $item[2],
				    scope    => $item[3] };
		      }

		      basename : string

		      sortname : string

		      dispname : string

		      scope : '/' name(s)

		      assoc : name '(' assocroles  ')' scope(?)
		      {
			my @components; # here I collect all which I generate here
			my $a = new XTM::association;

#			  use Data::Dumper;
#			  warn "scope : ".Dumper $item{scope};

			my $s = new XTM::scope;
			foreach my $scope (@{$item{scope}} ? @{$item{scope}->[0]}: ()) {
			  $s->add_reference_s (new XTM::topicRef (href => "#$scope"));
			}
			unless ($s->references) {
			  $s->add_reference_s (new XTM::topicRef (href =>  $XTM::PSI::xtm{universal_scope}));
			}
			$a->add ($s);

                        $a->add_instanceOf (new XTM::instanceOf (reference => new XTM::topicRef (href => "#$item{name}")));

#			warn "assocrole is : ".Dumper $item{assocroles};
 			foreach (@{$item{assocroles}}) {
			  if (ref ($_) eq 'XTM::member') {
			    $a->add__s ($_);
			  } elsif (ref ($_) eq 'HASH') {
			    $a->add__s ($_->{member});
			    push @components, $_->{topic};
			  }
		        }


			push @components, $a;
			$return = \@components;
		      }

                      assocroles : assocrole(s /,/)

		      assocrole : ( topic | name ) type(?)
		      {
			my $m = new XTM::member ();

#warn "type is ". Dumper $item{type};
			if (scalar @{$item{type}}) {
			  my $t = new XTM::topicRef (href => "#$item{type}->[0]");
			  my $r = new XTM::roleSpec ();
			  $r->add_reference ($t);
			  $m->add_roleSpec ($r);
			}

#warn "item1 is ".$item[1];
			if (ref ($item[1]) eq 'ARRAY') { # @components came back
			   # the deal is that there is ONLY one topic in this list
			   my $t = $item[1]->[0]; # we have the topic
			   $m->add_reference_s (new XTM::topicRef (href => '#'.$t->id));
			   $return = { member => $m, topic => $t };
			} elsif (!ref ($item[1])) {      # scalar => string
			    $m->add_reference_s (new XTM::topicRef (href => "#$item[1]"));
  			    $return = $m;
			} else {
			  die "XTM::LTM::Parser: internal consistency violation";
			}
		      }

		      type : ':' name

		      occur : '{' occtopic ',' occtype ',' resource '}' scope(?)
		      {
			my $o = new XTM::occurrence ();
			$o->add_resource ($item{resource});
			$o->add_scope    (new XTM::scope());
			foreach (@{$item{scope}->[0]}) {
			  $o->scope->add_reference_s (new XTM::topicRef (href => "#$_"));
			}
			$o->scope->add_reference_s (new XTM::topicRef (href => $XTM::PSI::xtm{universal_scope}) ) 
			  unless $o->scope->references;
			$o->add_instanceOf (new XTM::instanceOf ( reference => new XTM::topicRef (href => "#$item{occtype}")));

			$return = [ { topic      => $item{occtopic},
				      occurrence => $o, } ];

			#use Data::Dumper;
			#print "in occur: ", Dumper $return;
		      }

		      occtopic : name
		      occtype  : name

		      resource : uri
		      {
			$return = new XTM::resourceRef (href => $item{uri});
		      }
		               | data
		      {
			$return = new XTM::resourceData (data => $item{data});
		      }

		      data : '[[' /.*(?=\]\])/sx ']]' { $return = $item[2]; }

		      uri : string

		      comment : '/*' /.+/ '*/'

		      string : '"' /[^\"]*/ '"' { $return = $item[2]; }

		      name : /^\w[-\w]*/

		      };
	
sub handle_begin {
}

sub handle_end {
}

sub handle_comment {
  my $self = shift;
  my $comment = shift;
}

sub handle_encoding {
  my $self = shift;
  my $encoding = shift;
}

sub handle_naming {
  my $self = shift;
  my $name = shift;
}

sub handle_component {
  my $self = shift;
  my $c = shift;

}

sub handle_trailer_start {
  my $self = shift;
}

sub handle_trailer_end {
  my $self = shift;
}

sub handle_undefined_topic {
  my $self = shift;
  my $tid  = shift;
}

sub handle_ltm {
  my $self    = shift;
  my %options = @_;
  
  my $text          = ($options{text} || '') . "\n"; # MUST have a \n at the end
  my $log_level     = $options{log_level} || 0;
  my $auto_complete = $options{auto_complete};

  $options{err}  ||= sub { print STDERR @_; };
  
  
  $self->handle_begin();

  if ($text =~ s/\s*@"(.+?)"//s) {
    my $encoding = $1;
    use Text::Iconv;
    my $converter;
    eval {
      $converter = Text::Iconv->new($encoding, "utf8"); # into Perl utf8 encoding
    }; if ($@) {
      die "XTM::LTM::Parser: Could not convert encoding '$encoding' into utf-8 ($@)";
    };
    $text = $converter->convert($text);
  }

  $text =~ s#/\*.*?\*/##gs; # throw away comments


  $self->{tm} = new XTM::Memory ();
  $parser->startrule (\$text, 1, $self->{tm});
  die "XTM:LTM: Found unparseable '".substr($text,0,40)."....'"    unless $text =~ /^\s*$/s;

  $self->handle_end();
}

1;

__END__

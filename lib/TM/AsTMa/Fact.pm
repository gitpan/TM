####################################################################
#
#    This file was generated using Parse::Yapp version 1.05.
#
#        Don't edit this file, use source file instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
####################################################################
package TM::AsTMa::Fact;
use vars qw ( @ISA );
use strict;

@ISA= qw ( Parse::Yapp::Driver );
use Parse::Yapp::Driver;

#line 1 "yapp/astma-fact.yp"

use Data::Dumper;
use TM;
use TM::Literal;

use constant LEFT  => 'http://psi.tm.bond.edu.au/astma/1.0/#psi-left';
use constant RIGHT => 'http://psi.tm.bond.edu.au/astma/1.0/#psi-right';



sub new {
    my $class   = shift;
    my %options = @_;
    my $store   = delete $options{store} || new TM;       # the Yapp parser is picky and interprets this :-/

    ref($class) and $class=ref($class);

    my $self = $class->SUPER::new( 
##				   yydebug   => 0x01,
				   yyversion => '1.05',
				   yystates  =>
[
	{#State 0
		DEFAULT => -1,
		GOTOS => {
			'maplet_definitions' => 1
		}
	},
	{#State 1
		ACTIONS => {
			'' => 3,
			'ID' => 2,
			'LPAREN' => 4,
			'LBRACKET' => 5,
			'COMMENT' => 6,
			'CANCEL' => 8,
			'ENCODING' => 11,
			'LOG' => 13,
			'EOL' => 14
		},
		GOTOS => {
			'association_definition' => 12,
			'maplet_definition' => 10,
			'topic_definition' => 7,
			'template_definition' => 9
		}
	},
	{#State 2
		DEFAULT => -17,
		GOTOS => {
			'types' => 15
		}
	},
	{#State 3
		DEFAULT => 0
	},
	{#State 4
		ACTIONS => {
			'ID' => 16
		}
	},
	{#State 5
		DEFAULT => -40,
		GOTOS => {
			'@4-1' => 17
		}
	},
	{#State 6
		ACTIONS => {
			'EOL' => 18
		}
	},
	{#State 7
		DEFAULT => -8
	},
	{#State 8
		ACTIONS => {
			'EOL' => 19
		}
	},
	{#State 9
		ACTIONS => {
			'EOL' => 20
		}
	},
	{#State 10
		DEFAULT => -2
	},
	{#State 11
		ACTIONS => {
			'EOL' => 21
		}
	},
	{#State 12
		DEFAULT => -9
	},
	{#State 13
		ACTIONS => {
			'EOL' => 22
		}
	},
	{#State 14
		DEFAULT => -10
	},
	{#State 15
		ACTIONS => {
			'ISREIFIED' => 24,
			'ISINDICATEDBY' => 25,
			'ISA' => 26,
			'LPAREN' => 23,
			'REIFIES' => 28
		},
		DEFAULT => -13,
		GOTOS => {
			'type' => 27,
			'reification_indication' => 29
		}
	},
	{#State 16
		ACTIONS => {
			'RPAREN' => 30
		}
	},
	{#State 17
		ACTIONS => {
			'LPAREN' => 4
		},
		GOTOS => {
			'association_definition' => 31
		}
	},
	{#State 18
		DEFAULT => -4
	},
	{#State 19
		DEFAULT => -6
	},
	{#State 20
		DEFAULT => -3
	},
	{#State 21
		DEFAULT => -7
	},
	{#State 22
		DEFAULT => -5
	},
	{#State 23
		DEFAULT => -49,
		GOTOS => {
			'ids' => 32
		}
	},
	{#State 24
		ACTIONS => {
			'ID' => 33
		}
	},
	{#State 25
		ACTIONS => {
			'ID' => 34
		}
	},
	{#State 26
		ACTIONS => {
			'ID' => 35
		}
	},
	{#State 27
		DEFAULT => -18
	},
	{#State 28
		ACTIONS => {
			'ID' => 36
		}
	},
	{#State 29
		DEFAULT => -37,
		GOTOS => {
			'inline_assocs' => 37
		}
	},
	{#State 30
		ACTIONS => {
			'AT' => 38
		},
		DEFAULT => -35,
		GOTOS => {
			'scope' => 39
		}
	},
	{#State 31
		DEFAULT => -41
	},
	{#State 32
		ACTIONS => {
			'ID' => 40,
			'RPAREN' => 41
		}
	},
	{#State 33
		DEFAULT => -15
	},
	{#State 34
		DEFAULT => -16
	},
	{#State 35
		DEFAULT => -19
	},
	{#State 36
		DEFAULT => -14
	},
	{#State 37
		ACTIONS => {
			'ID' => 42,
			'EOL' => 44
		},
		GOTOS => {
			'inline_assoc' => 43
		}
	},
	{#State 38
		ACTIONS => {
			'ID' => 45
		}
	},
	{#State 39
		ACTIONS => {
			'ISREIFIED' => 24,
			'ISINDICATEDBY' => 25,
			'REIFIES' => 28
		},
		DEFAULT => -13,
		GOTOS => {
			'reification_indication' => 46
		}
	},
	{#State 40
		DEFAULT => -50
	},
	{#State 41
		DEFAULT => -20
	},
	{#State 42
		ACTIONS => {
			'ID' => 47
		}
	},
	{#State 43
		DEFAULT => -38
	},
	{#State 44
		DEFAULT => -11,
		GOTOS => {
			'@1-5' => 48
		}
	},
	{#State 45
		DEFAULT => -36
	},
	{#State 46
		ACTIONS => {
			'EOL' => 49
		}
	},
	{#State 47
		DEFAULT => -39
	},
	{#State 48
		DEFAULT => -21,
		GOTOS => {
			'characteristics_indication' => 50
		}
	},
	{#State 49
		ACTIONS => {
			'ID' => 51
		},
		GOTOS => {
			'member' => 52,
			'association_members' => 53
		}
	},
	{#State 50
		ACTIONS => {
			'OC' => 55,
			'IN' => 56,
			'BN' => 57,
			'SIN' => 58
		},
		DEFAULT => -12,
		GOTOS => {
			'characteristic_indication' => 54,
			'indication' => 59,
			'class' => 60,
			'characteristic' => 61
		}
	},
	{#State 51
		ACTIONS => {
			'COLON' => 62
		}
	},
	{#State 52
		DEFAULT => -43
	},
	{#State 53
		ACTIONS => {
			'ID' => 51
		},
		DEFAULT => -42,
		GOTOS => {
			'member' => 63
		}
	},
	{#State 54
		DEFAULT => -22
	},
	{#State 55
		DEFAULT => -30
	},
	{#State 56
		DEFAULT => -31
	},
	{#State 57
		DEFAULT => -29
	},
	{#State 58
		DEFAULT => -25,
		GOTOS => {
			'@2-1' => 64
		}
	},
	{#State 59
		DEFAULT => -24
	},
	{#State 60
		DEFAULT => -27,
		GOTOS => {
			'@3-1' => 65
		}
	},
	{#State 61
		DEFAULT => -23
	},
	{#State 62
		DEFAULT => -49,
		GOTOS => {
			'ids' => 66,
			'ids1' => 67
		}
	},
	{#State 63
		DEFAULT => -44
	},
	{#State 64
		ACTIONS => {
			'STRING' => 69
		},
		GOTOS => {
			'string' => 68
		}
	},
	{#State 65
		ACTIONS => {
			'AT' => 38
		},
		DEFAULT => -35,
		GOTOS => {
			'scope' => 70
		}
	},
	{#State 66
		ACTIONS => {
			'ID' => 71
		}
	},
	{#State 67
		ACTIONS => {
			'RBRACKET' => 72,
			'EOL' => 74
		},
		GOTOS => {
			'eom' => 73
		}
	},
	{#State 68
		DEFAULT => -26
	},
	{#State 69
		ACTIONS => {
			'EOL' => 75
		}
	},
	{#State 70
		ACTIONS => {
			'LPAREN' => 76
		},
		DEFAULT => -32,
		GOTOS => {
			'char_type' => 77,
			'assoc_type' => 78
		}
	},
	{#State 71
		ACTIONS => {
			'ID' => -50
		},
		DEFAULT => -48
	},
	{#State 72
		ACTIONS => {
			'EOL' => 79
		}
	},
	{#State 73
		DEFAULT => -45
	},
	{#State 74
		DEFAULT => -46
	},
	{#State 75
		DEFAULT => -51
	},
	{#State 76
		ACTIONS => {
			'ID' => 80
		}
	},
	{#State 77
		ACTIONS => {
			'STRING' => 69
		},
		GOTOS => {
			'string' => 81
		}
	},
	{#State 78
		DEFAULT => -33
	},
	{#State 79
		DEFAULT => -47
	},
	{#State 80
		ACTIONS => {
			'RPAREN' => 82
		}
	},
	{#State 81
		DEFAULT => -28
	},
	{#State 82
		DEFAULT => -34
	}
],
				   yyrules   =>
[
	[#Rule 0
		 '$start', 2, undef
	],
	[#Rule 1
		 'maplet_definitions', 0, undef
	],
	[#Rule 2
		 'maplet_definitions', 2, undef
	],
	[#Rule 3
		 'maplet_definitions', 3, undef
	],
	[#Rule 4
		 'maplet_definitions', 3, undef
	],
	[#Rule 5
		 'maplet_definitions', 3,
sub
#line 39 "yapp/astma-fact.yp"
{ warn "Logging $_[2]"; }
	],
	[#Rule 6
		 'maplet_definitions', 3,
sub
#line 40 "yapp/astma-fact.yp"
{ die  "Cancelled"; }
	],
	[#Rule 7
		 'maplet_definitions', 3,
sub
#line 41 "yapp/astma-fact.yp"
{
		                                              use Encode;
							      Encode::from_to ($_[0]->YYData->{INPUT}, "iso-8859-1", $_[2]);
							     }
	],
	[#Rule 8
		 'maplet_definition', 1, undef
	],
	[#Rule 9
		 'maplet_definition', 1, undef
	],
	[#Rule 10
		 'maplet_definition', 1, undef
	],
	[#Rule 11
		 '@1-5', 0,
sub
#line 53 "yapp/astma-fact.yp"
{
			$_[1] = $_[0]->{USER}->{store}->internalize ($_[1]);

			if (ref $_[3]) {                                                   # we have reification info
			    if (     $_[3]->[0] == 1) {                                    # 1 = REIFIES, means current ID is a shorthand for the other
				$_[0]->{USER}->{store}->internalize ($_[1] => $_[3]->[1]); 
			    } elsif ($_[3]->[0] == 0) {                                    # 0 = IS-REIFIED, this must be the other way round
				$_[0]->{USER}->{store}->internalize ($_[3]->[1] => $_[1]);
			    } elsif ($_[3]->[0] == 2) {                                    # 2 = ISINDICATEDBY, add the subject indicators
				$_[0]->{USER}->{store}->internalize ($_[1] => \ $_[3]->[1]);
			    } else {
				die "internal fu**up";
			    }
			}
			# assert instance/class
                        if (@{$_[2]}) {
			    $_[0]->{USER}->{store}->assert ( map {
							  [ undef, 
							    undef, 
							    'isa', 
							    undef,
							    [ 'class', 'instance' ], 
                                                            [ $_, $_[1] ], 
							    ]}  
							     @{$_[2]} );
			}
			{                                                                  # memorize that the types should be a 'topic' at the end (see end of parse)
			    my $implicits = $_[0]->{USER}->{implicits};
#			    my $s         = $_[0]->{USER}->{store};
			    map { $implicits->{'isa-thing'}->{$_}++ } 
			             (@{$_[2]}, $_[1]);                                    # the types and the ID are declared implicitely
			}
			
			if (ref $_[4]) {                                                   # there are inline assocs
#warn "test for inlines";
			    foreach (@{$_[4]}) {
				my $type      = $_->[0];
				my $player    = $_->[1];
				my $store     = $_[0]->{USER}->{store};
				my $templates = $_[0]->{USER}->{templates};
#warn "found type $type $player";
				if ($type eq 'is-subclass-of' || $type eq 'subclasses') {
				    $store->assert ([ undef,                          # LID
						      undef,                          # SCOPE
						      'is-subclass-of',               # TYPE
						      TM->ASSOC,               # KIND
						      [ 'subclass',  'superclass' ],  # ROLES
						      [ $_[1],       $player ],       # PLAYERS
						      undef ] );
				} elsif ($type eq 'is-a') {
				    $store->assert ([ undef,                   	      # LID
						      undef,                   	      # SCOPE
						      'isa',                  	      # TYPE
						      TM->ASSOC,        	      # KIND
						      [ 'instance', 'class' ], 	      # ROLES
						      [ $_[1],       $player ],	      # PLAYERS
						      undef ] );
				} elsif ($type eq 'has-a') {                          # same, but other way round
				    $store->assert ([ undef,                   	      # LID
						      undef,                   	      # SCOPE
						      'isa',               	      # TYPE
						      TM->ASSOC,        	      # KIND
						      [ 'instance', 'class' ], 	      # ROLES
						      [ $player,     $_[1] ],	      # PLAYERS
						      undef ] );
				} elsif ($templates->mids ( $type ) &&
					 (my @ts    = $templates->match (TM->FORALL, type => $templates->mids ( $type )  ))) {
#warn "found templates for $type";
				    warn "duplicate template for '$type' found (maybe typo?), taking one" if @ts > 1;
#warn Dumper $templates if @ts > 1;
				    my $t = $ts[0];                                   # I choose one
#warn "YYY cloning ($type)";
				    
				    $store->assert ([ undef,                   	      # LID
						      undef,                   	      # SCOPE
						      $type,               	      # TYPE
						      TM->ASSOC,        	      # KIND
						      [  	                      # ROLES
							map {
							    my $l = $templates->reified_by ($_);
							    ($l && $l eq LEFT ?
							           $_[1]
							     :
							           ($l && $l eq RIGHT ?
                                                                          $player
                                                                    :
								          $_)
							     )
							    } @{$t->[TM->ROLES]} 
						      ],
						      [                       	      # PLAYERS
							map {
							    my $l = $templates->reified_by ($_);
							    ($l && $l eq LEFT ?
							           $_[1]
							     :
							           ($l && $l eq RIGHT ?
                                                                          $player
                                                                    :
								          $_)
							     )
							    } @{$t->[TM->PLAYERS]} 
						      ],
						      undef ] );
				} else {
				    die "unknown association type '$type' in inlined association";
				}
			    }
			}
		     }
	],
	[#Rule 12
		 'topic_definition', 7,
sub
#line 164 "yapp/astma-fact.yp"
{
#warn "char/ind in topic: ".Dumper $_[7];
                         my $id = $_[1];
                         # add assertions for every characteristic
                         $_[0]->{USER}->{store}->assert ( map {[ undef,                                         # LID
                                                                 $_->[1],                                       # SCOPE
                                                                 $_->[2] ||                                     # TYPE
                                                                    ($_->[0] == TM->NAME ? 'name' : 'occurrence'),
                                                                 $_->[0],                                       # KIND
                                                                 [ 'thing', 'value' ],                          # ROLES
                                                                 [ $id,             $_->[3] ],                  # PLAYERS
                                                                 undef ] }
                                                               @{$_[7]->[0]} );

                         map { $store->internalize ($id => \ $_ ) } @{$_[7]->[1]};       # add the subject indicators

			 {                                                               # memorize basename types and scopes as implicitely defined
			     my $implicits = $_[0]->{USER}->{implicits};
			     map { $implicits->{'isa-scope'}->{$_}++ }
                                   map { $_->[1] } grep ($_->[1], @{$_[7]->[0]});                # get the bloody scopes and tuck them away

			     map { $implicits->{'subclasses'}->{ $_->[0] == TM->NAME ? 'name' : 'occurrence' }->{$_->[2]}++ }
                                   grep ($_->[2], @{$_[7]->[0]});                                # get all the characteristics with types

#warn "implicits then ".Dumper $implicits;
			 }
		         }
	],
	[#Rule 13
		 'reification_indication', 0, undef
	],
	[#Rule 14
		 'reification_indication', 2,
sub
#line 194 "yapp/astma-fact.yp"
{ [ 1, $_[2] ] }
	],
	[#Rule 15
		 'reification_indication', 2,
sub
#line 195 "yapp/astma-fact.yp"
{ [ 0, $_[2] ] }
	],
	[#Rule 16
		 'reification_indication', 2,
sub
#line 196 "yapp/astma-fact.yp"
{ [ 2, $_[2] ] }
	],
	[#Rule 17
		 'types', 0,
sub
#line 199 "yapp/astma-fact.yp"
{ [] }
	],
	[#Rule 18
		 'types', 2,
sub
#line 200 "yapp/astma-fact.yp"
{ push @{$_[1]}, @{$_[2]}; $_[1] }
	],
	[#Rule 19
		 'type', 2,
sub
#line 203 "yapp/astma-fact.yp"
{ [ $_[2] ] }
	],
	[#Rule 20
		 'type', 3,
sub
#line 204 "yapp/astma-fact.yp"
{   $_[2]   }
	],
	[#Rule 21
		 'characteristics_indication', 0, undef
	],
	[#Rule 22
		 'characteristics_indication', 2,
sub
#line 209 "yapp/astma-fact.yp"
{ push @{$_[1]->[ ref($_[2]) eq 'ARRAY' ? 0 : 1 ]}, $_[2]; $_[1] }
	],
	[#Rule 23
		 'characteristic_indication', 1, undef
	],
	[#Rule 24
		 'characteristic_indication', 1, undef
	],
	[#Rule 25
		 '@2-1', 0,
sub
#line 217 "yapp/astma-fact.yp"
{ $_[0]->{USER}->{string} ||= "\n" }
	],
	[#Rule 26
		 'indication', 3,
sub
#line 218 "yapp/astma-fact.yp"
{ $_[3] }
	],
	[#Rule 27
		 '@3-1', 0,
sub
#line 221 "yapp/astma-fact.yp"
{ $_[0]->{USER}->{string} ||= "\n" }
	],
	[#Rule 28
		 'characteristic', 5,
sub
#line 222 "yapp/astma-fact.yp"
{                           # check whether we are dealing with URIs or strings
				                                       if ($_[1] == TM->NAME) {  # names are always strings
									   $_[5] = new TM::Literal  ($_[5], 'xsd:string');
								       } elsif ($_[5] =~ /^\w+:\S+$/) { # can only be OCC, but is it URI?
									   $_[5] = new TM::Literal  ($_[5], 'xsd:uri');
								       } else {                  # occurrence and not a URI -> string
									   $_[5] = new TM::Literal  ($_[5], 'xsd:string');
								       }
## warn "char ".Dumper [ $_[1], $_[3], $_[4], $_[5] ];
								      [ $_[1], $_[3], $_[4], $_[5] ]
								      }
	],
	[#Rule 29
		 'class', 1,
sub
#line 235 "yapp/astma-fact.yp"
{ TM->NAME  }
	],
	[#Rule 30
		 'class', 1,
sub
#line 236 "yapp/astma-fact.yp"
{ TM->OCC  }
	],
	[#Rule 31
		 'class', 1,
sub
#line 237 "yapp/astma-fact.yp"
{ TM->OCC  }
	],
	[#Rule 32
		 'char_type', 0, undef
	],
	[#Rule 33
		 'char_type', 1, undef
	],
	[#Rule 34
		 'assoc_type', 3,
sub
#line 244 "yapp/astma-fact.yp"
{   $_[2]   }
	],
	[#Rule 35
		 'scope', 0, undef
	],
	[#Rule 36
		 'scope', 2,
sub
#line 248 "yapp/astma-fact.yp"
{ $_[2] }
	],
	[#Rule 37
		 'inline_assocs', 0, undef
	],
	[#Rule 38
		 'inline_assocs', 2,
sub
#line 253 "yapp/astma-fact.yp"
{ push @{$_[1]}, $_[2]; $_[1] }
	],
	[#Rule 39
		 'inline_assoc', 2,
sub
#line 256 "yapp/astma-fact.yp"
{ [ $_[1], $_[2] ] }
	],
	[#Rule 40
		 '@4-1', 0,
sub
#line 260 "yapp/astma-fact.yp"
{ ($_[0]->{USER}->{templates}, $_[0]->{USER}->{store}) = ($_[0]->{USER}->{store}, $_[0]->{USER}->{templates}); }
	],
	[#Rule 41
		 'template_definition', 3,
sub
#line 263 "yapp/astma-fact.yp"
{ ($_[0]->{USER}->{templates}, $_[0]->{USER}->{store}) = ($_[0]->{USER}->{store}, $_[0]->{USER}->{templates}); }
	],
	[#Rule 42
		 'association_definition', 7,
sub
#line 269 "yapp/astma-fact.yp"
{
##warn "members ".Dumper $_[5];
## ??? TODO SCOPE ????
			       my (@roles, @players);
			       foreach my $m (@{$_[7]}) {                 # one member
				   my $role = shift @$m;                  # first is role
				   
				   while (@$m) {
				       push @roles, $role;                # roles repeat for every player
				       my $player = shift @$m;
				       push @players, $player;
				   }
			       }
			       my ($a) = $_[0]->{USER}->{store}->assert ( [ undef, $_[4], $_[2], TM->ASSOC, \@roles, \@players, undef ] );
##warn "templates" .Dumper $_[0]->{USER}->{store};
                              { # reification
				  my $ms = $_[0]->{USER}->{store};
				  if (ref $_[5]) {
				      if ($_[5]->[0] == 1) {                   # 1 = REIFIES, 0 = IS-REIFIED
					  # (assoc) reifies http://.... means
					  #     1) the assoc will be addes as thing (is done already)
					  #     2) the http:// will be used as one subject indicator
					  die "reifier of association must be a URI" unless $_[5]->[1] =~ /^\w+:.+/;
					  $ms->internalize ($a->[TM::LID], $_[5]->[1]);
				      } elsif ($_[5]->[0] == 0) {              # something reifies this assoc
					  # (assoc) is-reified-by xxx   means
					  #     1) assoc is added as thing (is done already)
					  #     2) the local identifier is added as thing with the abs URL of the assoc as subject address
					  die "reifier must be local identifier" unless $_[5]->[1] =~ /^\w+$/;
					  $ms->internalize ($_[5]->[1] => $a->[TM::LID]);
				      } else { # this would be 'indication' but we do not want that here
					  die "indication for association are undefined";
				      }
				  }
			      }

			       { # memorize that association type subclasses association
#				   my $implicits = $_[0]->{USER}->{implicits};

# implicit			   $implicits->{'subclasses'}->{'association'}->{$_[2]}++;
				   $_[0]->{USER}->{implicits}->{'isa-scope'}->{$_[4]}++ if $_[4];
			       }
			   }
	],
	[#Rule 43
		 'association_members', 1,
sub
#line 314 "yapp/astma-fact.yp"
{                       [ $_[1] ] }
	],
	[#Rule 44
		 'association_members', 2,
sub
#line 315 "yapp/astma-fact.yp"
{ push @{$_[1]}, $_[2];   $_[1]  }
	],
	[#Rule 45
		 'member', 4,
sub
#line 318 "yapp/astma-fact.yp"
{ [ $_[1], @{$_[3]} ] }
	],
	[#Rule 46
		 'eom', 1, undef
	],
	[#Rule 47
		 'eom', 2, undef
	],
	[#Rule 48
		 'ids1', 2,
sub
#line 325 "yapp/astma-fact.yp"
{ push @{$_[1]}, $_[2]; $_[1] }
	],
	[#Rule 49
		 'ids', 0,
sub
#line 328 "yapp/astma-fact.yp"
{ [] }
	],
	[#Rule 50
		 'ids', 2,
sub
#line 329 "yapp/astma-fact.yp"
{ push @{$_[1]}, $_[2]; $_[1] }
	],
	[#Rule 51
		 'string', 2,
sub
#line 332 "yapp/astma-fact.yp"
{ die "empty string in characteristics" unless $_[1]; $_[1] }
	]
],
				   %options);
    $self->{USER}->{store}         = $store;
    return bless $self, $class;
}

#line 335 "yapp/astma-fact.yp"


sub _Error {
    die "Syntax error: Found ".$_[0]->YYCurtok." but expected ".join (' or ', $_[0]->YYExpect);
}

sub _Lexer {
    my $parser = shift;
    my $refINPUT = \$parser->YYData->{INPUT};

    my $aux;                                                                           # need this to store identifier/uri prefix temporarily (optimization)

    $$refINPUT                                        or  return ('',          undef);
    $$refINPUT =~ s/^[ \t]+//so;

#warn "lexer ($parser->{USER}->{string}):>>>".$parser->YYData->{INPUT}."<<<";

    $$refINPUT =~ s/^\n//so                           and return ('EOL',       	   undef);
    $$refINPUT =~ s/^in\b//o                          and return ('IN',        	   undef);
    $$refINPUT =~ s/^rd\b//o                          and return ('IN',        	   undef);
    $$refINPUT =~ s/^oc\b//o                          and return ('OC',        	   undef);
    $$refINPUT =~ s/^ex\b//o                          and return ('OC',        	   undef);
    $$refINPUT =~ s/^bn\b//o                          and return ('BN',        	   undef);

    $$refINPUT =~ s/^sin\b//o                         and return ('SIN',       	   undef);
    $$refINPUT =~ s/^is-a\b//o                        and return ('ISA',       	   undef);
    $$refINPUT =~ s/^reifies\b//o                     and return ('REIFIES',   	   undef);
    $$refINPUT =~ s/^=//o                             and return ('REIFIES',   	   undef);
    $$refINPUT =~ s/^is-reified-by\b//o               and return ('ISREIFIED', 	   undef);
    $$refINPUT =~ s/^~//o                             and return ('ISINDICATEDBY', undef);

    if (my $t = $parser->{USER}->{string}) {                                           # parser said we should expect a string now, defaults terminator to \n
##warn "scanning for string (..$t..) in ...". $$refINPUT . "....";
	$$refINPUT =~ s/^:\s*<<<\n/:/o                and                              # we know it better, it is <<<
	    $t = "\n<<<\n";

	$$refINPUT =~ s/^:\s*<<(\w+)\n/:/o            and                              # we know it better, it is <<SOMETHING
	    $t = "\n<<$1\n";

##warn "try finding string ..$t..  " ;
	$$refINPUT =~ s/^:\s*(.*?)\s*$t/\n/s          and 
##            (warn "returning $1" or 1) and
	    (undef $parser->{USER}->{string}          or  return ('STRING',    $1));
##warn "no string";
    }

    $$refINPUT =~ s/^://o                             and return ('COLON',     undef);

## unfortunately, this does not what I want
##  $$refINPUT =~ s/^([A-Za-z][A-Za-z0-9_-]*)(?!:)//o and return ('ID',        $1); # negative look-ahead
## tricky optimization: don't ask
    $$refINPUT =~ s/^([A-Za-z][.A-Za-z0-9_-]*)//o     and $aux = $1                 # save this for later
	                                              and $$refINPUT !~ /^:[\w\/]/
                                                      and return ('ID',        $aux);

    $$refINPUT =~ s/^\(//so                           and return ('LPAREN',    undef);
    $$refINPUT =~ s/^\)//so                           and return ('RPAREN',    undef);
    $$refINPUT =~ s/^@//so                            and return ('AT',        undef);

    $$refINPUT =~ s/^(:[^\s\)\(\]\[]+)//o             and return ('ID',        $aux.$1); # is a URL/URN actually

    $$refINPUT =~ s/^(\d{4}-\d{1,2}-\d{1,2})(\s+(\d{1,2}):(\d{2}))?//o
                                                      and return ('ID',        sprintf "urn:x-date:%s:%02d:%02d", $1, $3 || 0, $4 || 0); # is a date

    $$refINPUT =~ s/^%log\s+(.*?)(?=\n)//so           and return ('LOG',       $1); # positive look-ahead
    $$refINPUT =~ s/^%cancel(?=\n)//so                and return ('CANCEL',    $1); # positive look-ahead
    $$refINPUT =~ s/^%encoding\s+(.*?)(?=\n)//so      and return ('ENCODING',  $1); # positive look-ahead

    $$refINPUT =~ s/^\*//o                            and return ('ID',        sprintf "uuid-%010d", $TM::toplet_ctr++); ## $parser->{USER}->{topic_count}++);

    $$refINPUT =~ s/^\[//so                           and return ('LBRACKET',  undef);
    $$refINPUT =~ s/^\]//so                           and return ('RBRACKET',  undef);
    # should not be an issue except on error
    $$refINPUT =~ s/^(.)//so                          and return ($1,          $1);

}

sub parse {
    my $self               = shift;
    $self->YYData->{INPUT} = shift;

#warn "parse";
    $self->YYData->{INPUT} =~ s/\r/\n/sg;
    $self->YYData->{INPUT} =~ s/(?<!\\)\\\n//sg;   # a \, but not a \\
    $self->YYData->{INPUT} =~ s/ \~ /\n/g;         # replace _~_ with \n
    $self->YYData->{INPUT} =~ s/ \~\~ / \~ /g;     # stuffed ~~ cleanout
    $self->YYData->{INPUT} =~ s/^\#.*?\n/\n/mg;    # # at there start of every line -> gone
    $self->YYData->{INPUT} =~ s/\s+\#.*?\n/\n/mg;  # anything which starts with <blank>#, all blanks are ignored
    $self->YYData->{INPUT} =~ s/\n\n\n+/\n\n/sg;

    # we not only capture what is said EXPLICITELY in the map, we also collect implicit knowledge
    # we could add this immediately into the map at parsing, but it would slow the process down and
    # it would probably duplicate/complicate things
    $self->{USER}->{implicits} = {
	'isa-thing'  => undef,                                          # just let them spring into existence
	'isa-scope'  => undef,                                          # just let them spring into existence
	'subclasses' => undef
	};
#    $self->{USER}->{topic_count} = 0;
                                                                        # clone a pseudo map into which to store templates as assocs temporarily
    $self->{USER}->{templates} = new TM (baseuri => $self->{USER}->{store}->baseuri);

    eval {
	$self->YYParse ( yylex => \&_Lexer, yyerror => \&_Error );
    }; if ($@ =~ /^Cancelled/) {
	warn $@;                                                        # de-escalate Cancelling to warning
    } elsif ($@) {
	die $@;                                                         # otherwise re-raise the exception
    }
#warn "in parse end";
#warn "in parse end ".Dumper $self->{USER}->{implicits};

    { # resolving implicit stuff
	my $implicits = $self->{USER}->{implicits};
	my $store     = $self->{USER}->{store};

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

    return $self->{USER}->{store};
}

#my $f = new TM::AsTMa::Fact;
#$f->Run;


1;

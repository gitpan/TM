package TM;

use strict;
use warnings;

require Exporter;
use base qw(Exporter);

use Data::Dumper;
use Class::Struct;
use Time::HiRes;

use TM::PSI;



=pod

=head1 NAME

TM - Topic Maps, Base Class

=head1 SYNOPSIS

    my $tm = new TM (baseuri => 'tm://whatever/');   # empty map

    # add a midlet (= minimal topic, only identification, no characteristics)
    # by specifying an internal ID
    $tm->internalize ('aaa');                     # only internal identifier
    $tm->internalize ('bbb' =>   'http://bbb/');  # with a subject address
    $tm->internalize ('ccc' => \ 'http://ccc/');  # with a subject indicator

    # without specifying an internal ID (will be auto-generated)
    $tm->internalize (undef =>   'http://ccc/');  # with a subject address
    $tm->internalize (undef => \ 'http://ccc/');  # with a subject indicator

    # get rid of midlet(s)
    $tm->externalize ('tm://whatever/aaa', ...);

    # add an assertion (association or characteristic)
    $a = $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'subclass', 'superclass' ], players => [ 'rumsti', 'ramsti' ]));

    # get rid of assertion(s)
    $tm->retract ($a->[TM->LID], ...);

    # extract particular assertions
    my @as = $tm->retrieve (....);

    # find particular assertions
    my @as = $tm->match (TM->FORALL, scope   => 'tm://whatever/sss');

    my @bs = $tm->match (TM->FORALL, type    => 'tm://whatever/ttt',
                                     roles   => [ 'tm://whatever/aaa', 'tm://whatever/bbb' ]);

    my @cs = $tm->match (TM->FORALL, type    => 'tm://whatever/is-subclass-of', 
			             arole   => 'tm://whatever/superclass', 
			             aplayer => 'tm://whatever/rumsti', 
			             brole   => 'tm://whatever/subclass')


@@@ consolidate.....

=head1 ABSTRACT

This (monster) class provides read/write access to so-called I<materialized> maps, i.e. maps which
completely can reside in memory. Implementations for non-materialized maps can be derived from it.

=head1 DESCRIPTION

As it stands, this package implements directly so-called I<materialized> maps, i.e. those maps which
completely reside in memory. Non-materialized and non-materializable maps can be implemented by
deriving from this class by overloading one or all of the sub-interfaces. If this is done cleverly,
then any application, even a TMQL query processor can operate on non-materialized (virtual) maps in
the same way as on materialized ones.

The data manipulation interface is very low-level and B<directly> exposes internal data structures.
As long as do not mess with the information you get and you follow the API rules, this can provide a
convenient, fast, albeit not overly comfortable interface.

=head2 Consistency

An application using a map may expect that a map is I<consolidated>, i.e. that the following
consistency conditions are met:

=over

=item B<A1> (fixed on)

Every topic appearing in some association as type, role or player is also registered as topic.

=item B<A2> (fixed on)

Every association in the map is also a registered topic.

=item B<Indicator_based_Merging> (default: on)

Two (or more) topics sharing the same I<subject identifier> are treated as one topic.

=item B<Subject_based_Merging> (default: on)

Two (or more) topics sharing the same I<subject locator> are treated as one topic.

=item B<TNC_based_Merging> (default: off)

Two (or more) topics sharing the same name in the same scope are treated as one topic.

=back

=cut

use constant {
    Subject_based_Merging   => 1,
    Indicator_based_Merging => 2,
    TNC_based_Merging       => 3,
};

=pod

While the first two (A1, A2) are related with the internal consistency of the data structure, the
others are a choice the application can make. See method C<consistency>.

This consistency is not automatically provided when a map is modified by the application. It is the
applications responsibility to trigger the process to consolidate the map.

When an IO driver is consuming a map from a resource (say load an XTM file), then that driver will
ensure that the map is consolidated according to the current settings before it is handed to the
application. The application is then in full control of the map as it can change, add and delete
topics and associations. This implies that that can become unconsolidated in this process. The
method C<consolidate> reinstates consistency again.

You can change these defaults by (a) providing an additional option to the constructor

   new TM (....,
           consistency => [ TM->Subject_based_Merging,
                            TM->Indicator_based_Merging ]);

or (b) by using the accessor C<consistency> (see below).

=head1 INFRASTRUCTURE INTERFACE

=head2 Constructor

The constructor will create an empty map, or, to be more exact, it will fill the map with the
taxonomy from L<TM::PSI> which covers basic concepts such as I<topic> or I<associations>.

The constructor understands a number of key/value pair parameters:

=over

=item C<baseuri> (default: C<tm://nirvana/>)

Every item in the map has an unique local identifier (e.g. C<shoesize>). The C<baseuri> parameter
controls how an absolute URI is built from this identifier.

=item C<consistency> (default: [ Subject_based_Merging, Indicator_based_Merging ])

=item C<psis>

If you need to roll your own taxonomy to bootstrap with, you can pass in a structure which has
exactly the same structure as that in L<TM::PSI>.

=back

=cut

sub new {
  my $class = shift;
  my %self  = @_;

  $self{consistency} ||= [ Subject_based_Merging, Indicator_based_Merging ];
  $self{baseuri}     ||= 'tm://nirvana/';
  $self{baseuri}      .= '#' unless $self{baseuri} =~ m|[/\#:]$|;

  my $self = bless \%self, $class;

  unless ($self->{mid2iid}) {                                                     # we need to do fast cloning of basic vocabulary
      use TM::PSI;
      my $psis = $self{psis} || $TM::PSI::topicmaps;
      my $mids = $psis->{mid2iid};
      my $bu   = $self->{baseuri};
                                                                                  # now create low-level TM content via fast cloning
      $self->{mid2iid}        = { map { $bu.$_ => [ undef, [ @{$mids->{$_}} ] ] }   keys %{$mids} };
      $self->{usual_suspects} = { map { $_ => $self->mids ($_) } @TM::PSI::Usual_Suspects };
      $self->assert (             map { Assertion->new (type    => $_->[0],
							roles   => [@{$_->[1]}],  # here we clone the roles/player list
							players => [@{$_->[2]}])} @{$psis->{assertions}}  );
      delete $self{psis};                                                         # we do not need it anymore
  }

  $self->{last_mod} = 0;                                                          # book keeping
  $self->{created}  = Time::HiRes::time;

  return $self;
}

sub DESTROY {}                                                                    # not much to do here

=pod

=head2 Methods

=over

=item B<baseuri>

I<$bu> = I<$tm>->baseuri

This methods retrieves/sets the base URI component of the map. This is a read-only method. The base
URI is B<always> defined.

=cut

sub baseuri {
    my $self = shift;
    return $self->{baseuri};
}

=pod

=item B<consistency>

I<@merging_constraints> = I<$tm>->consistency

I<$tm>->consistency (I<@list_of_constants>)

This method provides read/write access to the consistency settings.

If no parameters are provided, then the current list of consistency settings is returned.  If
parameters are provided, that list must consist of the constants defined above (see
L</Consistency>).

B<NOTE>: Changing the consistency does B<NOT> automatically trigger C<consolidate>.

=cut

sub consistency {
  my $self   = shift;
  my @params = @_;

  $self->{consistency} = [ @params ] if @params;
  return @{$self->{consistency}};
}

=pod

=item B<consolidate>

I<$tm>->consolidate

This method I<consolidates> a map by performing the following actions:

=over

=item * 

perform merging based on subject address (see TMDM section 5.3.2)

=item * 

perform merging based on subject indicators (see TMDM section 5.3.2)

=item * 

remove all superfluous toplets (those which do not take part in any association)

NOTE: Not implemented yet!

=back

The optional parameter is a list of constants, all of which are defined in L<TM>. If the list is
empty, then the consistency of the map will be used, otherwise the consistency as defined with this
list will override.

B<NOTE>: In all cases the map will be modified.

B<NOTE>: After merging some of the I<lids> might not be reliably point to a topic.

=cut

# NOTE: Below there much is done regarding speed. First the toplets are sweeped detecting which have
# to be merged. This is not done immediately (as this is an expensive operation, but a 'merger' hash
# is built. Note how merging information A -> B and A -> C is morphed into A -> B and B -> C using
# the _find_free function.

# That merger hash is the consolidated by following edges until their end, so that there are no
# cycles.

sub consolidate {
  my $self = shift;
  my $cons = @_ ? [ @_ ] : $self->{consistency};
  my $indi = grep ($_ == Indicator_based_Merging, @{$self->{consistency}});
  my $subj = grep ($_ == Subject_based_Merging,   @{$self->{consistency}});
  my $tnc  = grep ($_ == TNC_based_Merging,       @{$self->{consistency}});

#warn "cond indi $indi subj $subj tnc $tnc";

  my %SIDs; # holds subject addresses found
  my %SINs; # holds subject indicators found
  my %BNs;  # holds basename + scope found

#warn Dumper $cons;

#== find merging points and memorize this in mergers =======================================================================
  my %mergers;                                                             # will contain the merging edges
  my $mid2iid = $self->{mid2iid};                                          # shortcut

MERGE:
  foreach my $this (keys %{$mid2iid}) {
#warn "looking at $this";
      my $thism = $mid2iid->{$this};
#warn "SIDs: ". Dumper \%SIDs;
#warn "SINs: ". Dumper \%SINs;
#-- based on subject indication ------------------------------------------------------------------------------------------
      if ($indi) {
	  foreach my $sin (@{$thism->[TM->INDICATORS]}) {                  # walk over the subject indicators
	      if (my $that  = $SINs{$sin}) {                               # $that is now a key pointing to a merging partner
#warn "merging (IND) $this >> $that"; #. Dumper $thism, $thatm;
		  $mergers{_find_free ($this, \%mergers)} = $that;

		  sub _find_free {
		      my $this = shift;
		      my $mergers = shift;
		      
		      my $this2 = $this;
		      my $this3;
		      while ($this3 = $mergers->{$this2}) {
			  if ($this3 eq $this || $this3 eq $this2) {       # loop, we do not need it
			      return $this3;
			  } else {
			      $this2 = $this3;                             # we follow the trail
			  }
		      }
		      return $this2;                                       # this2 was the end of the trail
		  }
              } else {                                                     # no merging, so enter the sins
                  $SINs{$sin} = $this;
	      }
	  }
      }
#-- based on subject address ---------------------------------------------------------------------------------------------
      if ($subj) {
	  if (my $sid = $thism->[TM->ADDRESS]) {
	      if (my $that = $SIDs{$sid}) {                                # found partner => should be merged
#warn "merging (ADDR) $this >> $that";
		  $mergers{_find_free ($this, \%mergers)} = $that;
		  # must obviously both have the same subject address, so, no reason to touch this
	      } else {                                                     # there is no partner, first one with this subject address
		  $SIDs{$sid} = $this;
	      }
	  }
      }
#warn "after TM->ADDRESS on '$this' ";#.Dumper $mid2iid;
  }
#-- based on TNC ---------------------------------------------------------------------------------------------
  if ($tnc) {
      my ($THING, $VALUE) = @{$self->{usual_suspects}}{'thing', 'value'};
      foreach my $a (values %{$self->{assertions}}) {
	  next unless $a->[TM->KIND] == TM->NAME;                          # we are only interested in basenames
#warn "checking assertion ".Dumper $a;
	  my ($v) = get_x_players ($self, $a, $VALUE);                     # if we get back a longer list, bad luck
	  my $bn_plus_scope = $v->[0] .                                    # the basename is a string reference
                              $a->[TM->SCOPE];                             # relative to the scope
	  my ($this) = get_x_players ($self, $a, $THING);                  # thing which plays 'topic'
#warn "    --> player is $this";
	  if (my $that = $BNs{$bn_plus_scope}) {                           # if we have seen it before
#warn "  -> SEEN";
	      $mergers{_find_free ($this, \%mergers)} = $that;
	  } else {                                                         # it is new to use, we store it into %BNs
#warn "  -> NOT SEEN";
	      $BNs{$bn_plus_scope} = $this;
#warn "BNs ".Dumper \%BNs;
	  }
      }
  }
#== consolidate mergers: no cycles, trail followed through ======================================================
#warn "mergers ".Dumper \%mergers;

  for (2..2) { # at most 2, theoretical only one should be sufficient
      my $changes = 0;
      foreach my $h (keys %mergers) {
#warn "working on $h";
	  if ($mergers{$h} eq $h) { # micro loop
	      delete $mergers{$h};
	  } elsif (defined $mergers{$mergers{$h}} && $mergers{$mergers{$h}} eq $h) {
	      delete $mergers{$h};
	  } else {
	      my $h2 = $mergers{$h};
	      my %seen = ($h => 1,  $h2 => 1); # loop avoidance
#warn "seeen start".Dumper \%seen;
	      while ($mergers{$h2} and !$seen{$mergers{$h2}}++) { $h2 = $mergers{$h} = $mergers{$h2}; $changes++;}
#warn "half consolidated (chagens $changes)" .Dumper $H;
	  }
      }
#      warn "consoli loop $_: changes: $changes";
      warn "early finish" if $_ == 1 and $changes == 0;
      last if $changes == 0;
#      die "not clean" if $_ == 2 and $changes > 0;
  }

#warn "consolidated mergers ".Dumper \%mergers;


#== actual merging ========================================================================================
  foreach my $that (keys %mergers) {
      my $this  = $mergers{$that};
      my $thism = $mid2iid->{$this};
      my $thatm = $mid2iid->{$that};                           # shorthand
      next if $thatm == $thism;                  # we already have merged

      die "two different subject addresses for two topics to be merged ($this, $that)" 
	  if $thism->[TM->ADDRESS] and $thatm->[TM->ADDRESS] and 
	     $thism->[TM->ADDRESS] ne  $thatm->[TM->ADDRESS];
#warn "merge now $that > $this";
             $thism->[TM->ADDRESS]  ||=   $thatm->[TM->ADDRESS];                 # first subject address, then indicators
      push @{$thism->[TM->INDICATORS]}, @{$thatm->[TM->INDICATORS]};
      $mid2iid->{$that} = $thism;
  }
#warn "after post-merger ". Dumper $mid2iid;

  $self->{mid2iid}  = $mid2iid;                                                  # this makes tie happy, in the case the map is tied
  $self->{last_mod} = Time::HiRes::time;
}

=pod

=item B<add>

I<$tm>->add (I<$tm2>, ...)

This method accepts a list of L<TM> objects and adds all content (associations and topics) from
these maps.

B<NOTE>: There is B<NO> merging done. Use explicitly method C<consolidate> for it.

=cut

sub add {
    my $self = shift;

#warn "store add". Dumper \@_;
    foreach (@_) {                                         # deal with one store after the other
	while (my ($k, $v) = each %{$_->{assertions}}) {
	    $self->{assertions}->{$k} = $v;                # there should not be any conflicts, using MD5 hashes over type/scope/roles/players should be good enough
	}
	my $mid2iid = $self->{mid2iid};                    # shorthand
	while (my ($k, $v) = each %{$_->{mid2iid}}) {
	    if (! $mid2iid->{$k}) {                        # we had no entry here => simply...
		$mid2iid->{$k} = $v;                       # ...add what the other has
	    } else {                                       # same internal identifier? danger lurking...
		if (!$v->[0]) {                            # new had undef there, leave what we have
		} elsif (!$mid2iid->{$k}->[0]) {           # old had nothing, =>
		    $mid2iid->{$k}->[0] = $v->[0];         # copy it
		} elsif ($mid2iid->{$k}->[0] eq $v->[0]) { # old had something and new has something and they are the same
		    # leave it
		} else {                                   # not good, subject addresses differ
		    die "using the same internal identifier '$k', but different subject addresses (".$mid2iid->{$k}->[0].",".$v->[0].") is not good (change the baseuri of one map)";
		}
		push @{$mid2iid->{$k}->[1]}, @{$v->[1]};   # simply add all the subject indication stuff
	    }
	}
    }
    $self->{last_mod} = Time::HiRes::time;
}

=pod

=item B<melt>

I<$tm>->melt (I<$tm2>)

This - probably more auxilary - function copies relevant aspect of a second map into the object.

=cut

our @ESSENTIALS = qw(mid2iid assertions baseuri usual_suspects variants);

sub melt {
    my $self = shift;
    my $tm2  = shift;

    @{$self}{@ESSENTIALS} = @{$tm2}{@ESSENTIALS};
    $self->{last_mod} = Time::HiRes::time;
}

=pod

=back

=head1 MANIPULATION INTERFACE

This package provides a low-level implementation of a memory-based assertion store. The assertions
are stored together with some hash information to speed up particular access patterns.  It is
designed to hold a significant amount of information in pure-Perl representation in memory. It is a
also a prime candidate to be implemented in C later.  All changes to the store are immediate; there
is no transaction concept at this level.

The whole map consists of two components: An assertion holds association information, occurrence
attachments to topics and name attachments to topics. Subject identifiers and one (!) subject
locator is kept in a minimalistic topic. Every assertion is ALSO a topic.

On this level you can modify each component individually giving you much freedom and direct access
to the map structure. Needless to say, that you can shoot yourself into the knee.

=head2 Identifiers

All identifiers which are passed into methods here MUST be absolute URIs. This interface makes no
attempt to I<absolutize> identifiers. The URIs are kept as strings, not L<URI> objects.

=head2 Assertions

One assertion is a record containing its own identifier, the scope, the type of the assocation, a
(redundant) field whether this is an association, an occurrence or a name and then all roles and all
players, in separate lists.

These lists B<always> have the same length, so that every player corresponds to exactly one role. If
one role is played by several players, the role appears multiple times.

These lists are also canonicalized, i.e. ordered in such a way, that assertions can be compared. To
flag that an assertion is canonicalized there is another field in the assertion record.

=cut

struct 'Assertion' => [
    lid         => '$',
    scope       => '$',
    type        => '$',
    kind        => '$', # argh
    roles       => '$',
    players     => '$',
    canon       => '$',
];

# indices into this array for fast access
use constant {
    LID     => 0,
    SCOPE   => 1,
    TYPE    => 2,
    KIND    => 3,
    ROLES   => 4,
    PLAYERS => 5,
    CANON   => 6
};

=pod

Assertions consist of the following components:

=over

=item C<LID>:

Every assertion is also a thing in the map, so it has an identifier. For toplet-related information
this is the absolute topic ID, for maplets this is a unique identifier generated from a canonicalized
form of the assertion itself.

=item C<SCOPE>:

Yes, the scope of the assertion.

=item C<KIND> (redundant information):

For technical reasons (read: it is faster) we distinguish between full associations (C<ASSOC>), and
characteristics (C<NAME>, C<OCC>).

=cut

# values for 'kind'
use constant {
    ASSOC    => 0,
    NAME     => 1,
    OCC      => 2,
};

=pod

=item C<TYPE>:

The topic ID of the type of this assertion.

=item C<ROLES>:

A list reference which holds a list of topic IDs for the roles.

=item C<PLAYERS>:

A list reference which holds a list of topic IDs for the players.

=item C<CANON>:

Either C<1> or undef to signal whether this assertion has been (already) canonicalized (see L</Canonicalization>).

=back

=head2 Assertion Construction Functions

These lowest-level functions deal with housekeeping functions for assertions.

=over

=item Constructor

I<$assertion> = Assertion->new (...)

Any of the above fields can be defined.

=item B<absolutize>

I<$assertion> = absolutize (I<$tm>, I<$assertion>)

This method takes one assertion and makes sure that all identifiers in it (for the type, the scope
and all the role and players) are made absolute for the context map. It returns this very assertion.

=cut

sub absolutize {
    my $self = shift;
    my $a    = shift;

    return $a if $a->[CANON];                                                                 # skip it if we are already canonicalized
#warn "in abosl ".Dumper $a;
    $a->[TYPE]    =            mids ($self,         $a->[TYPE])    if $a->[TYPE];
    $a->[SCOPE]   =            mids ($self,         $a->[SCOPE])   if $a->[SCOPE];

    map { $_ =                 mids ($self, $_) } @{$a->[ROLES]}   if $a->[ROLES];            # things which are references, we will keep
    map { $_ = ref ($_) ? $_ : mids ($self, $_) } @{$a->[PLAYERS]} if $a->[PLAYERS];          # the others are treated as ids (could be literal references!)
#warn "after abosl ".Dumper $a;
    return $a;
}

=pod

=item B<canonicalize>

I<$assertion> = canonicalize (I<$tm>, I<$assertion>)

This method takes an assertion and reorders the roles (together with their respective players) in a
consistent way. It also makes sure that the KIND is defined (defaults to C<ASSOC>), that the type is
defined (defaults to C<THING>) and that all references are made absolute LIDs. Finally, the field
C<CANON> is set to 1 to indicate that the assertion is canonicalized.

The function will not do anything if the assertion is already canonicalized.  The component C<CANON>
is set to C<1> if the assertion has been canonicalized.

Conveniently, the function returns the same assertion, albeit a maybe modified one.

=cut

sub canonicalize {
    my $self = shift;
#    my $LIDs = $store->{si};
#    my $base = $store->{baseuri};
    my $s    = shift;
#warn "in canon ".Dumper $s;
#warn "using LIDs ".Dumper $LIDs;

    return $s if $s->[CANON];                                  # skip it if we are already canonicalized

# reorder role/players canonically
    my $rs = $s->[ROLES];
    my $ps = $s->[PLAYERS];
    my @reorder = (0..$#$ps);                                  # create 0, 1, 2, ..., how many roles
#warn @reorder;
    # sort according to roles (alphanum) and at ties according to players on position $a, $b
    @reorder = sort { $rs->[$a] cmp $rs->[$b] || $ps->[$a] cmp $ps->[$b] } @reorder;
#warn @reorder;
    $s->[ROLES]   = [ map { $rs->[$_] } @reorder ];
    $s->[PLAYERS] = [ map { $ps->[$_] } @reorder ];

# we are done (almost)
    $s->[CANON]   = 1;

#warn "in canon return ".Dumper $s;
    return $s;
}

=pod

=item B<hash>

I<$hash> = hash (I<$assertion>);

For internal optimization all characteristics have an additional HASH component which can be used to
maintain indices. This function takes a assertion and computes an MD5 hash and sets the C<HASH>
component if that is not yet defined.

Such a hash only makes sense if the assertion is canonicalized, otherwise an exception is raised.

Example:

    my $a = Assertion->new (lid => 'urn:x-rho:important');
    print "this uniquely (well) identifies the assertion ". hash ($a);

=cut

sub hash {
  my $a = shift;
  die "refuse to hash non canonicalized assertion" unless $a->[CANON];
  use Digest::MD5 qw(md5_hex);
  return md5_hex ($a->[SCOPE], $a->[TYPE], @{$a->[ROLES]}, map { ref ($_) ? join ("", @$_) : $_ } @{$a->[PLAYERS]});  # recompute the hash if necessary
#                                                                           ^^^^^^^^^^^^^^                            # this is a literal value
#                                                                                            ^^                       # this is for a normal identifier
}

=pod

=back

=head2 Assertion Role Retrieval

=over

=item B<is_player>, B<is_x_player>

I<$bool> = is_player   (I<$tm>, I<$assertion>, I<$player_id>, [ I<$role_id> ])

I<$bool> = is_x_player (I<$tm>, I<$assertion>, I<$player_id>, [ I<$role_id> ])

This function returns C<1> if the identifier specified by the C<player_id> parameter plays any role
in the assertion provided as C<assertion> parameter.

If the C<role_id> is provided as third parameter then it must be exactly this role (or any subclass
thereof) that is played. The 'x'-version is using equality instead of 'subclassing' ('x' for
"exact").

=cut

sub is_player {
    my $self = shift;
    my $m    = shift;

#    warn "is_player ".Dumper \@_;
#    warn "caller: ". Dumper [ caller ];
#    foreach (0..0) {
#	warn "  ".join (' ---- ', caller($_));
#    }

    my $p = shift;# or die "must specify valid player: ".Dumper ([ $m ])." and role is ".shift;
#
#    warn "after shifting player '$p'";
    my $r = shift; # may be undefined

    die "must specify a player '$p' for role '$r'" unless $p;

    if ($r) {
	my ($ps, $rs) = ($m->[PLAYERS], $m->[ROLES]);

	for (my $i = 0; $i < @$ps; $i++) {
	    next unless $ps->[$i] eq $p;
	    next unless $self->is_subclass ($rs->[$i], $r);
	    return 1;
	}
    } else {
	return 1 if grep ($_ eq $p, @{$m->[PLAYERS]});
    }
    return 0;
}

sub is_x_player {
    my $self = shift;
    my $m = shift;
    my $p = shift or die "must specify x-player: ".Dumper ([ $m ]);
    my $r = shift; # may be undefined

    if ($r) {
	my ($ps, $rs) = ($m->[PLAYERS], $m->[ROLES]);

	for (my $i = 0; $i < @$ps; $i++) {
	    next unless $ps->[$i] eq $p;
	    next unless $rs->[$i] eq $r;
	    return 1;
	}
    } else {
	return 1 if grep ($_ eq $p, @{$m->[PLAYERS]});
    }
    return 0;
}

=pod

=item B<get_players>, B<get_x_players>

I<@player_ids> = get_players   (I<$tm>, I<$assertion>, I<$role_id>)

I<@player_ids> = get_x_players (I<$tm>, I<$assertion>, I<$role_id>)

This function returns the player(s) for the given role. The "x" version does not honor subclassing.

=cut

sub get_players {
    my $self = shift;
    my $a = shift;
    my $r = shift;
    
    my ($ps, $rs) = ($a->[PLAYERS], $a->[ROLES]);
    
    my @ps;
    for (my $i = 0; $i < @$ps; $i++) {
	next unless $self->is_subclass ($rs->[$i], $r);
	push @ps, $ps->[$i];
    }
    return @ps;
}

sub get_x_players {
    my $self = shift;
    my $a = shift;
    my $r = shift;

    my ($ps, $rs) = ($a->[PLAYERS], $a->[ROLES]);
    
    my @ps;
    for (my $i = 0; $i < @$ps; $i++) {
	next unless $rs->[$i] eq $r;
	push @ps, $ps->[$i];
    }
    return @ps;
}

=pod

=item B<is_role>, B<is_x_role>

I<$bool> = is_role   (I<$tm>, I<$assertion>, I<$role_id>)

I<$bool> = is_x_role (I<$tm>, I<$assertion>, I<$role_id>)

This function returns C<1> if the C<role_id> is a role in the assertion provided. The "x" version of
this function does not honor subclassing.

=cut

sub is_role {
    my $self = shift;
    my $m    = shift;
    my $r    = shift or die "must specify role: ".Dumper ([ $m ]);

    return 1 if grep ($self->is_subclass ($_, $r), @{$m->[ROLES]});
}

sub is_x_role {
    my $self = shift;
    my $m    = shift;
    my $r    = shift or die "must specify role: ".Dumper ([ $m ]);

    return 1 if grep ($_ eq $r, @{$m->[ROLES]});
}

=pod

=item B<get_roles>

I<@role_ids> = @{ get_roles (I<$tm>, I<$assertion>) }

This function extracts a reference to the list of role identifiers.

=cut

sub get_roles {
    my $self = shift;
    my $a = shift;

    return $a->[ROLES];
}


=pod

=back

=head2 Assertion Map Methods

=over

=item B<assert>

I<$tm>->assert (I<@list-of-assertions>)

This method takes a list of assertions, canonicalizes them and then injects them into the map. If
one of the newly added assertions already existed in the map, it will be ignored.

In this process, all assertions will be completed (if fields are missing) and will be canonicalized
(unless they already were). This implies that non-canonicalized assertions will be modified, in that
the role/player lists change.

If an assertion does not have a type, it will default to C<$TM::PSI::THING>. If an assertion does
not have a scope, it defaults to C<$TM::PSI::US>. Any assertion not having an LID will get one.

Examples:

  my $a = Assertion->new (type => 'rumsti');
  $ms->assert ($a);

The method returns a list of all asserted assertions (sic).

=cut

sub assert {
    my $self = shift;
    my ($THING, $US) = @{$self->{usual_suspects}}{'thing', 'us'};

##warn "sub assert $self".ref ($self);

    my $assertions = $self->{assertions};
    foreach (@_) {
	unless ($_->[CANON]) {
	    $_->[KIND]  ||= ASSOC;

	    $_->[TYPE]  ||= $THING;
	    $_->[TYPE]    = $self->internalize ($_->[TYPE] => undef);
	    $_->[SCOPE] ||= $US;
	    $_->[SCOPE]   = $self->internalize ($_->[SCOPE] => undef);

	    $_->[ROLES]   = [ map { $self->internalize ($_ => undef ) } @{$_->[ROLES]} ];
	    $_->[PLAYERS] = [ map { $_ = ref ($_) ? $_ : $self->internalize ($_ => undef) } @{$_->[PLAYERS]}  ];

	    $self->canonicalize ($_);

	    $_->[LID]   ||= hash ($_);                         # the LID is either already there, or it will default to a hash over the canonicalized info
	    $_->[LID]     = $self->internalize ($_->[LID] => undef);
	}
	$assertions->{$_->[LID]} = $_;
    }
    $self->{assertions} = $assertions; ##!! needed for Berkeley DBM recognize changes on deeper levels
    $self->{last_mod} = Time::HiRes::time;
    return @_;
}

=pod

=item B<retrieve>

I<$assertion>  = I<$tm>->retrieve (I<$some_assertion_id>)

I<@assertions> = I<$tm>->retrieve (I<$some_assertion_id>, ...)

This method takes a list of assertion IDs and returns the assertion(s) with the given (subject)
ID(s). If the assertion is not identifiable, C<undef> will be returned in its place. Called in list
context, it will return a list of assertion references.

=cut

sub retrieve {
  my $self = shift;

  if (wantarray) {
      return map { $self->{assertions}->{$_} } @_;
  } else {
      return $self->{assertions}->{$_[0]};
  }
}

=pod

=item B<is_asserted>

I<$bool> = I<$tm>->is_asserted (I<$a>)

This method will return C<1> if the passed-in assertion exists in the store. The assertion will be
canonicalized before checking, but no defaults will be added if parts are missing.

=cut

sub is_asserted {
    my $self  = shift;
    my $a     = shift;

    unless ($a->[CANON]) {
	absolutize   ($self, $a);
	canonicalize ($self, $a);
    }
    return $self->{assertions}->{$self->{baseuri} . hash ($a)};
}

=pod

=item B<retract>

I<$tm>->retract (I<@list_of_assertion_ids>)

This methods expects a list of assertion IDs and will remove the assertions from the map. If an ID
is bogus, it will be ignored.

Only these particular assertions will be deleted. Any topics in these assertions will remain. Use
C<consolidate> to remove unnecessary topics.

=cut

sub retract {
  my $self = shift;

# TODO: does delete $self->{assertions}->{@_} work?
  my $assertions = $self->{assertions};
  map { 
      delete $assertions->{$_} # delete them from the primary store
  } @_; 
  $self->{assertions} = $assertions; ##!! needed for Berkeley DBM recognize changes on deeper levels
  $self->{last_mod} = Time::HiRes::time;
}

=pod

=item B<match>

I<@list> = I<$tm>->match (C<FORALL> or C<EXISTS> [ , I<search-spec>, ... ]);

This method takes a search specification and returns all assertions matching.

If the constant C<FORALL> is used as first parameter, this method returns a list of assertions in
the store following the search specification. If the constant C<EXISTS> is used the method will
return a non-empty value if at least one can be found. The result list contains references to the
assertions themselves, not to copies. You can change the assertions themselves on your own risk
(read: better not do it).

B<NOTE>: C<EXISTS> is not yet implemented.

The search specification is a hash with the same fields as for the constructor of an assertion:

Example:

   $tm->match (FORALL, type    => '...',
                                  scope   => '...,
                                  roles   => [ ...., ....],
                                  players => [.... ]);

Any combination of assertion components can be used, all are optional, with the only constraint that
the number of roles must match that for the players. All involved IDs will be absolutized before
matching.

B<NOTE>: Some combinations will be very fast, while others quite slow. The latter is the case when
there is no special-purpose matcher implemented and the general-purpose one has to be used as a
fallback.

=cut

use constant {
    EXISTS => 1,
    FORALL => 0
    };

# implements 'forall' semantics

our %query_handlers = ('' => 
		       sub { # no params => want all of them
			   my $self   = shift;
			   my $exists = shift;
			   return values %{$self->{assertions}};
		       },

		       'nochar' =>
		       sub {
			   my $self   = shift;
			   my $exists = shift;
			   return
			       grep ($_->[KIND] <= ASSOC,
					   values %{$self->{assertions}});
		       },

		       'char.irole' =>
		       sub {
			   my $self   = shift;
			   my $exists = shift;
			   my $topic  = $_[1];
			   return undef unless $topic;
			   return
			       grep ($self->is_player ($_, $topic) &&                              # TODO: optimize this grep away (getting chars is expensive)
				        NAME <= $_->[KIND] && $_->[KIND] <= OCC,
					   values %{$self->{assertions}});
		       },

		       'lid' => # have unique ID?
		       sub {
			   my $self   = shift;
			   my $lid    = $_[1];
			   return
			       $self->{assertions}->{$lid};
		       },

		       'type' =>
		       sub {
			   my $self   = shift;
			   my $exists = shift;
			   my $type   = $_[0];
			   return 
			       grep ($self->is_subclass ($_->[TYPE], $type),
				     values %{$self->{assertions}});
		       },

		       'iplayer' =>
		       sub {
			   my $self   = shift;
			   my $exists = shift;
			   my $ip     = $_[0];
			   return 
			       grep ($self->is_player ($_, $ip), 
				     values %{$self->{assertions}});
		       },

		       'iplayer.type' =>
		       sub {
			   my $self      = shift;
			   my $exists    = shift;
			   my ($ip, $ty) = @_;
			   return 
			       grep ($self->is_player ($_, $ip)          &&
				     $self->is_subclass ($_->[TYPE], $ty),
				     values %{$self->{assertions}});
		       },

		       'iplayer.irole' =>
		       sub {
			   my $self      = shift;
			   my $exists    = shift;
			   my ($ip, $ir) = @_;
			   return 
			       grep ($self->is_player ($_, $ip, $ir), 
				     values %{$self->{assertions}});
		       },

		       'iplayer.irole.type' =>
		       sub {
			   my $self           = shift;
			   my $exists         = shift;
			   my ($ip, $ir, $ty) = @_;
			   return 
			       grep ($self->is_subclass ($_->[TYPE], $ty) && 
				     $self->is_player ($_, $ip, $ir), 
				     values %{$self->{assertions}});
		       },

		       'irole.type' =>
		       sub {
			   my $self      = shift;
			   my $exists    = shift;
                           my ($ir, $ty) = @_;
			   return
			       grep ($self->is_role ($_, $ir)             &&
				     $self->is_subclass ($_->[TYPE], $ty),
				     values %{$self->{assertions}});
		       },

		       'irole' =>
		       sub {
			   my $self      = shift;
			   my $exists    = shift;
                           my ($ir)      = @_;
			   return
			       grep ($self->is_role ($_, $ir),
				     values %{$self->{assertions}});
		       },

		       'subtype' =>
		       sub {
			   my $self   = shift;
                           my $exists = shift;
			   my $st     = shift;
			   my ($issc, $sub, $sup) = @{$self->{usual_suspects}}{'is-subclass-of', 'subclass', 'superclass'};
			   return
			       grep ( $self->is_x_role     ($_, $sup),
			       grep ( $self->is_x_player   ($_, $st, $sub),
			       grep ( $_->[TYPE] eq $issc,
				      values %{$self->{assertions}})));
		       },

		       'aplayer.arole.brole.type' =>
		       sub {
			   my $self   = shift;
			   my $exists = shift;
                           my ($ap, $ar, $br, $ty) = @_;
			   return
			       grep ( $self->is_role     ($_, $br),
			       grep ( $self->is_player   ($_, $ap, $ar),
			       grep ( $self->is_subclass ($_->[TYPE], $ty),
				      values %{$self->{assertions}})));
		       },

		       'aplayer.arole.bplayer.brole.type' =>
		       sub {
			   my $self  = shift;
			   my $exists = shift; # still ignored :-/
                           my ($ap, $ar, $bp, $br, $ty) = @_;
			   return
			       grep ( $self->is_player ($_, $bp, $br),
			       grep ( $self->is_player ($_, $ap, $ar),
			       grep ( $self->is_subclass ($_->[TYPE], $ty),
				      values %{$self->{assertions}})));
		       },

		       'anyid' =>
		       sub {
			   my $self   = shift;
			   my $exists = shift;
                           my $lid    = shift;
			   return
			       grep (
##				     $self->is_subclass ($_->[TYPE], $lid) ||   # probably not a good idea
				     $_->[TYPE]  eq         $lid            ||   # this seems a bit safer
				     $_->[SCOPE] eq         $lid            ||
				     $self->is_player ($_, $lid)           ||
				     $self->is_role   ($_, $lid)           ,
				     values %{$self->{assertions}});
		       },

		       'allinone' =>
		       sub {
			   my $self     = shift;
			   my $exists   = shift;
			   my $template = Assertion->new (@_);                              # we create an assertion on the fly
#warn "allinone ".Dumper $template;
#			   $self->absolutize   ($template);  
#warn "allinone2".Dumper $template;
			   $self->canonicalize ($template);                                # of course, need to be canonicalized
#warn "allinone3".Dumper $template;

#warn "in store match template ".Dumper $template;
			   my @mads;
			 ASSERTION:
			   foreach my $m (values %{$self->{assertions}}) {                 # arbitrary AsTMa! queries TBD, can be faster as well
			       
			       next if defined $template->[KIND]  && $m->[KIND]  ne $template->[KIND];         # does kind match?
#warn "after kind";
			       next if defined $template->[SCOPE] && $m->[SCOPE] ne $template->[SCOPE];        # does scope match?
#warn "after scope";
			       next if defined $template->[TYPE]  && !$self->is_subclass ($m->[TYPE], $template->[TYPE]);         # does type match?
#warn "after type";
			       
			       my ($rm, $rc) = ($m->[ROLES],   $template->[ROLES]);
			       push @mads, $m and next ASSERTION             if ! @$rc;     # match ok, if we have no roles
#warn "after push roles";
			       next if @$rm != @$rc;                                        # quick check: roles must be of equal length
#warn "after roles";
			       
			       my ($pm, $pc) = ($m->[PLAYERS], $template->[PLAYERS]);
			       push @mads, $m and next ASSERTION             if ! @$pc;     # match ok, if we have no players
			       next if @$pm != @$pc;                                        # quick check: roles and players must be of equal length
#warn "after players";
			       
			       for (my $i = 0; $i < @{$rm}; $i++) {                         # order is canonicalized, would not want to test all permutations
#warn "before role tests : is $rm->[$i] subclass of $rc->[$i]?";
				   next ASSERTION if defined $rc->[$i] && !$self->is_subclass ($rm->[$i], $rc->[$i]);              # go to next assertion if that does not match
#warn "after role ok";
				   next ASSERTION if defined $pc->[$i] && $pm->[$i] ne $pc->[$i];
			       }
#warn "after players  roles";
			       return (1) if $exists;                                       # with exists that's it
			       push @mads, $m;                                              # with forall we do continue to collect
			   }
#warn "we return ".Dumper \@mads;
			   return @mads;                                                    # and return what we got
		       }

		      );

sub match {
    my $self   = shift;
    my $exists = shift;

    my %query = @_;
#warn "store match query".Dumper \%query;
    my @skeys = sort keys %query;                                                       # all fields make up the key
    my $skeys = join ('.', @skeys);

    if (my $handler = $query_handlers{$skeys}) {                                        # there is a constraint and we have a handler
	return &{$handler} ($self, $exists, map { $query{$_} } @skeys);
    } else {                                                                            # otherwise
	&{$query_handlers{'allinone'}} ($self, $exists, %query);                        # we use a generic handler, slow but should do the trick
    }
}

=pod

=back


=head2 Midlets

Midlets are light-weight topics in that their information is quite minimal. One midlet is
represented by an array with two fields:

=over

=item C<ADDRESS>

It contains the B<subject locator> URI, if known, otherwise C<undef>.

=item C<INDICATORS>

This is a reference to a list containing B<subject identifiers>. The list can be empty, no duplicate
removal is attempted.

=back

=cut

use constant {
    ADDRESS    => 0,
    INDICATORS => 1
};

=pod

=head2 Midlet Methods

=over

=item B<internalize>

I<$iid>  = I<$tm>->internalize (I<$some_id>)

I<$iid>  = I<$tm>->internalize (I<$some_id> => I<$some_id>)

I<@iids> = I<$tm>->internalize (I<$some_id> => I<$some_id>, ...)

This method does some trickery when a new topic should be added to the map, depending on how
parameters are passed into it. The general scheme is that pairs of identifiers are passed in.  The
first is usually the internal identifier, the second the subject identifier or the subject
locator. The convention is that subject identifier URIs are passed in as string reference, whereas
subject locator URIs are passed in as strings.

The following cases are covered:

=over

=item C<ID =E<gt> undef>

If the ID is already an absolute URI and contains the C<baseuri> of the map as prefix, then this URI
is used. If the ID is some other URI, then a topic with that URI as subject locator is search in the
map. If such a topic already exists, then nothing special needs to happen.  If no such topic
existed, a new URI, based on the C<baseuri> and a random number will be created.

=item C<ID =E<gt> URI>

Like above, only that the URI is used as subject locator.

=item C<ID =E<gt> \ URI> (reference to string)

Like above, only that the URI is used as another subject identifier.

=item C<undef =E<gt> URI>

Like above, only that the internal identifier has to be (maybe) created.

=item C<undef =E<gt> undef>

A topic with a generated ID will be inserted. Not sure what this is good for.

=back

In any case, the internal identifier(s) of all inserted (or existing) topics are returned.

=cut

my $toplet_ctr = 0;

sub internalize {
    my $self    = shift;
    my $baseuri = $self->{baseuri};

#warn "internalize base: $baseuri";

    my @mids;
    my $mid2iid = $self->{mid2iid};
    while (@_) {
	my ($k, $v) = (shift, shift);                              # assume to get here undef => URI   or   ID => URI   or ID => \ URI   or ID => undef
#warn "internalize $k, $v";
	# make sure that $k contains a mid

	if (defined $k) {
	    if ($k =~ /^$baseuri/) {                               # ha, perfect
		# null                                             # keep it as it is
	    } elsif ($k =~ /^\w+:/) {                              # some other absURL
		if (my $k2 = $self->mids ($k)) {                   # we already had it
		    ($k, $v) = ($k2, $k);
		} else {                                           # it is unknown so far
		    ($k, $v) = ($baseuri.sprintf ("uuid-%010d", $toplet_ctr++), $k);
		}
	    } elsif (my $k2 = $self->mids ($k)) {
		$k = $k2;                                          # then we already have it, maybe under a different mid, take that

	    } else {                                              # this means we have a absURI and it is not from that map
		$k = $baseuri.$k;                                 # but now it is
	    }

	} elsif (my $k2 = $self->mids ($v)) {                      # k is not defined, lets look at v; we already had it
	    $k = $k2;                                              # this will be k then
	} else {                                                   # it is unknown so far
	    $k = $baseuri.sprintf ("uuid-%010d", $toplet_ctr++);   # generate a new one
	}

	push @mids, $k;

#warn "internalizing '$k' '$v'";

	# now see that we have an entry in the mid2iid table
	$mid2iid->{$k} ||= [ undef, [] ];
	my $kentry = $mid2iid->{$k};                               # keep this as a shortcut

	if ($v) {
	    if (ref($v)) {                                         # being a reference means that we have a subject indication
		push @{$kentry->[TM->INDICATORS]}, $$v;                # append it to the list
	    } elsif ($kentry->[TM->ADDRESS]) {                         # this is a subject address and, oh, there is already a subject address, not good
		die "duplicate subject address '$v' for '$k'" unless $v eq $kentry->[TM->ADDRESS];
	    } else {                                               # everything is fine, we can set it
		$kentry->[TM->ADDRESS] = $v;                 
	    }
	}
    }
    $self->{mid2iid}  = $mid2iid; #!! needed for Berkeley DBM recognize changes on deeper levels
    $self->{last_mod} = Time::HiRes::time;
    return wantarray ? @mids : $mids[0];
}

=pod

=item B<mids>

I<$mid>  = I<$tm>->mids (I<$some_id>)

I<@mids> = I<$tm>->mids (I<$some_id>, ...)

This function tries to build absolute versions of the identifiers passed in. C<undef> will be
returned if no such can be found. Can be used in scalar and list context.

If the passed in identifier is a relative URI, so it is made absolute by prefixing it with the map
C<baseuri> and then we look for a topic with that internal identifier.

If the passed in identifier is an absolute URI, where the C<baseuri> is a prefix, then that URI will
be used as internal identifier to look for a topic.

If the passed in identifier is an absolute URI, where the C<baseuri> is B<NOT> a prefix, then that
URI will be used as subject locator and such a topic will be looked for.

If the passed in identifier is a reference to an absolute URI, then that URI will be used as subject
identifier and such a topic will be looked for.

=cut

sub mids {
    my $self    = shift;
    my $baseuri = $self->{baseuri};
    my @ks;
  MID:
    foreach my $k (@_) {
	if (! defined $k) {                                            # someone put in undef
	    push @ks, undef;
	} elsif (ref ($k)) {                                           # would be subject indicator ref
	    my $kk = $$k;

	    foreach my $k2 (keys %{$self->{mid2iid}}) {
		if (grep ($_ eq $kk, 
			  @{$self->{mid2iid}->{$k2}->[TM->INDICATORS]}
			  )) {
		    push @ks, $k2;
		    next MID;
		}
	    }
	    push @ks, undef;

	} elsif ($k =~ /^$baseuri/) {                                  # we already have something which looks like a mid
	    push @ks, $self->{mid2iid}->{$k} ? $k : undef;

	} elsif ($k =~ /^\w+:/) {                                      # must be some other uri, must be subject address
	    no warnings;
	    my @k2 = grep ($self->{mid2iid}->{$_}->[TM->ADDRESS] eq $k, keys %{$self->{mid2iid}});
	    push @ks,  @k2 ? $k2[0] : undef;

	} else {                                                       # only a string, like 'aaa'
	    my $k2 = $baseuri.$k;                                      # make it absolute, and...
	    push @ks, $self->{mid2iid}->{$k2} ? $k2 : undef;          # see whether there is someting
	}
    }
#warn "mids returning, in".Dumper (\@_, \@ks);
#warn "self ".Dumper $self if $_[0] eq 'tm:return';
    return wantarray ? @ks : $ks[0];
}

=pod

=item B<externalize>

I<$tm>->externalize (I<$some_id>, ...)

This function simply deletes the topic entry for a given internal identifier(s). See C<mids> to find
these. The function returns all deleted topic entries.

B<NOTE>: Assertions in which this topic is involved will not be removed. Use C<consolidate> to clean
up all assertion where non-existing topics still exist.

=cut

sub externalize {
    my $self = shift;

    my $mid2iid = $self->{mid2iid};
    my @doomed = map { delete $mid2iid->{$_} } @_;
    $self->{mid2iid} = $mid2iid; ## !! needed for Berkeley DBM recognize changes on deeper levels
    $self->{last_mod} = Time::HiRes::time;
    return @doomed;
}

=pod

=item B<midlets>

I<@mids> = I<$tm>->midlets

This function returns all the things (actually their ids) known in the map.

=cut

sub midlets {
    my $self = shift;
    return keys %{$self->{mid2iid}};
}

=pod

=item B<midlet>

I<$t>  = I<$tm>->midlet (I<$mid>)

I<@ts> = I<$tm>->midlet (I<$mid>, ....)

This function returns a reference to a topic structure. That includes a subject address, if
available and a list (reference) for the optional subject indicators.

Can be used in scalar and list context.

=cut

sub midlet {
    my $self = shift;

    if (wantarray) {
	return @{$self->{mid2iid}}{$self->mids (@_)};
    } else {
	return $self->{mid2iid}->{$self->mids ($_[0])};
    }
}

=pod

=back

=head2 Taxonomics and Subsumption

The following methods provide useful basic, ontological functionality around subclassing (also
transitive) between classes and instance/type relationships.

Deriving classes may want to consider to overload/redefine these methods better suitable for their
representation of the a map. Saying this, the methods below are not optimized for speed.

B<NOTE>: There are NO subclasses of the C<thing>. But everything is an instance of C<thing>.

B<NOTE>: See L<TM::PSI> for predefined things.

=over

=item B<is_subclass>

I<$bool> = I<$tm>->is_subclass (I<$superclass_id>, I<$subclass_id>)

This function returns C<1> if the first parameter is a (transitive) superclass of the second,
i.e. there is an assertion of type I<is-subclass-of> in the context map. It also returns C<1> if the
superclass is a $TM::PSI::THING or if subclass and superclass are the same (reflexive).

TODO: memoize

=cut

sub is_subclass {
    my $self  = shift;
    my $class  = shift;
    my $super  = shift;

    my ($THING, $SUBCLASSES, $SUPERCLASS) = @{$self->{usual_suspects}}{'thing', 'is-subclass-of', 'superclass'};
#warn Dumper $self unless $THING;

#warn "is_subclass?: class $class   super $super , thing $THING, $SUBCLASSES, $SUPERCLASS";
    return 1 if $super eq $THING;                                    # everything is a topic
#warn "was not a thing, so continue";
    return 0 if $class eq $THING;                                    # a thing cannot be a subclass
    return 1 if $class eq $super;                                    # and warn "they are the same!";
#warn "both are not equal, so continue";
    return 0 if $class eq $SUBCLASSES;                               # that cannot be a subclass
#warn "subclass was not a thing, so continue";
#warn "checking the hard way";
    {                                                                # next strategy: try to find this EXACTLY in the map as assertion
##warn "XXXX $class '$super'";
	return 1 if $self->is_asserted (Assertion->new (scope   => 'us',
							type    => 'is-subclass-of', 
							roles   => [ 'subclass', 'superclass' ],
							players => [ $class, $super ])
					);
    }
#warn "trying to do indirect";
    { # if we still do not have a decision, we will check all super types of $class and see (recursively) whether we can establish is-subclass-of
#warn "YYY";
#
#  my @mx = match ($self, FORALL,
#		  subtype => $class,
#		  );
#warn " found superclasses ".Dumper \@mx;
#
#my @mp = map { $self->get_player ($_, mids ($self, 'superclass')) } @mx;#
#
#warn " found players superclass ".Dumper \@mp;
#
#my $r = grep (is_subclass ($self, $_, $super), @mp);
#
#warn "is one a indirect subclass of $super?: $r";
#
#return $r;

	return 1 if grep ($self->is_subclass ($_, $super),           # check all of the intermediate type whether there is a transitive relation
			  map { $self->get_x_players ($_, $SUPERCLASS) }  # find the superclass player there => intermediate type
			  $self->match (FORALL,
					 subtype => $class,
					 )
			  );
    }
#warn "no subclass";
    return 0;                                                        # ok, we give up now
}

=pod

=item B<subclasses>, B<subclassesT>

I<@lids> = I<$tm>->subclasses  (I<$lid>)

I<@lids> = I<$tm>->subclassesT (I<$lid>)

C<subclasses> returns all B<direct> subclasses of the thing identified by C<$lid>. If the thing does
not exist, the list will be empty. C<subclassesT> is a variant which honors the transitive
subclassing (so if A is a subclass of B and B is a subclass of C, then A is also a subclass of C).

=cut

sub subclasses {
    my $self = shift;
    my $lid  = shift;

    my ($issc, $sub, $sup) = @{$self->{usual_suspects}}{'is-subclass-of', 'subclass', 'superclass'};
    return map { $_->[PLAYERS]->[0] } $self->match (TM->FORALL, type => $issc, arole => $sup, aplayer => $lid, brole => $sub);
}

sub subclassesT {
    my $self = shift;
    my $lid  = shift;

    my @sc = $self->subclasses ($lid);
    my %dup;
    return map { $dup{$_}++ ? () : $_ } ($lid), (@sc), (map { $self->subclassesT ($_) } @sc);
}

=pod

=item B<superclasses>, B<superclassesT>

I<@lids> = I<$tm>->superclasses  (I<$lid>)

I<@lids> = I<$tm>->superclassesT (I<$lid>)

The method C<superclasses> returns all direct superclasses of the thing identified by C<$lid>. If
the thing does not exist, the list will be empty. C<superclassesT> is a variant which honors
transitive subclassing.

=cut

sub superclasses {
    my $self = shift;
    my $lid  = shift;

    my ($issc, $sub, $sup) = @{$self->{usual_suspects}}{'is-subclass-of', 'subclass', 'superclass'};
    return map { $_->[PLAYERS]->[1] } $self->match (TM->FORALL, type => $issc, arole => $sub, aplayer => $lid, brole => $sup);
}

sub superclassesT {
    my $self = shift;
    my $lid  = shift;

    my @sc = $self->superclasses ($lid);
    my %dup;
    return map { $dup{$_}++ ? () : $_ } ($lid), (@sc), (map { $self->superclassesT ($_) } @sc); # laziness equals recursion
}

=pod

=item B<types>, B<typesT>

I<@lids> = I<$tm>->types  (I<$lid>)

I<@lids> = I<$tm>->typesT (I<$lid>)

The method C<types> returns all direct classes of the thing identified by C<$lid>. If the thing does
not exist, the list will be empty. C<typesT> is a variant which honors transitive subclassing (so if
I<a> is an instance of type I<A> and I<A> is a subclass of I<B>, then I<a> is also an instance of
I<B>).

=cut

sub types {
    my $self = shift;
    my $lid  = shift;

    my ($isa, $inst, $class) = @{$self->{usual_suspects}}{'isa', 'instance', 'class'};
    return (map { defined $_ ? $_->[TYPE ] : () } $self->match (TM->FORALL, lid => $lid)),
           (map { $_->[PLAYERS]->[0] }            $self->match (TM->FORALL, type => $isa, arole => $inst, aplayer => $lid, brole => $class));
}

sub typesT {
    my $self = shift;
    my $lid  = shift;

    my @sc = $self->types ($lid);
    my %dup;
    return map { $dup{$_}++ ? () : $_ } (@sc), (map { $self->superclassesT ($_) } @sc);
}


=pod

=item B<instances>, B<instancesT>

I<@lids> = I<$tm>->instances  (I<$lid>)

I<@lids> = I<$tm>->instancesT (I<$lid>)

These methods return the direct (C<instances>) and also indirect (C<instancesT>) instances of the
thing identified by C<$lid>.

=cut

sub instances {
    my $self = shift;
    my $lid  = shift;

    my ($isa, $inst, $class, $thing) = @{$self->{usual_suspects}}{'isa', 'instance', 'class', 'thing'};
    return () if $lid eq $thing;                                             # a thing does not have direct instances? or everything?
    return  (map { $_->[LID ] }         $self->match (TM->FORALL, type => $lid)),
            (map { $_->[PLAYERS]->[1] } $self->match (TM->FORALL, type => $isa, arole => $class, aplayer => $lid, brole => $inst))
	;
}

sub instancesT {
    my $self = shift;
    my $lid  = shift;

    my ($thing) = @{$self->{usual_suspects}}{'thing'};

    return $self->midlets if $lid eq $thing;
    return map { $self->instances ($_) }   $self->subclassesT ($lid);
}

=pod

=item B<is_a>

I<$tm>->is_a (I<$something_lid>, I<$class_lid>)

This method returns C<1> if the thing referenced by the first parameter is an instance of the class
referenced by the second. The method honors transitive subclassing.

=cut

sub is_a {
    my $self    = shift;
    my $thingie = shift;
    my $type    = shift;                                                         # ok, what class are looking at?

    my ($isa, $inst, $class, $thing) = @{$self->{usual_suspects}}{'isa', 'instance', 'class', 'thing'};

#warn "isa thingie $thingie class $type";

    return 1 if $type eq $thing and                                              # is the class == 'thing' and
                $self->{mid2iid}->{$thingie};                                    # and does the thingie exist?

    my ($m) = $self->match (TM->FORALL, lid => $thingie);
    return 1 if $m and                                                           # is it an assertion ? and...
	        $self->is_subclass ($m->[TYPE], $type);                          # is the assertion type a subclass?

    return 1 if grep ($self->is_subclass ($_, $type),                            # check all of the intermediate type whether there is a transitive relation
		         map { $self->get_players ($_, $class) }                 # find the class player there => intermediate type
		             $self->match (TM->FORALL,
					   iplayer => $thingie,
					   irole   => $inst,
					   type    => $isa)
		      );
    return 0;
}

=pod

=back

=head3 Filters

Quite often one needs to walk through a list of things to determine whether they are instances (or
types, subtypes or supertypes) of some concept. This list of functions lets you do that: you pass in
a list and the function behaves as filter.

=over

=item B<are_instances>

I<@ids> = I<$tm>->are_instances (I<$class_id>, I<@list_of_ids>)

Returns all those ids where the topic is an instance of the class provided.

=cut

sub are_instances {
    my $self  = shift;
    my $class = shift;                                                           # ok, what class are we looking at?

    my ($THING, $ISA, $CLASS, $INSTANCE) = @{$self->{usual_suspects}}{'thing', 'isa', 'class', 'instance'};

    my @rs;
    foreach my $thing (@{$_[0]}) {                                               # we work through all the things we got
#warn "checking $thing";
	push @rs, $thing and next                                                # we happily take one if
	    if $class eq $THING and                                              #     is the class = 'thing' ? and
               $self->midlet ($thing);                                           #     then does the thing exist in the map ?

	my $m = $self->retrieve ($thing);
	push @rs, $thing and next                                                # we happily take one if
	    if $m and                                                            #    it is an assertion ? and...
	       ($class eq $THING                                                 #    either it is the class a THING (we did not explicitly store _that_)
                or
                $self->is_subclass ($m->[TYPE], $class)                          #    or is the assertion type a subclass?
	        );

	push @rs, $thing and next                                                # we happily take one if
	    if grep ($self->is_subclass ($_, $class),                            # finall we check all of the intermediate type whether there is a transitive relation
		     map { $self->get_players ($_, $CLASS) }                     # then we find the 'class' value
                           $self->match (FORALL,
					  iplayer => $thing,                     # first we find all classes of the thing
					  irole   => $INSTANCE,
					  type    => $ISA));

        # nothing                                                                # otherwise we do not push
    }
    return \@rs;
}

=pod

=item B<are_types> (Warning: placeholder only)

I<@ids> = I<$tm>->are_types (I<$instance_id>, I<@list_of_ids>)

Returns all those ids where the topic is a type of the instance provided.

=cut

sub are_types {
    die;
}

=pod

=item B<are_supertypes> (Warning: placeholder only)

I<@ids> = I<$tm>->are_supertypes (I<$class_id>, I<@list_of_ids>)

Returns all those ids where the topic is a supertype of the class provided.

=cut

sub are_supertypes {
    die;
}

=pod

=item B<are_subtypes> (Warning: placeholder only)

I<@ids> = I<$tm>->are_subtypes (I<$class_id>, I<@list_of_ids>)

Returns all those ids where the topic is a subtype of the class provided.

=cut

sub are_subtypes {
    die;
}

=pod

=back

=head2 Reification

=over

=item B<reified_by> (experimental)

Provided with an identifier, this method returns the subject locator. It returns C<undef> if there
is no such topic or no locator.

TODO: list context

TODO: name sucks

WARNING: this function may go away

=cut

sub reified_by {
    my $self = shift;
    my $mid  = shift;

    return $self->{mid2iid}->{$mid} ? $self->{mid2iid}->{$mid}->[TM->ADDRESS] : undef;
}

=pod

=item B<reifies> (experimental)

WARNING: this function may go away

=cut

sub reifies {
    my $self = shift;
    my $url  = shift;

    my $mid2iid = $self->{mid2iid};                                                               # shortcut
    my $s;                                                                                        # temp
    return grep (($s = $mid2iid->{$_}->[TM->ADDRESS]) && ($s eq $url), keys %{$mid2iid});
}

=pod

=back

=head2 Variants (aka "The Warts")

No comment.

=over

=item B<variants>

I<$tm>->variants (I<$id>, I<$variant>)

I<$tm>->variants (I<$id>)

With this method you can get/set a variant tree for B<any> topic. According to the standard only
basenames (aka topic names) can have variants, but, hey, this is such an ugly beast (I am
digressing). According to this data model you can have variants for B<all> toplets/maplets. You only
need their id.

The structure is like this:

  $VAR1 = {
    'tm:param1' => {
      'variants' => {
        'tm:param3' => {
          'variants' => undef,
          'value' => 'name for param3'
        }
      },
      'value' => 'name for param1'
    },
    'tm:param2' => {
      'variants' => undef,
      'value' => 'name for param2'
    }
  };

The parameters are the keys (there can only be one, which is a useful, cough, restriction of the
standard) and the data is the value. Obviously, one key value (i.e. parameter) can only exists once.

Caveat: This is not very well tested (read: not tested at all).

=cut

sub variants {
    my $self = shift;
    my $id   = shift;
    my $var  = shift;

    $self->{last_mod} = Time::HiRes::time if $var;
    return $var ? $self->{variants}->{$id} = $var : $self->{variants}->{$id};
}

=pod

=back

=head1 SEE ALSO

L<TM::PSI>, L<TM::Resource>


=head1 COPYRIGHT AND LICENSE

Copyright 200[1-6] by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.

=cut

our $VERSION  = '1.18';
our $REVISION = '$Id: TM.pm,v 1.29 2006/09/23 01:22:03 rho Exp $';


1;

__END__

^==cut

^==item B<filter>

I<$ms>->filter (I<$constraint>, ...)

This function takes a list of constraints and removed all component from the store which violate
the constraint.

^==cut

sub filter {
    my $self = shift;
    die "not yet implemented";
}

^==pod

^==item B<path>

I<$res> = I<$ms>->path (I<$pe>)

I<$res> = I<$ms>->path (I<$pe>, I<$val>)

This method takes a path expression (see L<TM::QL::Path>) and computes
the result. If an optional value is included then this value will be
set for the path expression given.

The method may either give back a list (reference) of simple results or
a ???? @@@

^==cut

sub path {
    my $self = shift;
    my $pe   = shift;

    return [1, 2, 3];
}

^==pod



use TM::PSI;

#-- indices support ----------------------------------------------------------------------------

sub _update_index {
  my $store = shift;
  my $index = shift;
  my $key   = shift;
  my $minus = shift;
  my $plus  = shift;

  if ($store->{indices}->{$index}) {    
#warn "fiddling with $index, minus $minus->[ID], plus $plus->[ID]";
    $store->{stats}->{indices}->{$index}->{write}++;

    push @{$store->{indices}->{$index}->{$key}}, ();    # make sure we have a list ref here
    my $l = $store->{indices}->{$index}->{$key};

    if ($minus) {
      for (my $i = 0; $i <= $#$l; $i++) {
#warn "checking $i, $l->[$i] against $minus";
	if ($l->[$i] == $minus) {
#warn "before list ".Dumper $l;
	  splice (@$l, $i, 1);
#warn "after list ".Dumper $l;
	  last;
	}
      }
    }
    push @$l, $plus if $plus;
##warn "leaving index ".Dumper $store->{indices}->{$index};
  }
}


sub _id_toplet_by_sid {
  my $store = shift;
  my $tid   = shift;

##warn "_id_toplet_by_sid '$tid' ";

  if ($store->{indices}->{subjects}) {                     # we use the index
#warn Dumper $store->{indices}->{subjects};
    $store->{stats}->{indices}->{subjects}->{read}++;
    return $store->{indices}->{subjects}->{$tid} ? $store->{indices}->{subjects}->{$tid}->[0] : undef;          # maybe there, maybe not
  } else {
    die "Cannot use index: subjects";
  }
}

sub _id_toplet_by_ind {
  my $store = shift;
  my $tid   = shift;

  if ($store->{indices}->{indicators}) {
    $store->{stats}->{indices}->{indicators}->{read}++;
    return $store->{indices}->{indicators}->{$tid} ? $store->{indices}->{indicators}->{$tid}->[0] : undef;     # maybe there, maybe not
  } else {
    die "Cannot use index: indicators";
  }
}

 =pod

 =item B<toplets>

I<@ts> = I<$ms>->toplets (I<$template>)

This method takes the template as search specification for a toplet
and returns all toplet ids which toplets match the template.

FIXME DOCS!!!B<NOTE>: Not implemented completely, returns currently all topics.

 =cut

sub toplets {
  my $self = shift;
  my $template = shift;

  unless ( ref($template) eq 'Toplet') {
    return keys %{$self->{toplets}};
  } else {
    my @ts;
  SKIP:
    foreach my $t (values %{$self->{toplets}}) {
##warn "checking ".$t->id;
      next if defined $template->[ID]  and $template->[ID] ne $t->[ID];
##warn "still after id";
      next if defined $template->[SID] and $template->[SID] ne $t->[SID];
##warn "still after sid";
# all chars in the template must be part of the topic
    CHARS:
      foreach my $ct (@{$template->[CHARS]}) {
##warn "checking char ".Dumper $ct;
	foreach my $c (@{$t->[CHARS]}) {
##warn "  against char ".Dumper $c;
	  next CHARS if (!defined $ct->[SCOPE] || $ct->[SCOPE] eq $c->[SCOPE]) &&
	                (!defined $ct->[TYPE]  || $ct->[TYPE]  eq $c->[TYPE])  &&
			(!defined $ct->[KIND]  || $ct->[KIND]  eq $c->[KIND])  &&
			(!defined $ct->[VALUE] || $c->[VALUE] =~ qr|$ct->[VALUE]|);
	}
	next SKIP;
      }
      push @ts, $t->[ID];
    }
    return @ts;
  }
}

 =pod

 =back

 =head3 Maplets, Assert and Retrieve

 =over

 =item B<assert_maplet>

I<$ms>->assert_maplet (I<$m>, ...)

This method asserts that the provided list of maplets is in the
store.

B<Note>: There is NO merging triggered and there is no check
that the objects are really maplets. Paranoia lives elsewhere.

 =cut

sub assert_maplet {
  my $self = shift;

  push @{$self->{maplets}}, @_;

#
#
#  my $m    = shift;
#
#  $self->assert_toplet (id => $m->type);
#  $self->assert_toplet (id => $m->scope);
#  my ($rs, $ps) = ($m->roles, $m->players);
#  for (my $i = 0; $i < @$rs; $i++) {
#    $self->assert_toplet (id => $rs->[$i]);
#    $self->assert_toplet (id => $ps->[$i]);
#  }
}

 =pod

 =item B<retract_maplet>

I<$ms>->retract_maplet (I<$m>, ...)

This method expects a list of maplets which are supposed to be
removed from the store. It is assumed that every maplet passed
in is currently in the store (otherwise the algorithm used -
symmetric difference - does not work).

 =cut

sub retract_maplet {
  my $self = shift;

# have two lists:  @{{$self->{maplets}}}, @_
# the first is usually much longer than the second

  my $l = $self->{maplets};

  for (my $i=0; $i <= $#$l;) {
    if (grep ($l->[$i] == $_, @_)) {
      splice (@$l, $i, 1);
    } else {
      $i++;
    }
  }

# Alternative would be to shorten @_, this performs better if second list
# would be longer


#-- algo below is fast, but ruins the references as keys !!
#-- way-out is to use Tie::HashRef, but that makes the algo slower than mine
#  # to my amazement this generic algorithm is still faster than one which
#  # uses splice to get rid of the unwanted entries
#
#  # ASSUMPTION: all elements from $_ are also elements in the maplet list !!!
#  my @difference = ();
#  my %count = ();
#  foreach my $element (@{$self->{maplets}}, @_) { $count{$element}++ }
#  foreach my $element (keys %count) {
#    push @difference, $element if $count{$element} <= 1;
#  }
#  @{$self->{maplets}} = @difference;
}

 =pod

 =pod

 =back

 =head3 Consistency

 =over

sub _scan_merge {
  my $store = shift;
  my $cons  = shift; # consistency requirements

  my $trans = {}; # hash of tid -> tid translations

##warn "store merge start ".Dumper $store;

##warn "todo merge ".Dumper ($t); #. ", rest: ". Dumper \@_;

##warn "before loop subjects:".Dumper [ keys %{$store->{subjects}} ];

#-- F.5.2.1 ---------------------------------------------------------------------------------------------------------------------
  if (grep ($_ eq 'Subject_based_Merging', @{$cons->{merge}})) {         # check F.5.2.1 (share both the same subject identifier)

    if ($store->{indices}->{subjects}) {                                 # we have kept track on our subjects
      foreach my $l (grep (scalar @$_ > 1,                               # find those lists which have more than one element
			   values %{$store->{indices}->{subjects}}       # these are all list references
			  )) {
#warn "subject collision ".Dumper $l;
	while (@$l > 1) {
#warn "trying reduce ".Dumper $l;
          if ($l->[0] == $l->[1]) {
	    shift @$l;                                                   # if we have duplicates, get rid of them
	  } else {
	    $trans->{$l->[1]->[ID]} = $l->[0]->[ID];                     # memorize this event for later
	    _do_merge ($store, $l->[0], $l->[1]);
	  }
	}
      }
    } else {
      die "Cannot merge without a subject index";
    }
  }

#-- F.5.2.3 ---------------------------------------------------------------------------------------------------------------------
  if ($store->{indices}->{indicators}) {                               # check F.5.2.3 (share at least on URI in references)
    foreach my $l (grep (scalar @$_ > 1,                               # find those lists which have more than one element
			 values %{$store->{indices}->{indicators}}     # these are all list references
			)) {
#warn "indicator collision ".Dumper $l;
      while (@$l > 1) {
#warn "trying reduce ".Dumper $l;
        if ($l->[0] == $l->[1]) {
	  shift @$l;                                                   # if we have duplicates, get rid of them
	} else {                                                       # are different
	  $trans->{$l->[1]->[ID]} = $l->[0]->[ID];                     # memorize this event for later
          _do_merge ($store, $l->[0], $l->[1]);
	}
      }
    }
  } else {
    die "Cannot merge without a indicator index";
  }


  return $trans;

  # other merging here
}


sub _do_merge {
  my $store = shift;
  my $t     = shift;
  my $s     = shift;

warn "do merge of ".$s->[ID]. " into ".$t->[ID];
  return if $s == $t;                                                             # same topics, no need to work here
#warn "different topics";

#warn "fixing subjects ".Dumper $store->{indices}->{subjects};

  # take care about the SID
  if ($s->[SID] && $t->[SID]) {          # both had it defined
    die "to-be-merged topics have two different subject identifiers '$s->[SID]' and '$t->[SID]'"
      if $s->[SID] ne $t->[SID];
    _update_index ($store, 'subjects', $s->[SID], $s, undef);
  } elsif ($s->[SID]) {                                                           # s had, but not t
    $t->[SID] = $s->[SID];
    _update_index ($store, 'subjects', $s->[SID], $s, $t);
  } elsif ($t->[SID]) {                                                           # t has, but there was none for s, leave it
  } else {                                                                        # none had something, leave it
  }

#warn "fixed subjects ".Dumper $store->{indices}->{subjects};
#warn "fixing char/indis ".Dumper ($store->{indices}->{characteristics}, $store->{indices}->{indicators});

  # take care about the characteristics, subject indicators
  foreach my $ch (@{$s->[CHARS]}) {
    push @{$t->[CHARS]}, $ch;                                                     # put it into t
#warn "fixing char at $ch->[HASH]: ".Dumper ($store->{indices}->{characteristics}->{$ch->[HASH]});
    _update_index ($store, 'characteristics', $ch->[HASH],  $s, $t);
#warn "fixed?? char at $ch->[HASH]: ".Dumper ($store->{indices}->{characteristics}->{$ch->[HASH]});

#    if ($ch->[KIND] == TM::Maplet::KIND_SIN) {

#warn "fixing indis at $ch->[VALUE]: ".Dumper ($store->{indices}->{indicators}->{$ch->[VALUE]});
    _update_index ($store, 'indicators',      $ch->[VALUE], $s, $t) if $ch->[KIND] == TM::Maplet::KIND_SIN;
#warn "fixed?? indis at $ch->[VALUE]: ".Dumper ($store->{indices}->{indicators}->{$ch->[VALUE]});

#    }

  }
#warn "fixed char/indi ".Dumper ($store->{indices}->{characteristics}, $store->{indices}->{indicators});

  $store->retract_toplet ($s);                                                    # get rid of merged-in topic
#warn "removed $s->[ID] ".Dumper $store;
}




 =pod

 =back

 =head3 Templates, Constraints and Queries

 =over

 =item B<assert_template>

I<$ms>->assert_template (I<$template>, ...)

This method takes a list of templates as parameter and makes sure they
is part of the store afterwards. Templates are stored according to
their association type. Existing templates are simply overwritten.

 =cut

sub assert_template {
  my $self = shift;
 
  map { $self->{'templates'}->{$_->[TYPE]} = $_ } @_;
}

 =pod

 =item B<assert_constraint>

I<$ms>->assert_constraint (I<$constraint>, ...)

This method takes a list of constraints as parameter and makes sure
they is part of the store afterwards. At the moment no constraint
check is done whether the list of constraints in a store are
consistent or not. There is not even a check whether the objects are
actually constraints or not.

 =cut

sub assert_constraint {
    my $self = shift;

    push @{$self->{'maplets*'}}, @_
}

 =pod

 =item B<assert_function>

I<$ms>->assert_function (I<$name>, I<$function>)

This function stores a function under a particular name into the store.
There is no check whether another function with the same name already
had existed.

 =cut

sub assert_function {
    my $self = shift;
    my $n    = shift;
    my $f    = shift;

    $self->{'functions'}->{$n} = $f;
}

 =pod

 =back

 =head1 SEE ALSO

L<TM::Overview>

 =head1 AUTHOR

Copyright 200[34], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.
http://www.perl.com/perl/misc/Artistic.html

 =cut

1;

__END__



    my $cond = shift;
    my $quan = shift || 'exists';  # if exists, then we only check yes or no, if forall we collect all
    
#    $log->debug ("condition: ".Dumper $cond);

    return $self->{maplets} unless $cond;

    if ($cond->scope        eq 'pxtm-universal-scope'   &&
	$cond->type         eq 'xtm-psi-class-instance' && 
	$cond->players->[0] eq 'xtm-psi-topic') {
#	$log->debug ("    special case: xtm-psi-class-instance with xtm-psi-topic");
	if ($self->{toplets}->{$cond->players->[1]}) {                  # the other topic is the instance of a topic, well, yes
	    return [ 1 ] if $quan eq 'exists';                           # shortcut
	    use Storable qw(dclone);
	    my $m2 = dclone ($cond);                                     # the condition is a good answer
	    return [ $m2 ];
	} else {
	    return [];
	}
    } else {                                                             # general purpose assoc
	my @mads;
      MAPLET:
	foreach my $m (@{$self->{maplets}}) {                            # arbitrary AsTMa! queries TBD, can be faster as well
#	    $log->debug ("    testing against: ".Dumper $m);
	    
	    next unless $m->scope eq $cond->scope;
	    next unless $m->type  eq $cond->type;
	    my ($rm, $rc) = ($m->roles,   $cond->roles);
	    my ($pm, $pc) = ($m->players, $cond->players);
	    for (my $i = 0; $i < @{$rm} && $i < @{$rc}; $i++) {
		next MAPLET unless $rm->[$i] eq $rc->[$i];
		next MAPLET unless $pm->[$i] eq $pc->[$i];
	    }
#	    $log->debug ("    testing successful");
	    # this survived the tests
	    return [ 1 ] if $quan eq 'exists';                           # with exists that's it
	    push @mads, $m;                                              # with forall we collect
	}
	return [ @mads ];
    }




 =item B<match>

 =cut

sub match {
    my $self     = shift;       # one store
    my $c        = shift;       # one constraint
    my $mode     = shift;       # validate, match (filter)
    my $bindings = shift || {}; # optional hash ref to var => value

    my $ctx = {                                                                   # this context will be handed down
	store         => $self,                                                   #     remains constant throughout
	constraint    => $c,                                                      #     remains constant throughout
	};

    if ($mode eq 'validate') {
	$ctx->{exploit} = sub { };                                                # this will be called in case of a match
    } else {
	$ctx->{exploit} = sub { push @{$ctx->{results}}, @_; };
    }

    my $mission;
    eval {
	$mission = _match_constraint ($ctx,
				      { bindings => $bindings,
					params   => [ $c ],
					style    => $mode eq 'validate' ? 'satisfy_one' : 'generate_all'});
	
#	$log->debug ("final mission statement ".Dumper ($mission));
    }; 
#    $log->debug ("toplevel and had exception? '$@'");

    if ($@ =~ /unsatisfyable/) {
	return undef;
    } elsif ($@) {
	die $@;                                                # reraise exception
    }

    if ($mode eq 'validate') {
	return $mission;
    } else {
	return $ctx->{results};
    }
}

sub _match_constraint {
    my $ctx   = shift; # context
    my $m     = shift; # current mission
    my @mss   = @_;    # more missions after that

    my $c     = $m->{params}->[0]; # one constraint

##warn "_match_constraint \n-- constraint -- ".$c; #.Dumper ($c);
##warn "$c ".$m->{style};

    if ($c->isa ('TM::ExistsClause')) {
	if ($c->modifier eq 'suggested') {                                        # suggested will always match
	    $log->error_die ("'suggested' constraint should not be used at validation");
	} elsif ($c->modifier eq 'derived') {                                     # derived should not be used
	    $log->error_die ("'derived' constraint should not be used at validation");
	} else {                                                                  # must be mandated now

	    # this can be set globally in ctx as every pattern will define this from scratch
	    unshift @{$ctx->{pattern}}, {                                         # information for THIS particular pattern
                                       pattern_variable => $c->variable,
				       matched_maplets  => [],
				       cstore           => $c->pattern->store
				       };

	    my $mission = _match_cstore ($ctx, 	                                  # try to match this against the store
					 { bindings => $m->{bindings},
					   params   => [ $ctx->{pattern}->[0]->{cstore}->{maplets}, 0 ],
					   style    => (@mss ? 'generate_all' : $m->{style}), },
					 @mss);
	    shift @{$ctx->{pattern}};                                             # throw info for this pattern away once we returned
	    return $mission;
	}

    } elsif ($c->isa ('TM::ForAllClause')) {
	my $e = TM::ExistsClause->new (pattern  => $c->pattern,
				       variable => $c->variable,
				       modifier => 'mandated');

	# result is not so relevant, or
	_match_constraint ($ctx,
			   { bindings => $m->{bindings},
			     params   => [ $e ],
			     style    => 'generate_all',
			     },
			   { function => \&_match_constraint,
			     params   => [ $c->constraint ],
			     style    => $m->{style},
			    },
			   @mss);
	return $m; # mission completed

    } elsif ($c->isa ('TM::AndConstraint')) {                                   # here I know that left cannot be forall [] => ...
	return _match_constraint ($ctx, 
				  { bindings => $m->{bindings},
				    params   => [ $c->left ],
				    style    => 'generate_all',
				    },
				  { function => \&_match_constraint,            # before satisfying right....
				    params   => [ $c->right ],
#				    style    => $m->{style},
				    },
				  @mss,
				  );

    } elsif ($c->isa ('TM::OrConstraint')) {
	my $mission = _match_constraint ($ctx,
					 { bindings => $m->{bindings},
					   params   => [ $c->left ],
					   style    => $m->{style},
				           },
					 @mss);
	return $mission if $mission && ($m->{style} eq 'satisfy_one');          # if have one and need one, we try further?
	return _match_constraint ($ctx,
				  { bindings => $m->{bindings},
				    params   => [ $c->right ],
				    style    => $m->{style},
				    },
				  @mss);

    }
}

sub dispatch {
    my $ctx     = shift;
    my $details = shift;
    my $mission = shift;

    $log->error_die ("ran out of missions") unless $mission;

    $mission->{bindings}   = $details->{bindings};
    $mission->{style}    ||= $details->{style};

##    $log->debug ("dispatching new missions ".$mission->{style}."  ". Dumper ($mission, \@_));

    return &{$mission->{function}} ($ctx, $mission, @_);
}

sub _match_cstore {
    my $ctx   = shift; # context
    my $m     = shift; # mission
    my @mss   = @_;    # more missions after that

    my $cml   = $m->{params}->[0]; # list ref for constraint maplets
    my $i     = $m->{params}->[1]; # position in this list

#    $log->debug ("_match_cstore \n---- index in cstore $i -----");

    if ($i > $#{$cml}) {
	die "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  do I end up here?";
    } else {
	return _match_cmaplet_all_smaplets ($ctx,
					    { bindings => $m->{bindings},
					      params   => [ $cml, $i ],
					      style    => $m->{style} },
					    ( $cml->[$i + 1] ? # only add if there is indeed one more to check
					      { function => \&_match_cstore,
						params   => [ $cml, $i + 1 ], } :
					      () ),
					    @mss);
    }
}

sub _match_charlist {
    my $ctx   = shift; # context
    my $m     = shift; # mission with binding so far
    my @mss   = @_;    # more missions after that

    my $cl    = $m->{params}->[0]; # reference to characteristics list, of the current variable toplet
    my $tl    = $m->{params}->[1]; # reference to characteristics list, of the currently matched toplet

##warn "!!!!! in _match_charlist".Dumper ($cl, $tl);
# static things here => %invb
    my %invb;

    my @var_cs;                                          # all those characteristics which contain a variable
  OUTER:                                                 # can use a non-recursive solution here, no need for backtracking
    foreach my $c (@$cl) {
        if (ref ($c->[2]) eq 'TM::Variable') {
	    push @var_cs, $c;                            # save it for later
	    next;                                        # ignore it for now
	} else {
#warn "checking out ".Dumper $c;
	    foreach my $t (@$tl) {                       # look at all characteristics in the toplet and
#warn "is invb? ". Dumper $t;
		next if $invb{$t};
		if ($c->[0] eq $t->[0] && $c->[1] eq $t->[1] && $c->[2] eq $t->[2]) { # this means they are the same
#warn "binding $t";
		    $invb{$t} = 1;                       # memorize that this value should not be used
		    next OUTER;                          # if that worked, take care about the next c
		}                                        # otherwise try to find another toplet char
#warn "did not match";
	    }
#warn "no way";
	    return undef;                                # looped through and did not find a match
	}
    }

# do the variable part now
##warn "variable char matching now";
    return _match_charlist_var ($ctx, 
				{ bindings => $m->{bindings},
				  params   => [ \@var_cs, [ grep (!$invb{$_}, @$tl) ] ],
				  style    => $m->{style} }, 
				@mss);
}

sub _match_charlist_var {
    my $ctx = shift;
    my $m   = shift;
    my @mss = @_;

    my $cl  = $m->{params}->[0];
    my $tl  = $m->{params}->[1];

##warn "!!!!! in _match_charlist_var".Dumper ($cl, $tl);

    my $vc = shift @$cl;

    if (!$vc) { # that's it, we have handled all variable characteristics, get on with life
##warn "get on with life using binding ";
	return _continuation ($ctx, $m, @mss);
    } else {   # there is more work to do
	foreach my $t (@$tl) {
	    my ($b2, $b3, $b4);
	    if (($b2 = _match_id ($m->{bindings},                                         # checking value
				  ref ($vc->[2]) ? $vc->[2]->name : $vc->[2], 
				  $t->[2])) &&
## TODO: add subtyping later
		($b3 = $vc->[0] eq 'pxtm-universal-scope' ? 
		            $b2 :
		            _match_id ($b2,                                               # checking scope
				       ref ($vc->[0]) ? $vc->[0]->name : $vc->[0],
				       $t->[0])) &&
## TODO: add subtyping later
		($b4 = $vc->[1] eq 'xtm-psi-occurrence' ?
                            $b3 :
		            _match_id ($b3,                                               # checking type
				       ref ($vc->[1]) ? $vc->[1]->name : $vc->[1],
				       $t->[1]))
		) {
##warn "got new matching ".Dumper $b2;
		my $mission =  _match_charlist_var ($ctx, { bindings => $b4,              # new binding
							    params   => [ $cl, grep ($_ != $t, @$tl) ], # all chars except that one
							    style    => $m->{style} }, @mss);
		return $mission if $mission && $m->{style} eq 'satisfy_one';              # if have one and need one, we try further?
	    }
	}
    }
    return undef; # no more options
}

ux se Algorithm::Graphs::TransitiveClosure qw /floyd_warshall/;

sub _make_subclass_graph {
    my $maplets = shift;

    my $g;

    # collect classes from the maplets, candidates are:
    #   - class in xtm-psi-class-instance
    #   - superclass, subclass in xtm-psi-superclass-subclass

    foreach my $m (@$maplets) {
        next unless $m->scope eq 'pxtm-universal-scope';

        if ($m->type  eq 'xtm-psi-superclass-subclass') {
            $g->{$m->players->[0]}->{$m->players->[1]} = 1; # sub -> sup
            $g->{$m->players->[0]}->{$m->players->[0]} = 1; # sub -> sub
            $g->{$m->players->[1]}->{$m->players->[1]} = 1; # sup -> sup
        } elsif ($m->type  eq 'xtm-psi-class-instance') {
            $g->{$m->players->[0]}->{$m->players->[0]} = 1; # class -> class
        }
    }

    floyd_warshall $g;                                      # build transitive closure ## PERFORMANCE N**3 !!!!!
    return $g;
}

sub _match_subclass_smaplet {
    my $ctx   = shift; # context
    my $m     = shift; # mission with binding so far
    my @mss   = @_;    # more missions after that

    my $csub  = $m->{params}->[0];
    my $csup  = $m->{params}->[1];

##warn "check out: sub $csub sup $csup";
##warn "   current binding--->".Dumper $m->{bindings};

    $ctx->{store}->{indices}->{subclassing} ||= _make_subclass_graph ($ctx->{store}->{maplets}); ## PERFORMANCE

    my $iter; # will hold a function to do that
    {
	my $subclassing = $ctx->{store}->{indices}->{subclassing};              # just a shorthand
	if ($csub =~ /^\$/ && $csup =~ /^\$/) {                                 # both ends loose
	    $iter = sub {
		my @r;
		foreach my $mysub (keys %$subclassing) {
		    push @r, map { [ $mysub => $_ ] } keys %{$subclassing->{$mysub}}
		}
		return \@r;
	    };
	} elsif ($csub =~ /^\$/) {                                              # this means csup is fix, one loose end
	    $iter = sub {
		my @r;
		foreach my $mysub (keys %$subclassing) {
		    push @r, [ $mysub, $csup ] if $subclassing->{$mysub}->{$csup};
		}
		return \@r;
	    };
	} elsif ($csup =~ /^\$/) {                                              # this means csub is fix, one loose end
	    $iter = sub {
		return [ map { [ $csub => $_ ] } keys %{$subclassing->{$csub}} ];
	    };
	} else {                                                                # both fixed
	    $iter = sub {
		return [ $subclassing->{$csub}->{$csup} ?
			   [ $csub , $csup ] 
			   :
			   () ];
	    };
	}
    }

#    # now make a distinction to reduce the costs of computing all necessary subclass, superclass pairs
#    # I could ALWAYS compute the hull of the subclass graph derived from the map, but this is only really useful when both ends are loose
#    my $iter; # will hold a function to do that
#
#    {
#	my $subclassing = $ctx->{store}->{indices}->{subclassing};              # just a shorthand
#	if ($csub =~ /^\$/ && $csup =~ /^\$/) {                                 # both ends loose
#	    $iter = sub {
#		die "cannot do yet";
#	    };
#	} elsif ($csub =~ /^\$/) {                                              # this means csup is fix, one loose end
#warn "csup fix";
#	    $iter = sub {
#		return [                   map { [ $_, $csup ] } $subclassing->predecessors ($csup), [ $csup, $csup ] ];
#	    };
#	} elsif ($csup =~ /^\$/) {                                              # this means csub is fix, one loose end
#warn "csub fix";
#	    $iter = sub {
#		return [ [ $csub, $csub ], map { [ $csub, $_ ] } $subclassing->successors ($csub) ];
#	    };
#	} else {                                                                # both fixed
#warn "both fix";
#	    $iter = sub {
#warn " in fix $csub -> $csup?";
#warn " getting successors : ".join ("->", $subclassing->successors ($csub));
#		return [ grep ($csup eq $_, $subclassing->successors ($csub)) ?
#			   [ $csub , $csup ] 
#			   :
#			   () ];
#	    };
#	}
#    }

sub _dump_b {
    my $b = shift;
    return Dumper { map { ref ($b->{$_}) ? () : ( $_ => $b->{$_}) } keys %$b };
}

    foreach my $e (@{$iter->()}) {
        my ($ssub, $ssup) = @$e;
##warn "generated: sub $ssub sup $ssup";
##warn "   starting with binding "._dump_b ($m->{bindings});
	my $b2 = _match_id ($m->{bindings}, $csup, $ssup);
	next unless $b2;
##warn "still here after b2"._dump_b($b2);
	my $b3 = _match_id ($b2,            $csub, $ssub);
	next unless $b3;
##warn "still here after b3 "._dump_b( $b3);

	push @{$ctx->{pattern}->[0]->{matched_maplets}}, 
                     new TM::Maplet (scope   => 'pxtm-universal-scope',	    # create a maplet here
				     type    => 'xtm-psi-superclass-subclass',
				     roles   => [ 'xtm-psi-subclass', 'xtm-psi-superclass' ],
				     players => [ $ssub, $ssup ]);

	my $mission = _continuation ($ctx, 
				     { bindings => $b3,
				       style    => $m->{style} },
				     @mss);
	return $mission if $mission && ($m->{style} eq 'satisfy_one');      # if have one and need one, we try further?
	
	pop @{$ctx->{pattern}->[0]->{matched_maplets}};                     # forget about the maplet again
    }
    return undef;
}

sub _match_cmaplet_all_smaplets {
    my $ctx   = shift; # context
    my $m     = shift; # mission with binding so far
    my @mss   = @_;    # more missions after that

    my $cml   = $m->{params}->[0]; # reference to maplet list (whole list)
    my $ci    = $m->{params}->[1]; # position in this list

##warn " _match_cmaplet_all_smaplets\n--- index in cml $ci ----->".Dumper $cml->[$ci];

    if ($cml->[$ci]->type eq 'xtm-psi-superclass-subclass') {       # special maplet
##warn " in _match_cmaplet_all_smaplets checking subclasses";
	return _match_subclass_smaplet ($ctx, 
				       { bindings => $m->{bindings},
					 params   => $cml->[$ci]->players,
					 style    => $m->{style} },
				       @mss);
    } elsif ($cml->[$ci]->type eq 'astma-sum-ergo-sum') {           # this is a special maplet type to signal that this is just a topic
##warn " in _match_cmaplet_all_smaplets checking sumergo";
	my $p = $cml->[$ci]->players->[0];

	if ($p =~ /^\$/) {                                          # player is variable -> all would match, have to iterate
	    foreach my $t (keys %{$ctx->{store}->{toplets}}) {
		my $b2 = _match_id ($m->{bindings},  $p,  $t);
		next unless $b2;                                    # if $p is already matched to sthg else, then do not take this
		push @{$ctx->{pattern}->[0]->{matched_maplets}}, $ctx->{store}->{toplets}->{$t};
                                                                    # memorize that this maplet/toplet could match
		my $mission = _continuation ($ctx, 
					     { bindings => $b2,
					       style    => $m->{style} }, 
					     { function => \&_match_charlist,
					       params   => [ $ctx->{pattern}->[0]->{cstore}->{toplets}->{$p}->basenames, 
							     $ctx->{store}->{toplets}->{$t}->basenames ], },
					     { function => \&_match_charlist,
					       params   => [ $ctx->{pattern}->[0]->{cstore}->{toplets}->{$p}->resources, 
							     $ctx->{store}->{toplets}->{$t}->resources ], },
					     { function => \&_match_charlist,
					       params   => [ $ctx->{pattern}->[0]->{cstore}->{toplets}->{$p}->datas, 
							     $ctx->{store}->{toplets}->{$t}->datas ], },
					     @mss);
		pop @{$ctx->{pattern}->[0]->{matched_maplets}};                     # forget about the maplet again

		return $mission if $mission && ($m->{style} eq 'satisfy_one');      # if have one and need one, we try further?
	    }
	    return undef;                                                           # in generate mode or satisfy and nothing found

	} elsif ($ctx->{store}->{toplets}->{$p}) {                  # is the topic in this maplet also a topic in the store -> BINGO
	    push @{$ctx->{pattern}->[0]->{matched_maplets}},  $ctx->{store}->{toplets}->{$p};  
                                                                                    # memorize that this maplet/toplet could match
	    return _continuation ($ctx, $m, @mss);
	    pop @{$ctx->{pattern}->[0]->{matched_maplets}};                         # forget about the maplet again

	} else {
	    return undef;
	}

    } else {                                                                        # arbitrary maplet
##warn " in _match_cmaplet_all_smaplets checking arbitrary";
	foreach my $cs (@{$ctx->{store}->{maplets}}) {                              # take every store maplet, one of them must match
	    push @{$ctx->{pattern}->[0]->{matched_maplets}},  $cs;                  # memorize that this maplet/toplet could match
	    my $mission = _match_cmaplet_smaplet ($ctx, 
						  { bindings => $m->{bindings},
						    params   => [ $cml->[$ci], $cs ],
						    style    => $m->{style} },
						  @mss);
	    pop @{$ctx->{pattern}->[0]->{matched_maplets}};                         # forget about the maplet again
	    return $mission if $mission && ($m->{style} eq 'satisfy_one');          # if have one and need one, we try further?

## what about returning something otherwise???

	    # as this is implicit all, try it again with another to prompt a match
	    # unless we already have one and need one

	}
#	$log->debug ("no more maplets in map");
	return undef; # in generate mode or satisfy and nothing found
    }
}

sub _match_cmaplet_smaplet {
    my $ctx   = shift; # context
    my $m     = shift; # mission with binding so far
    my @mss   = @_;    # more missions after that

    my $cm    = $m->{params}->[0]; # constraint maplet
    my $sm    = $m->{params}->[1]; # store maplet

#    $log->debug ("_match_cmaplet_smaplet \n---- constraint maplet -----".Dumper($cm). "\n------ store maplet -----". Dumper ($sm));

    my $b2 = _match_id ($m->{bindings},  $cm->type,  $sm->type);
    return unless $b2;
    my $b3 = _match_id ($b2,             $cm->scope, $sm->scope);
    return unless $b3;
    
    return _match_cmembers ($ctx, 
			    { bindings => $b3,
			      params   => [ $cm, 0, $sm ],
			      style    => $m->{style} },
			    @mss);
}

sub _match_cmembers {
    my $ctx   = shift; # context
    my $m     = shift; # current mission
    my @mss   = @_;    # more missions after that

    my $cm    = $m->{params}->[0]; # constraint maplet
    my $cmi   = $m->{params}->[1]; # index for member to match in $cm->roles/players
    my $sm    = $m->{params}->[2]; # store maplet

#    $log->debug ("_match_cmembers \n----- cm ---- ".Dumper($cm) . "----- cmi: $cmi ----". Dumper ($sm));

    if ($cmi > $#{$cm->roles}) {                                                  # all roles of this done, maybe have to check more
#	$log->debug ("all croles covered, current mission: ". Dumper $m);

	return _continuation ($ctx, $m, @mss);

    } else {                                                                      # more roles to match
#	$log->debug ("start checking roles/players (for $cmi)");
	my ($crole,  $cplayer)  = ($cm->roles->[$cmi], $cm->players->[$cmi]);
	my ($sroles, $splayers) = ($sm->roles,         $sm->players);
	for (my $i = 0; $i < @{$sroles}; $i++) {
	    if (    my $b2 = _match_id ($m->{bindings},  $crole,   $sroles->[$i])) {          # roles would match
		if (my $b3 = _match_id ($b2,             $cplayer, $splayers->[$i])) {        # also players match
#		    $log->debug ("achieved (new) binding ".Dumper $b3);
		    my $mission = _match_cmembers ($ctx,
						   { bindings => $b3,
						     params   => [ $cm, $cmi + 1, $sm ],
						     style    => $m->{style} },
						   @mss);              # !!!! NOT EFFICIENT !!!! ??????
		    return $mission if $mission && ($m->{style} eq 'satisfy_one');# if have one and need one, we try further?
		} else {                                                          # no match, no worries, try another 
#		    $log->debug ("players dont match: ".$cplayer."  ".$splayers->[$i]);
		}
	    } else {
#		$log->debug ("roles dont match: ".$crole."  ".$sroles->[$i]);
	    }                                                                     # else do not care, simply try another smember
##	    return if $m->{style} eq 'satisfy_one' && $m->{nr_matches};               # if have one and need one, why try further?
	}                                                                         # all done? then this is all which we can find here
#	$log->debug ("giving up member loop");
	return undef; # no match could be find for the crole here
    }
}

sub _match_id {
    my $b  = shift; # binding
    my $vc = shift; # variable or constant
    my $c  = shift; # constant

#warn " match id vc $vc, m $c";

    if ($vc =~ /^\$t-/) {                               # this is a system variable, special rules apply
	if (my $v = $b->{$vc}) {
	    return undef unless $c eq $v;
	    return $b;
	} else {
	    my %b2 = %$b;                               # make a copy of the binding (do not change upstream bindings)
            $b2{$vc} = $c;
            return \%b2;
	}
    } elsif ($vc =~ /^\$/) {                            # one is user variable
	if (my $v = $b->{$vc}) {                        # variable is already bound to something
	    return undef unless $c eq $v;               # must match perfectly then
	    return $b;                                  # no new binding
#	} elsif (my loook up store, find regexp) {
#           return undef unless $c =~ /$regexp/;
	} else {                                        # one variable, no value and no special regexp <-> and a new variable

	    return undef if grep ($c eq $_, values %$b);   # does $c already appear as value for another variable? then no match!

	    my %b2 = %$b;                               # make a copy of the binding (do not change upstream bindings)
	    $b2{$vc} = $c;
	    return \%b2;                                # return new binding
	}
    } else {                                            # both are constants
	return undef unless $c eq $vc;                  # two constants must be identical
	return $b;                                      # we have match, but no special binding
    }
}

sub _continuation {
    my $ctx   = shift;
    my $m     = shift; # mission + bindings (which are the only interesting here)
    my %b     = %{$m->{bindings}};

    my @mss   = @_;    # more missions after that

    my $pattern_variable = $ctx->{pattern}->[0]->{pattern_variable};
    my $matched_maplets  = $ctx->{pattern}->[0]->{matched_maplets};

##warn "in continuation checking ".Dumper $ctx->{pattern};
                                                                               # add the pattern variable together with matched maplets
    if ($pattern_variable && @$matched_maplets) {                              # if maplets have been matched then bind this
	$b{$pattern_variable} = {
	                         store   => $ctx->{store},                     # of course, only if we have a variable
				 maplets => (@$matched_maplets == 1) ?         # take this, regardless whether Toplet or Maplet
				     [ @$matched_maplets ] :                   # MAKE A COPY! 
				     [ grep (ref ($_) eq 'TM::Maplet', @$matched_maplets) ]
				 };                                            # otherwise copy only Maplets

##warn "in continuation, new binding for $pattern_variable is $b{$pattern_variable}";

    # ???? here a check for pattern variable is missing ?????
    }
    # successful completion of a maplet match

##warn " in continuation partial match ".Dumper { map { ref ($b{$_}) ? () : ( $_ => $b{$_}) } keys %b };

    if (@mss) {                                                                # there is more work to do before we can establish a match
#	$log->debug ("calling continuation ".Dumper \@mss);
	if (my $m2 = dispatch ($ctx, 
			       { bindings => \%b,
				 style    => $m->{style} },
			       @mss)) {
#	    $log->debug ("had result back");
	    return $m2;
	} elsif ($m->{style} eq 'satisfy_one') {
# ????????????????????????????????????????????????????????????????????????????????????????
#	    $log->debug ("no result and satisfy_all -> exception");
	    die "unsatisfyable";
	} else {
#	    $log->debug ("no result and not satisfy_all -> undef, force backtracking");
	    return undef;
	}
    } else {                                                                   # this is finally a match
#	$log->debug( "!!!!!!!!!!!!!!!!MATCH!!! ". Dumper \%b);
##warn " in continuation: !!!!!complete MATCH ".Dumper \%b;	

	$m->{nr_matches}++;                                                    # incr last inner match counter

##      $log->debug("before exploit ".$ctx->{exploit});
	&{$ctx->{exploit}} (\%b);
##	$log->debug("after exploit ");
	return $m;
    }
}

 =pod

 =item B<query>

Applies a function object to the store, returns either a string, a map (what?) or XML in DOM.

....parameters....

 =cut

sub query {
    my $self      = shift;
    my $function  = shift;
    my $params    = shift || {};
    my $exts      = shift || {};                                      # URL => module bindings

    $log->error_die ("must be hash ref")        unless ref($params) eq 'HASH';

    # if not already defined by the environment, use the system implementation of functions and operations
    $exts->{'http://astma.it.bond.edu.au/ns/query/functions'}  ||= 'TM::AsTMa::Functions';
    $exts->{'http://astma.it.bond.edu.au/ns/query/operations'} ||= 'TM::AsTMa::Operations';

    foreach (values %$exts) {
	eval "require $_" || die "cannot load extension module '$_'";
    }


#    $log->debug ("applying function '".$function->name."'");

    return _qeval ($self->{functions}->{$function}, [ $params, { '_env'  => $self,
#                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    ^^^^^^^    ^^^^^^^^^^^^^
#                  call this                          |||||||    |||||||||||||
#                                                     with these |||||||||||||
#                                                     params     this will always be added, local functions are defined here

								 # we also need the functions
								 '_ns_fn' => 'http://astma.it.bond.edu.au/ns/query/functions',
								 '_ns_op' => 'http://astma.it.bond.edu.au/ns/query/operations',

								 # and how the extensions are implemented by modules
                                                                 '_exts' => $exts,

                                                                                 } ], undef);
}

sub _qeval {
    my $e    = shift;
    my $ctxs = shift;                                                 # list of outside contexts
    my $type = shift;

##warn "qeval ".Dumper ($e)."\n type '$type'";

    if (ref ($e) eq '') {                                             # string easy
	return _cvt ($e, $type);

    } elsif (ref ($e) eq 'ARRAY') {
##	warn "in array !!!!". Dumper $e;
	my @l;
	foreach my $l (@$e) {
	    push @l, _qeval ($l, $ctxs, undef); # 'string' was there earlier
	}
	return _cvt ([ @l ], $type);

    } elsif (ref($e) eq 'TM::CodeBlock') {                           # code block
	my $ctx  = {};                                                # local context

	foreach my $v (@{$e->vars}) {
	    $ctx->{$v->name} = _qeval ($v->expr, [ $ctx, @$ctxs ], $v->type) if ($v->expr);
	}

	my $new_ctx = [ $ctx, @$ctxs ];
	my $res = _qeval ($e->stmt, $new_ctx, $type);
	if (ref ($res) eq 'ARRAY' && $e->slice) {
	    return _slice_it ($res, $e->slice, $new_ctx);
	} else {
	    return $res;
	}

    } elsif (ref($e) eq 'TM::Function') {                            # functions

##warn "evaling function ".$e->name;
	my $ctx  = {};                                                # local context

	my $nss = $e->namespaces;
	foreach my $ns (keys %$nss) {                                 # move the current values into the context
##warn "adding ns $ns and ".$nss->{$ns};
	    $ctx->{ "_ns_$ns" } = $nss->{$ns};
	}

	foreach my $p (@{$e->params}) {                               # go through the params
	    if (defined (my $v = _ctx_value ($p->name, $ctxs))) {     # if override from outside use that (do not use local ctx!!)
		$ctx->{$p->name} = _cvt ($v, $p->type);
	    } elsif ($p->expr) {                                      # elsif default, use that
		$ctx->{$p->name} = _qeval ($p->expr, $ctxs, $p->type);
	    } else {
		$ctx->{$p->name} = undef;
#		$log->debug ("no value for '".$p->name."' provided.");
	    }
	}
	return _qeval ($e->expr, [ $ctx, @$ctxs ], $e->type);

    } elsif (ref ($e) eq 'TM::List') {
	my @l;
	foreach my $l (@{$e->lst}) {
	    push @l, _qeval ($l, $ctxs, undef);  ## 'string' was here !!!! ????? what should the type be here?????
	}

##warn "in TM::List: have list ".Dumper (\@l);

	if ($e->slice) {
	    return _cvt ( _slice_it ([ @l ], $e->slice, $ctxs), 'tuple');
	} else {
	    return _cvt ([ @l ], 'tuple');
	}

    } elsif (ref ($e) eq 'TM::OrElses') {                            # this is an 'or else' cascade
	foreach my $a (@{$e->lst}) {
##warn "try $a". Dumper $a;
	    my $v;
	    eval {
		$v = _qeval ($a, $ctxs, $type);
	    };
	    next                    if $@;                           # on exception we try next
	    return _cvt ($v, $type) if defined $v;                   # if a value was generated, return that
	}
	die $@ if $@;                                                # if we had an exception at the end and are still here (no exception handling), then re-raise that
	return undef;

    } elsif ($e->isa ('TM::IfStatement')) {
##warn "eval conditiopn is ".Dumper $e->condition;
	my $c = _qeval ($e->condition, $ctxs, 'string');
##warn "conditiopn is ".Dumper $c;
	if (defined $c) {
	    return _qeval ($e->then, $ctxs, $type);
	} elsif ($e->else) {
	    return _qeval ($e->else, $ctxs, $type);
	} else {
	    return undef;
	}

    } elsif ($e->isa ('TM::InvokeStatement')) {
#warn "start calc parasm for ".$e->name." as tuple ". Dumper $e->params;
	my $params = _qeval ($e->params, $ctxs, 'tuple'); # get the actual parameters
#warn "params for ".$e->name." are ".Dumper $params;

	if ($e->prefix eq 'system') {      # system function
	    die "calling a system function, not implemented yet";
	} elsif ($e->prefix eq '') {       # local function
	    my $env = _ctx_value ('_env', $ctxs);
	    my $fun = $env->{functions}->{$e->name} || die "Calling unknown function '".$e->name."'";

##warn "function ".Dumper $fun;

	    my %params; # this will hold the binding
	    foreach my $p (@{$fun->params}) {
		$params{$p->name} = defined $params->[0] ? shift @$params : _qeval ($p->expr, $ctxs, undef);
	    }
##warn "params are ".Dumper \%params;
	    return _qeval ($fun, [ \%params, @$ctxs ], $type);
	} else {                           # some other prefix -> external function
	    my $fun = $e->name;
	    $fun =~ s/-/_/g;               # no - in the name, sorry
	    my $ns  = _ctx_value ("_ns_".$e->prefix, $ctxs)  || die "unknown namespace prefix '".$e->prefix."'";
	    my $mod = _ctx_value ("_exts", $ctxs)->{$ns}     || die "unknown module for namespace '$ns'";

#warn "invoking external function $mod $fun";
            no strict 'refs';
	    return &{"${mod}::${fun}"} (@$params);
	}

    } elsif ($e->isa ('TM::LoopStatement')) {
	my $iter = $e->iter;
	my @l2;

	my @sorter;
	my @orders = map { $_->ord } @{$e->sort};
##warn "order is ".Dumper \@orders;

	if ($iter->isa ('TM::ListIterator')) {
	    my $l1 = _qeval ($iter, $ctxs, 'list');

##warn "iter/list is ".Dumper ($iter, $l1);

	    my $ctx = {};
	    my $index = 0;                                          # loop counter
	    foreach my $l (@$l1) {
		$ctx->{$iter->var} = $l if ($iter->var);            # loop variable
		$ctx->{'$#'}       = $index++;                      # loop counter stored in $#
#warn " list iteration var is ".Dumper $l;
#		unshift @l2, _qeval ($e->expr, [ $ctx, @$ctxs ], $type eq 'list' ? undef : $type); ###'string');  ## or $type ???
#warn " list iter added one, now is ".Dumper \@l2;
		my $x = _qeval ($e->expr, [ $ctx, @$ctxs ], $type); ##$type eq 'list' ? undef : $type); ###'string');  ## or $type ???
#warn " list iter adding ".Dumper $x;
		unshift @l2, (ref ($x) eq 'ARRAY' ? @$x : $x) if defined $x;

		my $i = 0;
		foreach my $s (@{$e->sort}) {                               # !!!! PERFORMANCE !!!!!
		    $sorter[$i++]->{$l2[0]} = _qeval ($s->expr, [ $ctx, @$ctxs ], 'string');
		}

	    }
###	    warn "converting this to $type";
	} else { # must be constraint-based loop
	    my $tm = _eval_tau ($iter->tau_expr, $ctxs);
#	    $log->debug ("taued var is ---------------------\n".Dumper $tm);

#warn "handing in ctxs in to match: ".Dumper $bindings;
	    my $results = $tm->{store}->match ($iter->constraint, 'filter', _ctx2b ($ctxs));
##warn "results back ".Dumper $results;
#	    $log->debug ("results ---------------------\n".Dumper $results);

	    my $ctx = {};
	    my $index = 0;                                          # loop counter
	    foreach my $r (@$results) {
		$ctx->{'$#'}       = $index++;                      # loop counter stored in $#
		my $x = _qeval ($e->expr, [ $ctx, $r, @$ctxs ], $type); ## $type eq 'list' ? undef : $type); ##'string');
#warn " list match adding ".Dumper $x;
		unshift @l2, (ref ($x) eq 'ARRAY' ? @$x : $x) if defined $x;

		my $i = 0;
		foreach my $s (@{$e->sort}) {                               # !!!! PERFORMANCE !!!!!
		    $sorter[$i++]->{$l2[0]} = _qeval ($s->expr, [ $ctx, $r, @$ctxs ], 'string');
		}
	    }
	}

	if (@{$e->uniq}) {                                                  # get rid of duplicate entries
	    my %elements;
	    @l2 = grep (!$elements{$_}++, @l2);
	}

##warn "Sorter ".Dumper \@sorter;

	if ($e->sort) {
#warn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!sorting";
	    @l2 = sort {                                   # !!!!! PERFORMANCE !!!!!!!!
		my $v;
		for (my $i = 0; $i <= $#sorter; $i++) {
##warn "$i ".$sorter[$i]->{$a}."   ".$sorter[$i]->{$b};
		    return $v if ($orders[$i] eq 'asc' && ($v = $sorter[$i]->{$a} cmp $sorter[$i]->{$b}));
		    return $v if (                        ($v = $sorter[$i]->{$b} cmp $sorter[$i]->{$a}));
		}
		return 0; # dont know
	    } @l2;
	}

##warn "in loop have type $type";
##	my $xxx = _cvt ([ @l2 ], $type);
##warn "evaling ".Dumper (\@l2)." gives ".Dumper $xxx;
	return _cvt ([ @l2 ], $type);

    } elsif ($e->isa ('TM::ListIterator')) {
	return _qeval ($e->list, $ctxs, $type);

    } elsif ($e->isa ('TM::AsTMaPathVar')) {
##warn "variable ".$e->var."  contexts ".Dumper $ctxs;
	my $val = _ctx_value ($e->var, $ctxs);                              # val contains first store and then list of maplets

	if (@{$e->steps}) {
	    die "no path expression allowed on strings" unless ref($val);   # do not allow strings and /bn etc

	    # compute a list of values here
	    my @new = map { ref ($_) ?                                      # if it is a Maplet/Toplet
                                { store => $val->{store}, maplets => [$_] }:# then add store info to it, so that evaluation can be done later
                                $_ }                                        # otherwise leave it as it is
                             @{                                             # make it a list
			      _ap_eval ($val->{store}, 
					$val->{maplets}, 
					@{$e->steps})                       # eval path expr
			      };
##warn "new ".Dumper \@new;
	    return _cvt ([ @new ], $type);

	} else {
##warn "value $val -> $type";	    
	    return _cvt ($val, $type);
	}

    } elsif ($e->isa ('TM::AsTMaPathNull')) {
	return undef;

    } elsif ($e->isa ('TM::Numeric')) {
	return $e->val;

    } elsif ($e->isa ('TM::CurledString')) {                         # concatenate partial results
	my $s;
	foreach my $l (@{$e->lst}) {
	    $s .= _qeval ($l, $ctxs, 'string');
	}
	return _cvt ($s, $type);

    } elsif ($e->isa ('TM::XMLFragmentList')) { # should only happen at top level!
	my $node = XML::LibXML::DocumentFragment->new();

##warn "evaling XMLFrag".Dumper $e;

	foreach my $c (@{$e->list}) {
##warn "one component of the fragment list is ".$c;
	    my $node_or_nodes = _qeval ($c, $ctxs, 'xml');
##warn "XMLFragment append...".Dumper ($c)."....got evaled ".Dumper ($node_or_nodes). " which translates into ".$node_or_nodes->toString. "||||";
	    $node->appendChild ($node_or_nodes);
##warn "3....".$node->toString;
	}
	return $node;

    } elsif ($e->isa ('TM::XMLElement')) {
	my $node = XML::LibXML::Element->new( _qeval ($e->start, $ctxs, 'string') );

	foreach my $a (@{$e->attrs}) {
	    $node->setAttribute( _qeval ($a->name,  $ctxs, 'string'), 
				 _qeval ($a->value, $ctxs, 'string') );
##warn "1....".$node->toString;
	}

##warn "evaling XMLElem ".Dumper $e;
	foreach my $c ($e->content ? @{$e->content->list} : ()) {
##warn "one component of the XML elem is ".$c;
##warn "trying to append ".Dumper ($c) . " as xml";
	    my $node_or_nodes = _qeval ($c, $ctxs, 'xml');
##warn "got eval from ".Dumper ($c). " is ".Dumper $node_or_nodes;
	    if (ref ($node_or_nodes) eq 'ARRAY') {
		foreach my $n (@$node_or_nodes) {
##warn "appending (list) ".$n->toString." and node is ".ref ($n);
		    $node->appendChild ($n);
		}
	    } else {
##warn "appending ".$node_or_nodes->toString." and node is ".ref ($node_or_nodes);
		$node->appendChild ($node_or_nodes);
	    }
	}
##warn "3....".$node->toString;
	return $node;

    } else {
	$log->error_die ("cannot handle expression yet");
    }
}

sub _slice_it {
    my $res    = shift;
    my $slicer = shift;
    my $ctxs   = shift;

#warn "slice here ".Dumper $e->slice;
    my $start = $slicer->start ? _qeval ($slicer->start, $ctxs, 'string') : 0;
    my $end   = $slicer->end   ? _qeval ($slicer->end,   $ctxs, 'string') : $#{@$res};
#warn "$start ... $end";
#warn "before ".Dumper $res;
    return [ @$res[$start .. $end] ];
}


sub _ctx2b {
    my $cs = shift;
    my %b;
    foreach my $c (reverse @$cs) { # start from the 'outer' side, inner bindings 'win' that way
	foreach my $v (keys %$c) {
	    $b{$v} = $c->{$v};
	}
    }
    return \%b;
}

sub _ap_eval {
    my $store = shift;
    my $vals  = shift; # list ref
    my $step  = shift; # first in the list, the others we keep in @_

    return $vals unless $step;                                               # empty steps -> return list as-is

    my @l;
    my $ss = $step->step;
    foreach my $v (@$vals) {
##warn "work on ".Dumper ($v)." and step ".Dumper ($ss);
	my @l2; 
	if (ref ($ss) eq 'TM::AsTMaPathTD') {                                # topic drill down
	    my $t;
	    if (ref ($v) eq 'TM::Toplet') {
		$t = $v;
	    } elsif (ref ($v) eq 'TM::Maplet' && $v->type eq 'astma-sum-ergo-sum') {
		$t = $store->{toplets}->{$v->players->[0]};
	    } elsif (ref ($v) eq 'TM::Maplet' && $v->type eq 'xtm-psi-class-instance') {
		$t = $store->{toplets}->{$v->players->[1]};
	    } else { # ignore rest
		next;
	    }
#warn "t is now ".Dumper $t;

	    if ($ss->axis eq 'bn') {
		push @l2, @{$t->basenames};
	    } elsif ($ss->axis eq 'oc') {
		push @l2, @{$t->resources};
	    } else { # if $ss->axis eq 'in'
		push @l2, @{$t->datas};
	    }

#warn "got 1 list ". Dumper \@l2;
	    if (my $sc = $ss->scope) {
		@l2 = grep ($_->[0] eq $sc, @l2);
	    }
	    if (my $ty = $ss->type) {
		@l2 = grep ($_->[1] eq $ty, @l2);
	    }
#warn "got 2 list ". Dumper \@l2;

	    if ($ss->select eq 'text()') {                                       # get the values 
		grep ($_ = $_->[2], @l2);
	    } elsif ($ss->select eq '()') {                                      # get the types
		grep ($_ = $_->[1], @l2);
	    } else {                                                             # get the scopes
		grep ($_ = $_->[0], @l2);
	    }
#warn "got 3 list ". Dumper \@l2;

	} elsif (ref ($ss) eq 'TM::AsTMaPathTL') {
	    my $t;
	    if (ref ($v) eq 'TM::Toplet') {  # ok
		$t = $v;
	    } elsif (ref ($v) eq 'TM::Maplet' && $v->type eq 'astma-sum-ergo-sum') {
		$t = $store->{toplets}->{$v->players->[0]};
	    } elsif (ref ($v) eq 'TM::Maplet' && $v->type eq 'xtm-psi-class-instance') {
		$t = $store->{toplets}->{$v->players->[1]};
	    } else {
		next;
	    }
#warn "t for lateral is now ".Dumper $t. "role is ".$ss->role." and type ".$ss->type;

	  LATERAL:
	    foreach my $m (@{$store->{maplets}}) {                          # PERFORMANCE!!!!!
		next unless $m->type eq $ss->type;
		# add scope here?
		my ($rs, $ps) = ($m->roles, $m->players);
		for (my $i = 0; $i < @$rs; $i++) {
		    if ($t->id eq $ps->[$i] && ($ss->role eq '*' || $rs->[$i] eq $ss->role)) {
			push @l2, $m;
			next LATERAL;
		    }
		}
	    }
#warn "got lateral list ". Dumper \@l2;


	} elsif (ref ($ss) eq 'TM::AsTMaPathAD' ) {
	    my $a;
	    if (ref ($v) eq 'TM::Toplet') { # ignore toplets here
		next;
	    } else {
		$a = $v;
	    }

	    if ($ss->axis eq '@') {
		push @l2, $store->{toplets}->{$a->scope};
	    } elsif ($ss->axis eq '()') {
		push @l2, $store->{toplets}->{$a->type};
	    } elsif ($ss->axis eq ':') {
		push @l2, map { $store->{toplets}->{ $_ } } $a->roles;
	    } elsif ($ss->axis eq '*') {
		push @l2, map { $store->{toplets}->{ $_ } } $a->players;
	    } else { # axis is a topic id
		my ($rs, $ps) = ($a->roles, $a->players);
		for (my $i = 0; $i < @$rs; $i++) {
		    if ($rs->[$i] eq $ss->axis) {
			push @l2, $store->{toplets}->{$ps->[$i]};
			next;
		    }
		}
	    }
#warn "got ad list ". Dumper \@l2;

	}
	push @l, @l2;
    }
    return _ap_eval ($store, [ @l ], @_);
}

sub _eval_tau {
    my $e    = shift; # tau expression
    my $ctxs = shift;

    if ($e->isa ('TM::Driver::TauVar')) {
	return _cvt (_ctx_value ($e->var, $ctxs), 'map');
    } else {
	$log->error_die ("cannot handle tau expression '".ref($e)."'yet");
    }
}

sub _cvt {
    my $v = shift;
    my $t = shift;

    use Carp;

#    $log->debug ("convert to '$t' from ".ref($v). " " . $v);

    return $v unless defined $v;                   # no value, no fun
    return $v unless defined $t;                   # no type, no work

##warn "in cvt to $t from  ".Dumper $v;

    if ($t eq 'string') {
	if (ref($v) eq '') {                       # string -> string
	    return $v;

	} elsif (ref($v) eq 'TM::Numeric') {
	    return $v->val;

	} elsif (ref($v) eq 'ARRAY' || ref($v) eq 'Tuple') {      # list, tuple -> string
	    my $s;
	    foreach my $l (@{$v}) {
		$s .= _cvt ($l, 'string');
	    }
	    return $s;

	} elsif (ref ($v) eq 'TM::Toplet') {
	    return $v->id;

	} elsif ($v->isa ('XML::LibXML::Node')) {
	    return $v->toString(0);

	} else {
	    confess "cannot convert to $t from ".ref($v);
	}

    } elsif ($t eq 'list') {
	if (ref ($v) eq '') {                      # string -> list
	    return [ $v ];

	} elsif (ref($v) eq 'ARRAY') {
	    return $v;                             # list -> list

	} elsif (ref($v) eq 'Tuple') {             # tuple -> list
	    return [ $v ];
##	    return [ map { ref($_) eq 'ARRAY' ? @$_ : $_ } @$v ];

	} elsif (ref($v) eq 'TM::Numeric') {
	    return [ $v->val ];

	} else {
            confess "cannot convert to $t from ".ref($v);
	}

    } elsif ($t eq 'tuple') {
	if (ref($v) eq 'ARRAY') {
	    return bless [ map { ref($_) eq 'ARRAY' ? @$_ : $_ } @$v ], 'Tuple';

	} elsif (ref ($v) eq 'Tuple') {
	    return $v;

	} else {
	    return bless [ $v ], 'Tuple';
	}

    } elsif ($t eq 'map') {
	if (ref ($v) eq 'TM') {
	    return $v;
	} else {
            confess "cannot convert to $t from ".ref($v);
	}

    } elsif ($t eq 'xml') {
	if (ref ($v) eq '') {
	    return XML::LibXML::Text->new ( $v );

	} elsif (ref($v) eq 'TM::Numeric') {
	    return XML::LibXML::Text->new ( $v->val );

	} elsif (ref($v) eq 'ARRAY' || ref($v) eq 'Tuple') {
	    my @l;
	    foreach my $l (@$v) {
		push @l, _cvt ( $l, 'xml');
	    }
	    return [ @l ];
	} elsif ($v->isa ('XML::LibXML::Node')) {
	    return $v;
	} else {
            confess "cannot convert to $t from ".ref($v);
	}

    } else {
	confess "cannot convert to $t from ".ref($v);
    }
}

sub _ctx_value {
    my $n    = shift;
    my $ctxs = shift;
#    my $must = shift; # if this is set, it MUST be there

    foreach my $c (@$ctxs) { # start from the most recent
	return $c->{$n} if defined ($c->{$n});
    }
    return undef;
#    $must ? $log->error_die ("reference to unknown variable '$n'") : return undef;
}


use XML::LibXML;

struct 'TM::Pattern'       => [
				mode       => '$',
                                store      => '$',
                                ];

struct 'TM::ExistsClause'  => [
                                pattern    => '$',
                                variable   => '$',
                                modifier   => '$',
                                ];

struct 'TM::AndConstraint' => [
                                left       => '$',
		                right      => '$',
                                ];

struct 'TM::OrConstraint'  => [
                                left       => '$',
		                right      => '$',
                                ];

struct 'TM::ForAllClause'  => [
                                pattern    => '$',
                                variable   => '$',
                                constraint => '$',
                                ];

struct 'TM::Function'      => [
                                name       => '$',
                                params     => '@',
                                type       => '$',
                                functions  => '@',
			        namespaces => '%',
                                expr       => '$',
				];

struct 'TM::Variable'      => [
				name       => '$',
                                type       => '$',
                                expr       => '$',
                                ];

struct 'TM::CodeBlock'     => [
                                vars       => '@',
                                stmt       => '$',
                                slice      => '$',
				];

struct 'TM::InvokeStatement' => [
                                name       => '$',
                                prefix     => '$',
			        params     => '$',
                                ];

struct 'TM::IfStatement'   => [
                                condition  => '$',
                                then       => '$',
				else       => '$',
                                ];




struct 'TM::LoopStatement' => [
                               iter       => '$',
			       expr       => '$',
                               uniq       => '$',
			       sort       => '@',
			       ];

struct 'TM::SortClause'    => [
			       expr => '$',
                               ord  => '$',
			       ];

struct 'TM::ListIterator'  => [
			       list       => '$',
                               var        => '$',
			       ];

struct 'TM::ConstraintIterator' => [
                                constraint => '$',
				tau_expr   => '$',
                                ];

struct 'TM::CurledString'  => [
				lst        => '@',
				];

struct 'TM::Numeric'       => [
                                val        => '$',
			        ];

struct 'TM::OrElses'       => [
				lst        => '@',
				];

struct 'TM::List'          => [
                                lst        => '@',
                                slice      => '$',
                                ];

struct 'TM::XMLFragmentList'=> [
				list       => '$',
                                ];

struct 'TM::XMLElement'    => [
                                start      => '$',
                                end        => '$',
                                attrs      => '@',
                                content    => '$',
				];

struct 'TM::XMLAttribute'  => [
				name       => '$',
                                value      => '$',
				];

#-- belongs to its own package later

struct 'TM::AsTMaPathVar'  => [
                                var        => '$',
			        steps      => '@',
				];

struct 'TM::AsTMaPathStep' => [
			        step       => '$',
			        tests      => '@',
			       ];

struct 'TM::ListSlice'     => [
                                start      => '$',
			        end        => '$',
                                ];

struct 'TM::AsTMaPathTD'   => [
			        axis       => '$',
			        type       => '$',
                                scope      => '$',
			        select     => '$',
			       ];

struct 'TM::AsTMaPathTL'   => [
			        role       => '$',
                                type       => '$'
			       ];

struct 'TM::AsTMaPathAD'   => [
                                axis       => '$',
                               ];


struct 'TM::AsTMaPathNull' => [];




 =pod


__END__

    # check F.5.2.2 (new one pointing subjectIndicator to existing)
    foreach my $r ($t->{subjectIdentity} &&
                   $t->{subjectIdentity}->{references} ?
                   @{$t->{subjectIdentity}->{references}}: ()) {
#       print "afound reference for ", Dumper $r, "\n";
      if (isa($r, 'XTM::topicRef')) {
#         print "xfound reference for ", Dumper $r, "\n";
        my $t2; eval { $t2 = $self->{topics}->{$r->{href}} };               # try to find this topic in local map
        if ($t2) {
          # get rid of this topicRef, it is useless now
          $t->{subjectIdentity}->references ( [ grep ($_ != $r, @{$t->{subjectIdentity}->{references}}) ] );
          $self->__do_merge ($t2, $t, "new one pointing subjectIndicator to existing");
          return; # no need to do more with this topic
        }
      }
    }
    # check F.5.2.2 (existing one pointing subjectIndicator to new)
    {
      my $tid2;
#warn "topic in focus ", $t->id, Dumper $t;
#warn "subjects ", Dumper $self->{subjects};
#warn "our own ids " , Dumper $t->ids;
      if (($tid2 = $self->{subjects}->{$t->{id}}) && (!grep ($tid2 eq $_, @{$t->{ids}}))) { # this particular topic is topicRef'd by someth
ing
#warn "short before merging";
        my $t2 = $self->{topics}->{$tid2};
        $t2->{subjectIdentity}->references ( [ grep ($_->{href} ne $t->{id}, @{$t2->{subjectIdentity}->{references}}) ] );
        $self->__do_merge ($t2, $t, "existing one pointing subjectIndicator to new");
        return;
      }
    }

__END__



    return _scan_merge ($store, $trans, $cons, @_) unless $t->[SID];     # no SID => no new merging necessary, continue

    if (($t2 = _id_toplet_by_sid ($store, $t->[SID])) &&                 # subject identifier is already there
	 $t2 != $t) {                                                    # and it is not the same as the one we have
      return _do_merge ($store, $trans, $cons, $t, $t2, @_);
    }

    foreach my $sin (map {$_->[VALUE]}                                   # filter values
		     grep ($_->[KIND] == TM::Maplet::KIND_SIN, @{$t->[CHARS]})) { # only subject indicators
                                                                         # check F.5.2.3 (share at least on URI in references)
      if (($t2 = _id_toplet_by_ind ($store, $sin)) &&                    # subject indicator is already there
           $t2 != $t) {                                                  # and it is not the same as the one we have
	return _do_merge ($store, $trans, $cons, $t, $t2, @_);
      }
    }



    return _scan_merge ($store, $trans, $cons, @_);                      # if no merge, continue with other candidates

  } else {
    return _scan_merge ($store, $trans, $cons, @_);


# share subjec indicator?
    foreach my $r ($t->{subjectIdentity} && $t->{subjectIdentity}->{references} ?
                   @{$t->{subjectIdentity}->{references}} : ()) {
      my $uri = URI->new($r->{href})->canonical->as_string;            # canonical is better
#warn "checking for ", Dumper( $t), "sharing", $uri;
      my $tid2;
#warn "comparing with subjects " , Dumper $self->{subjects};
      if (($tid2 = $self->{subjects}->{$uri}) && (!grep ($tid2 eq $_, @{$t->{ids}}))) {# yes we share a subjectIndicator with ourselves, ..
        my $t2 = $self->{topics}->{$tid2};                               # but that is not the point
        $self->__do_merge ($t2, $t, "sharing a subjectIndicator");
        return;
      }
    }


 =item constraints

A list of AsTMa! constraints

 =item functions

Hash with names as keys and AsTMa? queries as values.

package TM::Maplet;


require Exporter;
use base qw(Exporter);

 =pod

 =head1 DESCRIPTION

 =head2 Toplets and Maplets

Where the classical introductions of XTM Topic Maps introduce the
concept of a topic and the association, we here have a slightly
different view to organize Topic Map data.

A I<maplet> is very similar to an association in that it consists of a
association type topic, topics for roles and player topics and a topic
for the scope of the association. A I<full maplet> also includes B<ALL>
information about the involved topics. In this implementation you only
get the references to the topics.

Or rather not to the topics, but to I<toplet>s which consist only of
topic characteristics, like the (scoped) basename, (scoped)
occurrences, subject indicators and reification information.

 =head2 Templates


TBD


 =head1 INTERFACE

 =head2 Functions

Most functions are rather low-level and auxiliary. They may only be of use
if you need to manipulate the in-memory data structure.

 =over


 =item B<players>

On some occasions you want to extract the players for one (or more)
particular roles out of a maplet. This function expects a maplet and
an identifier for the role. It will return the list (reference) of
those topics which are players for this role.

 Example:

    print join (", ", @{players ($maplet, 'role123')});

 =cut

sub players { # finds in maplet m role r and returns (list of) players for that role
  my $m = shift;
  my $r = shift;
  my $i = -1; # using an ARRAY counter (brr) and have to preincrement it

  return map { ($i++, $_ eq $r) ? $m->[PLAYERS]->[$i] : () } @{$m->[ROLES]};
}

 =pod


 =cut


1;


use constant KIND_SIN => 0;
use constant KIND_BN  => 1;
use constant KIND_OC  => 2;
use constant KIND_IN  => 3;



sub assert_toplet {
    my $self = shift;
  


##use Data::Dumper; warn "asserting toplets: ".Dumper \@_;
  my @added;
  foreach my $tid (@_) { # add them to the store
##warn "working on '$tid'";
    my $t;
    if (ref ($tid) eq 'Toplet') {                                                  # no need to generate a Toplet
      $t = $tid;

    } elsif (ref ($tid) eq '') {                                                   # a single id or URI
      unless ($t = $self->toplet ($tid)) {                                         # if it is not there, let's create it
##warn "in assert toplet generate new toplet for '$tid'";
	$t = $tid =~ /^\w+:/ ?
	              new Toplet (id => _generate_id(),                            # no id, so we make one
				  subject_identifier => $tid) :                    # and use the URI as subject identifier
                      new Toplet (id => $tid);
      }
    } else {
      die "unknown component";
    }

    $self->{toplets}-> {$t->[ID]}  = $t;                                           # add to primary storage
    push @added, $t;                                                               # remember this to be returned

    # add to indices
    _update_index ($self, 'subjects', $t->[SID],  undef, $t)
      if $t->[SID];                                                                # link subject identifier to topic

##warn "updating indeces for $t->[ID]".Dumper $t;
    foreach my $ch (@{$t->[CHARS]}) {                                              # we could restrict this to basenames??
      _update_index ($self, 'characteristics', TM::Maplet::hash ($ch),  undef, $t);
      _update_index ($self, 'indicators',      $ch->[VALUE],            undef, $t)
	if $ch->[KIND] == TM::Maplet::KIND_SIN;                                    # only for indicators
    }
  }
  return wantarray ? @added : $added[0];
}

sub _eval_list {
    my $store = shift;                       # the map to look at
    my $ls    = shift;                       # always list reference
    my @ss    = @_;                          # always list of next steps
    
    if (@ss) {                               # IF there are steps to do
        if (ref ($ss[0] =~ /APathPred/) {        # and IF this is a predicate
                                                 # this means that the list will shrink, only those which survive the predicate will be there
	    my @ls2 = grep ( _eval_l ($store, $_, @ss),  @$ls);
	} else {                                 # otherwise we will build a completely new list
	    my @l2;                              # here we collect ALL results
	    foreach my $l (@$ls) {                   # forall toplets/maplets, make all steps
		push @l2, _eval_l ($store, $l, @ss);   # collect results
	    }
	    return @l2;
	}
    } else {                                   # no steps, so we are happy with
	return @$ls;                           # the original list
    }
}

sub _eval_l {
  my $store = shift;                          # the map to look at
  my $l     = shift;                          # one thinglet
  my $s     = shift;                          # next step
  my @ss    = @_;                             # remaining steps

  my @l2 = _make_step ($s, $l, $store);
  return @ss ? _eval_list ($store, \@l2, @ss) : @l2;
}


struct 'TemplateAnyID'                     => [ sid   => '$'                                             ]; # any assertion which has this as type, scope, role or player
struct 'TemplateIPlayer'                   => [                             iplayer => '$' ];
struct 'TemplateIRoleType'                 => [ type  => '$', irole => '$'                 ]; # 
struct 'TemplateType'                      => [ type  => '$'                               ]; # 
struct 'TemplateIPlayerIRole'              => [               irole => '$', iplayer => '$' ];
struct 'TemplateIRolePlayerRoleType'       => [ type  => '$', arole => '$', aplayer => '$', brole => '$' ];  # 2 roles associated, plus type
struct 'TemplateXRolePlayerRoleType'       => [ type  => '$', arole => '$', aplayer => '$', brole => '$' ];  # full, 2 roles associated, plus type, exact
struct 'TemplateIRolePlayerRolePlayerType' => [ type  => '$', arole => '$', aplayer => '$', brole => '$', bplayer => '$' ];  # full, 2 roles associated, plus type
struct 'TemplateXRolePlayerRolePlayerType' => [ type  => '$', arole => '$', aplayer => '$', brole => '$', bplayer => '$' ];  # full, 2 roles associated, plus type, exact
struct 'TemplateIPlayerType '              => [ type  => '$',               iplayer => '$' ];
struct 'TemplateIPlayerIRoleType'          => [ type  => '$', irole => '$', iplayer => '$' ];
struct 'TemplateWildcard'                  => [];
struct 'TemplateCharacteristics'           => [               irole => '$' ];


 =item B<path>

I<@sids> = I<$ms>->path (I<$path_expression>, \I<@sids>)

This method can be used to evaluate a path expression in the context
of a topic map store and a list of toplets or maplets 

????? Example:

   my TM;
  ??? my $tm = new TM (....);
??   print Dumper $ap->eval ($tm->store, [ $tm->sid ('james', 'john', 'jack') ]);

The map passed in as first parameter must be an ins
L<TM::Retrieve>.  Accordingly, it can be a memory-based map, one which is
slurped from some file, a complete virtual map or a combination of
those.

The path expression will be evaluated for each of the
toplets/maplets. The outcome for each of them is a list of
toplets/maplets. All these lists are combined into one which is
finally returned.

TBD: with variable bindings
????

???val, no listr

^==cut

sub path {
  my $self  = shift;
  my $pe    = shift;
  my $val   = shift;                                    # optional, WILL BE IGNORED FOR NOW, NOT YET IMPLEMENTED

  my $exist = shift;                                    # if set then we will not generate ALL, but stop at the first 
                                                        # (usually important when evaluated inside predicates with exists semantics)
                                                        # ??? NOT YET IMPLEMENTED
  my $bind  = shift || {};                              # variable bindings ??? NOT YET IMPLEMENTED

##warn "pe ap: ". Dumper $pe;

  my @ls;
  if (! ref($pe->{base})) {
      @ls = keys %{$self->{assertions}};
  } elsif ($pe->{base}->[0] eq 'all') {
      @ls = keys %{$self->{assertions}};
  } elsif ($pe->{base}->[0] eq 'topic') {
      @ls = $self->mids ($pe->{base}->[1]);
  } elsif ($pe->{base}->[0] eq 'topic') {
      die "cannot handle this yet";
  }

# COND: @ls is identifier list
  foreach my $postfix (@{$pe->{cexpr}}) {
#warn "before step \@ls" . Dumper \@ls;
      my $step  = $postfix->step;
      my $rstep = ref ($step);                                          # the class of it (just a short cut to avoid repeated computation)
#warn "  this step ".Dumper $step;

# deal with step first, compute outgoing tuple sequence
      if ($rstep eq 'APathPredSC') {                                    # shorthand for [ * type-id ]

# PRECOND: @ls is assertion list
	  my $id = $self->sid ($step->id);
#warn "shortcut $id";
	  if ($step->kind eq ':') {
	      @ls = grep ($self->is_subclass ($_->[TYPE], $id), @ls);   # later add more sophisticated is-a
	  } elsif ($step->kind eq '@') {
	      @ls = grep ($_->[SCOPE] eq $id, @ls);                     # later add more sophisticated is-a
	  } elsif ($step->kind eq '^') {
	      die "reifier predicate not yet implemented";
	  } else {
	      die scalar __PACKAGE__ . ": inconsistent data structure";
	  }
# POSTCOND: @ls is assertion list



      } elsif ($rstep eq 'APathPred') {
# PRECOND: @ls is assertion list
	  if ($step->pe2) {
	      die "dual predicate not yet implemented";
	      foreach my $l (@ls) {
		  my @ls1 = $self->path ($step->pe1, [ $l ]);
		  my @ls2 = $self->path ($step->pe2, [ $l ]);
	      # depending on the operator, much optimization possible here
	      }
	  } else {
	      my $pe1 = $step->pe1;
	      @ls = grep ( $self->path ($pe1, [ $_ ], 1), @ls);
	  }
# POSTCOND: @ls is assertion list



      } elsif ($rstep eq 'APathInNavi') {

# PRECOND: @ls identifier list
	  my $ro = $self->sid ($step->irole);
	  my $ty = $self->sid ($step->atype);
	  my @ls2;
	  if ($ty) {
	      foreach (@ls) {
		  my $tpl = TemplateIPlayerIRoleType->new ( type => $ty, irole => $ro, iplayer => $_);
#warn "in innavi -> $ro \\ $ty : template :".Dumper $tpl;
		  push @ls2, $self->match ($tpl);
	      }
	  } else {
	      foreach (@ls) {
		  my $tpl = TemplateIPlayerIRole->new ( irole => $ro, iplayer => $_);
#warn "in innavi -> $ro  : template :".Dumper $tpl;
		  push @ls2, $self->match ($tpl);
	      }
	  }
	  @ls = @ls2;
# POSTCOND: @ls is assertion list


      } elsif ($rstep eq 'APathOutNavi') {

# PRECOND: @ls assertion list
	  my $ro = $self->sid ($step->orole);
	  @ls = map { $self->get_players ($_, $ro) } @ls;
# POSTCOND: @ls is identifier list


      } elsif ($rstep eq 'APathChar') {                 # something like /bn, here we only have to find the assertions for a given $l as player for 'topic'

# PRECOND: @ls is identifier list
	  my @ls2;
	  foreach my $l (@ls) {
	      my $tpl =	TemplateIRolePlayerRoleType->new ( type    => $self->sid ($CharInfo[$step->kind]->[0]),
							   arole   => $self->sid ($TM::PSI::THING),
							   aplayer => $l,
							   brole   => $self->sid ($CharInfo[$step->kind]->[1]) );
#warn "path, creating template ".Dumper $tpl;
	      push @ls2, $self->match ($tpl);
	  }
	  @ls = @ls2;
# POSTCOND: @ls is assertion list



      } else {
	  die "unimplemented path step";
      }
#warn "after ". Dumper \@ls;

# maybe this outgoing tuple sequence has to be sorted?
##    @ls = _path_sort ($postfix->order, @ls) if $postfix->order;
      die "sorting not yet implemented" if $postfix->order;
      die "uniquing not yet implemented" if $postfix->unique;
  }
  return map { ref ($_) ? $_->[SID] : $_ } @ls;                  # we return SIDs
}

^==pod

sub xsi_assert {
    my $self = shift;
    my $si   = $self->{si};                                 # shorthand
    my %vs   = @_;

    while (my ($k, $v) = each %vs) {
	push @{$si->{$k}}, $v;
    }
}


sub xsi {
    my $self = shift;
    my $si   = $self->{si};                                 # shorthand

  if (wantarray) {
      return map { $si->{$_} ||= [], $si->{$_} } @_;
  } else {
      my $id = shift;
      $si->{$id} ||= [];
      return $si->{$_};
  }
}

sub xsid {
    my $self = shift;



#    return TM::PSI::sid ($self->{si}, @_);    
}

^==pod

^==item B<mkabs>

I<$uri> = I<$ms>->mkabs (I<$identifier>);

???
This function takes a reference to a hash of SIDs, an Base URI string and a identifier and forms an absolute SID which
is put into the SIDs hash.

^==cut

sub xmkabs {
    my $self = shift;
    my $SIDs = $self->{si};
    my $base = $self->{baseuri};

    my $id   = shift;

    return undef unless defined $id;
    my ($sid) = TM::PSI::sid ($SIDs, $id);
#warn "see if $id is URL";
    $sid =~ /\w+:.+/ and return $sid;
#warn "make url from $id using $base";
    return $SIDs->{$id} = $base.$id;
}



#    # set up indices for faster access
#    # since this is redundant information, it has to be kept in sync, this is mainly the task of 'consolidate'
#    # NOTE: highly experimental
#    # the thinking is: "if the index is there, it is complete and can be used, if not, then do it the Bond way (=manually)
#    $self->{indices} = {
#			subjects        => {},
#			characteristics => {},
##			maplets         => {},
#			indicators      => {},
#		       };


#    _update_index ($self, 'subjects', $t->[SID], $t, undef) if $t->[SID];
#
#    foreach my $ch (@{$t->[CHARS]}) {                                              # we could restrict this to basenames??
#      _update_index ($self, 'characteristics', $ch->[HASH],  $t, undef);
#      _update_index ($self, 'indicators',      $ch->[VALUE], $t, undef)
#        if $ch->[KIND] == TM::Assertion::KIND_SIN;                                      # only for indicators
#    }


   { # Floyd-Warshall
       my @vertices = keys %graph;

       foreach my $k (@vertices) {
 	  foreach my $i (@vertices) {
 	      foreach my $j (@vertices) {
 		  $graph{$j}->{$i} = $graph{$i}->{$j} = 1 if ($graph{$k}->{$j} &&
		                                              $graph{$i}->{$k})
                                                              ||
                                                             ($graph{$j}->{$k} &&
                                                              $graph{$k}->{$i});
 	      }
 	  }
       }
   }

  foreach my $this (sort keys %graph) {
      my @c     = sort ($this, keys %{$graph{$this}}); # list has at least 2 elements
      my $this  = shift @c;
      my $thism = $mid2iid->{$this};
      foreach my $that (@c) {
	  my $thatm = $mid2iid->{$that};                           # shorthand
	  next if $thatm == $thism;                  # we already have merged
	  die "two different subject addresses for two topics to be merged ($this, $that)" 
	      if $thism->[ADDRESS] and $thatm->[ADDRESS] and 
                 $thism->[ADDRESS] ne  $thatm->[ADDRESS];
warn "merge now $that > $this";
                 $thism->[ADDRESS]  ||=   $thatm->[ADDRESS];                 # first subject address, then indicators
	  push @{$thism->[INDICATORS]}, @{$thatm->[INDICATORS]};
	  $mid2iid->{$that} = $thism;
      }
#warn "after post-merger ". Dumper $mid2iid;
   }


__END__

 =head3 Capabilities

Every implementation of a map must specify its capabilities to synchronize
with the external resource the content is drawn from. A driver reading from an
XTM file might have C<BATCH_IN> capability, a driver able to write to an AsTMa
file might have C<BATCH_OUT>.

The following constants are defined:

  constant       use it for a driver which can.... from external resource
  -----------------------------------------------------------------------
  BATCH_IN       read a whole map
  BATCH_OUT      write a whole map
  LIVE_IN        directly read maplet information
  LIVE_OUT       directly write maplet information

 =cut

use constant BATCH_IN  => 1;
use constant BATCH_OUT => 2;
use constant LIVE_IN   => 4;
use constant LIVE_OUT  => 8;

 =pod

 =over

 =item B<capabilities>

I<$listref> = I<$tm>->capabilities

This method returns a list (reference) containing a subset of the above constants;

Example:

   die "cannot write map" unless grep ($_ == BATCH_OUT, @{ $tm->capabilities });

 =cut

sub capabilities {
  die "abstract method";
}

 =pod


__END__

^==item B<consolidate>

__END__



  my $self = shift;
  my $mq   = shift;
  my $mqt  = $mq->[TM::Retrieve->CLASS];

  if ($mqt eq 'TypeRolePlayerRoleIsPlayer') {
      my $tpl = TemplateIRolePlayerRoleType->new ( type    => $self->sid ($mq->[TM::Retrieve::MTYPE]),
						   arole   => $self->sid ($mq->[TM::Retrieve::AROLE]),
						   aplayer => $self->sid ($mq->[TM::Retrieve::APLAYER]),
						   brole   => $self->sid ($mq->[TM::Retrieve::BROLE]) );
##warn "mat match tmpl ".Dumper $tpl;

      my @a   = $self->{store}->match ($tpl);
      return $tpl->arole lt $tpl->brole ?                                # we know that the template is canonicalized in the process
	        map { $_->[TM::Store::PLAYERS]->[1] } @a :           # so, we only have to figure out how the players are ordered
	        map { $_->[TM::Store::PLAYERS]->[0] } @a;


  } elsif ($mqt eq 'TypeRolePlayerIsAssoc') {
      my $tpl = TemplateIPlayerIRole->new        ( type    => $self->sid ($mq->[TM::Retrieve::MTYPE]),
						   irole   => $self->sid ($mq->[TM::Retrieve::AROLE]),
						   iplayer => $self->sid ($mq->[TM::Retrieve::APLAYER]));
#??? copy structure here ????
      return map { bless $_, 'Maplet' } $self->{store}->match ($tpl);


  } elsif ($mqt eq 'TypeRoleIsPlayer') {
      my $tpl = TemplateIRoleType->new           ( type    => $self->sid ($mq->[TM::Retrieve::MTYPE]),
						   irole   => $self->sid ($mq->[TM::Retrieve::AROLE]));
#warn "match a template ".Dumper $tpl;
my @a = $self->{store}->match ($tpl);
#warn "assertions matched ".Dumper \@a;

      return map { TM::Store::get_player ($_, $tpl->irole) } $self->{store}->match ($tpl);


  } else {
      die scalar (__PACKAGE__). ": Unknown micro query '$mqt'";
  }

  return $self->{store}->match (shift);
}


^==item B<sid>

This method takes one (or a list of) identifier(s) and tries to make
them absolute (see L<TM::Retrieve>).

^==cut

sub sid {
    my $self = shift;
    return $self->{store}->sid (@_);
}

^==pod



__END__


 =item B<toplets>

???????????????????????

I<$list_ref> = I<$tm>->topics ( [ I<$simple_query> ] )

The method returns a list reference of topic ids satisfying the given
query condition. If the condition is undef, then all topic ids are
returned.

Example:

   # get all of them (or at least what the implementation is willing to give)
   @greedy_user = @{$tm->topics ()};

   # only with some name
   @modest_user = @{$tm->topics ( "baseName regexps /rumsti.*/" )};

The query string follow the syntax:

  query         -> clause { 'AND' clause }
  clause        -> 'baseName'   'regexps' regexp_string                               |
                   'occurrence' 'regexps' regexp_string                               |
                   'text'       'regexps' regexp_string                               | # any text within the topic
                   'id'         'regexps' regexp_string                               |
                   'id'         'eq'      ''' string '''                              |
                   'assocs' [ 'via' topic_id ] [ 'with' topic_id ] [ 'transitively' ] |
                   'is-a'  topic_id                                                   |
                   'reifies'    'regexps' regexp_string                               |
                   'indicates'  'regexps' regexp_string                               |
##                 'instanceOfs' ( '<=' | '==' | '>=' )  set_of_topic_ids | NOT IMPLEMENTED
##                 'scoped_by' topic_id   ## NOT IMPLEMENTED
  regexp_string -> '/' regexp '/'
  regexp        -> <a perl pattern>
  topic_id      -> <id of a topic>
  string        -> <any string with no \' in it>

The regexps are all interpreted as //i, i.e. case-insensitive matching.

 =cut

sub _passes_filter {
    my $store   = shift;
    my $t       = shift;
    my $f       = shift;
    my $memoize = shift;
    
    if ($f =~ /^baseName\s+regexps\s+\/(.+)\/$/) {
	my $regexp = $1;
#    elog ('TM::Memory', 4, "    baseName regexps '$regexp'");
	foreach my $b (@{$t->{baseNames}}) {  # only one matches => ok
#      elog ('TM::Memory', 5, "       baseName", $b);
	    return 1 if $b->{baseNameString}->{string} =~ /$regexp/i;
	}
    } elsif ($f =~ /^indicates\s+regexps\s+\/(.+)\/$/) {
	my $regexp = $1;
	if ($t->{subjectIdentity}) {
	    foreach my $r (@{$t->{subjectIdentity}->{references}}) {
		return 1 if $r->{href} =~ /$regexp/i;
	    }
	}
	return 0;
    } elsif ($f =~ /^reifies\s+regexps\s+\/(.+)\/$/) {
	my $regexp = $1;
	return 
	    $t->{subjectIdentity} && 
	    $t->{subjectIdentity}->{resourceRef} &&
	    $t->{subjectIdentity}->{resourceRef}->{href} =~ /$regexp/i;
    } elsif ($f =~ /^occurrence\s+regexps\s+\/(.+)\/$/) { # make no distinction between resourceRef and resourceData
	my $regexp = $1;
	foreach my $o (@{$t->{occurrences}}) {
##      return 1 if $o->baseName && $o->baseName->baseNameString->string =~ /$regexp/i;
	    return 1 if $o->{resource}->isa ('TM::resourceRef')  && $o->{resource}->href =~ /$regexp/i;
	    return 1 if $o->{resource}->isa ('TM::resourceData') && $o->{resource}->data =~ /$regexp/i;
	}
    } elsif ($f =~ /^id\s+regexps\s+\/(.+)\/$/) {
	my $regexp = $1;
	return grep ($_ =~ /$regexp/i, @{$t->ids});
    } elsif ($f =~ /^text\s+regexps\s+\/(.+)\/$/) {
	my $regexp = $1;
	foreach my $b (@{$t->{baseNames}}) {  # only one matches => ok
	    return 1 if $b->{baseNameString}->{string} =~ /$regexp/i;
	}
	foreach my $o (@{$t->{occurrences}}) {
	    return 1 if $o->{baseName} && $o->{baseName}->{baseNameString}->{string} =~ /$regexp/i;
	    return 1 if $o->{resource}->isa ('TM::resourceRef')  && $o->{resource}->href =~ /$regexp/i;
	    return 1 if $o->{resource}->isa ('TM::resourceData') && $o->{resource}->data =~ /$regexp/i;
	}
    } elsif ($f =~ /^id\s+eq\s+\'(.+)\'$/) {
	return grep ($_ eq $1, @{$t->{ids}});
    } elsif ($f =~ /^assocs(\s+via\s+(\S+))?(\s+as\s+(\S+))?(\s+with\s+(\S+))?(\s+transitively)?$/) {
	my $via   = $1 ? $2 : '';
	my $role  = $3 ? "#$4" : undef;
	my $with  = $5 ? $6 : '';
	my $trans = $7 || '';
	my $id    = $t->id;
	
#    elog ('TM::Memory', 4, "    assocs via '$via' ".($role ? "role '$role'" : "")." with '$with', $trans");
	my $assocs = $memoize->{"a_instances => $via"} ||
die "broken code self unknown";
###	    ($memoize->{"a_instances => $via"} = $via ? $self->associations ("is-a $via") : $self->associations ());
	
	if ($with) { # then we better start from there, performance
	    ($id, $with) = ($with, $id);
#      elog ('TM::Memory', 4, "    assocs via with optimization");
	}
	my $s = $memoize->{$id.$trans} || 
die "broken code self unknown";

###	    ($memoize->{$id.$trans} = $self->_topic_tree ($id, {}, $assocs, undef, undef, 0, $trans ? undef : 1));
#    elog ('TM::Memory', 5, "        tree", $s);
	return 0 if ($via  && !scalar             @{$s->{'children*'}}); # no topics via via reached
	return 0 if (         !grep ($with eq $_, @{$s->{'children*'}}));
#    elog ('TM::Memory', 4, "       passed via, with");
	return 1;
    } elsif ($f =~ /^is-a\s+(.+)$/) {
	my $type = $store->toplet (id => $1) || $store->toplet (subject_identifier => $1) || $log->error_die ("unknown topic $1");
	return @{$store->maplets (new TM::Maplet (scope   => 'pxtm-universal-scope',
						   type    => 'xtm-psi-class-instance',
						   roles   => [ 'xtm-psi-class', 'xtm-psi-instance' ],
						   players => [ $type->id, $t->id ]),
				  'exists')};
    } else {
	$log->error_die ("Unimplemented filter '$f'\n");
    }
    return 0;
}



sub topics {
    my $self   = shift;
    my $store  = $self->{store};
    my $filter = shift || '';
    my $from   = shift || 0;
    my $to     = shift || undef;


    $main::log->debug ("filter $filter, $from -> ". (defined $to ? $to : undef));

    return [ keys %{$store->{toplets}} ] unless $filter;                 # shortcut

    my @ids;
    my $i = 0;                                                           # indexes the matches

    $filter =~ s/\s+$//;                                                 # strip leading and
    $filter =~ s/^\s+//;                                                 # trailing blanks
    my @filters = split (/\s+and\s+/i, $filter);                         # poor man's parsing, only ANDed clauses, no brackets

    my %memoize;                                                         # optimization: store intermediary results temp for this query
    my %seen;                                                            # only used for the uniq sort below
  TOPICS:
    foreach my $t ( grep (!$seen{$_}++, values  %{$store->{toplets}} )) { # they could be the same entries with different ids
	last TOPICS if defined $to && $i > $to;                          # have enough
	$log->debug ("  working on ".$t->id);

	foreach my $f ( @filters ) { # only ANDed clauses, yet
	    $log->debug ("        check filter $f");
	    next TOPICS unless _passes_filter ($store, $t, $f, \%memoize);
	    $log->debug ("        passed");
	}
	push @ids, $t->id if ($from <= $i++);
    }
    return [ @ids ];
}




#-- Templates -------------------------------------------------------------

struct TTemplate => [];

struct MTemplate => [
    class   => '$',
    type    => '$',
    arole   => '$',
    aplayer => '$',
    brole   => '$',
    bplayer => '$',
];

# TypeRolePlayerRoleIsPlayer: given a association type + role/player + role  returns the other player
# TypeRolePlayerIsAssoc     : given a association type + role/player         returns whole association
# TypeRoleIsPlayer          : given an association type + role               returns the player of that role

use constant {
    CLASS   => 0,
    MTYPE   => 1,
    AROLE   => 2,
    APLAYER => 3,
    BROLE   => 4,
    BPLAYER => 5,
};




 =head3 Toplet and Maplet Templates

 =over

 =item B<mtemplate>

I<$tpl> = I<$tm>->mtemplate (I<$class>, I<$id>, ....)

This will create a template for maplet searches using B<match>.

???classes available, parameters

????

 =cut

sub xxxmtemplate {
    my $self  = shift;

    my $tpl   = [];
    $tpl->[CLASS] = shift;

    if ($tpl->[CLASS] eq 'TypeRolePlayerRoleIsPlayer') {
	($tpl->[MTYPE],
	 $tpl->[AROLE],
	 $tpl->[APLAYER],
	 $tpl->[BROLE]) = TM::PSI::sid ($self->{store}->{si}, @_);
    } elsif ($tpl->[CLASS] eq 'TypeRolePlayerIsAssoc') {
	($tpl->[MTYPE],
	 $tpl->[AROLE],
	 $tpl->[APLAYER]) = TM::PSI::sid ($self->{store}->{si}, @_);
    } elsif ($tpl->[CLASS] eq 'TypeRoleIsPlayer') {
	($tpl->[MTYPE],
	 $tpl->[AROLE]) = TM::PSI::sid ($self->{store}->{si}, @_);
    } else {
	die scalar __PACKAGE__.": Unknown mtemplate '".$tpl->[CLASS]."'";
    }
#warn "created tmpl ".Dumper $tpl;
    return $tpl;
}

 =pod
 
 =item B<ttemplate>

I<$tpl> = I<$tm>->ttemplate (I<$class>, ?????I<$id>, ....)

???creates ttemplate



 =over

 =item C<default_consistency>: all but implicit follow-up of topic references to other maps

 =item C<max_consistency>: all

 =item C<backward_consistency>: backward compatible behavior


our $default_consistency  = {merge                 => [ qw(Topic_Naming_Constraint Subject_based_Merging Id_based_Merging) ],
                             duplicate_suppression => [ qw(Subject_Indicator Topic_Name Association Role_Player) ],
                             follow_maps           => [ qw(explicit) ]};

our $max_consistency      = {merge                 => [ qw(Topic_Naming_Constraint Subject_based_Merging Id_based_Merging) ],
                             duplicate_suppression => [ qw(Subject_Indicator Topic_Name Association Role_Player) ],
                             follow_maps           => [ qw(explicit implicit) ]};

our $backward_consistency = {merge                 => [ ],
                             duplicate_suppression => [ ],


 =item B<info>

I<$hashref> = I<$tm>->info (I<info-specification>)

returns some meta/statistical information about the map in form of
a hash reference containing one or more of the following components (you might
want to discover the return values with Data::Dumper):

^==over

 =item (a)

I<informational>: this hash reference contains the number of topics, the number of associations,
the UNIX date of the last modification and synchronisation with the external tied object and
a list reference to other topic maps on which this particular map depends.

 =item (b)

I<warnings>

This hash reference contains a list (reference) of topic ids of topics I<not_used> anywhere in the map.
There is also a list (I<no_baseName>) of topics which do not contain any baseName (yes this is allowed in section
3.6.1 of the standard).

 =item (c)

I<errors>

This component contains a list reference I<undefined_topics> containing a list of topic identifiers
of topics not defined in the map. 

 =item (d)

I<statistics>

This component contains a hash reference to various statistics information, as the number of clusters,
maximum and minimum size of clusters, number of topics defined and topics mentioned.


TODOs:

^==over

 =item

detect cyclic dependency of topic types

^==back

^==back

You can control via a parameter in which information you are interested in:

Example:

   $my_info = $tm->info ('informational', 'warning', 'errors', 'statistics');


^==cut

sub info {
  my $self  = shift;
  my @what  = @_;

  my $info;
  my $usage;

  foreach my $w (@what) {
    if ($w eq 'informational') {
      $info->{$w} = { #address     => $self,
		      nr_topics   => scalar @{$self->topics},
		      nr_assocs   => scalar @{$self->associations},
		      last_mod    => $self->{last_mod},
		      last_syncin => $self->{last_syncin},
		      depends     => [ map { $_->{memory}->{id} } @{$self->{depends}} ],
		      tieref      => ref ($self->{tie}),
		      id          => $self->{memory} ? $self->{memory}->{id} : undef
		    };
    } elsif ($w eq 'warnings') {
      # figure out those topics which do not seem to have a single baseName
      $info->{$w}->{'no_baseName'} = [];
      foreach my $tid (@{$self->topics()}) {
	push @{$info->{$w}->{'no_baseName'}}, $tid unless $self->topic($tid)->baseNames && @{$self->topic($tid)->baseNames};
      }
      $usage = $self->_usage() unless $usage;

sub _usage {
  my $self = shift;

  my $usage;
  # figure out which topics are used as topicRef (scope, member, role, instanceOf)
  foreach my $tid (@{$self->topics()}) {
    # instanceOfs
    foreach my $i (@{$self->topic($tid)->instanceOfs}) {
      $usage->{as_instanceOf}->{$1}++ if $i->reference->href =~ /^\#(.+)/;
      $usage->{as_instance}->{$tid}++ unless $i->reference->href eq $XTM::PSI::xtm{topic};
    }
    # scopes
    foreach my $b (@{$self->topic($tid)->baseNames}) { 
      foreach my $s (@{$b->scope->references}) {
	if ($s->href =~ /^\#(.+)/) {
	  $usage->{as_scope}->{$1}++;
	}
      }
    }
    foreach my $o (@{$self->topic($tid)->occurrences}) { 
	if ($o->instanceOf->reference->href =~ /^\#(.+)/) {
            $usage->{as_instanceOf}->{$1}++;
	}
        foreach my $r (@{$o->scope->references}) {
	    if ($r->href =~ /^\#(.+)/) {
                $usage->{as_scope}->{$1}++;
	    }
	}
    }
  }
  foreach my $aid (@{$self->associations()}) {
    # instanceOfs
    if (my $i = $self->association($aid)->instanceOf) {
      if ($i->reference->href =~ /^\#(.+)/) {
	$usage->{as_instanceOf}->{$1}++;
      }
    }
    foreach my $m (@{$self->association($aid)->members}) {
      # roles
      if ($m->roleSpec) {
	$usage->{as_role}->{$1}++ if ($m->roleSpec->reference->href =~ /^\#(.+)/);
      }
      # members
      foreach my $r (@{$m->references}) {
	$usage->{as_member}->{$1}++ if ($r->href =~ /^\#(.+)/);
      }
    }
  }
  return $usage;
}
      use Data::Dumper;
##      print STDERR Dumper \%as_instanceOf, \%as_scope, \%as_member, \%as_role;
##print Dumper $usage;

      $info->{$w}->{'not_used'} = [ 
         grep (! ( $usage->{as_instanceOf}->{$_} || 
		   $usage->{as_instance}->{$_}   || 
		   $usage->{as_scope}->{$_}      || 
		   $usage->{as_member}->{$_}     ||
		   $usage->{as_role}->{$_}), @{$self->topics()}) 
				  ];
    } elsif ($w eq 'errors') {
      $usage = $self->_usage() unless $usage;
      $info->{$w}->{'undefined_topics'} = [
         grep (!$self->is_topic($_), (keys %{$usage->{as_instanceOf}},
				      keys %{$usage->{as_instance}},
				      keys %{$usage->{as_scope}},
				      keys %{$usage->{as_member}},
				      keys %{$usage->{as_role}})
	      )
					    ];
    } elsif ($w eq 'statistics') {
      $usage       = $self->_usage() unless $usage;
#use Data::Dumper;
#print STDERR Dumper ($usage);
      my $clusters = $self->clusters();
      my ($tot, $min, $max) = (0, undef, 0);
      foreach my $c (keys %$clusters) {
	  $tot += scalar @{$clusters->{$c}};
	  $min = $min ? ($min > scalar @{$clusters->{$c}} ? scalar @{$clusters->{$c}} : $min) : scalar @{$clusters->{$c}};
	  $max =         $max < scalar @{$clusters->{$c}} ? scalar @{$clusters->{$c}} : $max;
      }

      $info->{$w} = {
		     nr_topics_defined   => scalar @{$self->topics},
		     nr_assocs           => scalar @{$self->associations},
		     nr_clusters         => scalar keys %$clusters,
		     mean_topics_per_cluster => %$clusters ? 1.0 * $tot / scalar keys %$clusters : 1, # empty map => 1 cluster (do not argue with me here)
		     max_topics_per_cluster  => $max,
		     min_topics_per_cluster  => $min,
		     nr_topics_mentioned     => $tot,
		     };
    }; # ignore other directives
  }
  return $info;
}

^==pod




__END__
^==head2 Events and Synchronisation

Maps - be they virtual or materialized - need to be synchronized with
their data sources. This can happen on a variety of occastions, so
events can be registered at particular maps to trigger synchronisation
in both directions, incoming and outgoing.

All derived packages can make use of the following events:

^==over

^==item C<on_tie>:

This event occurs when a C<TM> object is instantiated.

^==item C<on_change>:

This event occurs when the in-memory content is changed
or when the package realizes that content in a resource has been changed.
The former is detected when a transaction is completed (and not when
simple changes have been done to the C<store>). The latter may be
detected when the application accesses parts of the map.

Implementations may use timestamps and delays to suppress excessive
read/write actions. As such, this feature is NOT immediate. If an
application needs more control, then it should use explicit synchronisation.

^==item C<on_sync>:

This event occurs when the application triggers explicitely
a synchronisation request.

^==item C<on_untie>:

This event occurs when the C<TM> object goes out of scope
and is going to be destructed.

^==back

It is expected that implementations use these events to perform
synchronisation. For support we provide here the C<sync> function.

^==item B<sync>

I<$tm>->sync (I<$event>, I<$direction>)

This method can be used by derived packages to trigger C<sync_in> or
C<sync_out> depending on the event and the direction provided.  As
such, it simply dispatches the methods C<sync_in> or C<sync_out> by
analyzing the events listed in the components of the same names.

This is a very typical behavior for all, real and virtual maps.
Special subclasses may also want to override this method and to
propagate the synchronisation request to embedded objects.

The following constants can be used:

Events:

  on_tie
  on_untie
  on_sync
  on_change

Directions:

  incoming
  outgoing

^==cut

use constant incoming => 0;
use constant outgoing => 1;

use constant on_tie    => 1;
use constant on_untie  => 2;
use constant on_sync   => 4;
use constant on_change => 8;

our %events = ('on_tie'    => on_tie,
               'on_untie'  => on_untie,
               'on_sync'   => on_sync,
               'on_change' => on_change);

our @events = values %events;

sub sync {
  my $self      = shift;
  my $event     = shift;
  my $direction = shift;


##warn "rumsti sync ev $event dir $direction" . Dumper ($self->{sync_in}, $self->{sync_out});

  $self->sync_in  if ($direction == incoming && grep ($event == $_, @{$self->{sync_in}}));

##warn "outgoin ".outgoing;
##warn "rumsti sync outgoinf yes?" . ($direction == outgoing ? "yes" : "no");
##warn "rumsti sync outgoinf do syncout?". ( $direction == outgoing && grep ($event == $_, @{$self->{sync_out}}) ? "yes" : "no");

  $self->sync_out if ($direction == outgoing && grep ($event == $_, @{$self->{sync_out}}));

}

^==pod

##our @EXPORT = qw(incoming outgoing on_tie on_untie on_sync on_change);






^==head2 Materialized Maps

@@@@

Objects of this class are maps which have a snapshot, i.e. a presentation of the whole map in
memory. All subclasses inherit a component C<store> (see L<TM::Store>) and are supposed to provide
methods C<sync_in>, C<sync_out> and C<last_mod>. These methods are used to synchronize content
between the C<store> component and the external resource.

At which times the sychronization occurs can be either left to the defaults or can be controlled
explicitely using modifiers in tau expressions.

^==head2 Virtual Maps

@@@

Virtual maps can hover above a variety of data sources, be they originally organized as
Topic Maps or be they only mapped on-the-fly into Topic Maps. Applications using
implementations of this abstract interface can enjoy a homogeneous topicmappish view of
things like DNS or content in relational databases.



^==head2 Subclassing

All objects of subclasses B<MUST> implement the L<TM::Access> interface and B<MAY> implement the L<TM::Update> interface.

Conceptually, we distinguish between virtual maps (L<TM::Virtual>) and
maps which are loaded from an external source (usually at
instantiation time) and/or saved to a resource (usually at
deconstruction time, L<TM::Materialized>.  Any class you add should be
preferable a (direct or indirect) subclass of those, not this one.

^==head2 Ontology

@@@

While a map in its core functionality may be completely or in part virtual, it still is based on
some static concepts. This I<background ontology> has to be loaded whenever an application wants to
use a concrete virtual map.

Most implementations will come shipped with such a default ontology. Applications, though, may have
the wish to replace this ontology with something which suits them more. This functionality is
provided here.

After that, there should be no distinction between topics and associations coming from the
background ontology and those provided dynamically by the implementation of the virtual map.

For this purpose implementation must export a variable C<$ontology> which can be changed before maps
are instantiated. The application is free to inspect them and to replace them.

The format is expected to be AsTMa=. (Maybe later AsTMa!)




^==head2 Methods

@@@@

split into 

  meta (ontology, sync, lastmod, consistency, ...)

  low (assertions, midlets, match)

  high (maplets, toplets)

  ugly (TMDM :-)



^==head1 Meta Interface

@@@@bla


#-- shared directory of maps
# whenever a thread creates a new map, it will be registered here. at destruction time,
# it will be de-registered.

use threads::shared;
our %directory;

sub _register {
    my $baseuri = shift;
    my $obj     = shift;
warn "register $baseuri";
#    warn "another map with baseURI '$baseuri' already loaded" if $directory{$baseuri};
    $directory{$baseuri} = $obj;                                                      # register in directory
}

sub _deregister {
    my $baseuri = shift;
warn "deregister $baseuri";
#    warn "no map with baseURI '$baseuri' registered, ignoring" unless exists $directory{$baseuri};
    delete $directory{$baseuri};
}

#--------------------------



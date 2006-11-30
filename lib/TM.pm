package TM;

use strict;
use warnings;

require Exporter;
use base qw(Exporter);

our $VERSION  = '1.22';

use Data::Dumper;
# !!! HACK to suppress an annoying warning about Data::Dumper's VERSION not being numerical
$Data::Dumper::VERSION = '2.12108';
# !!! END of HACK

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
    my @as = $tm->retrieve ('id..of...assertion...here');

    # find particular assertions
    my @as = $tm->match_forall (scope   => 'tm://whatever/sss');

    my @bs = $tm->match_forall (type    => 'tm://whatever/ttt',
                                roles   => [ 'tm://whatever/aaa', 'tm://whatever/bbb' ]);

    my @cs = $tm->match_forall (type    => 'tm://whatever/is-subclass-of', 
			        arole   => 'tm://whatever/superclass', 
			        aplayer => 'tm://whatever/rumsti', 
			        brole   => 'tm://whatever/subclass');

    my @ds = $tm->match_forall (type    => 'tm://whatever/isa'
                                instance=> 'tm://whatever/person');


    # perform merging, cleanup, etc.
    $tm->consolidate;

    # find full URI of a topic (uhm, midlet)
    my $mid  = $tm->mids ('person');      # returns tm://whatever/person
    my @mids = $tm->mids ('person', ...)  # for a whole list

    # get all midlets
    my @ms   = $tm->midlets;    

    # taxonomy stuff
    warn "%-|" if $tm->is_a ($tm->mids ('gw_bush', 'moron')); # what a subtle joke

    die unless $tm->is_subclass ($tm->mids ('politician', 'moron');

    # returns Mr. Spock if Volcans are subclassing Aliens
    warn "my best friends: ". Dumper [ $tm->instancesT ($tm->mids ('alien')) ];


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
convenient, fast, albeit not overly comfortable interface. If you prefer more a TMDM-like style of
accessing a map then have a look at L<TM::DM>.

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

=back

If you need to roll your own taxonomy to bootstrap with, you can pass in a structure which has
exactly the same structure as that in L<TM::PSI>.


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
      $self->{usual_suspects} = { map { $_ => mids ($self, $_) } @TM::PSI::Usual_Suspects };
      assert ($self,              map { Assertion->new (type    => $_->[0],
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

B<NOTE>: Not implemented yet!

=back

The optional parameter is a list of constants, all of which are defined in L<TM>. If the list is
empty, then the consistency of the map will be used, otherwise the consistency as defined with this
list will override.

B<NOTE>: In all cases the map will be modified.

B<NOTE>: After merging some of the I<lids> might not be reliably point to a topic.

=cut

# NOTE: Below there much is done regarding speed. First the toplets are swept detecting which have
# to be merged. This is not done immediately (as this is an expensive operation, but a 'merger' hash
# is built. Note how merging information A -> B and A -> C is morphed into A -> B and B -> C using
# the _find_free function.

# That merger hash is then consolidated by following edges until their end, so that there are no
# cycles.

sub consolidate {
  my $self = shift;
  my $cons = @_ ? [ @_ ] : $self->{consistency};                           # override
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

=item B<last_mod>

Returns the last UNIX date of modifying the map content. This comes as a L<Time::HiRes> time.

=cut

sub last_mod {
    my $self = shift;
    return $self->{last_mod};
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

I<@role_ids> = get_roles (I<$tm>, I<$assertion>, I<$player>)

This function returns a list of roles a particular player plays in a given assertion

=cut

sub get_roles {
    my $self = shift;
    my $a = shift;
    my $p = shift; # the player

    my ($ps, $rs) = ($a->[PLAYERS], $a->[ROLES]);
    
    my @rs;
    for (my $i = 0; $i < @$ps; $i++) {
	next unless $ps->[$i] eq $p;
	push @rs, $rs->[$i];
    }
    return @rs;
}

=pod

=item B<get_role_s>

I<@role_ids> = @{ get_role_s (I<$tm>, I<$assertion>) }

This function extracts a reference to the list of role identifiers.

=cut

sub get_role_s {
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

#warn "sub $THING assert $self".ref ($self);

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

=item B<match>, B<match_forall>, B<match_exists>

I<@list> = I<$tm>->match (C<TM-E<gt>FORALL> or C<TM-E<gt>EXISTS> [ , I<search-spec>, ... ]);

I<@list> = I<$tm>->match_forall                    ( I<search-spec>, ... ]);

I<@list> = I<$tm>->match_exists                    ( I<search-spec>, ... ]);

These methods takes a search specification and return matching assertions. The result list contains
references to the assertions themselves, not to copies. You can change the assertions themselves on
your own risk (read: better not do it).

For C<match>, if the constant C<FORALL> is used as first parameter, this method returns a list of
all assertions in the store following the search specification. If the constant C<EXISTS> is used
the method will return a non-empty value if at least one can be found. Calling the more specific
C<match_forall> is the same as calling C<match> with C<FORALL>. Similar for C<match_exists>.

B<NOTE>: C<EXISTS> is not yet implemented.

The search specification is a hash with the same fields as for the constructor of an assertion:

Example:

   $tm->match (TM->FORALL, type    => '...',
                           scope   => '...,
                           roles   => [ ...., ....],
                           players => [ ...., ....]);

Any combination of assertion components can be used, all are optional, with the only constraint that
the number of roles must match that for the players. All involved IDs will be absolutized before
matching.

B<NOTE>: Some combinations will be very fast, while others quite slow. The latter is the case when
there is no special-purpose matcher implemented and the general-purpose one has to be used as a
fallback.

B<NOTE>: The implementation also understands a number of rather specialized query handlers. These
are not yet documented here as there may be some shifts in the near future.

=cut

use constant {
    EXISTS => 1,
    FORALL => 0
    };

our %exists_handlers = (); # they should be written at some point

our %forall_handlers = ('' => 
		       sub { # no params => want all of them
			   my $self   = shift;
			   return values %{$self->{assertions}};
		       },

		       'nochar' =>
		       sub {
			   my $self   = shift;
			   return
			       grep ($_->[KIND] <= ASSOC,
					   values %{$self->{assertions}});
		       },
#-- taxos ---------------------------------------------------------------------------------------------
		       'subclass.type' =>
		       sub {
			   my $self   = shift;
			   my $st     = shift;
			   my ($ISSC, $SUBCLASS) = @{$self->{usual_suspects}}{'is-subclass-of', 'subclass'};
			   return () unless shift eq $ISSC;
			   return
			       grep ( $self->is_x_player   ($_, $st, $SUBCLASS),
			       grep ( $_->[TYPE] eq $ISSC,
				      values %{$self->{assertions}}));
		       },

		       'superclass.type' =>
		       sub {
			   my $self   = shift;
			   my $st     = shift;
			   my ($ISSC, $SUPERCLASS) = @{$self->{usual_suspects}}{'is-subclass-of', 'superclass'};
			   return () unless shift eq $ISSC;
			   return
			       grep ( $self->is_x_player   ($_, $st, $SUPERCLASS),
			       grep ( $_->[TYPE] eq $ISSC,
				      values %{$self->{assertions}}));
		       },

		       'class.type' =>
		       sub {
			   my $self   = shift;
			   my $t      = shift;
			   my ($ISA, $CLASS) = @{$self->{usual_suspects}}{'isa', 'class'};
			   return () unless shift eq $ISA;
			   return
			       grep ( $self->is_x_player   ($_, $t, $CLASS),
			       grep ( $_->[TYPE] eq $ISA,
				      values %{$self->{assertions}}));
		       },

		       'instance.type' =>
		       sub {
			   my $self   = shift;
			   my $i      = shift;
			   my ($ISA, $INSTANCE) = @{$self->{usual_suspects}}{'isa', 'instance'};
			   return () unless shift eq $ISA;
			   return
			       grep ( $self->is_x_player   ($_, $i, $INSTANCE),
			       grep ( $_->[TYPE] eq $ISA,
				      values %{$self->{assertions}}));
		       },
#--

		       'char.irole' =>
		       sub {
			   my $self   = shift;
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
			       $self->{assertions}->{$lid} || ();
		       },

		       'type' =>
		       sub {
			   my $self   = shift;
			   my $type   = $_[0];
			   return 
			       grep ($self->is_subclass ($_->[TYPE], $type),
				     values %{$self->{assertions}});
		       },

		       'iplayer' =>
		       sub {
			   my $self   = shift;
			   my $ip     = $_[0];
			   return 
			       grep ($self->is_player ($_, $ip), 
				     values %{$self->{assertions}});
		       },

		       'iplayer.type' =>
		       sub {
			   my $self      = shift;
			   my ($ip, $ty) = @_;
			   return 
			       grep ($self->is_player ($_, $ip)          &&
				     $self->is_subclass ($_->[TYPE], $ty),
				     values %{$self->{assertions}});
		       },

		       'iplayer.irole' =>
		       sub {
			   my $self      = shift;
			   my ($ip, $ir) = @_;
			   return 
			       grep ($self->is_player ($_, $ip, $ir), 
				     values %{$self->{assertions}});
		       },

		       'iplayer.irole.type' =>
		       sub {
			   my $self           = shift;
			   my ($ip, $ir, $ty) = @_;
			   return 
			       grep ($self->is_subclass ($_->[TYPE], $ty) && 
				     $self->is_player ($_, $ip, $ir), 
				     values %{$self->{assertions}});
		       },

		       'irole.type' =>
		       sub {
			   my $self      = shift;
                           my ($ir, $ty) = @_;
			   return
			       grep ($self->is_role ($_, $ir)             &&
				     $self->is_subclass ($_->[TYPE], $ty),
				     values %{$self->{assertions}});
		       },

		       'irole' =>
		       sub {
			   my $self      = shift;
                           my ($ir)      = @_;
			   return
			       grep ($self->is_role ($_, $ir),
				     values %{$self->{assertions}});
		       },

		       'aplayer.arole.brole.type' =>
		       sub {
			   my $self   = shift;
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


		      );

sub _allinone {
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


sub match_forall {
    my $self   = shift;
    my %query  = @_;
#warn "forall ".Dumper \%query;

    my @skeys = sort keys %query;                                                           # all fields make up the key
    my $skeys = join ('.', @skeys);
    my @svals = map { $query{$_} } @skeys;

    if (my $index = $self->{indices}->{match}) {                                            # there exists a dedicated index
	my $key   = "$skeys:" . join ('.', @svals);
	if (my $lids  = $index->is_cached ($key)) {                                         # if result was cached, lets take the list of lids
	    return map { $self->{assertions}->{$_} } @$lids;                                # and return fully fledged
	} else {                                                                            # not defined means not cache => recompute
	    my @as = _dispatch_forall ($self, \%query, $skeys, @svals);                     # do it the hard way
	    $index->do_cache ($key, [ map { $_->[LID] } @as ]);                             # save it for later
	    return @as;
	}
    } else {                                                                                # no cache, let's do the ochsentour
	return _dispatch_forall ($self, \%query, $skeys, @svals);
    }

sub _dispatch_forall {
    my $self  = shift;
    my $query = shift;
    my $skeys = shift;

#warn "keys for this $skeys";
    if (my $handler = $forall_handlers{$skeys}) {                                           # there is a constraint and we have a handler
	return &{$handler} ($self, @_); 
    } else {                                                                                # otherwise
	return _allinone ($self, 0, %$query);                                               # we use a generic handler, slow but should do the trick
    }
}

}
sub match_exists {
    my $self   = shift;
    my %query  = @_;

#warn "exists ".Dumper $query;

    my @skeys = sort keys %query;                                                           # all fields make up the key
    my $skeys = join ('.', @skeys);

#warn "keys for this $skeys";
    if (my $handler = $exists_handlers{$skeys}) {                                           # there is a constraint and we have a handler
	return &{$handler} ($self, map { $query{$_} } @skeys); 
    } else {                                                                                # otherwise
	return _allinone ($self, 1, %query);                                                # we use a generic handler, slow but should do the trick
    }
}

sub match {
    my $self   = shift;
    my $exists = shift; # FORALL or EXIST, DOES NOT work yet

    return $exists ? match_exists ($self, @_) : match_forall ($self, @_);
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
	    push @ks, $self->{mid2iid}->{$k2} ? $k2 : undef;           # see whether there is something
	}
    }
#warn "mids ".Dumper (\@_)." returning ".Dumper (\@ks);
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

=cut

sub is_subclass {
    my $self  = shift;
    my $class = shift;
    my $super = shift;

    return 1 if $class eq $super;                                            # we always assume that A subclasses A

    my ($ISA, $US, $THING, $SUBCLASSES, $SUBCLASS, $SUPERCLASS, $INSTANCE, $CLASS) =
	@{$self->{usual_suspects}}{'isa', 'us', 'thing', 'is-subclass-of', 'subclass', 'superclass', 'instance', 'class'};

#warn "is_subclass?: class $class   super $super , thing $THING, $SUBCLASSES, $SUPERCLASS";
    return 1 if $super eq $THING;                                            # everything is a topic
# but not if the class is one of the predefined things, yes, there is a method to this madness
    return 0 if $class eq $ISA;
    return 0 if $class eq $US;
    return 0 if $class eq $THING;
    return 0 if $class eq $SUBCLASSES;
    return 0 if $class eq $SUBCLASS;
    return 0 if $class eq $SUPERCLASS;
    return 0 if $class eq $INSTANCE;
    return 0 if $class eq $CLASS;
#    # see whether there is an assertion that we have a direct subclasses relationship between the two

# This would be an optimization, but this does not go through match
#    return 1 if $self->is_asserted (Assertion->new (scope   => $US,                          # TODO OPTIMIZE
#						    type    => $SUBCLASSES, 
#						    roles   => [ $SUBCLASS, $SUPERCLASS ],
#						    players => [ $class,    $super ])
#				    );
    # if we still do not have a decision, we will check all super types of $class and see (recursively) whether we can establish is-subclass-of
    return 1 if grep ($self->is_subclass ($_, $super),                       # check all of the intermediate type whether there is a transitive relation
		      map { $self->get_x_players ($_, $SUPERCLASS) }         # find the superclass player there => intermediate type
		      $self->match_forall (type       => $SUBCLASSES,
					   subclass   => $class)
		      );
    return 0;                                                                # ok, we give up now
}

=pod

=item B<is_a>

I<$tm>->is_a (I<$something_lid>, I<$class_lid>)

This method returns C<1> if the thing referenced by the first parameter is an instance of the class
referenced by the second. The method honors transitive subclassing and B<everything> is a C<thing>.

=cut

sub is_a {
    my $self    = shift;
    my $thingie = shift;
    my $type    = shift;                                                         # ok, what class are looking at?

    my ($ISA, $CLASS, $THING) = @{$self->{usual_suspects}}{'isa', 'class', 'thing'};

#warn "isa thingie $thingie class $type";

    return 1 if $type eq $THING and                                              # is the class == 'thing' and
                $self->{mid2iid}->{$thingie};                                    # and does the thingie exist?

    my ($m) = $self->retrieve ($thingie);
    return 1 if $m and                                                           # is it an assertion ? and...
	        $self->is_subclass ($m->[TYPE], $type);                          # is the assertion type a subclass?

    return 1 if grep ($self->is_subclass ($_, $type),                            # check all of the intermediate type whether there is a transitive relation
		         map { $self->get_players ($_, $CLASS) }                 # find the class player there => intermediate type
		             $self->match_forall (type => $ISA, instance => $thingie)
		      );
    return 0;
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

    my ($SUBCLASSES) = @{$self->{usual_suspects}}{'is-subclass-of'};
    return map { $_->[PLAYERS]->[0] } $self->match_forall (type => $SUBCLASSES, superclass => $lid);
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
    my ($SUBCLASSES) = @{$self->{usual_suspects}}{'is-subclass-of'};
    return map { $_->[PLAYERS]->[1] } $self->match_forall (type => $SUBCLASSES, subclass => $lid);
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

    if (my $a = $self->retrieve ($lid)) { return ($a->[TYPE]) };
    my ($ISA) = @{$self->{usual_suspects}}{'isa'};
    return (map { $_->[PLAYERS]->[0] }  $self->match_forall (type => $ISA, instance => $lid));
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

    my ($ISA, $THING) = @{$self->{usual_suspects}}{'isa', 'thing'};

    return $self->midlets if $lid eq $THING;

    return  (map { $_->[LID ] }         $self->match_forall (type => $lid)),
            (map { $_->[PLAYERS]->[1] } $self->match_forall (type => $ISA, class => $lid))
	;
}

sub instancesT {
    my $self = shift;
    my $lid  = shift;

    return map { $self->instances ($_) }   $self->subclassesT ($lid);
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

    my ($THING, $ISA, $CLASS) = @{$self->{usual_suspects}}{'thing', 'isa', 'class'};

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
                           $self->match_forall (type => $ISA, instance => $thing));
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

L<TM::PSI>

=head1 COPYRIGHT AND LICENSE

Copyright 200[1-6] by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.

=cut

our $REVISION = '$Id: TM.pm,v 1.39 2006/11/29 10:31:10 rho Exp $';


1;

__END__

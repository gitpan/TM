package TM::DM::TopicMap;

sub new {
    my $class = shift;
    return bless { @_ }, $class;
}

sub id {
    my $self  = shift;
    my $map   = $self->{tmdm}->{map};
    return $map->baseuri;
}

sub topics {
    my $self  = shift;
    my $map   = $self->{tmdm}->{map};

    if (@_) {
	@_ = $map->mids (@_);                        # make all these fu**ing identifiers map-absolute
    } else {                                         # if the list was empty, we assume every thing in the map
	@_ = keys %{$map->{mid2iid}};
    }

    return map { 
	         TM::DM::Topic->new (
				     tmdm  => $self->{tmdm},
				     sad   =>     $map->midlet ($_)->[TM->ADDRESS],
				     sids  => [ @{$map->midlet ($_)->[TM->INDICATORS]} ],          # TODO: can be optimized
				     mid   => $_
				     )
		 } @_;
}

sub associations {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my %search = @_;
    foreach my $k (keys %search) {
	$search{$k} = $map->mids ($search{$k}) if $k =~ /role|type|player/;
    }

    return map { TM::DM::Association->new (lid  => $_->[TM->LID],
					   tmdm => $self->{tmdm}) }
           grep ($_->[TM->KIND] == TM->ASSOC,
                 $map->match (TM->FORALL, %search)
		 );
}

sub reifier {
    my $self  = shift;
    my $map   = $self->{tmdm}->{map};

    my ($mid) = $map->reifies ($map->baseuri)             # find a topic which has as subject address the baseuri
	or return undef;
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $mid);
}

# dies with invlaid id
# create ALWAYS new copy!!!

sub topic {
    my $self = shift;
    my $id   = shift;
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $self->{tmdm}->{map}->mids ($id));
}

1;

package TM::DM::Topic;

sub new {
    my $class = shift;
    bless { @_ }, $class;
}

sub _mk_topic {
    my $tmdm   = shift,
    my $mid    = shift;
    my $map    = $tmdm->{map};
    my $midlet = $map->midlet ($mid);

    return TM::DM::Topic->new (tmdm => $tmdm,
                               mid  => $mid,
                               sad  =>     $midlet->[TM->ADDRESS],
                               sids => [ @{$midlet->[TM->INDICATORS]} ]);
}

## only one!

sub subjectLocators {
    my $self = shift;
    return ($self->{sad});
}

sub subjectIdentifiers {
    my $self = shift;
    return @{ $self->{sids} };
}

# only one identifier

sub id {
    my $self = shift;
    return $self->{mid};
}

sub parent {
    my $self = shift;
    return TM::DM::TopicMap->new (tmdm => $self->{tmdm});
}

sub names {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};

    return
	map { TM::DM::Name->new (
                                 tmdm  => $self->{tmdm},
				 lid   => $_->[TM->LID],
				 ) }
             grep ($_->[TM->KIND] == TM->NAME,
                   $map->match (TM->FORALL, char    => 1,
                                            iplayer => $self->{mid} ));
}

sub occurrences {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};

    return
	map { TM::DM::Occurrence->new (
                                       tmdm  => $self->{tmdm},
				       lid   => $_->[TM->LID],
				       ) }
             grep ($_->[TM->KIND] == TM->OCC,
                 $map->match (TM->FORALL, char    => 1,
                                          iplayer => $self->{mid} ));

}

sub roles {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my $mid  = $self->{mid};

    my @rs;
    foreach my $a (   grep ($_->[TM->KIND] == TM->ASSOC,
			    $map->match (TM->FORALL, iplayer => $self->{mid} ))) {
	push @rs, map {
	               TM::DM::Role->new (
					  tmdm   => $self->{tmdm},
					  lid    => $a->[TM->LID],
					  player => $mid,
					  type   => $_
					  )
		       } $map->get_roles ($a, $mid);
    }
    return @rs;
}

1;

package TM::DM::Association;

sub new {
    my $class = shift;
    bless { @_ }, $class;
}

sub id {
    my $self   = shift;
    return $self->{lid};
}

sub type {
    my $self   = shift;
    my $map    = $self->{tmdm}->{map};
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $map->retrieve ($self->{lid})->[TM->TYPE]);
}

# only one scope!!!

sub scope {
    my $self   = shift;
    my $map    = $self->{tmdm}->{map};
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $map->retrieve ($self->{lid})->[TM->SCOPE]);
}

sub roles {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});

    my ($ps, $rs) = ($a->[TM->PLAYERS], $a->[TM->ROLES]);

    return map { TM::DM::Role->new (                         # make a role from it
				    tmdm   => $self->{tmdm},
				    lid    => $a->[TM->LID],
				    player => $_->[1],
				    type   => $_->[0]
				    ) }
           map { [ $rs->[$_], $ps->[$_] ] }                  # get the role and the player, $_ is index
           (0 .. $#$ps)                                      # get all indices for all roles
	       ;
}

sub parent {
    my $self = shift;
    return TM::DM::TopicMap->new (tmdm => $self->{tmdm});
}

sub reifier {
    my $self  = shift;
    my $map   = $self->{tmdm}->{map};

    my ($mid) = $map->reifies ($self->{lid})
	or return undef;
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $mid);
}

1;

package TM::DM::Occurrence;

#### datatype INSIDE the value


sub new {
    my $class = shift;
    return bless { @_ }, $class;
}

sub id {
    my $self   = shift;
    return $self->{lid};
}

# non-TMDM: bring back value + data type

sub value {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});
    return ref ($a->[TM->PLAYERS]->[0]) ? # it can only be one of them
	        $a->[TM->PLAYERS]->[0] :
		$a->[TM->PLAYERS]->[1];   # we keep the datatype
}

sub type {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $a->[TM->TYPE]);
}

# only one scope!!!

sub scope {
    my $self   = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $a->[TM->SCOPE]);
}

sub reifier {
    my $self  = shift;
    my $map   = $self->{tmdm}->{map};

    my ($mid) = $map->reifies ($self->{lid})
	or return undef;
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $mid);
}

sub parent {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});
    my $mid  = ref ($a->[TM->PLAYERS]->[0]) ? 
	            $a->[TM->PLAYERS]->[1]  :
		    $a->[TM->PLAYERS]->[0];
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $mid);
}

1;

package TM::DM::Name;

sub new {
    my $class = shift;
    return bless { @_ }, $class;
}

sub id {
    my $self   = shift;
    return $self->{lid};
}

sub value {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});
    my $v    = ref ($a->[TM->PLAYERS]->[0]) ? # it can only be one of them
	            $a->[TM->PLAYERS]->[0] :
		    $a->[TM->PLAYERS]->[1];
    return $v->[0]; # the data type is always a string, boring
}

sub type {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $a->[TM->TYPE]);
}

sub scope {
    my $self   = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $a->[TM->SCOPE]);
}

sub reifier {
    my $self  = shift;
    my $map   = $self->{tmdm}->{map};
    my ($mid) = $map->reifies ($self->{lid})
	or return undef;
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $mid);
}

sub parent {
    my $self = shift;
    my $map  = $self->{tmdm}->{map};
    my $a    = $map->retrieve ($self->{lid});
    my $mid  = ref ($a->[TM->PLAYERS]->[0]) ? 
	            $a->[TM->PLAYERS]->[1]  :
		    $a->[TM->PLAYERS]->[0];
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $mid);
}

1;

package TM::DM::Role;

sub new {
    my $class = shift;
    return bless { @_ }, $class;
}

## NO id

## no reifier




sub player {
    my $self = shift;
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $self->{player});
}

sub type {
    my $self = shift;
    return TM::DM::Topic::_mk_topic ($self->{tmdm}, $self->{type});
}

sub parent {
    my $self = shift;
    return TM::DM::Association->new (tmdm => $self->{tmdm}, lid => $self->{lid});
}

1;

package TM::DM;

require Exporter;
use base qw(Exporter);

use Data::Dumper;

=pod

=head1 NAME

TM::DM - Topic Maps, TMDM layer

=head1 SYNOPSIS

=head1 ABSTRACT

=head1 DESCRIPTION

@@@@@@@@ read-only for now

=head1 INTERFACE

=cut

sub new {
    my $class   = shift;
    my %options = @_;
    $main::log->logdie (scalar __PACKAGE__ .": map parameter is not really a TM instance")     unless ref ($options{map}) && $options{map}->isa ('TM');
    
    return bless { %options }, $class;
}

sub topicmap {
    my $self = shift;
    return new TM::DM::TopicMap (tmdm => $self);
}


=pod


=item B<topics>

I<@topics> = I<$tmdm>->topics (I<@list-of-ids>)

I<@topics> = I<$tmdn>->topics

@@@

This method expects a list containing topic identifiers and returns C<TM::DM::Topic> references
representing these topics. If any of the input identifiers do not denote a valid topic in the map,
undef will be returned in its place.

If the parameter list is empty, B<all> toplets will be returned. Have fun, I mean, use with care.

 Example:

    # round tripping topic ids
    print map { $_->[ID] } $tmdm->topics ('abel', 'cain' );

    print "topic known" if $tmdm->topics ('god');

=cut


=pod

=head1 SEE ALSO

L<TM::PSI>

=head1 COPYRIGHT AND LICENSE

Copyright 200[6] by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.

=cut

our $VERSION  = '0.01';
our $REVISION = '$Id: DM.pm,v 1.2 2006/11/17 09:22:45 rho Exp $';

1;

__END__


=head1 Mid-Level Interface

=cut



#-- Characteristics -------------------------------------------------------
struct 'Characteristic' => [
       sid     => '$',
       scope   => '$',
       type    => '$',
       kind    => '$',
       value   => '$',
       hash    => '$',
];

use constant KIND_ASSOC => 0;
use constant KIND_BN    => 1;
use constant KIND_OC    => 2;

our @Chars    = (KIND_ASSOC, KIND_BN, KIND_OC);
our @CharInfo = ( [], # empty, so what?
		  [ 'name' ,               'value' ],
		  [ 'has-occurrence',  'value' ]);



#-- Toplets ---------------------------------------------------------------
struct 'Toplet' => [
       lid     => '$',
       sad     => '$',
       sids    => '@',
       chars   => '@',
];

#use constant LID      => 0;
use constant SAD      => 1;
use constant SIDS     => 2;
use constant CHARS    => 3;

#use constant SCOPE    => 1;
#use constant TYPE     => 2;
#use constant KIND     => 3;
use constant VALUE    => 4;
use constant HASH     => 5;

#-- Maplets ---------------------------------------------------------------
struct Maplet => [
        lid         => '$',
        scope       => '$',
        type        => '$',
	kind        => '$', # not used
        roles       => '$',
	players     => '$',
];

#use constant ROLES   => 4;
#use constant PLAYERS => 5;

=pod

=head2 Toplets and Maplets

Where the classical introductions of XTM Topic Maps introduce the
concept of a topic and the association, we here have a slightly
different view to organize Topic Map data.

A I<maplet> is very similar to an association in that it consists of a
association type topic, topics for roles and player topics and a topic
for the scope of the association. A I<full maplet> also includes
B<ALL> information about the involved topics. In this implementation
you only get the references to the topics; or rather not to the topics
themselves, but to I<toplet>s.

I<toplet>s consist only of topic characteristics, like the (scoped)
basename(s), (scoped) occurrence(s) and subject indicators.

=head2 Toplets and Maplets

For retrieval there are several ways: If the identifier is known, then
toplets (and maplets, respectively) can be quickly extracted. This is
mostly useful for toplets, though, unless you happen to know an
identifier for the maplet.

To retrieve these, probably more useful is to use query specifications.
These constrain parts (or all) of the information you are looking for.
For example,

   type    => 'isa',
   aplayer => 'dog',
   arole   => 'class'

would be looking for all maplets being a (direct or indirect) instance
of C<isa> which have a player C<dog> which plays the role C<class>
there.

Most general but also the slowest method, is to query the map with a
@@@@ path expression (see L<TM::AsTMa::Path>).

=head2 Identifiers

Identifiers with TMs in general are a BIG pain. This interface adopts
the approach that developers can use a I<local identifier> as long as
possible and only have to fall back to full URI (URNs or URLs) in
cases this does not work.

A I<local identifier> is a string which can identify a toplet in a
map. Normally, these identifiers are maintained by the parsers when a
map is read from a file, although there is no obligation to do so
(neither by this package nor by the TMDM).

Internally, toplets are B<always> identified through URIs which are
built from the local identifier prefixed by the base URI. So, when you
have loaded a map with a base URI C<http://something/> and in that
map was a topic defined labelled C<else>, then the toplet's full
URI is C<http://something/else>.

You can use either this consistently or you may use the function
C<mids>.

I<Global identifiers> can also be used to identify toplets. In either
case, these must be fully-fledged URIs. To distinguish between URIs
which are a I<subject address> and those which refer to a I<subject
indicator> we have the convention that for the latter you always have
to use Perl string references.

=head2 Copy Semantics

Whenever you extract information via this interface, you will get a
Perl data structure. The policy is that no underlying data structures
are passed back to the calling application.

This means that it should be safe for you to alter everything you get
(although you mostly will not do this), without actually changing the
content of the map.

The downside of this is that every implementation of this interface has to copy the whole
information and cannot simply pass on references.  This certainly has an impact on the
performance. If that is an issue then you might want to consider to use the underlying technology.

=head2 Identifier-related Methods

Every implementation of this class must offer a method to convert a I<short identifier> into its
long, canonical form.

=over

=item B<mids> [ ABSTRACT ]

I<@sids> = I<$tm>->mids (I<@list-of-ids>)

I<$sids> = I<$tm>->mids (I<$id>)

This method takes a list of identifiers (relative or absolute ones)
and tries to expand all into their absolute form. C<undef> will be
mapped to C<undef>. Identifiers which are unknown remain as they are.

=cut

#@@@@@@@@@@@@@@

=pod

=item B<maplets> [ ABSTRACT ]

I<@maplets> = I<$tm>->maplets (I<%match_specification>)

This method takes a match specification returns a list of maplet references which match the specification.

TBW: match specification

=cut

sub maplets {
  my $self  = shift;

#warn "maplets mat";
#warn "return ".Dumper #

  my %query = @_;                                             # query spec is actually a hash

  grep ($query{$_} = $_ eq 'char' ? 
                        $_ : (
		     $_ eq 'roles' || $_ eq 'players' ?
                        [ $self->mids (@{$query{$_}}) ] :
			$self->mids ($query{$_})
			      ),
	keys %query); # make sure that all identifiers are map-absolute

  return
      map { defined $_ ?
		bless [ 
			$_->[LID],
			$_->[SCOPE],
			$_->[TYPE],
			undef,
			[ @{ $_->[ROLES] } ],
			[ @{ $_->[PLAYERS] } ],
			], 'Maplet' 
		:
		undef }
         $self->match (FORALL, %query);
}

=pod

=item B<assert_toplets>

I<$tm>->assert_toplets (I<@toplet_list>)

This method takes a list of toplets and puts it into the underlying
store. All topic references which are relative will be automatically
made absolute.

As toplets consist only of characteristics, only these are added. No
further information about the topic itself is stored. If you want that
that topic should be an instance of something, then this must either
be done with a separate maplets.

The method returns nothing.

=cut

my %kind2type = (TM->KIND_BN  => 'has-basename',
		 TM->KIND_OC  => 'has-uri-occurrence',
		 TM->KIND_IN  => 'has-data-occurrence');

my %kind2role = (TM->KIND_BN  => 'basename',
		 TM->KIND_OC  => 'xtm-psi-occurrence',
		 TM->KIND_IN  => 'xtm-psi-occurrence');

sub assert_toplets {
  my $self = shift;

  foreach my $t (@_) {
      my $id = $t->[LID];
#warn "dealing with $id";
      foreach my $c (@{$t->[CHARS]}) {
	  $self->assert ( [ undef,                                                     # lid
			    $c->[SCOPE],                                 # scope
			    $kind2type{$c->[KIND]},                      # type
			    $c->[KIND],                                  # kind
			    [ 'thing', $kind2role{$c->[KIND]} ],         # roles
			    [ $id,     \$c->[VALUE]],                    # players
			    undef                                                      # canon
			    ]
			  );
      }
  }
  $self->{last_mod} = Time::HiRes::time;
}



=pod

=item B<retract_toplets>

I<$tm>->retract_toplets (I<@identifier_list>)

This method takes a list of (local or global) identifiers and removes
all B<maplet> information about these topics. This method does not
return anything.

B<Note>: To get completely rid of these toplets (and also all which
have been orphaned by deleting the maplets), you will have to use
C<consolidate>.

=cut

sub retract_toplets {
  my $self = shift;

  foreach ($self->mids (@_)) {
      $self->retract (map { $_->[LID] } $self->match (FORALL, anyid => $_));
  }
  $self->{last_mod} = Time::HiRes::time;
}

=pod

=item B<assert_maplets>

I<$tm>->assert_maplets (I<@maplet_list>)

This method takes a list of maplets and stores then into the underlying
store.

The method does not return anything.

=cut

sub assert_maplets {
  my $self  = shift;

  map {
      $self->assert ( [ $_->[LID],               # lid
			 $_->[SCOPE],             # scope
			 $_->[TYPE],              # type
			 ASSOC,                      # kind
			 [ @{$_->[ROLES]} ],      # roles (cloned)
			 [ @{$_->[PLAYERS]} ],    # players (cloned)
			 undef                                  # canon
			 ]
		       );
  } @_;
  $self->{last_mod} = Time::HiRes::time;
}

=pod

=item B<retract_maplets>

I<$tm>->retract_maplets (I<@identifier_list>)

This method takes a list of identifiers and removes the maplets for these
identifiers.

The method does not return anything.

=cut

sub retract_maplets {
  my $self = shift;

  $self->retract (@_);
  $self->{last_mod} = Time::HiRes::time;
}

=pod

=back

=head2 Maplet-related Functions

While maplet components are all accessible, we provide here some useful functions to test maplets for particular
role/player combinations or to extract particular players.

=over

=item B<is_player>, B<is_x_player>

I<$tm>->is_player (I<$maplet>, I<$player_lid>, [ I<$role_lid> ])

I<$ms>->is_x_player (I<$maplet>, I<$player_id>, [ I<$role_id> ])

This function returns true (1) if the identifier specified by the second parameter plays any role in the maplet
provided as first parameter. If the role id is provided as third parameter then it must be exactly this role that is
played.

The 'x'-version is using equality instead of 'subclassing'.

=cut

sub xxxxis_player {
    my $self = shift;
    my $m = shift;

    my $p = $self->mids (shift) or die "must specify player"; # : ".Dumper ([ $m ])." and role is ".shift;
    my $r = $self->mids (shift); # may be undefined

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

sub xxxxxis_x_player {
    my $self = shift;
    my $m = shift;
    my $p = $self->mids (shift) or die "must specify x-player"; #: ".Dumper ([ $m ]);
    my $r = $self->mids (shift); # may be undefined

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

=item B<get_players>

I<@ps> = I<$tm>->get_players (I<$maplet>, I<$role_id> )

This function returns the players for the given role. As several players could play the same role this returns a list

=cut

sub xxxxxget_players {
    my $self = shift;
    my $a = shift;
    my $r = $self->mids (shift) or die "must specify role";
    
    my ($ps, $rs) = ($a->[PLAYERS], $a->[ROLES]);
    
    my @ps;
    for (my $i = 0; $i < @$ps; $i++) {
	next unless $self->is_subclass ($rs->[$i], $r);
	push @ps, $ps->[$i];
    }
    return @ps;
}

=pod

=item B<is_role>

I<$tm>->is_role (I<$maplet>, I<$role_id>)

This function returns true (1) if the identifier specified by the second parameter is a role in the maplet provided
as first parameter.

=cut

sub xxxxis_role {
    my $self = shift;
    my $m    = shift;
    my $r    = $self->mids (shift) or die "must specify role"; # .Dumper ([ $m ]);

    return 1 if grep ($self->is_subclass ($_, $r), @{$m->[ROLES]});
}

=pod

=back


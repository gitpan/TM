package TM::Bulk;

use Class::Trait 'base';

use Data::Dumper;

=pod

=head1 NAME

TM::Bulk - Topic Maps, Bulk Retrieval Trait

=head1 SYNOPSIS

  my $tm = .....                          # get a map from somewhere

  use TM::Bulk;
  use Class::Trait;
  Class::Trait->apply ($tm, 'TM::Bulk');  # give the map the trait

  my $vortex = $tm->vortex ('some-lid',
                           {
	  	 	    'types'       => [ 'types' ],
		 	    'instances'   => [ 'instances*', 0, 20 ],
			    'topic'       => [ 'topic' ],
			    'roles'       => [ 'roles',     0, 10 ],
			    'members'     => [ 'players' ],
			   },
			   [ 'scope1', 'scope2', .... ]
			   );


=head1 DESCRIPTION

Especially when you build user interfaces, you might need access to a lot of topic-related
information. Instead of collecting this 'by foot' the following methods help you achieve this
more effectively.

=over

=item B<basenames>

I<$name_hash_ref> = I<$tm>->basenames (I<$lid_list_ref>, I<$scope_list_ref> )

This method takes a list (reference) of topic ids and a list of scoping topic ids.  For the former
it will try to find the I<basename>s (I<topic names> for TMDM acolytes). The list of scopes directs
the method to look first for a basename in the first scoping topic, then second, and so on. If no
such basenames exist for a particular I<lid>, then an C<undef> is returned.

If the list of scoping topics is empty, then it will be interpreted as I<dont care>. In that case
B<any> basename may be returned (if such exists). You can make this explicit by adding a C<*> at the
end (or as sole entry) in the list. Otherwise the result is undefined.

The overall result is a hash (reference) having the lids as keys and the basename strings as values.

=cut

sub basenames {
    my $self   = shift;
    my $topics = shift || [];
    my $scopes = shift || [ '*' ];

    my $dontcare = 0;                                                  # one of the rare occasions I need a boolean
    if ($scopes->[-1] eq '*') {
	pop @$scopes;                                                  # get rid of this '*' to have a clean topic list
	$dontcare = 1;                                                 # remember this incident for below
    }
    my @scopes = grep ($_, $self->mids (@$scopes));                    # make them absolute, so that we can compare later (only keep existing ones)
#warn "scopes".Dumper \@scopes;

    my %dict;                                                          # this is what we are building
TOPICS:
    foreach my $lid (grep { $_ } $self->mids (@$topics)) {             # for all in my working list, make them absolute, and test
	my @as = grep { $_->[TM->KIND] == TM->NAME }                   # filter all characteristics for basenames
	            $self->match_forall (char => 1, topic => $lid);
        next unless @as;                                               # assertion: @as contains at least one entry!
	foreach my $sco ($self->mids (@scopes)) {                      # check out all scope preferences (note, there is at least one in @as!)
	    if (my @aas = grep ($_->[TM->SCOPE] eq $sco, @as)) {
		$dict{$lid} = $aas[0]->[TM->PLAYERS]->[1]->[0];
		next TOPICS;
	    }
	}
	$dict{$lid} = $dontcare ? $as[0]->[TM->PLAYERS]->[1]->[0]      # get the name and derereference it
	                        : undef;                               # otherwise we have none
    }
    return \%dict;
}

=pod

=item B<vortex>

I<$info> = I<$tm>->vortex (,
               I<$vortex_lid>,
               I<$what_hashref>,
               I<$scope_list_ref> )

This method returns B<a lot> of information about a particular toplet (vortex). The function expects
the following parameters:

=over

=item I<lid>:

the lid of the toplet in question

=item I<what>:

a hash reference describing the extent of the information (see below)

=item I<scopes>:

a list (reference) to scopes (currently NOT honored)

=back

To control B<what> exactly should be returned, the C<what> hash reference can contain following
components. All of them being tagged with <n,m> accept an additional pair of integer specify the
range which should be returned.  To ask for the first twenty, use C<0,19>, for the next
C<20,39>. The order in which the identifiers is returned is undefined but stable over subsequent
read-only calls.

=over

=item I<instances> (<n,m>):

fetches all toplets (and maplets) which are direct instances of the vortex (that is regarded as
class here);

=item I<instances*> (<n,m>):

same as C<instances>, but including all instances of subclasses of the vortex

=item I<types> (<n,m>):

fetches all (direct) types of the vortex (that is regarded as instance here)

=item I<types*> (<n,m>):

fetches all (direct and indirect) types of the vortex (that is regarded as instance here)

=item I<subclasses>  (<n,m>):

fetches all direct subclasses

=item I<subclasses*> (<n,m>):

same as C<subclasses>, but creates reflexive, transitive closure

=item I<superclasses> (<n,m>):

fetches all direct superclasses

=item I<superclasses*> (<n,m>):

same as C<superclasses>, but creates reflexive, transitive closure

=item I<roles> (<n,m>):

fetches all maplet ids where the vortex B<is> a role,

=item I<players> (<n,m>):

fetches all maplets where the vortex B<plays> a role

=item I<toplet>:

fetches the complete toplet itself (all characteristics, but no maplets)

=back

The function will determine all of the requested information and will prepare a hash reference
storing each information into a hash component. Under which name this information is stored, the
caller can determine with the hash above as the example shows:

Example:

  $vortex = $tm->vortex ('some-lid',
                         {
			  'types'       => [ 'types' ],
			  'instances'   => [ 'instances*', 0, 20 ],
			  'topic'       => [ 'topic' ],
			  'roles'       => [ 'roles',     0, 10 ],
			  'members'     => [ 'players' ],
			 },
			 [ 'scope1', 'scope2', .... ]
			);

The method dies if C<lid> does not identify a proper toplet.

=cut

sub vortex {
  my $self   = shift;
  my $lid    = shift;
  my $what   = shift;
  my $scopes = shift and die "scopes not supported yet";

  my $alid = $self->mids ($lid);
  my $t    = $self->midlet ($alid) or die "no midlet for '$alid'";
  my $_t; # here all the goodies go

  sub _calc_limits {
      my $last  = (shift) - 1; # last available
      my $from  = shift || 0;
      my $want  = shift || 10;
      my $to = $from + $want - 1;
      $to = $last if $to > $last;
      return ($from, $to);
  }
  
  foreach my $where (keys %{$what}) {
      my $w     = shift @{$what->{$where}};

      if ($w eq 'topic') {
	  $_t->{$where} = $t;
	  
      } elsif (grep ($w =~ /^$_\*?$/, qw(instances types subclasses superclasses))) {
	  $w =~ s/\*/T/;

	  my @lids  = $self->$w ($alid); # whoa, Perl late binding rocks !
	  my ($from, $to) = _calc_limits (scalar @lids, shift @{$what->{$where}}, shift @{$what->{$where}});
	  $_t->{$where} = [ @lids[ $from .. $to ] ];
	  
      } elsif ($w eq 'roles') {
	  my @lids = map { $_->[TM->LID ]} $self->match_forall (irole => $alid);
	  my ($from, $to) = _calc_limits (scalar @lids, shift @{$what->{$where}}, shift @{$what->{$where}});
	  $_t->{$where} = [ @lids[ $from .. $to ] ];
	  
      } elsif ($w eq 'players') {
	  my @lids =  map { $_->[TM->LID ]} $self->match_forall (iplayer => $alid);
	  my ($from, $to) = _calc_limits (scalar @lids, shift @{$what->{$where}}, shift @{$what->{$where}});
	  $_t->{$where} = [ @lids[ $from .. $to ] ];
	  
      }
  }
  return $_t;
}

=pod

=back

=head1 SEE ALSO

L<TM::Overview>

=head1 COPYRIGHT AND LICENSE

Copyright 200[3-57] by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

our $VERSION  = 0.2;
our $REVISION = '$Id: Bulk.pm,v 1.1 2005/06/20 10:03:26 rho Exp $';

1;

__END__

 =item B<baseNames>

I<$hash_ref> = I<$mem>->baseNames ( I<$topic_id_list_ref>,
                              I<$scope_list_ref> )

receives a list reference containing topic C<id>s. It returns a hash reference containing
the baseName for each topic as a value with the topic id the key. The additional parameter is interpreted as
list reference to scoping topics. If this list is undef, then any basename may be returned. If the list is
empty ([]), then NO basename will ever be returned. If it is non-empty, then - according to the order in
this list - the first basename matching will be selected.

Example:

   $names_ref = $tm->baseNames ([ 't-topic1', 't-topic-2' ],
                                [ 'http://www.topicmaps.org/xtm/language.xtm#en' ]);

 =cut

sub baseNames {
    my $self   = shift;
    my $names  = shift;
    my $scopes = shift;

    push @$scopes, $XTM::PSI::xtm{universal_scope} unless ($scopes && @$scopes); # default scope

    elog ('XTM::Memory', 3, "baseNames for.... ");
    elog ('XTM::Memory', 4, "  baseNames for ", $names, $scopes);

    my %dict;
    foreach my $n (@{$names}) {
	next if $dict{$n};
	(my $m = $n) =~ s/^\#//;
	if ( $self->{topics}->{$m} ) {  # skip ids where there is nothing
	    FIND:
	    foreach my $scope (@$scopes) { # iterate over all scopes and find first matching
		elog ('XTM::Memory', 5, "     looking for scope ", $scope);
		foreach my $b (@{$self->{topics}->{$m}->baseNames}) {
		    elog ('XTM::Memory', 5, "      in baseName ", $b, "scope", $b->scope->references);
		    if (grep ($_->href eq $scope, @{$b->scope->references})) { # OK, perfect match
			$dict{$n} = $b->baseNameString->string;
			elog ('XTM::Memory', 5, "      perfect match: found $dict{$n}");
			last FIND;
		    } elsif (grep ($_->href eq $XTM::PSI::xtm{universal_scope}, @{$b->scope->references})) {
            # topic map did not care
			$dict{$n} = $b->baseNameString->string;
			elog ('XTM::Memory', 5, "      map not care: found $dict{$n}");
			last FIND;
		    } elsif ($scope eq $XTM::PSI::xtm{universal_scope}) { # user did not care
			$dict{$n} = $b->baseNameString->string;
			elog ('XTM::Memory', 5, "      user not care: found $dict{$n}");
			last FIND;
		    }
		}
	    }
	}
	unless ($dict{$n}) { # silent desperation, leave it up to the app to handle this
	    $dict{$n} = undef;
	}
    }
}
elog ('XTM::Memory', 4, "  result ", \%dict);
return { %dict };

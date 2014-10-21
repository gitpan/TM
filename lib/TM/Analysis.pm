package TM::Analysis;

use TM;
use Data::Dumper;

use Class::Trait 'base';

=pod

=head1 NAME

TM::Analysis - Topic Maps, analysis functions

=head1 SYNOPSIS

  use TM::Materialized::AsTMa;
  my $tm = new TM::Materialized::AsTMa (file => 'test.atm');
  $tm->sync_in;

  Class::Trait->apply ($tm, 'TM::Analysis');

  print Dumper $tm->clusters;

  print Dumper $tm->statistics;


=head1 DESCRIPTION

This package contains some topic map analysis functionality.

=head1 INTERFACE

=over

=item B<clusters>

I<$hashref> = clusters (I<$tm>)

computes the I<islands> of topics. It figures out which topics are connected via associations and - in case they are -
will collate them into clusters. The result is a hash reference to a hash containing list references of topic ids
organized in a cluster.

In default mode, this function only regards topics to be in the same cluster if topics B<play> roles in one and the same
maplet. The role topics themselves or the type or the scope are ignored.

You can change this behaviour by passing in options like

  use_scope => 1

  use_roles => 1

  use_type  => 1

Obviously, with C<use_scope =E<gt> 1> you will let a lot of topics collapse into one cluster as most maplets usually are
in the unconstrained scope.

=cut

sub clusters {
    my $tm    = shift;

    my %opts = @_;
# by default
#   not using scope
#   not using type
#   not using roles
    $opts{use_lid} = 1 unless defined $opts{use_lid}; #   always use maplet ID

    my $i = 0;
    my $clusters = { map { $_ => $i++ } $tm->midlets };   # we store every toplet into its own cluster

    foreach my $m ($tm->match (TM->FORALL, nochar => 1)) {

	my   @candidates;
	push @candidates, $m->[TM->LID]         if $opts{use_lid};
	push @candidates, $m->[TM->TYPE]        if $opts{use_type};
	push @candidates, $m->[TM->SCOPE]       if $opts{use_scope};
	push @candidates, @ { $m->[TM->ROLES] } if $opts{use_roles};
	push @candidates, @ { $m->[TM->PLAYERS] };

#warn "candidates in a boat ". Dumper \@candidates;

#warn "starting with ".@roles[0];
	my $i = $clusters->{shift @candidates};
#warn "  with $i";
	foreach (@candidates) {
#warn "working on $_";
	    my $j = $clusters->{$_};
	    # now all entries which have currently $j must be turned into $i
#warn "considering $i and $j";
	    unless ($i == $j) {
#warn "has index $j, now merge all which have $j to have $i";
		map { $clusters->{$_} = $clusters->{$_} == $j ?  $i : $clusters->{$_} } keys %{$clusters};
#warn Dumper $clusters;
	    }
	}
    }

#warn Dumper $clusters;

    my @clusters = map { [] } values %$clusters;
    map { push @{@clusters[ $clusters->{$_} ]}, $_ } keys %$clusters ;
    return [ grep (@$_, @clusters)];  # get rid of empty clusters

}

=pod

=item B<statistics>

This (currently quite limited) function computes a reference to hash containing the
following fields:

=over

=item C<nr_midlets>

Nr of midlets in the map. This includes ALL midlets for topics and also those for
assertions.

=item C<nr_maplets>

Nr of assertions in the map.

=item C<nr_clusters>

Nr of clusters according to the C<cluster> function elsewhere in this document.

=back

=cut

sub statistics {
    my $tm    = shift;

    my $stats; # result

    $stats->{nr_midlets} = scalar $tm->midlets;
    $stats->{nr_maplets} = scalar $tm->match_forall;

    { # clusters
	my $clusters = TM::Analysis::clusters ($tm, use_roles => 1, use_type => 1);
	$stats->{nr_clusters} = scalar @$clusters;
    }

    # size of map
    # payload (basenames, occurrence data, variant

    return $stats;
}

=pod

=item B<orphanage>

This computes all topics which have either no supertype and also those which have no type. Without
further parameters, it returns a hash reference with the following fields:

=over

=item C<untyped>

Holds a list reference to all topic ids which have no type.

=item C<empty>

Holds a list reference to all topic ids which have no instance.

=item C<unclassified>

Holds a list reference to all topic ids which have no superclass.

=item C<unspecified>

Holds a list reference to all topic ids which have no subclass.

=back

Optionally, a list of the identifiers above can be passed in so that only that particular
information is actually returned (some speedup):

   my $o = TM::Analysis::orphanage ($tm, 'untyped');

=cut

sub orphanage {
    my $self = shift;

    my %types     = (); # each topic -> how many types
    my %instances = (); # each topic -> how many instances
    my %supers    = (); # each topic -> how many superclasses
    my %subs      = (); # each topic -> how many subclasses

    my ($ISA, $ISSC, $CLASS, $INSTANCE) = @{$self->{usual_suspects}}{'isa', 'is-subclass-of', 'class', 'instance'};

    foreach my $a (values %{$self->{assertions}}) {
	$types{$a->[TM->LID]}++; $instances{$a->[TM->TYPE]}++;

	if ($a->[TM->TYPE] eq $ISA) {
	    my ($class, $instance) = @{ $a->[TM->PLAYERS] };
	    $types{$instance}++; $instances{$class}++;
	} elsif ($a->[TM->TYPE] eq $ISSC) {
	    my ($sub, $super) = @{ $a->[TM->PLAYERS] };
	    $supers{$sub}++; $subs{$super}++;
	}
    }
#warn Dumper (\%types , \%instances, \%supers, \%subs);

    my @all = $self->midlets;
    my %o;
    foreach my $a (@_ ? @_ : qw(untyped empty unclassified unspecified)) {       # default is all
	$o{$a} = [ grep !$types{$_},     @all ] if $a eq 'untyped';
	$o{$a} = [ grep !$instances{$_}, @all ] if $a eq 'empty';
	$o{$a} = [ grep !$supers{$_},    @all ] if $a eq 'unclassified';
	$o{$a} = [ grep !$subs{$_},      @all ] if $a eq 'unspecified';
    };
    return \%o;
}

=pod

=back

=head1 SEE ALSO

L<TM>

=head1 COPYRIGHT AND LICENSE

Copyright 200[3-6] by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

our $VERSION  = 0.4;
our $REVISION = '$Id: Analysis.pm,v 1.4 2006/11/30 08:38:10 rho Exp $';


1;

__END__


 item B<info>

I<$hashref> = I<$tm>->info (I<list of info items>)

returns some meta/statistical information about the map in form of
a hash reference containing one or more of the following components (you might
want to discover the return values with Data::Dumper):

 over

 item (a)

I<informational>: this hash reference contains the number of topics, the number of associations,
the UNIX date of the last modification and synchronisation with the external tied object and
a list reference to other topic maps on which this particular map depends.

 item (b)

I<warnings>

This hash reference contains a list (reference) of topic ids of topics I<not_used> anywhere in the map.
There is also a list (I<no_baseName>) of topics which do not contain any baseName (yes this is allowed in section
3.6.1 of the standard).

 item (c)

I<errors>

This component contains a list reference I<undefined_topics> containing a list of topic identifiers
of topics not defined in the map. 

 item (d)

I<statistics>

This component contains a hash reference to various statistics information, as the number of clusters,
maximum and minimum size of clusters, number of topics defined and topics mentioned.


TODOs:

 over

 item

detect cyclic dependency of topic types

 back

 back

You can control via a parameter in which information you are interested in:

Example:

   $my_info = $tm->info ('informational', 'warning', 'errors', 'statistics');


 cut

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

 =pod


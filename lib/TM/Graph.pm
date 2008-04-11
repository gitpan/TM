package TM::Graph;

use strict;
use Data::Dumper;

use TM;

use Class::Trait 'base';

=pod

=head1 NAME

TM::Graph - Topic Maps, trait for graph-like operations

=head1 SYNOPSIS

  use TM::Materialized::AsTMa;
  my $tm = new TM::Materialized::AsTMa (file => 'old_testament.atm');
  $tm->sync_in;
  Class::Trait->apply ( $tm => 'TM::Graph' );

  # find groups of topics connected
  print Dumper $tm->clusters;


  # use association types to compute a hull
  print "friends of Mr. Cairo: ".
   Dumper [
       $tm->frontier ([ $tm_>tid ('mr-cairo') ], [ [ $tm->tids ('foaf') ] ])
   ];

  # see whether there is a link (direct
  print "I always knew it" 
     if $tm->is_path ( [ 'gw-bush' ],              # there could be more
                      (bless [ [ 'foaf' ] ], '*'),
                      'osama-bin-laden');


=head1 DESCRIPTION

Obviously a topic map is also a graph, the topics being the nodes, and the associations forming the
edges, albeit these connections connect not always only two nodes, but, ok, you should know TMs by now.

This package provides some functions which focus more on the graph-like nature of Topic Maps.

=head1 INTERFACE

=head2 Methods

This trait provides the following methods:

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

B<NOTE>: This is yet a somewhat expensive operation.

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
    my $clusters = { map { $_ => $i++ } map { $_->[TM->LID] } $tm->toplets };   # we store every toplet into its own cluster

    foreach my $m ($tm->match (TM->FORALL, nochar => 1)) {

	my   @candidates;
#	push @candidates, $m->[TM->LID]         if $opts{use_lid};
	push @candidates, $m->[TM->TYPE]        if $opts{use_type};
	push @candidates, $m->[TM->SCOPE]       if $opts{use_scope};
	push @candidates, @ { $m->[TM->ROLES] } if $opts{use_roles};
	push @candidates, @ { $m->[TM->PLAYERS] };

	my $i = $clusters->{shift @candidates};
	foreach (@candidates) {
	    my $j = $clusters->{$_};
                                                                   # now all entries which have currently $j must be turned into $i
	    unless ($i == $j) {
		map { $clusters->{$_} = $clusters->{$_} == $j ?  $i : $clusters->{$_} } keys %{$clusters};
	    }
	}
    }
    my @clusters = map { [] } values %$clusters;
    map { push @{@clusters[ $clusters->{$_} ]}, $_ } keys %$clusters ;
    return [ grep (@$_, @clusters)];  # get rid of empty clusters
}

=pod

=item B<frontier>

I<@hull> = I<$tm>->frontier (I<\@start_lids>, I<$path_spec>)

This method computes a I<qualified hull>, i.e. a list of all topics which are reachable from
I<@start_lids> via a path specified by I<$path_spec>. The path specification is a (recursive) data
structure, describing sequences, alternatives and repetition (the C<*> operator), all encoded as
lists of lists. The topics in that path specification are interpreted as assertion types.

Example:

   # a single step: start knows ...
   [             ]            # outer level: sequence (there is only one)
     [ 'knows' ]              # inner level: alternatives (there is only one)

   # two subsequent steps: start knows ... isa ...
   [                        ] # outer level: two entries
     [ 'knows' ], [ 'isa' ]   # inner level, one entry each

   # repetition: start knows ... knows ... knows ... ad infinitum
   bless [             ], '*' # outer level: one entry, but blessed
           [ 'knows' ]        # inner level

   # alternatives: start knows | hates ...
   [                      ]   # outer level: one entry
     [ 'knows', 'hates' ]     # inner level: alternatives


B<NOTE>: Cycles are detected.

B<NOTE>: I am not sure how this performs at rather large graphs, uhm, maps.

=cut

sub frontier {
    my $tm = shift;
    my $as = shift;
    my $ps = shift;

    my @bs = _frontier_star ($tm, $as, {}, $ps);                                     # {} are the axes followed so far

sub _frontier_star {
    my $tm = shift;
    my $as = shift;                                                                 # the list (ref) of things where we are now
    my $vs = shift;                                                                 # what have we visited so far, hash ref
    my $ps = shift;                                                                 # a list ref for the sequence of path items left to be done

    if (ref ($ps) eq '*') {                                                         # if *'ed then add the starting points
	my @front = @$as;                                                           # start off with the starting points, they belong to it
	while (1) {                                                                 # repeat ad-infinitum
	    my @bs = _frontier_seq ($tm, \@front, $vs, $ps) ;                       # compute from the current front
	    last unless @bs;                                                        # there might not be any new ($vs has side effects!!!!!!!!)
	    push @front, @bs;                                                       # what we got we collect
	    { my %X; map { $X{$_}++ } @front; @front = keys %X; }                  # make that unique (otherwise too many identical entries)
	}
	return @front;  # and finally return it

    } else {
	return _frontier_seq ($tm, $as, $vs, $ps)
    }
}

sub _frontier_seq {
    my $tm = shift;
    my $as = shift;                                                                 # the list (ref) of things where we are now
    my $vs = shift;                                                                 # what have we visited so far, hash ref
    my $ps = shift;                                                                 # a list ref for the sequence of path items left to be done

    my $front = $as;
    foreach my $p (@$ps) {                                                          # one step after the other
	$front = [ _frontier_alt ($tm, $front, $vs, $p) ];                          # compute what we can reach from there
	{ my %X; map { $X{$_}++ } @$front; $front = [ keys %X ]; }                  # make that unique (otherwise too many identical entries)
    }
    return @$front;
}

sub _frontier_alt {
    my $tm = shift;
    my $as = shift;
    my $vs = shift;                                                                 # what have we visited so far, hash ref
    my $os = shift;                                                                 # a list of alternative paths

    my @front;
    foreach my $o (@$os) {
	push @front, _frontier_step ($tm, $as, $vs, $o);
    }
    { my %X; map { $X{$_}++ } @front; @front = keys %X; };                          # make that unique (otherwise too many identical entries)
    return @front;
}

sub _frontier_step {
    my $tm = shift;
    my $as = shift;
    my $vs = shift;                                                                 # what have we visited so far, hash ref
    my $s  = shift;                                                                 # the step

    if (ref ($s) eq '*') {                                                          # something more complex, can only be sequence
	return _frontier_star ($tm, $as, $vs, $s);
    } elsif (ref ($s)) {                                                            # something more complex, can only be sequence
	return _frontier_seq  ($tm, $as, $vs, $s);
    } else {                                                                        # atomic step
	my @as = grep { $vs->{$_}->{$s}++ == 0 } @$as;                              # get rid of those where we already followed that axis
	if ($s eq 'isa') {                                                          # instance of, easy
	    return map { $tm->typesT ($_) } @as;                                    # compute their types

	} elsif ($s eq 'iko') {                                                     # subclasses, easy
	    return map { $tm->superclassesT ($_) } @as;                             # computer their superclasses

	} else {
	    my $tt = $tm->mids ($s);
	    return 
		map { $tm->get_players ($_) }
	        map { $tm->match_forall (type => $tt, iplayer => $_) }
	        @as;
	}
    }
}

}

=pod

=item B<is_path>

I<$bool> = I<$tm>->is_path (I<\@start_lids>, I<$path_spec>, I<$end_lid>)

This method returns C<1> if there is a path from I<start_lids> to I<end_lid> via the path
specification. See B<frontier> for that one.

=cut

sub is_path {
    my $tm = shift;
    my $as = shift;
    my $ps = shift;
    my $b  = shift;

    my $bt = $tm->mids ($b) or $TM::log->logdie ("end topic not in map");
    return grep { $_ eq $bt } $tm->frontier ( [ $tm->mids (@$as) ], $ps);
}

=pod

=back

=head1 SEE ALSO

L<TM>

=head1 COPYRIGHT AND LICENSE

Copyright 200[78] by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.

=cut

our $VERSION  = 0.2;
our $REVISION = '$Id: Graph.pm,v 1.1 2007/07/28 16:40:31 rho Exp $';


1;

__END__

sub _is_path {
    my $tm = shift;
    my $a  = shift;
    my $b  = shift;
    my $vs = shift;                                                                 # what have we visited so far
                                                                                    # @_ contains a sequence of steps
    return $a eq $b if scalar @_ == 0;                                              # empty path? then a == b
                                                                                    # ok, there is more of a path
    $vs->{$a} = 1;                                                                  # make an entry in the visitor's guestbook
    my $r = shift;                                                                  # take the first step
# or list
    foreach my $s (@$r) { # this is a list of or'ed steps, s is an atom (= list reference, possibly blessed, contains only ONE element)
	my $t = $s->[0];
	if (ref ($s) eq '*') {
	    die;
	} else {
	    my @bs = _make_step ($tm, $vs, $tm->mids ($t), $a);


	    return grep { $b eq $_ } @bs unless @_;                                 # if there is more, we have to continue, otherwise, all if a == b
	    foreach my $b2 (@bs) {
		return 1 if _is_path ($tm, $b2, $b, $vs, @_);
	    } 
	    return 0;
	}



    if (! ref ($r)) {                                                               # something simple, one id
	if ($r =~ /(\w+)\*$/) {                                                     # this is a repetition*
	    my $t2 = $tm->mids ($1);
	    return 1 if _is_path ($tm, $a, $b, $vs, @_);                            #     empty step is also ok
	    my @bs = _make_step ($tm, $vs, $t2, $a);                                 #     get the next in the front
	    foreach my $b2 (@bs) {
		return 1 if _is_path ($tm, $b2, $b, $vs, ($r, @_));                 # use the original expression
	    }
	    return 0;

	} else {                                                                    # one single step from $a via $r
	}
    } else { # an OR
	die;
    }
}

xxx=cut


computes a tree of topics based on a starting topic, an association type
and two roles. Whenever an association of the given type is found and the given topic appears in the
role given in this very association, then all topics appearing in the other given role are regarded to be
children in the result tree. There is also an optional C<depth> parameter. If it is not defined, no limit
applies. Starting from XTM::base version 0.34 loops are detected and are handled gracefully. The returned
tree might contain loops then.

Examples:


  $hierarchy = $tm->induced_assoc_tree (topic      => $start_node,
					assoc_type => 'at-relation',
					a_role     => 'tt-parent',
					b_role     => 'tt-child' );
  $yhcrareih = $tm->induced_assoc_tree (topic      => $start_node,
					assoc_type => 'at-relation',
					b_role     => 'tt-parent',
					a_role     => 'tt-child',
					depth      => 42 );

B<Note>


x=cut


x=pod


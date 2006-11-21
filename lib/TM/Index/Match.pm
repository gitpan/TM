package TM::Index::Match;

use strict;
use warnings;
use Data::Dumper;

require Exporter;
use base qw(Exporter);

=pod

=head1 NAME

TM::Index::Match - Topic Maps, Indexing support (match layer)

=head1 SYNOPSIS

    # somehow get a map (any subclass of TM will do)
    my $tm = ... 

    # one option: create a lazy index which learns as you go
    use TM::Index::Match;
    my $idx = new TM::Index::Match ($tm);
    
    # for most operations which involve match_forall to be called
    # reading and querying the map should be much faster

    # learn about some statistics, what keys are most likely to be useful
    my @optimized_keys = @{ $stats->{proposed_keys} };

    # another option: create an eager index
    my $idx = new TM::Index::Match ($tm, closed => 1);

    # pre-populate it, use the proposed keys
    $idx->populate (@optimized_keys);
    # this may be a lengthy operation if the map is big
    # but then the index is 'complete'

    # query map now, should also be faster

    # getting rid of an index explicitly
    $idx->detach;

    # cleaning an index
    $idx->discard;

=head1 DESCRIPTION

One performance bottleneck when using the L<TM> package or any of its subclasses are the low-level
query functions C<match_forall> and C<match_exists>. They are looking for assertions of a certain
nature. Almost all high-level functions, and certainly L<TM::QL> use these.

This package provides an indexing mechanism to speed up the C<match_*> functions by caching some
results in a very specific way. When an index is attached to a map, then it will intercept all
queries going to these functions.

=head2 Open vs. Closed Index

There are two options:

=over

=item C<open>:

The default is to keep the index I<lazy>. In this mode the index is empty at the start and it will
learn more and more by its own. In this sense, the index lives under an I<open world assumption>
(hence the name), as the absence of information does not mean that there is no result.

=item C<closed>:

A I<closed world> index has to be populated to be useful. If a query is launched and the result is
stored in the index, then it will be used, like for an open index. If no result in the index is
found for a query, the empty result will be assumed.

=back

=head2 Hash Technology

The default implementation uses an in-memory hash, no further fancy. Optionally, you can provide
your own hash object. Also one which is I<tied> to an DBM file, etc.

=head2 Map Attachment

To activate an index, you have to attach it to a map. This is done at constructor time.

It is possible (not sure how useful it is) to have one particular index to be attached to several
different maps. It is not possible to have several L<TM::Index::Match> indices attached to one map.
Indices of a different nature (non-match related) are not affected.

=head1 INTERFACE

=head2 Constructor

The only mandatory parameter for the constructor is the map for which this index should apply. The
map must be an instance of L<TM> or any of its subclasses, otherwise an exception is the
consequence. If the map already has an index of this nature, the constructor will fail with an
exception as well.

Optional parameters are

=over

=item C<closed> (default: C<0>)

This controls whether the index is operating under closed or open world assumptions.

=item C<cache> (default: C<{}>)

You optionally can pass in your own HASH reference.

=back

Example:

   my $idx = new TM::Index::Match ($tm)

B<NOTE>: When the index object goes out of scope, the destructor will make the index detach itself
from the map. Unfortunately, the exact moment when this happens is somehow undefined in Perl, so it
is better to do this manually at the end.

Example:

   {
    my $idx2 = new TM::Index::Match ($tm, closed => 1);
    ....
    } # destructor called and index detaches automatically, but only in theory

   {
    my $idx2 = new TM::Index::Match ($tm, closed => 1);
    ....
    $idx2->detach; # better do things yourself
    }

=cut

sub new {
    my $class = shift;
    my $tm    = shift;
    $main::log->logdie (scalar __PACKAGE__.": first parameter must be an instance of TM") unless ref ($tm) && $tm->isa ('TM');

    my %options = @_;
    $options{closed} ||= 0;  # we assume that this is 'open' and not 'closed'
    $options{cache}  ||= {};

    my $self = bless { 
		      %options,
		      map       => $tm
		  }, $class;

    $main::log->logdie (scalar __PACKAGE__.": cannot implant index as map has already one") if $tm->{indices}->{match};
    $self->{map}->{indices}->{match} = $self;
    return $self;
}

# has to be done, if reattachment is the plan

sub DESTROY {
    shift->detach;
}

=pod

=head2 Methods

=over

=item B<detach>

I<$idx>->detach

Makes the index detach safely from the map. The map is not harmed in this process.

=cut

sub detach {
    my $self = shift;
    $self->{map}->{indices}->{match} = undef;
    $self->{map}                     = undef;
}

=pod

=item B<populate>

I<$idx>->populate (I<@list_of_keys>)

To populate the index with canned results this method can be invoked. At this stage it is not very
clever and may take quite some time to work its way through a larger map. This is most likely
something to be done in the background.

The list of keys to be passed in is a bit black magic. Your current best bet is to look at the
index statistics method, and retrieve a proposed list from there:

   @optimized_keys = @{ $stats->{proposed_keys} };

   $idx->populate (@optimized_keys[0..2]); # only take the first few

If this list is empty, nothing clever will happen.

=cut

sub populate {
    my $self = shift;
    my @halfkeys = @_ or return;
    my $map  = $self->{map};

    $map->{indices}->{match} = undef; # detach

    my @mids = $map->midlets;
    foreach my $halfkey (@halfkeys) {
	my @keys = split /\./, $halfkey;
#warn "keys ".(join "    ", @keys);
	_combinatorial (\@mids, [], scalar @keys - 1, \@keys, $self->{closed}, $map, $self->{cache});
    }
    $map->{indices}->{match} = $self; # re-attach

sub _combinatorial {
    my $mids   = shift; # will be passed through
    my $idxs   = shift; # will be accumulated in every recursion
    my $depth  = shift; # will be decremented at every recursion
    my $keys   = shift; # just pass them through
    my $closed = shift; # pass through
    my $map    = shift;
    my $cache  = shift;

    for (my $i = 0; $i <= $#$mids; $i++) {                                     # iterate over all indices of mids
        my $l = [ @$idxs, $i ];                                                # build an uptodate index list
        if ($depth) {                                                            # we are still not at the bottom of things
            _combinatorial ($mids, $l, $depth - 1, $keys, $closed, $map, $cache);      # recurse
        } else {                                                               # we reached the correct length
#warn "have indices ".join ("..", @$l);
	    my @vals  = map { $mids->[$_] } @$l;                               # the values are all mids, taking from the mids list
	    my %query = map { $_ => shift @vals } @$keys;                      # build a match query
#warn "query ".Dumper \%query;
	    my @as    = $map->match_forall (%query);                           # compute the results
#warn "got back ".Dumper \ @as;
	    my @skeys = sort keys %query;                                      # recompute the total key (including the values)
	    my $skeys = join ('.', @skeys);
	    my @svals = map { $query{$_} } @skeys;
	    my $key   = "$skeys:" . join ('.', @svals);
#warn "computed key '$key'";

	    if (@as) {                                                         # if the match list is not empty
		$cache->{$key} = [ map { $_->[TM->LID] } @as ];                # memorize it
	    } elsif ($closed) {                                                # otherwise, if empty, check on close
		# don't do nothing, dude                                       # that's exactly the meaning of 'closed'
	    } else {
		$cache->{$key} = [];                                           # in an open world record the result
	    }
        }
    }
}
}

=pod

=item B<discard>

I<$idx>->discard

This throws away the index content.

=cut

sub discard {
    my $self = shift;
    $self->{cache} = {};
}

=pod

=item B<statistics>

I<$hashref> = I<$idx>->statistics

This returns a hash containing statistical information about certain keys, how much data is behind
them, how often they are used when adding information to the index, how often data is read out
successfully. The C<cost> component can give you an estimated about the cost/benefit.

=cut

sub statistics {
    my $self = shift;

    my %stats;
    foreach my $q (keys %{ $self->{cache} }) {
	$q =~ /([^:]+)/;
	my $ki;
	$ki->{writes}++;
	$ki->{reads} += $self->{reads}->{$q};
	$ki->{size}  += scalar @{ $self->{cache}->{$q} };

        $ki->{cost}              = $ki->{writes} / $ki->{reads};  # it is impossible that reads == 0
        $ki->{avg_size_of_read}  = $ki->{size}   / $ki->{reads};
        $ki->{avg_size_of_write} = $ki->{size}   / $ki->{writes};
        $stats{keys}->{$1} = $ki;
    }
    $stats{proposed_keys} = [ sort { $stats{keys}->{$a}->{cost} <=> $stats{keys}->{$b}->{cost} } keys %{$stats{keys}} ];
    return \%stats;
}


# these do the actual work
# like always, the working class is not even mentioned in the glossy brochure

sub is_cached {
    my $self = shift;
    my $key  = shift;
    $self->{reads}->{$key}++;
    return $self->{closed} ? $self->{cache}->{$key} || []  # we this is to be understood 'closed', then "Not stored" means "not true", i.e. empty result
                           : $self->{cache}->{$key};       # in an open interpretation, we never know
}

sub do_cache {
    my $self = shift;
    my $key = shift;
    my $data = shift;
    return $self->{cache}->{$key} = $data;
}

=pod

=back

=head1 SEE ALSO

L<TM>

=head1 COPYRIGHT AND LICENSE

Copyright 200[6] by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.

=cut

our $VERSION = 0.1;
our $REVISION = '$Id: Match.pm,v 1.1 2006/11/21 09:14:10 rho Exp $';

1;

__END__

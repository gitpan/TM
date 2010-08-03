package TM::IndexAble;

use strict;
use warnings;

use Data::Dumper;
use Class::Trait 'base';

=pod

=head1 NAME

TM::IndexAble - Topic Maps, Trait to provide lazy and eager indices

=head1 SYNOPSIS

      my $tm = new TM...                           # get any map
      use Class::Trait;
      Class::Trait->apply ($tm, "TM::IndexAble");  # apply the trait

      # add a lazy cache for subclassing and instanceOf
      $tm->index ({ axis => 'taxo' });
      $tm->index ({ axis => 'taxo', closed => 0}); # the same, lazy is NOT closed

      # add eager cache (= index) for taxonometrics
      $tm->index ({ axis => 'taxo', closed => 1}); # eager is closed, will take some time

      # add index for characteristics
      $tm->index ({ axis => 'char'});
      $tm->index ({ axis => 'char', closed => 1}); # can be closed as well

      # ditto for reification
      $tm->index ({ axis => 'reify'});
      $tm->index ({ axis => 'reify', closed => 1});

      # create index/caches, but separate from map itself
      $tm->index ({ axis => 'reify', closed => 0, detached => {} });


      my %stats = $tm->index;                      # get current indices + statistics

=head1 DESCRIPTION

Like L<TM::Index>, this package also adds index/caching capabilities to any topic map stored via
L<TM> (or any of its subclasses). The difference, though, is that the index/caching functionality
is added as a trait, and not via an explicit attachment. The indices are - by default - part of
the map, and not standalone objects as with L<TM::Index>.

When you add an index/cache then you simply use precomputed navigation results for the TM methods
C<match_forall> and C<is_reified> (but not used for C<reifies>).

As with L<TM::Index> you can create caching (lazy indexing) and full indices (eager precaching).

=head2 Map Attachment

To enrich a map with an index/cache, you call the method C<index> provided here. The index/cache
will by default be stored I<inside> the map. That may be convenient in most cases.

If not - as with some storage techniques - you can detach the index to live within your scope. For
that purpose you simply pass in an empty hash reference. It is then your responsibility to get rid
of it afterwards.

Having the index detached also opens the way for you to make the index persistent.

=head1 INTERFACE

=head2 Methods

Following methods are mixed into the class/object:

=over

=item B<index>

I<$tm>->index ({ %spec }, ...)

This method establishes one or more indices/caches to the topic map. Each cache/index is described
with its own hash reference.

Which navigation axes should be covered by a single cache/index is specified with the C<axis> field. It
can have as value one of the axes in L<TM::Axes>, or one of the following values:

=over

=item C<taxo>

Shortcut for the axes: C<subclass.type> C<superclass.type> C<class.type> C<instance.type>

=item C<char>

Shortcut for the axes: C<char.topic> C<char.value> C<char.type> C<char.type.value> C<char.topic.type>

=item C<reify>

=back

To control whether a cache (lazy indexing) or a full index (eager caching) should be
used, the field C<closed> can have two values (default is C<0>):

=over

=item C<0>:

The default is to keep the index I<lazy>. In this mode the index is empty at the start and it will
learn more and more on its own. In this sense, the index lives under an I<open world assumption>
(hence the name), as the absence of information does not mean that there is no result.

=item C<1>:

A I<closed world> index has to be populated to be useful. If a query is launched and the result is
stored in the index, then it will be used, like for an open index. If no result in the index is
found for a query, the empty result will be assumed.

=back

Additionally, a field C<detached> can be passed in for one cache/index. It MUST contain a hash
reference.

Example:

   $tm->index (
           { axis => 'reify', closed => 0, detached => {} },
           { axis => 'char',  closed => 1 }
   );


The method returns a hash with some statistical information for every axis:

=over

=item C<requests>

Number of requests since inception of the index.

=item C<hits>

Number of cache hits since inception. For an eager cache (i.e. index) this number
should be the same as C<requests>

=back


=cut

our %cachesets;

sub index {
    my $self = shift;

    my $index = $self->{index};
    $index ||= {};                                                     # just to create some infrastructure

    foreach my $idx (@_) {                                             # whatever we are given by the user
	my @a;
	use feature 'switch';
	given ($idx->{axis}) {
	    when ('taxo') {                                            # "taxo" shortcuts some axes
		@a = qw(subclass.type superclass.type class.type instance.type);
	    }
	    when ('char') {                                            # char shortcut
		@a = qw(char.topic char.value char.type char.type.value char.topic.type);
	    }
	    when ('reify') {                                           # this is a special one
		@a = qw(reify);
	    }
	    default {                                                  # take that as-is
		@a = $idx->{axis};
	    }
	}

	my $cache;                                                     # handle
	if (my $detached = $idx->{detached}) {                         # if user provided a detachable cache, we take that
	    $cachesets{$detached} = $detached;                         # register that locally (as key this will be stringified)
	    $index->{$_} = "$detached" foreach @a;                     # and memorize that the real information is in a detached one, not inside the map
	    $cache         = $detached;                                # from now on we work with that
	} else {
	    $cache        = $index;                                    # we take the indices to be part of the map
	}

	map {
	    $cache->{$_}->{hits}     //= 0,                            # initialize stats
	    $cache->{$_}->{requests} //= 0,
	    $cache->{$_}->{data}     //= {}
	} @a;

	_populate ($self, $cache, @a)                                  # fill the cache for these axes if ..
	    if $idx->{closed};                                         # ... if the cache is a real index (not just a cache)
    }
    $self->{index} = $index;                                           # kick MLDBM in the arse

    return                                                             # we return a hash (ref) with fields
    map { $_->[0] => {
	  hits     => $_->[1]->{hits},                                 # hits holding the number of cache hits since inception
	  requests => $_->[1]->{requests}                              # and the number of requests
	  } }
    map  { 
	[
	 $_,
	 ref ($self->{index}->{$_})
	      ? $self->{index}->{$_}
  	      : $cachesets{ $self->{index}->{$_} }->{$_}
	]
         }
    keys %{ $self->{index} };
}

sub _populate {
    my $tm = shift;
    my $cache = shift;

    foreach my $a (@_) {                                                                   # walk over all (closed, otherwise we would not be here) axes
	if ($a eq 'reify') {                                                               # this is a special case
	    _populate_reify ($tm, $cache->{$a}->{data});
	} else {
	    my $enum = $TM::forall_handlers{$a}->{enum}                                    # how to generate all assertions of that axes
	    or die "unsupported index axes $a";                                            # complain if enumeration is not yet supported
	    my $key  = $TM::forall_handlers{$a}->{key};                                    # how to derive a full cache key from one assertion
	    
	    my %as;                                                                        # collect the assertions for that axis $a
	    map { push @{ $as{ &$key ($tm, $_) } } , $_->[TM->LID] }                       # sort them according to the key
	        &$enum ($tm) ;                                                             # generate all assertions fitting this axis

	    map { $cache->{$a}->{data}->{$_} = $as{$_} }                                   # store the corresponding lists into the cache
   	        keys %as;                                                                  # walk through keys
	}
	$cache->{$a}->{closed} = 1;
    }
}

sub _populate_reify {
    my $tm   = shift;
    my $data = shift;

    my $mid2iid = $tm->{mid2iid};

    %$data = map  { $mid2iid->{$_}->[TM->ADDRESS] => $_ }                                  # invert the index
             grep { $mid2iid->{$_}->[TM->ADDRESS] }                                        # only those which "reify" something survive
                keys %{$mid2iid};                                                          # all toplet tids
}

=pod

=item B<deindex>

I<$tm>->deindex (I<$axis>, ....)

This method gets rid of certain indices/caches, as specified by their axes.

=cut

sub deindex {
    my $self = shift;

    my $index = $self->{index};
    foreach my $a (@_) {
	if (ref ($index->{$a})) {
	    delete $index->{$a};
	} else {
	    delete $cachesets{ $index->{$a} };
	    delete $index->{$a};
	}
    }
    $self->{index} = $index;
}

=pod

=cut

#-- trait mixins

sub match_forall {
    my $self   = shift;
    my %query  = @_;
#warn "forall ".Dumper \%query;

    my @skeys = sort keys %query;                                                           # all fields make up the key
    my $skeys = join ('.', @skeys);
    my @svals = map { $query{$_} } @skeys;
    my $key   = "$skeys:" . join ('.', @svals);

#    warn "i match ".$skeys;
    unless ( my $c = $self->{index}->{$skeys} ) {
	return TM::_dispatch_forall ($self, \%query, $skeys, @svals);

    } else {
	$c = $cachesets{$c}->{$skeys}                                          # replace referenced cache with the in-memory
	      unless ref ($c);                                                 # except it it already a workable cache

	$c->{requests}++;
	my $data  = $c->{data};
	
	if (my $lids = $data->{ $key }) {
#	    warn "cached $key";
	    $c->{hits}++;
	    return map { $self->{assertions}->{$_} } @$lids;                            # and return fully fledged assocs
	}
	return [] if $c->{closed};                                                      # the end of wisdom
	my @as = TM::_dispatch_forall ($self, \%query, $skeys, @svals);
	$data->{ $key } = [ map { $_->[TM->LID] } @as ];
	return @as;
    }
}



sub is_reified {
    my $self = shift;                                                          # the map
    my $a    = shift;                                                          # the thing (assertion or otherwise)

    my $index = $self->{index};
    unless ( my $c = $index->{reify} ) {                                       # if an index over reify has NOT been activated
	return $self->_is_reified ($a);                                        # we look only at the source

    } else {                                                                   # we have an index
	$c = $cachesets{$c}->{reify}                                           # replace referenced cache with the in-memory
	      unless ref ($c);                                                 # except it it already a workable cache

	$c->{requests}++;                                                      # bookkeeping
	my $data  = $c->{data};                                                # shortcut

	my $k = ref ($a) ? $a->[TM->LID] : $a;
	if (my $tid = $data->{ $k }) {                                         # cache always holds list references
	    $c->{hits}++;                                                      # bookkeeping
	    return ($tid);
	}
	return () if $c->{closed};                                             # the end of wisdom
	warn "no hit!";
	my @tids = $self->_is_reified ($a);                                    # returns a list (strangely)
	$data->{ $k } = $tids[0];                                              # tuck it into the cache
	return @tids;                                                          # and give it back to the caller
    }
}

=pod

=back

=head1 SEE ALSO

L<TM>, L<TM::Index>

=head1 COPYRIGHT AND LICENSE

Copyright 20(10) by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.

=cut

our $VERSION = 0.2;

1;

__END__


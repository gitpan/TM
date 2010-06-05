package TM::Index::Reified;

use strict;
use warnings;
use Data::Dumper;

use TM;
use base qw(TM::Index);

=pod

=head1 NAME

TM::Index::Reified - Topic Maps, Indexing support (reification axis)

=head1 SYNOPSIS

    # somehow get a map (any subclass of TM will do)
    my $tm = ... 

    # one option: create an eager index (immediate population)
    use TM::Index::Reified;
    my $idx = new TM::Index::Reified ($tm, closed => 1);
    
    # for most operations which involve is_reified to be called
    # should be much faster

=head1 DESCRIPTION

This index can be attached to a map if the method C<is_reified> is about to be called very often.
Most likely you will want to have the index to be closed, i.e. populated.

The package inherits most of its functionality from L<TM::Index>.

B<NOTE>: As for all indices, modifications of the underlying map are not reflected automatically.

=head1 INTERFACE

=head2 Constructor

The constructor/destructor is inherited from L<TM::Index>.

=head2 Methods

=over

=item B<attach>

=item B<detach>

This index attaches in a special way to the map.

=cut

sub attach {
    my $self = shift;
    $self->{map}->{rindex} = $self;          # there will only be one
}

sub detach {
    my $self = shift;
    $self->{map}->{rindex} = undef;
    $self->{map} = undef;
}

=pod

=item B<populate>

Invoking this, you will fill this index with authoritative information.

=cut

sub populate {
    my $self  = shift;
    my $map   = $self->{map};
    my $cache = $self->{cache};

    my $mid2iid = $map->{mid2iid};

    map { $cache->{ $mid2iid->{$_}->[TM->ADDRESS] } = $_ }  # invert the index
        grep { $mid2iid->{$_}->[TM->ADDRESS] }              # only those which "reify" something survive
        keys %{$mid2iid};                                   # all toplet tids

    $self->{closed} = 1;

}

=back

=head1 SEE ALSO

L<TM>, L<TM::Index>

=head1 COPYRIGHT AND LICENSE

Copyright 2010 by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.

=cut

our $VERSION = 0.2;

1;

__END__

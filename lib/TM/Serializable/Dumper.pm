package TM::Serializable::Dumper;

use base qw(TM::Serializable);

sub serialize {
    my $self = shift;
    use Data::Dumper;

    return Data::Dumper->Dump ([$self], ['tm']);
}

sub deserialize {
    my $self = shift;
    my $s    = shift;
    my $tm;
    eval $s;
    $self->melt ($tm);
}

1;

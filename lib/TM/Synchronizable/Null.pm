package TM::Synchronizable::Null;

use base qw(TM::Synchronizable);

sub source_in {
    my $self = shift;
    $self->{_ins}++;
}

sub source_out {
    my $self = shift;
    $self->{_outs}++;
}

1;



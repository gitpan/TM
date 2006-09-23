package TM::Materialized::MLDBM;

use TM::Resource;
use base qw (TM::Resource);

use Data::Dumper;

use MLDBM qw(DB_File Storable);        # use Storable for serializing
use Fcntl qw(:DEFAULT);
use MLDBM::Sync;

=pod

=head1 NAME

TM::Materialized::MLDBM - Topic Maps, DBM Storage (asynchronous)

=head1 SYNOPSIS

    my $tm = new TM::Materialized::MLDBM (file => '/tmp/map.dbm');
    # modify the map here.....
    # and flush everything onto the file
    $tm->sync_out;

    # later in this game, get it back from file
    my $tm2 = new TM::Materialized::MLDBM (file => '/tmp/map.dbm');
    $tm2->sync_in;


=head1 DESCRIPTION

This package just implements L<TM::Resource> with a MLDBM store. The methods C<sync_in> and
C<sync_out> do the obvious things of copying between the DBM file and the in-memory
representation. Only during the synchronisation the DBM file is locked. Otherwise the two copies are
independent.

The synchronisation is quite performant, certainly faster than using a text representation of the
map.

=cut

sub new {
    my $class = shift;
    my %options = @_;

    $main::log->logdie ("no file specified") unless $options{file};
    return bless $class->SUPER::new (%options), $class;
}

sub _sync_in {
    my $self = shift;
    (my $filename = $self->url) =~ s/^file://;                                     # get rid of this
#warn "_sync in  '$filename'";
    my %map;
    my $sync_dbm = tie %map, 'MLDBM::Sync', $filename, O_RDWR|O_CREAT, 0600 or $main::log->logdie (scalar __PACKAGE__ .": $!");
    $sync_dbm->Lock;
    %{$self} = %{$map{data}};
    $sync_dbm->UnLock;
}

sub _sync_out {
    my $self = shift;
    (my $filename = $self->url) =~ s/^file://;                                     # get rid of this
#warn "_sync out  '$filename'";
    my %map;
    my $sync_dbm = tie %map, 'MLDBM::Sync', $filename, O_CREAT|O_RDWR, 0600 or $main::log->logdie (scalar __PACKAGE__ .": $!");
    $sync_dbm->Lock;
    $map{data} = $self;
    $sync_dbm->UnLock;
}

=pod

=head1 SEE ALSO

L<TM::Resource>

=head1 AUTHOR INFORMATION

Copyright 200[6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.
http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION  = '0.01';
our $REVISION = '$Id: MLDBM.pm,v 1.1 2006/09/19 10:20:33 rho Exp $';

1;

__END__

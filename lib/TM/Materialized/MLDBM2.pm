package TM::Materialized::MLDBM2;

use TM::Resource;
use base qw (TM::Resource);

use Data::Dumper;

use BerkeleyDB ;
use MLDBM qw(BerkeleyDB::Hash) ;
use Fcntl qw(:DEFAULT);

=pod

=head1 NAME

TM::Materialized::MLDBM2 - Topic Maps, DBM Storage (synchronous

=head1 SYNOPSIS

   {
    my $tm = new TM::Materialized::MLDBM2 (file => '/tmp/map.dbm');
    # modify the map here.....

    } # it goes out of scope here, and all changes are written back

   # later in the game
   {
    my $tm = new TM::Materialized::MLDBM2 (file => '/tmp/map.dbm');
    # we are back in business
    }

=head1 DESCRIPTION

This package just implements L<TM::Resource> with a BerkeleyDB store. Unlike
L<TM::Materialized::MLDBM> this module does not need explicit synchronisation with the external
resource (the DBM file here).  It ties content-wise with the DBM file at constructor time and unties
at DESTROY time.

The advantage of this storage form is that there is little memory usage. Only those fractions of the
map are loaded which are actually needed. If one has very intense interactions with the map (as a
query processor has), then this storage technique is not optimal.

=cut

sub new {
    my $class = shift;
    my %options = @_;

    $main::log->logdie ("no file specified") unless $options{file};

    my %self;

    if (-e $options{file}) {                                                        # file does exist already
	tie %self, 'MLDBM', -Filename => $options{file}
	    or main::log->logdie ( "Cannot create DBM file '$options{file}: $!");

    } else {                                                                        # no file yet
	tie %self, 'MLDBM', -Filename => $options{file},                            # bind to one
                            -Flags    => DB_CREATE                                  # which we create here
	    or main::log->logdie ( "Cannot create DBM file '$options{file}: $!");
	my $parent = TM::Resource->new (%options);
	foreach (keys %$parent) {                                                   # clone all components
	    $self{$_} = $parent->{$_};                                              # this makes sure that Berkeley'ed tie picks it up
	}
    }
    return bless \%self, $class;                                                    # give the reference a blessing
}

sub _sync_in {
}

sub _sync_out {
}

sub DESTROY {                                                                       # if an object went out of scope
    my $self = shift;
    untie %$self;                                                                   # release the tie with the underlying resource
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
our $REVISION = '$Id: MLDBM2.pm,v 1.1 2006/09/23 01:23:25 rho Exp $';

1;

__END__

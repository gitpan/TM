package TM::Materialized::File;

use TM;
use base qw(TM::Resource);

=pod

=head1 NAME

TM::Materialized::File - Topic Maps, abstract class for stream (map) based input/output drivers

=head1 SYNOPSIS

  # this class will never be directly used for instantiation
  # see the description in TM and individual low-level drivers

=head1 DESCRIPTION

This class is a subclass of L<TM::Resource>, so it implements maps which are connected to a
resource. It is abstract, though, as it only defined how a stream-based driver package should
behave. It may thus be inherited by classes which implement external formats
(L<TM::Materialized::AsTMa>, ....).

=head1 INTERFACE

=head2 Methods

The methods of synchronisation is defined here. They should not be directly called by a user, as
they will be triggered by C<sync_in> and C<sync_out> from the superclass.

What deriving implementations MUST implement are methods how to I<deserialize> streams into maps and
how to I<serialize> maps to streams. If the serialization format supports this, of course.

Note: No map consolidation is triggered here. The application has to do this separately.

=cut

sub _sync_in {
    my $self = shift;
    my $url  = $self->{_in_url} || $self->{url} || die "something's wicked";
    
    use TM::Utils;
    my $content = TM::Utils::get_content ($url);
#  $main::log->debug ("synced in stream from $url:". $content);
    
    $self->deserialize ($content);
    $self->{last_mod} = time;
}

sub _sync_out {
    my $self = shift;
    my $url  = $self->{url} || $self->{_out_url} || die "something's wicked";

    my $content = $self->serialize;
    TM::Utils::put_content ($url, $content);
}

=pod

=head1 SEE ALSO

L<TM>

=head1 AUTHOR INFORMATION

Copyright 200[2-6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.
http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION = 0.12;
our $REVISION = '$Id: File.pm,v 1.10 2006/09/17 02:10:39 rho Exp $';

1;

__END__

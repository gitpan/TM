package TM::Resource;

use strict;
use warnings;

use TM;
use base qw(TM);

use TM::Utils;
use Data::Dumper;

=pod

=head1 NAME

TM::Resource - Topic Maps, abstract class for resource-backed Topic Maps

=head1 SYNOPSIS

  # this class is probably only interesting for implementors of individual
  # low-level drivers

  # see TM for the 'application engineer' API

=head1 DESCRIPTION

This class is a subclass of L<TM>, so it implements map objects. It is abstract, though, as it only
defined how a resource-backed driver package should behave. It may thus be inherited by classes
which implement external formats (L<TM::Materialized::AsTMa>, L<TM::Materialized::XML>, ....) or
virtual maps connected to resources.

It defines synchronisations with external resources (read: local/remote files, everything which can
be addressed via a URL) given that the map has a last-modified data as the resource.

The methods C<sync_in>, C<sync_out> and C<last_mod> implement the synchronization between the
in-memory data structure and the content on the external resource. That resource is specified via a
URI.

=head2 Predefined URIs

The following resources, actually their URIs are predefined:

=over

=item C<io:stdin>

Symbolizes the UNIX STDIN file descriptor. The resource is all text content coming from this file.

=item C<io:stdout>

Symbolizes the UNIX STDOUT file descriptor.

=item C<null:>

Symbolizes a resource which never delivers any content and which can consume any content silently
(like C</dev/null> under UNIX).

=back

=head2 Predefined URI Methods

=over

=item C<inline>

An I<inlined> resource is a resource which contains all content as part of the URI.

=back

=head1 INTERFACE

=head2 Constructor

The constructor of implementations should expect a hash as parameter containing the field(s) from
L<TM> and one or more of the following:

=over

=item I<url>:

If given, then the instance will be read from this url whenever synced in.

=item I<file>:

If given, then the data will be read/written from/to this file. This is just a convenience as it
will be mapped to I<url>.

=item I<inline>:

If given, then the instance will be read directly from this text provided inline when synced.

=back

If several fields (C<file>, C<url>, C<inline>) are specified, it is undefined which one will be
used.

Examples (using the AsTMa driver as example):

   # opening from an AsTMa= file
   $atm = new TM::Materialized::AsTMa (file   => 'here.atm');

   # why need a file? files are evil, anyway
   $atm = new TM::Materialized::AsTMa (inline => '# this is AsTMa');

=cut

sub new {
  my $class   = shift;
  my %options = @_;

  my $url   = 'inline:'.delete $options{inline} if defined $options{inline};
     $url   = 'file:'.  delete $options{file}   if defined $options{file};
     $url   =           delete $options{url}    if defined $options{url};
     $url ||= 'null:'; # default

  return bless $class->SUPER::new (%options, url => $url), $class;
}

=pod

=head2 Methods

=over

=item B<url>

I<$tm>->url

Once an object of this class is instantiated it keeps the URL of the resource to which it is
associated. This descriptor will always be in form of a URL, regardless whether you used a URL, a
file or inline text at instantiation time.

With this method you can retrieve that. You cannot change the URL (well you can, but nothing special
will happen).

=cut

sub url {
    my $self = shift;
    return $self->{_in_url} || $self->{_out_url} || $self->{url};
}

=pod

=item B<last_mod>

I<$time> = I<$tm>->last_mod

This function returns the UNIX time when the resource has been modified last. C<undef> is returned
if the result cannot be determined.

=cut

sub last_mod {
    my $self = shift;

    use TM::Utils;
    return TM::Utils::last_mod ($self->{_in_url} || $self->{_out_url} || $self->{url});
}

=pod

=item B<sync_in>

I<$tm>->sync_in

If the map is connected to a resource, then this method will try to load the content behind the
resource into the map. Depending on the driver, most likely any existing content will be replaced.

If the resource URI is C<io:stdout>, then nothing happens.

If the resource URI is C<null:>, then nothing happens.

If the last modification date of the resource is not younger than that of the map, then no
synchronisation happens.

This method provides only the main logic, wether a synchronisation should occur. Implementations,
such as materialized maps synchronizing from an XTM resource, will have to implement their specific
mechanism which will be triggered here. This is accomplished by a virtual method

   sub _sync_in {
       my $self = shift;

       # here the real work happens

   }

which all implementations have to provide. (See L<TM::Materialized::File> for an example.)

=cut

sub _sync_in {
};

sub sync_in {
    my $self = shift;

#warn "generic TM sync in";
    if (my $url = $self->{_in_url} || $self->{url}) {
#warn __PACKAGE__ . ": is it stdout? null?";
        return if $url eq 'io:stdout';   # no syncing in from STDOUT
        return if $url eq 'null:';       # no syncing in from null
#warn __PACKAGE__ . ":syncing in native, checking last mods (tm = ".$self->{last_mod}." resource= ".$self->last_mod;
	$self->_sync_in if ! $self->{last_mod} ||                                             # virginal map, let's deflour.
	                     $self->{last_mod} < ( $self->last_mod || Time::HiRes::time + 1); # if undef was returned, we just do it
        $self->{last_mod} = Time::HiRes::time;
    } 
}

=pod

=item B<sync_out>

I<$tm>->sync_out

If a map is connected to a resource, then this method contains the logic under which circumstances
to synchronize with the external resource.

If the resource URI is C<io:stdin>, nothing happens.

If the resource URI is C<null:>, nothing happens.

If the resource URI is C<inline:..> nothing happens.

If the map has not changed since the last modification of the external resource, nothing happens.

The real functionality has to be provided by implementations which define

  sub _sync_out {
      my $self = shift;

      # hard work here
  }

=cut

sub _sync_out {
}

sub sync_out {
    my $self = shift;

    my $url = $self->{_out_url} || $self->{url};
#warn __PACKAGE__ . "mat sync out of $self (". $url .")";

    return unless $url;
#warn __PACKAGE__ . ": $url is it stdin? null? inline?";
    return if $url eq 'io:stdin'; # no syncing out to STDIN
    return if $url eq 'null:';    # no syncing out to null
    return if $url =~ /^inline:/; # no syncing out to inline
#warn __PACKAGE__ . ": no it was not";
#warn __PACKAGE__ . ":syncing out native, lastmod (map) ".$self->{last_mod}. " last_mod (resource): ".$self->last_mod;
    $self->_sync_out if  $self->{last_mod} &&                           # virginal map does not go out!
	                 $self->{last_mod} > ( $self->last_mod || 0 );  # if undef was returned, we just do it
#warn __PACKAGE__. ": end of mat sync out";
}

=pod

=back

=head1 SEE ALSO

L<TM>

=head1 AUTHOR INFORMATION

Copyright 200[2-6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.
http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION = 0.3;
our $REVISION = '$Id: Resource.pm,v 1.4 2006/09/24 08:27:27 rho Exp $';

1;

__END__

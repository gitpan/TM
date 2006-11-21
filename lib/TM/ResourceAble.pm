package TM::ResourceAble;

use strict;
use warnings;

use Class::Trait 'base';

our @REQUIRES  = qw(last_mod);

use Data::Dumper;
use Time::HiRes;

=pod

=head1 NAME

TM::ResourceAble - Topic Maps, abstract trait for resource-backed Topic Maps

=head1 SYNOPSIS

   package MyNiftyMap;

     use TM;
     use base qw(TM);
     use Class::Trait ('TM::ResourceAble');
 
     1;

   my $tm = new MyNiftyMap;
   $tm->url ('http://nirvana/');

   warn $tm->mtime;

   # or at runtime even:

   use TM;
   Class::Trait->apply ('TM', qw(TM::ResourceAble));
   my $tm = new TM;
   warn $tm->mtime;
   

=head1 DESCRIPTION

This traits adds methods to provide the role I<resource> to a map. That allows a map to be
associated with a resource which is addressed by a URL (actually a URI for that matter).

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

=head2 Methods

=over

=item B<url>

I<$url> = I<$tm>->url
          I<$tm>->url (I<$url>)

Once an object of this class is instantiated it keeps the URL of the resource to which it is
associated. With this method you can retrieve and set that. No special further action is taken
otherwise.

=cut

sub url {
    my $self = shift;
    my $url  = shift;
    return $url ? $self->{url} = $url : $self->{url};
}

=pod

=item B<mtime>

I<$time> = I<$tm>->mtime

This function returns the UNIX time when the resource has been modified last. C<undef> is returned
if the result cannot be determined. All methods from L<LWP> are supported.

Special resources are treated as follows:

=over

=item C<null:> 

always has mtime C<0>

=item C<io:stdin> 

always has an mtime 1 second in the future. The idea is that STDIN always has new
content.

=item C<io:stdout> 

always has mtime C<0>. The idea is that STDOUT never changes by itself.

=back

=cut

sub mtime {
    my $self = shift;

#warn "xxxx mtime in $self for url $self->{url}";

    my $url = $self->{url} or die "no URL specified for this resource\n";

    if ($url =~ /^file:(.+)/) {
	use File::stat;
	my $stats = stat ($1) or die "file '$1' is not accessible (or does not exist)";
#warn "file stats ".Dumper $stats;
#warn "will return ".$stats->mtime;
	return $stats->mtime;
    } elsif ($url =~ /^inline:/) {
	return $self->{created}; ## Time::HiRes::time + 1;      # how can I know?
    } elsif ($url eq 'null:') {
	return 0;
    } elsif ($url eq 'io:stdin') {
	return Time::HiRes::time + 1;                           # this always changes, by definition
    } elsif ($url eq 'io:stdout') {
	return 0;
    } else {                                                    # using LWP is a bit heavyweight, but anyways
	use LWP::UserAgent;
	my $ua = LWP::UserAgent->new;
	$ua->agent("TimeTester 1.0");
	
	my $req = HTTP::Request->new(GET => $url);
	my $res = $ua->request($req);
	
	use HTTP::Date;
	return str2time($res->headers->{'last-modified'});
    }
}

=pod

=back

=head1 SEE ALSO

L<TM>

=head1 AUTHOR INFORMATION

Copyright 200[6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.  http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION = 0.1;
our $REVISION = '$Id: ResourceAble.pm,v 1.1 2006/11/13 08:02:33 rho Exp $';

1;

__END__



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

@@@@@@@@@@@@@@@@

   sub _sync_in {
       my $self = shift;

       # here the real work happens

   }

which all implementations have to provide. (See L<TM::Materialized::File> for an example.)

 =cut

sub source_in {
    warn "SOURCE IN";
}

sub resource {
    my $self = shift;
    my $res  = shift;

    return $res ? $self->{_resource} = $res : $self->{_resource};
}

sub sync_in {
    my $self = shift;

    $self->source_in if $self->last_mod           # modification in map
                        <
                        $self->resource->last_mod # modification of resource
}


sub xsync_in {
    my $self = shift;

#warn "generic TM sync in";
    if (my $url = $self->{url}) {
#warn __PACKAGE__ . ": is it stdout? null?";
        return if $url eq 'io:stdout';   # no syncing in from STDOUT
        return if $url eq 'null:';       # no syncing in from null
#warn __PACKAGE__ . ":syncing in native, checking last mods (tm = ".$self->{last_mod}." resource= ".$self->last_mod;
	$self->_sync_in if ! $self->{last_mod} ||                                             # virginal map, let's deflour.
	                     $self->{last_mod} < ( $self->last_mod || Time::HiRes::time + 1); # if undef was returned, we just do it
        $self->{last_mod} = Time::HiRes::time;
warn "syncin last_mod".$self->{last_mod};
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

sub xxxxsync_out {
    my $self = shift;

    my $url = $self->{_out_url} || $self->{url};
warn __PACKAGE__ . "mat sync out of $self (". $url .")";

    return unless $url;
#warn __PACKAGE__ . ": $url is it stdin? null? inline?";
    return if $url eq 'io:stdin'; # no syncing out to STDIN
    return if $url eq 'null:';    # no syncing out to null
    return if $url =~ /^inline:/; # no syncing out to inline
warn __PACKAGE__ . ": no it was not (package $self)";
warn __PACKAGE__ . ":syncing out native, lastmod (map) ".$self->{last_mod}. " last_mod (resource): ".$self->last_mod;
    $self->_sync_out if  $self->{last_mod} &&                             # virginal map does not go out!
	                 $self->{last_mod} > ( $self->last_mod || 0 );  # if undef was returned, we just do it
warn __PACKAGE__. ": end of mat sync out";
}

 =pod



xx=head2 Constructor

The constructor of implementations should expect a hash as parameter containing the field(s) from
L<TM> and one or more of the following:

x=over

x=item I<url>:

If given, then the instance will be read from this url whenever synced in.

x=item I<file>:

If given, then the data will be read/written from/to this file. This is just a convenience as it
will be mapped to I<url>.

x=item I<inline>:

If given, then the instance will be read directly from this text provided inline when synced.

x=back

If several fields (C<file>, C<url>, C<inline>) are specified, it is undefined which one will be
used.

Examples (using the AsTMa driver as example):

   # opening from an AsTMa= file
   $atm = new TM::Materialized::AsTMa (file   => 'here.atm');

   # why need a file? files are evil, anyway
   $atm = new TM::Materialized::AsTMa (inline => '# this is AsTMa');

x=cut

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




 =head1 SYNOPSIS

@@@@@@@

  # this class is probably only interesting for implementors of individual
  # low-level drivers

  # see TM for the 'application engineer' API

 =head1 DESCRIPTION

@@@@

This class is a subclass of L<TM>, so it implements map objects. It is abstract, though, as it only
defined how a resource-backed driver package should behave. It may thus be inherited by classes
which implement external formats (L<TM::Materialized::AsTMa>, L<TM::Materialized::XML>, ....) or
virtual maps connected to resources.

It defines synchronisations with external resources (read: local/remote files, everything which can
be addressed via a URL) given that the map has a last-modified data as the resource.

The methods C<sync_in>, C<sync_out> and C<last_mod> implement the synchronization between the
in-memory data structure and the content on the external resource. That resource is specified via a
URI.


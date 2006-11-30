package TM::Synchronizable;

use strict;
use warnings;

use Data::Dumper;

use Class::Trait 'base';
use Class::Trait 'TM::ResourceAble';

#use TM::ResourceAble;
#use base qw(TM::ResourceAble);
##our @REQUIRES = qw(source_in source_out);

=pod

=head1 NAME

TM::Synchronizable - Topic Maps, trait for synchronizable resources

=head1 SYNOPSIS

   # you write an input/output driver
   # see for example TM::Synchronizable::MLDBM
   package My::WhatEver;

     # provides source_in and/or source_out methods
     sub source_in  { .... }

     sub source_out { .... }

     1;

   # you construct your map class
   package MySyncableMap;

     use TM;
     use base qw(TM);
     use Class::Trait qw(TM::ResourceAble TM::Synchronizable My::WhatEver);
 
     1;

   # you then use that
   my $tm = MySyncableMap (url => 'file:/where/ever');
   $tm->sync_in;
   # work with the map, etc...
   $tm->sync_out;

=head1 DESCRIPTION

This trait implements the abstract synchronization between in-memory topic maps and the resources
which are attached to them, i.e. files, web pages, etc. whatever can be addressed via a URI.
Consequently, this trait inherits from L<TM::ResourceAble>, although L<Class::Trait> does not do
this for you (sadly).

The trait provides the methods C<sync_in> and C<sync_out> to implement the synchronization. In this
process it uses the timestamp of the map (C<last_mod>) and that of the resource C<mtime>.

Unfortunately, the granularity of the two are different (at least on current UNIX systems): for the
I<last modification> time values from L<Time::HiRes> is used. UNIX resources only use an integer.

B<Note>: This needs a bit of consideration from the user's side.

=head1 INTERFACE

=head2 Methods

=over

=item B<sync_in>

I<$tm>->sync_in

This method provides only the main logic, wether a synchronisation from the resource into the
in-memory map should occur. If the last modification date of the resource (C<mtime>) is more recent
than that of the map (C<last_mod>), then synchronisation from the resource to the in-memory map will
be triggered. For this, a method C<source_in> has to exist for the map object; that will be invoked.

=cut

sub sync_in {
    my $self = shift;
    my $url  = $self->url;

#warn "sync in $self";
#warn "map change:      ".$self->last_mod;
#warn "resource change: ".$self->mtime;

    $self->source_in if $self->last_mod           # modification in map
                        <                         # earlier than
                        $self->mtime + 1;         # modification of resource
}

=pod

=item B<sync_out>

I<$tm>->sync_out

This method provides the logic, whether synchronisation from the in-memory map towards the attached
resource should occur or not. If the last modification date of the map (C<last_mod>) is more recent
than that of the resource (C<mtime>), then a method C<source_out> for the object is triggered.

=cut

sub sync_out {
    my $self = shift;
    my $url  = $self->url;

#    warn "sync out";
#    warn "map change:      ".$self->last_mod;
#    warn "resource change: ".$self->mtime;

    $self->source_out if $self->last_mod         # map modification
                         >                       # later than
                         ( $self->mtime || 0 );  # resource modification
}

=pod

=back

=head1 SEE ALSO

L<TM>, L<TM::ResourceAble>

=head1 AUTHOR INFORMATION

Copyright 200[6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.  http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION = 0.2;
our $REVISION = '$Id: Synchronizable.pm,v 1.4 2006/11/26 22:01:32 rho Exp $';

1;

__END__


x=item B<sync>



@@@@ this is only a full-bidir sync @@@@

x=cut

sub sync {
    my $self = shift;
    my $url  = $self->url;

#    warn "sync";
#    warn "map change:      ".$self->last_mod;
#    warn "resource change: ".$self->mtime;

    if ($self->last_mod < $self->mtime + 1) {      # map changed earlier than resource => resource is younger, + benefit of doubt (integer cutoff)
#warn "decided to source in in synchronizable";
	$self->source_in;

    } elsif ($self->mtime < $self->last_mod) { # map is younger
#warn "decided to source out in synchronizable";
	$self->source_out;
    }                                          # the unlikely case, that they are the same age
}

x=pod


sub xxxxsync_out {
    my $self = shift;

    my $url = $self->{_out_url} || $self->{url};
warn __PACKAGE__ . "mat sync out of $self (". $url .")";

    return unless $url;
#warn __PACKAGE__ . ": $url is it stdin? null? inline?";
warn __PACKAGE__ . ": no it was not (package $self)";
warn __PACKAGE__ . ":syncing out native, lastmod (map) ".$self->{last_mod}. " last_mod (resource): ".$self->last_mod;
    $self->_sync_out if  $self->{last_mod} &&                             # virginal map does not go out!
	                 $self->{last_mod} > ( $self->last_mod || 0 );  # if undef was returned, we just do it
warn __PACKAGE__. ": end of mat sync out";
}


sub xsync_in {
    my $self = shift;

#warn "generic TM sync in";
    if (my $url = $self->{url}) {
#warn __PACKAGE__ . ": is it stdout? null?";
#warn __PACKAGE__ . ":syncing in native, checking last mods (tm = ".$self->{last_mod}." resource= ".$self->last_mod;
	$self->_sync_in if ! $self->{last_mod} ||                                             # virginal map, let's deflour.
	                     $self->{last_mod} < ( $self->last_mod || Time::HiRes::time + 1); # if undef was returned, we just do it
        $self->{last_mod} = Time::HiRes::time;
warn "syncin last_mod".$self->{last_mod};
    } 
}



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

x=pod


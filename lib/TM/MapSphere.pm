package TM::MapSphere;

use strict;
use warnings;

use TM;
use TM::PSI;

use base qw(Exporter);
use Data::Dumper;

=pod

=head1 NAME

TM::MapSphere - Interface for a TM Repository

=head1 SYNOPSIS

    use TM::MapSphere;
    # yet empty repository
    my $ms = new TM::MapSphere;

    # mount something at /
    $ms->mount ('/' => new TM);
    # mount something else
    $ms->mount ('/abc/' => new TM);    

    # any subclass of TM can be here
    $ms->mount ('/def/' => new TM::Materialized::AsTMa);

    # check a mount point
    print "damn" if $ms->is_mounted ('/xxx/');

    # do some sync stuff on any resource-connected map
    $ms->is_mounted ('/def/')->sync_in;

    # get rid of some mount point
    $ms->umount ('/abc/');

    # map spheres behave as maps!
    # find all child maps
    @maps = $ms->instances  (\ TM::PSI->TOPICMAP);

    print $ms->mids ('def');  # this topic exists and should be a map

=head1 DESCRIPTION

This package serves as a container to hold topic maps (and other objects), all organized as a topic
map itself. Obviously, this provides a hierarchical topic map database. On the top level, addressed
as C</> is the root map. The child maps have addresses such as C</abc/> and
C</internet/web/browsers>.

The individual maps can be of different provenance. Any subclass of L<TM> will do, consequently
also any subclass of L<TM::Resource>. This implies that this database can be heterogenic, in that
different maps can be stored differently, or can even be kept remotely. And it implies that you can
decide whether the whole repository is ephemeral (using a memory-only map) or whether it is
persistent by using a subclass of L<TM::Resource>.

=head2 Tau Objects

Map spheres can only store I<Tau objects>. At the moment, these are only maps. See L<TM::PSI> for
predefined things.

=head2 Namespace

To address a particular object in a repository we follow a convention similar to file system paths:
Every object has a path like

  /something/complete/else/

Note, that all paths for maps should start and end with a C</>, so that maps can be seen as
I<directories>. All other objects (constraints, queries, ...) are represented as topics.

The namespace cannot have I<holes>, so that the following will result in an error:

   $ms->mount ('/something/', ...);
   $ms->mount ('/something/completely/else/', ....);  # we will die here

=head2 Map Meta Data

Since a map sphere behaves like a map, any meta data about child maps should be modelled according
to the TM paradigm.

=head1 INTERFACE

This interface allows to I<mount> and I<unmount> other maps into another.

The idea is that a map can contain other maps, simply by having topics which stand for these child
maps. In that sense, a map is always a tree of maps (I<Map Sphere>). These trees are not necessarily
static. At any point, a new map can be hooked in, or removed. This process is quite similar to
I<mounting> devices into a UNIX file system, hence the naming.

=head2 Constructor

The constructor does not expect any parameters.

=cut

sub new {
    my $class = shift;
    my %opts  = @_;

    return bless { %opts }, $class;
}

#sub DESTROY {
#}

=pod

=head2 Methods

=over

=item B<mount>

I<$ms>->mount (I<$path> => I<$tm2>)

This mounts a map C<$tm2> into the map sphere at the given path. Only maps can be mounted,
everything else results in an error. If the mount point is already taken, this is an error too.

A topic of type C<topicmap> (see L<TM::PSI>) is created in the map above the mount point. The base
URI of the map is used as subject address. If the mounted map has a resource URL that is used as
subject indicator.


=cut

sub _find_longest_match {
    my $p = shift;
    my @ps = sort { length($b) <=> length($a) } @_;

#warn "_find_longest_match  $p ". Dumper \@ps;
    foreach (@ps) {
        return $_ if ($p =~ /^$_/);
    }
    return undef;
}


sub mount {
    my $self = shift;
    my $path = shift;
    my $obj  = shift;

    $main::log->logdie (scalar __PACKAGE__ .": can only mount map objects")         unless (ref ($obj) && $obj->isa ('TM'));
    $main::log->logdie (scalar __PACKAGE__ .": mount point '$path' already taken")  if     exists $self->{mounttab}->{$path};

#warn "trying to mount $path";

    unless ($path eq '/') {
	my $p = _find_longest_match ($path, keys %{$self->{mounttab}});
#warn "found max path $p for new path $path";
	(my $id = $path) =~ s/^$p([\w\.\-\_]+)\/$/$1/
	                          or $main::log->logdie (scalar __PACKAGE__ .": mount point for '$path' does not yet exist");
#warn "id now $id";
	{ 
#warn "finding map for $p";
	    my $map = $self->{mounttab}->{$p}                                            # find map above the mount point
                                  or $main::log->logdie (scalar __PACKAGE__ .": no map above this mount point");
	    my $mid = $map->internalize ($id);
#warn "adding baseuri as address ".$obj->baseuri;
	    $map->internalize ($mid =>   $obj->baseuri);
	    $map->internalize ($mid => \ $obj->url)     if $obj->isa ('TM::Resource');
	    $map->assert (Assertion->new (type => 'isa',            roles => [ 'class', 'instance' ], players => [ 'topicmap',                   $mid ]));
	    $map->assert (Assertion->new (type => 'implementation', roles => [ 'value', 'thing' ],    players => [ new TM::Literal (ref ($obj)), $mid ]));
	}
    }
    $self->{mounttab}->{$path} = $obj;                                                   # link it into our mounttab
}

=pod

=item B<umount>

I<$ms>->umount (I<$path>)

This unmounts a map from the object map. Obviously, the path must point to an existing topic. All maps
beyond this mount point are removed.

=cut

sub umount {
    my $self = shift;
    my $path = shift;

    my $mounttab = $self->{mounttab};
    map { delete $mounttab->{$_} }                                                      # get rid of all mount entries which have this a prefix
         grep ($_ =~ /^$path/, 
	       keys %$mounttab);

    unless ($path eq '/') {                                                             # this is the only case where there is no parent
	my $p = _find_longest_match ($path, keys %$mounttab);
#warn "found max path $p for new path $path";
	(my $id = $path) =~ s/^$p([\w\.\-\_]+)\/$/$1/
	                          or $main::log->logdie (scalar __PACKAGE__ .": mount point for '$path' does not exist");
#warn "id now $id";
	my $map = $mounttab->{$p}                                                       # find map above the mount point
	                          or $main::log->logdie (scalar __PACKAGE__ .": no map above this mount point");
	$map->retract (map { $_->[TM->LID] } $map->match (TM->FORALL, 
							  iplayer => $map->mids ($id)));# remove all involvements
	$map->externalize ($map->mids ($id));                                           # remove the midlet and return it
    }
}

=pod

=item B<is_mounted>

I<$tm> = I<$ms>->is_mounted (I<$path>)

Simply returns a map on that given mount point. C<undef> if there is none.

=cut

sub is_mounted {
    my $self = shift;
    my $path = shift;
    return $self->{mounttab}->{$path};
}

=pod

=item B<sync_in>

I<$ms>->sync_in (I<$path>)

A whole subtree of the repository can be I<sync'ed in>, i.e. synchronized with contents in an
associated resource. If this method is triggered with a particular path, then the map there will be
(a) synced in, (b) queried for sub-maps and (c) these sub-maps will be instantiated.  Recursively,
these submaps will be sync'ed in, etc. All these sub maps will be mounted under this branch of the
tree.

When a map is instantiated, its implementation package will be extracted from the parent map using a
C<implementation> characteristic. The resource URL will be determined from one of the subject
indicators, the base URI will be determined from the subject address of the map topic. If any of
these are missing, this particular sub-map is ignored.

B<Example>: Let us assume that a map has a C<baseuri> C<http://whatever/> and a resource URL
C<http://example.org/here.xtm>. It is a materialized map using the XTM driver. If this map is
mounted into a root map under C</foo/>, then the entry will take the form (using AsTMa= 2.0 as
notation):

   foo isa topicmap
   ~ http://example.org/here.xtm
   = http://whatever/
   implementation: TM::Materialized::XTM
   
=cut

sub sync_in {
    my $self = shift;
    my $path = shift;

    my $map = $self->{mounttab}->{$path} or
                        $main::log->logdie (scalar __PACKAGE__ .": mount point '$path' does not exist");
    _do_sync_recursive ($self, $path, $map);

sub _do_sync_recursive {
    my $ms     = shift;
    my $path   = shift;
    my $parent = shift;

    $parent->sync_in;

    foreach my $m ( $parent->instances ($parent->mids (\ TM::PSI->TOPICMAP)) ) {
        (my $id = $m) =~ s|.+/(.+)|$1|;                                                  # throw away the baseuri stuff
#warn "id $id";
	my $mid = $parent->midlet ($m);                                                  # get the topic itself
#warn Dumper $mid;
	my ($url)            = @{$mid->[TM->INDICATORS]}                        or next; # if there is no subject indicator, we could not load it anyway
	my ($baseuri)        =   $mid->[TM->ADDRESS]                            or next; # if there is no subject address, we could not load it anyway
	my ($implementation) = map { $_->[ TM->PLAYERS ]->[1]->[0] }
                                  $parent->match (TM->FORALL, type    => $parent->mids ('implementation'),
						              iplayer => $m )
                                                                                or next;
	my $child;
#warn "implementation $implementation";
	eval {
	    $child = $implementation->new (url => $url, baseuri => $baseuri );
	}; $main::log->logdie (scalar __PACKAGE__ .": cannot instantiate '$implementation' (maybe 'use' it?) for URL '$url' ($@)") if $@;

	$ms->mount ($path . "$id/" => $child)                                            # finally mount this thing into the current
	    unless $ms->is_mounted ($path . "$id/");                                     # unless there is already something there
#warn "-------mounted $path  $id/";
	_do_sync_recursive ($ms, $path . "$id/", $child);                                # go down recursively
    }
}
}

=pod

=item B<sync_out>

I<$ms>->sync_out (I<$path>)

@@@ TBW @@@

=cut

sub sync_out {
    die "not implemented yet";
}

=pod

=cut

# everything else is routed to the underlying map
# did I mention that OO programming sucks big time?

use vars qw($AUTOLOAD);
sub AUTOLOAD {
    my($method) = $AUTOLOAD =~ m/([^:]+)$/;
    my $self = shift;

#warn "AUTOLOAD forwarding '$method' to map object";
    no strict 'refs';

    return if $method eq 'DESTROY';
    my $map = $self->{mounttab}->{'/'} or die "mount something to / first";
    return $map->$method (@_);
#
#    *$AUTOLOAD = sub { $self->{map}->$method(@_) };
#    goto &$AUTOLOAD;
}

=pod


=back

=head1 AUTHOR

Robert Barta, E<lt>drrho@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 200[4-6] by Robert Barta

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself, either Perl version 5.8.4 or, at your option, any later version of Perl 5 you may have
available.

=cut

our $VERSION  = 0.03;
our $REVISION = '$Id: MapSphere.pm,v 1.21 2006/09/28 08:26:02 rho Exp $';

1;

__END__

 =pod

This is an abstract class. Still, applications can use the constructor
to let the system decide which implementation to use:

    # this creates a local store
    my $ms = new TM::MapSphere (BaseURL => 'file:/tmp/');

    # retrieve a map from there
    my $tm = $ms->tao ('/rumsti/');

    # alternatively using a TM server
    my $ms = new TM::MapSphere (BaseURL => 'tm://host/');
    my $tm = $ms->tao ('/rumsti/');



This is an B<abstract> class which defines the functionality an
application can get from a topic map repository. See
L<TM::MapSphere::MLDBM>, L<TM::MapSphere::Memory>,
L<TM::MapSphere::Dispatch> and L<TM::MapSphere::Client> (that comes
with the TM::Server package) for implementations of this interface.



@@@@ TODO: source -> text/x-astma, xml/x-xtm

 =xxxhead1 INTERFACE

  =xhead2 Constructor

The constructor (obviously) creates a new instance. Optionally you
can pass in a hash with the following fields:

 =over

 =item B<AutoList> (default: 0)

If an object is requested from a location and there is nothing,
the map sphere will try to list all objects on the next level beyond
the path provided (Similar to AutoIndexing in Apache).

That table of contents is delivered as a topic map. Use the constant
C<ONTOLOGY> to find out about the ontology used therein.

 =item B<BaseURL> (default: current directory)

Depending on the protocol method used in this URL, the constructor
will select different implementations. Currently supported methods
are

 =over

 =item B<tm:> (example: C<tm://localhost:13131/>)

In this case the TM protocol is used to communicate with a (remote)
TM server. See L<TM::Server>. If that package is not installed, the
constructor will die.

 =item B<file:> (example C<file:/tmp/>)

This selects a store implementation on the local file system. The provided
directory is used to store map data and meta data. The directory must exist;
it will not be automagically created.

 =back

If no C<BaseURL> is passed in, a pure-memory solution is picked.

Examples:

    my $ms = new TM::MapSphere (BaseURL => 'file:/tmp/');
    # returns a TM::MapSphere::Local object

    my $ms = new TM::MapSphere (BaseURL => 'tm://host/');
    # returns a client stub

    my $ms = new TM::MapSphere;
    # get the memory version, no persistency across invocations

 =back

 =cut

sub xnew {
    my $class = shift;
    my %opts  = @_;



    if (!$opts{BaseURL}) {
	use TM::MapSphere::Memory;
	return new TM::MapSphere::Memory (AutoList => $opts{AutoList});
    } elsif ($opts{BaseURL} =~ /^tm:/) { # here we try to load the remote driver
	eval "use TM::MapSphere::Client;";
	die $@ if $@;
	return new TM::MapSphere::Client (AutoList => $opts{AutoList}, MapSphere => $opts{BaseURL});
    } elsif ($opts{BaseURL} =~ /^file:/) {
	use TM::MapSphere::MLDBM;
	return new TM::MapSphere::MLDBM (AutoList => $opts{AutoList}, FileBase => $opts{BaseURL});
    } else {
	$main::log->logdie (scalar __PACKAGE__ . ": unknown BaseURL method '$opts{BaseURL}'");
    }
}

 =pod

  xxxx=head2 Methods

 =over

 =item B<feature>

Retrieve or set a feature.

 =cut

sub feature {
    my $self = shift;
    my $f    = shift;
    my $v    = shift;

    return unless grep ($_ eq $f, qw(AutoList));
    return $v ? $self->{$f} = $v : $self->{$f};
}

 =pod

 =item B<meta>

I<$map_meta> = I<$ms>->meta (I<$location>)

I<$ms>->meta (I<$location>, I<$map_meta>)

This method get/sets the meta information. Particular implementations
of this method might have special restrictions.

The meta information associated with the object with the provided
location will be returned.

The meta data of a particular object can be set to a new value by specifying not only a location but also a new hash
reference value. All fields will be copied verbatim; there are no restrictions except that you may collide with the
intrinsic field mentioned in L</Meta Data>. Note, that you are always adding/changing fields, not overwriting them.

TODO { sync_in => 1 } triggers syncinout when it was not before


TODO { source => { ....} }

 =item B<sync_in>, B<sync_out>

I<$ms>-sync_in  (I<$location>)

I<$ms>-sync_out (I<$location>)

Given an object location, a synchronisation of the addressed object is initiated. It may make an ugly noise if there is
no map at that location.

 =cut

sub sync_in {
    $main::log->logdie (scalar __PACKAGE__ . ": abstract method");
}

sub sync_out {
    $main::log->logdie (scalar __PACKAGE__ . ": abstract method");
}

 =pod

 =item B<locations>

I<@strings> = I<$ms>->locations

This method returns a hash containing the locations at which objects
are stored currently. The keys of the hash holds the paths (starting
with C</>) and the values contain the type of the object (if known,
otherwise C<undef>).

 =cut

sub locations {
    $main::log->logdie (scalar __PACKAGE__ . ": abstract method");
}

 =pod

 =item B<toc>

I<$tm> = I<$ms>->toc (I<$prefix>)

This map returns a map containing a I<table of contents> of the part
of the map sphere prefixed with C<$prefix>.

Example:

  my $toc = $ms->toc ('/internet/web/');

 =cut

# sub toc {
#     my $self   = shift;
#     my $prefix = shift || '/';

#     my %locs = $self->locations;                        # get a table of contents, deep
# #warn "in toc ".Dumper \%locs;
#     my $toc   = new TM::Materialized::AsTMa (baseuri => 'tm:'.$prefix,
# 					     inline  => ONTOLOGY);
#     $toc->sync_in;
#                                                         # this is what we have to build, a table of contents (one level deep), as map

#     while (my ($l, $o) = each %locs) {
# 	next unless $l =~ /^$prefix/;                   # disregard anything not in my prefix
# 	$l =~ s/^$prefix//;                             # strip away prefix something/ or some/../thing/
# 	$l =~ s/\/$//;                                  # strip away trailing slash
# 	my @l = split (m|/|, $l);                       # see what path we actually have
# 	$toc->assert_maplets (new Maplet (type    => 'isa',
# 					  roles   => [ 'class',                                  'instance' ],
# 					  players => [ (@l == 1 ? 'tau-map' : 'tau-collection'), $l[0]   ]));
# 	                                                # if it is only one entry long, then there is an object
#                                                         # if something is buried deeper in the hierarchy, then a collection
#     }
#     return $toc;
# }

 =pod

 =item B<tao>

I<$o> = I<$ms>->tao (I<$location>)

I<$ms>->tao (I<$location>, I<$o>, [ I<$meta_hash_ref> ])

This accessor methods adds and/or retrieves an object with a given
path location. In the writing case an additional hash reference can be
passed in. It carries meta data which will be copied as described for
the method C<meta>.

If there is no object at the location provided the method will throw
an exception, unless...unless the C<AutoList> has been configured for
that map sphere. In that case a listing of the objects is produced with
the method C<toc>.

 =cut

sub tao {
    $main::log->logdie (scalar __PACKAGE__ . ": abstract method");
}

 =pod

 =item B<untao>

I<$ms>->untao (I<$location>)

This method deletes the object under this address. It is silently
ignored if there is no such object.

 =cut

sub untao {
    $main::log->logdie (scalar __PACKAGE__ . ": abstract method");
}

 =pod

 =item B<path>

print I<$ms>->path (I<$tau_path_expression>)

This method retrieves values using Tau path expressions. Examples
of these are

@@@@

  /my/customers//person[rd[:friendly] = 'yes']    # return the topics of friendly customers
  /my/network//servers<bn, rd[:status]>           # return name and status of my servers

TODO: ????

The method will die if it cannot handle the path expression.

TODO: @@@ return values are lists of tuples!

TODO: read/write, more description where?

TODO: exception when not found??

 =cut

sub path {
    my $self = shift;
    my $path = shift;
##    my $val  = shift;

#     if ($path =~ m|(.+/)(/.*)|) {                      # have a path expression (together with a base)
# 	my ($tmpath, $base_pe) = ($1, $2);
# 	my $pe;
# 	if ($base_pe =~ m|/\*(.*)|) {                 # something like /internet/ldap//*/....
# 	    $pe = $1;
# 	} elsif ($base_pe =~ m|//(\w+)(.*)|) {        # something like /internet//server/.... ( equiv //* [: server]...)
# 	    $pe = ("[: $1] $2");
# 	} else {	
# 	    $main::log->logdie (scalar __PACKAGE__ . ": invalid path expression: '$path'");
# 	}
# 	my $tm = $self->tao ($tmpath);
# 	use TM::AsTMa::Path;
# 	my $ap = new TM::AsTMa::Path ($pe);
# 	my @th = $tm->path ($ap, $val);
# 	return \@th;

    if ($path =~ m|(.+/)(\w+$)|) {               # here we are checking paths like /internet/ldap, this simply asks for the toplet
	my ($tmpath, $tid) = ($1, $2);
 #warn "match in mapsphere for $tid in $tmpath";
 	my $tm = $self->tao ($tmpath);
 #warn "map is ".Dumper $tm;
 	my ($midlet) = $tm->midlet ($tm->mids($tid));
 #warn "path found midlet ".$midlet;
 	return $midlet ? $midlet : $main::log->logdie (scalar __PACKAGE__ . ": empty result: '$path'");

    } else {
 	$main::log->logdie (scalar __PACKAGE__ . ": unhandled path expression: '$path'");
    }
}

 =pod

 =back


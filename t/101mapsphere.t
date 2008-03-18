package MyMapSphere2;

use TM::Materialized::MLDBM2;
use base qw(TM::Materialized::MLDBM2);
use Class::Trait qw(TM::MapSphere);

1;


#-- test suite

use strict;
use warnings;

# change 'tests => 1' to 'tests => last_test_to_print';
use Test::More qw(no_plan);

use Data::Dumper;
$Data::Dumper::Indent = 1;
use Time::HiRes;


my @tmp;
foreach (qw(0 1 2)) {
    use IO::File;
    use POSIX qw(tmpnam);
    do { $tmp[$_] = tmpnam() ;  } until IO::File->new ($tmp[$_], O_RDWR|O_CREAT|O_EXCL);
}

END { unlink (@tmp, '/tmp/xxx') || warn "cannot unlink tmp files '@tmp'"; }

#== TESTS =====================================================================

{
    # first basic MLDBM functionality 
    { # start a sphere and add a topic
	unlink '/tmp/xxx'; # just make sure it's gone
	my $tm = new MyMapSphere2 (file => '/tmp/xxx');
	$tm->internalize ('aaa' => 'http://AAA/');
	ok ($tm->mids ('aaa'),                            'found topic');
    }
    { # check whether it is still there
	my $tm = new MyMapSphere2 (file => '/tmp/xxx');
	ok ($tm->mids ('aaa'),                            'found topic again');
    }
    { # now also mount an unsync'ed map
	my $tm = new MyMapSphere2 (file => '/tmp/xxx');

	use TM::Materialized::AsTMa;
	$tm->mount ('/xxx/' => new TM::Materialized::AsTMa (inline => "ccc (ddd)\n\n"));

	ok ($tm->is_mounted ('/xxx/'),                    'found child mounted');
	ok ($tm->mids ('xxx'),                            'found child map topic');
	ok (eq_set ([ $tm->instances  ('topicmap') ],
		    [ 'tm://nirvana/xxx' ]),              'regained map instance');
    }    
    { # is the map still there?
	my $tm = new MyMapSphere2 (file => '/tmp/xxx');
	ok ($tm->is_mounted ('/xxx/'),                    'found child mounted, again');
	ok ($tm->mids ('xxx'),                            'found child map topic, again');

	my $mt = $tm->mounttab;

	my $child = $mt->{'/xxx/'};

	ok (!$child->mids ('ccc'),                        'child map not synced in');

	$child->sync_in;
	$tm->mounttab ($mt);

#        touch ($tm, '/xxx/');
# sub touch {
#     my $self = shift;
#     my $path = shift;
#     my $mt = $self->{mounttab};
#     my $ch = $mt->{$path};
#     $mt->{$path} = undef;
#     $mt->{$path} = $ch;
#     $self->{mounttab} = $mt;
# }

	ok ( $child->mids ('ccc'),                         'child map synced in');

    }
    { # check whether the map is mounted & contains it sync'ed content
	my $tm = new MyMapSphere2 (file => '/tmp/xxx');
#warn Dumper $tm;
	my $child = $tm->is_mounted ('/xxx/');
	ok ( $child->mids ('ccc'),                         'child map synced in, again');
    }
    { # now umount xxx again
	my $tm = new MyMapSphere2 (file => '/tmp/xxx');
	$tm->umount ('/xxx/');
	ok (!$tm->is_mounted ('/xxx/'),                    'found child unmounted immediatly');
    }
    { # and check it is still unmounted
	my $tm = new MyMapSphere2 (file => '/tmp/xxx');
	ok (!$tm->is_mounted ('/xxx/'),                    'found child unmounted after reload');
    }
}


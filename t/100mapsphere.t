#-- test suite

package main;

use Log::Log4perl;
Log::Log4perl->init("t/log.conf");
our $log = Log::Log4perl->get_logger("TM");

1;

use strict;
use warnings;

# change 'tests => 1' to 'tests => last_test_to_print';
use Test::More qw(no_plan);

use Data::Dumper;
use Time::HiRes;


##unlink </tmp/meta*>;

require_ok('TM::MapSphere');

#== TESTS =====================================================================

{
    my $ms = new TM::MapSphere;

    eval {
	$ms->mids ('thing');
    }; like ($@, qr/mount something/,                   'non-existing root');

    # now mount something on /
    $ms->mount ('/' => new TM);
    ok ($ms->mids ('thing'),                            'root map, thing');

    # use a broken map
    eval {
	$ms->mount ('/xxx/' => 42);
    }; like ($@, qr/can only mount/,                    'non-map mounting error');

    # use a take mount point
    eval {
	$ms->mount ('/' => new TM);
    }; like ($@, qr/taken/,                             'taken mount point error');

    eval {
	$ms->mount ('/xxx/yyy/' => new TM);
    };  like ($@, qr/does not yet exist/,               'missing mount point error');
}

{
    my $ms = new TM::MapSphere;

    use TM::Materialized::AsTMa;
    my $tm = new TM::Materialized::AsTMa (inline => 'xxx (yyy)
');
    $tm->sync_in;
    $ms->mount ('/' => $tm);
#warn Dumper $ms;
    ok ($ms->mids ('xxx'),                              'background map, xxx');
    ok ($ms->mids ('yyy'),                              'background map, yyy');
}

{
    my $ms = new TM::MapSphere; #-- use map as map sphere

#-- root first
    $ms->mount ('/' => new TM);

#-- one map
    use TM::Materialized::AsTMa;
    my $tm1 = new TM::Materialized::AsTMa (inline => 'xxx (yyy)
');
    $ms->mount ('/rumsti/' => $tm1);

    ok ($ms->mids ('rumsti'),                            'mounted map, midlet');

    is ($ms->midlet ('rumsti')->[TM->ADDRESS], 
	'tm://nirvana/',                                 'mounted midlet, address');
    ok (eq_array ($ms->midlet ('rumsti')->[TM->INDICATORS], 
		  [
		   'inline:xxx (yyy)
'		   
		   ]),                                   'mounted midlet, indicators');
    ok (eq_set ([ $ms->instances  ('tm://nirvana/topicmap') ],
		[ 'tm://nirvana/rumsti' ]),              'mounted midlet, found as map instance');
    ok (eq_set ([ $ms->instances  ($ms->mids (\ 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-topicmap')) ],
		[ 'tm://nirvana/rumsti' ]),              'mounted midlet, found as map instance');

#-- second map
    my $tm2 = new TM;
    $ms->mount ('/ramsti/' => $tm2);

    ok ($ms->is_mounted ('/ramsti/'),                     'one map mounted');

    ok (eq_set ([ $ms->instances  ('tm://nirvana/topicmap') ],
		[ 
		  'tm://nirvana/rumsti',
		  'tm://nirvana/ramsti'
		  ]),                                    'mounted midlets, found as map instances');

    $ms->umount ('/rumsti/');
    ok (eq_set ([ $ms->instances  ('tm://nirvana/topicmap') ],
		[ 'tm://nirvana/ramsti' ]),              'mounted midlet, found as map instance');
}

{
    my $ms = new TM::MapSphere;

    $ms->mount ('/'     => new TM);
    $ms->mount ('/xxx/' => new TM);
    eval {
	$ms->mount ('/xxx/yyy/zzz' => new TM);
    }; like ($@, qr/does not yet exist/,                 'deep: missing mount point error');

    $ms->mount ('/xxx/yyy/' => new TM);
    $ms->mount ('/xxx/yyy/zzz/' => new TM);

    foreach my $p (qw( / /xxx/ /xxx/yyy/ /xxx/yyy/zzz/)) {
	ok (ref ($ms->is_mounted ($p)) eq 'TM',  "mounting: mount point $p verified");
    }

    $ms->umount ('/xxx/yyy/');
    foreach my $p (qw( / /xxx/ )) {
	ok (ref ($ms->is_mounted ($p)) eq 'TM',  "unmounting: mount point $p verified");
    }
}


use IO::File;
use POSIX qw(tmpnam);

my ($tmp);
do { $tmp = tmpnam() ;  } until IO::File->new ($tmp, O_RDWR|O_CREAT|O_EXCL);
END { unlink ($tmp) ; unlink ($tmp.".lock") ; }

{
    { # part I: writing something into the top
	my $ms = new TM::MapSphere;
	
	use TM::Materialized::MLDBM;
	$ms->mount ('/'     => new TM::Materialized::MLDBM (file => $tmp));
	use TM::Materialized::AsTMa;
	$ms->mount ('/xxx/' => new TM::Materialized::AsTMa (inline => "aaa (bbb)\n\n"));
	
	$ms->is_mounted ('/')->sync_out;

    }
    { # part II: getting it out again
	my $ms = new TM::MapSphere;

        use TM::Materialized::MLDBM;
        $ms->mount ('/'     => new TM::Materialized::MLDBM (file => $tmp));
	$ms->is_mounted ('/')->sync_in;
#warn Dumper $ms->is_mounted ('/');

	ok (eq_set ([ $ms->instances  ('tm://nirvana/topicmap') ],
		    [ 'tm://nirvana/xxx' ]),              'regained map instance');

	ok (!$ms->is_mounted ('/xxx/'),                   'no recursive automount');
    }
    { # part III: let MapSphere do the sync
	my $ms = new TM::MapSphere;
	use TM::Materialized::MLDBM;
	$ms->mount   ('/'     => new TM::Materialized::MLDBM (file => $tmp));
	$ms->sync_in ('/');

	ok (eq_set ([ $ms->instances  ('tm://nirvana/topicmap') ],
		    [ 'tm://nirvana/xxx' ]),              'regained map instance');

	my $child = $ms->is_mounted ('/xxx/');
	ok ($child->mids ('aaa'),                         'child map, midlet');
	ok ($child->mids ('bbb'),                         'child map, midlet');
    }
}


__END__



{ # try to use automatic selection
   my $ms = new TM::MapSphere (BaseURL => 'file:/tmp/');
   is (ref($ms), 'TM::MapSphere::MLDBM', 'selected MLDBM version');
   is ($ms->{FILEBASE}, '/tmp/', 'correct filebase');

   eval {
       my $ms2 = new TM::MapSphere (BaseURL => 'tm://something/');
       is (ref ($ms2), 'TM::MapSphere::Client', 'selected Client version');
   }; if ($@) {
       like ($@, qr/Can\'t locate/, 'exception ok, client driver not found');
   }

   $ms = new TM::MapSphere; # nothing here => should be memory
   is (ref($ms), 'TM::MapSphere::Memory', 'selected memory version');

   eval {
       $ms = new TM::MapSphere (BaseURL => 'rumsti:xxx');
   }; like ($@, qr/unknown/, 'exception ok, driver not existing');
}

require_ok ('TM::MapSphere::MLDBM');
{
    my $ms = new TM::MapSphere::MLDBM (FileBase => '/tmp/');
    is (ref ($ms), 'TM::MapSphere::MLDBM', 'class ok');
    is ($ms->{FILEBASE}, '/tmp/', 'filebase ok');
    is ($ms->{METANAME}, 'meta',  'metaname ok');
}

require_ok ('TM::MapSphere::Memory');

require_ok ('TM::MapSphere::Dispatch');

{
    my $md = new TM::MapSphere::Dispatch ('/' => new TM::MapSphere (BaseURL => 'file:/tmp/'));
    ok ($md->isa ('TM::MapSphere::Dispatch') &&
	$md->isa ('TM::MapSphere'), 'dispatch: class ok');
    ok ($md->{mapspheres}->{'/'}->isa ('TM::MapSphere::MLDBM') &&
	keys %{$md->{mapspheres}} == 1, 'single sphere ok');
}

{ # test internal _find_longest_match
    my @ps = ('/', '/xxx/', '/yyy/xxx/', '/xxx/yyy/zzz/');
    is (TM::MapSphere::Dispatch::_find_longest_match ('/xxx/yyy/',         @ps), '/xxx/',         'longest match 1');
    is (TM::MapSphere::Dispatch::_find_longest_match ('/xxx/yyyz/',        @ps), '/xxx/',         'longest match 2');
    is (TM::MapSphere::Dispatch::_find_longest_match ('/xx/',              @ps), '/',             'longest match 3');
    is (TM::MapSphere::Dispatch::_find_longest_match ('/',                 @ps), '/',             'longest match 4');
    is (TM::MapSphere::Dispatch::_find_longest_match ('/xxx/yyy/zzz/aaa/', @ps), '/xxx/yyy/zzz/', 'longest match 5');
    eval {
	TM::MapSphere::Dispatch::_find_longest_match ('xxxx', @ps);
    }; like ($@, qr/no matching/, 'no path found');
}

{ # testing Mat::MapSpere a bit
    use TM::Materialized::MapSphere;
    eval {
	my $tm = new TM::Materialized::MapSphere (ms => []);
    }; ok ($@, 'faulty mapsphere');
    my $ms = new TM::MapSphere;
    eval {
	my $tm = new TM::Materialized::MapSphere (ms => $ms, url => 'rumsti');
    }; ok ($@, 'faulty URL');
    eval {
	my $tm = new TM::Materialized::MapSphere (ms => $ms, url => 'tm:/rumsti');
    }; ok ($@, 'faulty URL');

    my $tm = new TM::Materialized::MapSphere (ms => $ms, url => 'tm:/rumsti/');
    ok ($tm->isa ('TM'), 'proxy class');
}

{
    mkdir '/tmp/.ms1';
    mkdir '/tmp/.ms2';

    my $ms1 = new TM::MapSphere (BaseURL => 'file:/tmp/.ms1/');
    my $ms2 = new TM::MapSphere (BaseURL => 'file:/tmp/.ms2/');
    my $ms3 = new TM::MapSphere::Memory;
    my $md = new TM::MapSphere::Dispatch ('/'     => $ms1,
					  '/xxx/' => $ms2,
					  '/yyy/' => $ms3);
    use TM::Materialized::AsTMa;
    my $tm = new TM::Materialized::AsTMa (inline => 'xxx (yyy)
');
    $md->tao ('/rumsti/', $tm);

    ok ($ms1->tao ('/rumsti/'),    'map /rumsti/ must be in /');
    eval {
       $ms2->tao ('/rumsti/');
    }; like ($@, qr/unknown map/,  'map /rumsti/ not in /xxx/');

#    $md->clear;
#    ok (!$ms1->meta ('/rumsti/'),  'no rumsti in meta');
#    ok (!$ms2->meta ('/rumsti/'),  'no rumsti in meta');

    $md->tao ('/xxx/ramsti/', $tm);
    ok ($ms2->tao ('/ramsti/'), '/xxx/ramsti/: found /ramsti/ again');

    $md->tao ('/yyy/romsti/', $tm);
    ok ($ms3->tao ('/romsti/'), '/yyy/ramsti/: found /romsti/ again');


    $md->untao ('/yyy/romsti/');
    eval {
	$md->tao ('/yyy/romsti/');
    }; like ($@, qr/unknown/, 'romsti is gone');

    $md->untao ('/xxx/ramsti/');
    ok (-r '/tmp/.ms2/%2Framsti%2F', 'ramsti file itself here');
    eval {
	$md->tao ('/xxx/ramsti/');
    }; like ($@, qr/unknown/, 'ramsti is gone');

    unlink </tmp/.ms1/*>;
    unlink </tmp/.ms2/*>;
    rmdir  '/tmp/.ms1';
    rmdir  '/tmp/.ms2';
}


LOOP:
    foreach my $ms (
                    new TM::MapSphere::Memory, 
#		    new TM::MapSphere::MLDBM    (FileBase => 'file:/tmp/'),
#		    new TM::MapSphere::Dispatch ('/'      => new TM::MapSphere::Memory)
                    ) {
	my $c = ref ($ms);
	ok ($ms->isa ('TM::MapSphere'), "$c: class ok");
	
	{ # store a normal map there
	    use TM::Materialized::AsTMa;
	    my $tm = new TM::Materialized::AsTMa (inline => 'xxx (yyy)
');
	    my $t1 = Time::HiRes::time;
	    $ms->tao ('/rumsti/', $tm);
	    my $t2 = Time::HiRes::time;
	    
	    my $mm = $ms->meta ('/rumsti/');

##warn Dumper $mm;
	    ok ($t1 <= $mm->{created}  && $mm->{created}  <= $t2,               "$c: creation date ok");
	    ok ($t1 <= $mm->{modified} && $mm->{modified} <= $t2,               "$c: modified date ok");
	    
	    my %l = $ms->locations;
	    ok (eq_set ([keys   %l], [ '/rumsti/' ]),                           "$c: locations");
	    ok (eq_set ([values %l], [ 'TM::Materialized::AsTMa' ]) ||
		eq_set ([values %l], [ undef ]),                                "$c: locations");
	    
	    my $tm2 = $ms->tao ('/rumsti/');
	    ok ($tm2->isa ('TM::Materialized::AsTMa'),                          "$c: found /rumsti/ again");
	    ok ($tm2->{url} eq 'inline:xxx (yyy)
',                                                                              "$c: maps stored and retrieved");

	    eval {
		my $t = $ms->path ('/rumsti/yyy');                               # map not synced in, so it can't have it
		ok (0,                                                          "$c: toplet should not be there");
	    }; like ($@, qr/empty/,                                             "$c: got no toplet");
	    exit;
	    {
		# I probably should not refer to internals, here, but for testing it's ok?
		my $store = $ms->tao ('/rumsti/');
		ok ($store,                                                         "$c: store defined");
		my $nr = scalar keys %{$store->{assertions}};
		
		$ms->sync_in ('/rumsti/');                                          # this should propagate to the store
		
		$store = $ms->tao ('/rumsti/');
		ok ($nr < scalar keys %{$store->{assertions}},                      "$c: mapsphere sync increased assertions");
	    }

	    my $t = $ms->path ('/rumsti/yyy');                                  # now it should work
#warn "toplet rturned ".Dumper $t;
	    ok (ref ($t) eq 'Toplet',                                           "$c: got toplet");

	    # adding a second one, with meta data
	    $ms->tao ('/rumsti/ramsti/', $tm, { xxx => 'yyy' });

	    my %l2 = $ms->locations;
	    ok (eq_set ([keys %l2], [ '/rumsti/', '/rumsti/ramsti/' ]),         "$c: locations 2");

	    $mm = $ms->meta ('/rumsti/ramsti/');
	    ok ($mm->{xxx} eq 'yyy',                                            "$c: additional meta data (via tao)");
	    $ms->meta ('/rumsti/ramsti/', { aaa => 'bbb' });
	    $mm = $ms->meta ('/rumsti/ramsti/');
	    ok ($mm->{aaa} eq 'bbb',                                            "$c: additional meta data (via meta)");

	    $ms->untao ('/rumsti/');
	    ok ($ms->tao ('/rumsti/ramsti/'),                                   "$c: existing map found");
	    eval {
		$ms->tao ('/rumsti/');
	    }; like ($@, qr/unknown/,                                           "$c: deleted map not found");

	    {
		use TM::Materialized::AsTMa;
		my $tm    = new TM::Materialized::AsTMa (inline => 'aaa (bbb)
');
		$ms->tao  ('/ramsti/', $tm);
		$ms->meta ('/ramsti/', { sync_in => 1 });
		my $t = $ms->path ('/ramsti/aaa');
#warn "toplet returned ".Dumper $t;
		ok ($t->[TM->LID] eq 'tm://nirvana/aaa' &&
		    eq_array ( [ map { ref ($_) } @{$t->[TM->CHARS]} ],
			       [  ]),                                           "$c: ramsti path fetch");

	    }
	}

#	    next LOOP;

#    { # TODO check non-map objects
#    }

	{ # check T::M::MapSphere
	    use TM::Materialized::AsTMa;
	    my $tm    = new TM::Materialized::AsTMa (inline => 'aaa (bbb)
');
	    $ms->tao ('/remsti/', $tm);

	    use TM::Materialized::MapSphere;
	    my $vtm   = new TM::Materialized::MapSphere (ms  => $ms,
							 url => 'tm:/remsti/');

	    { # bad boy again, looking at internals
		my $store = $ms->tao ('/remsti/');
		my $nr    = scalar keys %{$store->{assertions}};
		$vtm->sync_in;
		ok ($vtm->is_a ('aaa', 'bbb'), 'vtm synced in');

#warn "vtm ".Dumper $vtm;
		$store = $ms->tao ('/remsti/');
		ok ($nr < scalar keys %{$store->{assertions}}, 'TM::Materialized::MapSphere: sync increased assertions remotely');
	    }

	    ok (!$vtm->is_a ('vvv', 'uuu'), 'local change, not');
	    my $tm3 = $ms->tao ('/remsti/');
	    ok (!$tm3->is_a ('vvv', 'uuu'), 'remote change, not');

	    $vtm->assert_maplets ([ undef, undef, 'isa', TM->KIND_ASSOC, [ 'class', 'instance' ], [ 'uuu', 'vvv' ]]);
#warn "vtm after uuu is ".Dumper $vtm;

	    ok ( $vtm->is_a ('vvv', 'uuu'), 'local change');
	    ok (!$tm3->is_a ('vvv', 'uuu') ||                            # the reason for this bizarre is that TM::Materialized::Memory uses SHARED memory,
                 $tm3->is_a ('vvv', 'uuu'), 'remote change, or not');    # so changes will be immediate, this may change in the future
	    $vtm->sync_out;
	    $tm3 = $ms->tao ('/remsti/');                                # have to refetch it
	    ok ( $tm3->is_a ('vvv', 'uuu'), 'remote change');
	}

	{ # testing clear
	    my %l = $ms->locations;
	    foreach (keys %l) {
		$ms->untao ($_);
	    }
	    eval {
		$ms->tao ('/rumsti/');
	    }; like ($@, qr/unknown map/, 'exception on non-existent map');
	}

    }

unlink </tmp/meta*>;

LOOP2:
    foreach my $ms (
		    new TM::MapSphere::Memory   (AutoList => 1),
		    new TM::MapSphere::MLDBM    (AutoList => 1, FileBase => 'file:/tmp/'),
		    new TM::MapSphere::Dispatch (AutoList => 1, '/'      => new TM::MapSphere::Memory),
		    ) {
	ok ($ms->{'AutoList'}, ref ($ms).': autolisting sticks');

	use TM::Materialized::AsTMa;
	my $tm    = new TM::Materialized::AsTMa (inline => 'aaa (bbb)
');
	$ms->tao ('/rumsti/',               $tm);
	$ms->tao ('/ramsti/rimsti/',        $tm);
	$ms->tao ('/ramsti/remsti/',        $tm);
	$ms->tao ('/ramsti/remsti/romsti/', $tm);

	my $toc = $ms->tao ('/ramsti/');                 # building table of contents

	ok ($toc->isa ('TM'), 'got map');
	ok ($toc->mids ('rimsti'), 'got entry 1');
	ok ($toc->mids ('remsti'), 'got entry 2');

	is (scalar $toc->maplets (type => 'isa', irole => 'instance', iplayer => 'rimsti'), 1, 'found map entry');
	is (scalar $toc->maplets (type => 'isa', irole => 'instance', iplayer => 'remsti'), 2, 'found map+collection entry');
    }

__END__





{ # testing Materialized mapsphere map
    my $ms = new TM::MapSphere ();

    use TM::Materialized::AsTMa;
    my $a  = new TM::Materialized::AsTMa (inline => 'xxx (yyy)
');
    $ms->tao (location => '/rumsti/', object => $a);                                    # NOTE: here we did not yet sync

warn "============= MS ==========".Dumper $ms;


}

__END__


package Rumsti;

use TM;
use base qw(TM);
use Class::Trait ('TM::Synchronizable', 'TM::ResourceAble' => { exclude => 'mtime' } );


our $sync_in_called = 0;
our $sync_out_called = 0;

# I use this to reset the counters
sub reset {
  $sync_in_called = 0;
  $sync_out_called = 0;
}

sub synced {
  return [ $sync_in_called, $sync_out_called ];
}

sub source_in {
  my $self = shift;

#warn "rumsti source  $self";
  $sync_in_called++;
}

sub source_out {
##warn "rumsti source out";
  $sync_out_called++;
}

sub mtime {
#warn "Rumsti mtime";
    return time + 1; # fake that we always have something new
}


1;

package Ramsti;

use TM::Tau::Filter;
use base qw(TM::Tau::Filter);

our $sync_in_called = 0;
our $sync_out_called = 0;

# I use this to reset the counters
sub reset {
  $sync_in_called = 0;
  $sync_out_called = 0;
}

sub synced {
    return [ $sync_in_called, $sync_out_called ];
}

sub source_out {
    $sync_out_called++;
}

1;

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

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}

# create tmp file
my $tmp;
use IO::File;
use POSIX qw(tmpnam);
do { $tmp = tmpnam().".atm" ;  } until IO::File->new ($tmp, O_RDWR|O_CREAT|O_EXCL);

##warn "tmp is $tmp";

my $fh = IO::File->new ("> $tmp") || die "so what?";
print $fh "
aaa (bbb)
bn: AAA

(ccc)
ddd: eee
fff: ggg
";
$fh->close;

END { unlink ($tmp) || die "cannot unlink '$tmp' file, but I am finished anyway"; }

#== TESTS =====================================================================

require_ok ('TM::Tau');

#{
#    my $tm = new TM::Tau ('(null: * null: + null:) > (null:) > (null:)', sync_in => 0, sync_out => 0);
#}

{ # basic tests
    my $tm = new TM::Tau ('null: > null:', sync_in => 0, sync_out => 0);
#warn "============> ". ref ($tm->left) . " <-- left -- " . ref ($tm);
#    warn "this is it ".Dumper $tm;

    ok ($tm->isa ('TM::Tau'),              'class');
    ok ($tm->isa ('TM::Tau::Filter'),      'class');
    ok ($tm->isa ('TM'),                   'class');
    ok ($tm->does ('TM::Serializable::AsTMa'), 'default: filter trait');

    ok ($tm->left->isa ('TM::Materialized::Null'),     'left null becomes a memory map');
}

{ # default
    my $tm = new TM::Tau;
    ok ($tm->isa ('TM::Tau::Filter'),              'default: top level');
    ok ($tm->left->isa ('TM::Materialized::Null'), 'default: left');

    ok ($tm->does ('TM::Serializable::AsTMa'), 'default: filter trait');

#    warn "this is it ".Dumper $tm;
}

eval { # errors
    my $tm = new TM::Tau ({});
}; like ($@, qr/undefined scheme/, _chomp ($@));

eval { # error
  my $tm = new TM::Tau ('rumsti:');
}; like ($@, qr/undefined scheme/, _chomp $@);

use TM::Tau;
{ # testing events
    $TM::Tau::sources{'rumsti:'} = 'Rumsti';
    $TM::Tau::filters{'rumsti:'} = 'Ramsti';

    my $tests = {
 	'01 rumsti: > rumsti:'      => { uc => [ 1, 0 ],
					 ac => [ 0, 0 ], debug => 0,
					 ud => [ 1, 0 ],
					 ad => [ 0, 1 ] },
	
 	'02 rumsti:'                => { uc => [ 1, 0 ],
					 ac => [ 0, 0 ],
					 ud => [ 1, 0 ],
					 ad => [ 0, 0 ] },

   	'03 rumsti: >'              => { uc => [ 1, 0 ],
					 ac => [ 0, 0 ],
					 ud => [ 1, 0 ],
					 ad => [ 0, 0 ] },

  	'04 > rumsti:'              => { uc => [ 0, 0 ],
					 ac => [ 0, 0 ],
					 ud => [ 0, 0 ],
					 ad => [ 0, 1 ] },

  	'05 > rumsti: <'            => { uc => [ 1, 0 ],
					 ac => [ 0, 0 ],
					 ud => [ 1, 0 ],
					 ad => [ 0, 0 ] },

   	'06 > rumsti: >'            => { uc => [ 1, 0 ],
					 ac => [ 0, 0 ],
					 ud => [ 1, 0 ],
					 ad => [ 0, 1 ] },

   	'07 < rumsti: <'            => { uc => [ 0, 0 ],
					 ac => [ 0, 0 ],
					 ud => [ 0, 0 ],
					 ad => [ 0, 0 ] },

  	'08 < rumsti: >'            => { uc => [ 0, 0 ],
					 ac => [ 0, 0 ],
					 ud => [ 0, 0 ],
					 ad => [ 0, 1 ] },

    };

    foreach my $t (sort { $a cmp $b } keys %$tests) {
	Rumsti::reset;
	Ramsti::reset;
#	  next unless $t =~ /02/;
	  (my $tau = $t) =~ s/\d+\s*//;
        {
	    my $tm = new TM::Tau ($tau);
#	    warn Dumper $tm;
#warn "============> ". ref ($tm->left) . " <-- left -- " . ref ($tm);
#warn "test $tau";
warn Dumper $tm if $tests->{$t}->{debug};      

#    warn "synced after create ".Dumper Rumsti::synced;
	    ok (eq_array ($tests->{$t}->{uc}, Rumsti::synced), "$tau : rumsti after creation");
	    ok (eq_array ($tests->{$t}->{ac}, Ramsti::synced), "$tau : ramsti after creation");
#warn "Ramsti after create ".Dumper Ramsti::synced;

            $tm->internalize ('remsti'); # do something with the map, so that the timestamp is modified

	}
#warn "Rumsti synced after destruct ".Dumper Rumsti::synced;
#warn "Ramsti after decon ".Dumper Ramsti::synced;
	ok (eq_array ($tests->{$t}->{ud}, Rumsti::synced), "$tau : rumsti after deconstruction");
	ok (eq_array ($tests->{$t}->{ad}, Ramsti::synced), "$tau : ramsti after deconstruction");
    }
}

__END__

#{ # canonicalization trivia
#    foreach my $s ('null:', '> null: <', '> null: >', '< null: >') { # 
#	ok (new TM::Tau ($s), "parsing $s");
#    }
#}

__END__


#-- synchronisation ----------------------------------

{ # tests with materialized
  my $tau;

  Rumsti::reset;
  $tau = '> rumsti: <';
  { # 
      my $tm = new TM::Tau ($tau);
#      warn Dumper $tm;
      ok ($tm->chain->isa ('Ramsti'),                  'ramsti');
      ok ($tm->chain->left->isa ('Rumsti'),            'rumsti');
  }
  is ($Rumsti::sync_in_called,  1,         $tau.': tried sync in once');
  is ($Ramsti::sync_out_called, 1,         $tau.': tried sync out once');

  Rumsti::reset;
  $tau = 'rumsti:';
  {
    my $tm = new TM::Tau ($tau);
#    warn Dumper $tm;
  }
  is ($Rumsti::sync_in_called,  1,         $tau.': tried sync in once');
  is ($Rumsti::sync_out_called, 0,         $tau.': tried sync out never');

  Rumsti::reset;
  $tau = 'rumsti: >';
  { # only reading
    my $tm = new TM::Tau ($tau);
  }
  is ($Rumsti::sync_in_called,  1,         $tau.': tried sync in once');
  is ($Rumsti::sync_out_called, 0,         $tau.': tried sync out never');


  Rumsti::reset;
  $tau = ' > rumsti:';
  { # only writing
    my $tm = new TM::Tau ($tau);
  }
  is ($Ramsti::sync_in_called,  0,         $tau.': tried sync in never');
  is ($Rumsti::sync_out_called, 1,         $tau.': tried sync out once');

}

{ # test to override driver module
  my $tm = new TM::Tau ('> ramsti: { Rumsti } > ');
  ok (ref ($tm->{map}->{map}) eq 'Rumsti',       'override: in special driver');

  { # test to override driver module
      eval {
	  my $tm = new TM::Tau ('> ramsti: { Rxxxumsti } > ');
      }; like ($@, qr/cannot instantiate/, _chomp $@);
  }
}

{ # tests with files, explicit sync
    my $tau = "< file:$tmp >";
    my $tm = new TM::Tau ($tau);
    ok ($tm->{map}->isa ('TM::Tau::Filter') &&
	$tm->{map}->{map}->isa ('TM::Materialized::AsTMa'), "$tau: filter structure");
    is ($tm->{map}->{map}->{last_mod}, undef, "$tau : no sync in");
    $tm->sync_in;
    ok ($tm->{map}->{map}->{last_mod},        "$tau : synced in");

    eval {
	$tm->sync_out; # AsTMa does not support this
    }; like ($@, qr/not implement/, _chomp $@);
}

{ # test with +
    foreach (1..4) {
	Rumsti::reset;
	{
	   my $tm = new TM::Tau (join (" + ", ('rumsti:') x $_). ' > ');
	}
	is ($Rumsti::sync_in_called,  $_,                                "merged ($_): sync in");
	is ($Rumsti::sync_out_called, 0,                                 "merged ($_): sync out");
    }
}

__END__

#%TM::Tau::STDIN  = (module => 'TM::Materialized::AsTMa',  url => 'io:stdin');
#%TM::Tau::STDOUT = (module => 'TM::Materialized::Memory', url => 'io:stdout');


{ # test mapsphere
    my $tm = new TM::Tau ("file:$tmp > tm:/test/");
    $tm->{map}->sync_out;

    like ($tm->{mapsphere}->{maps}->{'/test/'}->mids ('aaa'), qr/aaa$/, 'map established in mapsphere');
}

{ # test with +, one virtual
  $TM::Tau::schemes{'rumsti:'} = 'Rumsti';
  $TM::Tau::schemes{'ramsti:'} = 'Ramsti';
  Rumsti::reset;
  {
    my $tm = new TM::Tau ('ramsti: + ramsti: + rumsti: > ');
#    is (scalar ($tm->maplets), 1,                             'found maplets');
#    is ($tm->toplet ('xxx-1'), 1,                             'found toplets');
  }
  is ($Rumsti::sync_in_called,  1,                            'merged: sync in');
  is ($Rumsti::sync_out_called, 0,                            'merged: sync out');
}



__END__

__END__


$TM::Tau::schemes{'ramsti:'} = 'Ramsti';

{
    # this should not try to sync_in/out, otherwise an exception would be raised
    my $tm = new TM::Tau ('> ramsti: >');
    ok (1, 'no sync_in/out on virtual'); 
}


{ # testing (), + and *
    my $tm = new TM::Tau ('> null: + null: >');
    ok (ref ($tm->{transit})          eq 'TM::Materialized::Merge',  '+ parsed');
    ok (ref ($tm->{transit}->{left})  eq 'TM::Materialized::Memory', '+ parsed');
    ok (ref ($tm->{transit}->{right}) eq 'TM::Materialized::Memory', '+ parsed');

    $tm = new TM::Tau ('> null: * null: > ');
    ok (ref ($tm->{transit})          eq 'TM::Materialized::Filter', '* parsed');
    ok (ref ($tm->{transit}->{left})  eq 'TM::Materialized::Memory', '* parsed');
    ok (ref ($tm->{transit}->{right}) eq 'TM::Materialized::Memory', '* parsed');

    $tm = new TM::Tau ('> ( null: + null: ) * rumsti: >');
    ok (ref ($tm->{transit}) eq                  'TM::Materialized::Filter', '()*+ parsed');
##warn $tm->{transit}->{left};
    ok (ref ($tm->{transit}->{left})          eq 'TM::Materialized::Merge',  '()*+ parsed');
    ok (ref ($tm->{transit}->{left}->{left})  eq 'TM::Materialized::Memory', '* parsed');
    ok (ref ($tm->{transit}->{left}->{right}) eq 'TM::Materialized::Memory', '* parsed');
    ok (ref ($tm->{transit}->{right})         eq 'Rumsti', '()*+ parsed');

    $tm = new TM::Tau ('> ( null: * ramsti: ) + rumsti: >');
    ok (ref ($tm->{transit}) eq                  'TM::Virtual::Merge',       '+* parsed');
    ok (ref ($tm->{transit}->{left})          eq 'TM::Virtual::Filter',      '()+* parsed');
    ok (ref ($tm->{transit}->{left}->{left})  eq 'TM::Materialized::Memory', '* parsed');
    ok (ref ($tm->{transit}->{left}->{right}) eq 'Ramsti',                   '* parsed');
    ok (ref ($tm->{transit}->{right})         eq 'Rumsti',                   '()*+ parsed');
}




__END__

eval {
  require TM::Virtual::DNS;
  $TM::schemes{'dns:'} = 'TM::Virtual::DNS';
  my $tm = new TM (tau => 'dns:whatever');
  ok ($tm->toplet ('localhost'), 'dns: found localhost');
##print Dumper $localhost;
}; if ($@) {
  like ($@, qr/Can't locate/, "skipping DNS test ("._chomp($@).")");
}


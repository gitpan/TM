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

sub mtime {
#warn "rumsti mtime";
    return time + 1; # always a change
}

sub source_in {
    $sync_in_called++;
# warn "sync_in_called $sync_in_called";
}

sub source_out {
    $sync_out_called++;
# warn "sync_out_called $sync_out_called";
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

sub source_out {
    $sync_out_called++;
# warn "Ramsti sync_out_called $sync_out_called";
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
$Data::Dumper::Indent = 1;

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}

#== TESTS =====================================================================

use TM;
require_ok ('TM::Tau::Filter');

eval {
    my $f = new TM::Tau::Filter (left => 1);
}; like ($@, qr/must be an instance/, 'left must be a TM instance');

{ # structural
    my $tm = new TM;
    my $f  = new TM::Tau::Filter (left => $tm);

    ok ($f->isa ('TM::Tau::Filter'),            'class');
    ok ($f->isa ('TM'),                         'superclass');
    ok ($f->does ('TM::ResourceAble'),          'trait: resource');
    ok ($f->does ('TM::Synchronizable'),        'trait: sync');
    is ($f->url, 'null:', 'default url');

    is ($f->left, $tm,                          'left operand');
}

{ # short chain
    my $f = new Ramsti (left => new Rumsti (url => 'in:whatever'), url => 'out:whatever');
    $f->left->reset;
    $f->reset;
    $f->sync_in;
    is ($Rumsti::sync_in_called,  1,         'Rumsti: tried sync in once');
    $f->sync_in;
    is ($Rumsti::sync_in_called,  2,         'Rumsti: tried sync in twice');
    $f->sync_out;
    is ($Rumsti::sync_out_called, 0,         'Rumsti: tried sync out never');
    is ($Ramsti::sync_out_called, 1,         'Ramsti: tried sync out once');
}

{ # longer chain
    my $f = new Ramsti (left => new Ramsti (left => new Rumsti (url => 'in:'),
					    url  => 'what:ever'),
			url => 'what:ever');
    $f->left->left->reset;
    $f->left->reset;
    $f->reset;
    $f->sync_in;
    is ($Rumsti::sync_in_called,  1,         'Rumsti: tried sync in once');
    $f->sync_in;
    is ($Rumsti::sync_in_called,  2,         'Rumsti: tried sync in twice');
    $f->sync_out;
    is ($Rumsti::sync_out_called, 0,         'Rumsti: tried sync out never');
    is ($Ramsti::sync_out_called, 1,         'Ramsti: tried sync out once');
}

__END__


## TODO !!!!!!

{ # testing analysis filter

    use TM::Materialized::AsTMa;
    my $tm = new TM::Materialized::AsTMa (inline => "aaa (bbb)\n");

    use TM::Tau::Filter::Analyze;

    my $bu = 'tm:';
    my $f = new TM::Tau::Filter::Analyze (left => $tm, baseuri => $bu);
    $f->sync_in;

    my $stats = TM::Analysis::statistics ($tm);                   # has to be here, as this is the time after parsing AsTMa

    ok (eq_set ([ $f->instances ($f->mids ('metric')) ],
		[ map { $bu . $_ } keys %$stats ]),                    'got all metrics');

    foreach my $t ( $f->instances ($f->mids ('metric')) ) {
	my ($v) = map { $_->[0] }
                       map { TM::get_players ($f, $_, $f->mids ('value')) }
                 	   $f->match (TM->FORALL, type => $f->mids ('occurrence'), iplayer => $t);
	(my $k = $t) =~ s/^$bu//;
	is ($v, $stats->{$k}, 'metric values')
    }
}


__END__


## junk, can be thrown away at some stage....

{ # basic tests
    my $tm = new TM::Tau;
    ok ($tm->isa ('TM::Tau'),      'class');
    is ($tm->{tau}, '(null:) * -', 'default expansion');
    is ($tm->{tau}, $tm->tau,      'tau accessor');
#    warn "this is it ".Dumper $tm;
}

eval { # errors
    my $tm = new TM::Tau ({});
}; like ($@, qr/undefined scheme/, _chomp ($@));

eval { # error
  my $tm = new TM::Tau ('rumsti:');
}; like ($@, qr/undefined scheme/, _chomp $@);



{ # parsing
    my $tm;
    foreach ('null:', '> null: <', '> null: >', '< null: >') { # 
	ok (new TM::Tau ($_), "parsing $_");
    }
}

#-- testing +

{ # test with +, two materialized
  Rumsti::reset;
  {
    my $tm = new TM::Tau ('rumsti: + rumsti: + rumsti: > ');

#    is (scalar ($tm->maplets), 3,                                  'found maplets');
#    my @xxxs = $tm->toplet ('xxx-1', 'xxx-2', 'xxx-3');
#    is (@xxxs, 3,                                                  'found toplets');

  }
  is ($Rumsti::sync_in_called,  3,                                 'merged: sync in');
  is ($Rumsti::sync_out_called, 0,                                 'merged: sync out');
}

{ # test with +, one virtual
  $TM::Tau::Tau::schemes{'rumsti:'} = 'Rumsti';
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


{ # tests with files, explicit sync
    use TM::PSI;
    my $tm = new TM::Tau ("< file:$tmp >");
    is ($tm->{store}, undef, "> file:$tmp < : store is undefined");
    $tm->sync_in;
#    warn Dumper $tm;
    ok ((scalar keys %{$tm->{transit}->{store}->{si}}) > (scalar keys %TM::PSI::PSIs), 'files, explicit, added topics to PSIs');

    eval {
	$tm->sync_out; # AsTMa does not support this
    }; like ($@, qr/not implement/, _chomp $@);
}



{ # files, implicit sync in, no out
    my $tm = new TM::Tau ($tmp);
    ok ((scalar keys %{$tm->{transit}->{store}->{si}}) > (scalar keys %TM::PSI::PSIs), 'files, implicit, added topics to PSIs');
##warn Dumper $tm;
    ok (1, 'files, implicit, no out');         # if there is no sync_out, then there will be no exception
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



{ # testing events
    $TM::Tau::schemes{'rumsti:'} = 'Rumsti';
    $TM::Tau::schemes{'ramsti:'} = 'Ramsti';

    my $tests = {
	'rumsti:'                => { a => [ 1, 0 ],
				      b => [ 1, 0 ] },
	'rumsti: >'              => { a => [ 1, 0 ],
				      b => [ 1, 0 ] },
	'> rumsti:'              => { a => [ 0, 0 ],
				      b => [ 0, 1 ] },
	'> rumsti: >'            => { a => [ 0, 0 ],
				      b => [ 0, 0 ] },
	'< rumsti: >'            => { a => [ 0, 0 ],
				      b => [ 0, 1 ] },
	'> rumsti: <'            => { a => [ 1, 0 ],
				      b => [ 1, 1 ] },
	'rumsti: > rumsti:'      => { a => [ 1, 0 ],
				      b => [ 1, 1 ], debug => 0},
	'null: > rumsti: '       => { a => [ 0, 0 ],
				      b => [ 0, 1 ] },
	'rumsti:  > null:   '    => { a => [ 1, 0 ],
				      b => [ 1, 0 ] },
	};

    foreach my $t (keys %$tests) {
	Rumsti::reset;
	  {
	      my $tm = new TM::Tau ($t);

#warn "test $t";
warn Dumper $tm if $tests->{$t}->{debug};      

#    warn "synced after create ".Dumper Rumsti::synced;
	      ok (eq_array ($tests->{$t}->{a}, Rumsti::synced), "$t: after creation");
	  }
#    warn "synced after destruct ".Dumper Rumsti::synced;
	  ok (eq_array ($tests->{$t}->{b}, Rumsti::synced), "$t: after deconstruction");
      }
}

__END__

# USE CASE testing

{
    my $tm = new TM ('test
}

__END__

{ # checking classes
    my $tm = new TM;
    is (ref($tm->{transit}), 'TM::Materialized::Memory', 'tau expr class');

    use TM::Tau;
    my $tau = TM::Tau::parse ('null:');
    $tm = new TM ($tau);
    is ($tm->{transit}, $tau, 'manual tau override');
}



__END__



{ # load ATM, 
  use File::Temp qw/ tempfile /;
  my ($fh, $filename) = tempfile( );
  print $fh "

aaa

bbb

";
  close ($fh);

  use File::stat;
  my $st = stat ($filename) or die "No $filename: $!";
  my $creation_time = $st->mtime;

  {
    my $tm = new TM (tau => "file:$filename");
    # default behavior:
    #   open file at start up
    ok ($tm->toplet ('aaa') && $tm->toplet ('bbb'), 'toplets loaded');

    #   allow changes
    {
      use TM::Transaction;
      my $tx = new TM::Transaction (map => $tm);
      is (ref ($tx), 'TM::Transaction', 'transaction ok');

      $tx->assert_toplet ($tx->id_toplet ('ggg'));
    } # transaction commit

    sleep 2;
    
  } # $tm goes out of scope, may try a sync
  
  # check:  do not write back
  $st = stat ($filename) or die "No $filename: $!";
  is ($creation_time, $st->mtime, 'waited a bit...file was not touched anymore');

  unlink $filename;
}


eval {
  require TM::Virtual::DNS;
  $TM::schemes{'dns:'} = 'TM::Virtual::DNS';
  my $tm = new TM (tau => 'dns:whatever');
  ok ($tm->toplet ('localhost'), 'dns: found localhost');
##print Dumper $localhost;
}; if ($@) {
  like ($@, qr/Can't locate/, "skipping DNS test ("._chomp($@).")");
}




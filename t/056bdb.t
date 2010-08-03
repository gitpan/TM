use strict;
use warnings;

# change 'tests => 1' to 'tests => last_test_to_print';
use Test::More qw(no_plan);

use Data::Dumper;

use_ok ('TM::ResourceAble::BDB');

my ($tmp);
use IO::File;
use POSIX qw(tmpnam);
do { $tmp = tmpnam() ;  } until IO::File->new ($tmp, O_RDWR|O_CREAT|O_EXCL);

END { unlink ($tmp) || warn "cannot unlink tmp file '$tmp'"; }


use constant DONE => 1;
my $STATEMENTS = 60;
my $ITERATIONS = 10;

if (DONE) {
    my $bdb = new TM::ResourceAble::BDB (file => $tmp);

#warn Dumper $bdb;
#warn "tied at ".$bdb->{baseuri};
    ok ($bdb->baseuri, 'baseuri method');

    ok ($bdb->{mid2iid}->{isa}, 'mid2iid direct access');
    ok ($bdb->{assertions}->{'97b634a43b47218b9970e86f61671ce9'}, 'assertions direct access');

    ok ((scalar $bdb->match_forall (nochar => 1)), 'match_forall over infrastructure');

#$bdb->internalize ('aaa' => 'http://xxx');
#warn Dumper $bdb;

#    diag ('populating map ...');
    use TM::Literal;
    $bdb->assert (
	map { Assertion->new (kind => TM->NAME, type => 'name', scope => 'us', roles => [ 'thing', 'value' ], players => [ 'aaa', new TM::Literal ("AAA$_") ]) }
	   (1..$STATEMENTS)
	    );
#    diag ('...done');

#    use Benchmark qw(:hireswallclock) ;
#    timethis ($ITERATIONS, sub {
	my @as = $bdb->match_forall (char => 1, topic => 'tm://nirvana/aaa');
	ok (@as == $STATEMENTS, 'found all inserted');
#	    });
}

__END__

use HTML::Mason;


sleep 10;
use TM::Materialized::AsTMa;
my $tm1 = new TM::Materialized::AsTMa (file => 'maps/mapreduce.atm')->sync_in;

{
    use TM::ResourceAble::MLDBM;
    my $tm2 = new TM::ResourceAble::MLDBM (file => '/tmp/map.map');
    $tm2->clear;
    $tm2->add ($tm1);
}

{
    use TM::ResourceAble::MLDBM;
    my $tm3 = new TM::ResourceAble::MLDBM (file => '/tmp/map.map');
    warn "toplets: ". scalar $tm3->toplets;
}
sleep 20;

exit;

__END__

$update = $format->{package}->new (file => "$mp." . $format->{extension});  

__END__

use strict;
use warnings;

$Data::Dumper::Indent = 1;

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}

##warn "\n# annoying warning about Data::Dumper can be ignored";

my $warn = shift @ARGV;
unless ($warn) {
    close STDERR;
    open (STDERR, ">/dev/null");
    select (STDERR); $| = 1;
}

#== TESTS ===========================================================================

require_ok( 'TM::ResourceAble::MLDBM' );

eval {
  my $tm = new TM::ResourceAble::MLDBM ();
}; like ($@, qr/no file/, _chomp ($@));

{
     my $tm = new TM::ResourceAble::MLDBM (file => $tmp);
    
     ok ($tm->isa('TM'),                       'correct class');
     ok ($tm->isa('TM::ResourceAble::MLDBM'), 'correct class');
 }

unlink ($tmp) || warn "# cannot unlink $tmp file, but that is ok";

my $whatever;  # just a temp
my $whatever2; # just a temp
my $whatever3; # just a temp

{
    my $tm = new TM::ResourceAble::MLDBM (file => $tmp, baseuri => 'tm:');
# test 1
    $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'subclass', 'superclass' ], players => [ 'ramsti', 'rumsti' ]));
    $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'superclass', 'subclass' ], players => [ 'ramsti', 'rimsti' ]));

# test 2
    $tm->internalize ('aaa' => 'http://AAA/');
# test 3
    $tm->internalize ('aaa' => \ 'http://aaa/');
# test 4
    $tm->internalize ('tm:ccc' => undef);
# test 5
    $whatever = $tm->internalize ('http://bbb/');

# test 6
    $tm->internalize ('tm:fff' => undef);
    $tm->externalize ('tm:fff');
    ok (!$tm->retrieve ('fff'), 'looking for fff, not anymore there (pre test)');

# test 7
    my $m = Assertion->new (scope => 'sss', type => 'ttt', roles => [ 'aaa', 'bbb' ], players => [ 'xxx', 'yyy' ]);
    ($whatever2) = $tm->assert ($m);

# test 8
    ($whatever3) = $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'superclass', 'subclass' ], players => [ 'romsti', 'rumsti' ]));
    $tm->retract ($whatever3->[TM->LID]);
    ok (!$tm->retrieve ($whatever3->[TM->LID]), 'looking for '.$whatever3->[TM->LID].', not anymore there (pre test)');
}

{
    my $tm = new TM::ResourceAble::MLDBM (file => $tmp);
#warn Dumper $tm;

    is ($tm->url,     "file:$tmp", 'url survived');
    is ($tm->baseuri, 'tm:',       'baseuri survived');

# test 1
    is ($tm->tids ('rumsti') , 'tm:rumsti', 'found inserted by assertion 1');
    is ($tm->tids ('ramsti') , 'tm:ramsti', 'found inserted by assertion 2');
    ok ($tm->is_subclass ($tm->tids ('ramsti', 'rumsti')), 'found subclass 1');
    ok ($tm->is_subclass ($tm->tids ('rimsti', 'rumsti')), 'found subclass 2');

# test 2
    ok (eq_array ([ $tm->tids ('aaa') ],                        [ 'tm:aaa' ]), 'found inserted 1');
    ok (eq_array ([ $tm->tids (\ 'http://aaa/') ],              [ 'tm:aaa' ]), 'found inserted 2');
# test 3
    ok (eq_array ([ $tm->tids ('http://AAA/') ],                [ 'tm:aaa' ]), 'found inserted 3');

    is_deeply ( $tm->externalize ('tm:aaa'),
		[
		 'tm:aaa',
		 'http://AAA/',
		 [ 'http://aaa/' ]
		 ] ,                                                           'externalize 1');
# test 4
    ok (eq_array ([ $tm->tids ('tm:ccc') ],                     [ 'tm:ccc' ]), 'found inserted 4');
# test 5
    ok (eq_array ([ $tm->tids ('http://bbb/') ],                [ $whatever ]), 'found inserted 5');
# test 6
    ok (!$tm->retrieve ('tm:fff'),                                              'looking for fff, not anymore there (real test)');
# test 7
    my ($m) = $tm->match (TM->FORALL, scope => 'tm:sss');
    is_deeply ( $m,$whatever2,                                                  'identical assertion');
# test 8
    ok (!$tm->retrieve ($whatever3->[TM->LID]), 'looking for '.$whatever3->[TM->LID].', not anymore there (real test)');
}

{ # testing taxonometric functions
    my $tm = new TM::ResourceAble::MLDBM (file => $tmp);
#warn Dumper $tm;

    ok (eq_set ([ $tm->instances  ('assertion-type') ],[ 'isa', 'is-subclass-of' ]),                             'subsumption: instances 1');
    ok (eq_set ([ $tm->instances  ('scope') ],         [ 'us' ]),                                                'subsumption: instances 2');

#warn Dumper [ $tm->instancesT ('thing') ];
    ok (eq_set ([ $tm->instancesT ('thing') ],         [ map { $_->[TM->LID] } $tm->toplets ]),                  'subsumption: instances 4');
#warn Dumper [ $tm->instances ('isa') ];
    ok (eq_set ([$tm->instances ('isa') ], [
					    'c667ce5f4e485b45698c75621bc63893',
					    '9aa74da04e36d6f5c05ffe1c91eab7d2',
					    '8168aba8d6a9284c70e9c461a8977892'
					    ]),                                                                  'subsumption: instances 5');

    ok (eq_set ([$tm->types ('isa')],  [ 'assertion-type']),                                                     'subsumption: types 1');
    ok (eq_set ([$tm->typesT ('isa')], [ 'assertion-type',  'class' ]),                                          'subsumption: typesT 1');


    ok (eq_set ([ $tm->subclasses ('thing') ],        [  ]),                                                     'subsumption: subclasses 1');
    ok (eq_set ([ $tm->subclasses ('characteristic') ],
                [ 'occurrence', 'unique-characteristic', 'name' ]),                                              'subsumption: subclasses 2');

    ok (eq_set ([ $tm->subclassesT ('characteristic') ],
                [ 'characteristic', 'occurrence', 
		  'unique-characteristic', 'name' ]),                                                            'subsumption: subclassesT 1');
}

__END__

TODO: test merging

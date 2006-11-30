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

##warn "\n# annoying warning about Data::Dumper can be ignored";

#== TESTS ===========================================================================

require_ok( 'TM::Materialized::MLDBM2' );

eval {
  my $tm = new TM::Materialized::MLDBM2 ();
}; like ($@, qr/no file/, _chomp ($@));

my $tmp = '/tmp/xxx';

END { unlink ($tmp) || warn "# cannot unlink tmp file"; }

 {
     my $tm = new TM::Materialized::MLDBM2 (file => $tmp);
    
     ok ($tm->isa('TM'),                       'correct class');
     ok ($tm->isa('TM::Materialized::MLDBM2'), 'correct class');
 }

unlink ($tmp) || warn "# cannot unlink $tmp file, but that is ok";

my $whatever;  # just a temp
my $whatever2; # just a temp
my $whatever3; # just a temp

{
    my $tm = new TM::Materialized::MLDBM2 (file => $tmp, baseuri => 'tm:');
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
    my $tm = new TM::Materialized::MLDBM2 (file => $tmp);
#warn Dumper $tm;

    is ($tm->url,     "file:$tmp", 'url survived');
    is ($tm->baseuri, 'tm:',       'baseuri survived');

# test 1
    is ($tm->mids ('rumsti') , 'tm:rumsti', 'found inserted by assertion 1');
    is ($tm->mids ('ramsti') , 'tm:ramsti', 'found inserted by assertion 2');
    ok ($tm->is_subclass ($tm->mids ('ramsti', 'rumsti')), 'found subclass 1');
    ok ($tm->is_subclass ($tm->mids ('rimsti', 'rumsti')), 'found subclass 2');

# test 2
    ok (eq_array ([ $tm->mids ('aaa') ],                        [ 'tm:aaa' ]), 'found inserted 1');
    ok (eq_array ([ $tm->mids (\ 'http://aaa/') ],              [ 'tm:aaa' ]), 'found inserted 2');
# test 3
    ok (eq_array ([ $tm->mids ('http://AAA/') ],                [ 'tm:aaa' ]), 'found inserted 3');

    is_deeply ( $tm->externalize ('tm:aaa'),
		[
		 'http://AAA/',
		 [ 'http://aaa/' ]
		 ] ,                                                           'externalize 1');
# test 4
    ok (eq_array ([ $tm->mids ('tm:ccc') ],                     [ 'tm:ccc' ]), 'found inserted 4');
# test 5
    ok (eq_array ([ $tm->mids ('http://bbb/') ],                [ $whatever ]), 'found inserted 5');
# test 6
    ok (!$tm->retrieve ('tm:fff'),                                              'looking for fff, not anymore there (real test)');
# test 7
    my ($m) = $tm->match (TM->FORALL, scope => 'tm:sss');
    is_deeply ( $m,$whatever2,                                                  'identical assertion');
# test 8
    ok (!$tm->retrieve ($whatever3->[TM->LID]), 'looking for '.$whatever3->[TM->LID].', not anymore there (real test)');
}

{ # testing taxonometric functions
    my $tm = new TM::Materialized::MLDBM2 (file => $tmp);
#warn Dumper $tm;

    ok (eq_set ([ $tm->instances  ('tm:assertion-type') ],[ 'tm:isa', 'tm:is-subclass-of' ]), 'subsumption: instances 1');
    ok (eq_set ([ $tm->instances  ('tm:scope') ],         [ 'tm:us' ]),                       'subsumption: instances 2');

#warn Dumper [ $tm->instancesT ('thing') ];
    ok (eq_set ([ $tm->instancesT ('tm:thing') ],         [ $tm->midlets ]),                  'subsumption: instances 4');
    ok (eq_set ([$tm->instances ('tm:isa') ], [
                                             'tm:52f4b78b40050b928e3f0bc945ca974e',
                                             'tm:4a0acace7864ce2c66ff9ff89575b0a4',
                                             'tm:55a68be5cad02dd73034330f1407db3a'
					    ]),                                               'subsumption: instances 5');

    ok (eq_set ([$tm->types ('tm:isa')],  [ 'tm:assertion-type']),                           'subsumption: types 1');
    ok (eq_set ([$tm->typesT ('tm:isa')], [ 'tm:assertion-type',  'tm:class' ]),             'subsumption: typesT 1');


    ok (eq_set ([ $tm->subclasses ('tm:thing') ],        [  ]),                              'subsumption: subclasses 1');
    ok (eq_set ([ $tm->subclasses ('tm:characteristic') ],
                [ 'tm:occurrence', 'tm:unique-characteristic', 'tm:name' ]),                 'subsumption: subclasses 2');

    ok (eq_set ([ $tm->subclassesT ('tm:characteristic') ],
                [ 'tm:characteristic', 'tm:occurrence', 
		  'tm:unique-characteristic', 'tm:name' ]),                                  'subsumption: subclassesT 1');
}

__END__

TODO: test merging

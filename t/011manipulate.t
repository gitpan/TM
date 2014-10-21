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

{
    my $tm = new TM;
    $tm->internalize ('aaa' => undef);
    $tm->internalize ('tm://nirvana/ccc' => undef);

# normal insert
    ok (eq_array ([ $tm->mids ('aaa') ],              [ 'tm://nirvana/aaa' ]), 'found inserted 1');
    ok (eq_array ([ $tm->mids ('tm://nirvana/aaa') ], [ 'tm://nirvana/aaa' ]), 'found inserted 2');
    ok (eq_array ([ $tm->mids ('xxx') ],              [ undef ]),              'not found not inserted 1');
    ok (eq_array ([ $tm->mids ('tm://nirvana/xxx') ], [ undef ]),              'not found not inserted 2');
# with full name
    ok (eq_array ([ $tm->mids ('tm://nirvana/ccc') ], [ 'tm://nirvana/ccc' ]), 'found inserted 3');
    ok (eq_array ([ $tm->mids ('ccc') ],              [ 'tm://nirvana/ccc' ]), 'found inserted 3a');

# double insert
    $tm->internalize ('bbb' => undef);
    $tm->internalize ('bbb' => undef);
    ok (eq_array ([ $tm->mids ('bbb') ],              [ 'tm://nirvana/bbb' ]), 'found inserted 4a');

# using a subject address
    $tm->internalize ('ddd', 'http://ddd/');
    ok (eq_array ([ $tm->mids ('ddd') ],              [ 'tm://nirvana/ddd' ]), 'found inserted 5');
    ok (eq_array ([ $tm->mids ('http://ddd/') ],      [ 'tm://nirvana/ddd' ]), 'found inserted 6');

# using a subject indicator
    $tm->internalize ('eee', \ 'http://eee/');
    ok (eq_array ([ $tm->mids ('eee') ],              [ 'tm://nirvana/eee' ]), 'found inserted 7');
    ok (eq_array ([ $tm->mids (\ 'http://eee/') ],    [ 'tm://nirvana/eee' ]), 'found inserted 8');
#warn Dumper $tm;

    is_deeply ( $tm->externalize ('tm://nirvana/aaa'),
		[
		 undef,
		 [ ]
		 ] ,                                                           'externalize 1');
    ok (!$tm->mids ('aaa'),                                                    'externalize 2');
    is_deeply ( $tm->externalize ($tm->mids ('http://ddd/')),
		[
		 'http://ddd/',
		 [ ]
		 ] ,                                                           'externalize 3');
    ok (!$tm->mids ('ddd'),                                                    'externalize 4');
    is_deeply ( $tm->externalize ($tm->mids (\ 'http://eee/')),
		[
		 undef,
		 [
		  'http://eee/'
		  ]
		 ] ,                                                           'externalize 5');
    ok (!$tm->mids ('eee'),                                                    'externalize 6');
}

{ # adding one assertion, finding the toplets, finding it
    my $tm = new TM ();
    $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'subclass', 'superclass' ], players => [ 'rumsti', 'ramsti' ]));
    is ($tm->mids ('rumsti') , 'tm://nirvana/rumsti', 'found inserted by assertion 1');
    is ($tm->mids ('ramsti') , 'tm://nirvana/ramsti', 'found inserted by assertion 2');
}

{ # reasserting
    my $tm = new TM;
    my $npa = $tm->match (TM->FORALL);

    $tm->assert (Assertion->new (type => 'is-a', roles => [ 'class', 'instance' ], players => [ 'rumsti', 'ramsti' ]));
    $tm->assert (Assertion->new (type => 'is-a', roles => [ 'class', 'instance' ], players => [ 'rumsti', 'ramsti' ]));
    is (scalar $tm->match (TM->FORALL),                                 1+$npa, 'double assert 1');
    is (scalar $tm->match (TM->FORALL, iplayer => 'tm://nirvana/rumsti'),    1, 'double assert 2');
#warn Dumper $tm;
}

{ # simple matching
    my $tm = new TM;

    $tm->assert (Assertion->new (type => 'is-a', roles => [ 'class', 'instance' ], players => [ 'rumsti', 'ramsti' ]));
    is (scalar $tm->match (TM->FORALL, iplayer => 'tm://nirvana/rumsti'),    1, 'isa match 1');
    is (scalar $tm->match (TM->FORALL, iplayer => 'tm://nirvana/ramsti',
			               irole   => 'tm://nirvana/instance'),  1, 'isa match 2');
    is (scalar $tm->match (TM->FORALL, iplayer => 'tm://nirvana/ramsti'),    1, 'isa match 3');

    is (scalar $tm->match (TM->FORALL, iplayer => 'tm://nirvana/romsti'),    0, 'isa match 4');
#warn Dumper $tm;
}

{ # testing subclassing
    my $tm = new TM;
    $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'superclass', 'subclass' ], players => [ 'rumsti', 'ramsti' ]));
    $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'superclass', 'subclass' ], players => [ 'rumsti', 'remsti' ]));

    ok ($tm->is_subclass ($tm->mids ('ramsti', 'rumsti')), 'found subclass');
    ok ($tm->is_subclass ($tm->mids ('remsti', 'rumsti')), 'found subclass 2');

    ok (eq_set ([
		 map { $tm->get_players ($_, 'tm://nirvana/subclass') }
		 $tm->match (TM->FORALL,
			     type    => 'tm://nirvana/is-subclass-of', 
			     arole   => 'tm://nirvana/superclass', 
			     aplayer => 'tm://nirvana/rumsti', 
			     brole   => 'tm://nirvana/subclass')
		 ], 
		[
		 'tm://nirvana/ramsti',
		 'tm://nirvana/remsti'
		 ]
		), 'found all subclasses');
#warn Dumper $tm;

    # indirect subclassing
    $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'superclass', 'subclass' ], players => [ 'ramsti', 'rimsti' ]));
    ok ($tm->is_subclass ($tm->mids('rimsti', 'rumsti')), 'found indirect subclass');
    ok ($tm->is_subclass ($tm->mids('ramsti', 'rumsti')), 'found subclass');
    ok ($tm->is_subclass ($tm->mids('remsti', 'rumsti')), 'found subclass');
}

{ # retracting
  my $tm = new TM;
  ok (!$tm->retrieve ('tm://nirvana/aaa'), 'looking for aaa, not there');
  $tm->assert (Assertion->new (lid => 'tm://nirvana/aaa'),
	       Assertion->new (lid => 'tm://nirvana/bbb'),
	       Assertion->new (lid => 'tm://nirvana/ccc'));
#warn Dumper $tm;
  ok ($tm->retrieve ('tm://nirvana/aaa'),  'looking for aaa, is there');
  $tm->retract ('tm://nirvana/bbb');
  ok ($tm->retrieve ('tm://nirvana/aaa'),  'looking for aaa, is still there');
  $tm->retract ('tm://nirvana/aaa');
  ok (!$tm->retrieve ('tm://nirvana/aaa'), 'looking for aaa, not anymore there');
}

{ # exact result
  my $tm = new TM;
  my $m = Assertion->new (scope => 'sss', type => 'ttt', roles => [ 'aaa', 'bbb' ], players => [ 'xxx', 'yyy' ]);
  ($m) = $tm->assert ($m);
#warn Dumper $tm, $m;

  my @res = $tm->match (TM->FORALL, scope => 'tm://nirvana/sss');
  is (@res, 1,       'assertions detected');
  ok ($res[0] == $m, 'assertion correct');
}

{
  my $tm = new TM;
  my ($m1, $m2) = $tm->assert (Assertion->new (type => 'ttt', scope => 'sss', roles => [ 'aaa', 'bbb' ], players => [ 'xxx', 'yyy' ]),
			       Assertion->new (type => 'ttt', scope => 'sss', roles => [ 'aaa', 'bbb' ], players => [ 'zzz', 'uuu' ]));
#warn Dumper $tm;
  my @res = $tm->match        (TM->FORALL,
			       type => 'tm://nirvana/ttt',    roles => [ 'tm://nirvana/aaa', 'tm://nirvana/bbb' ]);
  ok (@res == 2, 'assertions detected');

  $tm->retract ($m2->[TM->LID], $m1->[TM->LID]);
  @res = $tm->match           (TM->FORALL,
			       type => 'tm://nirvana/ttt',    roles => [ 'tm://nirvana/aaa', 'tm://nirvana/bbb' ]);
  ok (@res == 0, 'assertions not detected');
}

{ # testing add, same baseuri
    my $tm1 = new TM;
    $tm1->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'superclass', 'subclass' ], players => [ 'rumsti', 'ramsti' ]));
    $tm1->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'superclass', 'subclass' ], players => [ 'rumsti', 'remsti' ]));

    my $tm2 = new TM;
    $tm2->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'superclass', 'subclass' ], players => [ 'rumsti', 'rimsti' ]));

    $tm2->add ($tm1);
#warn Dumper $tm2;
    ok (eq_set ([
		 map { $tm2->get_players ($_, 'tm://nirvana/subclass') }
		 $tm2->match (TM->FORALL,
			      type    => 'tm://nirvana/is-subclass-of', 
			      superclass => 'tm://nirvana/rumsti')
		 ], 
		[
		 'tm://nirvana/ramsti',
		 'tm://nirvana/remsti',
		 'tm://nirvana/rimsti',
		 ]
		), 'add: found all subclasses');
    ok (eq_array ([ 
		    $tm2->mids ('ramsti', 
				'remsti',
				'rimsti',) ], 
		  [  'tm://nirvana/ramsti',
		     'tm://nirvana/remsti',
		     'tm://nirvana/rimsti', ]), 'add: found all rumstis');
}

{ # testing taxonometric functions
    use TM;
    my $tm = new TM (baseuri => 'tm:', psis => $TM::PSI::topicmaps);
#warn Dumper $tm;

    ok (eq_set ([ $tm->instances  ('tm:assertion-type') ],[ 'tm:isa', 'tm:is-subclass-of' ]), 'subsumption: instances 1');

    ok (eq_set ([ $tm->instances  ('tm:scope') ],         [ 'tm:us' ]),                       'subsumption: instances 2');
#warn Dumper [ $tm->instancesT ('thing') ];

    ok (eq_set ([$tm->instances  ('tm:thing')], [$tm->midlets]),                              'instances of thing');
    ok (eq_set ([$tm->instancesT ('tm:thing')], [$tm->midlets]),                              'instances of thing');

    ok (eq_set ([$tm->instances ('tm:isa') ], [
                                             'tm:52f4b78b40050b928e3f0bc945ca974e',
                                             'tm:4a0acace7864ce2c66ff9ff89575b0a4',
                                             'tm:55a68be5cad02dd73034330f1407db3a'
					    ]),                                               'subsumption: instances 5');

    foreach my $a ($tm->instances ('tm:isa')) {
	ok (eq_set ([ $tm->types($a)] , [ 'tm:isa' ]),                                       'subsumption assertions type');
	is ($tm->retrieve ($a)->[TM->TYPE], 'tm:isa',                                        'subsumption assertions instances')
    }
    foreach my $a ($tm->instances ('tm:is-subclass-of')) {
	ok (eq_set ([ $tm->types($a)] , [ 'tm:is-subclass-of' ]),                            'subsumption assertions type');
	is ($tm->retrieve ($a)->[TM->TYPE], 'tm:is-subclass-of',                             'subsumption assertions instances')
    }



    ok (eq_set ([$tm->types ('tm:isa')],  [ 'tm:assertion-type']),                           'subsumption: types 1');
    ok (eq_set ([$tm->typesT ('tm:isa')], [ 'tm:assertion-type',  'tm:class' ]),             'subsumption: typesT 1');


    ok (eq_set ([ $tm->subclasses ('tm:thing') ],        [  ]),                              'subsumption: subclasses 1');
    ok (eq_set ([ $tm->subclasses ('tm:characteristic') ],
                [ 'tm:occurrence', 'tm:unique-characteristic', 'tm:name' ]),                 'subsumption: subclasses 2');

    ok (eq_set ([ $tm->subclassesT ('tm:characteristic') ],
                [ 'tm:characteristic', 'tm:occurrence', 
		  'tm:unique-characteristic', 'tm:name' ]),                                  'subsumption: subclassesT 1');
}

{ # testing taxonometric functions
    my $tm = new TM;

    $tm->assert (Assertion->new (type => 'is-subclass-of', scope => 'us', roles => [ 'subclass', 'superclass' ], players => [ 'XXX', 'AAA' ]));
    $tm->assert (Assertion->new (type => 'is-subclass-of', scope => 'us', roles => [ 'subclass', 'superclass' ], players => [ 'AAA', 'BBB' ]));
    $tm->assert (Assertion->new (type => 'is-subclass-of', scope => 'us', roles => [ 'subclass', 'superclass' ], players => [ 'AAA', 'CCC' ]));

    $tm->assert (Assertion->new (type => 'isa', scope => 'us', roles => [ 'class', 'instance' ], players => [ 'XXX', 'xxx' ]));
    $tm->assert (Assertion->new (type => 'isa', scope => 'us', roles => [ 'class', 'instance' ], players => [ 'ZZZ', 'xxx' ]));
    $tm->assert (Assertion->new (type => 'isa', scope => 'us', roles => [ 'class', 'instance' ], players => [ 'XXX', 'yyy' ]));

    $tm->assert (Assertion->new (type => 'isa', scope => 'us', roles => [ 'class', 'instance' ], players => [ 'AAA', 'aaa' ]));
    $tm->assert (Assertion->new (type => 'isa', scope => 'us', roles => [ 'class', 'instance' ], players => [ 'BBB', 'bbb' ]));

    ok ($tm->is_a  ('tm://nirvana/aaa',  'tm://nirvana/thing'), 'is_a 1');
    ok ($tm->is_a  ('tm://nirvana/us',   'tm://nirvana/scope'), 'is_a 2');

    ok (!$tm->is_a ('tm://nirvana/uuu',  'tm://nirvana/thing'), 'is_a 3');

    ok ($tm->is_a  ('tm://nirvana/aaa',  'tm://nirvana/AAA'),   'is_a 4');
    ok ($tm->is_a  ('tm://nirvana/aaa',  'tm://nirvana/BBB'),   'is_a 5');
    ok ($tm->is_a  ('tm://nirvana/aaa',  'tm://nirvana/CCC'),   'is_a 6');

    ok (eq_set ($tm->are_instances ('tm://nirvana/XXX', [ $tm->midlets ]),
		[  'tm://nirvana/yyy',
		   'tm://nirvana/xxx'  ]),                           'are_instance: XXX');

    ok (eq_set ($tm->are_instances ('tm://nirvana/AAA', [ $tm->midlets ]),
		[  'tm://nirvana/aaa',
		   'tm://nirvana/yyy',
                   'tm://nirvana/xxx' ]),                            'are_instance: AAA');

    ok (eq_set ($tm->are_instances ('tm://nirvana/BBB', [ $tm->midlets ]),
		[  'tm://nirvana/aaa',
		   'tm://nirvana/bbb',
		   'tm://nirvana/yyy',
                   'tm://nirvana/xxx' ]),                            'are_instance: BBB');


    ok ($tm->is_subclass ('tm://nirvana/AAA', 'tm://nirvana/thing'), 'subclass thing');
    ok ($tm->is_subclass ('tm://nirvana/AAA', 'tm://nirvana/BBB'),   'subclass 1');
    ok ($tm->is_subclass ('tm://nirvana/XXX', 'tm://nirvana/BBB'),   'subclass 2');

    ok (eq_set ([ $tm->subclasses ('tm://nirvana/BBB') ],
		[ 'tm://nirvana/AAA' ]),                             'subclasses 1');
    ok (eq_set ([ $tm->subclasses ('tm://nirvana/AAA') ],
		[ 'tm://nirvana/XXX' ]),                             'subclasses 2');
    ok (eq_set ([ $tm->subclassesT ('tm://nirvana/BBB') ],
		[ 'tm://nirvana/XXX',
		  'tm://nirvana/BBB',
		  'tm://nirvana/AAA']),                              'subclasses 3');

    ok (eq_set ([ $tm->superclasses ('tm://nirvana/XXX') ],
		[ 'tm://nirvana/AAA' ]),                             'superclasses 1');
    ok (eq_set ([ $tm->superclasses ('tm://nirvana/AAA') ],
		[ 'tm://nirvana/BBB',
		  'tm://nirvana/CCC' ]),                             'superclasses 2');
    ok (eq_set ([ $tm->superclassesT ('tm://nirvana/XXX') ],
		[ 'tm://nirvana/XXX',
		  'tm://nirvana/BBB',
		  'tm://nirvana/CCC',
		  'tm://nirvana/AAA']),                              'superclasses 3');

    ok (eq_set ([ $tm->types ('tm://nirvana/xxx') ],
		[ 'tm://nirvana/XXX',
                  'tm://nirvana/ZZZ' ]),                             'types 1');
    ok (eq_set ([ $tm->typesT ('tm://nirvana/xxx') ],
		[ 'tm://nirvana/XXX',
                  'tm://nirvana/ZZZ',
		  'tm://nirvana/BBB',
		  'tm://nirvana/CCC',
		  'tm://nirvana/AAA' ]),                             'types 2');

    ok (eq_set ([ $tm->instances ('tm://nirvana/AAA') ],
		[ 'tm://nirvana/aaa' ]),                             'instances 1');
    ok (eq_set ([ $tm->instancesT ('tm://nirvana/AAA') ],
		[ 'tm://nirvana/xxx',
                  'tm://nirvana/yyy',
		  'tm://nirvana/aaa' ]),                             'instances 2');
}

{ # testing characteristics
    my $tm = new TM;

    use TM::Literal;

    $tm->assert (Assertion->new (kind => TM->NAME, type => 'name', scope => 'us', roles => [ 'thing', 'value' ], players => [ 'aaa', new TM::Literal ('AAA') ]));
    $tm->assert (Assertion->new (kind => TM->NAME, type => 'name', scope => 'us', roles => [ 'thing', 'value' ], players => [ 'aaa', new TM::Literal ('AAAA') ]));
    $tm->assert (Assertion->new (kind => TM->OCC,  type => 'occurrence', scope => 'us', roles => [ 'thing', 'value' ], players => [ 'aaa', new TM::Literal ('OOOO') ]));
    $tm->assert (Assertion->new (kind => TM->NAME, type => 'name', scope => 'us', roles => [ 'thing', 'value' ], players => [ 'ccc', new TM::Literal ('CCC') ]));
#warn Dumper $tm;

    ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $tm->match_forall (char => 1, topic => $tm->mids ('aaa')) ] ,
		[ 'AAA',
		  'OOOO',
                  'AAAA' ]),                             'chars of aaa');
}

__END__

TODO: are_instances, ...filters

TODO: reification

TODO: variants


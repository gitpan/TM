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
use Test::Deep;

use Data::Dumper;
$Data::Dumper::Indent = 1;

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}

#== TESTS =====================================================================

require_ok ('TM::DM');

use TM::Materialized::AsTMa;
my $atm = new TM::Materialized::AsTMa (baseuri => 'tm:rumsti',
				       inline  => '

adam
bn: Adam Adamovich
bn @ en: Adam Adamovichev
oc: http://adam.akest.hewor.ldgorou.nd/
sin: tm:ramsti/
sin: tm:romsti/
in: the first will be the last
in @ en (opinion): he is highly overrated
oc (homepage): http://oldtesta.ment/~adam/


(begets)
parent: adam eve
child: cain

(begets)
parent: adam eve
child: abel

(begets)
parent: adam eve
child: seth

(begets)
parent: adam eve
child: azura

#--

(begets)
parent: cain
child: enoch

#--

(begets)
parent: enoch
child: irad

#--

(begets)
parent: irad
child: mehajael

#--

(begets)
parent: seth
child: enosh

(begets) @ old_testament
parent: seth
child: noam

   ');

$atm->sync_in;

eval {
    my $tmdm = new TM::DM (map => 12);
}; like ($@, qr/not really/i, _chomp($@));

{ # test TopicMap
    my $tmdm = new TM::DM (map => $atm);

    my $tm = $tmdm->topicmap;

    is (scalar $tm->topics,       scalar $atm->midlets, 'all topics there');
    is (scalar $tm->associations, scalar $atm->match (TM->FORALL, nochar => 1),   'all assocs there');

    is ($tm->reifier,             undef,                'no reifier');
    $atm->internalize ('rumsti' => 'tm:rumsti#');
    is ($tm->reifier->id,         'tm:rumsti#rumsti',   'reifier');
}

{ # test Topic
    my $tmdm = new TM::DM (map => $atm);

    my $tm = $tmdm->topicmap;
    $atm->internalize ('adam' =>   'tm:rumsti#rimsti');
#    $atm->internalize ('remsti' => \ 'tm:ramsti/');
#    $atm->internalize ('remsti' => \ 'tm:romsti/');

    my $to = $tm->topic ('adam');

#    warn Dumper $to;

    is ($to->id, 'tm:rumsti#adam',                'id');

    is ($to->subjectLocators, 'tm:rumsti#rimsti', 'subject locator');
    ok (eq_set ([
		 $to->subjectIdentifiers
		 ],
		[
		 'tm:ramsti/',
		 'tm:romsti/'
		 ]),                              'subject indicators');
    is ($to->parent->id, $tmdm->topicmap->id,     'parent');

    ok (eq_set ( [ map { $_->value } $to->names ],
		 [
		  'Adam Adamovich',
		  'Adam Adamovichev'
		  ]),                             'names');

    is (scalar $to->occurrences, 4, 'occurrences');

    ok (! grep ($_ ne 'tm:rumsti#parent', map {$_->type->id}   $to->roles), 'adam is only parent');
    ok (! grep ($_ ne 'tm:rumsti#adam',   map {$_->player->id} $to->roles), 'adam is only parent');
} 

{ # assoc
    my $tmdm = new TM::DM (map => $atm);
    my $tm = $tmdm->topicmap;
    my @as = $tm->associations (iplayer => 'adam');

    is (scalar @as, 4,                                                'all adam involvements');

    ok (! grep ($_ ne 'tm:rumsti#', map { $_->parent->id } @as ),     'assocs parent');
    ok (! grep ($_ ne 'tm:rumsti#begets', map { $_->type->id } @as ), 'assocs types');

    foreach my $a (@as) {
	foreach my $r ($a->roles) {
# warn Dumper $r;
	    next if $r->type->id   eq 'tm:rumsti#child' or $r->type->id eq 'tm:rumsti#parent';
	    next if $r->type->id   eq 'tm:rumsti#parent' &&
                   ($r->player->id eq 'tm:rumsti#adam'  or $r->type->id eq 'tm:rumsti#eve');
	    die;
	}
	ok (1, 'adam and eve play parents, makes creepy sense');
    }

    is (scalar grep ($_ ne 'tm:rumsti#us', map { $_->scope->id }
               $tm->associations ), 1,                                'others are us scoped assocs');
    @as = grep ($_->scope->id ne 'tm:rumsti#us',
		$tm->associations (anyid => 'tm:rumsti#old_testament'));
    is (scalar @as, 1,                                                'one scoped assoc');

    $atm->internalize ('sethnoam' => $as[0]->id);

    is ($as[0]->reifier->id, 'tm:rumsti#sethnoam',                    'reified assoc');
}

{ # name
    my $tmdm = new TM::DM (map => $atm);
    my $tm = $tmdm->topicmap;

    my $to = $tm->topic ('adam');
    my @ns = $to->names;

    ok (eq_set ( [ map { $_->value } @ns ],
		 [
		  'Adam Adamovich',
		  'Adam Adamovichev'
		  ]),                             'names (again)');

    ok (! grep ($_ ne 'tm:rumsti#name', map { $_->type->id } @ns ),     'name type');

    @ns = grep ($_->scope->id ne 'tm:rumsti#us', $to->names);
    is (scalar @ns, 1,                                                'one scoped name');
    is ($ns[0]->scope->id, 'tm:rumsti#en',                            'name scope');

    $atm->internalize ('adamnamer' => $ns[0]->id);
    is ($ns[0]->reifier->id, 'tm:rumsti#adamnamer',                   'reified name');

    is ($ns[0]->parent->id, $to->id,                                  'name parent')
}


{ # occurrence
    my $tmdm = new TM::DM (map => $atm);
    my $tm = $tmdm->topicmap;

    my $to = $tm->topic ('adam');
    my @oc = $to->occurrences;

    cmp_set ([
		 map { [ $_->value->[0], $_->value->[1], $_->type->id, $_->scope->id ] } @oc
		 ],
		[
		 [
		  'http://adam.akest.hewor.ldgorou.nd/',
		  'http://www.w3.org/2001/XMLSchema#anyURI',
		  'tm:rumsti#occurrence',
		  'tm:rumsti#us'
		  ],
		 [
		  'http://oldtesta.ment/~adam/',
		  'http://www.w3.org/2001/XMLSchema#anyURI',
		  'tm:rumsti#homepage',
		  'tm:rumsti#us'
		  ],
		 [
		  'the first will be the last',
		  'http://www.w3.org/2001/XMLSchema#string',
		  'tm:rumsti#occurrence',
		  'tm:rumsti#us'
		  ],
		 [
		  'he is highly overrated',
		  'http://www.w3.org/2001/XMLSchema#string',
		  'tm:rumsti#opinion',
		  'tm:rumsti#en'
		  ],
		 ],                                                   'occurrences (again)');

    $atm->internalize ('adamnamer2' => $oc[0]->id);
    is ($oc[0]->reifier->id, 'tm:rumsti#adamnamer2',                  'reified occur');

    ok (! grep ($_ ne 'tm:rumsti#adam', map { $_->parent->id } @oc ), 'occur parent');
}

{ # roles
    my $tmdm = new TM::DM (map => $atm);
    my $tm = $tmdm->topicmap;

    my ($a) = grep ($_->scope->id ne 'tm:rumsti#us',
		    $tm->associations (anyid => 'tm:rumsti#old_testament'));

    cmp_set ([    map { [ $_->type->id, $_->player->id ] }    $a->roles ],
	     [
	      [
	       'tm:rumsti#child',
	       'tm:rumsti#noam'
	       ],
	      [
	       'tm:rumsti#parent',
	       'tm:rumsti#seth'
	       ]
	      ],                                                        'roles all there');

    ok (! grep ($_ ne $a->id, map { $_->parent->id } $a->roles ),       'roles parent');

    eval {
	map { $_->id } $a->roles;
    }; like ($@, qr/object method/i, _chomp($@));

}

{ # topic search spec
    my $tmdm = new TM::DM (map => $atm);
    my $tm = $tmdm->topicmap;

    eval {
	$tm->topics (\ 'rumsti');
    }; like ($@, qr/unhandled/i, _chomp($@));

    eval {
	$tm->topics (\ '+rumsti');
    }; like ($@, qr/unknown/i, _chomp($@));


    is (scalar $tm->topics,            scalar $atm->midlets, 'spec: empty, all topics there');
    is (scalar $tm->topics (\ '+all'), scalar $atm->midlets, 'spec: expl, all topics there');
    ok (eq_set ([ grep (!/[0-9a-f]{32}/, $atm->midlets) ],
		[ map { $_->id } $tm->topics (\ '+all -associations -names -occurrences') ]),
                                                              'spec: expl, all non assertion topics there');
    ok (eq_set ([ map { $_->id } $tm->topics (\ '+all +all') ],
		[ map { $_->id } $tm->topics (\ '+all') ]),                  'spec: expl, no duplicates');

#warn Dumper scalar map { $_->id } $tm->topics (\ '+all +all') ; exit;
#    warn Dumper [ map { $_->id } $tm->topics (\ '+all -names +names') ]; exit;

    ok (eq_set ([ map { $_->id } $tm->topics (\ '+all -names +names') ],
		[ map { $_->id } $tm->topics (\ '+all +names') ]),          'spec: expl, no duplicates');

    ok (eq_set ([ map { $_->id } $tm->topics (\ '+all -names +names') ],
		[ map { $_->id } $tm->topics (\ '+all') ]),                 'spec: expl, no duplicates');

    ok (eq_set ([ map { $_->id } $tm->topics (\ '+all -names -names') ],
		[ map { $_->id } $tm->topics (\ '+all -names') ]),          'spec: expl, no duplicates');

    ok (eq_set ([ map { $_->id } $tm->topics (\ '+names +occurrences') ],
		[ map { $_->id } $tm->topics (\ '+occurrences +names') ]),  'spec: expl, commutative');

#    warn Dumper [map {$_->{mid} } $tm->topics (\ '+all -infrastructure -associations -names -occurrences') ];
    is (scalar $tm->topics (\ '+all -infrastructure -associations -names -occurrences'),
	                               22,
                                                             'spec: expl, all user, non assertion topics there');
}

__END__

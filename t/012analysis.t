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

use Class::Trait;

#== TESTS =====================================================================

require_ok ('TM::Analysis');

{ # clustering
    use TM::Materialized::AsTMa;
    my $tm = new TM::Materialized::AsTMa (baseuri => 'tm:',
					  inline => '
# cluster 1
aaa subclasses bbb

bbb is-a ccc

(ddd)
eee: fff
ggg: hhh
iii: ccc

#------------------------------------
# cluster 2
zzz subclasses yyy

zzz subclasses xxx

www is-a zzz

(vvv)
uuu: rrr
sss: ttt
qqq: www
    ');
    $tm->sync_in;

#warn Dumper $tm; exit;

    Class::Trait->apply ($tm, 'TM::Analysis');

    my $clusters = $tm->clusters (use_lid => 0);
#    foreach (@$clusters) {
#	print "we are connnected: ", join (",", @$_), "\n\n";
#    }

    my ($c1) = grep (grep ($_ eq 'tm:aaa', @$_), @$clusters);
    ok (eq_set ($c1,  [
    'tm:fff',
    'tm:aaa',
    'tm:bbb',
    'tm:hhh',
    'tm:ccc'
    ]), 'cluster 1');
    my ($c2) = grep (grep ($_ eq 'tm:www', @$_), @$clusters);
    ok (eq_set ($c2,  [
    'tm:www',
    'tm:yyy',
    'tm:xxx',
    'tm:ttt',
    'tm:rrr',
    'tm:zzz'
    ]), 'cluster 2');
    my ($c3) = grep (grep ($_ eq 'tm:sss', @$_), @$clusters);
    ok (eq_set ($c3, [    'tm:sss',  ]), 'cluster 3');

    $clusters = $tm->clusters (use_roles => 1, use_type => 1, use_lid => 0);
#warn Dumper $clusters;
    my ($c4) = grep (grep ($_ eq 'tm:aaa', @$_), @$clusters);

    is (scalar @$c4, 33, 'cluster 4');
}

{ # statistics
    use TM;
    my $tm1 = new TM;
    Class::Trait->apply ($tm1, 'TM::Analysis');


    my $stats1 = $tm1->statistics;
#warn Dumper $stats1;

    is ($stats1->{'nr_maplets'},  8, 'nr_maplets');
    is ($stats1->{'nr_midlets'},  scalar $tm1->midlets, 'nr_midlets');
    is ($stats1->{'nr_clusters'}, 15, 'nr_clusters');

    use TM::Materialized::AsTMa;
    my $tm2 = new TM::Materialized::AsTMa (baseuri => 'tm:',
					   inline => '
aaa subclasses bbb

bbb is-a ccc
    ');
    $tm2->sync_in;
#warn Dumper $tm2;

    Class::Trait->apply ($tm2, 'TM::Analysis');
    my $stats2 = $tm2->statistics;
#warn Dumper $stats;

    is ($stats2->{'nr_maplets'},  $stats1->{'nr_maplets'} + 2,     'nr_maplets');
    is ($stats2->{'nr_midlets'},  $stats1->{'nr_midlets'} + 3 + 2, 'nr_midlets');
    is ($stats2->{'nr_clusters'}, 15,                              'nr_clusters');
    
}

{ # orphanage
    use TM::Materialized::AsTMa;
    my $tm = new TM::Materialized::AsTMa (baseuri => 'tm:',
					  inline => '
aaa subclasses bbb

bbb is-a ccc
    ');
    $tm->sync_in;
    Class::Trait->apply ($tm, 'TM::Analysis');

    my $o = $tm->orphanage;
#warn Dumper $o;

    ok (!grep ($_ eq 'tm:bbb', @{$o->{untyped}}),        'bbb is not untyped');
    ok ( grep ($_ eq 'tm:ccc', @{$o->{untyped}}),        'ccc is     untyped');

    ok (!grep ($_ eq 'tm:ccc', @{$o->{empty}}),          'ccc is not untyped');
    ok ( grep ($_ eq 'tm:aaa', @{$o->{empty}}),          'aaa is     untyped');

    ok (!grep ($_ eq 'tm:aaa', @{$o->{unclassified}}),   'aaa is not unclassified');
    ok ( grep ($_ eq 'tm:ccc', @{$o->{unclassified}}),   'ccc is     unclassified');

    ok (!grep ($_ eq 'tm:bbb', @{$o->{unspecified}}),    'bbb is not unspecified');
    ok ( grep ($_ eq 'tm:aaa', @{$o->{unspecified}}),    'aaa is     unspecified');

    $o = TM::Analysis::orphanage ($tm, 'untyped');
    ok ($o->{untyped}, 'only untyped');
}

require_ok ('TM::Tree');

{ # tree


    use TM::Materialized::AsTMa;
    my $tm = new TM::Materialized::AsTMa (baseuri => 'tm:',
					  inline => '
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

(begets)
parent: seth
child: noam

   ');

    Class::Trait->apply ($tm => 'TM::Tree');
    $tm->sync_in;

#warn Dumper $tm;

  my $pedigree =  $tm->tree ($tm->mids ('adam',
                                       'begets',
                                       'parent',
                                       'child')
                                        );

#warn Dumper $pedigree;

  ok ($pedigree->{lid} eq 'tm:adam', 'tree');
  ok (eq_set([ map { $_->{lid} } @{$pedigree->{children}} ],
             [ 'tm:cain', 'tm:abel', 'tm:azura', 'tm:seth' ]), 'tree');
  my ($seth) = grep ($_->{lid} eq 'tm:seth', @{$pedigree->{children}});
  ok (eq_set([ map { $_->{lid} } @{$seth->{children}} ],
             [ 'tm:noam', 'tm:enosh' ]), 'tree');

  my $pedigree2 =  $tm->tree_x ($tm->mids ('adam',
                                           'begets',
                                           'parent',
                                           'child')
                                           );

#warn Dumper $pedigree2;
    use Test::Deep;

    cmp_deeply( $pedigree2,$pedigree, "tree = tree_x" );
}


{ # taxonomy
    use TM::Materialized::AsTMa;
    my $tm = new TM::Materialized::AsTMa (baseuri => 'tm:',
					  inline => '
aaa subclasses thing

bbb subclasses thing

ccc subclasses aaa

ddd subclasses aaa

');
    Class::Trait->apply ($tm => 'TM::Tree');
    $tm->sync_in;

#warn Dumper $tm;

    my $taxo =  $tm->taxonomy ($tm->mids ('aaa'));

#warn Dumper $taxo;

    ok ($taxo->{lid} eq 'tm:aaa',                'taxo 1');
    ok (eq_set([ map { $_->{lid} } @{$taxo->{children}} ],
	       [ 'tm:ccc', 'tm:ddd' ]),          'taxo 2');

    $taxo =  $tm->taxonomy;
#warn Dumper $taxo;
    ok ($taxo->{lid} eq 'tm:thing',              'taxo 3');
    ok (eq_set([ map { $_->{lid} } @{$taxo->{children}} ],
	       [ 'tm:aaa', 'tm:bbb' ]),          'taxo 4');
    my ($aaa) = grep ($_->{lid} eq 'tm:aaa', @{$taxo->{children}});
    ok (eq_set([ map { $_->{lid} } @{$aaa->{children}} ],
	       [ 'tm:ccc', 'tm:ddd' ]),          'taxo 5');

}

__END__


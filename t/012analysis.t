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

#warn Dumper $tm;

    my $clusters = TM::Analysis::clusters ($tm, use_lid => 0);
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

    $clusters = TM::Analysis::clusters ($tm, use_roles => 1, use_type => 1, use_lid => 0);
#warn Dumper $clusters;
    my ($c4) = grep (grep ($_ eq 'tm:aaa', @$_), @$clusters);
    is (scalar @$c4, 33, 'cluster 4');
}

{ # statistics
    use TM;
    my $tm1 = new TM;

    my $stats1 = TM::Analysis::statistics ($tm1);
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

    my $stats2 = TM::Analysis::statistics ($tm2);
#warn Dumper $stats;

    is ($stats2->{'nr_maplets'},  $stats1->{'nr_maplets'} + 2,     'nr_maplets');
    is ($stats2->{'nr_midlets'},  $stats1->{'nr_midlets'} + 3 + 2, 'nr_midlets');
    is ($stats2->{'nr_clusters'}, 15,                              'nr_clusters');
    
}
__END__

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
    $tm->sync_in;

#warn Dumper $tm;

  use TM::Tau::Tree;
  my $pedigree =  TM::Tau::Tree::tree ($tm,
                                       'adam',
                                       'begets',
                                       'parent',
                                       'child',
                                        );

#warn Dumper $pedigree;

  ok ($pedigree->{lid} eq 'tm:adam', 'tree');
  ok (eq_set([ map { $_->{lid} } @{$pedigree->{children}} ],
             [ 'tm:cain', 'tm:abel', 'tm:azura', 'tm:seth' ]), 'tree');
  my ($seth) = grep ($_->{lid} eq 'tm:seth', @{$pedigree->{children}});
  ok (eq_set([ map { $_->{lid} } @{$seth->{children}} ],
             [ 'tm:noam', 'tm:enosh' ]), 'tree');
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
    $tm->sync_in;

#warn Dumper $tm;

    use TM::Tau::Tree;
    my $taxo =  TM::Tau::Tree::taxonomy ($tm, 'aaa');

#warn Dumper $taxo;

    ok ($taxo->{lid} eq 'tm:aaa',                'taxo 1');
    ok (eq_set([ map { $_->{lid} } @{$taxo->{children}} ],
	       [ 'tm:ccc', 'tm:ddd' ]),          'taxo 2');

    $taxo =  TM::Tau::Tree::taxonomy ($tm);
    ok ($taxo->{lid} eq 'tm:thing',              'taxo 3');
    ok (eq_set([ map { $_->{lid} } @{$taxo->{children}} ],
	       [ 'tm:aaa', 'tm:bbb' ]),          'taxo 4');
    my ($aaa) = grep ($_->{lid} eq 'tm:aaa', @{$taxo->{children}});
    ok (eq_set([ map { $_->{lid} } @{$aaa->{children}} ],
	       [ 'tm:ccc', 'tm:ddd' ]),          'taxo 5');
}

#------------------------------------------------------------------------------

#-------------------------------------------------------------


__END__


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

use TM::PSI;

#== TESTS =====================================================================

require_ok ('TM');


{
    my $tm = new TM;
    ok ($tm->isa ('TM'), 'class');


    is ($tm->baseuri, 'tm://nirvana/', 'baseuri default');

    ok (!$tm->{psis}, 'no junk left');
    ok ($tm->{created}, 'created there');
    ok ($tm->{usual_suspects}, 'usual suspects there');
}

{ # baseuri
    my $tm = new TM (baseuri => 'xxx:yyy');
    is ($tm->baseuri, 'xxx:yyy#', 'baseuri set');

    $tm->baseuri ('xxx');
    is ($tm->baseuri, 'xxx:yyy#', 'baseuri immutable');
}

{ # consistency accessors
    my $tm = new TM;
    ok (eq_set([ $tm->consistency ],
	       [ TM->Subject_based_Merging,
		 TM->Indicator_based_Merging ] ), 'default consistency');

    $tm = new TM (consistency => [ TM->Subject_based_Merging ]);
    ok (eq_set([ $tm->consistency ],
               [ TM->Subject_based_Merging ] ),   'explicit consistency');

    $tm->consistency (TM->Indicator_based_Merging);
    ok (eq_set([ $tm->consistency ],
	       [ TM->Indicator_based_Merging ] ), 'changed consistency');
}


require_ok ('TM::PSI');

{
    is (  scalar (keys %{$TM::PSI::core->{mid2iid}})
	+ scalar (keys %{$TM::PSI::topicmaps_inc->{mid2iid}})
	+ scalar (keys %{$TM::PSI::astma_inc->{mid2iid}})
	, 
	scalar  keys %{$TM::PSI::topicmaps->{mid2iid}}, 'merging topicmaps');
}

{
  my $tm = new TM (psis => {});

  is (0, 
      grep (defined $_, $tm->mids ( keys %{$TM::PSI::topicmaps->{mid2iid}} ))
      ,  'psis: nothing defined');
}

__END__
{ # check predefined
    my $tm = new TM;
#warn Dumper $tm;
    is (grep (!defined $_, $tm->mids (keys %{$TM::PSI::core->{mid2iid}})), 0, 'no undefined iid (core)');
    ok (eq_array ([
		   $tm->mids (qw(thing is-subclass-of isa us))
		   ], 
		  [
		   'tm://nirvana/thing',
		   'tm://nirvana/is-subclass-of',
		   'tm://nirvana/isa',
		   'tm://nirvana/us',
		   ]
		  ), 'found predefined');

    ok (eq_array ([
		   $tm->mids (\ 'http://psi.topicmaps.org/sam/1.0/#type-instance',
			      \ 'http://www.topicmaps.org/xtm/#psi-superclass-subclass')
		   ], 
		  [
		   'tm://nirvana/isa',
		   'tm://nirvana/is-subclass-of',
		   ]
		  ), 'found predefined 2');

    is (scalar $tm->match (TM->FORALL, type => $tm->mids('isa'), iplayer => $tm->mids('assertion-type')),    2, 'assertion-type: all instances');
}

{
    my $tm = new TM (psis => $TM::PSI::topicmaps);
#warn Dumper $tm;
    is (grep (!defined $_, $tm->mids (keys %{$TM::PSI::topicmaps->{mid2iid}})), 0, 'no undefined iid (topicmaps)');
    is (scalar $tm->match(), scalar @{$TM::PSI::topicmaps->{assertions}}, 'same number of assertions (topicmaps)');
}


__END__

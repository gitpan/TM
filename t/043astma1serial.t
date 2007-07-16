use strict;
use warnings;

# change 'tests => 1' to 'tests => last_test_to_print';
use Test::More qw(no_plan);

use Data::Dumper;
$Data::Dumper::Indent = 1;

#== TESTS ===========================================================================
use TM::Materialized::AsTMa;
require_ok( 'TM::Serializable::AsTMa' );

my $tm=TM::Materialized::AsTMa->new(baseuri=>"tm://", inline=>
'nackertes_topic 

atop
bn: just a topic

btop (ctop)
bn: something
bn@ascope: some other thing

ctop
bn: over the top!
in: something
in: somemore
oc: http://somewhere
in@ascope: scoped
in@ascope (sometype): also typed
oc (sometype): http://typedoc
oc @ascope (sometype): http://typedandscopedoc

(sucks-more-than)
sucker: ctop
winner: atop
winner: others

(sucks-more-than) @ascope
sucker: nobody
winner: nobody

thistop reifies atop
bn: reification
sin: http://nowhere.never.ever

(sucks-more-than) is-reified-by atop
winner: nobody
sucker: nobody

');
$tm->sync_in;
my $content=$tm->serialize;
ok($content,"serialize returned something");

# now do the round trip
my $rt=TM::Materialized::AsTMa->new(baseuri=>"tm://", inline=>$content);
$rt->sync_in;
ok($rt->is_a($rt->mids("atop"),$rt->mids("thing")),"serialized stuff is parseable AsTMa");

# check that the topics/reification info has survived
my @otopics=sort $tm->midlets;
my @ntopics=sort $rt->midlets;
ok(eq_array(\@otopics,\@ntopics),"all topics have survived");

# and the rest: topic-chars and associations
my @oass=sort { $a->[TM->LID] cmp $b->[TM->LID] } $tm->match(TM->FORALL);
my @nass=sort { $a->[TM->LID] cmp $b->[TM->LID] } $rt->match(TM->FORALL);
ok(eq_array(\@oass,\@nass),"all assertions have survived");

# test omission options
$content=$tm->serialize(omit_trivia=>1);
ok($content,"serialize with options returns something");
ok($content!~/nackertes_topic/,"suppression of naked topics works");

# time for some destruction!
# reified bn or oc: no go in AsTMa 1.
my @stuff=$rt->match(TM->FORALL,char=>1,topic=>$rt->mids("ctop")); # topic must have 1 or more bn and oc
for my $nogo (TM->NAME,TM->OCC)
{
    my $thing=$rt->midlet((map { $_->[TM->LID] } (grep $_->[TM->KIND] == $nogo, @stuff))[0]);
    $thing->[TM->ADDRESS]="http://subject";
    eval 
    {
	$content=$rt->serialize;
    }; 
    ok($@,"serialize throws exception on non-AsTMa-1 construct");
}

use strict;
use warnings;

use Test::More qw(no_plan);

use TM;
use Class::Trait;
use Data::Dumper;
$Data::Dumper::Indent = 1;

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}

my $warn = shift @ARGV;
unless ($warn) {
    close STDERR;
    open (STDERR, ">/dev/null");
    select (STDERR); $| = 1;
}

use constant DONE => 0;
#-------------------------------------------------------

use_ok ('TM::Serializable::CSV');

if (DONE) { # 
    my $content = q|association-type,location,bio-unit
is-born,gold-coast,rumsti
is-born,vienna,ramsti
|;

    my $tm = new TM (baseuri=>"tm:");
    Class::Trait->apply ($tm, "TM::Serializable::CSV");
    $tm->deserialize ($content);
#warn Dumper $tm;
    is ($tm->tids ('rumsti'), 'tm:rumsti', 'topic found');
    is ($tm->tids ('ramsti'), 'tm:ramsti', 'topic found');
    is (scalar $tm->match (TM->FORALL, type => 'tm:is-born', arole => 'tm:location', aplayer => 'tm:gold-coast', 
			                                     brole => 'tm:bio-unit', bplayer => 'tm:rumsti'), 1, 'assoc 1');
    is (scalar $tm->match (TM->FORALL, type => 'tm:is-born', arole => 'tm:location', aplayer => 'tm:vienna',
			                                     brole => 'tm:bio-unit', bplayer => 'tm:ramsti'), 1, 'assoc 2');

     $content = q|bio-unit,association-type,location
remsti,is-born,gold-coast
rimsti,is-born,vienna
|;
    $tm->deserialize ($content);

#warn Dumper $tm;
    is ($tm->tids ('remsti'), 'tm:remsti', 'topic found');
    is ($tm->tids ('rimsti'), 'tm:rimsti', 'topic found');
    is (scalar $tm->match (TM->FORALL, type => 'tm:is-born', arole => 'tm:location', aplayer => 'tm:gold-coast', 
			                                     brole => 'tm:bio-unit', bplayer => 'tm:remsti'), 1, 'assoc 1');
    is (scalar $tm->match (TM->FORALL, type => 'tm:is-born', arole => 'tm:location', aplayer => 'tm:vienna',
			                                     brole => 'tm:bio-unit', bplayer => 'tm:rimsti'), 1, 'assoc 2');


}

if (1||DONE) {
    my $content = q|name,id,location,homepage
"Rumsti",rumsti,gold-coast,http://rumsti.com
"Ramsti",ramsti,vienna,http://ramsti.com
|;

    my $tm = new TM (baseuri=>"tm:");
    Class::Trait->apply ($tm, "TM::Serializable::CSV");
    $tm->deserialize ($content);
#warn Dumper $tm;
    is ($tm->tids ('rumsti'), 'tm:rumsti', 'topic found');
    is ($tm->tids ('ramsti'), 'tm:ramsti', 'topic found');

    ok (eq_array ([
	            sort
	            map {  $_->[TM->PLAYERS]->[0] }
	            $tm->match (TM->FORALL, type => 'name') ],
		  [
		   'tm:ramsti',
		   'tm:rumsti'
		  ]), 'name owners');
    ok (eq_array ([
	            sort
	            map {  $_->[TM->PLAYERS]->[1]->[0] }
	            $tm->match (TM->FORALL, type => 'name') ],
		  [
		   'Ramsti',
		   'Rumsti'
		  ]), 'names');

    ok (eq_array ([
	            sort
	            map {  $_->[TM->PLAYERS]->[0] }
	            $tm->match (TM->FORALL, type => 'tm:location') ],
		  [
		   'tm:ramsti',
		   'tm:rumsti'
		  ]), 'location owners');
    ok (eq_array ([
	            sort
	            map {  $_->[TM->PLAYERS]->[1]->[0] }
	            $tm->match (TM->FORALL, type => 'tm:location') ],
		  [
		   'gold-coast',
		   'vienna'
		  ]), 'locations');

    ok (eq_array ([
	            sort
	            map {  $_->[TM->PLAYERS]->[0] }
	            $tm->match (TM->FORALL, type => 'tm:homepage') ],
		  [
		   'tm:ramsti',
		   'tm:rumsti'
		  ]), 'homepage owners');
    ok (eq_array ([
	            sort
	            map {  $_->[TM->PLAYERS]->[1]->[0] }
	            $tm->match (TM->FORALL, type => 'tm:homepage') ],
		  [
		   'http://ramsti.com',
		   'http://rumsti.com'
		  ]), 'homepages');
    use TM::Literal;
    ok (eq_array ([
	            sort
	            map {  $_->[TM->PLAYERS]->[1]->[1] }
	            $tm->match (TM->FORALL, type => 'tm:homepage') ],
		  [
		   TM::Literal->URI,
		   TM::Literal->URI
		  ]), 'homepages types');


}

__END__



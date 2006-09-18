package main;

use Log::Log4perl;
Log::Log4perl->init("t/log.conf");
our $log = Log::Log4perl->get_logger("TM");

1;

use strict;

# change 'tests => 1' to 'tests => last_test_to_print';
use Test::More qw(no_plan);

use Data::Dumper;
$Data::Dumper::Indent = 1;

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}

use TM;
use TM::PSI;

sub _parse {
  my $text = shift;
  my $ms = new TM (baseuri => 'tm:', psis => $TM::PSI::topicmaps);
  my $p  = new TM::AsTMa::Fact (store => $ms);
  my $i  = $p->parse ($text);
  return $ms;
}

sub _q_players {
    my $ms = shift;
#    my @res = $ms->match (TM->FORALL, @_);
#    warn "res no filter ".Dumper \@res;
    my @res = grep ($_ !~ m|^tm:|, map { ref($_) ? $_->[0] : $_ } map { @{$_->[TM->PLAYERS]} } $ms->match (TM->FORALL, @_));
#    warn "res ".Dumper \@res;
    return \@res;
}

##===================================================================================


#== TESTS ===========================================================================

require_ok( 'TM::AsTMa::Fact' );

my $npa = @{$TM::PSI::topicmaps->{assertions}};
my $npt = $npa + keys %{$TM::PSI::topicmaps->{mid2iid}};


{ # class ok
    my $p = new TM::AsTMa::Fact;
    ok (ref($p) eq 'TM::AsTMa::Fact', 'class ok');
}

{ #-- structural
    my $ms = _parse ('aaa (bbb)
');
#warn Dumper $ms;
    is (scalar $ms->match (TM->FORALL, type => 'tm:isa', arole => 'tm:instance', aplayer => 'tm:aaa', 
			                                 brole => 'tm:class',    bplayer => 'tm:bbb'), 1, 'one type for aaa');
    ok (eq_array ([
		   $ms->mids ('aaa', 'bbb')
		   ],
		  [
		   'tm:aaa', 'tm:bbb'
		   ]), 'aaa, bbb internalized');
}

{
    my $ms = _parse ('aaa
');
#warn Dumper $ms;
    is ($ms->mids ('aaa'), 'tm:aaa', 'aaa implicitely internalized');
}

{ # structural topic
  my $ms = _parse (q|
aaa is-a bbb
bn: AAA
oc: http://BBB
in: blabla bla

|);
#warn Dumper $ms;
  is (scalar $ms->match (TM->FORALL, type => 'tm:isa',        irole => 'tm:instance', iplayer => 'tm:aaa' ), 1, 'one type for aaa');
  is (scalar $ms->match (TM->FORALL,                          irole => 'tm:thing',    iplayer => 'tm:aaa' ), 4, 'chars for aaa');
  is (scalar $ms->match (TM->FORALL, type => 'tm:name',       irole => 'tm:thing',    iplayer => 'tm:aaa' ), 1, 'basenames for aaa');
  is (scalar $ms->match (TM->FORALL, type => 'tm:occurrence', irole => 'tm:thing',    iplayer => 'tm:aaa' ), 2, 'occurrences for aaa 1');
}

#-- syntactic issues ----------------------------------------------------------------

{
  my $ms = _parse (q|
# this is AsTMa

|);
#warn Dumper $ms;
  is (scalar $ms->match(), $npa, 'empty map 1 (assertions)');
  is ($ms->midlets,        $npt, 'empty map 2 (toplets)');
}

{ # empty line with blanks
  my $ms = _parse (q|
topic1
   
topic2

|);
##warn Dumper $ms;
  is (scalar $ms->midlets(), $npt+2, 'empty line contains blanks');
}

{ # empty lines with \r
    my $ms = _parse (q|
topic1
topic2
topic3
|);

    is (scalar $ms->midlets(), $npt+3, 'empty line \r contains blanks');
}

{ # using TABs as separators
    my $ms = _parse (q|
topic1	(	topic2	)
	# comment
|);
#warn Dumper $ms;
    is (scalar $ms->midlets, $npt+2+1, 'using TABs as separators');
}

{
  my $ms = _parse (q|
# comment1

aaa (bbbbb cccc dddd)

#comment2

#comment4
ccc (bbb)
#comment3
#comment4
ddd (xxxx)
#comment5
|);
##warn Dumper $ms;

  is (scalar $ms->midlets, $npt+8+5, 'test comment/separation');
  is (scalar $ms->match (TM->FORALL, type => 'tm:isa', irole => 'tm:instance', iplayer => 'tm:aaa' ), 3, 'types for aaa');
  is (scalar $ms->match (TM->FORALL, type => 'tm:isa', irole => 'tm:instance', iplayer => 'tm:ccc' ), 1, 'type  for ccc');
  is (scalar $ms->match (TM->FORALL, type => 'tm:isa', irole => 'tm:instance', iplayer => 'tm:ddd' ), 1, 'type  for ddd');
}

{ # line continuation with comments
    my $ms = _parse (q|

topic1
# comment \
topic2

|);
    is (scalar $ms->midlets, $npt+1, 'continuation in comment');
}

{ # line continuation with comments
    my $ms = _parse (q|

topic1
# comment \

topic2

|);
    is (scalar $ms->midlets, $npt+2, 'continuation in comment, not 1');
}

{ # line continuation with comments
    my $ms = _parse (q|
topic1
# comment \ 
topic2

|);
    is (scalar $ms->midlets, $npt+2, 'continuation in comment, not 2');
}

{ # line continuation
  my $ms = _parse (q|
aaa (bbbbb \
cccc \
dddd)

|
);
  is (scalar $ms->midlets, $npt+7, 'line continuation');
  is (scalar $ms->match (TM->FORALL, type => 'tm:isa', irole => 'tm:instance', iplayer => 'tm:aaa' ), 3, 'types for aaa');
}

{ # line continuation, not
  my $ms = _parse (q|
aaa
 bn: AAA
 in: a \ within the text is ok
 in: also one with a \\ followed by a blank: \\ 
 in: this is a new one \\
 in: this is not a new one
|);
##warn Dumper $ms;

  my @res = $ms->match (TM->FORALL, type => 'tm:occurrence', irole => 'tm:thing', iplayer => 'tm:aaa' );
  is (scalar @res, 3, 'ins for aaa');
##warn Dumper \@res;
##warn Dumper [ map { ${$_->[TM->PLAYERS]->[1]}} @res ];
  ok (eq_set ([ 
		map { $_->[0] }
		map { $_->[TM->PLAYERS]->[1] } @res ],
	      [ 'a \ within the text is ok',
		'also one with a \ followed by a blank: \\',   # blank is gone now
		'this is a new one  in: this is not a new one']), 'same text');
}

{ # line continuation, not \\
  my $ms = _parse (q|
aaa (bbbb \
) # this is a continuation
bn: but not this \\\\
in: should be separate

|
);
##warn Dumper $ms;
  is (scalar $ms->match, $npa+3, 'line continuation, =3');
}

{ # string detection
  my $ms = _parse (q|
aaa
in: AAA

bbb
in: <<<
xxxxxxxxxxxxx
yyyyyyyyyy
zzzzzz
<<<

ccc
in: <<EOM
rumsti
ramsti
romsti
<<EOM

|);
##  warn Dumper $ms;
  is (scalar $ms->match, $npa+3, 'string detection');
  my @res = $ms->match (TM->FORALL, type => 'tm:occurrence', irole => 'tm:thing', iplayer => 'tm:bbb' );
  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } @res ],
	      [ 'xxxxxxxxxxxxx
yyyyyyyyyy
zzzzzz',
		]), 'same text [<<<]');

  @res = $ms->match (TM->FORALL, type => 'tm:occurrence', irole => 'tm:thing', iplayer => 'tm:ccc' );
  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } @res ],
	      [ 'rumsti
ramsti
romsti',
		]), 'same text [<<EOM]');
}

#-- line separation -----------------------------------------------

{ # line separation
  my $ms = _parse (q|
aaa (bbb) ~ bn: AAA ~ in: rumsti

ccc (ddd) ~ bn: CCC
|);
##  warn Dumper $ms;

  is (scalar $ms->match, $npa+5, '~ separation: assertion');
  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:name', iplayer => 'tm:aaa' ) ] ,
	      [ 'AAA' ]), '~ separation: AAA basename');
}

{ # line no separation
  my $ms = _parse (q|
aaa (bbb) ~ bn: AAA ~ in: rumsti is using ~~ in: text
|);
##  warn Dumper $ms;
  is (scalar $ms->match, $npa+3, '~~ no-separation: assertions');
  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL,  type => 'tm:occurrence', iplayer => 'tm:aaa' ) ] ,
	      [ 'rumsti is using ~ in: text' ]), 'getting back ~ text');
}

{ # inline comments
  my $ms = _parse (q|
aaa
bn: AAA  # comment
bn: AAA# no-comment
oc: http://rumsti#no-comment
|);
##  warn Dumper $ms;

  is (scalar $ms->match, $npa+3, 'comment + assertions');
  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:name', iplayer => 'tm:aaa' ) ] ,
	      [ 'AAA',
		'AAA# no-comment' ]), 'getting back commented basename');
  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:occurrence', iplayer => 'tm:aaa' ) ] ,
	      [ 'http://rumsti#no-comment' ]), 'getting back commented occ');
}

#-- structural: assocs ----------------------------------------------------------

{
    my $ms = _parse (q|
(xxx)
role : player

|);
##warn Dumper $ms;

  is (scalar $ms->match,                                                                               $npa+1, 'basic association');
  is (scalar $ms->match (TM->FORALL,                                       iplayer => 'tm:player' ), 1, 'finding basic association 1');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx',                     iplayer => 'tm:player' ), 1, 'finding basic association 2');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx', irole => 'tm:role', iplayer => 'tm:player' ), 1, 'finding basic association 3');
}

{
  my $ms = _parse (q|
(xxx)
role : p1 p2 p3
|);
##  warn Dumper $ms;

  is (scalar $ms->match,                                                                           $npa+1, 'basic association');
  is (scalar $ms->match (TM->FORALL,                                       iplayer => 'tm:p1' ), 1, 'finding basic association 4');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx',                     iplayer => 'tm:p2' ), 1, 'finding basic association 5');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx', irole => 'tm:role', iplayer => 'tm:p3' ), 1, 'finding basic association 6');
}

{
  my $ms = _parse (q|
(xxx)
  role : aaa bbb

(xxx)
  role : aaa

|);
##  warn Dumper $ms;

  is (scalar $ms->match,                                                                            $npa+2, 'basic association');
  is (scalar $ms->match (TM->FORALL,                                       iplayer => 'tm:aaa' ), 2, 'finding basic association 7');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx',                     iplayer => 'tm:bbb' ), 1, 'finding basic association 8');
}

{
  my $ms = _parse (q|
(xxx)
  role1 : aaa bbb
  role2 : ccc

|);
##warn Dumper $ms;

  is (scalar $ms->match, $npa+1, 'basic association');
  is (scalar $ms->match (TM->FORALL,                                        iplayer => 'tm:aaa' ), 1, 'finding basic association 10');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx',                      iplayer => 'tm:ccc' ), 1, 'finding basic association 11');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx', irole => 'tm:role2', iplayer => 'tm:ccc' ), 1, 'finding basic association 12');
}

{
  my $ms = _parse (q|
(aaa) @ sss
  role : player

|);
#warn Dumper $ms;

#  ok ($ms->is_subclass ('aaa', 'association'), 'association: subclassed');
# is (scalar $ms->match (TM->FORALL, type=> 'isa',                        iplayer => 'tm:sss'    ),   1, 'association scoped 1');

  is (scalar $ms->match, $npa+2, 'association scoped');
  is (scalar $ms->match (TM->FORALL,                                      iplayer => 'tm:player' ),   1, 'association scoped 2');
  is (scalar $ms->match (TM->FORALL, scope => 'tm:sss',                   iplayer => 'tm:player' ),   1, 'association scoped 3');
}

#-- reification --------------------------------------

{
  my $ms = _parse (q|
http://rumsti.com/ is-a website

urn:x-rumsti:xxx is-a rumsti
|);
#warn Dumper $ms;

  ok (eq_array ([
		 $ms->mids ('http://rumsti.com/','urn:x-rumsti:xxx')
		 ],
		[
		 'tm:uuid-0000000000', 'tm:uuid-0000000001'
		 ]),
		'reification: identifiers');
  is (scalar $ms->match, $npa+2, 'external reification: association');
  is (scalar $ms->match (TM->FORALL,                                       iplayer => 'tm:uuid-0000000001' ), 1, 'reification: finding');
  is (scalar $ms->match (TM->FORALL,                     type => 'tm:isa', iplayer => 'tm:uuid-0000000000' ), 1, 'finding basic association');
}

{
  my $ms = _parse (q|
cpan reifies http://cpan.org/

(xxx)
aaa: cpan
bbb: ccc

|);
#warn Dumper $ms;

  is (scalar $ms->match, $npa+1,                                                                                        'reification: association');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx',               iplayer => $ms->mids ('http://cpan.org/') ), 1, 'reification: finding basic association');
  is (scalar $ms->match (TM->FORALL, type => 'tm:xxx',               iplayer => 'tm:cpan' ),  1, 'reification: finding basic association');

  ok (eq_set (
	      [ $ms->match (TM->FORALL, type => 'tm:xxx',            iplayer => $ms->mids ('http://cpan.org/') ) ],
	      [ $ms->match (TM->FORALL, type => 'tm:xxx',            iplayer => 'tm:cpan' )          ]
	      ), 'reification: finding, same');
}

{
  my $ms = _parse (q|
(http://xxx)
  http://role1 : aaa http://bbb
  http://role2 : ccc
|);
#warn Dumper $ms;
  is (scalar $ms->match, $npa+1, 'reification: association');
  is (scalar $ms->match (TM->FORALL, type    =>   $ms->mids('http://xxx'), 
			             roles   => [ $ms->mids ('http://role1', 'http://role2', 'http://role1') ],
			             players => [ $ms->mids ('tm:aaa', undef, 'http://bbb') ] ), 1, 'reification: association');
}

{ # reification explicit
  my $ms = _parse (q|
xxx (http://www.topicmaps.org/xtm/1.0/#psi-topic)
|);
#warn Dumper $ms;
  is (scalar $ms->match, $npa+1, 'reification: type');

  ok ($ms->is_asserted (Assertion->new (scope   => 'tm:us',
					type    => 'tm:isa',
					roles   => [ 'tm:class', 'tm:instance' ],
					players => [ 'http://www.topicmaps.org/xtm/1.0/#psi-topic', 'tm:xxx' ])), 'xxx is-a found');
  my $m = $ms->mids ('http://www.topicmaps.org/xtm/1.0/#psi-topic');
  ok ($ms->is_asserted (Assertion->new (scope   => 'tm:us',
					type    => 'tm:isa',
					roles   => [ 'tm:class', 'tm:instance' ],
					players => [ $m, 'tm:xxx' ])), 'xxx is-a found (via mids)');
}

{
  my $ms = _parse (q|
(xxx) is-reified-by aaa
  role : player
|);
#warn Dumper $ms;
  my ($a) = $ms->match (TM->FORALL, type => 'tm:xxx');
  is ($ms->reified_by ('tm:aaa'), $a->[TM->LID], 'assoc reified: regained');
  is ($ms->reified_by ('tm:xxx'), undef,                'assoc reified: regained 2');
}

eval {
  my $ms = _parse (q|
(xxx) reifies aaa
  role : player
|);
}; like ($@, qr/must be a URI/i, _chomp($@));

{
  my $ms = _parse (q|
(xxx) reifies http://rumsti/
  role : player
|);
#warn Dumper $ms;

  my ($a) = $ms->match (TM->FORALL, type => 'tm:xxx');
  is ($ms->reified_by ($a->[TM->LID]), 'http://rumsti/', 'assoc reified: regained 3');
}

eval {
  my $ms = _parse (q|
(xxx) is-reified-by http://aaa/
  role : player
|);
}; like ($@, qr/local identifier/i, _chomp($@));

#-- syntax errors

eval {
  my $ms = _parse (q|
(xxx zzz)
member : aaa
|);
}; like ($@, qr/syntax error/i, _chomp($@));

eval {
  my $ms = _parse (q|
(xxx)
|);
}; like ($@, qr/syntax error/i, _chomp($@));

eval {
  my $ms = _parse (q|
(xxx)
role : aaa
role2 : 
|);
}; like ($@, qr/syntax error/i, _chomp($@));

eval {
  my $ms = _parse (q|
(xxx)

rumsti

|);
}; like ($@, qr/syntax error/i, _chomp($@));

eval {
  my $ms = _parse (q|
()
role : player
|);
}; like ($@, qr/syntax error/i, _chomp($@));

#-- autogenerating ids

{
  my $ms = _parse (q|
* (aaa)

* (aaa)
|);
## warn Dumper $ms;

  is (scalar $ms->match, $npa+2, 'autogenerating ids');
  is (scalar (
              grep /tm:uuid-\d{10}/, 
	      map {$_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:isa', iplayer => 'tm:aaa' ) ), 2, 'generated ids ok');
}

#-- structural: midlets/characteristics -----------------------------------------

#- negative tests

eval {
   my $ms = _parse (q|
ttt
bn:    
|);
warn Dumper $ms;
}; ok ($@, "raises except on empty bn:");

eval {
   my $ms = _parse (q|
ttt
oc: 
|);
}; ok ($@, "raises except on empty oc:");

eval {
   my $ms = _parse (q|
ttt
in: 
|);
}; ok ($@, "raises except on empty in:");

eval {
   my $ms = _parse (q|
(aaa)
aaa :
|);
   fail ("raises except on empty role");
}; ok ($@, "raises except on empty role");

eval {
   my $ms = _parse (q|
(aaa)
aaa:bbb
|);
fail ("raises except on empty role 2");
}; ok ($@, "raises except on empty role 2");

eval {
   my $ms = _parse (q|
(ddd)
bbb:aaa:ccc
|);
fail ("raises except on empty role 3");
}; ok ($@, "raises except on empty role 3");


eval {
   my $ms = _parse (q|
aaa
sin (ttt): urn:xxx
|);
fail ("raises except on subject indicator");
}; ok ($@, "raises except on subject indicator");

eval {
   my $ms = _parse (q|
aaa
sin @ sss : urn:xxx
|);
fail ("raises except on subject indicator");
}; ok ($@, "raises except on subject indicator");

#-- positive tests -----------------------------------

{
  # testing toplets with characteristics
  my $ms = _parse (q|
xxx
bn: XXX
|);
##warn Dumper $ms;

  is (scalar $ms->match (TM->FORALL, type => 'tm:name', roles => [ 'tm:value', 'tm:thing' ], players => [ undef, 'tm:xxx' ]), 1, 'basename characteristics');
}

{
  # testing toplets with URI
  my $ms = _parse (q|
http://xxx
bn: XXX
|);
##warn Dumper $ms;

  is (scalar $ms->match (TM->FORALL, type => 'tm:name', roles => [ 'tm:value', 'tm:thing' ], 
			                                players => [ $ms->mids (undef, 'http://xxx') ]), 1, 'basename characterisistics (reification)');
}

{
my $ms = _parse (q|
aaa (bbbbb)
bn: AAA
in:         blabla  
|);
##warn Dumper $ms;

  ok (eq_set ([ map { map { $_->[0] } $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:occurrence', iplayer => 'tm:aaa' ) ] ,
	      [ 'blabla' ]), 'test blanks in resourceData 1');
}

{
  my $ms = _parse (q|
xxx
bn: XXX
oc: http://xxx.com
ex: http://yyy.com
|);
##warn Dumper $ms;

  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:occurrence', iplayer => 'tm:xxx' ) ] ,
	      [ 'http://yyy.com', 'http://xxx.com' ]), 'occurrence char, value ok');
}


#- adding types

{
  my $ms = _parse (q|
aaa
 bn: AAA
 bn (rumsti) : AAAT
 in: III
 in (bumsti) : IIIT
 oc: http://xxx/
 oc (ramsti) : http://xxxt/
 oc (rimsti) : http://yyy/
 bn (remsti) : http://zzz/
 in (remsti) : bla
|);
#warn Dumper $ms;
#warn "occurrences of aaa ".Dumper [ $ms->match (TemplateIPlayerType->new ( type => 'tm:occurrence',   iplayer => 'tm:aaa' )) ];

  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } 
                grep ($_->[TM->TYPE] eq 'tm:name', 
                      $ms->match (TM->FORALL, type => 'tm:name',   iplayer => 'tm:aaa' )) ] ,
	      [ 'AAA' ]), 'basename untyped char, value ok');

  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:rumsti',         iplayer => 'tm:aaa' ) ] ,
	      [ 'AAAT' ]), 'basename typed char, value ok');

  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:occurrence', iplayer => 'tm:aaa' ) ] ,
	      [ 'http://xxxt/',
		'http://yyy/',
		'http://zzz/', # yes, this is also now an occurrence, since remsti is that too!
		'III',
		'IIIT',
		'bla',
		'http://xxx/' ]), 'occurr typed char, value ok');

  ok (eq_set ([ map { $_->[0] } map { $_->[TM->PLAYERS]->[1] } $ms->match (TM->FORALL, type => 'tm:bumsti',         iplayer => 'tm:aaa' ) ] ,
	      [ 'IIIT' ]), 'occurr typed char, value ok');
  ok (eq_set (_q_players ($ms, type => 'tm:ramsti',         iplayer => 'tm:aaa' ) ,
	      [ 'http://xxxt/' ]), 'occurr typed char, value ok');
  ok (eq_set (_q_players ($ms, type => 'tm:remsti',         iplayer => 'tm:aaa' ) ,
	      [ 
		'http://zzz/',
		'bla' ]), 'occurr typed char, value ok');
}

{ # subject indication 
    my $ms = _parse (q|
aaa
bn: AAA
sin: http://AAA
sin: http://BBB

|);
#warn Dumper $ms;

    my $t = $ms->midlet ('tm:aaa');
    ok (eq_set (
		$t->[TM->INDICATORS],
		[
		 'http://AAA',
		 'http://BBB',
		 ]), 'indicators');

    is (scalar $ms->match (TM->FORALL, type => 'tm:name',        irole => 'tm:thing',    iplayer => $ms->mids (\ 'http://AAA') ), 1, 'names for aaa via indication');
    is (scalar $ms->match (TM->FORALL, type => 'tm:name',        irole => 'tm:thing',    iplayer => $ms->mids (\ 'http://BBB') ), 1, 'names for aaa via indication');
}

#-- associations with URIs

{
  my $ms = _parse (q|
(aaa)
aaa:bbb : ccc

(ddd)
bbb: aaa:ccc
|);
##  warn Dumper $ms;

  ok (eq_set ([ map { $_->[TM->PLAYERS]->[0] } $ms->match (TM->FORALL, type => 'tm:aaa',         irole => $ms->mids ('aaa:bbb') ) ] ,
	      [ 'tm:ccc' ]), 'assoc with URIs 1');
  ok (eq_set ([ map { $_->[TM->PLAYERS]->[0] } $ms->match (TM->FORALL, type => 'tm:ddd',         irole => 'tm:bbb' ) ] ,
	      [ $ms->mids ('aaa:ccc') ]), 'assoc with URIs 2');

}

#- adding scopes

{
  my $ms = _parse (q|
aaa
 bn: AAA
 bn @ sss : AAAS
 in: III
 in @ sss : IIIS
 oc: http://xxx/
 oc @ sss : http://xxxs/
|);
##  warn Dumper $ms;

  ok (eq_set (_q_players ($ms, type => 'tm:name',   iplayer => 'tm:aaa' ),
	      [ 'AAA', 'AAAS' ]), 'basename untyped, scoped, value ok');
  ok (eq_set (_q_players ($ms, scope => 'tm:us', type => 'tm:name',   iplayer => 'tm:aaa' ),
	      [ 'AAA' ]), 'basename untyped, scoped, value ok');
  ok (eq_set (_q_players ($ms, scope => 'tm:sss', type => 'tm:name',   iplayer => 'tm:aaa' ),
	      [ 'AAAS' ]), 'basename untyped, scoped, value ok');

  ok (eq_set (_q_players ($ms, type => 'tm:occurrence',   iplayer => 'tm:aaa' ),
	      [ 'III', 'IIIS', 'http://xxx/', 'http://xxxs/' ]), 'occurrences untyped, mixscoped, value ok');
  ok (eq_set (_q_players ($ms, scope => 'tm:sss', type => 'tm:occurrence',   iplayer => 'tm:aaa' ),
	      [ 'IIIS', 'http://xxxs/' ]), 'occurrences untyped, scoped, value ok');
}

{ # typed and scoped characteristics
  my $ms = _parse (q|
aaa
 bn (ramsti): AAA
 bn @ sss (rumsti): AAAS
 in: III
 in @ sss (ramsti): IIIS
 oc: http://xxx/
 oc @ sss (ramsti): http://xxxs/

xxx (yyy)
|);
#  warn Dumper $ms;

  ok (eq_set (_q_players ($ms, type => 'tm:ramsti',   iplayer => 'tm:aaa' ),
	      [ 'AAA', 'IIIS', 'http://xxxs/' ]), 'basename typed, mixscoped, value ok');
  ok (eq_set (_q_players ($ms, scope => 'tm:us', type => 'tm:ramsti',   iplayer => 'tm:aaa' ),
	      [ 'AAA' ]), 'basename untyped, scoped, value ok');
  ok (eq_set (_q_players ($ms, scope => 'tm:sss', type => 'tm:rumsti',   iplayer => 'tm:aaa' ),
	      [ 'AAAS' ]), 'basename untyped, scoped, value ok');

  ok (eq_set (_q_players ($ms, type => 'tm:name',   iplayer => 'tm:aaa' ),
	      [   'http://xxxs/',  'AAA',  'IIIS',  'AAAS' ]), 'basenames typed, mixscoped, value ok');
  ok (eq_set (_q_players ($ms, type => 'tm:occurrence',   iplayer => 'tm:aaa' ),
	      [ 'http://xxxs/',  'http://xxx/', 'AAA',  'IIIS',  'III' ]), 'occurrences typed, mixscoped, value ok');
  ok (eq_set (_q_players ($ms, kind => TM->OCC, type => 'tm:occurrence',   iplayer => 'tm:aaa' ),
	      [ 'http://xxx/', 'http://xxxs/', 'IIIS',  'III' ]), 'occurrences untyped, mixscoped, value ok');
}

#-- inlined

{ # checking inlined subclassing
  my $ms = _parse (q|
aaa is-subclass-of bbb

(is-subclass-of)
 superclass: ddd
 subclass: ccc

eee is-subclass-of fff is-subclass-of ggg

hhh subclasses iii is-subclass-of jjj

|);
##warn Dumper $ms;

  is (scalar $ms->match(TM->FORALL, type => 'tm:is-subclass-of', roles => [ 'tm:subclass', 'tm:superclass' ], players => [ 'tm:aaa', 'tm:bbb' ] ), 1, 'intrinsic is-subclass-of, different forms 1');

  is (scalar $ms->match(TM->FORALL, type => 'tm:is-subclass-of', roles => [ 'tm:subclass', 'tm:superclass' ], players => [ 'tm:ccc', 'tm:ddd' ] ), 1, 'intrinsic is-subclass-of, different forms 2');

  is (scalar $ms->match(TM->FORALL, type => 'tm:is-subclass-of', roles => [ 'tm:subclass', 'tm:superclass' ], players => [ 'tm:eee', 'tm:fff' ] ), 1, 'intrinsic is-subclass-of, different forms 3');

  is (scalar $ms->match(TM->FORALL, type => 'tm:is-subclass-of', roles => [ 'tm:subclass', 'tm:superclass' ], players => [ 'tm:eee', 'tm:ggg' ] ), 1, 'intrinsic is-subclass-of, different forms 4');

  is (scalar $ms->match(TM->FORALL, type => 'tm:is-subclass-of', roles => [ 'tm:subclass', 'tm:superclass' ], players => [ 'tm:hhh', 'tm:iii' ] ), 1, 'intrinsic is-subclass-of, different forms 5');

  is (scalar $ms->match(TM->FORALL, type => 'tm:is-subclass-of', roles => [ 'tm:subclass', 'tm:superclass' ], players => [ 'tm:hhh', 'tm:jjj' ] ), 1, 'intrinsic is-subclass-of, different forms 6');

}

{
  my $ms = _parse (q|
aaa

bbb is-a thing

bbb is-a ccc

ddd (  )

eee is-a bbb is-a ccc is-a ddd

xxx has-a aaa
|);
##warn Dumper $ms;

  is (scalar $ms->match(TM->FORALL, type => 'tm:isa', roles => [ 'tm:class', 'tm:instance'  ], players => [ 'tm:xxx',   'tm:aaa' ] ), 1, 'explicit is-a');
  is (scalar $ms->match(TM->FORALL, type => 'tm:isa', roles => [ 'tm:class', 'tm:instance'  ], players => [ 'tm:ccc',   'tm:bbb' ] ), 1, 'explicit is-a 2');

  is (scalar $ms->match(TM->FORALL, type => 'tm:isa', roles => [ 'tm:class', 'tm:instance'  ], players => [ 'tm:ddd',   'tm:eee' ] ), 1, 'explicit is-a 3');
  is (scalar $ms->match(TM->FORALL, type => 'tm:isa', roles => [ 'tm:class', 'tm:instance'  ], players => [ 'tm:ccc',   'tm:eee' ] ), 1, 'explicit is-a 4');
  is (scalar $ms->match(TM->FORALL, type => 'tm:isa', roles => [ 'tm:class', 'tm:instance'  ], players => [ 'tm:bbb',   'tm:eee' ] ), 1, 'explicit is-a 5');
}

#-- templates --------------------

eval {
   my $ms = _parse (q|
xxx bbb zzz

|);
}; ok ($@, "raises except on undefined inline assoc");

{
  my $ms = _parse (q|
[ (bbb)
ccc: ddd
eee: fff  ]

xxx bbb zzz

uuu bbb vvv

|);
#warn Dumper $ms;
  is (scalar $ms->match(TM->FORALL, type => 'tm:bbb', roles => [ 'tm:ccc', 'tm:eee'  ], players => [ 'tm:ddd',   'tm:fff' ] ), 1, 'template: static');
}

{
  my $ms = _parse (q|
[ (bbb)
ccc: http://psi.tm.bond.edu.au/astma/1.0/#psi-left
eee: fff  ]

xxx bbb zzz

[ (bbb2)
ccc: http://psi.tm.bond.edu.au/astma/1.0/#psi-left
eee: http://psi.tm.bond.edu.au/astma/1.0/#psi-right  ]

xxx bbb2 zzz

[ (bbb3)
http://psi.tm.bond.edu.au/astma/1.0/#psi-left : ccc
http://psi.tm.bond.edu.au/astma/1.0/#psi-right : eee  ]

xxx bbb3 zzz

|);

#warn Dumper $ms;
  is (scalar $ms->match(TM->FORALL, type => 'tm:bbb',  roles => [ 'tm:ccc', 'tm:eee'  ], players => [ 'tm:xxx',   'tm:fff' ] ), 1, 'template: dyn left');
  is (scalar $ms->match(TM->FORALL, type => 'tm:bbb2', roles => [ 'tm:ccc', 'tm:eee'  ], players => [ 'tm:xxx',   'tm:zzz' ] ), 1, 'template: dyn both, players');
  is (scalar $ms->match(TM->FORALL, type => 'tm:bbb3', roles => [ 'tm:xxx', 'tm:zzz'  ], players => [ 'tm:ccc',   'tm:eee' ] ), 1, 'template: dyn both, roles');
}

#-- scopes as dates

{
  my $ms = _parse (q|
aaa
 bn : AAA
 bn @ 2004-01-12 : XXX
 bn @ 2004-01-12 12:23 : YYY
|);
#  warn Dumper $ms;

  ok (eq_set (_q_players ($ms, scope => $ms->mids ('urn:x-date:2004-01-12:00:00'), type => 'tm:name',   iplayer => 'tm:aaa' ),
	      [ 'XXX' ]), 'date scoped 1');
  ok (eq_set (_q_players ($ms, scope => $ms->mids ('urn:x-date:2004-01-12:12:23'), type => 'tm:name',   iplayer => 'tm:aaa' ),
	      [ 'YYY' ]), 'date scoped 2');

}

#-- directives ------------------------------------------------------------

#-- encoding

{ #-- default
  my $ms = _parse (q|
aaa
in: Ich chan Glaas ässe, das tuet mir nöd weeh

bbb
in: Mohu jíst sklo, neublí?í mi


|);

  ok (eq_set (_q_players ($ms, type => 'tm:occurrence',         iplayer => 'tm:aaa' ),
	      [ 'Ich chan Glaas ässe, das tuet mir nöd weeh' ]), 'encoding: same text');

  ok (eq_set (_q_players ($ms, type => 'tm:occurrence',         iplayer => 'tm:bbb' ),
	      [ 'Mohu jíst sklo, neublí?í mi' ]),                'encoding: same text');

}

{ # -- explicit
  my $ms = _parse (q|
%encoding iso-8859-1

aaa
in: Ich chan Glaas ässe, das tuet mir nöd weeh
|);

##warn Dumper $ms;

  ok (eq_set (_q_players ($ms, type => 'tm:occurrence',         iplayer => 'tm:aaa' ),
	      [ 'Ich chan Glaas ässe, das tuet mir nöd weeh' ]), 'encoding: same text');


#   ok (eq_set ([ $ms->toplets (new Toplet (characteristics => [ [ 'universal-scope',
# 								 'xtm-psi-occurrence',
# 								 TM::Maplet::KIND_IN,
# 								 '\x{E4}sse' ]])) ],
# 	       [   'aaa' ]), 'encoding: match in with umlaut');
}

{ #-- explicit different
  my $ms = _parse (q|
%encoding iso-8859-2

aaa
in: Ich chan Glaas ässe, das tuet mir nöd weeh

|);

  ok (eq_set (_q_players ($ms, type => 'tm:occurrence',         iplayer => 'tm:aaa' ),
	      [ 'Ich chan Glaas ässe, das tuet mir nöd weeh' ]), 'encoding: same text');

}

open (STDERR, '>/dev/null');

{
  my $ms = _parse (q|
aaa

%cancel

bbb
|);

 is (scalar $ms->midlets, $npt+1, 'cancelling');
##warn Dumper $ms;
}

{
  my $ms = _parse (q|
aaa

%log xxx

bbb
|);

 is (scalar $ms->midlets, $npt+2, 'logging');
}

__END__



__END__

# testing corrupt TM
# testing TNC

my $text = '

aaa (bbb)
bn: AAA
';
  foreach my $i (1..100) {
    $text .= "

aaa$i (bbb)
bn: AAA$i
";
  }


$tm = new TM (tie => new TM::Driver::AsTMa (auto_complete => 0, text => $text));

warn "Parse RecDescent inclusive: $Parse::RecDescent::totincl";
warn "Parse RecDescent exclusive: $Parse::RecDescent::totexcl";

#warn "instartrule: $Parse::RecDescent::namespace000001::totincl";
warn "instartrule: $TM::Driver::AsTMa::Parser::totincl";

#warn "instartrule: $TM::AsTMa::Parser::totexcl";
warn "namespace0001 instartrule: $Parse::RecDescent::namespace000001::astma";
warn "namespace0001 cparserincl: $Parse::RecDescent::namespace000001::cparserincl";

__END__

TODO: { # assoc with multiple scope
   local $TODO = "assoc with multiple scope";

   eval {
      my $tm = new TM (tie => new TM::Driver::AsTMa (text => '
@ aaa bbb (is-ramsti-of)
ramsti : xxx
rumsti : yyy;
'));
   };

   ok (!$@);
} 

__END__

##=========================================================



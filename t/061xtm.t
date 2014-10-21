use strict;
use warnings;

use Test::More qw(no_plan);
##use Test::Deep;

use Data::Dumper;
$Data::Dumper::Indent = 1;
use TM::Materialized::AsTMa;

#== TESTS ===========================================================================
my $tm = new TM::Materialized::AsTMa (baseuri=>"tm://", inline=> '
nackertes_topic 

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

thistop reifies http://rumsti
bn: reification
sin: http://nowhere.never.ever
sin: http://nowhere.ever.never

(sucks-more-than) is-reified-by atop
winner: nobody
sucker: nobody

')->sync_in;

Class::Trait->apply ($tm, "TM::Serializable::XTM");

can_ok $tm, 'serialize';

{
    my $content = $tm->serialize (version => '1.0');

#warn $content;
    use XML::LibXML;
    my $xp  = XML::LibXML->new();
    my $doc = $xp->parse_string($content);
    
    ok (eq_set ([
		 map { "tm://$_" }
		 map { $_->nodeValue } $doc->findnodes('/topicMap/topic/@id')
		 ],
		[
		 map { $_->[TM->LID] } $tm->toplets (\ '+all -infrastructure')
		 ]), 'topic ids');
    
    is ('#ctop', $doc->findnodes('/topicMap/topic[@id="btop"]/instanceOf/topicRef/@xlink:href'), 'instance btop');

    ok ($doc->findnodes('/topicMap/association[@id="068ce15eb7cf7cc4536d504c73a4c05c"]/instanceOf/topicRef[@xlink:href="#sucks-more-than"]'),
	'found assoc');
    
    ok (
	eq_set ([
		 '#atop', '#others'
		 ],
		[
		 map { $_->nodeValue }
		 $doc->findnodes('/topicMap/association[@id="4abe49897cefeb950e4affaab0418e4f"]/member[roleSpec/topicRef/@xlink:href = "#winner"]/topicRef/@xlink:href')]), 'found assoc');

    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes('/topicMap/topic[@id="btop"]/baseName/baseNameString/text()')
		 ],
		['something', 'some other thing' ]), 'basename value');

    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes('/topicMap/topic[@id="btop"]/baseName[scope/topicRef/@xlink:href = "#ascope"]/baseNameString/text()')
		 ],
		['some other thing' ]), 'basename value, scoped');

    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes('/topicMap/topic[@id="ctop"]/occurrence[instanceOf/topicRef/@xlink:href = "#sometype"]/resourceRef/@xlink:href')
		 ],
		[
		 'http://typedoc',
		 'http://typedandscopedoc'
		 ]), 'occ value, scoped and unscoped');

    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes('/topicMap/topic[@id="ctop"]/occurrence[instanceOf/topicRef/@xlink:href = "#sometype"][scope/topicRef/@xlink:href = "#ascope"]/resourceRef/@xlink:href')
		 ],
		[
		 'http://typedandscopedoc'
		 ]), 'occ value, scoped');

    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes('/topicMap/topic[@id="thistop"]/subjectIdentity/subjectIndicatorRef/@xlink:href')
		 ],
		[
		 'http://nowhere.never.ever',
		 'http://nowhere.ever.never'
		 ]), 'indicators');

    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes('/topicMap/topic[@id="thistop"]/subjectIdentity/resourceRef/@xlink:href')
		 ],
		[
		 'http://rumsti'
		 ]), 'address');

    ok (! $doc->findnodes('/topicMap/topic[@id="ctop"]/subjectIdentity/resourceRef/@xlink:href'),         'no address');
    ok (! $doc->findnodes('/topicMap/topic[@id="ctop"]/subjectIdentity/subjectIndicatorRef/@xlink:href'), 'no indicators');

    my ($aid) = map { $_->nodeValue } $doc->findnodes('/topicMap/topic[@id="atop"]/subjectIdentity/topicRef/@xlink:href');

    ok ($aid =~ /^\#.{32}$/, 'internal reification');

    $aid =~ s/\#//;

    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes(qq|/topicMap/association[\@id="$aid"]/member
						       [roleSpec/topicRef/\@xlink:href = "#winner"]
						       /topicRef/\@xlink:href|)
		 ],
		[
		 '#nobody'
		 ]), 'reified assoc, role + players');
    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes(qq|/topicMap/association[\@id="$aid"]/member
						       [roleSpec/topicRef/\@xlink:href = "#sucker"]
						       /topicRef/\@xlink:href|)
		 ],
		[
		 '#nobody'
		 ]), 'reified assoc, role + players II');

    ok (
	eq_set ([
		 map { $_->nodeValue } $doc->findnodes('/topicMap/topic[count(*) = 0]/@id')
		 ],
		[
		 'nackertes_topic',
		 'nobody',
		 'others',
		 'sometype',
		 'sucker',
		 'sucks-more-than',
		 'winner'
		 ]), 'empty topics');

#print Dumper [
#	      map { $_->nodeValue } $doc->findnodes('/topicMap/topic[count(*) = 0]/@id')
#	     ];
}

{
    my $content = $tm->serialize (omit_trivia => 1, version => '1.0');

    use XML::LibXML;
    my $xp  = XML::LibXML->new();
    my $doc = $xp->parse_string($content);
    
    ok (eq_set ([
		 map { "tm://$_" }
		 (
		 'nackertes_topic',
		 'nobody',
		 'others',
		 'sometype',
		 'sucker',
		 'sucks-more-than',
		 'winner',
		  map { $_->nodeValue } $doc->findnodes('/topicMap/topic/@id')
		  )
		 ],
		[
		 map { $_->[TM->LID] } $tm->toplets (\ '+all -infrastructure')
		 ]), 'topic ids');
}

{
    my $tm2 = new TM (baseuri=>"tm://");
    Class::Trait->apply ($tm2, "TM::Serializable::XTM");
    can_ok $tm2, 'deserialize';

    $tm2->deserialize ($tm->serialize (version => '1.0'));

#warn Dumper $tm2;

    is_deeply( $tm->{mid2iid},    $tm2->{mid2iid},    'toplet structure identical' );
    is_deeply( $tm->{assertions}, $tm2->{assertions}, 'asserts structure identical' );
}

{
    my $content = $tm->serialize (version => '1.0');

    my $tm2 = new TM (baseuri=>"tm://");
    Class::Trait->apply ($tm2, "TM::Serializable::XTM");
    $tm2->deserialize ($tm->serialize (version => '1.0'));
    my $content2 = $tm2->serialize (version => '1.0');
    is ($content, $content2, 'round tripping');

#    use Text::Diff;
#    my $diff = diff \$content, \$content2, { STYLE => "Context" }; #,   \%options;
#    warn Dumper $diff;
}

{ # xtm default namespace
    my $content = q|<topicMap
   xmlns="http://www.topicmaps.org/xtm/1.0/"
   xmlns:xlink="http://www.w3.org/1999/xlink">
  <topic id="rumsti" />
</topicMap>
|;

    my $tm2 = new TM (baseuri=>"tm://");
    Class::Trait->apply ($tm2, "TM::Serializable::XTM");
    $tm2->deserialize ($content);
#warn Dumper $tm2;
    is ($tm2->tids ('rumsti'), 'tm://rumsti', 'default namespace: topic found');
}

{ # explicit namespace + prefix
    my $content = q|<xtm:topicMap
   xmlns:xlink="http://www.w3.org/1999/xlink"
   xmlns:xtm="http://www.topicmaps.org/xtm/1.0/"
>
  <xtm:topic id="rumsti" />
</xtm:topicMap>
|;

    my $tm2 = new TM (baseuri=>"tm://");
    Class::Trait->apply ($tm2, "TM::Serializable::XTM");
    $tm2->deserialize ($content);
#warn Dumper $tm2;
    is ($tm2->tids ('rumsti'), 'tm://rumsti', 'prefixed namespace: xtm: topic found');
}

{ # explicit namespace + prefix
    my $content = q|<bamsti:topicMap
   xmlns:xlink="http://www.w3.org/1999/xlink"
   xmlns:bamsti="http://www.topicmaps.org/xtm/1.0/"
>
  <bamsti:topic id="rumsti" />
</bamsti:topicMap>
|;

    my $tm2 = new TM (baseuri=>"tm://");
    Class::Trait->apply ($tm2, "TM::Serializable::XTM");
    $tm2->deserialize ($content);
#warn Dumper $tm2;
    is ($tm2->tids ('rumsti'), 'tm://rumsti', 'prefixed namespace: bamsti: topic found');
}


__END__


TODO: variants




use TM::Materialized::XTM;
require_ok( 'TM::Materialized::XTM' );



# now do the round trip
my $rt=TM::Materialized::XTM->new(baseuri=>"tm://", inline=>$content);
$rt->sync_in;
ok($rt,"deserialization works");

cmp_deeply($tm,noclass{%{$rt},variants=>ignore(),
last_mod=>ignore(),created=>ignore(),url=>ignore()},"deserialized and original maps are the same");

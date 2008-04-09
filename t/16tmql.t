#-- test suite

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
use XML::Simple;
$XML::Simple::PREFERRED_PARSER = 'XML::LibXML::SAX';
use XML::LibXML::SAX;
my $tests  = XMLin('t/tmql-use-cases.xml', KeyAttr => { database => 'title' }, ForceArray => [ 'database', 'query', 'use-case', 'solution', 'axiom' ]);

use TM::Literal;
use TM::Index::Match;

require_ok ('TM::QL::TS');
require_ok ('TM::QL::PE');
require_ok ('TM::QL');

#warn "# no tests activated";

my $UC = shift @ARGV;

foreach my $testdb (map { $tests->{database}->{$_} } (
#						      'PE Database',
						      'TMQL Database'
						      ) ) {

    use TM::Materialized::AsTMa;
    my $tm = new TM::Materialized::AsTMa (inline => $testdb->{data}->{content});
    $tm->sync_in;

#    my $idx1 = new TM::Index::Match ($tm);
#warn Dumper $tm; exit;

    foreach my $u (sort { $a->{qid} cmp $b->{qid} } @{$testdb->{'use-case'}}) {
#	warn "testing '".$u->{qid} ."'";
	next if         $u->{qid} =~ /^\*/;
	next if (defined $UC and $u->{qid} ne $UC);
#	next unless $u->{qid} =~ /^s/;
	my $d = $u->{title} || "";
	$d =~ s/\n//g;

	my $sol_ctr = 0;
	foreach my $q (@{$u->{query}}) {
	    
	    my $expected = $q->{interface}->{output};
	    $expected->{content} ||= '';
	    if ($expected->{type} eq 'list' || $expected->{type} eq 'set') {
		$expected->{content} =~ s/^\s*\n+//s;
		$expected->{content} =~ s/\n+\s*$//s;
	    } elsif ($expected->{type} eq 'xml') {
# ok
	    } elsif ($expected->{type} eq 'xpath') {
		$expected->{content} =~ s/^\s*\n+//s;
		$expected->{content} =~ s/\n+\s*$//s;
	    } else {
		die;
	    }
	    
#warn "query ".Dumper $q;
	    
	    my @only_tests = (); # assume that there is none
	    foreach my $s (@{$q->{solution}}) {
#warn "test ".Dumper $s;
		push @only_tests, $s if $s->{operational} eq "tyes";
	    }

#warn Dumper \@only_tests;

	    foreach my $s (@only_tests ? @only_tests : @{$q->{solution}}) {        # take only_tests or fall back to complete list
		$sol_ctr++;
		if ($s->{operational} eq 'TODO') {
		  TODO: {
		      local $TODO = $d."/".$q->{title}.": ".$u->{qid}."/$sol_ctr: ";
		      ok (0, $TODO); # $s->{code}
		  }
		} elsif ($s->{operational} =~ /yes/) {
#warn "working on solution $sol_ctr".Dumper $s;
		    if ($expected->{type} eq 'list' || $expected->{type} eq 'set') {
			my $exp;
			if ($expected->{content} =~ /\[\.\.\.\]/) {
			    $exp = [ map { [ $_->[TM->LID] ] } $tm->toplets  ];
			} elsif ($expected->{content} =~ /^$/) {
			    $exp = [];
			} elsif ($expected->{content} =~ /\[\.\.\.\.\.\.\]/) {
			    my @ts = map { $_->[TM->LID] } $tm->toplets;
			    $exp = [ map { my $x = $_; map { [ $x, $_ ] } @ts  } @ts ];
			} else {
			    my @s = map { $_ =~ s/\s+// or $_ } split (/\n/, $expected->{content});
			    @$exp = ();
			    foreach my $s (@s) {
				push @$exp, [ map 
					      { 
					         $_ =~ /^\w+:/ and $_
					      or $_ =~ /(true|false)/              and new TM::Literal ($1, 'xsd:boolean')
					      or $_ =~ /\[(.*)\]/                  and $tm->tids ($1)
#or (warn $_ and 0)
					      or $_ =~ /\[([a-f0-9]{32})\]/        and $1
					      or $_ =~ /"(http:.*)"/   and $_ = $1 and new TM::Literal ($_, 'xsd:anyURI')
					      or $_ =~ /"(urn:.*)"/    and $_ = $1 and new TM::Literal ($_, 'xsd:anyURI')
					      or $_ =~ /"(.*)"/   and $_ = $1      and new TM::Literal ($_, 'xsd:string')
					      or $_ =~ /(\-?\d+(\.\d+))/           and new TM::Literal ($1, 'xsd:decimal')
					      or $_ =~ /(\-?\d+)/                  and new TM::Literal ($1, 'xsd:integer')
					      or $_ =~ /null/                      and undef  }
					      split (/\s*,\s*/, $s)
					      ];
			    }
#warn "expectation is: ".Dumper ($exp); exit;
			}
			_check_list ($tm, $d."/".$q->{title}.": ".$u->{qid}."/$sol_ctr: ", $expected->{type} eq 'list', $s->{code}, $exp, $s->{language}); # ordered or not

		    } elsif ($expected->{type} eq 'xml') {
			my $exp = $expected->{content};
#			$exp =~ s/^\n+//; $exp =~ s/\n+$//;
#			warn "EXPECTING XML $exp";

			my $res = _eval_q ($tm, $s->{code}, $s->{language});
#			warn "GOT $res". Dumper $res;
			my $xml = $res->[0]->[0]->[0];
			my $s   = $xml->toString (0);
			is ($s, $exp, $d."/".$q->{title}.": ".$u->{qid}."/$sol_ctr: ");

		    } elsif ($expected->{type} eq 'xpath') {
			my $res = _eval_q ($tm, $s->{code}, $s->{language});
#			warn "GOT $res". Dumper $res;

			my $doc  = XML::LibXML::Document->new;
			my $frag = $res->[0]->[0]->[0];
			$doc->setDocumentElement( ($frag->childNodes)[1]);

#			warn "serialized: $doc".$doc->toString (0);
			foreach my $xp (split (/\n/, $expected->{content})) {
			    ok ($doc->findnodes( $xp ), $d."/".$q->{title}.": ".$u->{qid}. " $xp");
			}

		    } else {
			die;
		    }
		}
	    }
	}
	foreach my $a (@{$u->{axiom}}) {
	    $sol_ctr++;
	    next unless $a->{operational} =~ /yes/;
#warn "working on axiom $sol_ctr".Dumper $a;
	    my ($left, $right) = split (/===/, $a->{code});
	    
	    _check_list ($tm, $d."/axiom: ".$u->{qid}."/a$sol_ctr", 0, $left, $right, $a->{language});
	}
    }
}

sub _eval_q {
    my $tm   = shift;
    my $code = shift;
    my $lang = shift;

#warn "in eval_q $code";

    if ($lang eq 'tmql') {
# #warn "compiling $code";
	use TM::QL;
	my $q  = new TM::QL ($code);
# #warn "path compiled ".Dumper $q;
	return $q->eval ({'%_' => $tm});
    } elsif ($lang eq 'tau') {
	my $pe  = new TM::QL::PE ($code);
	my $pec = $pe;
#### TODO    my $pec = TM::QL::PE::optimize ($pe);
	return TM::QL::PE::eval ({'%_' => $tm}, $pec);
    }
}

sub _check_list {
    my $tm     = shift;
    my $desc   = shift;
    my $ordered= shift;
    my $left   = shift;
    my $right  = shift;
    my $lang   = shift;

    $left  = _eval_q ($tm, $left, $lang)  unless ref ($left);
#warn scalar @$left;
#warn "left  ".Dumper $left;

    $right = _eval_q ($tm, $right, $lang) unless ref ($right);
#warn "right ".Dumper $right;

    ok ($ordered 
	   ? TM::QL::TS::ts_identical ($left, $right) 
	   : TM::QL::TS::ts_uo_eq     ($left, $right), 
	$desc) or die "got: ".Dumper ($left). "but expected ".Dumper ($right);
}



__END__




do_xml_test ('
function test (map $m) as xml return
<aaaa>{
     forall $t [ $a (sss) ] in $m
     return
        <bbb>{
              forall $bn in $t/bn
              return
                 <ccc>{$bn}</ccc>
              }</bbb>
     sort by $a desc
      }</aaaa>
', { '$m' => $tm }, [ '/aaaa/bbb/ccc[text() = "MMM"]', '/aaaa/bbb/ccc[text() = "NNN"]' ], 'xml:', 0);


do_xml_test ('
function test () as xml return
<element>{
     forall in ("aaa", "bbb", "ccc") return
        " yyy"
}</element>
', {  }, '/element/text()', 'xml:', 0);

do_xml_test ('
function test (map $m, string $a := "xxx") as xml return
<element>{
   let string $b := {$a}
   let string $c := {$b}
$c }</element>
', {  }, '/element', 'xml:', 0);

do_xml_test ('
function test (map $m, string $a := "xxx") as xml return
<element>{$a}yyy{$a}</element>
', {  }, '/element[text() = "xxxyyyxxx"]', 'xml:', 0);



do_xml_test ('
function test (string $a := "xxx") as xml return
<elem{$a}ent uuu="aaa" vvv="bbb">
    huhu <mentel www="ccc"/>
</element>
', {  }, '/elemxxxent', 'xml:', 0);

do_string_test ('
function test () as string return
"xxx{
   forall in ("aaa", "bbb", "ccc")
   return
       " yyy"
} zzz"
', {}, 'xxx yyy yyy yyy zzz', 'forall:', 0);

do_xml_test ('
function test (string $a := "xxx") as xml return
<elem{$a}ent uuu="aaa" vvv="bbb">
    huhu <mentel www="ccc"/>
</element>
', {}, '//mentel[@www="ccc"]', 'xml:', 0);

do_xml_test ('
function test (string $a := "xxx") as xml return
<elem{$a}ent uuu="aaa" vvv="bbb">
    huhu
</element>
', {}, '//@uuu', 'xml:', 0);






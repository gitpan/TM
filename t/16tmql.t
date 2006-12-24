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
	next unless $u->{qid} =~ /^\*s/;
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
		next unless $s->{operational} =~ /yes/;
#warn "working on solution $sol_ctr".Dumper $s;
		if ($expected->{type} eq 'list' || $expected->{type} eq 'set') {
		    my $exp;
		    if ($expected->{content} =~ /\[\.\.\.\]/) {
			$exp = [ map { [ $_ ] } $tm->midlets  ];
		    } elsif ($expected->{content} =~ /^$/) {
			$exp = [];
		    } elsif ($expected->{content} =~ /\[\.\.\.\.\.\.\]/) {
			my @ts = $tm->midlets;
			$exp = [ map { my $x = $_; map { [ $x, $_ ] } @ts  } @ts ];
		    } else {
			my @s = map { $_ =~ s/\s+// or $_ } split (/\n/, $expected->{content});
			@$exp = ();
			foreach my $s (@s) {
			    push @$exp, [ map 
					  { 
					      $_ =~ /^\w+:/ and $_
					   or $_ =~ /\[(.*)\]/ and $tm->mids ($1)
					   or $_ =~ /"(http:.*)"/   and $_ = $1 and new TM::Literal ($_, 'xsd:anyURI')
					   or $_ =~ /"(.*)"/   and $_ = $1      and new TM::Literal ($_, 'xsd:string')
					   or $_ =~ /(\-?\d+(\.\d+))/           and new TM::Literal ($1, 'xsd:decimal')
					   or $_ =~ /(\-?\d+)/                  and new TM::Literal ($1, 'xsd:integer')
					   or $_ =~ /null/                      and undef  }
					  split (/\s*,\s*/, $s)
					  ];
			}
#warn "expectation is: ".Dumper ($exp);
		    }
		    _check_list ($tm, $d."/".$q->{title}.": ".$u->{qid}."/$sol_ctr: ", $expected->{type} eq 'list', $s->{code}, $exp, $s->{language}); # ordered or not
		} else {
		    die;
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

    ok ($ordered ? TM::QL::TS::ts_identical ($left, $right) : TM::QL::TS::ts_uo_eq ($left, $right), $desc) or 
	die "got: ".Dumper ($left). "but expected ".Dumper ($right);
}





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

my $debug;

use Data::Dumper;
$Data::Dumper::Indent = 1;

use Time::HiRes;

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}


sub mk_taxo {
    my $max_d = shift;
    my $max_c = shift;
    my $max_i = shift;

    return _mk_taxo ('0', 0, $max_d, $max_c, $max_i);

sub _mk_taxo {
    my $root  = shift;
    my $d     = shift;
    my $max_d = shift;
    my $max_c = shift;
    my $max_i = shift;

    return { "C$root" => [
                          ( $d < $max_d ? ( map { _mk_taxo ($root . $_, $d+1, $max_d, $max_c, $max_i) } ( 0 .. ($debug ||rand($max_c)))) : () ), # make concepts
                          (                 map { "i$root$_" }                                          ( 0 .. ($debug ||rand($max_i)))       )  # make kids
                          ] };
}
}

sub implant {
    my $tm = shift;
    my $ta = shift;

    my ($root) = keys %$ta;

    foreach my $ch (@{$ta->{$root}}) {
	if (ref ($ch)) { # this is a subtree
	    $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'subclass', 'superclass' ], players => [ (keys %$ch)[0], $root ]));
	    implant ($tm, $ch);
	} else { # this is just an instance
	    $tm->assert (Assertion->new (type => 'isa',            roles => [ 'class', 'instance' ],      players => [ $root, $ch ]));
	}
    }
}

sub verify {
    my $tm = shift;
    my $ta = shift;
    my $si = shift; # silencio?

    my ($root) = keys %$ta;

    foreach my $ch (_flatten_tree ($ta)) {
#warn "for $root finding child $ch";
	if ($ch =~ /C/) { # this is a subtree node
	    if ($si) {
		die "fail $ch (indirect) subclass of $root" unless $tm->is_subclass ($tm->mids ($ch, $root));
	    } else {
		ok ($tm->is_subclass ($tm->mids ($ch, $root)), "$ch (indirect) subclass of $root");
	    }
	} else { # this is just an instance
	    if ($si) {
		die "fail $ch (indirect) instance of $root" unless $tm->is_a ($tm->mids ($ch, $root));
	    } else {
		ok ($tm->is_a ($tm->mids ($ch, $root)),        "$ch (indirect) instance of $root");
	    }
	}
    }

    foreach my $ch (@{$ta->{$root}}) {
	if (ref ($ch)) { # this is a subtree
	    verify ($tm, $ch, $si);
	}
    }
sub _flatten_tree {
    my $ta     = shift;
    my ($root) = keys %$ta;
    my @kids;

    push @kids, $root;
    foreach my $ch (@{$ta->{$root}}) {
	push @kids, ref ($ch) ? _flatten_tree ($ch) : $ch;
    }
    return @kids;
}

}


#== TESTS =====================================================================

use TM;

require_ok( 'TM::Index::Match' );

eval {
    my $idx = new TM::Index::Match (42);
}; like ($@, qr/parameter must be an instance/, _chomp ($@));

eval {
    my $tm = new TM;
    my $idx  = new TM::Index::Match ($tm);
    my $idx2 = new TM::Index::Match ($tm);
}; like ($@, qr/cannot implant/, _chomp ($@));

{
    my $tm = new TM;
    {
	my $idx  = new TM::Index::Match ($tm);
	$idx->detach;
    }
    ok (!$tm->{indices}->{match}, 'first indexed autoremoved');
    {
	my $idx2 = new TM::Index::Match ($tm);
	ok (1, 'second index implanted');
    }
};

my @optimized_keys; # will be determined next

#$debug = 2; # pins down somewhat the tree structure

if (0) { # lazy index, built by use, functional test
    my $taxo = mk_taxo (3, 2, 3);
#warn Dumper $taxo;

    my $tm = new TM;
    implant ($tm, $taxo);
#warn Dumper $tm;

    my $idx = new TM::Index::Match ($tm);
    verify ($tm, $taxo, 0); # non-silent mode

    my $stats = $idx->statistics;
    @optimized_keys = @{ $stats->{proposed_keys} };
}

$debug = 2; # pins down somewhat the tree structure

if (0) { # lazy index, built by use
    my $taxo = mk_taxo (4, 3, 3);
#warn Dumper $taxo;

    my $tm = new TM;
    implant ($tm, $taxo);
#warn Dumper $tm;

    my $idx = new TM::Index::Match ($tm);

#    warn "\n# verifying first run, testing speed....";

    my $start = Time::HiRes::time;
    verify ($tm, $taxo, 1);
    my $unindexed = (Time::HiRes::time - $start);

#    warn Dumper $idx->{cache};

#    warn "# verifying second run, testing speed....";
    $start = Time::HiRes::time;
    verify ($tm, $taxo, 1);
    my $indexed = (Time::HiRes::time - $start);
    ok ($indexed < $unindexed / 2, "measurable speedup with lazy index ($indexed < $unindexed)");

#    warn "# ====== total time =============== ".(Time::HiRes::time - $start);
#warn Dumper $idx->statistics;
    my $stats = $idx->statistics;
    @optimized_keys = @{ $stats->{proposed_keys} };
}

#warn Dumper \  @optimized_keys; exit;

if (0) { # prepopulated
    my $taxo = mk_taxo (2, 1, 1);
    my $tm = new TM;
    implant ($tm, $taxo);

    my $idx = new TM::Index::Match ($tm);

    my $start = Time::HiRes::time;
#    warn "\n# verifying first run, should be medium fast";
    verify ($tm, $taxo, 1);
    my $unindexed = (Time::HiRes::time - $start);

    $idx->detach;

    $idx = new TM::Index::Match ($tm, closed => 1);
#    warn "# prepopulating, takes time";
    $idx->discard and $idx->populate (@optimized_keys);
#    warn Dumper $idx->{cache}; exit;

    $start = Time::HiRes::time;
#    warn "# verifying second run, should be faster";
    verify ($tm, $taxo, 1);
    my $indexed = (Time::HiRes::time - $start);
    ok ($indexed < $unindexed, "measurable speedup with eager (populated) index ($indexed < $unindexed)");

}

my @tmp;

sub _mktmps {
    foreach (qw(0)) {
	use IO::File;
	use POSIX qw(tmpnam);
	do { $tmp[$_] = tmpnam() ;  } until IO::File->new ($tmp[$_], O_RDWR|O_CREAT|O_EXCL);
    }
}

_mktmps;
#warn Dumper \@tmp;

END { map { unlink <$_*> } @tmp; };

{
    use BerkeleyDB ;
    use MLDBM qw(BerkeleyDB::Hash) ;
    use Fcntl;

    my $taxo = mk_taxo (4, 3, 3);

    my $unindexed;

    {
	my %cache;
	tie %cache, 'MLDBM', -Filename => $tmp[0], -Flags    => DB_CREATE
	    or die ( "Cannot create DBM file '$tmp[0]: $!");

	my $tm   = new TM;
	implant ($tm, $taxo);

	my $idx = new TM::Index::Match ($tm, cache => \%cache);
    
#	warn "\n# verifying first run, should be medium fast";
	my $start = Time::HiRes::time;
	verify ($tm, $taxo, 1);
	$unindexed = (Time::HiRes::time - $start);

#	warn "# ====== total time =============== ".(Time::HiRes::time - $start);

#	warn "# verifying second run, should be faster";
	$start = Time::HiRes::time;
	verify ($tm, $taxo, 1);
	my $indexed = (Time::HiRes::time - $start);
	ok ($indexed < $unindexed, "measurable speedup with persistent index ($indexed < $unindexed)");

#	warn "# ====== total time =============== ".(Time::HiRes::time - $start);
	
	untie %cache;
    }

    {
	my %cache;
	tie %cache, 'MLDBM', -Filename => $tmp[0], -Flags => DB_CREATE
	    or die ( "Cannot open DBM file '$tmp[0]: $!");

#	warn Dumper \%cache; exit;

	my $tm   = new TM;
	implant ($tm, $taxo);

	my $idx = new TM::Index::Match ($tm, cache => \%cache);
    
#	warn "\n# re-verifying second run, should be as fast";
	my $start = Time::HiRes::time;
	verify ($tm, $taxo, 1);
	my $indexed = (Time::HiRes::time - $start);
	ok ($indexed < $unindexed, "measurable speedup with persistent index ($indexed < $unindexed)");

#	warn "# ====== total time =============== ".(Time::HiRes::time - $start);
	
	untie %cache;
    }

}

__END__


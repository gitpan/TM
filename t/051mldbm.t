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

#== TESTS ===========================================================================

require_ok( 'TM::Materialized::MLDBM' );

{
    my $tm = new TM::Materialized::MLDBM (file => '/tmp/xxx');
    
    ok ($tm->isa('TM'),                      'correct class');
    ok ($tm->isa('TM::Resource'),            'correct class');
    ok ($tm->isa('TM::Materialized::MLDBM'), 'correct class');
}

eval {
  my $tm = new TM::Materialized::MLDBM ();
}; like ($@, qr/no file/, _chomp ($@));

my ($tmp);
use IO::File;
use POSIX qw(tmpnam);
do { $tmp = tmpnam() ;  } until IO::File->new ($tmp, O_RDWR|O_CREAT|O_EXCL);

END { unlink ($tmp) || warn "cannot unlink tmp file '$tmp'"; }

{
    my $tm = new TM::Materialized::MLDBM (file => $tmp, baseuri => 'tm:');

    $tm->assert (Assertion->new (type => 'is-subclass-of', roles => [ 'subclass', 'superclass' ], players => [ 'ramsti', 'rumsti' ]));
    $tm->sync_out;
}

{
    my $tm = new TM::Materialized::MLDBM (file => $tmp, baseuri => 'tmx:');
    $tm->sync_in;
#warn Dumper $tm;
    is ($tm->mids ('rumsti') , 'tm:rumsti', 'found inserted by assertion 1');
    is ($tm->mids ('ramsti') , 'tm:ramsti', 'found inserted by assertion 2');
    ok ($tm->is_subclass ($tm->mids ('ramsti', 'rumsti')), 'found subclass');
}

__END__

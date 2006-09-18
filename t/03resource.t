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


#== TESTS ===========================================================================

require_ok( 'TM::Resource' );

{
  my $tm = new TM::Resource (baseuri => 'tm:');
#warn Dumper $tm;
  ok ($tm->isa('TM::Resource'),  'correct class');
  ok ($tm->isa('TM'),            'correct class');
    is ($tm->baseuri, 'tm:',       'baseuri ok');
}

{
    my $tm = new TM::Resource (url => '123:');
    is ($tm->url, '123:', 'url ok');
}

{
    my $tm = new TM::Resource (inline => 'xxx');
    like ($tm->url,  qr/^inline:/, 'url ok');
}

{
    my $tm = new TM::Resource (file => 'xxx.xxx');
    is ($tm->url, 'file:xxx.xxx', 'url ok');
}

package Testus;

use TM::Resource;
use base qw(TM::Resource);

our $in  = 0;
our $out = 0;

sub _sync_in {
#    warn "innnn";
    $in++;
}

sub _sync_out {
#    warn "outtttt";
    $out++;
}

sub last_mod {
#    warn "lasttttttttt";
    return time();
}
1;


{
    my $tm = new Testus (url => 'xxx:');

    $tm->sync_in;
    sleep 2;
    $tm->sync_out;

    Test::More::is ($Testus::in,  1, 'synced in once');
    Test::More::is ($Testus::out, 0, 'synced out never');
    $tm->consolidate; # do something, whatever
    $tm->sync_out;
    Test::More::is ($Testus::out, 1, 'synced out now');
}

__END__



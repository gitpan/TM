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

require_ok( 'TM::Materialized::AsTMa' );

{
  my $tm = new TM::Materialized::AsTMa (inline => '# this is AsTMa
');
#warn Dumper $tm;
  ok ($tm->isa('TM'),                      'correct class');
  ok ($tm->isa('TM::Resource'),            'correct class');
  ok ($tm->isa('TM::Materialized::File'),  'correct class');
  ok ($tm->isa('TM::Materialized::AsTMa'), 'correct class');
}

eval {
  my $tm = new TM::Materialized::AsTMa (url => 'file:xxx');
  $tm->sync_in;
}; like ($@, qr/unable to load/, _chomp ($@));

{ # basic operation
  my $tm = new TM::Materialized::AsTMa (inline => '# this is AsTMa
aaa (bbb)
');

  $tm->sync_in;
  ok ($tm->is_a ('tm://nirvana/aaa', 'tm://nirvana/bbb'), 'AsTMa 1: sync in');
  $tm->sync_out; # should not do anything, not die
}

{ # basic operation (2.x)
  my $tm = new TM::Materialized::AsTMa (inline => '%version 2.1

aaa isa bbb
');

  $tm->sync_in;
  ok ($tm->is_a ('tm://nirvana/aaa', 'tm://nirvana/bbb'), 'AsTMa 2: sync in');
  $tm->sync_out; # should not do anything, not die
}

__END__

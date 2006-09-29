package main;

use Log::Log4perl;
Log::Log4perl->init("t/log.conf");
our $log = Log::Log4perl->get_logger("TM");

1;

use strict;
use warnings;

use Data::Dumper;

#== TESTS =====================================================================

use strict;

use Test::More;
eval "use Test::Pod 1.00";
plan skip_all => "Test::Pod 1.00 required for testing POD" if $@;

my @PODs = qw(
	      lib/TM.pm
	      lib/TM/Analysis.pm
	      lib/TM/Resource.pm
	      lib/TM/MapSphere.pm
	      lib/TM/Materialized/File.pm
	      lib/TM/Materialized/AsTMa.pm
	      lib/TM/Materialized/MLDBM.pm
	      lib/TM/Materialized/MLDBM2.pm
	      lib/TM/Tau/Filter.pm
	      lib/TM/Tau/Filter/Analyze.pm
	      );
plan tests => scalar @PODs;

map {
    pod_file_ok ( $_, "$_ pod ok" )
    } @PODs;

__END__

use Test::Pod;


__END__


use Test::More;
eval "use Test::Pod 1.00";
plan skip_all => "Test::Pod 1.00 required for testing POD" if $@;
my @poddirs = qw( blib script );
all_pod_files_ok( all_pod_files( @poddirs ) );

__END__


# Before `make install' is performed this script should be runnable with # `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..5\n"; }
END {print "not ok 1\n" unless $loaded;}
use TM;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

my $tm = TM::TopicMap->new();
if(defined $tm) { print "ok 2\n"; }
else { print "not ok 2\n"; }

if( $tm->require("http://www.gooseworks.org/disclosures/SAM.xml") ) { print "ok 3\n"; }
else { print "not ok 3\n"; } 

my $t = $tm->get_topic("IS13250::UniqueName","AtBaseNamedBaseName");
if(defined $t) { print "ok 4\n"; }
else { print "not ok 4\n"; }

$tm->load_file("/opt/maps/apache.xtm","xtm_simple","xml");
my @a = ("Cactus");
#$t = $tm->get_topic("http://www.gooseworks.org/disclosures/SAM.xml::BaseNames",@a);
#$t = $tm->get_topic("http://www.gooseworks.org/disclosures/SAM.xml::BaseNames",["Cactus","Jetspeed"]);
$t = $tm->get_topic("http://www.gooseworks.org/disclosures/SAM.xml::BaseNames",["Cactus"]);
if(defined $t) { print "ok 5\n"; }
else { print "not ok 5\n"; }



package TM;

require 5.005_62;
use strict;
use warnings;

require Exporter;
require DynaLoader;

our @ISA = qw(Exporter DynaLoader);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use TM ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
	
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);
our $VERSION = '0.01';

bootstrap TM $VERSION;

# Preloaded methods go here.

1;
__END__
# Below is stub documentation for your module. You better edit it!

=head1 NAME

TM - Perl extension for TmTk

=head1 SYNOPSIS

  use TM;

  $tm = TM::TopicMap->new();
  $tm->load_file("/opt/maps/mymap.xtm","xtm_simple","xml");
  $tm->require("SAM");
  $tm->dump();
  $t = $tm->get_topic("IS13250::UniqueName","RoleBaseName");
  $val = $tm->get_property($t,"IS13250::UniqueName");

=head1 DESCRIPTION

TM - Perl extension for TmTk, the Topic Maps Toolkit. TmTk is available
at Gooseworks.org [http://www.gooseworks.org].



=head2 Creating A Topic Map

$tm = TM::TopicMap->new();

=head2 Loading A Topic Map

$tm->load_file("/opt/maps/mymap.xtm","xtm_simple","xml");

=head2 Loading a Topic Map Application (TMA)

$tm->require("SAM") or die("cannot load SAM, " . $tm->get_error());

=head2 Using the 'topic'-view

$tm = TM::TopicMap->new;


sub topic_start
{
	my ($ud, $name, $href) = @_;
	#print " start $name\n";
	if( $name eq "topic" )
	{
		my $hr = {};
		
		$hr->{topic} = $href->{topic};
		$hr->{indicators} = $href->{indicators};
		$hr->{names} = [];
		
		$$ud->{last_topic} = $hr;
	}
	if( $name eq "basename" )
	{
		push(@{$$ud->{last_topic}->{names}}, $href->{string});

	}
}
sub topic_end
{
	my ($ud,$name) = @_;
	#print " end $name\n";
	if( $name eq "topic" )
	{
		my $hr = $$ud->{last_topic};
		print "Topic: $hr->{topic}\n";
		foreach(@{$hr->{indicators}})
		{
			print "  $_\n";
		}
		foreach(@{$hr->{names}})
		{
			print "  $_\n";
		}
		print "\n\n";
	}
}

$tm->query(\$user_data, \&topic_start, \&topic_end, "VIEW topic(topic=200)" );









=head1 AUTHOR

Jan Algermissen  algermissen@acm.org

=cut

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
our $VERSION = '0.03';

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
  $t = $tm->get_topic("IS13250::UniqueName","RoleBaseName");
  $val = $tm->get_property($t,"IS13250::UniqueName");
  $tm->dump();
  $tm->require("SAM");


=head1 DESCRIPTION

TM - Perl extension for TmTk, the Topic Maps Toolkit. TmTk is available
at Gooseworks.org [http://www.gooseworks.org].


=head2 Creating A Topic Map

$tm = TM::TopicMap->new();

Creates a new TopicMap object.

=head2 Loading A Topic Map from a file

In order to load a topic map document into your TopicMap,
you need to tell the internal parsing component, where the
document is found, what processing model to apply to it and
also, what parser to invoke:

$tm->load_file("/opt/maps/mymap.xtm","xtm_simple","xml") or
                die("unable to load topic map,",$tm->get_error());

Available rpocessing models are:

=over 4

=item xtm_simple

=item rdf_common

=item iso2788

=back

Available parse modes are:

=over 4

=item xml

=item rdfxml

=item lines

=back


You can load maps at any time and all loaded maps will be merged.


=head2 Loading A Topic Map from a string

If you have your topic map in a string and not in a file, use


  $tm->load_string(
	qq{<topicMap>
	   <topic id="t1" />
	   </topicMap>},
	"xtm_simple","xml") or
                die("unable to load topic map,",$tm->get_error());


A method for loading a topic map in chunks is planned for the next
release.

=head2 Loading a Topic Map Application (TMA)

Sometimes it is desired to load additional TMAs (ones that have
not been loaded by the applied processing model(s) implicitly). To
do this, you simply call the C<require()> method and supply it the
name of the TMA:

$tm->require("MYTMA") or die("cannot load MYTMA, " . $tm->get_error());







=head2 Using the Topic Map Views

TmTk provides a special way of querying a topic map, that is called
I<parameterized views>. A view is a conceptual abstraction of a subset
of the semantics of a (or a combination of) TMA. Examples for views
are "Index", "Taxonomy", or "RolePlayings". 

The process of querying a topic map with TmTk is almost identical to
event-oriented parsing of an XML document: Together with the actual
query you pass a start-event- and an end-event callback function
to the query method of a topic map object. The topic map object will
call these callbacks according to certain events in the view processing.



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


That;s all






=head1 AUTHOR

Jan Algermissen  algermissen@acm.org

=cut

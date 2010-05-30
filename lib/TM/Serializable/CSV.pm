package TM::Serializable::CSV;

use strict;
use warnings;

use Class::Trait 'base';
use Class::Trait 'TM::Serializable';

use Data::Dumper;


=pod

=head1 NAME

TM::Serializable::CSV - Topic Maps, trait for parsing (and later dumping) CSV stream

=head1 SYNOPSIS

   # 1) bare bones
   my $tm = .....;  # get a map from somewhere (can be empty)
   Class::Trait->apply ($tm, "TM::Serializable::CSV");

   use Perl6::Slurp;
   $tm->deserialize (slurp 'myugly.csv');

   # 2) exploiting the timed sync in/out mechanism
   my $tm = new TM::.... (url => 'file:myugly.csv'); # get a RESOURCEABLE map from somewhere
   $tm->sync_in;

=head1 DESCRIPTION

This trait provides parsing and dumping from CSV formatted text streams.

=head1 INTERFACE

=head2 Methods

=over

=item B<deserialize>

I<$tm>->deserialize (I<$text>)

This method consumes the text string passed in and interprets it as CSV formatted information. What topic
map information is generated, depends on the header line (the first line):

=over

=item

If the header line contains a field called C<association-type>, then all rows will be interpreted as
assertions. In that the remaining header fields (in that order) are interpreted as roles (role
types). For all rows in the CSV stream, the position where the C<association-type> field was is
ignored.  The other fields (in that order) are affiliated with the corresponding roles.

Example:

   association-type,location,bio-unit
   is-born,gold-coast,rumsti
   is-born,vienna,ramsti

Scoping cannot be controlled. Also all players and roles (obviously) are directly interpreted as
identifiers. Subject identifiers and locators are not (yet) implemented.

=item

If the header line contains a field called C<id>, then all further rows will be interpreted as topic
characteristics, with each topic on one line. The column position where the C<id> field in the
header is will be interpreted as toplet identifier.

All further columns will be interpreted according to the following:

=over

=item

If the header column is named C<name>, the values will be used as topic names.

=item

Otherwise if the value looks like a URI, an occurrence with that URI value will be be added to the topic.

=item

Otherwise an occurrence with a string value will be added to the topic.

=back

Example:

   name,id,location,homepage
   "Rumsti",rumsti,gold-coast,http://rumsti.com
   "Ramsti",ramsti,vienna,http://ramsti.com

=back

=cut

sub deserialize {
    my $self = shift;
    my $stream = shift;
##    my $config = shift; # ????

    $stream =~ s/\r//g;                                                                      # F...ing M$
    my @lines = split /\n/, $stream;

    use Text::CSV;
    my $csv = Text::CSV->new({ always_quote => 1 });

    #-- first line -------------------------
    $csv->parse (shift @lines); # and warn $csv->error_diag ();
    my @headers = map { s/ /-/g; $_ }                                                        # get rid of blanks, I hate blanks
                  $csv->fields();

    if (defined (my $pos = _find_and_kill (\@headers, 'association-type'))) {
	foreach my $line (@lines) {
	    $csv->parse ($line) ; #and warn  $csv->error_diag ();
	    my @players = $csv->fields();
	    my $type    = splice @players, $pos, 1;

#	    warn "$type: ".Dumper (\@roles, \@players);
	    
	    $self->assert (
		Assertion->new (kind    => TM->ASSOC,
				type    => $type,
				scope   => 'us',
				roles   => \@headers,
				players => \@players)
		);
	}
	sub _find_and_kill {
	    my $roles = shift;
	    my $kill  = shift;
	    foreach my $i (0 .. $#$roles) {
		if ($roles->[$i] eq $kill) {
		    splice @$roles, $i, 1;  # remove that from the roles list, but ...
		    return $i;              # we memorize its position
		}
	    }
	    return undef;
	}
    } elsif (defined ($pos = _find_and_kill (\@headers, 'id'))) {
	foreach my $line (@lines) {
	    $csv->parse ($line) ; #and warn  $csv->error_diag ();
	    my @attrs = $csv->fields();
	    my $id    = splice @attrs, $pos, 1;

#	    warn "$type: ".Dumper (\@roles, \@players);
	    $self->internalize ($id);

	    use Regexp::Common qw /URI/;
	    use TM::Literal;
	    foreach my $i (0..$#attrs) {
		if ($headers[$i] eq 'name') {
		    $self->assert (
			Assertion->new (kind    => TM->NAME,
					type    => 'name',
					scope   => 'us',
					roles   => [ 'topic', 'value'],
					players => [ $id,    new TM::Literal ($attrs[$i], TM::Literal->STRING)  ])
			);

		} elsif ($attrs[$i] =~ /$RE{URI}/) {
		    $self->assert (
			Assertion->new (kind    => TM->OCC,
					type    => $headers[$i],
					scope   => 'us',
					roles   => [ 'thing', 'value'],
					players => [ $id,  new TM::Literal ($attrs[$i], TM::Literal->URI) ])
			);

		} else {
		    $self->assert (
			Assertion->new (kind    => TM->OCC,
					type    => $headers[$i],
					scope   => 'us',
					roles   => [ 'thing', 'value'],
					players => [ $id,   new TM::Literal ($attrs[$i], TM::Literal->STRING) ])
			);
		}
	    }
	    
	}
    } else {
	die;
    }

}

=pod

=item B<serialize>

Not yet implemented. But it's easy.

=cut

sub serialize {
    die "not implemented";
}

=pod

=back

=head1 SEE ALSO

L<TM>, L<TM::Serializable>

=head1 AUTHOR INFORMATION

Copyright 2010 Robert Barta.

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.  http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION = 0.01;

1;

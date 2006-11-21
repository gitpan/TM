package TM::Serializable::AsTMa;

use base qw (TM::Serializable);

use Data::Dumper;

=pod

=head1 NAME

TM::Serializable::AsTMa - Topic Maps, trait for parsing AsTMa instances.

=head1 SYNOPSIS

  # this is not an end-user package
  # see the source in TM::Materialized::AsTMa how this can be used

=head1 DESCRIPTION

This trait provides parsing functionality for AsTMa= instances. AsTMa= is a textual shorthand
notation for Topic Map authoring. Currently, AsTMa= 1.3 and the (experimental) AsTMa= 2.0 is
supported.

=over

=item B<AsTMa= 1.3>

This follows the specification: L<http://astma.it.bond.edu.au/authoring.xsp> with the following
constraints/additions:

=over

=item following directives are supported:

=over

=item %cancel

Cancels the parse process on this very line and ignores the rest of the AsTMa instance. Useful for
debugging faulty maps. There is an appropriate line written to STDERR.

=item %log [ message ]

Writes a line to STDERR reporting the line number and an optional message. Useful for debugging.

=item %encoding [ encoding ]

Specifies which encoding to use to interpret the B<following> text. This implies that this
directive may appear several times to change the encoding. Whether this is a good idea
in terms of information management, is a different question.

Note: If no encoding is provided, utf8 is assumed.

=back

A directive can be inserted anywhere in the document but must be at the start of a line.

=back


=item B<AsTMa= 2.0>

It follows the specification on http://astma.it.bond.edu.au/astma=-spec-2.0r1.0.dbk with
the following changes:

=over

=item this is work in progress

=back

=back

=head1 INTERFACE

=head2 Methods

=over

=item B<deserialize>

This method take a string and tries to parse AsTMa= content from it. It will raise an exception on
parse error.

=cut

sub deserialize {
    my $self    = shift;
    my $content = shift;

#warn "DESERIALIZA ASTMA: $content";

    if ($content =~ /^\s*%version\s+2/s) {                                     # this is version 2.x
	use TM::AsTMa::Fact2;
	my $ap = new TM::AsTMa::Fact2 (store => $self);
	$ap->parse ($content);

    } else {                                                                   # assume it is 1.x
	use TM::AsTMa::Fact;
	my $ap = new TM::AsTMa::Fact (store => $self);
	$ap->parse ($content);                                                 # we parse content into the ap object component 'store'
    }
}

=pod

=item B<serialize>

This is not implemented. Maybe it never will.

=cut

sub serialize {
    $main::log->logdie ( scalar __PACKAGE__ .": serialization not implementable (not easily, but be my guest :-)" );
}

=pod

=back

=head1 SEE ALSO

L<TM>, L<TM::Serializable>

=head1 AUTHOR INFORMATION

Copyright 200[1-6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.  http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION  = '0.2';
our $REVISION = '$Id: AsTMa.pm,v 1.1 2006/11/13 08:02:34 rho Exp $';

1;

__END__

package TM::Tau::Filter;

use TM::Resource;
use base qw(TM::Resource);

use Data::Dumper;

=pod

=head1 NAME

TM::Tau::Filter - Topic Maps, abstract filter class

=head1 SYNOPSIS

   my $tm     = ... some map (or another filter)
   my $filter = new TM::Tau::Filter (left => $tm);

   $filter->sync_in; # this will pass on the sync in to the left operand

   # after that, the filter itself holds the result (which is a map)
   $filter->instances (....);

=head1 DESCRIPTION

Filters are special maps in that their content depends on another map and a particular
transformation to get the map result. If you consider the expression

   some_map.atm * some_transformation

then C<some_transformation> is applied to the map coming from the map C<some_map.atm>.
This scheme can be expanded to the left:

   some_map.atm * some_transformation1 * some_transformation2

so that a whole chain of transformations can be applied to a map. The expression has to be
interpreted left-associative, so as if written as

   (some_map.atm * some_transformation1) * some_transformation2

When you build a filter expression, then you have to respect this left-associativeness:

   my $map    = new TM....;
   my $trafo1 = new TM::Tau::Filter (left => $map);
   my $trafo2 = new TM::Tau::Filter (left => $trafo1);

The variable C<$trafo2> then holds this expression, but nothing is actually computed at this
stage. To trigger this process, the method C<sync_in> can be used (read: apply). It will trigger the
in-synchronisation of C<$trafo1> and that will pass it on to the C<$map>. That will do something (or
not) to ensure that the map is up-to-date relative to the resource it is possibly associated with.
Once this is done, the filter C<$trafo1> will do its work. Once the result is available, C<$trafo2>
will do its work.

=head2 Transformations

Filters are not constrained in what they are doing. Some filters might only extract a particular
portion out of a map. Others will map more complex conversions, say, to adapt to a different
background ontology. Others will completely change the map, or compute new stuff from it. It is also
possible to have transformers which actually do nothing, except than mediating between different
formats a map is written in.

To specify B<what> the transformation is supposed to do, you can either overload the method
C<sync_in>, or alternatively keep it and overload only C<transform>:

   sub transform {
       my $self = shift;       # this is the filter
       my $map  = shift;       # this is the left operand map

       ....                    # do whatever you need to do
       $result = .....         # this might be your result
       return $result;         # return it
   }

Your result will be used as content for the filter (which is map itself, remember). See L<TM::Tau::Filter::Analyze>
for an example.

The default transformation is the empty one, i.e. the map is simply passed through (not copied,
btw).

=head1 INTERFACE

=head2 Constructor

The constructor of implementations should expect a hash as parameter containing the field(s) from
L<TM>, L<TM::Resource> and one or more of the following:

=over

=item I<left>:

This must be an object of class L<TM>.

=item I<url> (default C<null:>)

If the URL is missing here (filters are resourced maps), then it defaults to C<null:>

=back

=cut

sub new {
    my $class   = shift;
    my %options = @_;

    $options{url} ||= 'null:'; # a filter may have nothing to which it is attached outgoingly
    ref ($options{left}) and $options{left}->isa ('TM')
	or $main::log->logdie ( scalar __PACKAGE__ .": left operand must be an instance of TM" );
    return bless $class->SUPER::new (%options), $class;
}

=pod

=head2 Methods

=over

=item B<left>

I<$tm> = I<$filter>->left

This is an accessor to get the left operand.

=cut

sub left {
    my $self = shift;
    return $self->{left};
}

=pod

=item B<transform>

I<$tm2> = I<$filter>->transform (I<$tm>)

This method performs the actual transformation. If you develop your own filter, then this has to be
overloaded. The default implementation here only hands back the same map.

=cut

sub transform {
    return $_[1];
}

sub sync_in {
    my $self = shift;

#warn __PACKAGE__ . "sync in". $self->{_in_url};
    if ($self->{left}->can ('sync_in')) {                   # this is a resourced map itself
	$self->{left}->sync_in;                             # lets get the upstream crap, uhm map
	$self->melt (
		     $self->transform ($self->{left}, $self->{baseuri})
		     );
    }
}

=pod

=back

=head1 SEE ALSO

L<TM>, L<TM::Resource>, L<TM::Tau::Filter::Analyze>

=head1 AUTHOR INFORMATION

Copyright 200[4-6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.
http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION = 0.2;
our $REVISION = '$Id: Filter.pm,v 1.5 2006/09/29 06:53:45 rho Exp $';

1;

__END__




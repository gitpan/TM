package TM::FAQ;

=pod

=head1 NAME

TM::FAQ - Topic Maps, Frequently Angering Quirks

=head1 FAQ, TIPS AND PITFALLS

=over

=item

Q: How can I get rid of these annoying topics C<occurrence>, C<name>, etc. which I have not put into
the map in the first place, but which are in there in all cases?

A: These infrastructure topics are needed to make the map I<complete> (everything used is defined). If
you want to filter these out, you can get them first via

    my @infra = $tm->mids (keys %{$TM::PSI::topicmaps->{mid2iid}})

To deduct them from the list of all topics

    my @all = $tm->midlets;

You can do:

    # make lookup for speed
    my %infra;
    @infra{ @infra } = (1) x @infra;

    my @non_infra = grep (!$infra{$_}
                       && !$tm->retrieve ($_),
                    $tm->midlets);

The C<retrieve> looks up whether the topic is an assertion. Most likely, you would not want these as
well then.

=item

Q: How can I get all names and/or occurrences of a topic?

A: If you have a topic identifier C<mytopic> in your hand, then you can get all characteristics (=
names and occurrences, but not associations) with

    my @cs = $tm->match_forall (char => 1, irole => $tm->mids ('mytopic'));

Then you can filter occording the type, the scope, the data type value or the C<KIND>. The latter
tells you whether an assertion is a name or occurrence. For example those occurrences which are
C<occurrence>s and not anything specials as, say, C<homepage>:

    my @occs  = map { $_->[0] } map { TM::get_x_players ($tm, $_, 'tm://value') } grep ($_->[TM->TYPE] eq 'tm://occurrence', @cs);

=item

Q: How do these C<get_x_role> functions work? What does I<does not honor subclassing> mean?

A: @@@@@@@@@@@@@

(is-father-of)
father: godzilla
child:  kidzilla

father subclasses parent

dann kriegst du mit

  get_x_role ($tm, $a, '...father')     =======> godzilla
  get_x_role ($tm, $a, '...parent')     =======> nix
  get_role   ($tm, $a, '...parent')     =======> godzilla



=item 

Timestamps of resources

@@@ files have UNIX time resolution (second), TM uses HiRes::Time @@@@

=back

=head1 COPYRIGHT AND LICENSE

Copyright 200[3-6] by Robert Barta, E<lt>drrho@cpan.orgE<gt>

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.

=cut

our $VERSION  = 0.3;
our $REVISION = '$Id: Overview.pm,v 1.3 2006/11/29 10:31:13 rho Exp $';

1;

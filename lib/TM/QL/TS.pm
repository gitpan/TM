package TM::QL::TS;

use strict;
use Data::Dumper;

require Exporter;
use base qw(Exporter);

our @EXPORT = qw(ts_interleave ts_subtract ts_intersect ts_concat ts_share ts_literalify ts_cmp ts_op ts_identical ts_uo_eq ts_eq);

use TM;
use TM::Literal;

=pod

=head1 NAME

TM::QL::TS - TMQL Tuple Sequence handling

=head1 SYNOPSIS

=head1 DESCRIPTION

Tuple sequences (TSs) are sequences of tuples of values. Values are either literals (L<TM::Literal>)
or internal map identifiers. Naturally, TSs are modelled as array of array of values.

This package simply provides manipulation functions for this data structure.

@@@@

states

ATOMIFY

ATOMIFIED


=head1 INTERFACE

=head2 Functions

=over

=item B<ts_concat>

@@@ multiplies one TSs with another @@@

=cut

sub ts_concat {
    my $tss1 = shift;
    my $tss2 = shift;
#warn "flattening ".Dumper $tss1, $tss2;
    return $tss2 unless $tss1;

    my $tss;
    foreach my $t1 (@$tss1) {
	foreach my $t2 (@$tss2) {
	    push @$tss, [ @$t1, @$t2 ];
	}
    }
#warn "flattener ".Dumper $tss;
    return $tss;
}

=pod

=item I<ts_share>

=cut

# very naive (=slow) implementation
sub ts_share {
    my $as = shift;
    my $bs = shift;

#warn "sharing?".Dumper $as, $bs;
  OUTER:
    foreach my $a (@$as) {
      INNER:
	foreach my $b (@$bs) {
	    next INNER unless $#$a == $#$b;
	    for (my $i = 0; $i <= $#$a; $i++) {

		my ($va,   $vb)   = ($a->[$i], $b->[$i]);
		my ($refa, $refb) = (ref ($va), ref ($vb));

		if ($refa eq 'TM::Literal' && $refb eq 'TM::Literal') {
#warn "comparing ".Dumper ($a->[$i], $b->[$i]);
		    next INNER unless $va->[1] eq $vb->[1];                 # check whether types are ok
		    my $compare = $TM::Literal::comparators->{$va->[1]} || die "unknown comparison for ".$va->[1];
		    next INNER unless &$compare ($va->[0],  $vb->[0]);      # check the values
		} elsif ($refa eq 'TM::Literal') {                          # one is literal, the other not => not good
		    next INNER;
		} elsif ($refb eq 'TM::Literal') {                          # one is literal, the other not => again not good
		    next INNER;
		} else {
		    next INNER unless $va eq $vb;                           # both non-literal, lets compare them as (string) identifiers
		}
	    }
#warn "yes, equal";
	    return 1;
	}
    }
    return 0;
}

=pod

ts_interleave

simply vertically combine TSes

implements |||

!! first param is modified!!! (cheap)

=cut

sub ts_interleave {
    my $as = shift;
    my $bs = shift;

    push @$as, @$bs;
    return $as;
}

=pod

ts_subtract

implements --- op
computes difference (a - b)

=cut

sub ts_subtract {
    my $as = shift;
    my $bs = shift;

    my %MD5s;                                                  # will contain MD5 sigs from each tuple

    map { $MD5s{_tuple_md5 ($_)}++ } @$bs;

    my $xs = [];                                               # will contain result TS
    foreach $a (@$as) {
	push @$xs, $a unless $MD5s{ _tuple_md5 ($a) };         # if we have seen that one above, take it
    }
    return $xs;
}

=pod

ts_intersect

implements the === operator

potentially expensive operation!

=cut

sub _tuple_md5 {
    use Digest::MD5 qw(md5);
    my $t = shift; # gets tuple

    my $ctx = Digest::MD5->new;
    foreach my $md5 (map { ref ($_)                           # first MD5'ing each value in the tuple
			       ? md5 ($_->[0])
			       : md5 ($_) } @$t) {
	$ctx->add($md5);                                      # with these, build overall MD5
    }
    return $ctx->hexdigest;
}

sub ts_intersect {
    my $as = shift;
    my $bs = shift;

    my %MD5s;                                                  # will contain MD5 sigs from each tuple
    map { $MD5s{_tuple_md5 ($_)}++ } @$as;

    my $xs = [];                                               # will contain result TS
    foreach $b (@$bs) {
	push @$xs, $b if $MD5s{ _tuple_md5 ($b) };             # if we have seen that one above, take it
    }
    return $xs;
}

=pod

=item I<ts_literalify>

=cut

sub ts_literalify {
    my $tss = shift;
    my $tm  = shift;

#warn "serializer!! $tm";
#warn Dumper $tm;
    my $VALUE = $tm->mids ('value');
    foreach my $t (@$tss) {
	foreach my $i (0 .. $#$t) {
	    my $v = $t->[$i];
	    next if ! defined $v;                              # undef remains untouched
	    next if ref ($v) eq 'TM::Literal';                 # literals remain untouched
	    my $a = $tm->retrieve ($v) or next;                # try to find an assertion/characteristics in the context map
	    next if $a->[TM->KIND] == TM->ASSOC;               # association ==> keep the identifier

	    ($v) = $tm->get_players ($a, $VALUE);              # there should only be one!
            $v = new TM::Literal ($$v, 'xsd:string') if ref ($v) eq 'SCALAR'; # convert old style string into literal
	    $t->[$i] = $v;
	}
    }
    return bless $tss, 'ATOMIFIED';                            # mark it as such
}

# TODO @ REMOVE


sub ts_atomify {
    my $v  = shift;
    my $tm = shift;

    return $v if ! defined $v;                                 # undef remains untouched
    return $v if ref ($v) eq 'TM::Literal';                    # literals remain untouched
    my $a = $tm->retrieve ($v) or return $v;                   # try to find an assertion/characteristics in the context map
    return $v if $a->[TM->KIND] == TM->ASSOC;                  # association ==> keep the identifier

#warn "atomify ".Dumper $a;
    my $VALUE = $tm->mids ('value');
    my ($v2) = $tm->get_players ($a, $VALUE);              # there should only be one!
    return new TM::Literal ($$v2, 'xsd:string')                # convert old style string into literal
	if ref ($v2) eq 'SCALAR';                              # TODO: @@@@ when map has actually typed data
    return $v2;                                                 # give up
}

=pod

=item I<ts_op>

=cut

sub ts_op {
    my $op  = shift;
    my $tss = shift;
    my $ts  = shift;

#warn "applying $op with ".Dumper ($tss, $ts);
    foreach my $t (@$tss) {
	for (my $j = 0; $j < @$t; $j++) {
	    my $opp = $TM::Literal::operators->{$op}->{$t->[$j]->[1]}->{$ts->[$j]->[1]} || die "unknown op '$op' for ".$t->[$j]->[1]." x ".$ts->[$j]->[1];
	    $t->[$j] = &$opp ($t->[$j], $ts->[$j]);
	}
    }
#warn "rendering ".Dumper ($tss);
}

=pod

=item I<ts_fun>

gets a tuple sequence, takes a tuple, interprets this as argument 

=cut

sub ts_fun {
    my $fun = shift;
    my $tss = shift;

    my $tss2 = [];                                                      # resulting TS
    foreach my $t (@$tss) {                                             # for all tuples in my sequence
	push @$tss2, [ &$fun (@$t) ];                                   # add the result of the function app to the resulting
    }
    return $tss2;
}

=pod

=item I<ts_cmp>

=cut

sub ts_cmp { #old _tuple_seq_cmp
    my $op = shift;
    my $tss1 = shift;
    my $tss2 = shift;

    if ($op eq '==') {                                             # we return literal 1 on success
	return [ [ new TM::Literal  (ts_eq ($tss1, $tss2), 'xsd:integer') ] ];
    } else {
	die "tuple comparison $op not yet implemented";
    }
}

=pod

=item I<ts_eq>, I<ts_uo_eq>

These boolean functions compare two TSs on a I<semantic> basis, in
that the individual value entries are compared according to their type
semantics. Two floats, C<3.14> and C<3.140> for instance are the same,
although their representation may differ.

I<ts_eq> respects the ordering of the sequence, whereas I<ts_uo_eq> treats
the sequences as bags (multisets).

B<Note>: This contrasts with I<ts_identical> which makes a verbatim comparison
on the representation.

B<Note>: I<ts_uo_eq> is a B<DESTRUCTIVE> test, and it is not YET honoring type
semantics. TO BE FIXED.

=cut

sub ts_eq { # __tuple_seq_eq
    my $ss = shift;
    my $tt = shift;
    
    return 0 unless $#$ss == $#$tt;
    foreach my $i (0 .. $#$ss) {
	return 0 unless tuple_eq ($ss->[$i], $tt->[$i]);
    }
    return 1;
}

sub tuple_eq { # old: __tuple_eq
    my $s = shift;
    my $t = shift;

    return 0 unless $#$s == $#$t;
    foreach my $i (0..$#$s) {
	my $cmp = $TM::Literal::operators->{'=='}->{$s->[$i]->[1]}->{$t->[$i]->[1]} || die "unknown op '==' for ".$s->[$i]->[1]." x ".$t->[$i]->[1];
	return 0 unless &$cmp ($s->[$i], $t->[$i]);
    }
    return 1;
}

sub ts_uo_eq {                                             # destructive test
    my $r = shift;
    my $e = shift;
#warn "compare set".Dumper ($r, $e);
    
#    return 0 unless @$e == @$r; # quick check whether same length
#warn "before loop";
    foreach my $te (@$e) { # go through expected tuples
#warn ".... $te";
#warn "te ".Dumper $te;
#warn "r ".Dumper $r;
	my @r2 = grep (!eq_array ($te, $_), @$r);
#warn "r2 ".Dumper \@r2;
	return 0 if @$r == @r2;                            # if the last step did not reduce the length, then the set cannot be the same
        $r = \@r2;
    }
#warn "leftovers ".Dumper ($r);
    ! @$r;                                                 # only equal if there is no element left
}


=pod

=item I<ts_identical>

This boolean function makes a deep comparison of the two TSs passed
in. Only if they are exactly identical, the result will be non-zero.

=cut

use Test::More;

sub ts_identical {
    my $r = shift;
    my $e = shift;
#warn "compare list".Dumper ($r, $e);
    eq_array ($r, $e);
}

=pod

=item I<ts_navigation>

move, move, move

   # apply one step of navigation


=cut

sub ts_navigation {
    my $tm  = shift;                                                                 # the context map
    my $tss = shift;                                                                 # incoming TS
    my $nav = shift;                                                                 # the navigation

       $_   = $nav->axi . $nav->dir;                                                 # what is it what we have to do?
    my ($TID, $THING) = $tm->mids ($nav->tid, 'thing');                              # for the case that we need it

#warn "navi $_ for ".Dumper $tss;

    return bless $tss, 'ATOMIFY'                                                     # schedule for it (at the end of a prs sequence, that it)
	if ($_ eq 'atomify1');                                                       #      if it is forward atomification

    TM::QL::TS::ts_literalify ($tss, $tm)                                            # otherwise, we do in-replacement of $tss
	if (ref ($tss) eq 'ATOMIFY' && $_ eq 'atomify');                             #      if it is reverse atomification
                                                                                     # BUT, we continue here
    my $tss2;                                                                        # we have to build a new tuple sequence

    foreach my $t (@$tss) {                                     	             # forall tuples in the incoming sequence
	my $tss3;                                                                    # one TS subsequence for one incoming tuple
	foreach my $v (@$t) {                                                        # forall values in that tuple
	    my @v2;
	    
	    if (/epsilon/) {                                                         # then we do not care about the direction, or the topic
		push @v2, $v;
		

	    } elsif (/superclasses1/) {
		push @v2, $tm->superclassesT ($v);
		
	    } elsif (/superclasses/) {
		push @v2, $tm->subclassesT ($v);
		

	    } elsif (/classes1/) {
		push @v2, $tm->typesT ($v);
		
	    } elsif (/classes/) {
		push @v2, $tm->instancesT ($v);
		

	    } elsif (/players1/) {
		push @v2, grep (defined $_, $tm->get_players ($tm->retrieve ($v), $TID));

	    } elsif (/players/) {
		push @v2, map { $_->[TM->LID] }                                           # we are only interested in the ID, not the whole thing
                          $tm->match_forall (irole   => $TID, iplayer => $v);

	    } elsif (/roles1/) {
		push @v2, @{$tm->get_role_s ($tm->retrieve ($v))};
		
	    } elsif (/roles/) {
		die "NOT YET IMPLEMENTED"; # TODO @@@@@@@@@@@;
		

	    } elsif (/characteristics1/) {
		push @v2, map { $_->[TM->LID] }                                       # in any case, we are only interested in the ID, not the whole thing
		          $tm->match_forall (char  => 1,
			            ($TID ? (type  => $TID) : ()),
					     topic => $v);

	    } elsif (/characteristics/) {
		push @v2, $tm->get_x_players ($tm->retrieve ($v), $THING);
		

	    } elsif (/scope1/) {
		push @v2, $tm->retrieve ($v)->[TM->SCOPE];
		
	    } elsif (/scope/) {
		die "inverse scoping not yet implemented";
		

	    } elsif (/reifier1/) {
		push @v2, $tm->reified_by ($v);
		
	    } elsif (/reifier/) {
		push @v2, $tm->reifies ($v);
		
	    } elsif (/indicators1/) {
		push @v2, map { new TM::Literal ($_, TM::Literal->URI) } @{ $tm->midlet ($v)->[TM->INDICATORS] };
		
	    } elsif (/indicators/) {
		push @v2, $tm->mids (\ $v->[0] ) if ref ($v) eq 'TM::Literal' && $v->[1] eq TM::Literal->URI;

	    } elsif (/locators1/) {
		my $addr = $tm->midlet ($v)->[TM->ADDRESS];
		push @v2, new TM::Literal ($addr, TM::Literal->URI) if $addr;
		
	    } elsif (/locators/) {
		push @v2, $tm->mids ( $v->[0] ) if ref ($v) eq 'TM::Literal' && $v->[1] eq TM::Literal->URI;

	    } elsif (/atomify/) {                                                    # this MUST be reverse atomify
#warn "atomify <". Dumper $v;
		push @v2, map { $_->[TM->LID] } $tm->match_forall (char  => 1,
								   value => $v);
#warn "atomify v2 ".Dumper \@v2;
	    } else {
		die "unhandled axis: ".$nav->axi. ($nav->dir ? ':>:' : ':<:');
	    }
#warn "adding v2 ".Dumper (\@v2)." to tss3";
	    $tss3 = ts_concat ($tss3, [ map { [ $_ ] } @v2 ]);   # here we flatten
#warn "tss3 now: ".Dumper $tss3;
	}
#warn "adding tss3 $tss3 to tss2";
	push @$tss2, @$tss3;                                     # append TS subsequence to overall result
    }
#warn "returning ".Dumper $tss2;
    return $tss2;
}


=pod

=back

=head1 AUTHOR

Robert Barta, E<lt>drrho@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 200[56] by Robert Barta

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut

our $VERSION  = 0.03;
our $REVISION = '$Id: TS.pm,v 1.10 2006/12/23 10:37:08 rho Exp $';

1;

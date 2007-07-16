package TM::Serializable::AsTMa;

use Class::Trait 'base';
use Class::Trait 'TM::Serializable';

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

This method serialized the map object into AsTMa notation and returns the resulting string.  It will
raise an exception if the object contains constructs that AsTMa cannot represent. The result is a
standard Perl string, so you may need to force it into a particular encoding.

The method understands a number of key/value pair parameters:

=over

=item C<version> (default: C<1>)

Which AsTMa version the result should conform to.

=item C<omit_trivia> (default: C<0>)

This option suppresses the output of completely "naked" topics (topics without any characteristics).

=back

=cut

sub serialize  {
    my ($self,%opts)=@_;
    $opts{version}||=1;
    
    $TM::log->logdie(scalar __PACKAGE__ .": serialization not implemented for AsTMa version ".$opts{version} )
	if ($opts{version} ne 1);

    my $base=$self->{baseuri};
    # the work stats: collect info from the assertions.
    my (%topics,%assocs);
    for my $m ($self->match(TM->FORALL))
    {
	my $kind=$m->[TM->KIND];
	my $type=$m->[TM->TYPE];
	my $scope=$m->[TM->SCOPE];
	$scope=~s/^$base//;
	undef $scope if ($scope eq "us");
	$type=~s/^$base//;
	my $lid=$m->[TM->LID];

	if ($kind==TM->ASSOC) 
	{
	    if ($type eq "isa")
	    {
		my ($p,$c);
		$p=($self->get_x_players($m,$base."class"))[0];
		$c=($self->get_x_players($m,$base."instance"))[0];
		$p=~s/^$base//;
		$c=~s/^$base//;
		push @{$topics{$p}->{children}},$c;
		push @{$topics{$c}->{parents}},$p;
	    }
	    else
	    {
		my %thisa;
		$thisa{type}=$type;
		$thisa{scope}=$scope;
		$thisa{reifies}=$self->midlet($lid)->[TM->ADDRESS];
		
		for my $role (@{$self->get_role_s($m)})
		{
		    my $rolename=$role;
		    $rolename=~s/^$base//;
		    # must prime the array...
		    $thisa{roles}->{$rolename}=[];
		    
		    for my $player ($self->get_x_players($m,$role))
		    {
			$player=~s/^$base//;
			push @{$thisa{roles}->{$rolename}}, $player;
		    }
		}
		$assocs{$lid}=\%thisa;
	    }
	}
	elsif ($kind==TM->NAME)
	{
	    my $name=($self->get_x_players($m,$base."thing"))[0];
	    $name=~s/^$base//;
	    undef $type if ($type eq "name");
	    
	    die "astma 1.x does not offer reification of basenames (topic $name)\n"
		if ($self->midlet($lid)->[TM->ADDRESS]);
	    
	    for my $p ($self->get_x_players($m,$base."value"))
	    {
		next if (!($p->[0]));
		push @{$topics{$name}->{bn}}, [ $p->[0], $type, $scope ];
	    }
	}
	elsif ($kind==TM->OCC)
	{
	    my $key="oc";
	    my $name=($self->get_x_players($m,$base."thing"))[0];
	    $name=~s/^$base//;
	    
	    die "astma 1.x does not offer reification of occurrences (topic $name)\n"
		if ($self->midlet($lid)->[TM->ADDRESS]);
	    
	    undef $type if ($type eq "occurrence");
	    for my $p ($self->get_x_players($m,$base."value"))
	    { 
		next if (!($p->[0]));
		$key="in" if ($p->[1] eq XSD."string");
		push @{$topics{$name}->{$key}}, [ $p->[0], $type, $scope ];
	    }
	}
    }
    
    # then from the topics
    # we also need to run this part because of all the reification-crap...
    # uuuggly distinction between topics and assertions
    for my $t (grep(!$self->retrieve($_), $self->midlets))
    {
	my $tn=$t;
	$tn=~s/^$base//; 
	$topics{$tn}||={}; 
	
	die "astma 1.x does not offer variants (topic $tn)\n"
	    if ($self->variants($t));
	
	my $reifies=$self->midlet($t)->[TM->ADDRESS];
	if ($reifies)
	{
	    if ($self->retrieve($reifies)) 
	    {
		# the topic reifies a PARTICULAR assoc, thus this must 
		# be attached as passive reification to the assoc
		$assocs{$reifies}->{reifiedby}=$tn;
	    }
	    else
	    {		       
		# active reification
		$topics{$tn}->{reifies}=$reifies;
	    }
	}
	my $sin=$self->midlet($t)->[TM->INDICATORS];
	if ($sin && @$sin)
	{
	    $topics{$tn}->{sin}=$sin;
	}
    } 
    
    # finally the actual dumping of the information
    my @result=("# serialized object, originally from ".($self->{url}=~/^inline:/?"inline":$self->{url}),
		"# base $base","");
    for my $t (sort keys %topics)
    {
	my $tn=$topics{$t};
	next if ($opts{omit_trivia} && !keys %$tn);

	push @result, 
	($tn->{parents}? "$t (".join(" ",@{$tn->{parents}}).")":$t)
	    .($tn->{reifies}?(" reifies ".$tn->{reifies}):"");
	
	for my $k (qw(bn in oc))
	{
	    if ($tn->{$k})
	    {
		push @result, 
		map {  $k.($_->[2]?("@ ".$_->[2]):"")
			   .($_->[1]?("(".$_->[1].")"):"").": ".$_->[0] } 
		(@{$tn->{$k}});
	    }
	}
	if ($tn->{sin})
	{
	    map { push @result, "sin: ".$_; } (@{$tn->{sin}});
	}
	push @result,"";
    }

    map 
    {
	my $a=$assocs{$_};
	push @result,"(".$a->{type}.")"
	    .($a->{scope}?(" @".$a->{scope}):"")
	    .($a->{reifies}?(" reifies ".$a->{reifies}):"")
	    .($a->{reifiedby}?(" is-reified-by ".$a->{reifiedby}):"");
	map 
	{ 
	    push @result, 
	    "$_: ".join(" ",@{$a->{roles}->{$_}});  
	} (sort keys %{$a->{roles}});
	push @result,"";
    } (sort { $assocs{$b}->{type} cmp $assocs{$a}->{type}} keys %assocs);
    
    return join("\n",@result)."\n";
}

=pod

=back

=head1 SEE ALSO

L<TM>, L<TM::Serializable>

=head1 AUTHOR INFORMATION

Copyright 200[1-6], Robert Barta <drrho@cpan.org>, Alexander Zangerl <he@does.not.want.his.email.anywhere>, All rights reserved.

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself.  http://www.perl.com/perl/misc/Artistic.html

=cut

our $VERSION  = '0.3';
our $REVISION = '$Id: az';

1;

__END__

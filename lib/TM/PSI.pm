package TM::PSI;

=pod

=head1 NAME

  TM::PSI - Topic Map management, (published) subject identifiers

=head1 DESCRIPTION

???

@@@@

=cut

our $core = { # this makes the TM::Store work
    mid2iid => {
	'thing'          => [ 'http://virtual.cvut.cz/kifb/en/concepts/_entity.html' ],
#            'assertion'      => \ 'http://psi.tm.bond.edu.au/pxtm/1.0/#assertion',
        'assertion-type' => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/#assertion-type' ],
	'is-subclass-of' => [ 'http://psi.topicmaps.org/sam/1.0/#supertype-subtype',
			      'http://www.topicmaps.org/xtm/#psi-superclass-subclass' ],
	'isa'            => [ 'http://psi.topicmaps.org/sam/1.0/#type-instance',
			      'http://www.topicmaps.org/xtm/core.xtm#class-instance' ],
	'class'          => [ 'http://psi.topicmaps.org/sam/1.0/#type',
			      'http://www.topicmaps.org/xtm/core.xtm#class' ],
	'instance'       => [ 'http://psi.topicmaps.org/sam/1.0/#instance',
			      'http://www.topicmaps.org/xtm/core.xtm#instance' ],
	'superclass'     => [ 'http://psi.topicmaps.org/sam/1.0/#supertype',
			      'http://www.topicmaps.org/xtm/#psi-superclass' ],
	'subclass'       => [ 'http://psi.topicmaps.org/sam/1.0/#subtype',
			      'http://www.topicmaps.org/xtm/#psi-subclass' ],
	'scope'          => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/#scope' ],
	'us'             => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-universal-scope' ],

    },
    assertions => [
#		   [ 'isa', [ 'class', 'instance' ], [ 'ontology', 'astma' ] ],
#		   [ 'isa', [ 'class', 'instance' ], [ 'ontology', 'xsd' ] ],

		   [ 'isa', [ 'class', 'instance' ], [ 'scope', 'us' ] ],
		   [ 'isa', [ 'class', 'instance' ], [ 'assertion-type', 'isa' ] ],
		   [ 'isa', [ 'class', 'instance' ], [ 'assertion-type', 'is-subclass-of' ] ],
		   [ 'is-subclass-of', [ 'subclass', 'superclass' ], [ 'assertion-type',    'class' ] ],
		   ],
};

our $topicmaps_inc = {
    mid2iid => {
	'xtm-topic'              => [ 'http://www.topicmaps.org/xtm/1.0/#psi-topic' ],
	'association'            => [ 'http://psi.topicmaps.org/sam/1.0/#association',
				      'http://www.topicmaps.org/xtm/1.0/#psi-association' ],
	'sort'                   => [ 'http://psi.topicmaps.org/sam/1.0/#sort',
				      'http://www.topicmaps.org/xtm/#psi-sort' ],
	'display'                => [ 'http://psi.topicmaps.org/sam/1.0/#display',
				      'http://www.topicmaps.org/xtm/#psi-display' ],
	'characteristic'         => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/characteristic'],
	'unique-characteristic'  => [ 'http://psi.topicmaps.org/sam/1.0/#unique-characteristic'],
	'basename'               => [ 'http://psi.topicmaps.org/sam/1.0/#topic-name'],
	'xtm-psi-occurrence'     => [ 'http://www.topicmaps.org/xtm/1.0/#psi-occurrence' ],
	'variant'                => [ 'http://psi.topicmaps.org/sam/1.0/#variant'],
	'occurrence'             => [ 'http://psi.topicmaps.org/sam/1.0/#occurrence',
				      'http://www.topicmaps.org/xtm/1.0/#psi-occurrence' ],
	'association-role'       => [ 'http://psi.topicmaps.org/sam/1.0/#association-role' ],
	
#	'has-data-occurrence'    => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-has-data-occurrence' ],
#	'has-uri-occurrence'     => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-has-uri-occurrence' ],
	'name'                   => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/name' ],

# astma stuff
        'ontology'       => [ 'http://topicmaps.bond.edu.au/astma/2.0/#ontology' ],
        'template'       => [ 'http://topicmaps.bond.edu.au/astma/2.0/#template' ],
        'return'         => [ 'http://topicmaps.bond.edu.au/astma/2.0/#return' ],

    },
    assertions => [
		   [ 'is-subclass-of', [ 'subclass', 'superclass' ], [ 'characteristic',        'association' ] ],
		   [ 'is-subclass-of', [ 'subclass', 'superclass' ], [ 'occurrence',            'characteristic' ] ],
		   [ 'is-subclass-of', [ 'subclass', 'superclass' ], [ 'unique-characteristic', 'characteristic' ] ],
		   [ 'is-subclass-of', [ 'subclass', 'superclass' ], [ 'name',                  'characteristic' ] ],
		   
#		   [ 'is-subclass-of', [ 'subclass', 'superclass' ], [ 'has-data-occurrence',   'occurrence' ] ],
#		   [ 'is-subclass-of', [ 'subclass', 'superclass' ], [ 'has-uri-occurrence',    'occurrence' ] ],
		   ],
};

our $topicmaps; # = core + topicmaps_inc
%{$topicmaps->{mid2iid}}    = (%{$core->{mid2iid}},    %{$topicmaps_inc->{mid2iid}});
@{$topicmaps->{assertions}} = (@{$core->{assertions}}, @{$topicmaps_inc->{assertions}});

our @Usual_Suspects = ('thing', 'is-subclass-of', 'subclass', 'superclass', 'isa', 'instance', 'class', 'us');

=pod

=head1 SEE ALSO

L<TM>

=head1 AUTHOR INFORMATION

Copyright 200[1-6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

  L<http://www.perl.com/perl/misc/Artistic.html>

=cut

our $VERSION  = '0.12';
our $REVISION = '$Id: PSI.pm,v 1.22 2006/09/14 10:37:46 rho Exp $';

1;

__END__

our %PSIs = (
# core
	     'xtm-psi-topic'               => 'http://www.topicmaps.org/xtm/1.0/#psi-topic',
	     'xtm-psi-association'         => 'http://www.topicmaps.org/xtm/1.0/#psi-association',
	     'is-a'                        => 'http://www.topicmaps.org/xtm/core.xtm#class-instance',
	     'class'                       => 'http://www.topicmaps.org/xtm/core.xtm#class',
	     'instance'                    => 'http://www.topicmaps.org/xtm/core.xtm#instance',
	     'is-subclass-of'              => 'http://www.topicmaps.org/xtm/#psi-superclass-subclass',
	     'superclass'                  => 'http://www.topicmaps.org/xtm/#psi-superclass',
	     'subclass'                    => 'http://www.topicmaps.org/xtm/#psi-subclass',
	     'xtm-psi-sort'                => 'http://www.topicmaps.org/xtm/#psi-sort',
	     'xtm-psi-display'             => 'http://www.topicmaps.org/xtm/#psi-display',

# Perl TM extensions
	     'universal-scope'             => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-universal-scope',
	     'basename'                    => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-basename',
	     'name'                        => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-name',
	     'has-indicator'               => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-has-indicator',
	     'subject-indicator'           => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-subject-indicator',
	     'is-reified-by'               => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-is-reified-by',
	     'reified'                     => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-reified',
	     'reifier'                     => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-reifier',
	     'has-data-occurrence'         => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-has-data-occurrence',
	     'has-uri-occurrence'          => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-has-uri-occurrence',

# AsTMa extensions
	     'sum-ergo-sum'                => 'http://psi.tm.bond.edu.au/astma/1.0/#psi-sum-ergo-sum',
	     'regexp'                      => 'http://psi.tm.bond.edu.au/astma/1.0/#psi-regexp',
	     'validates'                   => 'http://psi.tm.bond.edu.au/astma/1.0/#psi-validates',
	     'left'                        => 'http://psi.tm.bond.edu.au/astma/1.0/#psi-left',
	     'right'                       => 'http://psi.tm.bond.edu.au/astma/1.0/#psi-right',

);

our @NATURAL_CONSTANTS = qw(
			    thing
			    universal-scope
			    is-a
			    instance
			    class
			    is-subclass-of
			    superclass
			    subclass
			    xtm-psi-association
			    );



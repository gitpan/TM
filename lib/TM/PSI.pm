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

	'topicmap'       => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-topicmap' ],

    },
    assertions => [
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
	'xtm-psi-occurrence'     => [ 'http://www.topicmaps.org/xtm/1.0/#psi-occurrence' ],
	'variant'                => [ 'http://psi.topicmaps.org/sam/1.0/#variant'],
	'occurrence'             => [ 'http://psi.topicmaps.org/sam/1.0/#occurrence',
				      'http://www.topicmaps.org/xtm/1.0/#psi-occurrence' ],
	'association-role'       => [ 'http://psi.topicmaps.org/sam/1.0/#association-role' ],
	
	'name'                   => [ 'http://psi.tm.bond.edu.au/pxtm/1.0/name' ],

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

our $astma_inc = {
    mid2iid => {
	'thing'          => [ 'http://virtual.cvut.cz/kifb/en/concepts/_entity.html' ],
        'value'          => [ 'http://psi.tm.bond.edu.au/astma/2.0/#thing' ],
        'ontology'       => [ 'http://psi.tm.bond.edu.au/astma/2.0/#ontology' ],
        'implementation' => [ 'http://psi.tm.bond.edu.au/astma/2.0/#implementation' ],
        'template'       => [ 'http://psi.tm.bond.edu.au/astma/2.0/#template' ],
        'return'         => [ 'http://psi.tm.bond.edu.au/astma/2.0/#return' ],
        'body'           => [ 'http://psi.tm.bond.edu.au/astma/2.0/#body' ],
    },
    assertions => [
		   ],
};

our $tmql_inc = {
    mid2iid => {
#	'function'       => [ 'http://www.isotopicmaps.org/tmql/#function' ],
    },
    assertions => [
		   ],
};


our $topicmaps;              # default set = core + topicmaps_inc + astma_inc
%{$topicmaps->{mid2iid}}    = (%{$core         ->{mid2iid}},    
			       %{$topicmaps_inc->{mid2iid}},
			       %{$tmql_inc     ->{mid2iid}},
			       %{$astma_inc    ->{mid2iid}}
			       );
@{$topicmaps->{assertions}} = (@{$core         ->{assertions}},
			       @{$topicmaps_inc->{assertions}},
			       @{$tmql_inc     ->{assertions}},
			       @{$astma_inc    ->{assertions}}
			       );

our @Usual_Suspects = ('thing', 'is-subclass-of', 'subclass', 'superclass', 'isa', 'instance', 'class', 'us', 'name', 'value');

use constant {
    TOPICMAP => 'http://psi.tm.bond.edu.au/pxtm/1.0/#psi-topicmap'
};

=pod

=head1 SEE ALSO

L<TM>

=head1 AUTHOR INFORMATION

Copyright 200[1-6], Robert Barta <drrho@cpan.org>, All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

  L<http://www.perl.com/perl/misc/Artistic.html>

=cut

our $VERSION  = '0.14';
our $REVISION = '$Id: PSI.pm,v 1.25 2006/09/24 08:27:27 rho Exp $';

1;

__END__

=pod

use constant ONTOLOGY => '
tau-object
bn: Tau Object
sin: http://astma.it.bond.edu.au/ns/tau/1.0/object

tau-map subclasses tau-object
bn: map
sin: http://astma.it.bond.edu.au/ns/tau/1.0/map

tau-ontology subclasses tau-object
bn: ontology
sin: http://astma.it.bond.edu.au/ns/tau/1.0/ontology

tau-query subclasses tau-object
bn: query
sin: http://astma.it.bond.edu.au/ns/tau/1.0/query

tau-collection subclasses tau-object
bn: collection
sin: http://astma.it.bond.edu.au/ns/tau/1.0/collection

';



#	'sum-ergo-sum'                => [ 'http://psi.tm.bond.edu.au/astma/1.0/#psi-sum-ergo-sum'],
#	'regexp'                      => [ 'http://psi.tm.bond.edu.au/astma/1.0/#psi-regexp'],
#	'validates'                   => [ 'http://psi.tm.bond.edu.au/astma/1.0/#psi-validates'],
#	'astma-left'                  => [ 'http://psi.tm.bond.edu.au/astma/1.0/#psi-left'],
#	'astma-right'                 => [ 'http://psi.tm.bond.edu.au/astma/1.0/#psi-right'],



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



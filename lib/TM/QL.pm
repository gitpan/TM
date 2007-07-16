package TM::QL;

use strict;

use Data::Dumper;
use Class::Struct;

use TM::Literal;
use TM::QL::TS;
use TM::QL::PE;

=pod

=head1 NAME

TM::QL - Topic Maps, Query Language Expressions

=head1 SYNOPSIS

   use TM::QL;
   # compile a TMQL query (here path expression style)
   my $q = new TM::QL ('// hypocrite [ . <- employee -> organisation == $org ]');

   use TM;
   my $tm = new TM (....);                         # get a map from somewhere

   my $or  = $tm->mids ('vagabond-university');    # find internal id
   my $ts = $q->eval ('%_' => $tm,                 # explicitly pass in a default map
                      '$o' => $or);                # and any number of other variable bindings

   foreach my $tuple (@$ts) {                      # iterate through the tuple sequence
      foreach my $value (@$tuple) {                # do something with the tuple
        print ref ($value) 
                 ? $value->[0]                     # literal
                 : $value                          # internal map identifier
      }
   }

=head1 DESCRIPTION

This class implements TMQL expression handling. A TMQL expression can be parsed, stored in an
object, optimized and then evaluated. The results of this process is a tuple sequence as described
in

    http://topicmaps.it.bond.edu.au/junk/tmql.html

B<NOTE>: This is work in progress, so there might be discrepancies.

=cut

our $grammar = q{

    {
	use Data::Dumper;

	sub _mk_fun_tree {                                                                    # produces binary function tree from...
	    my @item = @_;                                                                    # ...a list of items: param op param op param ...., 
	    my $d1 = shift @{$item[1]};                                                       # get the first param
	    while (my $op = shift @{$item[1]}) {                                              # as long as there is another op/param
		my $d2  = shift @{$item[1]};                                                  # capture the param
		$d1     = new PEfun (fun  => $op || die "unregistered function '$item[1]'",   # create a function node, with the params....
				     args => [ $d1, $d2 ],                                    # ... being the arguments
				     uri  => undef);
	    }
	    return $d1;                                                                       # finally return the root of the tree
	}

	sub _mk_if {                                                                          # creates if node
	    my $cond = shift || TM::QL::PE::mk_prs (new PEall);                               # with the condition variable, if (%_) then prid else prno if is default
	    return new PEif (con  => $cond,
			     then => TM::QL::PE::mk_prs (new PEvar (nam => '@_')),            # pass all
			     else => TM::QL::PE::mk_prs (undef));                             # pass nothing
	}

	sub _invert_if {                                                                         # inverts an if node...
	    my $if = shift;
	    my ($then, $else) = ($if->then, $if->else);
	    $if->then ($else); $if->else ($then);                                             # by swapping 'then' with 'else'
	    return $if;
	}

	my @metamaps;                                                                         # this will contain a list of ontologies
    }

    startrule                 : query_expression

#-- top-level ------------------------------------------------------------------------------------------------------

    content                   : content_l0(s /(--)/)                  { $return = TM::QL::PE::mk_prs ($item[1]); }

    content_l0                : content_l1(s /(\+\+)/)

    content_l1                : content_l2(s /(==)/)

    content_l2                : '{' query_expression '}'              { $return = $item[2]; } # unconditional
                              | 'if'     path_expression                                      # conditional
                                  'then' content
                                ( 'else' content )(?)                 { $return = TM::QL::PE::mk_prs (new PEif (con  => $item[2],
														then => $item[4],
														else => $item[5]->[0] ? $item[5]->[0]
														                      : TM::QL::PE::mk_prs ())); }
                              | tm_content
                              | xml_content
# geht nicht                  | simple_content                        { $return = TM::QL::PE::mk_prs ($item[1]); } 

    query_expression          : environment_clause(?)
                                ( select_expression
                                | flwr_expression
                                | path_expression )

    environment_clause        : tm_content                                                    # TODO: #{ # merge this with the context map }

#-- SELECT ---------------------------------------------------------------------------------------------------------

    select_expression         : select_clause
                                from_clause(?)
                                where_clause(?)                       {
				                                        $return = $item[1];
									my $where = $item[3]->[0] || TM::QL::PE::mk_prs ( _mk_if() );  # get an 'if', default is 'true'
									my @frees = TM::QL::PE::find_free_vars ($where);               # find _free_ vars in where
									$return = TM::QL::PE::unshift_vars ([['@_' => $where]], $return);
									$return = TM::QL::PE::unshift_vars ([
                                                                                   map {                                               # and keep these assignments
									                [ $_ => TM::QL::PE::mk_prs (new PEall) ]     # create pairs var => %_ all of them
										        } @frees ], $return);                          # and push them
								       }
#  order_by_clause(?)
#  unique_clause(?)
#  offset_clause(?)
#  limit_clause(?)

    select_clause             : 'select' value_expression(s? /,/)     { $return = TM::QL::PE::mk_prs (new PEpr (arr => [ map { [[[ $_ ]]] } @{$item[2]} ]) ); }

    from_clause               : 'from' '%_'  # TODO
#   from_clause               : 'from' value_expression # TODO

#   order_by_clause           : 'order' 'by' value_expression(s? /,/) # TODO

#   unique_clause             : 'unique'

#   offset_clause             : 'offset' value_expression

#   limit_clause              : 'limit' value_expression

#-- FLWR expressions -----------------------------------------------------------------------------------------------

    flwr_expression           : for_clause(s?)
                                where_clause(?)
 # TODO                         order_by_clause(?) # TODO
                                return_clause                         {
                                                                        $return = $item[3];                                        # first collect the return clause
									$return = TM::QL::PE::unshift_vars ([['@_' => $item[2]->[0]]],
													    $return)               # merge it in
									    if $item[2]->[0];                                      # if there is a where clause,
									my %vars;                                                  # registers seen variables
									my @fors = reverse                                         # outest variable first
									    map {    ($_->[0] = $vars{ $_->[0] } ? '$_' : $_->[0]) # if variable is seen inside => $_
										 and ++$vars{ $_->[0] }                            # mark variable as seen
									         and $_ }                                          # and forward the whole pair
									    reverse                                                # start from the end
									    map { @$_ }                                            # unwrap double nested list
									@{$item[1]};                                 # take the var-assocs [[ $a => ...],[ $b => ..] ]
									$return = TM::QL::PE::unshift_vars (\@fors, $return);      # and push them
								       }

    for_clause                : 'for' variable_association(s /,/)     { $return = $item[2]; }

##  variable_association      : variable 'in' path_expression         { $return = [ $item[1]->nam => $item[3] ]; }
    variable_association      : variable 'in' content                 { $return = [ $item[1]->nam => $item[3] ]; }

    return_clause             : 'return' content

#-- WHERE, EXISTS, FORALL, ... -------------------------------------------------------------------------------------

    where_clause              : 'where' boolean_expression            { $return = TM::QL::PE::mk_prs ($item[2]); }         # wrap PEif into a PEprs

    boolean_expression        : boolean_expression_or(s /\|/)         { 
                                                                        my @ors = map { [ $_ ] } @{$item[1]};              # every list element is a PEprs
									@ors = ( $ors[0],                                  # wrap it with [] for ===
										 map { ('++', $_) }  @ors[1..$#ors] );     # and squeeze in ++ before every element
									$return = _mk_if (TM::QL::PE::mk_prs ([ \@ors ]));
								       }
    boolean_expression_or     : boolean_primitive(s     /\&/)         { $return = PEprs->new (arr => $item[1]); }
    boolean_primitive         : 'not' boolean_primitive               { $return = _invert_if ($item[2]); }
                              | 'false'                               { $return = _mk_if (TM::QL::PE::mk_prs ()); }
                              | '(' boolean_expression ')'            { $return = $item[2]; }
                              | forall_clause                         { $return = _invert_if (_mk_if ($item[1])); }           # make NOT here
                              | exists_clause                         { $return = _mk_if ($item[1]); }

    exists_clause             : 'some'      variable_association(s)
                                'satisfies' boolean_expression        { 
									$return = TM::QL::PE::mk_prs ($item[4]);           # build prototype
									$return = TM::QL::PE::unshift_vars ($item[2], $return);
								       }
    forall_clause             : 'every'     variable_association(s /,/)
                                'satisfies' boolean_expression        {                                                    # forall A satisfies B = !some A satisfy not B
									$return = TM::QL::PE::mk_prs (_invert_if ($item[4])); # negate boolean expression
									$return = TM::QL::PE::unshift_vars ($item[2], $return);
									
							               }

#-- path expressions -------------------------------------------------------------------------------------------------------------

    path_expression           : association_predicate
                              | path_l0_expression

    path_l0_expression        : ( tuple_expression | simple_content )
                                postfix(s?)                           { 
	                                                                my @PRs = map { TM::QL::PE::mk_prs ($_) } ($item[1], @{ $item[2] });
									$return = pop @PRs;
									while (my $q = pop @PRs) { # start from the and and
									    $return = TM::QL::PE::unshift_vars ([['@_' => $q]], $return); # unshift 
									}
									$return;
								        }
    postfix                   : predicate_postfix
                              | projection_postfix

    predicate_postfix         : '[' boolean_primitive ']'             { $return = $item[2]; }                # one PEif with the boolean as PEprs condition

    projection_postfix        : tuple_expression

    simple_content            : anchor navigation(?)                  { $return = new PEpr (arr => [ [[[ new PEpe (val => $item[1], mos => $item[2]->[0] || []) ]]]]); }

    navigation                : step navigation(?)                    { $return = [ $item[1], $item[2]->[0] ? @{$item[2]->[0]} : () ];}   # and append the tail

    step                      : ( '<<' | '>>' ) axis item_reference(?){ $return = new PEna (axi => $item[2],                              # build the navigation
											    dir => $item[1] eq '>>',
											    tid => $item[3]->[0] ? $item[3]->[0] : undef);
								        }
    axis                      : 'epsilon'
                              | 'classes'
                              | 'superclasses'
                              | 'players'
                              | 'roles'
                              | 'characteristics'
                              | 'scope'
                              | 'reifier'
                              | 'atomify'
                              | 'locators'
                              | 'indicators'

#-- association predicate --------------------------------------------------------------------------------------------------------

    association_predicate     : { undef; } item_reference '(' roles ellipsis(?) ')' # TODO: not yet implemented

#path_l0_expression 'is-a' path_l0_expression

    roles                     : role(s? /,/) 

    role                      : role_type ':' player

    role_type                 : item_reference

    player                    : path_expression

    ellipsis                  : ',' '...'

#-- tuple expressions and values -----------------------------------------------------------------------------------------------

    tuple_expression          : '(' value_expression(s? /,/) ')'      { $return = new PEpr (arr => [ map { [[[ $_ ]]] } @{$item[2]} ]); }

    value_expression          : value_l0_expression
# TODO: [ order_direction ]
#   order_direction           : 'asc' | 'desc'

    value_l0_expression       : value_l1_expression(s /(<|<=|=>|>)/)  { $return = _mk_fun_tree (@item); }

    value_l1_expression       : value_l2_expression(s /([+-])/)       { $return = _mk_fun_tree (@item); }
# TODO: infix and prefix ops, are there more?
# TODO - and no blanks and then number is literal!
                              | /-/ value_l2_expression               {
                                                                        $return = new PEfun (fun  => 'tmql:unary-minus',
											     args => [ $item[2] ],
											     uri  => undef);
                                                                       }

    value_l2_expression       : value_l3_expression(s /(\*|div|mod)/) { $return = _mk_fun_tree (@item); }

    value_l3_expression       : content

# TODO: | function_invocation (first)

#-- function invocation ---------------------------------------------------------------------------------------------------------

    function_invocation       : item_reference ( tuple_expression | roles )
#TODO

#-- variables ------------------------------------------------------------------------------------------------------

    variable                  : '%_'                                  { $return = new PEall; }
                              | '@_'                                  { $return = new PEvar (nam => $item[1]); }
                              | '$_'                                  { $return = new PEvar (nam => $item[1]); }
                              | /(\$\d+)/                             { $return = new PEvar (nam => $1); }
                              | /[%@\$][\w\#\_][\w\-\.]*/             { $return = new PEvar (nam => $item[1]); }
#                                      ^^^^^^^^^^^^^^^^^ identifier pattern

#-- simple content -------------------------------------------------------------------------------------------------------------

    anchor                    : constant 
                              | variable

    constant                  : literal
                              | item_reference                        { $return = new PEti (tid => $item[1]); }

    item_reference            : identifier

#    item_reference            : prefix(?) identifier                  { $return = $item[1]->[0] ? $item[1]->[0] . $item[2] : $item[2]; }

    identifier                : /([\w\#\_][\w\-\.]*)/


#    item_reference            : (/[=~]/)(?)  uri_or_qname             { $return = ($item[1]->[0] and $item[1]->[0] eq '~')
#									    ? \ $item[2]              # a reference indicates, uhm, indication :-)
#									    : $item[2];               # otherwise it is a subject locator
#                                                                        }
                              | '*'                                   { $return = 'thing'; }
# TODO: as shorthand

    uri_or_qname              : prefix(?) identifier                  { $return = $item[1]->[0] ? $item[1]->[0] . $item[2] : $item[2]; }
##                              | uri                                      

    prefix                    : /\w+/ ':'                             {
                                                                        my @prefixes; ## TODO prefixes
	                                                                $return = undef;             # pessimism rules this planet
									foreach my $prefix (@prefixes) {
									    if ($prefix->{$item[1]}) {
										$return = $prefix->{$item[1]}->[1]; # URI for prefix
										last;
									    }
									}
									1;                           # parsing is ok otherwise
								      }

#-- tm content  --------------------------------------------------------------------------------------

    tm_content                : '"""' ctm_instance '"""'              { $return = $item[2]; }  # TODO: reinstate original SKIP, reinstate orig meta map

    ctm_instance              : astma_instance                        # for the time being

    astma_instance            : 'xxx'                                 # TODO !!!!

#-- XML content -------------------------------------------------------------------------------------

    xml_content               : <skip:""> /\s*/ xml_element(s) /\s*/  {
                                                                        $return = TM::QL::PE::mk_prs (
												      new PExml (con => [ new TM::Literal ($item[2]),
															  @{$item[3]},
															  new TM::Literal ($item[4])
															  ])
												      );
								      }

    xml_element               : '<' xml_id xml_attribute(s?) xml_rest
                                                                      {
                                                                        if ($item[4] eq '/>') {                  # no end tag
									    $return = new PExml (sta => $item[2],
												 ats => $item[3]);
									} else {
									    $return = new PExml (sta => $item[2],
												 ats => $item[3],
												 end => $item[4]->[1],
												 con => $item[4]->[0]);
									}
								      }
    xml_id                    : /[:\w]+/

    xml_attribute             : <skip:'\s*'> xml_id '=' '"' <skip:""> xml_fragment['[^\"\{]+'](s) '"'
                                                                      { $return = [ $item[2], $item[6] ]; }

    xml_rest                  : '/>'
                              | '>' <skip:""> xml_segment(s) '</' xml_id '>'
                                                                      { $return = [ $item[3], $item[5] ]; }

    xml_segment               : xml_element | xml_fragment['[^<\{]+']


    xml_fragment              : xml_text[$arg[0]]
                              | '{' <skip:'\s*'> query_expression '}' { $return = $item[3]; }

    xml_text                  : /$arg[0]/                             { $return = new TM::Literal ($item[1]); }


#-- auxiliary non terminals to allow for macros to be parsed -----------------------------------------------------
# NOTE: this could be automated further

     item_reference            : '~~~item_reference_1~~~'
                               | '~~~item_reference_op_1~~~' { $return = [ '~~~item_reference_op_1~~~' ]; }
     variable                  : '~~~variable_1~~~'
     constant                  : '~~~constant_1~~~'
     integer                   : '~~~integer_1~~~'
     content                   : '~~~content_1~~~'
     anchor                    : '~~~anchor_1~~~'
     path_expression           : '~~~path_expression_1~~~' | '~~~path_expression_2~~~'
     path_l0_expression        : '~~~path_l0_expression_1~~~'
     navigation                : '~~~navigation_op_1~~~'   { $return = [ '~~~navigation_op_1~~~' ]; }

##   postfix                   : '~~~postfix_op_1~~~'      { $return = [ '~~~postfix_op_1~~~' ]; }
};

my $macros = {

    q{_00_variable            : '.' }                                          => '$0',

    q{_00_tuple_expression    : 'null' }                                       => '()',

# item_reference : '*' => 'tmdm:subject'

    q{_02_navigation          : '<-' ~~~item_reference_1~~~ ~~~navigation_op_1~~~ }  => ' << players ~~~item_reference_1~~~ ~~~navigation_op_1~~~',
    
    q{_02_navigation          : '->' ~~~item_reference_1~~~ ~~~navigation_op_1~~~ }  => ' >> players ~~~item_reference_1~~~ ~~~navigation_op_1~~~',


    q{_03_content_l2          : ~~~path_expression_1~~~ '||' ~~~path_expression_2~~~}
                                                                               => 'if ~~~path_expression_1~~~ then { ~~~path_expression_1~~~ } else { ~~~path_expression_2~~~ }',

    q{_04_content_l2          : ~~~path_expression_1~~~ }                      => '{ ~~~path_expression_1~~~ }',


    
    q{_04_exists_clause       : 'exists' ~~~content_1~~~ }                     => ' some $_ in { return ~~~content_1~~~ } satisfies not false',

    q{_05_exists_clause       : ~~~path_l0_expression_1~~~ 'is-a' ~~~anchor_1~~~ }
                                                                               => ' exists ~~~path_l0_expression_1~~~  ==  ~~~anchor_1~~~ << classes * ',
    q{_05_exists_clause       : ~~~path_l0_expression_1~~~ 'iko'  ~~~anchor_1~~~ }
                                                                               => ' exists ~~~path_l0_expression_1~~~  ==  ~~~anchor_1~~~ << superclasses * ',

    q{_06_exists_clause       : ~~~content_1~~~ }                              => ' exists ~~~content_1~~~ ',

    q{_07_predicate_postfix   : '[' ~~~integer_1~~~ ']' }                      => ' [ $# == ~~~integer_1~~~ ] ',

## TODO  [ ~~~integer_1~~~ .. ~~~integer_2~~~ ] => [ $# < ~~~integer_2~~~ ] [ ~~~integer_1~~~ <= $# ]
    
## remove?    q{_05_predicate_postfix   : '[' ~~~path_expression_1~~~ ']' }              => ' [ ~~~path_expression_1~~~ == ~~~path_expression_1~~~ ]',
    
    q{_06_navigation          : '>>' 'instances' ~~~navigation_op_1~~~ }       => ' << classes      ~~~navigation_op_1~~~ ',
    
    q{_06_navigation          : '>>' 'subclasses' ~~~navigation_op_1~~~ }      => ' << superclasses ~~~navigation_op_1~~~ ',

    q{_06_navigation          : '~' ~~~navigation_op_1~~~ }                    => ' << indicators   ~~~navigation_op_1~~~',

    q{_06_navigation          : /=(?!=)/ ~~~navigation_op_1~~~ }               => ' << locators     ~~~navigation_op_1~~~',

    q{_06_navigation          : '^' ~~~navigation_op_1~~~ }                    => ' >> classes      ~~~navigation_op_1~~~',

# TODO: navigation: '????' => '<<< >>> reifier'

    q{_07_boolean_primitive   : '^' ~~~item_reference_1~~~ }                   => ' . >> classes == ~~~item_reference_1~~~ ',
    
    q{_07_boolean_primitive   : '@' ~~~item_reference_1~~~ }                   => ' . >> scope   == ~~~item_reference_1~~~ ',
    
    q{_08_predicate_postfix   : '//' ~~~item_reference_1~~~ }                  => ' [ ^ ~~~item_reference_1~~~ ] ',
    
    q{_08_navigation          : '/'  ~~~item_reference_1~~~ ~~~navigation_op_1~~~}
                                                                               => ' >> characteristics ~~~item_reference_1~~~ >> atomify ~~~navigation_op_1~~~',

    q{_08_navigation          : '\\\\'  ~~~item_reference_1~~~ ~~~navigation_op_1~~~} 
                                                                               => ' << atomify << characteristics ~~~item_reference_1~~~ ~~~navigation_op_1~~~',

## TODO
##  q{_09_path_l0_expression  : '//' ~~~item_reference_1~~~ ~~~postfix_op_1~~~} => ' %_ // ~~~item_reference_1~~~ ~~~postfix_op_1~~~ ',

# @@@, unfortunately this below will not work, because the current algorithm would try to swap then/else inside the constants
#    q{_12_boolean_primitive   : 'forall' ~~~variable_associations_1~~~
#                                'satisfy' ~~~boolean_expression_1~~~}           => ' not some ~~~variable_associations_1~~~ satisfy not ( ~~~boolean_expression_1~~~ )',

};

my $parser;                                                           # will hold a local copy to avoid repeated compilation of the RecDescent parser

sub _init_parser {                                                    #-- initialize a parser object
                                                                      # that parser object is global
                                                                      # instantiate only one if we have not done this before
    eval {                                                            # probe whether there is a precompiled version of the grammar
	require TM::QL::CParser;
	$parser = TM::QL::CParser->new();
    }; if ($@) {                                                      # if not, generate one on the fly
	$TM::log->warn ("could not find precompiled CParser, compiling");
	use Parse::RecDescent;
#	$::RD_TRACE = 1;
	$::RD_HINT = 1;
	$::RD_WARN = 1;
	$parser = new Parse::RecDescent ($grammar . $TM::Literal::grammar)    # TODO add $astma later
	    or $TM::log->logdie (__PACKAGE__ . "Problem in grammar");
    }
    _extend_parser ($parser);                                         # add all the macros/shortcuts
}

sub _extend_parser {                                                  #-- takes a parser as parameter and adds all macros to is, returns this --
    my $parser = shift;

    foreach my $m (sort keys %$macros) {                              # sort them according to _1_..., _2_...
#warn "extending with $m";
	my ($nt, $abbr) = split (/\s*:\s*/, $m, 2);                   # get non terminal and the abbreviated form (only want two fields)

	$nt =~ s/^_\d+_//;                                            # get rid of sorting information
	my $exp = $macros->{$m};                                      # and the long form

	# now find all ==....~~~ in the abbreviated form and count on which position it is
	# this will the be used as $item[1], $item[2] later in the code
	my $pos = 0;          
	my %replaces = map { ++$pos and
                             $_ =~ /^(~~~.+?_op_.+?~~~)/              # for optional items we collect them with $item[3]->[0] ? @{...} : ()
                                ? ($1 => "\$item[$pos]->[0] ? \@{\$item[$pos]->[0]} : ()") 
                                : ( $_ =~ /^(~~~.+?~~~)/              # for non-optional items we just take as they will be delivered
                                       ? ($1 => "\$item[$pos]")
                                       : () )                         # all other things we ignore
                            } split (/\s+/, $abbr);
#warn "replaces". Dumper \%replaces;

#warn "abbr before >>>$abbr<<<";
	$abbr =~ s/(~~~.+?)_(op)_/$1\(\?\)_/g;
##	$exp  =~ s/~~~(.+?)_op_\d+~~~/$1\(\?\)/g;
	$abbr =~ s/(~~~.+?)_(es)_/$1\(s\?\)_/g;
	$abbr =~ s/~~~(.+?)_\d~~~/$1/g;                               # the actual rule only needs the non-terminal, not that ~~~ ~~~ junk
#warn "abbr >>>$abbr<<<";

#warn "suggested: $nt : '$abbr' for '$exp' before parse";
#	$::RD_TRACE = 1;
	# parse the whole thing into a TMQL data structure
	my $data = $parser->$nt (\$exp) or $TM::log->logdie (__PACKAGE__ . ": Problem generating grammar generator");
	                                   $TM::log->logdie (__PACKAGE__ . ": Found unparseable '$exp'")    unless $exp =~ /^\s*$/s;
#warn "converted for $nt  to ".Dumper $data;

	my $code = Data::Dumper->Dump ([$data], ['return']);          # Dump it as $return = ....

#warn "non final code is >>$code<<";
	$code =~ s/\[\s*'(~~~.+?_es_\d+~~~)'\s*\]/$replaces{$1}/sg;   # for non-terms which return lists (s?), find all bogus identifiers and...
	$code =~ s/\[\s*'(~~~.+?_op_\d+~~~)'\s*\]/$replaces{$1}/sg;   # for non-terms which return lists (s?), find all bogus identifiers and...

#	$code =~ s/'(~~~.+?~~~)'/$replaces{$1}/sg;                    # replace it with $item[1], $item[2], ..., here for normal nonterminals (tricky, I know)

	my %seen;
	while ($code =~ /'(~~~.+?~~~)'/ and ++$seen{$1}) {
	    if ($seen{$1} == 1) { # first time
		$code =~ s/'(~~~.+?~~~)'/$replaces{$1}/;              # first one as-is
	    } else {                                                  # the rest have to be cloned (otherwise we would have the same structure twice)
		$code =~ s/'(~~~.+?~~~)'/TM::QL::PE::clone ( $replaces{$1} )/s;
	    }
	}
#warn "final code is >>$code<<";

#warn "adding '$nt : $abbr { $code }'";
#warn "adding '$nt : $abbr'";

	$parser->Extend("$nt : $abbr { $code }");                     # add the whole beast to the grammar
    }
##exit;
}

=pod

=head2 Constructor

Given a TMQL query expression, this constructor will build a compiled version of the query, which
then can be executed with the method C<eval>.

If an already compiled is passed as parameter, the constructor behaves as a cloner. (Note: no deep
copy of the compiled query).

=cut

sub new {
    my $class = shift;
    my $q     = shift;
    my $self;

    if (ref ($q) eq 'PEprs') {                                                 # easy if we already got a compiled query
	$self = bless { src  => undef,
			cq   => $q }, $class;
    } else {                                                                   # otherwise we build from scratch
	$self = bless { src  => $q }, $class;
	$parser    or _init_parser;                                            # make sure that we have a parser
#warn "--------------new $q";
	eval {
#	    $::RD_TRACE = 1;                                                   # turn on parser debugging
	    $self->{cq} = $parser->startrule (\$q);
#	    $::RD_TRACE = 0;                                                   # turn off parser debugging
	    $TM::log->logdie (__PACKAGE__ . ": Found unparseable '$q'")    unless $q =~ /^\s*$/s;
	    $TM::log->logdie (__PACKAGE__ . ": Incomplete input")          unless $self->{cq};
	}; if ($@) {
	    $TM::log->logdie (__PACKAGE__ . ": $@");
	}
# TODO move this into the grammar
#warn "in grammar before optimize ".$self->{src}." = ".TM::QL::PE::prs2str ($self->{cq});
#warn "   ".Dumper $self->{cq};
	TM::QL::PE::optimize ($self->{cq});
#warn "in grammar after optimize".TM::QL::PE::prs2str ($self->{cq});
#warn "   ".Dumper $self->{cq};
    }
    return $self;
}

=pod

=head2 Methods

=over

=item B<eval>

Given a query object, this method will evaluate it according to the TMQL semantics. The result is a
tuple sequence (L<TM::QL::TS>).

Optionally, a hash reference can be passed into this method. It will be used as variable binding.

=cut

sub eval {
    my $q  = shift;
    my $bs = shift; # variable bindings (hash ref)

    return TM::QL::PE::eval ($bs, $q->{cq});
#                            ^^^                                            # create context list and prime it with this one binding
}


=pod

=back

=head1 AUTHOR

Robert Barta, E<lt>drrho@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 200[4-6] by Robert Barta

This library is free software; you can redistribute it and/or modify it under the same terms as Perl
itself, either Perl version 5.8.4 or, at your option, any later version of Perl 5 you may have
available.

=cut

our $VERSION  = 0.05;
our $REVISION = '$Id: QL.pm,v 1.67 2007/01/08 05:36:05 rho Exp $';

1;

__END__


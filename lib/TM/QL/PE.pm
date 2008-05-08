package TM::QL::PE;

use strict;

use Data::Dumper;

use TM::Literal;
use XML::LibXML;
use TM::QL::TS;

use Class::Struct;

struct PEprs  => [ arr => '$', maps => '$' ];                             # contains a list of pr/if nodes, not sure about the maps yet
struct PEpr   => [ arr => '$' ];                                          # general projection
struct PEif   => [ con => '$', then => '$', else => '$' ];                # the if/condition/then/else thing we all love (or hate)
struct PEfun  => [ fun => '$', args => '$', uri  => '$' ];                # a function node, fun says which, uri is the URI for it, otherwise a list of arguments
struct PEpe   => [ val => '$', mos  => '$' ];                             # the value (constant, variable, ....), followed by a list of navigational movements
struct PEna   => [ axi => '$', dir  => '$', tid  => '$' ];                # one navigation has an axis, a direction (in/out) and an optional topic
struct PEvar  => [ nam => '$' ];                                          # a variable with a name, these are only $a, $b, ... variables, not $0, $1 (see PEpi)
struct PEpi   => [ idx => '$' ];                                          # projection variable, $0, $1, ...
struct PEti   => [ tid => '$' ];                                          # an association/topic identifier
struct PEall  => [];                                                      # the universe, read: the whole map
struct PEvoid => [];                                                      # the undefined value (SCHEDULED for removal)

struct PExml  => [
		   sta => '$', # start tag
		   end => '$', # end tag
		   ats => '@', # attributes
		   con => '@', # content
		  ];

struct PEtm   => [];                                                      # TM content

our %FUNS =                                                               # this will contain all functions (uri => sub {})
    %TM::Literal::OPS;

our $grammar = q{

    {	
	use Data::Dumper;
	my $FUNS;
    }

        startrule : { $FUNS = $arg[0]; } prs

        prs       : /__prs_(\d+)__/                                       ## means a whole list of pr's
	          | /__prsc_(\d+)__/                                      ## means a whole list of pr's, but no $0, $1, ... variables in there (constant for evaluation
	          | (/~~/)(?) <leftop: pr0 '.' pr0>              { $return = new PEprs (arr => $item[2],
										       neg => $item[1]->[0] ? 1 : undef); }

        pr0       : /__pr_(\d+)__/                                        ## means a single pr
                  | /__pr0s_(\d+)__/                                      ## means a subsequence of pr0's
##                  | '(...)'                                     { $return = new PEprid; }
                  | '(' pr1(s? /,/) ')'                         { $return = new PEpr (arr => $item[2]); }  # ARRAY
                  | 'if' prs 'then' prs 'else' prs 'fi'         { $return = new PEif (con  => $item[2],
										      then => $item[4],
										      else => $item[6]); }
        pr1       : /__pr1_(\d+)__/
                  | /__pr1s_(\d+)__/
                  | /__pr1sc_(\d+)__/
                  | pr2

	pr2       : <leftop: pr3 /(--)/    pr3>

	pr3       : <leftop: pr4 /(\+\+)/  pr4>

	pr4       : <leftop: pr5 /(==)/    pr5>

        pr5       : uri '(' pr5(s? /,/) ')'                     { $return = new PEfun (fun  => ($FUNS->{$item[1]} || die "unregistered function '$item[1]'"),
										       args => $item[3],
										       uri  => $item[1]); }                        # keep the URI
                  | prs
                  | pe

	pe        : /__pe_(\d+)__/
                  | va mo(s?)                                   { $return = new PEpe  (val  => $item[1], mos => $item[2]);}

	va        : '%_'                                        { $return = new PEall; }
                  | 'void'                                      { $return = new PEvoid; }
                  | '@_'                                        { $return = new PEvar (nam => $item[1]); }
                  | /\$(\d+)/                                   { $return = new PEpi  (idx => $1); }
                  | /\$(\w+)/                                   { $return = "__".$item[1]."__"; }
                  | li

        mo        : ax /:[<>]:/ id                              { $return = new PEna (axi => $item[1],
										      dir => $item[2] eq ':>:',
										      tid => $item[3]);}
        ax        : 'epsilon'
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

        li        : /__lit_(\d+)__/
                  | /__tid_(\d+)__/
	          | /\-?\d+\.\d+/                               { $return = new TM::Literal  ($item[1], 'xsd:decimal'); }       
                  | /\-?\d+/                                    { $return = new TM::Literal  ($item[1], 'xsd:integer'); }
                  | /"([^\"]*)"/ ('^^' uri)(?)                  { $return = new TM::Literal  ($1,       $item[2]->[0] || 'xsd:string'); }
                  | (/[sil]\`/)(?)  id                          { $return = new PEti (tid => $item[2]); }
# TODO: import Literal grammar here?

        id        : uri | /([\w\#\_][\w\-\.]*)/

	uri       : /(\w+:[^\s)\]]+)/
};


our $parser;

sub _init_parser {
    use Parse::RecDescent;
#    $::RD_TRACE = 1;
    $::RD_HINT = 1;    $::RD_WARN = 1;
    $parser = new Parse::RecDescent ($grammar) or $TM::log->logdie (__PACKAGE__ . "Problem in grammar");
}

sub new {
    my $class = shift;
    my $s     = shift;                                                 # a string to be parsed
    $parser    or _init_parser;                                        # make sure that we have a parser

    my $p;                                                             # will hopefully contain internal rep of s
    eval {
#	$::RD_TRACE = 1;
	$p = $parser->startrule (\$s, 1, \%FUNS);
	$TM::log->logdie (__PACKAGE__ . ": Found unparseable '$s'")    unless $s =~ /^\s*$/s;
	$TM::log->logdie (__PACKAGE__ . ": Incomplete input for '$s'") unless $p;
    }; if ($@) {
	$TM::log->logdie (__PACKAGE__ . ": $@");
    }
##warn "expr ".Dumper $pr;
    return $p;
}

sub mk_prs { # put in one value, this make a whole prs
    my $v = shift;

    if (ref ($v) eq 'PEprs') {
	return $v;
    } elsif (ref ($v) eq 'PEprid') {
	return PEprs->new (arr => [ $v ]);
    } elsif (ref ($v) eq 'PEif') {
	return PEprs->new (arr => [ $v ]);
    } elsif (ref ($v) eq 'PEpr') {
	return PEprs->new (arr => [ $v ]);
    } elsif (ref ($v) eq 'ARRAY') { # some operator crap
	return PEprs->new (arr => [ PEpr->new (arr => [ $v ]) ]);
    } elsif (ref ($v) eq 'PExml') {
	return PEprs->new (arr => [ PEpr->new (arr => [ [[[ $v ]]] ]) ] );
    } elsif (! defined $v) { # undefined? => empty prs
	return PEprs->new (arr => [ PEpr->new (arr => [ ]) ]);
    } else { # everything else will just be wrapped
	return PEprs->new (arr => [ PEpr->new (arr => [ [[[ PEpe->new (val => $v, mos => []) ]]]  ]) ]);
    }
}


#-- Stringification ---------------------------------------------------------------------------------------------------------------------------

sub prs2str {
    my $prs = shift;
#warn "dumping ".Dumper $prs;
    return $prs unless ref ($prs);                                        # we also allow to pass in META strings, they should be bounced back as-is
##    return ($prs->neg ? '~~' : '') . join " . ", map { _dump_pr ($_) } @{$prs->arr} ;
    return "{ " . join (" . ", map { _dump_pr ($_) } @{$prs->arr}) . " }" ;

    sub _dump_ve {
	my $n = shift;
	my $rn = ref ($n);

	if ($n =~ /~~~.+~~~/) { # we allow META strings
	    return $n;
	} elsif ($rn eq 'ARRAY') {
#	    return '[' . ( join " ", map { _dump_ve ($_) } (@$n) ) . ']';
	    return ''. ( join " ", map { _dump_ve ($_) } (@$n) ) . '';
	} elsif ($rn eq 'PEprs') {
	    return prs2str ($n);
	} elsif ($rn eq 'PEall') {
	    return '%_';
	} elsif ($rn eq 'PEvoid') {
	    return 'void';
#	} elsif ($rn eq 'PEpi') {
#	    return '$'.$n->low."..".$n->hig;
	} elsif ($rn eq 'PEpi') {
	    return '$_'.$n->idx;
	} elsif ($rn eq 'PEti') {
	    return $n->tid;
	} elsif ($rn eq 'PEpe') {
	    return _dump_ve ($n->val) . ( @{$n->mos} ? " :::" . join (" ", map { ($_->dir? '>>' : '<<').$_->axi.($_->tid ? $_->tid : '??') } @{$n->mos}) : '' );
	} elsif ($rn eq 'PEfun') {
	    return $n->uri . "(" . join (", ", prs2str ($n->args)) . ")";
	} elsif ($rn eq 'PEvar') {
	    return $n->nam;
	} elsif ($rn eq 'PExml') {
	    return "<xml...>" . (join "...", map { _dump_ve ($_) } @{$n->con}  ) . "</xml>";
	} elsif ($rn eq 'TM::Literal') {
	    return $n->[0];
	} elsif (grep ($n eq $_, ('==', '++', '--'))) {
	    return $n;
	} else {
	    return $n;
	}
    }

    sub _dump_pr {
	my $pr = shift;
	if (ref ($pr) eq 'PEif') {
	    return " if ". prs2str ($pr->con) ." then ". prs2str ($pr->then) ." else ". prs2str ($pr->else) ." fi ";
	} elsif (ref ($pr) eq 'PEprid') {
	    return '(...)';
	} elsif (ref ($pr) eq 'PEpr') {
	    return " (". (join ", ", map {_dump_ve ($_) } @{$pr->arr}) . ") ";
	} else {
	    return $pr;
#	    die "unexpected node $pr";
	}
    }
}

#my %clonies;

sub clone {
    my $p = shift;
#    my $h = shift;                                                   # use no-hash of clonies to avoid unnecessary cloning?

#warn "cloning ? $h what $p";
#    return $p if $h and ! $clonies{$p};                              # return the original if there was no previous cloning (just an optimization)
#warn "yes ! cloning what $p";

    my $q;
    eval Data::Dumper->Dump([$p], ['q']);
#    $clonies{$p}++;
    return $q;
}


#-- Variable Management --------------------------------------------------------------------------------------------------------------------------

sub unshift_vars {
    my $fors = shift;
    my $p    = shift; # a PEprs

#warn "in unshift";
    foreach my $as (reverse @$fors) {
	my ($v, $q) = @$as;                                                  # there is ALWAYS only one $var => $pe pair!

#warn " before unshift of $v: ".prs2str ($p) . " \n       with ".prs2str($q);
	_unshift_dollar_0 ($v, 1, $p);                                       # unshift/replace
#warn " after  unshift of $v: ".prs2str ($p);

	my $last = $q->arr->[ $#{$q->arr} ];                                 # find last
	if (ref ($last) eq 'PEif') {
	    push @{$last->then->arr}, @{$p->arr};                            # appends that to then branch
	    push @{$last->else->arr}, @{clone ($p->arr)};                    # and a cloned version to the else branch
	    $p = $q;
	} elsif (ref ($last) eq 'PEpr') {
	    unshift @{$p->arr}, @{$q->arr};                                  # just prepend $q crap to what $p has
	} else {
	    die "unsure what to do with a $last";
	}
#warn " after unshift total: ".prs2str ($p);
    }
    optimize ($p);
#warn " after optimizing: ".prs2str ($p);
    return $p;

    sub _unshift_dollar_0 {
	my $v  = shift;                                               # variable to be replaced by $0
	my $s  = shift;                                               # should we shift right? 1 = yes
	my $n  = shift;                                               # PE* node

	my $rn = ref ($n);                                            # shorthand
#warn "unshifting var $v (shift: $s) in $rn".Dumper $n;

	if ($rn eq 'ARRAY' || $rn eq 'AND' || $rn eq 'OR') {          # here we simply propagate the process downwards, TODO: remove AND, OR
	    map { _unshift_dollar_0 ($v, $s, $_) } @$n;

	} elsif (grep ($n eq $_, ('==', '++', '--'))) {               # null

	} elsif ($rn eq 'PEpr') {                                     # projection
	    _unshift_dollar_0 ($v, 0, $n->arr);                       # handled by ARRAY, might be empty, but that is ok
	    unshift @{$n->arr}, [[[ mk_prs (PEpi->new (idx => 0)) ]]] # prepend $0 into the list
		if $s                                                 # we only do this if we were asked to do it
                   and @{$n->arr}                                     # and this is NOT the empty projection
                   and $v ne '@_';                                    # and it is not @_ that we are pushing

	} elsif      ($rn eq 'PEprs') {                               # a whole prs = pr . if . pr ...
	    my $last = $n->arr->[-1];                                 # get a hold on the last in this sequence
	    map { _unshift_dollar_0 ($v, ($_ != $last), $_) } @{$n->arr};

	} elsif ($rn eq 'PEif') {                                     # we KNOW, that if's will NEVER be a last one => then and else MUST shift
	    _unshift_dollar_0 ($v, 0, $n->con);
	    _unshift_dollar_0 ($v, 0, $n->then);
	    _unshift_dollar_0 ($v, 0, $n->else);

	} elsif ($rn eq 'PEfun') {
	    map { _unshift_dollar_0 ($v, undef, $_) } @$n->args;      # deal with the arguments

	} elsif ($rn eq 'PEpe') {
	    _unshift_dollar_0 ($v, $s, $n->val);                      # leave alone the navigation, only deal with the value

	} elsif ($rn eq 'PEvar') {
	    if ($v eq '@_') {                                         # means: we address $0, $1, ....
		if ($n->nam =~ /\$(\d+)/) {
		    bless $n, 'PEpi';                                 # oh, oh reblessing magic here
		    $n->idx ($1);                                     # make it a PEpi
		}
	    } elsif ($n->nam eq $v) {                                      # this is a variable to be removed/replaced with $0
		bless $n, 'PEpi';                                     # oh, oh reblessing magic here
		$n->idx (0);                                          # make it a PEpi
	    }                                                         # else ignore this variable

	} elsif ($rn eq 'PEpi') {                                     # all $0 becomes $1, $1 -> $2, ....
	    $n->idx ($n->idx + 1) unless $v eq '@_';                  # ....unless we unshift @_

	} elsif ($rn eq 'PExml') {
	    map { _unshift_dollar_0 ($v, undef, $_) } @{$n->con};     # check out the content and do unshift there

	    map { _unshift_dollar_0 ($v, undef, $_) }                 # do the walk
                map { @{ $_->[1] } }                                  # look at the value component, it is also a list
                @{$n->ats};                                           # take all attributes

	} elsif (grep ($rn eq $_, qw(PEti PEall PEvoid))) {           # null;

	} elsif ($rn eq 'TM::Literal') {                              # null;

	} else {
	    die "no node match '$rn' ($n)";
	}
    }
}

sub find_free_vars {                                                  # finds all occurrences of _free_ variables in a PE
    my $n    = shift;                                                 # get node
    my $rn   = ref ($n);                                              # shorthand of what it is

#warn "find vars $rn $n";
    if      ($rn eq 'PEprs') {
	return ( find_free_vars ($n->arr) );
    } elsif (grep ($rn eq $_, ('PEprid'))) {
	return ();
    } elsif ($rn eq 'PEpr') {
	return ( find_free_vars ($n->arr) );
    } elsif ($rn eq 'PEif') {
	return (find_free_vars ($n->con), find_free_vars ($n->then), find_free_vars ($n->else));
    } elsif ($rn eq 'PEfun') {
	return (find_free_vars ($n->args));
    } elsif ($rn eq 'PEpe') {
	return (find_free_vars ($n->val));
    } elsif ($rn eq 'ARRAY') {
	return map { find_free_vars ($_) } @$n;
    } elsif (grep ($n eq $_, ('==', '++', '--'))) {
	return ();
    } elsif ($rn eq 'PEvar') {                                        # this is probably the only relevant place
	return () if $n->nam =~ /(%|\@|\$)_/;                         # unless that name is something special
	return ($n->nam);                                             # take the money and run
    } elsif ($rn eq 'PExml') {
	return ();                                                    # TODO: not sure. Maybe it is cool?
    } elsif ($rn eq 'TM::Literal') {
	return ();
    } elsif (grep ($rn eq $_, qw(PEti PEall PEvoid PEpi PEna))) {
	return ();
    } else {
	warn "no node match '$rn' '$n'"; # safeguard, will go away
	return ();
    }
}

sub find_pis {                                                        # checks whether an expression contains $0, $1, .... things (or also a $# variable)
                                                                      # i.e. whether the evaluation will depend on a given context
    my $n = shift;                                                    # get node
    my $rn   = ref ($n);                                              # shorthand of what it is

    if      ($rn eq 'PEprs') {
	return ( find_pis ($n->arr) );
    } elsif ($rn eq 'PEprid') {
	return ( $n );                                                # OK, probably not really useful, but as long as we only count...
    } elsif ($rn eq 'PEpr') {
	return ( find_pis ($n->arr) );
    } elsif ($rn eq 'PEif') {
	return (find_pis ($n->con), find_pis ($n->then), find_pis ($n->else));
    } elsif ($rn eq 'PEfun') {
	return (find_pis ($n->args));
    } elsif ($rn eq 'PEpe') {
	return (find_pis ($n->val));
    } elsif ($rn eq 'ARRAY') {
	return map { find_pis ($_) } @$n;
    } elsif (grep ($n eq $_, ('==', '++', '--'))) {
	return ();
    } elsif ($rn eq 'PEpi') {                                         # this is probably the only relevant place
	return ($n);                                                  # take the money and run
    } elsif ($rn eq 'PEvar') {
	return $n->nam eq '$#' ? ($n) : ();
    } elsif ($rn eq 'TM::Literal') {
	return ();
    } elsif ($rn eq 'PExml') {
	return find_pis ($n->con),
    } elsif (grep ($rn eq $_, qw(PEti PEall PEvoid PEvar PEna))) {
	return ();
    } else {
	warn "no node match '$rn' '$n'"; # safeguard, will go away
	return ();
    }
}


#-- Optimizer ---------------------------------------------------------------------------------------------------------------------------------

our %rules;

sub _init_rules {
    foreach my $r (sort 
			 '002NEST     : ( ( __pe_1__ ) )                                              ===>   ( __pe_1__ )',
			 '003NEST     : ( ( __pe_1__ ) == ( __pe_2__) )                               ===>   ( __pe_1__ == __pe_2__ )',
			 '004NEST     : (__pr1s_1__, ( __pr1s_2__ ), __pr1s_3__)                      ===>   (__pr1s_1__, __pr1s_2__ , __pr1s_3__)',
			 '005NEST     : ( (%_) ++ __pr_1__ )                                          ===>   ( %_ ++ __pr_1__ )',

#'006NEST : ( @_ ) ===> (...)',
#'007NEST : ((...)) ===> (...)',


			 '010EMPTY    : __pr_1__ . ()                                                 ===>   ()',
			 '011EMPTY    : () . __pr_1__                                                 ===>   ()',

			 '012EMPTYO    : ( () ++ __pr_1__ )                                           ===> ( __pr_1__ )',
			 '013EMPTYO    : ( () ++ __prs_1__ ++ () )                                    ===> ( __prs_1__ )',
			 '014EMPTYO    : ( __pr_1__ ++ () )                                           ===> ( __pr_1__ )',

                         '0015EQ0      : ( __pe_1__ == __pe_1__ )                                     ===> ( __pe_1__ )',


			 '020IFvoi    : if () then __prs_1__ else __prs_2__ fi                        ===>   __prs_2__',

                         '030IFuni    : if __pr_1__ . (%_) then __prs_1__ else __prs_2__ fi           ===>   if __pr_1__  then __prs_1__ else __prs_2__ fi',
                         '030IFunix   : if __pr_1__ . __pr_2__ . (%_)
                                           then __prs_1__ else __prs_2__ fi                           ===>   if __pr_1__  . __pr_2__ then __prs_1__ else __prs_2__ fi',

 			 '031IFuni    : if (%_) then __prs_1__ else __prs_2__ fi                      ===>   __prs_1__ ',
 			 '033IFuni    : if (@_) then __prs_1__ else __prs_2__ fi                      ===>   __prs_1__ ',
 			 '032IFuni    : if (__pr1s_1__, %_, __pr1s_2__)
                                           then __prs_1__ else __prs_2__ fi                           ===>   if (__pr1s_1__, __pr1s_2__) 
                                                                                                                then __prs_1__ else __prs_2__ fi',
 			 '033IFlit    : if (__lit_1__)
                                           then __prs_1__ else __prs_2__ fi                           ===>   __prs_1__ ',


			 '034IFunior : if (%_ ++ __pr0s_1__) then __prs_1__ else __prs_2__ fi         ===>   if (%_) then __prs_1__ else __prs_2__ fi',

			 '035IFXX     : if __prs_1__  then  ()  else  ()  fi                          ===>   ()',



			 '040IFAR1    : ( __pe_1__) . if __prs_1__ then (@_) else __prs_3__ fi        ===>   ( __pe_1__) . 
                                                                                                             if __prs_1__ then ($0) else __prs_3__ fi',
#			 '041IFAR2    : ( __pe_1__, __pe_2__) .
#                                        if __prs_1__ then ($0...) else __prs_3__ fi                   ===>   ( __pe_1__, __pe_2__) .
#                                                                                                             if __prs_1__ then ($0, $1) else __prs_3__ fi',
#			 '042IFAR3    : ( __pe_1__, __pe_2__, __pe_3__) .
#                                        if __prs_1__ then ($0...) else __prs_3__ fi                   ===>   ( __pe_1__, __pe_2__, __pe_3__) .
#                                                                                                             if __prs_1__ then ($0, $1, $2) else __prs_3__ fi',

                         '045IFD0     : (__pe_1__, __pr1s_1__) .
                                        if ($0)    then __pr0s_1__ else __prs_2__ fi                  ===>   (__pe_1__, __pr1s_1__) . __pr0s_1__',
                         '046IFD0     : (__pe_1__, __pr1s_1__) .
                                        if ($0, __pr1s_2__) 
                                           then __prs_1__ else __prs_2__ fi                           ===>   (__pe_1__, __pr1s_1__) .
                                                                                                             if (__pr1s_2__)
                                                                                                                then __prs_1__ else __prs_2__ fi',
                         '047IFD0     : (__pe_1__ == __pe_2__, __pr1s_1__) .
                                        if ($0)    then __pr0s_1__ else __prs_2__ fi                  ===>   (__pe_1__ == __pe_2__, __pr1s_1__) . __pr0s_1__',

                         '048IFD0     : (__pe_1__ == __pe_2__, __pr1s_1__) .
                                        if ($0, __pr1s_2__) 
                                           then __prs_1__ else __prs_2__ fi                           ===>   (__pe_1__ == __pe_2__, __pr1s_1__) .
                                                                                                             if (__pr1s_2__)
                                                                                                                then __prs_1__ else __prs_2__ fi',
                         '047IFD1     : (__pe_1__, __pe_2__, __pr1s_1__) .
                                        if ($1)
                                           then __pr0s_1__ else __prs_2__ fi                          ===>   ( __pe_1__, __pe_2__, __pr1s_1__) . __pr0s_1__',
                         '048IFD2     : (__pe_1__, __pe_2__, __pr1s_1__) .
                                        if ($1, __pr1s_2__)
                                           then __prs_1__ else __prs_2__ fi                            ===>   ( __pe_1__, __pe_2__, __pr1s_1__) .
                                                                                                              if (__pr1s_2__)
                                                                                                                 then __prs_1__ else __prs_2__ fi',

			 '049IDD0     : if ( __pr_1__ . ($0)) then __prs_1__ else __prs_2__ fi        ===>    if ( __pr_1__ ) then __prs_1__ else __prs_2__ fi',



                         '050PRD00    : __pr_1__ . (@_)                                               ===>   __pr_1__',
                         '050PRD01    : (@_) . __pr_1__                                               ===>   __pr_1__',
			 '050PRD02    : ( __pe_1__, @_ )  .  ($0)                                     ===>   ( __pe_1__ )',

##                         '052PRD01    : (@_) . ($0)                                                   ===>   ($0) ',
## not true              '050PRD02    : (@_) . ($1)                                                   ===>   ($1) ',

                         '051PRD0     : ( __pr1_1__ ) . ($0)                                          ===>   ( __pr1_1__ )',
                         '052PRD1     : ( __pr1_1__, __pr1_2__ ) . ($0, $1)                           ===>   ( __pr1_1__, __pr1_2__ )',
                         '053PRD1     : ( __pr1_1__, __pr1_2__ ) . ($0, $1, __pr_3__)                 ===>   ( __pr1_1__, __pr1_2__, __pr_3__ )',
                         '054PRD1     : ( __pr1_1__, __pr1_2__ ) . ($1, $0)                           ===>   ( __pr1_2__, __pr1_1__ )',
                         '055PRD1     : ( __lit_1__, __pr1_2__ ) . ($1)                               ===>   ( __pr1_2__ )',

                         '060PRvoi    : (void) . ( __lit_1__ )                                        ===>   ( __lit_1__ )', # TODO: could be more general!!!

# these only would work if tail does not depend on them!
#			 '060VOIDPR   : (void)      . __prsc_1__                                      ===>   __prsc_1__',
#			 '061LITEPR   : (__lit_1__) . __prsc_1__                                      ===>   __prsc_1__',

#                         '053PRD01    : (__pe_1__, __pe_2__, __pr1s_1__) . ($1...)                    ===>   (__pe_2__, __pr1s_1__)',


			 '070CLASS    : if (__pr1s_1__, $n classes :>: thing == __tid_1__, __pr1s_2__) 
                                           then __prs_1__ else __prs_2__ fi                           ===>   if ($n == __tid_1__ classes :<: thing,
                                                                                                                 __pr1s_1__, 
                                                                                                                 __pr1s_2__)
                                                                                                                then __prs_1__ else __prs_2__ fi',

			 '070CLASS1   : (%_)  .  ($0 == __pe_1__)                                     ===>   (%_ == __pe_1__ )',
			 '070CLASS2   : (%_ == thing classes :<: thing)                               ===>   (%_)',


                         '071IFIF     : if ( if __prs_1__ then (@_) else () fi ) 
                                           then __prs_2__
                                           else __prs_3__ fi                                          ===>   if __prs_1__ then __prs_2__ else __prs_3__ fi',

 			 '080IFSIn    : if ($n == __pe_1__, __pr1s_1__)
                                           then (__pr1s_2__, $n, __pr1s_3__) else ()     fi           ===>   if ($n, __pr1s_1__)
                                                                                                                then (__pr1s_2__, $n == __pe_1__, __pr1s_3__) 
                                                                                                                else () fi',
 			 '081IFSIn    : ( __pr1_1__, __pr1_2__ ) . 
                                        if ($1 == __pe_1__, __pr1s_1__)
                                           then __pr_1__ else ()     fi                               ===>   ( __pr1_1__, __pr1_2__ ) .
                                                                                                             if ($1, __pr1s_1__)
                                                                                                                then ($0, $1 == __pe_1__) . __pr_1__
                                                                                                                else () fi',

#			 '090IFPRfol : if __prs_1__ then __pr_2__ else __pr_3__ fi . ($0)              ===>   if __prs_1__ 
#                                                                                                                then __pr_2__ . ($0) 
#                                                                                                                else __pr_3__ . ($0) fi ', ## should be __prs_x__


			 '091PRIFfol : __pr_1__ . if __prsc_1__ then __pr0s_1__ else __pr0s_2__ fi     ===>   if __prsc_1__ 
                                                                                                                 then __pr_1__ . __pr0s_1__ 
                                                                                                                 else __pr_1__ . __pr0s_2__ fi',


			 '10PREQ0     : ( __pe_1__, __pr1s_1__) . 
                                        (__pr1s_2__, $0 == __lit_1__, __pr1s_3__)                     ===>   ( __pe_1__ == __lit_1__, __pr1s_1__) .
                                                                                                             (__pr1s_2__, $0, __pr1s_3__)',

			 '10PREQ1     : ( __pe_1__, __pe_2__, __pr1s_1__) . 
                                        (__pr1s_2__, $1 == __lit_1__, __pr1s_3__)                    ===>   ( __pe_1__, __pe_2__ == __lit_1__, __pr1s_1__) .
                                                                                                             (__pr1s_2__, $1, __pr1s_3__)',

			 '100PRREP0    : ( __pe_1__ ) . (__pr1sc_1__, $0, __pr1sc_2__ )                ===>  (__pr1sc_1__, __pe_1__,  __pr1sc_2__ )',


			 '120PRINT     : (__pe_1__, __pe_2__)  .  ($0 == $1)                          ===> ( __pe_1__ == __pe_2__ )'


		       ) {
	my ($id,   $rule)  = $r =~ /(\w+)\s*:(.+)/s;
	my ($left, $right) = split /===>/s, $rule;
	$rules{$id} = {s     => $r,
		       left  => new ('PE', $left),
		       right => new ('PE', $right) };
    }
#warn "-----------------Rules ".Dumper \%rules; exit;
}

##my $opt_deb = 0;


sub optimize {
    my $p = shift;
    %rules or _init_rules;
    my $rs = shift;    # optionally a rule list
    return _match_n_replace ([ map { $rules{$_} } ( $rs ? @$rs : sort keys %rules) ],     $p);
}

sub _match_n_replace {                                                # recursive descent of the expression tree, on return trying to apply rules to PEprs nodes
    my $rs = shift;                                                   # optional rules list
    my $p  = shift;

#warn "matchn repl $p";
    return $p if $p =~ /^~~~.*~~~$/;                                  # we cannot do unification yet, only matching, anything with ~~~ is a meta_production
    my $pref = ref ($p);                                              # shorthand
#warn "matchn replace $pref";
    if (grep ($pref eq $_, qw(PEall PEvoid PEpi PEti PEpe PExml))) {  # nothing important to be done, TODO: PExml!!
	return $p;                                                    # climb up back again

    } elsif ($pref eq 'PEif') {                                       # climb down
	$p->con  ( _match_n_replace ($rs, $p->con) );                 # first condition
	$p->then ( _match_n_replace ($rs, $p->then) );                # then the then
	$p->else ( _match_n_replace ($rs, $p->else) );                # then the else
	return $p;

    } elsif ($pref eq 'ARRAY') {
	return [ map { _match_n_replace ($rs, $_) } @$p ];            # follow lists down

    } elsif (grep ($p eq $_, ('==', '++', '--'))) {                   # any operators ? climb up
	return $p;

    } elsif ($pref eq 'PEpr') {                                       # a projection
	$p->arr (_match_n_replace ( $rs, $p->arr ));                  # climb down each
	return $p;

    } elsif ($pref eq 'PEprid') {
	return $p;

    } elsif ($pref eq 'PEprs') {
	my $potential_for_optimization = 1;                           # we assume one; it's like with Bond University: at the beginning we were optimistic
	while ($potential_for_optimization) {
	    $p->arr (_match_n_replace ( $rs, $p->arr ));              # hoping now that all sub expressions (on the PEprs level) have been optimized
#warn "-- backing up ----------------------------------------------------------------------";
	    $potential_for_optimization = 0;                          # hypothesis before loop, maybe one rule is applied, then we reconsider
	    foreach my $r (@$rs) {                                    # walk through all rules
#warn "before optimizing   ".prs2str($p);
#warn "                                                              testing:". prs2str ($r->{left}) . " ===> ".prs2str ($r->{right});
##$opt_deb = 1 if prs2str ($r->{left}) =~ /pr0s/;
		if (_match_prs ($p, $r->{left}, $r->{right})) {
		    $potential_for_optimization = 1;                  # resurrect hopes
#warn "                                                              SUCCESS:". prs2str ($r->{left}) . " ===> ".prs2str ($r->{right});
#warn "after optimizing    ".prs2str($p);
#warn "after optimizing    ".Dumper $p;
		}
##$opt_deb = 0;
	    }
	}
	return $p;                                                    # this is as good as it gets

#    } elsif ($pref eq 'PEfun') {  ## TODO
#	$p->a (_match_n_replace ( $rs, $p->args ));
#	return $p;
#    } elsif ($pref eq '' && $p =~ /^~~~.*~~~$/) {
#	# this is a meta variable
    } else {
	die "unhandled '$pref' ($p)";
    }
}

sub _match_prs {
    my $f = shift;    # factual expression, a PEprs node
    my $p = shift;    # pattern, another PEPrs node
    my $q = shift;    # pattern which is the replacing one, in case p matches

#warn "in starting match_prs ".prs2str ($f);
#warn "match prs ".Dumper ($f, $p);

###    return undef if $f->neg; # @@@@ for the time being: if neg => no opt

    foreach my $i (0..  $#{$f->arr}) {                                     # TODO: - length would not hurt
#warn "using $i in f ".Dumper $f->arr;
#warn "p length is ".scalar @{$p->arr};
	my @fslice = @{$f->arr}[$i .. $i + scalar @{$p->arr} - 1];         # cut out a slice from the factual expression, with the length of the pattern
#warn "using $i, sliced out ".Dumper \@fslice;
	last unless scalar @fslice == scalar @{$p->arr};                   # if they do not have the same length, no way they match, and we stop the game

#warn "seeing whether the slice matches....";
	if (my $b = _match (\@fslice, $p->arr, {})) {
#warn "FOUND MATCH ".Dumper $b;
	    my $qclone = clone ($q);
	    my $qexp   = _interpolate ($qclone, $b);                       # expand b into clone
##warn "expanded version is ".Dumper $qexp;
	    splice (@{$f->arr}, $i, scalar @{$p->arr}, @{$qexp->arr});     # remove from i what we had matched, and inject the expanded list
#warn "final version is      ".prs2str($f);
	    return $b;                                                     # signal up that we have achieved something
	}
    }
    return undef;                                                          # not a single match, we are exhausted
}

sub _associate {                                                           # associate a variable to a value (unless it is not yet bound to something else
    my $b   = shift;
    my $var = shift;
    my $val = shift;

    if ($b->{$var}) {                                                      # the placeholder has already been bound to a value
	use Test::More;
	return eq_array ($b->{$var}, $val) ? $b : undef;                            # and it fits => good,  or, already bound and not the same => no match
    } else {                                                               # placeholder not bound yet
	$b->{$var} = $val;                                                 # bind it
#warn "new association ".Dumper $b;
	return $b;                                                         # sell this as match
    }
}

sub _match {
    my $f = shift;
    my $p = shift;
    my $b = shift;

#warn "---- fact -- ".Dumper $f;# if $opt_deb;
#warn "---- fiction -- ".Dumper $p;# if $opt_deb;
#warn "--- with b ".Dumper $b;

    return $b    if !defined $f and !defined $p;                           # if both are undefined return the binding as-is
#warn "still there";
    return undef if  defined $f xor  defined $p;                           # if exactly one is undefined no match
#warn "still there";
    return undef if $f =~ /^~~~.*~~~$/;                                    # we cannot do unification yet, only matching, anything with ~~~ is a meta_production
#warn "still there";

    my $fref = ref ($f);                                                   # shorthand
    if ($fref eq 'PEprs') {
	if ($p =~ /__prs_\d+__/) {                                         # this this was a placeholder for the whole projection list
	    return _associate ($b, $p, $f);
	} elsif ($p =~ /__prsc_\d+__/) {                                   # this this was a placeholder for the whole projection list, but with NO variables
	    return (scalar find_pis ($f)) ? undef : _associate ($b, $p, $f);
	} elsif (ref ($p) eq 'PEprs') {
	    if ($p->arr->[0] =~ /__pr0s_\d+__/) {                          # placeholder for a list of pr0's
		return _associate ($b, $p->arr->[0], $f->arr);             # take the whole list and bind it
	    } else {
		return _match ($f->arr, $p->arr, $b);                      # do it the hard way
	    }
	} else {
	    return undef;
	}

    } elsif ($fref eq 'PEprid') {
	return _associate ($b, $p, $f) if     $p =~ /__pr_\d+__/;          # identity matches any projection meta-var
	return ref ($p) eq 'PEprid' ? $b : undef;                          # or perfect match

    } elsif ($fref eq 'PEpr') {
	return _associate ($b, $p, $f) if     $p =~ /__pr_\d+__/;
	return undef                   unless ref ($p) eq 'PEpr';
	my $b2 = { %$b };                                                  # clone existing binding
	my ($farr, $parr) = ($f->arr, $p->arr);                            # shorthands
	my $i = 0;                                                         # i points to facts, j to fiction
	for (my $j = 0; $parr->[$j]; $j++) {
#warn "looking at $i $j";
	    if ($parr->[$j] =~ /__pr1_(\d+)__/ && $farr->[$i]) {           # we found a variable, so we match EXACTLY one (if there is something to bind, that is)
#warn "looking at $parr->[$j] for ";
		$b2 = _associate ($b2, $parr->[$j], $farr->[$i++]);        # bind it with the pattern, accumulate the bindings

#warn "assoced $parr->[$j] with $farr->[$i++] ? ".Dumper $b2;
	    } elsif ($parr->[$j] =~ /__pr1sc?_(\d+)__/) {                  # we found a variable, so we greedily match (variable and constant version)
#warn "in $parr->[$j] matching";
		my @m;                                                     # will contain the matched values for this variable
		if (! defined $parr->[$j+1]) {                             # we are at the last pattern => take the whole fact list
		    while ($farr->[$i]) {
			last if $parr->[$j] =~ /__pr1sc_(\d+)__/ &&        # and this thing is not dependent on any $n
			    scalar find_pis ($farr->[$i]) > 0;
			push @m, $farr->[$i++]; 
		    }
		} else {                                                   # collect all list members until we found one which matches
		    while ($farr->[$i]) {                                  # as long as there is still something in the fact list
#warn "collecting for $parr->[$j] : ". "nr of pis in ".Dumper ($farr->[$i])." is ".scalar find_pis ($farr->[$i]);
			last if $parr->[$j] =~ /__pr1sc_(\d+)__/ &&        # and this thing is not dependent on any $n
			    scalar find_pis ($farr->[$i]) > 0;
			last if _match ($farr->[$i], $parr->[$j+1], $b2);  # if that matches the next pattern we now we have to stop looking ahead
			push @m, $farr->[$i++];                            # otherwise put it into our match list
		    }
		}
		$b2 = _associate ($b2, $parr->[$j], \@m);

	    } else {
		$b2 = _match ($farr->[$i++], $parr->[$j], $b2);            # try to match it with the pattern, accumulate the bindings
	    }
	    return undef unless $b2;                                       # no need to continue if all chances are gone
	}
#warn "before undef i: $i, farr[i] is".Dumper $farr->[$i];
	return undef if $farr->[$i];                                       # facts must be exhausted as well, otherwise the match is not complete
#warn "finall binding is now ".Dumper $b2;
	return $b2;

    } elsif ($fref eq 'ARRAY') {                                           # operator (++, -, ., ===) list

	if (@$f != @$p) {                                                  # if they have not the same length, give up
	    return undef;
	} else {
	    my $b2 = { %$b };                                              # clone existing binding
	    for (my $i = 0; $i < @$f; $i++) {
		$b2 = _match ($f->[$i], $p->[$i], $b2)                     # try to match it with the pattern, accumulate the bindings
		    or return undef;                                       # or simply give up
	    }
#warn "FULL LIST MATCH";
	    return $b2;                                                    # we survived up to here: return what we found
	}

    } elsif (grep ($f eq $_, ('==', '++', '--'))) {
#warn "match ===.. $f, $p";
	return $f eq $p ? $b : undef;

    } elsif ($fref eq 'PEif') {
#warn "PEif match";
	if (ref ($p) eq 'PEif') {
	    $b = _match ($f->con, $p->con, $b)
		or return undef;
	    $b = _match ($f->then, $p->then, $b)
		or return undef;
	    $b = _match ($f->else, $p->else, $b)
		or return undef;
	    return $b;
	} else {
	    return undef;
	}

    } elsif ($fref eq 'PEpe') {
#warn "PEpe match?";
	if ($p =~ /__pe_\d+__/) {                                 # actually variable placeholder for a pe
	    return _associate ($b, $p, $f);
	} elsif (ref ($p) eq 'PEpe') {
	    $b = _match ($f->val, $p->val, $b);

	    my ($fmos, $pmos) = ($f->mos, $p->mos);
#warn "fmos, pmos ".Dumper ($fmos, $pmos);
	    return undef unless scalar @$fmos == scalar @$pmos;
	    for (my $i = 0; $i <= $#$fmos; $i++) {
#warn "testing axis $i";
		return undef if $fmos->[$i]->axi ne $pmos->[$i]->axi;
		return undef if $fmos->[$i]->dir ne $pmos->[$i]->dir;
#		return undef if $fmos->[$i]->tid ne $pmos->[$i]->tid;
	    }
#warn "PEpe match!";
	    return $b;

	} else {
	    return undef;
	}

    } elsif ($fref eq 'PEpi') {
#warn "PEpi match with $p";
	if ($p =~ /__\$\w+__/) {                                  # actually a placeholder for pi projections
#warn "p is detected as variable $p, associated with $f";
	    return _associate ($b, $p, $f);
	} elsif (ref ($p) eq 'PEpi') {
#	    return undef unless $f->low == $p->low;
#	    return $b    if     $f->hig eq $p->hig;
#	    return undef;
	    return $f->idx == $p->idx ? $b : undef;

	} else {
	    return undef;
	}

    } elsif ($fref eq 'PEvar') {                                              # not overly useful
	return _associate ($b, $p, $f) if ref ($p) eq 'PEvar' && $f->nam eq $p->nam;
	return undef;

    } elsif ($fref eq 'PExml') {                                   # no idea whether this will be ever used
	return undef;

    } elsif ($fref eq 'TM::Literal') {
#warn "literal matching $p";
	if ($p =~ /__lit_\d+__/) {
	    return _associate ($b, $p, $f);
	} elsif (ref ($p) eq 'TM::Literal') {
	    use Test::More;
	    return eq_array ($f, $p) ? $b : undef;
	} else {
	    return undef;
	}

    } elsif ($fref eq 'PEti') {
#warn "identifier matching $p";
	if ($p =~ /__tid_\d+__/) {
	    return _associate ($b, $p, $f);
	} elsif (ref ($p) eq 'PEti') {
	    return $f->tid eq $p->tid ? $b : undef;
	} else {
	    return undef;
	}

    } elsif ($fref eq 'PEvoid') {
#warn "literal matching $p";
        if ($p =~ /__lit_\d+__/) {
            return _associate ($b, $p, $f);
	} elsif (ref ($p) eq 'PEvoid') {
	    return $b;
	} else {
	    return undef;
	}

    } elsif (grep ($fref eq $_, qw(PEall)) ) {
#warn "testing basic thing";

	return undef unless $fref eq ref ($p);
	use Test::More;
	return eq_array ($f, $p) ? $b : undef;

    } else {
	die "unhandled node $fref for $f";
    }
}

sub _dissassociate { # I like confusing names, but this is really the inverse of associate
    my $b   = shift;
    my $var = shift;

    my $val = $b->{$var} or die "accessing an undefined meta variable '$var' in a binding should not happen";
    my $clone;
    eval Data::Dumper->Dump([$val], ['clone']);
    return $clone;
}

sub _interpolate {
    my $p = shift;
    my $b = shift;

    return undef unless $p;
#warn "expanding $p". Dumper $p;
    my $pref = ref ($p);

    if ($p =~ /__pr0s_\d+__/) {
	return @{ _dissassociate ($b, $p) };   # expand into list (from list ref)
    } elsif ($p =~ /__prs_\d+__/) {
	return _dissassociate ($b, $p);
    } elsif ($p =~ /__prsc_\d+__/) {
	return _dissassociate ($b, $p);
    } elsif ($p =~ /__pr1_\d+__/) {
	return _dissassociate ($b, $p);
    } elsif ($pref eq 'PEprs') {
	$p->arr (_interpolate ( $p->arr, $b ));
	return $p;

    } elsif ($p =~ /__pr_(\d+)__/) {
	return _dissassociate ($b, $p);
    } elsif ($pref eq 'PEprid') {
	return $p;
    } elsif ($pref eq 'PEpr') {
	my @l;                                                    # new, expanded list
	foreach my $e (@{$p->arr}) {
	    if ($e =~ /__pr1sc?_(\d+)__/) {
#warn "interpolating list";
		push @l, @{_dissassociate ($b, $e)};
	    } else {
		push @l, _interpolate ( $e, $b );
	    }
	}
	$p->arr (\@l);                                            # set it
#warn "expanded PEpr is now ".Dumper $p;
	return $p;

    } elsif ($pref eq 'PEif') {
	$p->con  (_interpolate ($p->con,  $b));
	$p->then (_interpolate ($p->then, $b));
	$p->else (_interpolate ($p->else, $b));
	return $p;

    } elsif ($p =~ /__pe_\d+__/) {
	return _dissassociate ($b, $p);
    } elsif ($pref eq 'PEpe') {
	$p->val ( _interpolate ($p->val, $b) );
	return $p;

    } elsif ($pref eq 'PEpi') {
	return $p;

    } elsif ($pref eq 'PExml') {
	return $p;

    } elsif ($pref eq 'ARRAY') {
	return [ map { _interpolate ($_, $b) } @$p ];

    } elsif (grep ($p eq $_, ('==', '++', '--'))) {
	return $p;

    } elsif ($p =~ /\$\w+/) {
	return _dissassociate ($b, $p);

    } elsif ($p =~ /__lit_\d+__/) {
	return _dissassociate ($b, $p);

    } elsif ($p =~ /__tid_\d+__/) {
	return _dissassociate ($b, $p);

    } elsif (grep ($pref eq $_, ('PEall', 'PEvoid', 'PEti'))) {
	return $p;

## TODO functions

    } else {
	die "unhandled $pref for $p";
    }
}

#-- Evaluation ------------------------------------------------------------------------------------------

sub eval {
#warn "eval ". Dumper (@_);  #    die;
    $_[0]->{'%__'} = {};                                                    # start with empty meta map
    return _eval_prs ([ $_[0] ], $_[1]);                                    # create [] around context and pass in the PEprs unchanged
}

#@@@@ TODO: align it with prs structure (last if amalgated continuations)


sub _eval_prs {
    my $cs  = shift;                                                        # a list ref of hash refs containing a stack variable bindings
    my $prs = shift;                                                        # the path expression (optimized or not)

#warn "eval cs".Dumper $cs;
#warn "eval_prs: ". Dumper $prs;

    my $cs2 = [ { '%__' => $prs->maps }, @$cs ];                            # clone context locally (not deep cloning)
##    $cs2->[0]->{'%__'}   = { %{ $cs2->[0]->{'%__'} }, %{ $prs->maps } };    # fetch local maps to enrich my meta-map

#warn "new context is".Dumper $cs2;

    my $tss = undef;                                                        # this will be the result, undef signals that we have not done anything
    foreach my $pr (@{$prs->arr}) {                                         # we work our way through the pr's or if's
	if (ref ($pr) eq 'PEpr') {
	    unless ($tss) {                                                 # kick off, in case this is the very first tuple (= base)
		$tss = _eval_pr ($cs2, $pr->arr);
	    } else {                                                        # we already have created a base
		my $tss2;
		my $ctr = 0;
		foreach my $t (@$tss) {                                     # for all tuples in the incoming TS
		    my $cs3 = [ { '@_' => $t,                               # we push the tuple into the context 
				  '$#' => new TM::Literal ($ctr++, 'xsd:integer') },
				@$cs2 ];                                    # and a counter as well
		    push @$tss2, @{_eval_pr ($cs3, $pr->arr)};              # and with that we eval the tuple
		}
		$tss = $tss2;
	    }
	} elsif (ref ($pr) eq 'PEif') {
	    unless ($tss) {                                                 # kick off, in case this is the very first tuple (= base)
		my $con = _eval_prs ($cs2, $pr->con);
		$tss = @$con                                                # if there is a single tuple in that
			? _eval_prs ($cs2, $pr->then)
			: _eval_prs ($cs2, $pr->else);

	    } else {
		my $tss2;
		my $ctr = 0;
		foreach my $t (@$tss) {                                     # for all tuples in the incoming TS
		    my $cs3 = [ { '@_' => $t,                               # we push the tuple into the context
				  '$#' => new TM::Literal ($ctr++, 'xsd:integer') },
				@$cs2 ];                                    # and a counter as well
		    my $con = _eval_prs ($cs3, $pr->con);
		    push @$tss2, @{                                         # get a list from the reference
                                   (@$con                                   # if there is a single tuple in that
			            ? _eval_prs ($cs3, $pr->then)
				    : _eval_prs ($cs3, $pr->else) )};
		}
		$tss = $tss2;
	    }

	} else {
	    die "this is all sooo wrong";
	}
    }
    $tss ||= [];                                                      # if we have still undef, then return the empty TS
#warn "======= TSS (before literal) =================".Dumper $tss;
   {                                                                  # should we convert whole TS to literal values (if possible)?
	my $tm  = _eval_var     ($cs, '%_');                          # get the context map
	TM::QL::TS::ts_atomify ($tss, $tm);                           # otherwise, we do in-replacement of $tss;
    }
#warn "======= done prs TSS (after literal) =================".Dumper $tss;
    return $tss;
}

sub _eval_pr {
    my $cs = shift;
    my $ss = shift;

    my $tss;                                                          # build a complete TS
    foreach my $c (@{$ss}) {                                          # we walk through the val component
	$tss = TM::QL::TS::ts_concat ($tss, _eval_arr ($cs, $c));
    }
    return $tss || [];                                                # flatten out this tuple of lists into a list ref of simple tuples
}

sub _eval_arr {
    my $cs = shift;
    my $va = shift;

#warn "eval_arr with $va";

    if (ref ($va) eq 'ARRAY') {
	my $tss = _eval_arr ($cs, $va->[0]);                               # compute first operand value -> TS
	my $i = 1;
	while (my $op = $va->[$i]) {                                       # collect the operator
	    my $tss2 = _eval_arr ($cs, $va->[$i+1]);                       # compute second operand -> TS
	    if ($op eq '==') {
		$tss = TM::QL::TS::ts_intersect  ($tss, $tss2);
	    } elsif ($op eq '++') {
		$tss = TM::QL::TS::ts_interleave ($tss, $tss2);
	    } elsif ($op eq '--') {
		$tss = TM::QL::TS::ts_subtract   ($tss, $tss2);
	    } else {
		die "unknown op $op";
	    }
	    $i += 2;
	}
	return $tss;

    } elsif (ref ($va) eq 'PEfun') {
	my $tss;                                                           # build a complete TS
	foreach my $vaa (@{$va->args}) {                                   # we walk through the args component
#warn "old tss is ".Dumper $tss;
	    $tss = TM::QL::TS::ts_concat ($tss, _eval_arr ($cs, $vaa));
#warn "built new tss is ".Dumper $tss;
	}
	{                                                                  # should we convert whole TS to literal values (if possible)?
	    my $tm  = _eval_var     ($cs, '%_');                           # get the context map
	    TM::QL::TS::ts_atomify ($tss, $tm);                            # otherwise, we do in-replacement of $tss;
	}
#warn "eval_arr: function args, whole TS is now ".Dumper $tss;
	return TM::QL::TS::ts_fun ($va->fun, $tss);

    } elsif (ref ($va) eq 'PEprs') {
	return _eval_prs ($cs, $va);

    } elsif (ref ($va) eq 'PEpe') {
	my $tss = _eval_val ($cs, $va->val);
	if (@{$va->mos}) {
	    my $tm  = _eval_var     ($cs, '%_');                     # get the context map
	    foreach my $mo (@{$va->mos}) {                           # for all prescribed movements
#warn "navigating ".Dumper $mo;
#warn "starting from ".Dumper $tss;
		$tss = TM::QL::TS::ts_navigation ($tm, $tss, $mo);   # make one step
#warn "got back ".Dumper $tss;
	    }
	}
	return $tss;

    } elsif (ref ($va) eq 'PExml') {
	return [ [ new TM::Literal (_eval_xml ($cs, $va), 'xsd:anyXML') ] ];

    } else {
	die "unrecognized node $va";
    }
}

sub _eval_xml {
    my $cs = shift;
    my $xn = shift;

    if (ref ($xn) eq 'PExml') {
	if ($xn->sta) {
	    my $elm = XML::LibXML::Element->new ( $xn->sta );
	    
	    foreach my $at (@{$xn->ats}) {
		my $frg = XML::LibXML::DocumentFragment->new;
		foreach my $fr (@{$at->[1]}) {                        # collect the textified fragments
		    $frg->appendChild (_eval_xml ($cs, $fr));
		}
		$elm->setAttribute( $at->[0], $frg->toString(0) );    # add attribute
	    }
	    foreach my $ch (@{$xn->con}) {                            # go through all fragments
		$elm->appendChild (_eval_xml ($cs, $ch));
	    }
	    return $elm;
	} else {
	    my $frg = XML::LibXML::DocumentFragment->new;
	    foreach my $fr (@{$xn->con}) {                            # go through all fragments
		$frg->appendChild (_eval_xml ($cs, $fr));
	    }
	    return $frg;
	}
    } elsif (ref ($xn) eq 'PEprs') { # embedded query
	my $frg = XML::LibXML::DocumentFragment->new;
	my $res = _eval_prs ($cs, $xn);

	map {
	    $frg->appendChild ($_)                               # append it to the XML fragment
	}
	map {
        # assertion: any ATOMIFICATION is already done, so no \ references there
            ref ($_) eq 'TM::Literal' 
                ? (                                              # a literal
                   ref ($_->[0]) =~ /XML::LibXML/                # check whether this is already XML
		        ? $_->[0]                                # as-is
		        : XML::LibXML::Text->new ( $_->[0] )     # convert the value into a XML text node
		   )
		: XML::LibXML::Text->new ( $_ )                  # an item identifier
	    } 
	map { 
	    @$_                                                  # look at one tuple
	    } @$res;                                             # take the tuple sequence

	$frg->appendChild ( XML::LibXML::Text->new ( '' ) )      # XML::Lib fragments cannot be empty? bug or feature?
	    unless @$res;
	return $frg;

    } elsif (ref ($xn) eq 'TM::Literal') { # string
	return XML::LibXML::Text->new ( $xn->[0] );

    } else {
	die "unknown XML node '".ref ($xn)."'";
    }
}


sub _eval_val {
    my $cs = shift;
    my $va = shift;

    if (ref ($va) eq 'PEpi') {
	my $v = _eval_var ($cs, '@_');                                   # try to find @_
	return [[ $v->[ $va->idx ] ]]; 
#	return [[ defined $va->hig 
#		  ? @{$v}[ $va->low .. $va->hig ]                        # return the ith value, do not care whether it is defined
#		  : @{$v}[ $va->low .. $#$v ]                            # return everything up to the end, starting with 'low'
#		  ]]; 

    } elsif (ref ($va) eq 'PEvoid') {
	return [ [ undef ] ];

    } elsif (ref ($va) eq 'TM::Literal') {
	return [ [ $va ] ];                                              # this is a value, so create a tuple sequence from that

    } elsif (ref ($va) eq 'PEall') {
	my $tm  = _eval_var     ($cs, '%_');                             # get the context map
	return [ map { [ $_->[TM->LID] ] } $tm->toplets ];               # we had a whole map as value, this equals the set of things in the map

    } elsif (ref ($va) eq 'PEti') {
#warn "eval ti".Dumper $va;
##@@@@@@@@
	my $tm  =  _eval_var ($cs, '%_');
	my $tid = $tm->tids ($va->tid) ;
#warn "tid after $tid";
	return $tid ? [ [ $tid ] ]                                       # by definition flattened
                    : [          ];                                      # very, very empty

    } elsif (ref ($va) eq 'PEvar') {
	my $name = $va->nam;
	my $v = _eval_var ($cs, $name);                                  # try to find value
        if ($name =~ /^%/) {                                             # if it is already a TS, take it
	    return $v;
	} elsif ($name =~ /^@/) {                                        # if it is a list, wrap it into a TS
	    return [ $v ];
	} else {
	    return [[ $v ]];                                             # if it is a single value, make a tiny TS
	}

    } else {
	die "unhandled node $va";
    }
}

sub _eval_var {
    my $cs  = shift;                                                  # list of contexts to search through
    my $var = shift;                                                  # the variable
#warn "bindings". Dumper $cs;
    foreach my $c (@$cs) {                                            # start from the most recent
	return $c->{$var} if defined ($c->{$var});
    }
    die "variable '$var' has no defined value";
}

1;

__END__



# sub arity {
#     my $p = shift;

#     if (ref ($p) eq 'PEif') {
# 	my $then = arity ($p->then);
# 	my $else = arity ($p->else);
# 	die "arities of then and else differ, this SHOULD NEVER happen" unless $then = $else;
# 	return $then;
#     } elsif ( ref ($p) eq 'PEpr') {
# 	my $ar = 0;
# 	foreach my $q (@{$p->arr}) {
# 	    $ar += arity ($q);
# 	}
# 	return $ar;
#     } elsif (ref($p) eq 'ARRAY') {
# 	return arity ($p->[0]); # there MUST be at least one thing here, and we assume the operands are of the same arity @@@@ TODO
#     } elsif (ref ($p) eq 'PEfun') {
# 	return $p->arity;  # here we will probably die without having this information
#     } elsif (ref ($p) eq 'PEpe') {
# 	return 1;
#     } else {
# 	die "unhandled node '$p'";
#     }
# }

# sub merge_into_if {                                                  # takes
#     my $if = shift;                                                  # PEif and
#     my @ps = @_;                                                     # one PEpr/PEif

# #warn "merging into if $if";

#     push @{$if->then->arr}, @ps;                                     # appends that to then branch
#     push @{$if->else->arr}, @{clone (\@ps)};                         # and a cloned version to the else branch
#     return $if;                                                      # and returns PEif again
# }

	} elsif ($rn eq 'PEprid') {                                   # identity projection
#	    bless $n, 'PEpr';                                         # reblessing is a blessing
#	    $n->arr ( [ [[[ mk_prs (PEpi->new (idx => 0)) ]]] ]);     # prepend $0 into the list


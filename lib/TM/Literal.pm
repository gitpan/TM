package TM::Literal;

use constant XSD => "http://www.w3.org/2001/XMLSchema#";



use constant {
    INTEGER => XSD.'integer',
    DECIMAL => XSD.'decimal',
    FLOAT   => XSD.'float',
    DOUBLE  => XSD.'double',
    STRING  => XSD.'string'
    };

our $grammar = q{

    literal                   : decimal
                              | integer
                              | luri
                              | string

    integer                   : /\d+/                                 { $return = new TM::Literal  ($item[1], 'xsd:integer'); }

    decimal                   : /\d+\.\d+/                            { $return = new TM::Literal  ($item[1], 'xsd:decimal'); }

    luri                      : uri                                   { $return = new TM::Literal  ($item[1], 'xsd:uri'); }

    string                    : /\"{3}(.*?)\"{3}|"([^\n]*?)"/s
                                ('^^' uri)(?)                         { $return = new TM::Literal  ($1,       $item[2]->[0] || 'xsd:string'); }

};

our $comparators = {
    ( INTEGER ) => sub { return $_[0] == $_[1]; },
    ( DECIMAL ) => sub { return $_[0] == $_[1]; },
    ( FLOAT   ) => sub { return $_[0] == $_[1]; },
    ( DOUBLE  ) => sub { return $_[0] == $_[1]; },
    ( STRING  ) => sub { return $_[0] eq $_[1]; },
};

our $operators = { 
    '+' => {
	(INTEGER) => {
	    (INTEGER) => \&op_numeric_add,
	},
	(DECIMAL) => {
	    (DECIMAL) => \&op_numeric_add,
	},
	(FLOAT) => {
	    (FLOAT) => \&op_numeric_add,
	},
	(DOUBLE) => {
	    (DOUBLE) => \&op_numeric_add,
	},
    },
    '-' => {
	(INTEGER) => {
	    (INTEGER) => \&op_numeric_subtract,
	},
	(DECIMAL) => {
	    (DECIMAL) => \&op_numeric_subtract,
	},
	(FLOAT) => {
	    (FLOAT) => \&op_numeric_subtract,
	},
	(DOUBLE) => {
	    (DOUBLE) => \&op_numeric_subtract,
	},
    },
    '*' => {
	(INTEGER) => {
	    (INTEGER) => \&op_numeric_multiply,
	},
	(DECIMAL) => {
	    (DECIMAL) => \&op_numeric_multiply,
	},
	(FLOAT) => {
	    (FLOAT) => \&op_numeric_multiply,
	},
	(DOUBLE) => {
	    (DOUBLE) => \&op_numeric_multiply,
	},
    },
    'div' => {
	(INTEGER) => {
	    (INTEGER) => \&op_numeric_divide,
	},
	(DECIMAL) => {
	    (DECIMAL) => \&op_numeric_divide,
	},
	(FLOAT) => {
	    (FLOAT) => \&op_numeric_divide,
	},
	(DOUBLE) => {
	    (DOUBLE) => \&op_numeric_divide,
	},
    },
    '==' => {
	(INTEGER) => {
	    (INTEGER) => \&cmp_numeric_eq,
	},
	(DECIMAL) => {
	    (DECIMAL) => \&cmp_numeric_eq,
	},
	(FLOAT) => {
	    (FLOAT) => \&cmp_numeric_eq,
	},
	(DOUBLE) => {
	    (DOUBLE) => \&cmp_numeric_eq,
	},
    },
};

our %OPS = (
	    'tmql:add_int_int' => \&TM::Literal::op_numeric_add
	    );

sub new {
    my ($class, $val, $type) = @_;

    $type ||= 'xsd:string';
    $type   =~ s/^xsd:/XSD/e;
    return bless [ $val, $type ],$class;
}

sub _lub {
    my $a = shift;
    my $b = shift;

    if (     $a eq DOUBLE  || $b eq DOUBLE) {
	return DOUBLE;
    } elsif ($a eq FLOAT   || $b eq FLOAT) {
	return FLOAT;
    } elsif ($a eq DECIMAL || $b eq DECIMAL) {
	return DECIMAL;
    } else {
	return INTEGER;
    }
}

sub op_numeric_add { # (A, B)
    return new TM::Literal ($_[0]->[0] + $_[1]->[0], _lub ($_[0]->[1], $_[1]->[1]));
}

sub op_numeric_subtract { # (A, B)
    return new TM::Literal ($_[0]->[0] - $_[1]->[0], _lub ($_[0]->[1], $_[1]->[1]));
}

sub op_numeric_multiply { # (A, B)
    return new TM::Literal ($_[0]->[0] * $_[1]->[0], _lub ($_[0]->[1], $_[1]->[1]));
}

sub op_numeric_divide { # (A, B)
    return new TM::Literal ($_[0]->[0] / $_[1]->[0], 
			    $_[0]->[1] eq INTEGER && $_[1]->[1] eq INTEGER ?
			            INTEGER :
			            DECIMAL);
## @@ needs to be fixed
}

sub op_numeric_integer_divide { # (A, B)
    return new TM::Literal (int ($_[0]->[0] / $_[1]->[0]), 'xsd:integer');
}

sub cmp_numeric_eq {
    return $_[0]->[0] == $_[1]->[0] && $_[0]->[1] eq $_[1]->[1];
}

1;

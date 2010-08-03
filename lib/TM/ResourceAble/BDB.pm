package TM::ResourceAble::BDB::main;

use Tie::Hash;
use base qw(Tie::StdHash);

sub FETCH {
    my ($self, $key) = @_;
#    warn "FETCH main  $key";
    if ($key eq 'assertions' || $key eq 'mid2iid') {
	return $self->{$key};   # where the BDB is tied to
    } else {
	return $self->{'__main'}->{$key}; # tuck it into the secret store
    }
}

sub STORE {
    my ($self, $key, $val) = @_;
#    warn "STORE main $key";
    if ($key eq 'assertions' || $key eq 'mid2iid') {
	return $self->{$key} = $val;   # those go directly into the hash
    } else {
	return $self->{'__main'}->{$key} = $val;  # those will be redirected into the tied store
    }
}

1;

package TM::ResourceAble::BDB;

use strict;
use warnings;

use Data::Dumper;

use TM;
use base qw(TM);

sub new {
    my $class = shift;
    my %options = @_;

    my $file = $options{file} or die "file?";

    my %self;
    tie %self, 'TM::ResourceAble::BDB::main', "$file.main";

    use BerkeleyDB;
    my %flags = (-Flags => DB_CREATE ) unless -e "$file.main" && -s "$file.main";

#    warn Dumper \%flags;

    my $dbm = tie %{ $self{'__main'} },   'BerkeleyDB::Hash',
                                          -Filename => "$file.main", %flags;

    my $dba = tie %{ $self{assertions} }, 'BerkeleyDB::Hash',
                                          -Filename => "$file.assertions", %flags;

    $dba->filter_store_value ( sub { 
	use Storable qw(freeze);
	$_ = freeze ($_);
			       } ) ;
    $dba->filter_fetch_value ( sub { 
	use Storable qw(thaw);
	$_ = thaw ($_);
			       } ) ;
    my $dbt = tie %{ $self{mid2iid} },    'BerkeleyDB::Hash',
                                          -Filename => "$file.toplets", %flags;

    $dbt->filter_store_value ( sub { 
	use Storable qw(freeze);
	$_ = freeze ($_);
			       } ) ;
    $dbt->filter_fetch_value ( sub { 
	use Storable qw(thaw);
	$_ = thaw ($_);
			       } ) ;

    { # careful cloning from prototypical TM
	my $tmp = bless $class->SUPER::new (%options), $class;
	foreach my $k (keys %$tmp) {
	    if ($k eq 'mid2iid') {
		my $mid2iid = $self{mid2iid}; # fetch once
		$mid2iid->{$_} = $tmp->{mid2iid}->{$_} for keys %{ $tmp->{mid2iid} };

	    } elsif ($k eq 'assertions') {
		my $asserts = $self{assertions};
		$asserts->{$_} = $tmp->{assertions}->{$_} for keys %{ $tmp->{assertions} };

	    } else {
		$self{$k} = $tmp->{$k};
	    }
	}
    }
    return bless \%self, $class;
}

1;

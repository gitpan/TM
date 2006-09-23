package TM::Utils;

our $STDIN; # here we store the STDIN content to be able to reuse it later

sub get_content {
    my $url = shift;

    $main::log->logdie (scalar __PACKAGE__ . ": url is empty") unless $url;

    if ($url =~ /^inline:(.*)/s) {
	return $1;
    } elsif ($url eq 'io:stdin') {
	unless ($STDIN) {
	    local $\ = undef;
	    $STDIN = scalar <STDIN>;
	}
	return $STDIN;
    } else {                                    # some kind of URL?
	use LWP::Simple;
	return get($url) || die "unable to load '$url'\n";
    }
}

sub put_content {
    my $url = shift;
    my $s   = shift;

#warn "put content '$s' to ".$url;
    $main::log->logdie (scalar __PACKAGE__ . ": url is empty") unless $url;

    if ($url eq 'io:stdin') {        # no, I will not do that
#    } elsif ($url eq 'null:') {      # we should not be there, but in case, nothing will be written
    } elsif ($url eq 'io:stdout') {
	print STDOUT $s;
    } elsif ($url =~ /^file:(.*)/) { # LWP does not support file: PUT?
	open (F, ">$1") or die "cannot open file '$1' for writing";
	print F $s;
	close F;
    } else {
	die "other URL schemes '$url' not yet implemented";
    }
}

sub last_mod {
    my $url     = shift;
    my $default = shift || time;

    if ($url =~ /^file:(.+)/) {
	use File::stat;
	my $stats = stat ($1);
	return $stats ? $stats->mtime : undef;
    } elsif ($url =~ /^inline:/) {
	return undef;                        # how can I know
    } elsif ($url eq 'io:stdin') {
	return time+1;
    } elsif ($url eq 'io:stdout') {
	return 0;
    } else {                                 # using LWP is a bit heavyweight, but anyways
	use LWP::UserAgent;
	$ua = LWP::UserAgent->new;
	$ua->agent("TimeTester 1.0");
	
	my $req = HTTP::Request->new(GET => $url);
	my $res = $ua->request($req);
	
	use HTTP::Date;
	return str2time($res->headers->{'last-modified'});
    }
}

sub xmlify_hash {
    my $hash = shift;

    use XML::LibXML::SAX::Builder;
    my $builder = new XML::LibXML::SAX::Builder;
    use TM::Utils::TreeWalker;
    my $walker  = new TM::Utils::TreeWalker (Handler => $builder);
    $walker->walk ($hash);

    return $builder->result()->toString;
}

sub is_xml {
    my $s = shift;
    use XML::LibXML;
    my $parser = XML::LibXML->new();

    eval {
	my $doc = $parser->parse_string ($s);
    }; $@ ? 0 : 1;
}

our $VERSION  = '1.03';
our $REVISION = '$Id: Utils.pm,v 1.4 2006/09/19 10:20:33 rho Exp $';


1;

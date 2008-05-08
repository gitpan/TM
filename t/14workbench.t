#-- test suite

use strict;
use warnings;

# change 'tests => 1' to 'tests => last_test_to_print';
use Test::More qw(no_plan);

use Data::Dumper;
$Data::Dumper::Indent = 1;

# create tmp files
my @tmp;
use IO::File;
use POSIX qw(tmpnam);
for (0..1) {
    do { $tmp[$_] = tmpnam().".atm" ;  } until IO::File->new ($tmp[$_], O_RDWR|O_CREAT|O_EXCL);
}
END { unlink (@tmp) || die "cannot unlink '@tmp' file(s), but I am finished anyway"; }

{
    use TM::Workbench::Plugin::Tau;
    my $p = new TM::Workbench::Plugin::Tau;

    ok ($p->matches ("file:$tmp[0] > file:$tmp[1]"), 'loud on match');

    my $fh = IO::File->new ("> $tmp[0]") || die "so what?";
    print $fh "
aaa (bbb)
bn: AAA

(ccc)
ddd: eee
fff: ggg
";
    $fh->close;

    ok (! $p->execute ("file:$tmp[0] > file:$tmp[1]"), 'silent on copy');

    $fh = IO::File->new ($tmp[1]) || die "cannot reopen what I just wrote";
    local $/ = undef;
    my $s = <$fh>;

    like ($s, qr/aaa/, 'found aaa');
    close $fh;

}




__END__

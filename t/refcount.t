use Test::More tests => 14;
use Devel::Peek;
BEGIN { use_ok( 'Data::Dump::Streamer', qw(refcount sv_refcount is_numeric looks_like_number)); }

my $sv="Foo";
my $rav=[];
my $rhv={};

is sv_refcount($sv),1;
is refcount(\$sv),2;
my $ref=\$sv;
is sv_refcount($sv),2;
is refcount(\$sv),3;
is refcount($rav),1;
is refcount($rhv),1;

{
    use strict;
my $sv="Foo";
my $iv=100;
my $nv=1.234;
my $dbl=1e40;

my %hash=(100=>1,1.234=>1,1e40=>1);

for my $t ([$sv,''],[$iv,1],[$nv,1],[$dbl,1],map {[$_,'']} keys %hash) {
    is is_numeric($t->[0]),$t->[1],"Test:".$t->[0];
}
}
__END__

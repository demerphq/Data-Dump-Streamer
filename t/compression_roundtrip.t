use 5.018;
use strict;
use warnings;
use Test::More;
use File::Temp           qw(tempfile);
use Data::Dump::Streamer qw(Dump);

plan skip_all => 'needs Compress::Zlib and MIME::Base64'
    unless eval { require Compress::Zlib; require MIME::Base64; 1 };

my $original= "compress me\0\xff" x 100;
my $text= Dump($original)->Compress(-1)->Terse(1)->Out;
my ($program, $filename)= tempfile(SUFFIX => '.pl', TMPDIR => 1, UNLINK => 1);
print {$program} "use strict;\nuse warnings;\nmy \$value = do {\n",
    $text, "\n};\nprint unpack('H*', \$value), qq(\\n);\n"
    or die "Cannot write $filename: $!";
close $program or die "Cannot close $filename: $!";

# Generation loads compression modules in this process. Reconstruction must
# load its own dependencies from the generated prelude and helper call.
my @includes= map { "-I$_" } grep { !ref } @INC;
open my $child, '-|', $^X, @includes, $filename
    or die "Cannot start reconstruction interpreter: $!";
my $output= do { local $/; <$child> };
my $success= close $child;
ok($success, 'compressed output reconstructs in a fresh interpreter')
    or diag "Reconstruction process status: $?";
is(
    $output,
    unpack('H*', $original) . "\n",
    'binary value survives decompression'
);

done_testing;

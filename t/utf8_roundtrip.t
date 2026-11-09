use 5.022;
use strict;
use warnings;
use Test::More;
use Data::Dump::Streamer qw(Dump :undump);

my @values = ('', '0', '123', '-1', '12.5', 'plain ASCII',
              q($name @{[1]} "quoted" \\), 'a' x 200,
              join('', map { chr($_) } 0 .. 255), "wide \x{100} \x{263a}");
push @values, map { chr($_) } 0 .. 255;

for my $original (@values) {
    for my $upgrade (0, 1) {
        my $source = $original;
        utf8::upgrade($source) if $upgrade;
        my $value;
        my $dump = Dump($source)->Names('value')->Out;
        my $copy = eval $dump . '$value';
        is($@, '', 'Unicode-aware scalar output evaluates') or diag $dump;
        is($copy, $source, 'character content survives');
        is(!!utf8::is_utf8($copy), !!utf8::is_utf8($source), 'UTF-8 flag survives');
    }
}

{
    my $unicode = 'same';
    utf8::upgrade($unicode);
    my $value;
    my $text = Dump([$unicode, 'same'])->Names('value')->Out;
    my $copy = eval $text . '$value';
    is($@, '', 'mixed byte/Unicode array evaluates');
    ok(utf8::is_utf8($copy->[0]), 'RLE preserves a Unicode element');
    ok(!utf8::is_utf8($copy->[1]), 'RLE keeps the byte element distinct');
}

{
    require Scalar::Util;
    my $string = 'Unicode text';
    utf8::upgrade($string);
    my $source = Scalar::Util::dualvar(7, $string);
    my $value;
    my $copy = eval Dump($source)->Names('value')->Out . '$value';
    is($@, '', 'Unicode dualvar output evaluates');
    is(0 + $copy, 7, 'Unicode dualvar numeric value survives');
    is("$copy", $string, 'Unicode dualvar text survives');
    ok(utf8::is_utf8($copy), 'Unicode dualvar UTF-8 flag survives');
}

{
    require Scalar::Util;
    for my $stem ('ascii', "caf\x{e9}", "wide\x{100}") {
        utf8::upgrade($stem);
        for my $sigil ('$', '@', '%', '&') {
            my $shared = [];
            my $input = $sigil eq '@' ? [$shared, $shared]
                      : $sigil eq '%' ? {a => $shared, b => $shared}
                      : $sigil eq '&' ? sub { 7 } : 7;
            my $name = $stem . ord($sigil);
            my $dump = Dump($input)->Names(($sigil eq '$' ? '' : '*') . $name)->Out;
            ok(utf8::is_utf8($dump), 'Unicode variable names keep the source UTF-8 flag');
            my $expression = ($sigil eq '$' ? '' : '\\') . $sigil . $name;
            my $copy = eval "# \x{100}\npackage DDSUnicodeNames; use utf8; no strict;\n"
                          . $dump . $expression;
            is($@, '', "Unicode $sigil variable output evaluates") or diag $dump;
            if ($sigil eq '@') {
                is_deeply($copy, [[], []], 'named Unicode array contents survive');
                is(Scalar::Util::refaddr($copy->[0]), Scalar::Util::refaddr($copy->[1]),
                   'Unicode array fixup paths retain sharing') if ref($copy) eq 'ARRAY';
            }
            elsif ($sigil eq '%') {
                is_deeply($copy, {a => [], b => []}, 'named Unicode hash contents survive');
                is(Scalar::Util::refaddr($copy->{a}), Scalar::Util::refaddr($copy->{b}),
                   'Unicode hash fixup paths retain sharing') if ref($copy) eq 'HASH';
            }
            elsif ($sigil eq '&') {
                is(ref($copy), 'CODE', 'named Unicode subroutine survives');
                is($copy->(), 7, 'Unicode subroutine evaluates correctly') if ref($copy) eq 'CODE';
            }
            else { is($copy, 7, 'named Unicode scalar survives') }
        }
    }
}

done_testing;

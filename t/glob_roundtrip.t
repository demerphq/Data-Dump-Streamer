use 5.022;
use strict;
use warnings;
use Test::More;
use Scalar::Util qw(refaddr);
use Symbol ();
use Data::Dump::Streamer qw(Dump :undump);

for my $anonymous (0, 1) {
    for my $nested (0, 1) {
        for my $kind (qw(ARRAY HASH)) {
            local *DDSGlobRoundtrip::source;
            my $glob = $anonymous ? Symbol::gensym() : \*DDSGlobRoundtrip::source;
            my $container = $kind eq 'ARRAY'
                ? [[9], undef, 'slot', undef]
                : {a => [9], b => undef, c => 'slot', d => undef};
            if ($kind eq 'ARRAY') {
                $container->[1] = $container->[0];
                alias_av(@$container, 3, $container->[2]);
            }
            else {
                $container->{b} = $container->{a};
                alias_hv(%$container, 'd', $container->{c});
            }
            *{$glob} = $container;
            my $input = $nested ? [$glob] : $glob;
            my $text = Dump($input)->Names('value')->Out;
            # Named globs are reconstructed in their original package.
            # Keep the old container separately to detect accidental reuse.
            *{$glob} = $kind eq 'ARRAY' ? [] : {};
            my $value;
            my $copy = eval $text . '$value';
            my $label = "$kind anonymous=$anonymous nested=$nested";
            is($@, '', "$label: output evaluates") or diag $text;
            SKIP: {
                skip 'output did not evaluate', 5 if $@;
                my $copy_glob = $nested ? $copy->[0] : $copy;
                my $rebuilt = *{$copy_glob}{$kind};
                isnt(refaddr($rebuilt), refaddr($container), "$label: slot is newly constructed");
                is_deeply($rebuilt, $container, "$label: slot values round trip");
                if ($kind eq 'ARRAY') {
                    is(refaddr($rebuilt->[0]), refaddr($rebuilt->[1]), "$label: referent sharing survives");
                    is(refaddr(\$rebuilt->[2]), refaddr(\$rebuilt->[3]), "$label: scalar aliases survive");
                    $rebuilt->[3] = 'changed';
                    is($rebuilt->[2], 'changed', "$label: assignment through alias changes its source");
                }
                else {
                    is(refaddr($rebuilt->{a}), refaddr($rebuilt->{b}), "$label: referent sharing survives");
                    is(refaddr(\$rebuilt->{c}), refaddr(\$rebuilt->{d}), "$label: scalar aliases survive");
                    $rebuilt->{d} = 'changed';
                    is($rebuilt->{c}, 'changed', "$label: assignment through alias changes its source");
                }
            }
        }
    }
}

done_testing;

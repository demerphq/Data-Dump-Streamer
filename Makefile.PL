use ExtUtils::MakeMaker;

my $xs_version;

open my $f,'./lib/Data/Dump/Streamer.pm' or die "Failed to open file\n";
while (<$f>) {
    if (/([\$*])(([\w\:\']*)\bXS_VERSION)\b.*\=/){
        $xs_version=eval "$_;$1$2;\n";
        !$@ and last
    }
}

WriteMakefile(
    'NAME'         => 'Data::Dump::Streamer',
    'VERSION_FROM' => 'lib/Data/Dump/Streamer.pm',    # finds $VERSION
    'PREREQ_PM'    => {
        'Text::Balanced'  => 1.95,
        'Algorithm::Diff' => 1.15,
        'Test::More'      => undef,
        'Data::Dumper'    => 2.121
    },                                         # e.g., Module::Name => 1.1
    (
        $] >= 5.005
        ?                                      ## Add these new keywords supported since 5.005
          (
            ABSTRACT_FROM => 'lib/Data/Dump/Streamer.pm',                 # retrieve abstract from module
            AUTHOR        => 'Yves Orton <demerphq@hotmail.com>'
          )
        : ()
    ),
    ($xs_version ? (XS_VERSION => $xs_version) : () ),
    dist => {COMPRESS => 'gzip -9f', SUFFIX => 'gz',},
);

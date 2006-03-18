use ExtUtils::MakeMaker;

sub DDS {
    (my $text=<<'EOF_DDS')=~s/^#//gm; return $text;
##This all has to be one line for MakeMaker version scanning.
#use Data::Dump::Streamer (); BEGIN{ *DDS:: = \%Data::Dump::Streamer:: } $VERSION=$DDS::VERSION;
#1;
#
#=head1 NAME
#
#DDS - Alias for Data::Dump::Streamer
#
#=head1 SYNOPSIS
#
#  perl -MDDS -e "Dump \%INC"
#
#=head1 DESCRIPTION
#
#See L<Data::Dump::Streamer>.
#
#=cut
#
EOF_DDS
}

print "Installing Data::Dump::Streamer\n";
my ($override,$value);
$override= $ARGV[0]=~/^(?:NO)?DDS$/i ? shift @ARGV : undef;
if ($override) {
    $value=$override=~/^no/i ? 'no' : 'yes';
} else {
    $default= eval("require DDS; 1") || -e("./lib/DDS.pm")
            ? 'yes' : 'no';
    print "\n";
    print "I can install a shortcut so you can use the package 'DDS'\n";
    print "as though it was 'Data::Dump::Streamer'. This is handy for oneliners.\n\n";
    $value = prompt("Would you like me to install the shortcut? (yes/no)",
                   $default);
}
if ( $value=~/yes/i )
{
    print "Ok, I will also install DDS as an alias.\n";
    open my $ofh,">","./lib/DDS.pm" or die "Failed to open ./lib/DDS.pm:$!";
    print $ofh DDS();
    close $ofh;
    @opts=(realclean => { FILES => './lib/DDS.pm' });
} else {
    unlink "./lib/DDS.pm";
}

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
        #'Algorithm::Diff' => undef,
        #'Data::Dumper'    => undef,
        'Text::Balanced'  => 0,
        'Test::More'      => 0,
        'Text::Abbrev'    => 0,
        'B::Deparse'      => 0,
        'B::Utils'        => 0,
        # recommends: 'PadWalker'       => 1,
    },
    (
        $] >= 5.005
        ?                                      ## Add these new keywords supported since 5.005
          (
            ABSTRACT_FROM => 'lib/Data/Dump/Streamer.pm',                 # retrieve abstract from module
            AUTHOR        => 'Yves Orton (demerphq)'
          )
        : (
            ABSTRACT => qq[Stream a highly accurate easy to read data dump in perl code form to a var or file. (Also known as 'DDS')]
        )
    ),
    ($xs_version ? (XS_VERSION => $xs_version) : () ),
    dist => {COMPRESS => 'gzip -9f', SUFFIX => 'gz',},
    @opts
);




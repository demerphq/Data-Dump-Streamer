use ExtUtils::MakeMaker;
#$Id: Makefile.PL 30 2006-04-16 15:33:25Z demerphq $#

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
#=head1 VERSION
#
# $Id: Makefile.PL 30 2006-04-16 15:33:25Z demerphq $
#
#=cut
#
EOF_DDS
}

{
    my $B_Utils_required = 0.05;
    eval {
        require B::Utils;
    };
    if ( $@ or B::Utils->VERSION < $B_Utils_required ) {

        # If I don't have B::Utils then I must have ExtUtils::Depends
        my $ExtUtils_Depends_required = 0.302; #minimum version that works on Win32+gcc
        eval {
            require ExtUtils::Depends;
        };
        if ( $@ or ExtUtils::Depends->VERSION < $ExtUtils_Depends_required ) {
            print "ExtUtils::Depends $ExtUtils_Depends_required is required to configure our B::Utils dependency, please install it manually or upgrade your CPAN/CPANPLUS shell\n";
            exit(0);
        }        
    };
}



print "Installing Data::Dump::Streamer\n";
my ($override,$value);
$override= $ARGV[0]=~/^(?:NO)?DDS$/i ? shift @ARGV : undef;
if ($override) {
    $value=$override=~/^no/i ? 'no' : 'yes';
} else {
    if (-e '.answer' and open my $fh,"<",'.answer') {
        print "I will install (or not) the DDS shortcut as you requested previously.\n";
        print "If you wish to override the previous answer then state so explicitly\n";
        print "by saying 'perl Makefile.PL [NO]DDS'\n";
        chomp($value=<$fh>);
        print "Previous answer was: $value\n";
    }
    if (!$value) {
        $default= eval("require DDS; 1") || -e("./lib/DDS.pm")
                ? 'yes' : 'no';
        print "\n";
        print "I can install a shortcut so you can use the package 'DDS'\n";
        print "as though it was 'Data::Dump::Streamer'. ";
        print "This is handy for oneliners.\n*Note* that if you ";
        print "select 'no' below and you already\nhave it installed ";
        print "then it will be removed.\n\n";
        $value = prompt("Would you like me to install the shortcut? (yes/no)",
                       $default);
        if (open my $fh,">",'.answer') {
            print $fh $value;
            close $fh;
        }
    }
}
if ( $value=~/yes/i )
{
    print "I will also install DDS as an alias.\n";
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


my $v=ExtUtils::MM_Unix->parse_version('lib/Data/Dump/Streamer.pm');
#if ($ENV{username} eq 'demerphq') {
#    system('SubWCRev . .build.tmpl .patch');
#}
#my $build;
#{ open my $fh,"<",".patch" or die "No '.patch' file!:$!";
#  chomp($b= <$fh>);
#  close $fh;
#};
#
#
#{package MY;
#sub distdir {
#    my $MM=shift;
#    my $orig= $MM->SUPER::distdir();
#    $orig=~s/^(\s+\$)/$1(PREOP)\n$1/m;
#    return $orig;
#}}


WriteMakefile(
    'NAME'         => 'Data::Dump::Streamer',
    'VERSION'      => "$v",    # finds $VERSION
    'PREREQ_PM'    => {
        #'Algorithm::Diff' => undef,
        #'Data::Dumper'    => undef,
        'Text::Balanced'  => 0,
        'Test::More'      => 0,
        'Text::Abbrev'    => 0,
        'B::Deparse'      => 0,
        'B::Utils'        => 0.05,
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
    dist => {
        COMPRESS => 'gzip -9f',
        SUFFIX => 'gz',
        #PREOP => 'SubWCRev . -n',

    },
    @opts
);




sub DDS {
    (my $text=<<'EOF_DDS')=~s/^#//gm; return $text;
#    package DDS;
#
#use Data::Dump::Streamer qw(:all);
#@ISA=qw(Data::Dump::Streamer);
#sub import {
#    my $r=shift;
#    unshift @_,'Data::Dump::Streamer';
#    goto &{Data::Dump::Streamer->can('import')};
#}
#1;
#
#=head1 NAME
#
#DDS - Alias for Data::Dump::Streamer
#
#=head1 SYNOPSIS
#
#  perl -MDDS -e "Dump [qw(foo bar)]"
#
#  use DDS;
#
#=head1 DESCRIPTION
#
#Alias for L<Data::Dump::Streamer|Data::Dump::Streamer>. Look there for details.
#
#
#=cut
EOF_DDS
}
BEGIN {
    print "******************************************************\n";
    print "*** This is  Data::Dump::Streamer. Enjoy!          ***\n";
    if ($ARGV[0] eq 'DDS') {
        print "***------------------------------------------------***\n";
        print "*** Will also install DDS as an alias.             ***\n";
        print "******************************************************\n";
        open my $ofh,">","./lib/DDS.pm" or die "Failed to open ./lib/DDS.pm:$!";
        print $ofh DDS();
        close $ofh;
        @opts=(clean => { FILES => './lib/DDS.pm' });
        shift;
    } else {
        print "***------------------------------------------------***\n";
        print "*** use 'perl Makefile.PL DDS [STANDARD OPTS]'     ***\n";
        print "*** to have a shortcut to Data::Dump::Streamer     ***\n";
        print "*** called 'DDS' installed as well. Currently it   ***\n";
        print "*** won't be installed.                            ***\n";
        print "******************************************************\n";
        unlink "./lib/DDS.pm";
    }
}

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
    @opts
);




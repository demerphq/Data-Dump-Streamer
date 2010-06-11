package My::Builder;

use strict;
use warnings;

use Module::Build;
our @ISA = 'Module::Build';

sub ACTION_config {
    my ( $self ) = @_;

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
                print "ExtUtils::Depends $ExtUtils_Depends_required is required to configure our B::Utils dependency, please install it manually or upgrade your CPAN/CPANPLUS\n";
                exit(0);
            }
        };
    }


    print "Installing Data::Dump::Streamer\n";
    my $override =
        $ARGV[0] =~ /^(?:NO)?DDS$/i
        ? shift( @ARGV )
        : undef;

    my $value;
    if ( $override ) {
        $value = $override =~ /^no/i ? 'no' : 'yes';
    }
    else {
        if ( -e '.answer' && open my $fh, "<", '.answer') {
            print "I will install (or not) the DDS shortcut as you requested previously.\n";
            print "If you wish to override the previous answer then state so explicitly\n";
            print "by saying 'perl Makefile.PL [NO]DDS'\n";
            $value = <$fh>;
            chomp $value;
            print "Previous answer was: $value\n";
        }

        if ( ! $value ) {
            my $default =
                ( eval("require DDS; 1")
                  || ( -e "./lib/DDS.pm") )
                ? 'yes'
                : 'no';
            print "\n";
            print "I can install a shortcut so you can use the package 'DDS'\n";
            print "as though it was 'Data::Dump::Streamer'. This is handy for oneliners.\n";
            print "*Note* that if you select 'no' below and you already\n";
            print "have it installed then it will be removed.\n";
            print "\n";
            $value = prompt("Would you like me to install the shortcut? (yes/no)",
                            $default);
            if (open my $fh, ">", '.answer') {
                print $fh $value;
                close $fh;
            }
        }
    }

    if ( $value=~/yes/i ) {
        print "I will also install DDS as an alias.\n";
        open my $ofh, ">", "./lib/DDS.pm"
            or die "Failed to open ./lib/DDS.pm: $!";
        print $ofh DDS();
        close $ofh;

        $self->add_to_cleanup( './lib/DDS.pm' );
    }
    else {
        unlink "./lib/DDS.pm";
    }

    return;
}

sub DDS {
    my $text = <<'EOF_DDS';
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
    $text =~ s/^#//gm;
    return $text;
}



1;

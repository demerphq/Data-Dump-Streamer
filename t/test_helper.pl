use strict;
use warnings;
require Test::More;

# my own version of Text::Diff. :-) This should be removed to depend on that module
sub string_diff {
    my ( $str1, $str2, $title1, $title2 ) = @_;
    $title1 ||= "Got";
    $title2 ||= "Expected";

    my $line = ( caller(2) )[2];

    require Algorithm::Diff;

    #print $str1,"\n---\n",$str2;
    my $seq1 = ( ref $str1 ) ? $str1 : [ split /\n/, $str1 ];
    my $seq2 = ( ref $str2 ) ? $str2 : [ split /\n/, $str2 ];

    # im sure theres a more elegant way to do all this as well
    my @array;
    my $are_diff;
    Algorithm::Diff::traverse_sequences(
        $seq1, $seq2,
        {
            MATCH => sub {
                my ( $t, $u ) = @_;
                push @array, [ '=', $seq1->[$t], $t, $u ];
            },
            DISCARD_A => sub {
                my ( $t, $u ) = @_;
                push @array, [ '-', $seq1->[$t], $t, $u ];
                $are_diff++;
            },
            DISCARD_B => sub {
                my ( $t, $u ) = @_;
                push @array, [ '+', $seq2->[$u], $t, $u ];
                $are_diff++;
            },
        }
    );
    return "" unless $are_diff;
    my $return = "-$title1\n+$title2\n";

    #especially this bit.
    my ( $last, $skipped ) = ( "=", 1 );
    foreach ( 0 .. $#array ) {
        my $elem = $array[$_];
        my ( $do, $str, $pos, $eq ) = @$elem;

        if (   $do eq $last
            && $do eq '='
            && ( $_ < $#array && $array[ $_ + 1 ][0] eq "=" || $_ == $#array ) )
        {
            $skipped = 1;
            next;
        }

        $str .= "\n" unless $str =~ /\n\z/;
        if ($skipped) {
            $return .= sprintf( "\@%d,%d (%d)\n", $eq + 1, $pos + 1, $line + $eq + 1 );
            $skipped = 0;
        }
        $last = $do;
        $return .= join ( "", $do, " ", $str );
    }
    return $return;
}

sub capture { \@_ }

sub _same {
    my ( $str1, $str2, $name, $obj ) = @_;

    s/\s+$//gm for $str1,                          $str2;
    s/\r\n/\n/g for $str1,                         $str2;
    s/\(0x[0-9a-xA-X]+\)/(0xdeadbeef)/g for $str1, $str2;
    my @vars = $str2 =~ m/^(?:my\s*)?(\$\w+)\s*=/gm;

    #warn "@vars";
    unless ( ok( "\n" . $str1 eq "\n" . $str2, $name ) ) {
        if ( $str2 =~ /\S/ ) {
            eval {
                print string_diff( "\n" . $str2, "\n" . $str1, "Expected", "Result" );
                print "Got:\n" . $str1 . "\n";
              }
              or do {
                print "Expected:\n$str2\nGot:\n$str1\n";
              }
        } else {
            print $str1, "\n";
        }
        $obj->diag;
    }
}
{
    my $version="";
    my %errors;
    my @errors=('');

sub _dumper {
    my ($todump)=@_;
    my ($dump,$error);
    foreach my $use_perl (1) {
        my $warned="";
        local $SIG{__WARN__}=sub { my $err=join ('',@_); $warned.=$err unless $err=~/^Subroutine|Encountered/};
        $dump=eval { scalar Data::Dumper->new( $todump )->Purity(1)->Sortkeys(1)->Quotekeys(1)->Useperl($use_perl)->Dump() };
        unless ($@) {
            return ($dump,$error.$warned);
        }else {
            unless ($version) {
                $version="\tSomething is wrong with Data::Dumper v" . Data::Dumper->VERSION . "\n";
                $error=$version;
            }
            my $msg=$@.$warned;
            unless ($errors{$msg}) {
                (my $err=$msg)=~s/^/\t/g;
                push @errors,$msg;
                $errors{$msg}=$#errors;
                $error.=sprintf "\tData::Dumper (Useperl==$use_perl) Error(%#d):\n\t%s",
                        $#errors,$err;
            } else {
                $error.=sprintf "\tData::Dumper (Useperl==$use_perl) Error %#d\n",$errors{$msg};
            }
            next
        }
    }
    #warn $error;
    return ($dump,$error);
}
}

sub same {
    goto &_same unless ref( $_[1] );
    my $name   = shift;
    my $obj    = shift;
    my $expect = shift;

    my $result = $obj->Data(@_)
                     #->diag()
                     ->Out();
    #$obj->diag;

    $result=~s/^\s*use.*\n//gm;
    my @declare=grep { /^[\$\@\%]/ } @{$obj->{declare}};
    my @dump   =map  { /^[\@\%\&]/ ? "\\$_" : $_  } @{$obj->{out_names}};


    my ($dumper,$error) = _dumper(\@_);
    if ($error) {
        diag( "$name\n$error" );
    }
    if ($dumper) {
        my $dumpvars=join ( ",", @dump );
        my $result2_eval = $result . "\n" . 'scalar( $obj->Data(' . $dumpvars . ")->Out())\n";
        my $dd_result_eval =
          $result . "\nscalar(Data::Dumper->new("
          . 'sub{\@_}->(' . $dumpvars . ")"
          . ")->Purity(1)->Sortkeys(1)->Quotekeys(1)->"
          . "Useperl(1)->Dump())\n";
        unless ( $obj->Declare ) {
            $dd_result_eval = "my(" . join ( ",", @declare ) . ");\n" . $dd_result_eval;
            $result2_eval   = "my(" . join ( ",", @declare ) . ");\n" . $result2_eval;
        }
        foreach my $test ( [ "Dumper", $dd_result_eval, $dumper ], [ "Precise::Dump", $result2_eval, $result ] ) {
            my ( $test_name, $eval, $orig ) = @$test;

            my ($warned,$res);
            {
                local $SIG{__WARN__}=sub { my $err=join ('',@_); $warned.=$err unless $err=~/^Subroutine|Encountered/};
                $res  = eval $eval;
                if ($warned) { print "Eval produced warnings:$warned\n$eval" };
            }
            $res=~s/^\s*use.*\n//gm;
            my $fail = 0;
            if ($@) {
                print join "\n", "Failed $test_name eval()", $eval, $@, "";
                $fail = 1;
            } elsif ( $res ne $orig ) {
                print "Failed $test_name second time\n";
                eval { print string_diff( $orig, $res, "Orig", "Result" ); };
                print "Orig:\n$orig\nResult:\n$res\nEval:\n$eval\n";
                $fail = 1;
            }
            $obj->diag if $fail;
            return fail($name) if $fail;
        }

        #print join "\n",$result,$result2,$dumper,$dd_result,"";
    }
    s/\s+$//gm for $result,                          $expect;
    s/\r\n/\n/g for $result,                         $expect;
    s/\(0x[0-9a-xA-X]+\)/(0xdeadbeef)/g for $result, $expect;

    #warn "@vars";
    unless ( ok( "\n" . $result eq "\n" . $expect, $name ) ) {
        if ( $expect =~ /\S/ ) {
            eval {
                print string_diff( "\n" . $expect, "\n" . $result, "Expected", "Result" );
                print "$name Got:\n" . $result . "\nEXPECT\n";
              }
              or do {
                print "$name Expected:\n$expect\nGot:\n$result\n";
              }
        } else {
            print $result, "\n";
        }
        $obj->diag;
    }
}

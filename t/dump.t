use Test::More tests => 28;
use lib './lib';
BEGIN { use_ok( 'Data::Dump::Streamer', qw(:undump Dump) ); }
use strict;
use warnings;
use Data::Dumper;

# imports same()
(my $helper=$0)=~s/\w+\.\w+$/test_helper.pl/;
require $helper;
# use this one for simple, non evalable tests. (GLOB)
#   same ( $got,$expected,$name,$obj )
#
# use this one for eval checks and dumper checks but NOT for GLOB's
# same ( $name,$obj,$expected,@args )

my $dump;
my $o = Data::Dump::Streamer->new();

isa_ok( $o, 'Data::Dump::Streamer' );
{
    our ($foo,@foo,%foo,$bar);
    local $foo='yada';
    local @foo=((1)x10,(2) x 10);
    no warnings;
    local %foo=(2,*bar,3,sub{ print ('this is a test'),'foo'; print qq(\"bar\"\n); });
    use warnings;
    local $bar='BAR';
    my $x=*foo;
    same( do {$dump = $o->Data( $x )->Out; $dump=~s/^\s*(?:use|no).*\n//mg; $dump},
    <<'EXPECT', "DumpGlob, Rle, Deparse", $o );
$VAR1 = *::foo;
*::foo = \do { my $v = 'yada' };
*::foo = {
           2 => *::bar,
           3 => sub {
                  print('this is a test'), 'Useless const omitted';
                  print qq["bar"\n];
                }
         };
*::foo = [
           ( 1 ) x 10,
           ( 2 ) x 10
         ];
*::bar = \do { my $v = 'BAR' };
EXPECT
}
{
    same(   "Bart's Refs", $o,<<'EXPECT', ( \{},\[],\do{my $x="foo"},\('bar') ) );
$REF1 = \{};
$REF2 = \[];
$SCALAR1 = \do { my $v = 'foo' };
$SCALAR2 = \'bar';
EXPECT
    # originally the $o was an accident that exposed a bug
    # it was supposed to be $t all along, but they tickle different things.
    my $t={};
    bless $t,"Barts::Object::${t}::${o}";
    same(   "Bart's Funky Refs", $o,<<'EXPECT', ( $t ) );
$Barts_Object_HASH1 = bless( {}, 'Barts::Object::HASH(0xdeadbeef)::Data::Dump::Streamer=HASH(0xdeadbeef)' );
EXPECT
}

{
    my ($a,$b);
$a = [{ a => \$b }, { b => undef }];
$b = [{ c => \$b }, { d => \$a }];
    same("Simple Arrays of Simple Hashes", $o, <<'EXPECT', ( $a,$b ) );
$ARRAY1 = [
            { a => \$ARRAY2 },
            { b => undef }
          ];
$ARRAY2 = [
            { c => $ARRAY1->[0]{a} },
            { d => \$ARRAY1 }
          ];
EXPECT
    same(  "Predeclare Simple Arrays of Simple Hashes", $o->Declare(1),
        <<'EXPECT',( $a,$b ) );
my $ARRAY1 = [
               { a => 'R: $ARRAY2' },
               { b => undef }
             ];
my $ARRAY2 = [
               { c => 'V: $ARRAY1->[0]{a}' },
               { d => \$ARRAY1 }
             ];
$ARRAY1->[0]{a} = \$ARRAY2;
$ARRAY2->[0]{c} = $ARRAY1->[0]{a};
EXPECT
}
{
    my $x=\\\"foo";
    my $y=\\\$x;
    same( "Many Refs ( \$x, \$y )", $o->Declare(0),
         <<'EXPECT', ( $x, $y )  );
$REF1 = \\\'foo';
$REF2 = \\\$REF1;
EXPECT
    #same( "Many Refs ( \$x, \$y )", $o, <<'EXPECT', $x, $y  );
    #same( $dump = $o->Data( $x,$y )->Declare(1)->Out, <<'EXPECT', "Many Refs Declare ( \$x, \$y )", $o );
    same( "Many Refs Declare ( \$x, \$y )", $o->Declare(1),
         <<'EXPECT', ( $x, $y )  );
my $REF1 = \\\'foo';
my $REF2 = \\\$REF1;
EXPECT
    same( "Many Refs Declare ( \$y, \$x )", $o->Declare(1),
         <<'EXPECT', ( $y,$x ) );
my $REF1 = \\do{my $f='R: $REF2'};
my $REF2 = \\\'foo';
$$$REF1 = \$REF2;
EXPECT
    same("Many Refs ( \$y, \$x )", $o->Declare(0),
        <<'EXPECT', ( $y,$x ) );
$REF1 = \\\$REF2;
$REF2 = \\\'foo';
EXPECT
}
# with eval testing
{
    my $x=[(1) x 4, 0, (1) x 4];
    same( "Rle(1)", $o->Declare(0)->Rle(0), <<'EXPECT', ( $x ) );
$ARRAY1 = [
            1,
            1,
            1,
            1,
            0,
            1,
            1,
            1,
            1
          ];
EXPECT

    same( "Rle(1) Tight", $o->Verbose(0)->Indent(0)->Rle(1), <<'EXPECT', ( $x ) );
$A1 = [ ( 1 ) x 4, 0, ( 1 ) x 4 ];
EXPECT
    same( "Rle(1)", $o->Verbose(1)->Indent(2)->Rle(1), <<'EXPECT', ( $x ) );
$ARRAY1 = [
            ( 1 ) x 4,
            0,
            ( 1 ) x 4
          ];
EXPECT
    #local $Data::Dump::Streamer::DEBUG=1;
    my $one=1;
    #do this to avoid problems with differing behaviour in (1) x 3
    my @one=(1,1,1);
    my @two=(1,1,1);
    my $y=sub { \@_ }->(@one,$one,0,$one,@two);
    same( "Rle(1) Alias", $o->Rle(1), <<'EXPECT', ( $y ) );
$ARRAY1 = [
            ( 1 ) x 3,
            1,
            0,
            'A: $ARRAY1->[3]',
            ( 1 ) x 3
          ];
make_ro($ARRAY1->[4]);
alias_av(@$ARRAY1, 5, $ARRAY1->[3]);
EXPECT

}
{
    my $x={
            hash  => {0..5},
            array => [0..5],
            object => bless(\do{my $x='Foo!'},'Bar'),
            regex => qr/(?:baz)/,
          };

    same( "Indent", $o->Indent(2), <<'EXPECT', ( $x ) );
$HASH1 = {
           array  => [
                       0,
                       1,
                       2,
                       3,
                       4,
                       5
                     ],
           hash   => {
                       0 => 1,
                       2 => 3,
                       4 => 5
                     },
           object => bless( \do { my $v = 'Foo!' }, 'Bar' ),
           regex  => qr/(?:baz)/
         };
EXPECT
    same( "Indent(0)", $o->Indent(0), <<'EXPECT', ( $x ) );
$HASH1 = { array => [ 0, 1, 2, 3, 4, 5 ], hash => { 0 => 1, 2 => 3, 4 => 5 }, object => bless( \do { my $v = 'Foo!' }, 'Bar' ), regex => qr/(?:baz)/ };
EXPECT
    same( "IndentCols(0)", $o->Indent(2)->IndentCols(0), <<'EXPECT', ( $x ) );
$HASH1 = {
         array  => [
                   0,
                   1,
                   2,
                   3,
                   4,
                   5
                   ],
         hash   => {
                   0 => 1,
                   2 => 3,
                   4 => 5
                   },
         object => bless( \do { my $v = 'Foo!' }, 'Bar' ),
         regex  => qr/(?:baz)/
         };
EXPECT
    same( "IndentCols(4)", $o->Indent(2)->IndentCols(4), <<'EXPECT', ( $x ) );
$HASH1 = {
             array  => [
                           0,
                           1,
                           2,
                           3,
                           4,
                           5
                       ],
             hash   => {
                           0 => 1,
                           2 => 3,
                           4 => 5
                       },
             object => bless( \do { my $v = 'Foo!' }, 'Bar' ),
             regex  => qr/(?:baz)/
         };
EXPECT
    same( "IndentCols(2)", $o->Indent(2)->IndentCols(2), <<'EXPECT', ( $x ) );
$HASH1 = {
           array  => [
                       0,
                       1,
                       2,
                       3,
                       4,
                       5
                     ],
           hash   => {
                       0 => 1,
                       2 => 3,
                       4 => 5
                     },
           object => bless( \do { my $v = 'Foo!' }, 'Bar' ),
           regex  => qr/(?:baz)/
         };
EXPECT
}
{
    my $nums=['00123','00','+001','-001','1e40','-0.1000',-0.1000,1.0,'1.0'];
    same( "Numbers", $o, <<'EXPECT', ( $nums ) );
$ARRAY1 = [
            '00123',
            '00',
            '+001',
            '-001',
            '1e40',
            '-0.1000',
            -0.1,
            1,
            '1.0'
          ];
EXPECT
}
# with eval testing
{
    my ($x,$y)=10;
    my $obj=Dump();
    isa_ok($obj, "Data::Dump::Streamer","Dump() Return noarg/scalar");
    $obj=Dump($x,$y);
    isa_ok($obj, "Data::Dump::Streamer","Dump() Return arg/scalar");
    my @lines=Dump($x,$y);
    ok(!ref($lines[0]),"Dump() Return args/list");
    @lines=Dump($x,$y)->Indent(0)->Out();
    ok(!ref($lines[0]),"Dump() Return args/list-scalar");
}
# with eval testing
{
    my $x=1;
    my $y=[];
    my $array=sub{\@_ }->( $x,$x,$y );
    push @$array,$y,1;
    unshift @$array,\$array->[-1];
    #Dump($array);

    same( "Documentation example", $o, <<'EXPECT', ( $array ) );
$ARRAY1 = [
            'R: $ARRAY1->[5]',
            1,
            'A: $ARRAY1->[1]',
            [],
            'V: $ARRAY1->[3]',
            1
          ];
$ARRAY1->[0] = \$ARRAY1->[5];
alias_av(@$ARRAY1, 2, $ARRAY1->[1]);
$ARRAY1->[4] = $ARRAY1->[3];
EXPECT
}
# with eval testing
{
    my @a = ('a0'..'a9');
    unshift @a, \\$a[2];
    same( "merlyns test", $o, <<'EXPECT', ( \\@a ) );
$REF1 = \[
          \do { my $v = 'R: ${$REF1}->[3]' },
          'a0',
          'a1',
          'a2',
          'a3',
          'a4',
          'a5',
          'a6',
          'a7',
          'a8',
          'a9'
        ];
${${$REF1}->[0]} = \${$REF1}->[3];
EXPECT
}
{
    my @a = ('a0'..'a9');
    unshift @a, \\$a[2];
    test_dump( {name=>"merlyns test 2",
                verbose=>1}, $o, ( \\@a ),
               <<'EXPECT',  );
$REF1 = \[
          \do { my $v = 'R: ${$REF1}->[3]' },
          'a0',
          'a1',
          'a2',
          'a3',
          'a4',
          'a5',
          'a6',
          'a7',
          'a8',
          'a9'
        ];
${${$REF1}->[0]} = \${$REF1}->[3];
EXPECT
}


__END__
# with eval testing
{
    same( "", $o, <<'EXPECT', (  ) );
EXPECT
}
# without eval testing
{
    same( $dump = $o->Data()->Out, <<'EXPECT', "", $o );
EXPECT
}

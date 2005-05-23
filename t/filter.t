use Test::More tests => 9;
BEGIN { use_ok( 'Data::Dump::Streamer', qw(:undump) ); }
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
    my $ig=bless {},"Ignore";
    my %h=(One=>1,Two=>2,Three=>$ig);

    same( $dump = $o->IgnoreClass('Ignore'=>1)->Data( \%h )->Out, <<'EXPECT', "Ignore(1)", $o );
$HASH1 = {
           One   => 1,
           Three => 'Ignored Obj [Ignore=HASH(0x24b89cc)]',
           Two   => 2
         };
EXPECT
    same( $dump = $o->IgnoreClass('Ignore'=>0)->Data( \%h )->Out, <<'EXPECT', "Ignore(0)", $o );
$HASH1 = {
           One   => 1,
           Three => bless( {}, 'Ignore' ),
           Two   => 2
         };
EXPECT

}
{
    sub FreezeThaw::Freeze {
        my ($self)=@_;
        $_[0]=bless \do{my $x=join "-",@$self},ref $self;
    }
    sub FreezeThaw::Thaw {
        my ($self)=@_;
        $_[0]=bless [ map {split /-/,$_ } $$self ],ref $self;
    }
    my $ig=bless ["A".."D"],"FreezeThaw";
    my %h=(One=>1,Two=>2,Three=>$ig);

    same( $dump = $o->FreezeClass('FreezeThaw'=>'Freeze')
                    ->ThawClass('FreezeThaw'=>'Thaw')
    ->Data( \%h )->Out, <<'EXPECT', "FreezeThaw", $o );
$HASH1 = {
           One   => 1,
           Three => bless( \do { my $v = 'A-B-C-D' }, 'FreezeThaw' ),
           Two   => 2
         };
$HASH1->{Three}->Thaw();
EXPECT
    same( $dump = $o->FreezeClass('FreezeThaw'=>'Freeze')
                    ->ThawClass('FreezeThaw'=>'->Thaw')
    ->Data( \%h )->Out, <<'EXPECT', "FreezeThaw ->", $o );
$HASH1 = {
           One   => 1,
           Three => bless( \do { my $v = 'A-B-C-D' }, 'FreezeThaw' )->Thaw(),
           Two   => 2
         };
EXPECT
    same( $dump = $o->FreezeClass('FreezeThaw'=>'')
                    ->ThawClass('FreezeThaw'=>'')
    ->Data( \%h )->Out, <<'EXPECT', "FreezeThaw False", $o );
$HASH1 = {
           One   => 1,
           Three => bless( [
                      'A',
                      'B',
                      'C',
                      'D'
                    ], 'FreezeThaw' ),
           Two   => 2
         };
EXPECT
    same( $dump = $o->FreezeThaw('FreezeThaw'=>'Freeze','->Thaw')
    ->Data( \%h )->Out, <<'EXPECT', "FreezeThaw()", $o );
$HASH1 = {
           One   => 1,
           Three => bless( \do { my $v = 'A-B-C-D' }, 'FreezeThaw' )->Thaw(),
           Two   => 2
         };
EXPECT
    $o->FreezeClass();
    $o->ThawClass();
    same( $dump = $o->Data( \%h )->Out, <<'EXPECT', "FreezeThaw False", $o );
$HASH1 = {
           One   => 1,
           Three => bless( [
                      'A',
                      'B',
                      'C',
                      'D'
                    ], 'FreezeThaw' ),
           Two   => 2
         };
EXPECT

}__END__
# with eval testing
{
    same( "", $o, <<'EXPECT', (  ) );

}
# without eval testing
{
    same( $dump = $o->Data()->Out, <<'EXPECT', "", $o );
EXPECT
}

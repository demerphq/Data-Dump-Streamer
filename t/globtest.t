use Test::More tests=>11;
use lib './lib';
BEGIN { use_ok( 'Data::Dump::Streamer', qw(regex Dump alias_av alias_hv) ); }
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


my $o = Data::Dump::Streamer->new();

isa_ok( $o, 'Data::Dump::Streamer' );

{
	no strict;
	# no. 3 - a glob
	{
		local *g;
		same( scalar $o->Data(*g)->Out, <<'EXPECT', "a glob", $o );
$VAR1 = *::g;
EXPECT
	}

	# no. 4 - scalar slot
	{
		local *g = \"a string";
		## XXX: the empty globs are an icky 5.8.0 bug
		$^V lt v5.8 ?
		same( scalar $o->Data(*g)->Out, <<'EXPECT', "scalar slot", $o )
$VAR1 = *::g;
*::g = \'a string';
EXPECT
		:
		same( scalar $o->Data(*g)->Out, <<'EXPECT', "scalar slot", $o )
$VAR1 = *::g;
*::g = \'a string';
*::g = {};
*::g = [];
EXPECT
		;
	}

	# no. 5 - data slots
	{
		local *g;
		$g = 'a string';
		@g = qw/a list/;
		%g = qw/a hash/;
		same( scalar $o->Data(*g)->Out, <<'EXPECT', "data slots", $o );
$VAR1 = *::g;
*::g = \do { my $v = 'a string' };
*::g = { a => 'hash' };
*::g = [
         'a',
         'list'
       ];

EXPECT
	}

	# no. 6 - self glob
	{
		local *g;
		$g = *g{SCALAR};
		same( scalar $o->Data(*g)->Out, <<'EXPECT', "self glob", $o );
$VAR1 = *::g;
*::g = \do { my $v = 'V: *::g{SCALAR}' };
${*::g} = *::g{SCALAR};
EXPECT
	}

	# no. 7 - icky readonly scalars
	{
		local(*g, $s);
		*g = \"cannae be modified";
		$s = "do as you please";

		same( scalar $o->Data($g,$s)->Out, <<'EXPECT', "icky SCALAR slot", $o );
$RO1 = 'cannae be modified';
make_ro($RO1);
$VAR1 = 'do as you please';
EXPECT
	}
}
{
    #local $Data::Dump::Streamer::DEBUG=1;
    our $foo = 5;
    our @foo = (-10,\*foo);
    our %foo = (a=>1,b=>\$foo,c=>\@foo);
    $foo{d} = \%foo;
    $foo[2] = \%foo;
    same( "Named Globs", $o->Declare(0)->Names('*foo', '*bar', '*baz'), <<'EXPECT', ( \\*foo, \\@foo, \\%foo ) );
$foo = \\*::foo;
*::foo = \do { my $v = 5 };
$bar = \[
         -10,
         $$foo,
         'V: $$baz'
       ];
*::foo = $$bar;
$baz = \{
         a => 1,
         b => *::foo{SCALAR},
         c => $$bar,
         d => 'V: $$baz'
       };
*::foo = $$baz;
${$bar}->[2] = $$baz;
${$baz}->{d} = $$baz;
EXPECT
    same( "Named Globs Two", $o->Names('foo', 'bar', 'baz'), <<'EXPECT', ( \\*foo, \\@foo, \\%foo ) );
$foo = \\*::foo;
*::foo = \do { my $v = 5 };
$bar = \[
         -10,
         $$foo,
         'V: $$baz'
       ];
*::foo = $$bar;
$baz = \{
         a => 1,
         b => *::foo{SCALAR},
         c => $$bar,
         d => 'V: $$baz'
       };
*::foo = $$baz;
${$bar}->[2] = $$baz;
${$baz}->{d} = $$baz;
EXPECT
    same( "Named Globs Declare", $o->Declare(1)->Names('*foo', '*bar', '*baz'), <<'EXPECT', ( \\*foo, \\@foo, \\%foo ) );
my $foo = \\*::foo;
*::foo = \do { my $v = 5 };
my $bar = \[
            -10,
            $$foo,
            'V: $$baz'
          ];
*::foo = $$bar;
my $baz = \{
            a => 1,
            b => *::foo{SCALAR},
            c => $$bar,
            d => 'V: $$baz'
          };
*::foo = $$baz;
${$bar}->[2] = $$baz;
${$baz}->{d} = $$baz;
EXPECT
    same( "Named Globs Two Declare", $o->Names('foo', 'bar', 'baz'), <<'EXPECT', ( \\*foo, \\@foo, \\%foo ) );
my $foo = \\*::foo;
*::foo = \do { my $v = 5 };
my $bar = \[
            -10,
            $$foo,
            'V: $$baz'
          ];
*::foo = $$bar;
my $baz = \{
            a => 1,
            b => *::foo{SCALAR},
            c => $$bar,
            d => 'V: $$baz'
          };
*::foo = $$baz;
${$bar}->[2] = $$baz;
${$baz}->{d} = $$baz;
EXPECT
}

__END__
# with eval testing
{
    same( "", $o, <<'EXPECT', (  ) );

}
# without eval testing
{
    same( $dump = $o->Data()->Out, <<'EXPECT', "", $o );
EXPECT
}

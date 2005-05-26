package Data::Dump::Streamer;
use strict;
use warnings;
use Exporter;
use DynaLoader;
use Text::Balanced qw(extract_bracketed);
use B::Deparse;
use B qw(svref_2object);
use B::Utils qw(walkoptree_filtered opgrep);
use IO::File;
use Data::Dumper ();
use Data::Dump::Streamer::_::Printers;
use Symbol;
use warnings;
use warnings::register;

require overload;
use vars qw(
             $VERSION
             $XS_VERSION
             $AUTOLOAD
             @ISA
             @EXPORT @EXPORT_OK @EXPORT_FAIL %EXPORT_TAGS
             $DEBUG
           );

$DEBUG=0;

BEGIN {
    $VERSION   ='1.12';
    $XS_VERSION='1.12';
    $VERSION = eval $VERSION; # used for beta stuff.
    @ISA       = qw(Exporter DynaLoader);

    @EXPORT=qw(Dump);
    @EXPORT_OK = qw(
        Dump
        Stream
        alias_av
        alias_hv
        alias_ref
        push_alias
        dualvar

        alias_to

        blessed
        reftype
        refaddr
        refcount
        sv_refcount
        looks_like_number
        regex
        readonly
        make_ro
        _make_ro
        reftype_or_glob
        refaddr_or_glob
        globname
        is_numeric

        all_keys
        legal_keys
        hidden_keys
        lock_ref_keys
        lock_keys
        lock_ref_keys_plus
        lock_keys_plus
        SvREADONLY_ref
        SvREFCNT_ref
        isweak
        weaken
        weak_refcount

        readonly_set

        Dumper
        DDumper


   );

    %EXPORT_TAGS = (
        undump => [ qw( alias_av alias_hv alias_ref make_ro
                        lock_ref_keys
                        lock_keys
                        lock_ref_keys_plus
                        lock_keys_plus
                        alias_to
                        dualvar
                        weaken
                      )
                  ],
        special=> [ qw( readonly_set ) ],
        all    => [ @EXPORT,@EXPORT_OK ],
        alias  => [ qw( alias_av alias_hv alias_ref push_alias ) ],
        bin    => [ @EXPORT_OK ],
        Dumper => [ qw( Dumper DDumper )],
        util   => [ qw (
                        dualvar
                        blessed reftype refaddr refcount sv_refcount
                        readonly looks_like_number regex is_numeric
                        make_ro readonly_set reftype_or_glob
                        refaddr_or_glob globname
                        weak_refcount isweak weaken
                      )
                  ],

    );


    sub alias_to { return shift }

    #warn $VERSION;
    Data::Dump::Streamer->bootstrap($XS_VERSION);
    if ($]<=5.008) {
        *hidden_keys=sub(\%)  { return () };
        *legal_keys=sub(\%)   { return keys %{$_[0]} };
        *all_keys=sub(\%\@\@) { @{$_[1]}=keys %{$_[0]}; @$_[2]=(); };
    }        
    if ( $]<5.008 ) {
            no strict 'refs';
            foreach my $sub (qw(lock_keys lock_keys_plus )) {
                *$sub=sub(\%;@) {
                    warnings::warn "$sub doesn't do anything before Perl 5.8.0\n";
                    return $_[0];
                }
            }
            foreach my $sub (qw(lock_ref_keys lock_ref_keys_plus )) {
                *$sub=sub($;@) {
                    warnings::warn "$sub doesn't do anything before Perl 5.8.0\n";
                    return $_[0];
                }
            }
    } else {
        eval <<'EO_HU'
        use Hash::Util qw(lock_keys);
        sub lock_ref_keys($;@) {
            my $hash=shift;
            Carp::confess("lock_ref_keys(): Not a ref '$hash'")
                unless ref $hash;
            lock_keys(%$hash,@_);
            $hash
        }
EO_HU
        ;
        *lock_ref_keys_plus=sub($;@){
            my ($hash,@keys)=@_;
            my @delete;
            Internals::hv_clear_placeholders(%$hash);
            foreach my $key (@keys) {
                unless (exists($hash->{$key})) {
                    $hash->{$key}=undef;
                    push @delete,$key;
                }
            }
            SvREADONLY_ref($hash,1);
            delete @{$hash}{@delete};
            $hash
        };
        *lock_keys_plus=sub(\%;@){lock_ref_keys_plus(@_)};
    }
    my %fail=map { ( $_ => 1 ) } @EXPORT_FAIL;
    @EXPORT_OK=grep { !$fail{$_} } @EXPORT_OK;
}

# NOTE
# ----
# This module uses the term 'sv' in a way that is misleading.
# It doesnt always mean the same as it would in the core.
#
# 1. data is breadth first traversed first, in the pretty much
# self contained Data() routine which farms out a bit to
# _reg_ref and _reg_scalar which handle "registering" items for
# later use, such as their depth, refcount, "name", etc. But
# ONLY for references and scalars whose refcount is over 2.
# Most real SV's will have a refcount of 2 when we look at them
# (from the perl side) so we actually dont know about them (trust me)
# They _cant_ be referenced twice, and they can't be aliased so we can
# can just ignore them until the second pass.
# 2.Once this has happened Out() is called which starts off a
# normal depth first traverse over the structure. It calls into
# 3._dump_sv which in the case of a reference falls through to _dump_rv.
# Aliasing and a bunch of stuff like that are checked here before we even
# look at the reference type.
# 4.If its a ref we fall through to dumping the reference in _dump_rv.
# Here we handle duplicate refs, and manage depth  checks, blessing, refs
#(which is scary nasty horrible code) and then pass on to _dump_type where
# type is one of 'code', 'qr', 'array' etc. Each of these which have children
# then call back into _dump_sv as required.
# 5. Because of the way perl works, we can't emit anthing more than a DAG in a
# single statment, so for more complex structures we need to add in the broken
# links. I call these "fix statements", and they encompass copying reference
# values, creating aliases, or even dumping globs.  When a fix statment is needed
# any of the _dump_foo methods will call _add_fix and add to the list of fixes.
# after every root level _dump_sv call from Out() any fix statements possible to be
# resolved will be emitted and removed from the fix list. This happens in
# _dump_apply_fix, which is another piece of horrible code.
#
# Anyway, its terribly ugly, but for anything I can think to throw at it it works.
# demerphq

=head1 NAME

Data::Dump::Streamer - Stream a highly accurate breadth first data dump in perl code form to a var or file.
(Also known as 'DDS')

=head1 SYNOPSIS

  use Data::Dump::Streamer;
  use DDS;                           # optionally installed alias

  Dump($x,$y);                       # Prints to STDOUT
  Dump($x,$y)->Out();                #   "          "

  my $o=Data::Dump::Streamer->new(); # Returns a new ...
  my $o=Dump();                      # ... uninitialized object.

  my $o=Dump($x,$y);                 # Returns an initialized object
  my $s=Dump($x,$y)->Out();          #  "  a string of the dumped obj
  my @l=Dump($x,$y);                 #  "  a list of code fragments
  my @l=Dump($x,$y)->Out();          #  "  a list of code fragments

  Dump($x,$y)->To(\*STDERR)->Out();  # Prints to STDERR

  Dump($x,$y)->Names('foo','bar')    # Specify Names
             ->Out();

  Dump($x,$y)->Indent(0)->Out();     # No indent

  Dump($x,$y)->To(\*STDERR)          # Output to STDERR
             ->Indent(0)             # ... no indent
             ->Names('foo','bar')    # ... specify Names
             ->Out();                # Print...

  $o->Data($x,$y);                   # OO form of what Dump($x,$y) does.
  $o->Names('Foo','Names');          #  ...
  $o->Out();                         #  ...

=head1 DESCRIPTION

Converts a data structure into a sequence of perl statements sufficient for
recreating the original via eval.  This module is very similar in concept to
L<Data::Dumper|Data::Dumper> and L<Data::Dump|Data::Dump>, with the major differences being that this
module is designed to output to a stream instead of constructing its output
in memory, and that the traversal over the data structure is effectively breadth
first versus the depth first traversal done by the others.

In fact the data structure is scanned twice, first in breadth first mode to
perform structural analysis, and then in depth first mode to actually produce
the output, but obeying the depth relationships of the first pass.

=head2 Usage

While Data::Dump::Streamer is at heart an object oriented module, it is expected
(based on experience with using L<Data::Dumper|Data::Dumper>) that the common
case will not exploit these features. Nevertheless the method based approach
is convenient and accordingly a compromise hybrid approach has been provided
via the C<Dump()> subroutine.

All attribute methods are designed to be chained together.  This means that
when used as set attribute (called with arguments) they return the object they
were called against. When used as get attributes (called without arguments)
they return the value of the attribute.

From an OO point of view the key methods are the C<Data()> and C<Out()> methods.
These correspond to the breadth first and depth first traversal, and need to be
called in this order. Some attributes I<must> be set prior to the C<Data()> phase
and some need only be set before the C<Out()> phase.

Attributes once set last the lifetime of the object, unless explicitly reset.

=head3 Caveats Dumping Closures (CODE Refs)

As of version 1.11 DDS has had the ability to dump closures properly. This means 
that the lexicals that are bound to the closure are dumped along with the 
subroutine that uses them. This makes it much easier to debug code that uses 
closures and to a certain extent provides a persistancy framework for closures. 
The way this works is that DDS figures out what all the lexicals are that are 
bound to CODE refs it is dumping and then pretends that it had originally been 
called with all of them as its arguements.

One consequence of the way the dumping process works is that all of the 
recreated subroutines will be in the same scope. This of course can lead to 
collisions as two subroutines can easily be bound to different variables that 
have the same name. 

The way that DDS resolves these collisions is that it renames one of the variables 
with a special name so that presumably there are no collisions.  
However this process is very simplistic, no checks currently occur to prevent 
collisions with other lexicals or other globals that may be 
used by the dumped code.  In some situations it may be necessary to change the
default value of the rename template which may be done by using the C<EclipseName>
method.

Similarly to the problem of colliding lexicals is the problem of colliding
lexicals and globals. DDS pays no attention to globals when dumping closures
which can potentially result in lexicals being declared that will eclipse their
global namesake. There is currently no way around this other than to avoid
accessing a global and a lexical with the same name from the subs being dumped. 
And example is

  my $a=sub { $a++ }; 
  Dump( sub { $a->() } );

which will not be dumped correctly. Generally speaking this kind of thing is
bad practice anyway, so this could probably be viewed as a less than desirable
"feature". :-)

Generally if the closures being dumped avoid accessing lexicals and globals 
with the same name from out of scope and that all of the CODE being dumped 
avoids vars with the C<EclipseName> in their names the dumps should be valid 
and should eval back into existance properly.

Note that the behaviour of dumping closures is subject to change in future versions
as its possible that I will put some additional effort into more sophisiticated
ways of avoiding name collisions in the dump.

=head3 Controlling Hash Traversal and Display Order

Data::Dump::Streamer supports a number of ways to control the traversal order of hashes.
This functionality is controlled via the SortKeys() and HashKeys() accessor methods.
SortKeys() is used to specify the generic ordering of all hashes, and HashKeys() is
for specifying the ordering for a specific hashreference, or for all hashes of a
given class. SortKeys() takes only a single parameter, and HashKeys() takes a list
of pairs. See their documentation for more detail.

By default the traversal of hashes is in B<C<'smart'>> order, which is something like
a dictionary order, and is suitable for mixed numeric and text keys or for either.
Two other standard orders are provided, B<C<'alpha'>>-betical or B<C<'lex'>>-icographical and
B<C<'num'>>-eric. You may also specify that perls native ordering of the hash be used by
specifying false but defined, and have it fallback to a more general ordering rule
with B<C<undef>>.

In addition to these preprogrammed orderings you may also provide an B<C<ARRAY>> ref
containing a list of keys (and implicitly their order), or a B<C<HASH>> ref used to
determine which keys are displayed, and if they are always shown (key=>1) or
only shown if they exist (key=>0).

For extremely fine tuning you can provide a B<C<CODE>> ref that will be provided
the hash reference being dumped and the pass on which it is being dumped which
is expected to return one of the above values. Thus you can say:

  ->SortKeys('lex') # use lex by default
  ->HashKeys('Foo::Bar::Baz'=>sub{
                  my $hash=shift;
                  if ($hash == $special) {return [qw(a b c)]}
                  elsif (UNIVERSAL::isa($hash,'Foo::Bar')) { return 'smart' }
                  elsif (scalar keys %$hash>1000) { return 0 } # force each() use
                  else {return undef} #fallback
                })

And have it all work out as expected. (Well, that is if you really need to apply
such a crazy rule :-)

The order in which the rules are applied is:

  1. Object Specific via HashKeys() settings
  2. Class Specifc via HashKeys() settings
  3. Generic via SortKeys() settings
  4. Use perls internal hash ordering.

=head3 Controlling Array Presentation and Run Length Encoding

By default Data::Dump::Streamer will "run length encode" array values.
This means that when an array value is simple (ie, its not referenced and
does contain a reference) and is repeated mutliple times the output will
be single a list multiplier statement, and not each item output seperately.
Thus: C<Dump([0,0,0,0])> will be output somthing like

   $ARRAY1 = [ (0) x 4 ];

This is particularly useful when dealing with large arrays that are only
partly filled, and when accidentally the array has been made very large,
such as with the improper use of pseudo-hash notation.

To disable this feature you may set the Rle() property to FALSE, by default
it is enabled and set to TRUE.

=head3 Controlling Object Representation (Freeze/Thaw)

This module provides hooks for specially handling objects. Freeze/Thaw for generic
handling, and FreezeClass/ThawClass for class specific handling. These hooks work as
follows (and it should be understood that Freeze() below refers to both it and FreezeClass
as does Thaw() refer to ThawClass() as well.

If a Freeze() hook is specified then it is called on the object during the
Data() phase prior to traversing the object. The freeze hook may perform whatever
duties it needs and change its internal structure, _or_ it may alter $_[0] providing
a substitute reference to be dumped instead (note that this will not alter the data
structure being dumped). This reference may even be a totally different type!

If a Thaw() hook is specified then as part of the dump code will be included to
rebless the reference and then call the hook on the newly created object. If the code
was originally frozen (not replaced) the method will be called on the object to unfreeze
it during the Out() phase of the dump, leaving the structure unmodified after the dump.
If the object was replaced by the freeze hook this doesnt occur as it assumed the data
structure has not changed.  A special rule applies to Thaw() hooks in that if they include
the prefix "->" then they are not executed inline, and as such expected to return the object,
but as an independent statement after the object hash been created created, and the return
of the statement is ignored. Thus a method that simply changes the internal state of the object
but doesn't return an object reference may be used as a Thaw() handler.

For now these options are specified as string values representing the method names. Its
possible a later version will extend this to also handle codrefs.

B<Note> that the Freeze/Thaw methods will NOT be executed on objects that don't support those
methods. The setting in this case will be silently ignored.

=head2 Installing I<DDS> as an alias

Its possible to have an alias to Data::Dump::Streamer created and installed
for easier useage in one liners and short scripts. Data::Dump::Streamer is a
bit long to type sometimes. However because this technically means polluting
the root level namespace, and having it listed on CPAN, I have elected to have
the installer not install it by default. If you wish it to be installed you
must explicitly state so when Makefile.Pl is run:

  perl Makefile.Pl DDS [Other MakeMaker options]

Then a normal 'make test, make install' invocation will install DDS.

Using DDS is identical to Data::Dump::Streamer.

=head2 Data::Dumper Compatibility

For drop in compatibility with the Dumper() usage of Data::Dumper, you may request
that the L<Dumper> method is exported. It will not be exported by default. In addition
the standard Data::Dumper::Dumper() may be exported on request as 'DDumper'. If you
provide the tag ':Dumper' then both will be exported.

=over 4

=item Dumper

=item Dumper LIST

A synonym for scalar Dump(LIST)->Out for usage compatibility with L<Data::Dumper|Data::Dumper>

=item DDumper

=item DDumper LIST

A secondary export of the actual L<Data::Dumper::Dumper|Data::Dumper> subroutine.

=back 4

=head2 Constructors

=over 4

=item new

Creates a new Data::Dump::Streamer object. Currently takes no arguments and simply
returns the new object with a default style configuration.

See C<Dump()> for a better way to do things.

=cut

sub new {
    my $class = shift;
    my $self = bless {
        style => {
            hashsep      => ' => ',    # use this to seperate key vals
            bless        => 'bless()', # use this to bless ojects, needs fixing
            indent       => 2,    # should we indent at all?
            indentkeys   => 1,         # indent keys
            declare      => 0,         # predeclare vars? allows refs to root vars if 0
            sortkeys     => 'smart',   # sort ordering, smart, alpha, numeric, or subref
            hashkeys     => undef,
            verbose      => 1,         # use long names and detailed fill ins
            dumpglob     => 1,         # dump glob contents
            deparseglob  => 1,
            deparse      => 1,         # deparse code refs?
            freeze       => '',        # default freezer
            thaw         => '',        # default thaw
            freezeclass  => {},        # freeze classes
            thawclass    => {},        # thaw classes
            rle          => 1,         # run length encode arrays
            ignoreclass  => {},        # ignore classes
            indentcols   => 2,         # indent this numbe of cols
            ro           => 1,         # track readonly vars
            dualvars     => 1,         # dump dualvars
            eclipsename  => "%s_eclipse_%d",

            purity       => 1,         # test

            # use this if deparse is 0
            codestub     => 'sub { Carp::confess "Dumped code stub!" }',
            formatstub   => 'do{ local *F; eval "format F =\nFormat Stub\n.\n"; *F{FORMAT} }',
            # use these opts if deparse is 1
            deparseopts  => ["-sCi2v'Useless const omitted'"],
            special     =>0,



            # not yet implemented
            array_warn  => 10_000,    # warn if an array has more than this number of elements
            array_chop  => 32_767,    # chop arrays over this size
            array_max   => 1_000_000, # die if arrays have more than this size
            smart_array => 1,         # special handling of very large arrays
                                      # with hashes as their 0 index. (pseudo-hash error detection)
        },
        debug=>0,
        cataloged => 0,
        ref_id =>0,
        sv_id =>0
    }, $class;

    return $self;
}

sub _safe_self {
    my $self = shift;
    unless ( ref $self ) {
        $self = $self->new();
    }
    return $self;
}

sub Dumper {
    return scalar Dump(@_)->Out();
}

sub DDumper {
    return Data::Dumper::Dumper(@_);
}

#sub _is_utf8 { length $_[0] != do { use bytes; length $_[0] } }

BEGIN {
    my $numeric_rex=qr/^-?(?:0|[1-9]\d*)(\.\d+(?<!0))?$/;

    # used by _qquote below
    my %esc = (
        "\a" => "\\a",
        "\b" => "\\b",
        "\t" => "\\t",
        "\n" => "\\n",
        "\f" => "\\f",
        "\r" => "\\r",
        "\e" => "\\e",
    );

    # Taken from Data::Dumper::qquote() 2.12.
    # Changed utf8 handling from that version
    # put a string value in double quotes
    # Fixes by [ysth]
    sub _qquote {
        local ($_) = shift;
        s/([\\\"\@\$])/\\$1/g;

        return qq("$_") # fast exit
          unless /[^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~]/;

        s/([\a\b\t\n\f\r\e])/$esc{$1}/g;

        if ( ord('^') == 94 ) {
            # ascii / utf8
            # no need for 3 digits in escape if followed by a digit
            s/([\0-\037])(?!\d) / sprintf '\\%o',    ord($1)/xeg;
            s/([\0-\037\177])   / sprintf '\\%03o',  ord($1)/xeg;

            if (length $_ != do { use bytes; length $_ }) {
                use utf8; #perl 5.6.1 needs this, 5.9.2 doesnt. sigh
                s/([\200-\377]) / sprintf '\\%03o',  ord($1)/xeg;
                s/([^\040-\176])/ sprintf '\\x{%x}', ord($1)/xeg;
            } else {
                # must not be under "use utf8" for 5.6.x
                s/([\200-\377]) / sprintf '\\%03o',  ord($1)/xeg;
            }
        } else {
            # ebcdic
            s{([^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~])(?!\d)}
               {my $v = ord($1); '\\'.sprintf(($v <= 037 ? '%o' : '%03o'), $v)}eg;
            s{([^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~])}
               {'\\'.sprintf('%03o',ord($1))}eg;
        }

        return qq("$_");
    }


    # single quote
    sub _quote {
        my $v = join "", @_;
        if ($v=~$numeric_rex) {
            return $v;
        } elsif ($v!~/[^\x20-\x7E]/) {
            $v =~ s/([\\''])/\\$1/g;
            return "'$v'";
        }
        return _qquote($v);
    }

    # quote a key
    sub _quotekey {
        my $key = shift;
        if (!defined($key) or $key eq '') {
            return '""'
        } elsif ($key=~$numeric_rex or $key =~ /^[-A-Za-z_]\w*$/) {
            return $key
        } else {
            _qquote($key);
        }
    }
}

my %ttrans = (
    reftype( {} )      => '%',
    reftype( [] )      => '@',
    reftype( \ 'foo' ) => '$',
    reftype( \\'foo' ) => '$', # REF
    reftype( sub{} )   => '&',
    ''                 => '$',
);


sub _make_name {
    my ( $self, $obj, $indx ) = @_;
    #warn Dumper($self->{unames})."'$self->{unames}' : @{$self->{unames}||[]} @{[defined $indx ? $indx : '-']}";
    my $uname = ( $self->{unames} || [] )->[ $indx || 0 ];
    unless ($uname) {
        my $name = blessed($_[1])
                  || reftype($_[1])
                  || (readonly($_[1])  ? "RO" : "VAR");
        unless ($self->{style}{verbose}) {
            my $n=1;
            (my $abr=$name)=~s/(\w)\w*::/$1/g;
            $self->{type_abrv}{$name}||=$name;
            while ($n<=length($abr) and
                   $self->{type_abrv}{substr($abr,0,$n)} and
                   $self->{type_abrv}{substr($abr,0,$n)} ne $name) {
                $n++;
            }
            if ($n<=length($abr)) {
                $self->{type_abrv}{substr($abr,0,$n)}=$name;
                return '$' . substr($abr,0,$n) . ( ++$self->{type_ids}{$name} );
            }
        }
        $name =~ s/::/_/g;
        ($name)=$name=~/(\w+)/; #take the first word;
        return '$' . $name . ( ++$self->{type_ids}{$name} );
    } elsif ( $uname =~ /^\*/ ) {
        my $type = reftype( $_[1] ) || '';
        $uname =~ s//$ttrans{$type}/;
        $uname;
    } else {
        return '$' . $uname;
    }
}

#=item diag
#
#Outputs to STDOUT a list of all values that have been identified of being
#worth of study. For development/debugging purposes only at this point.
#
#=cut

sub diag {
    my $self=shift;
    my $handle=shift || \*STDOUT;
    print $handle "+---+\n";

    my $oidx;
    foreach my $idx (1..$self->{sv_id}) {
        print $handle $self->diag_sv_idx($idx);
    }
    print "-----\n" if $self->{ref_id} and $self->{sv_id};
    foreach my $idx (1..($self->{ref_id}||0)) {
        print $handle $self->diag_ref_idx($idx);

    }
    print $handle "+---+\n";
    $self;
}

sub remove_deref {
    my $var=shift;

    my ($brace,$rest,$sigil);
    if ($var=~s/^([\@\%\$])(?=\$)//) {
        ($sigil,$brace)=($1,$var)
    } else {
        local $@;
        ($brace,$rest,$sigil)= extract_bracketed( $var, '{q}',qr/[\@\%\$]/ );
    }
    if ($brace and !$rest) {
        $brace=~s/^\{(.*)\}$/$1/;
        return wantarray ? ($sigil,$brace) : $brace;
    } else {
        return;
    }
}

my %tname=qw(HASH % ARRAY @ SCALAR $ REF $);

sub _build_name {
    my ( $self, $name, $type, $val ) = @_;

    $DEBUG>1 and print STDOUT "  _build_name( $name '$type' => ";
    $type=$tname{$type} if $tname{$type};
    if ($type=~/[[{]/) {

        $name=~s/[\@\%]\$/\$/;
        my ($sigil,$brace)=remove_deref($name);
        if ( $name =~ /^([\@\%\$])(\w+)$/ or $sigil
             or $name=~/^\*.*\{(?:SCALAR|HASH|ARRAY)\}$/
           )
        {

            $name .= '->' if !($name =~ s/^[\@\%]/\$/)
                          or $sigil;
            $name=~s/^\$(\$.*)->$/\$\{$1\}->/;
        }

        $DEBUG>1 and print STDOUT "$name => ";

        if ( $type eq '[' ) {
            $name .= "[$val]";
        } elsif ( $type eq '{' ) {
            $name .= "{" . _quotekey($val) . "}";
        } else {
            Carp::confess "Fallen off the end of the world...";
        }
    } elsif ( $type =~ /^[\@\%\$]$/ ) {
        $name = "{$name}"
           if $name =~ /[\[\{]/ or $name=~/^\*/;
        $name = $type . $name
          unless substr( $name, 0, 1 ) eq $type and $type ne '$';

    } else {
        no warnings;
        Carp::confess "unimplemented _build_name";
    }
    $DEBUG>1 and print "$name )\n";
    $name;
}

sub _reset {
    my $self=shift;
    foreach my $key (keys %$self) {
        next unless $key=~/^(sv|ref|fix|cat|type|names|reqs)/;
        delete $self->{$key};
    }
    $self->{sv_id}=$self->{ref_id}=0;
    $self;
}

sub diag_sv_idx {
    my $self=shift;
    my $idx=shift;
    my $prefix=shift||'';
    my $oidx=$self->{ref}{$self->{sva}[$idx]};
    my $ret=$prefix.
    sprintf "S%s%2d : %#x(c%2d|%2d) Dp:%2d %s Du:%s => %s %s %s %s\n",
        ($self->{special}{$idx} ? '*' : ' '),$idx,
        (map { $self->{$_}[$idx] } qw( sva svc svt svd )),
        ($self->{svro}[$idx] ? 'RO ' : 'RW'),
        (!$self->{svdu}[$idx] ? '-' : defined ${$self->{svdu}[$idx]} ? ${$self->{svdu}[$idx]} : '?'),
        $self->{svn}[$idx],
        (defined $self->{unames}[$idx-1] ? "($self->{unames}[$idx-1])" : ""),
        (($oidx) ? "< $self->{refn}[$oidx] >" : ""),
        ($self->{svon}{$idx} ? ": $self->{svon}{$idx}" : "")
        ;
    if ($prefix and $oidx) {
        $ret.=$prefix.$self->diag_ref_idx($oidx);
    }
    $ret;
}

sub diag_ref_idx {
    my $self=shift;
    my $idx=shift;
    my $oidx=$self->{sv}{$self->{refa}[$idx]};
    sprintf "R %2d : %#x(c%2d|%2d) Dp:%2d    Du:%s => %s %s\n",
        $idx,(map { defined $self->{$_}[$idx] ?  $self->{$_}[$idx] : -1} qw(refa refc reft refd )),
        (!$self->{refdu}[$idx] ? '-' : defined ${$self->{refdu}[$idx]} ? ${$self->{refdu}[$idx]} : '?'),
        $self->{refn}[$idx],
        (($oidx) ? " < $self->{svn}[$oidx] >" : "")
        ;
}


=item Dump

=item Dump VALUES

Smart non method based constructor.

This routine behaves very differently depending on the context it is called in
and whether arguments are provided.

If called with no arguments it is exactly equivelent to calling

  Data::Dump::Streamer->new()

which means it returns an object reference.

If called with arguments and in scalar context it is equivelent to calling

  Data::Dump::Streamer->new()->Data(@vals)

except that the actual depth first traversal is I<delayed> until C<Out()> is called.
This means that options that must be provided before the C<Data()> phase can be provided
after the call to C<Dump()>.  Again, it returns a object reference.

If called with arguments and in void or list context it is equivelent to calling

  Data::Dump::Streamer->new()->Data(@vals)->Out()

The reason this is true in list context is to make C<print Dump(...),"\n";> do the right
thing. And also that combined with method chaining options can be added or removed as
required quite easily and naturally.

So to put it short:

  my $obj=Dump($x,$y);         # Returns an object
  my $str=Dump($x,$y)->Out();  # Returns a string of the dump.
  my @code=Dump($x,$y);        # Returns a list of the dump.

  Dump($x,$y);                 # prints the dump.
  print Dump($x,$y);           # prints the dump.

It should be noted that the setting of C<$\> will affect the behaviour of both of

  Dump($x,$y);
  print Dump($x,$y);

but it will not affect the behaviour of

  print scalar Dump($x,$y);

B<Note> As of 1.11 Dump also works as a method, with identical properties as when called
as a subroutine, with the exception that when called with no arguments it is a synonym for
C<Out()>. Thus

  $obj->Dump($foo)->Names('foo')->Out();
  
will work fine, as will the odd looking:

  $obj->Dump($foo)->Names('foo')->Dump();

which are both the same as

  $obj->Names('foo')->Data($foo)->Out();
  
Hopefully this should make method use more or less DWIM.  

=cut

my %args_insideout;

sub DESTROY {
    my $self=shift;
    delete $args_insideout{$self} if $self;
}

sub Dump {
    my $obj;
    if ( blessed($_[0]) and blessed($_[0]) eq __PACKAGE__ ) {
        $obj=shift;
    }
    if (@_) {
        if ( defined wantarray and !wantarray ) {
            $obj ||= __PACKAGE__->new();
            $args_insideout{$obj}= $obj->_make_args(@_);
            return $obj;
        } else {
            $obj||=__PACKAGE__;
            return $obj->Data(@_)->Out();
        }
    } else {
        if ($obj) {
            return $obj->Out();
        } else {
            return __PACKAGE__->new();
        }
    }
}


sub _reg_ref {
    my ($self,$item,$depth,$name,$cnt,$arg)=@_;

    my $addr=refaddr $item;
    $arg->{raddr}=$addr if $arg;
    my $idx;
    unless ($idx=$self->{ref}{$addr}) {
        $idx=$self->{ref}{$addr}=++$self->{ref_id};
        $arg->{ridx}=$idx if $arg;
        $self->{refn}[$idx]=$name;
        $self->{refd}[$idx]=$depth;
        $self->{refa}[$idx]=$addr;
        $self->{refc}[$idx]=$cnt;
        return wantarray ? ($idx,0) : $idx
    }
    $self->{reft}[$idx]++;
    $arg->{ridx}=$idx if $arg;
    return wantarray ? ($idx,1) : undef;
}


sub _reg_scalar {
    my ($self,$item,$depth,$cnt,$ro,$name,$arg)=@_;
    Carp::cluck $name if $name=~/^\$\*/;
    my $addr=refaddr \$_[1];
    my $idx;
    $arg->{addr}=$addr if $arg;
    unless ($idx=$self->{sv}{$addr}) {
        $idx=$self->{sv}{$addr}=++$self->{sv_id};
        $self->{svd}[$idx]=$depth;
        $self->{sva}[$idx]=$addr;
        $self->{svro}[$idx]=$ro;
        $self->{svc}[$idx]=$cnt;
        $self->{svw}{$addr}=!0 
            if isweak($_[1]);
        ($self->{svn}[$idx]=$name)=~s/^[\@\%\&]/\$/;
        if ($self->{svn}[$idx] ne $name) {
            $self->{svn}[$idx].="_"; #XXX
            #warn "$self->{svn}[$idx] ne $name"
            $self->{svon}{$idx}=$name;
        }

    } else{
        if ($DEBUG>9) {
            print $self->diag_sv_idx($idx);
           print "$name is already registered as $self->{svn}[$idx] Depth ($self->{svd}[$idx]) $depth\n";
        }
        if ($self->{svn}[$idx]=~/^\$\{?\$/ and $name!~/^\$\{?\$/) {
            $self->{svn}[$idx]=$name;
        }
    }
    $self->{svt}[$idx]++;
    $arg->{idx}=$idx if $arg;
    Carp::confess "Dupe name!" if $self->{svrt}{$name};
    $self->{svrt}{$name}=$idx;
    return $name;
}

*Precise=\&Dump;

sub _make_args {
    my $self=shift;
            [
                map {
                        {
                                item   => \$_[$_],
                                ro     => readonly($_[$_]),
                                refcnt => sv_refcount($_[$_]),
                        }
                    } 0..$#_
            ];
}

=back 4

=head2 Methods

=over 4

=item Data

=item Data LIST

Analyzes a list of variables in breadth first order.

If called with arguments then the internal object state is reset before
scanning the list of arguments provided.

If called with no arguments then whatever arguments were provided to C<Dump()>
will be scanned.

Returns $self.

=cut


sub _add_queue {
    my ($self,$queue,$type,$item,$depth,$name,$rcount,$arg)=@_;
    if (substr($type,0,1) ne '*') {
        push @$queue,[\$item,$depth,$name,$rcount,$arg];
    } elsif($self->{style}{dumpglob}) {
        local @_;
        foreach my $t ($self->_glob_slots('FORMAT')) {

            #warn $type.":$t\n";
            #register?
            #$self->_reg_scalar(*$item{$t},$depth+1,sv_refcount(*$item{$t}),readonly(*$item{$t}),'*'.$name."{$t}");

            my $v=*$item{$t};
            next unless defined $v;
            next if $t eq 'SCALAR' and !defined($$v);
            push @$queue,[\*$item{$t},$depth+1,$type."{$t}",refcount(\*$item{$t})];
        }
    }
    #use Scalar::Util qw(weaken);
    $self;
}

sub Data {
    my $self=shift->_safe_self;
    my $args;
    print "Data(".scalar(@_)." vars)\n"
        if $DEBUG;
    if (@_) {
        $self->_reset;
        $args_insideout{$self}=$self->_make_args(@_);
    } elsif ($self->{cataloged}) {
        $self->_reset;
    }
    $args=$args_insideout{$self} || Carp::confess "No arguments!";
    my $pass=1;
PASS:{    
        my @queue;
        my $idx=0;
        foreach my $arg (@$args) {
            #($self,$item,$depth,$cnt,$ro,$name)
            my $make_name=$self->_make_name(${ $arg->{item} },$idx++);
            my $name=$self->_reg_scalar(
                ${ $arg->{item} },
                1,
                $arg->{refcnt},
                $arg->{ro},
                $make_name,
                $arg
            );
            $arg->{name}=$name;
            if (my $type=reftype_or_glob ${ $arg->{item} }) {
                $self->_add_queue(\@queue, $type, ${ $arg->{item} }, 2, $name, refcount ${ $arg->{item} },$arg)
            }
        }
        
        my %lex_addr;
        my %lex_addr2name;
        my %lex_name;
        my %lex_special;
        
        while (@queue) {
            # If the scalar (container) is of any interest it is
            # already registered by the time we see it here.
            # at this point we only care about the contents, not the
            # container.
            print Data::Dumper->new([\@queue],['*queue'])->Maxdepth(3)->Dump
                if $DEBUG>=10;
    
            my ($ritem,
                $cdepth,
                $cname,
                $rcnt,
                $arg)=@{shift @queue};
                
            
    
            my ($frozen,$item,$raddr,$class);
            DEQUEUE:{
                $item=$$ritem;
                $raddr=refaddr($item);
                $class=blessed($item);
    
                $DEBUG and
                print "Q-> $item $cdepth $cname $rcnt ($raddr)\n";
    
                unless ($raddr) {
                    $DEBUG and
                    print "  Skipping '$cname' as it isn't a reference.\n";
                    next;
                }
    
                if ($class and !$frozen) {
                    if ($self->{style}{ignoreclass}{$class}) {
                        $DEBUG and
                        print "Ignoring '$cname' as its class ($class) in our ignore list.\n";
                        next;
                    } elsif (my $meth=$self->{style}{freezeclass}{$class}||$self->{style}{freeze}){
                        if (${$ritem}->can($meth)) {
                            ${$ritem}->$meth();
                            unless (refaddr($$ritem)==$raddr) {
                                $self->{ref_fz}{$raddr}=$$ritem;
                            }
                            $frozen=1;
                            redo DEQUEUE;
                        }
                    }
                }
            }
    
            my ($idx,$dupe)=$self->_reg_ref($item,$cdepth,$cname,$rcnt,$arg);
            $DEBUG and print "  Skipping '$cname' as it is a dupe of ".
                             "$self->{refn}[$idx]\n"
                if $dupe;
            $DEBUG>9 and $self->diag;
            next if $dupe;
    
    
            my $reftype=reftype $item;
            my $cnt=refcount($item);
            my $overloaded=undef;
            if (defined $class and overload::Overloaded($item)) {
                bless $item, 'Does::Not::Exist';
                $overloaded=$class;
            }
            
    
            if ($reftype eq 'SCALAR' or $reftype eq 'REF' or $reftype eq 'GLOB') {
                my $name=$self->_build_name($cname,'$');
                my $cnt=sv_refcount($$item);
                if ($cnt>1) {
                    $self->_reg_scalar($$item,$cdepth+1,$cnt,readonly($$item),$name);
                }
                if (my $type=reftype_or_glob $$item) {
                    $self->_add_queue(\@queue,$type,$$item,$cdepth+2,$name,$cnt)
                }
    
            } elsif ($reftype eq 'ARRAY') {
                foreach my $idx (0..$#$item) {
                    my $name=$self->_build_name($cname,'[',$idx);
                    my $cnt=sv_refcount($item->[$idx]);
                    if ($cnt>1) {
                        print "refcount($name)==$cnt\n"
                            if $DEBUG>9;
                        $self->_reg_scalar($item->[$idx],$cdepth+1,$cnt,readonly($item->[$idx]),$name);
                    }
                    if (my $type=reftype_or_glob $item->[$idx]) {
                        $self->_add_queue(\@queue,$type,$item->[$idx],$cdepth+2,$name,$cnt)
                    }
                }
            } elsif ($reftype eq 'HASH') {
                my $ik=$self->{style}{indentkeys};
                my $keys=$self->_get_keys($item,$raddr,$class,0);
                my $key_len=0;
                my $key_sum=0;
                my $key_count=0;
                while (defined(my $key=defined $keys ? $keys->[$key_count] : each %$item)) {
                    if ($ik) {
                        my $qk=_quotekey($key);
                        $key_sum+=length($qk);
                        $key_len=length($qk) if $key_len<length($qk);
                    }
                    $key_count++;
                    my $name=$self->_build_name($cname,'{',$key);
                    my $cnt=sv_refcount($item->{$key});
                    if ($cnt>1) {
                        $self->_reg_scalar($item->{$key},$cdepth+1,$cnt,readonly($item->{$key}),$name);
                    }
                    if (my $type=reftype_or_glob $item->{$key}) {
                        $self->_add_queue(\@queue,$type,$item->{$key},$cdepth+2,$name,$cnt);
                    }
                }
                my $avg=$key_count>0 ? $key_sum/$key_count : 0;
                $self->{ref_hklen}{$raddr}=($key_len>8 and (2/3*$key_len)>$avg) ? int(0.5+$avg) : $key_len;
                $self->{ref_hkcnt}{$raddr}=$key_count;
                #warn "$raddr => $key_count";
    
            } elsif ($reftype eq 'CODE') {
                if ($pass == 1) {
                    my $used=get_lexicals($item);
                    foreach my $name (keys %$used) {
                        next unless $name=~/\D/;
                        my $addr=refaddr($used->{$name});
                        if ( !$lex_addr{$addr} ) {
                            $lex_addr{$addr}=$used->{$name};
                            if ( $lex_name{$name} ) {
                                my $tmpname=sprintf "%s".$self->{style}{eclipsename},
                                                substr($name,0,1),
                                                $self->{style}{eclipsename}=~/^[^%]*%s/ 
                                                   ? ( substr($name,1),
                                                       ++$lex_special{$name}, )
                                                   : ( ++$lex_special{$name},
                                                       substr($name,1), );
                                $lex_name{$tmpname}=$addr;
                                $lex_addr2name{$addr}=$tmpname;
                                $self->_add_queue(\@queue,reftype_or_glob $used->{$name},
                                    $used->{$name},$cdepth+1,$tmpname,2);
                            } else {
                                $lex_name{$name}=$addr;
                                $lex_addr2name{$addr}=$name;
                                $self->_add_queue(\@queue,reftype_or_glob $used->{$name},
                                    $used->{$name},$cdepth+1,$name,2);
                            }
                        }
                    }
                }
            } elsif ($reftype eq 'FORMAT') {
                # nothing
            } else {
                # IO?
                Carp::confess "Data() can't handle '$reftype' objects yet\n :-(\n";
            }
            bless $item,$overloaded if defined $overloaded;
    
        }
        if ( $pass++ == 1 ) {
            
            my %items;
            for my $idx ( 0..$#{$args_insideout{$self}} ) {
                my $item=$args_insideout{$self}[$idx];
                $items{ refaddr $item->{item} } = $idx;
            }
            
            my @add;
            my $added=0;
            if (0) {
                @add=keys %lex_addr;
            } else {
                for my $addr (keys %lex_addr) {
                    if ( exists $items{$addr} ) {
                        #warn sprintf "Gotcha: %0x | %s | %s",$addr,$self->{unames}[$idx]||'',$lex_addr2name{$addr};
                        my $idx = $items{$addr};
                        if ( !$self->{unames}[$idx] ){
                            for ($self->{unames}[$idx] = $lex_addr2name{$addr}) {
                                s/^[^\$]/*/; s/^\$//;
                            }
                            $added++;
                        } else {
                            my $new=$self->{unames}[$idx];
                            my $old=$lex_addr2name{$addr};
                            $new=~s/^(\*)?/substr($old,0,1)/e;
                            delete $lex_name{$lex_addr2name{$addr}};
                            $lex_addr2name{$addr}=$new;
                            $lex_name{$self->{unames}[$idx]} = $addr;  # xxx 
                        }
                    } else {
                        push @add,$addr;
                    }
                }
            }
            @add=sort {$lex_addr2name{$a} cmp $lex_addr2name{$b}} @add;

            $self->{lexicals}={
                               a2n => \%lex_addr2name,
                               name => \%lex_name
                              };
            
            if (@add) {
                unshift @{$args_insideout{$self}},
                        map {
                            my $rt=reftype($lex_addr{$_});
                            my $item;
                            if ($rt ne 'SCALAR' and $rt ne 'GLOB' and $rt ne 'REF') {
                                $item=\$lex_addr{$_};
                            } else {
                                $item=$lex_addr{$_};
                            }
                            {
                                    item   => $item,
                                    usemy  => 1,
                                    ro     => 0,
                                    refcnt => refcount($lex_addr{$_}),
                            }
                        } @add;
                $self->{lexicals}{added}={ map { $lex_addr2name{$_} => 1 } @add };
                unshift @{$self->{unames}},
                    map { 
                            (my $n=$lex_addr2name{$_})=~s/^[^\$]/*/; 
                            $n=~s/^\$//; 
                            $n 
                        } @add;
                $self->_reset;
                redo PASS;
            } elsif ($added) {
                $self->_reset;
                redo PASS;
            }
        }
    }    
    $self->{cataloged}=1;
    return $self;
}

sub _add_fix {
    my ($self,@args)=@_;
    # 'var','glob','method call','lock','ref','sv','#'
    # TODO
    # add a fix statement to the list of fixes.
    my $fix=@args==1 ? shift @args : [@args];
    unless ($fix->[0]=~/^(var|glob|method call|ref|sv|#|sub call|lock|bless)$/) {
        Carp::confess "Unknown variant:".Dumper($fix);
    }

    if ($args[0] eq 'var') {
        unshift @{$self->{fix}},$fix;
    }   else {
        push @{$self->{fix}},$fix;
    }
}

sub _glob_slots {
    my ($self,$inc_format)=@_;
    # $inc_format is for a special case.
    return (
            qw(SCALAR HASH ARRAY),
             (($self->{style}{deparse} && $self->{style}{deparseglob})
                ? 'CODE' : ()),
             (($inc_format && $self->{style}{deparse} && $self->{style}{deparseglob})
                ? 'FORMAT' : () )
           );
}

sub _dump_apply_fix { #handle fix statements and GLOB's here.
    my ($self,$isfinal)=@_;
    # go through the fix statements and out any that are
    # now fully dumped.
    # curently the following types are grokked:
    # 'var','glob','method call','tlock','ref','sv','#'

    my @globs;
    GLOB:{
        @globs=();
        @{$self->{fix}}=grep {
            my $keep=1;
            my $fix=$_;
            if (ref $fix) {
                my ($type,$lhs,$rhs,$class)=@$fix;

                if ($type eq '#') {
                    $self->{fh}->print(map "# $_\n",@$fix[0..$#$fix]);
                    $keep=0;
                } elsif ($type eq 'bless') {
                    if ($isfinal) { # $self->{"refdu"}[$lhs]
                        $lhs=$self->{"refn"}[$lhs];
                        $self->{fh}->print(
                            substr($self->{style}{bless},0,-1)," ",$lhs,", ",
                           _quote($rhs)," ",substr($self->{style}{bless},-1),
                           ";\n");
                        $keep=0;
                    }
                } elsif ($type eq 'sv') {

                    my $dref=$_->[-1];
                    if ($self->{$type."du"}[$rhs] and ${$self->{$type."du"}[$rhs]}) {
                        $rhs=$self->{$type."n"}[$rhs];
                        my ($sigil,$var)=remove_deref($lhs);
                        if ($sigil) {
                            $rhs="\\".$rhs;
                            $lhs=$var;
                        }
                        $self->{fh}->print("$lhs = $rhs;\n");
                        $$dref=1 if ref $dref;
                        $keep=0
                    }
                } elsif ($type eq 'ref') {

                    if ($self->{$type."du"}[$rhs] and ${$self->{$type."du"}[$rhs]}) {

                        $rhs=$self->{$type."n"}[$rhs];

                        if ($rhs=~/^[\@\%\&]/) {
                            $rhs="\\".$rhs;
                            $rhs="bless( $rhs, "._quote($class).' )'
                                if $class;                            
                        } # Warn if
                        $self->{fh}->print("$lhs = $rhs;\n");
                        $keep=0
                    }
                } elsif ($type eq 'lock') {
                    if ($self->{refdu}[$lhs] and ${$self->{"refdu"}[$lhs]}) {
                        $lhs=$self->{"refn"}[$lhs];
                        $self->{fh}->print(@$rhs ? "lock_keys_plus( $lhs, "
                                             : "lock_keys( $lhs ",
                                        join(", ",map{ _quote($_) } @$rhs),
                                        ");\n");
                        $keep=0;
                    }
                } elsif ($type eq 'method call') {
                    if ($self->{refdu}[$lhs] and ${$self->{"refdu"}[$lhs]}) {
                        $lhs=$self->{"refn"}[$lhs];
                        my @args=@$_[3..$#$_];
                        $self->{fh}->print("$lhs->$rhs(".join(", ",@args).");\n");
                        $keep=0
                    }
                } elsif ($type eq 'glob') {
                    push @globs,$_;
                    $keep=0;
                } elsif ($type eq 'var') {
                    $rhs="\\".$rhs;
                    $rhs="bless( $rhs, "._quote($class).' )'
                        if $class;
                    $self->{fh}->print(($self->{style}{declare} ? 'my ' : ""),"$lhs = $rhs;\n");
                    $keep=0;
                }  elsif ($type eq 'sub call') {
                    my @r=grep { ref $_ and (!$self->{svdu}[$$_] or !${$self->{svdu}[$$_]}) } @$fix;
                    unless (@r) {
                        my ($type,$sub,@args)=map { ref $_ ? $self->{svn}[$$_] : $_ } @$fix;
                        $self->{fh}->print("$sub(",join(", ",@args),");\n");
                        $keep=0;
                    }
                } else {
                    die Dumper($fix);
                }

            }
            $keep;
        } @{$self->{fix}};
        foreach my $glob (@globs) {
            my ($type,$lhs,$rhs,$depth,$name)=@$glob;
            print "Symbol: $name\n" if $DEBUG and $name;
            local @_;
            $name=$name ? '*'.$name : $rhs;
            my $overloaded=undef;
            if (defined( blessed $lhs ) and
                overload::Overloaded( $lhs ) ) 
            {
                $overloaded=blessed $lhs;
                bless $lhs,"Does::Not::Exist";
            }
            foreach my $t ($self->_glob_slots(''))
            {
                my $v=*$lhs{$t};
                
                if ( not(defined $v) or
                    ($t eq 'SCALAR' and !defined($$v))) 
                {
                    next;
                }
                            

                my $dumped=0;


                my $gaddr=refaddr(*$lhs{$t});
                my $gidx=$self->{ref}{$gaddr};
                unless ($gidx) {
                    next
                } elsif ($self->{refd}[$gidx]<$depth+1) {
                    $self->_add_fix('ref',$name,$gidx,blessed(*$lhs{$t}));
                    next;
                }

                $self->{fh}->print("$name = ");
                my $ret=$self->_dump_sv(*$lhs{$t},$depth,\$dumped,$name,length($name)+3);
                Carp::confess "\nUnhandled alias value '$ret' returned to _dump_apply_fix()!"
                    if $ret;
                $self->{fh}->print(";\n");
                $dumped=1;
            }
            
            if ($self->{style}{deparse} && $self->{style}{deparseglob}
                #and defined *$lhs{FORMAT}
            ) {
                # from link from [ysth]: http://groups.google.com/groups?selm=laUs8gzkgOlT092yn%40efn.org
                # translate arg (or reference to it) into a B::* object
                my $Bobj = B::svref_2object(\*$lhs);

                # if passed a glob or globref, get the format
                $Bobj = B::GV::FORM($Bobj) if ref $Bobj eq 'B::GV';

                if (ref $Bobj eq 'B::FM') {
                    (my $cleaned=$name)=~s/^\*(::)?//;
                    $self->{fh}->print("format $cleaned =\n");
                    my $deparser = Data::Dump::Streamer::Deparser->new();
                    $self->{fh}->print(
                        $deparser->indent($deparser->deparse_format($Bobj))
                    );
                    $self->{fh}->print("\n");
                }
            }
            bless $lhs,$overloaded 
                    if defined $overloaded;
            
        }
        redo GLOB if @globs;
    }
}

=item Out

=item Out VALUES

Prints out a set of values to the appropriate location. If provided a list
of values then the values are first scanned with C<Data()> and then printed,
if called with no values then whatever was scanned last with C<Data()> or
C<Dump()> is printed.

If the C<To()> attribute was provided then will dump to whatever object was
specified there (any object, including filehandles that accept the print()
method), and will always return $self.

If the C<To()> attribute was not provided then will use an internal printing
object, returning either a list or scalar or printing to STDOUT in void context.

This routine is virtually always called without arguments as the last method in
the method chain.

 Dump->Arguments(1)->Out(@vars);
 $obj->Data(@vars)->Out();
 Dump(@vars)->Out;
 Data::Dump::Streamer->Out(@vars);

All should DWIM.

=cut

#
# Out is just a wrapper. The overall dumping process works like this:
#
# Out
#   foreach root value
#     _dump_sv
#       _dump_rv if ref
#         (optionally one of)
#         _dump_array
#         _dump_hash
#         _dump_code
#         _dump_qr
#     _dump_apply_fix
#       (which may call)
#       _dump_sv
#
# _dump_array, _dump_hash, _dump_rv  if needed may also call _dump_sv
#
# essentially _dump_sv and _dump_rv handle uniqueness checks for scalars,
# and refs. _dump_sv handles the SV's containers and _dump_rv
# handles the things that the SV contains a reference to. _dump_sv also
# handles simple values and globs, and works with _dump_rv to handle
# references to scalars correctly. If "fix" statements are required
# to complete the definition of the structure (self referential structures)
# then _add_fix adds them to the list, and _dump_apply_fix pulls them off.
# note that _dump_apply_fix can also call _dump_sv if needed (to handle globs),
# and will also emit fix statements as early as possible. no require/use
# logic is currently in place. its the evalers responsibility to use the mod
# w/the right tags for now...

sub Out {
    my $self = shift->_safe_self;
    print "Out(".scalar(@_)." vars)\n"
        if $DEBUG;
    if ( @_ or !$self->{cataloged} ) {
        $self->Data(@_);
    }

    my $fh;
    unless ( $self->{fh} ) {
        print "  no filehandle using "
            if $DEBUG;
        if (defined wantarray) {
            my $class= __PACKAGE__ . (wantarray ? "::_::ListPrinter" : "::_::StringPrinter");
            print $class,"\n"
                if $DEBUG;
            $fh = $class->new()
              or Carp::confess "$class failed to build!";
            $self->{'return'} = $fh;
        } else {
            print "STDOUT\n" if $DEBUG;
            $fh = \*STDOUT;
        }
        $self->{fh} = $fh;
    }
    # loop over the list
    # and dump out each one in turn
    # handling any potential fixes after
    # each definition is complete
    $self->{out_names}=[];
    $self->{declare}=[];
    $self->{special}={};
    $DEBUG>9 and $self->diag;

    my @items=@{$args_insideout{$self}};

    my $namestr="";

    push @{$self->{out_names}},map{$_->{name}}@items; #must
    push @{$self->{declare}},map{$_->{name}}@items;

    if ($self->{style}{special}) {

        warn DDumper(\@items) if $DEBUG;

        $namestr="# (".join (", ",@{$self->{out_names}}).")\n";

        @items=sort { $self->{svc}[$b->{idx}] <=> $self->{svc}[$a->{idx}]||
                  ($b->{raddr} ? $self->{refc}[$b->{ridx}] : 0)
                   <=>
                  ($a->{raddr} ? $self->{refc}[$a->{ridx}] : 0)
            } @items;




        warn DDumper(\@items) if $DEBUG;
    }

    $self->{fh}->print("my (",join(",",sort keys %{$self->{lexicals}{added}}),");\n")
        if $self->{lexicals}{added};
        
    foreach my $item (@items) {
        my $dumped=0;
        my $ret=$self->_dump_sv(${$item->{item}},1,\$dumped,$item->{name});
        Carp::confess "\nUnhandled alias value '$ret' returned to Out()!"
            if $ret;
        $self->{fh}->print(";\n");
        $dumped=1;
        $self->_dump_apply_fix();
    }
    $self->_dump_apply_fix('final');
    $self->{fh}->print($namestr) if $namestr;

    $self->diag if $DEBUG;
    #warn "@{$self->{out_names}}";
    if ( $self->{return} and defined wantarray) {
        my $r = delete $self->{return};
        delete $self->{fh};
        return $r->value;
    } else {
        return $self;
    }

}


sub _dump_sv {
    my ($self,$item,$depth,$dumped,$name,$indent,$is_ref)=@_;


    $self->{do_nl}=0;

    my $addr=refaddr(\$_[1]);
    my $idx=$self->{sv}{$addr};
    my $ro;
    $DEBUG and printf "_dump_sv %d %s %#x - %d\n",$depth, $name,$addr,$idx||0;


    $name||=$self->{svn}[$idx];
    (my $clean_name=$name)=~s/^[\@\%\&](\w+)/\$${1}_/; # XXX

    if ($idx) {

        # Its a monitored scalar.

        my $pre_dumped=$self->{svdu}[$idx];
        my $name_diff=(
                           $self->{svd}[$idx]==$depth
                       and $self->{svn}[$idx] ne $clean_name
                       and $clean_name!~/\*/
                       and $name!~/^\&/
                      );

        #print "Idx: $idx Special keys:",join("-",keys %{$self->{special}}),"\n"
        #    if $DEBUG and keys %{$self->{special}};

        print "sv_dump Monitored:\n",$self->diag_sv_idx($idx,"  ") if $DEBUG;


        if (( $pre_dumped and !$self->{svon}{$idx}) or (!$self->{svon}{$idx} ? ($self->{svd}[$idx]<$depth or $name_diff) : undef) ) {

            print "PREDUMPED: $self->{svon}{$idx}\n"
                if $DEBUG and $self->{svon}{$idx} and $pre_dumped and $$pre_dumped;

            # We've seen it before.
            # Unless its a ref it must be an alias
            print(($name_diff ? "Name diff" : "No name diff"), " $name, $clean_name","\n")
                if $DEBUG;

            my ($str,$ret)=('',undef);

            if ($is_ref) {
                if ($self->{svd}[$idx]==1 && !$self->{style}{declare}
                    || ($pre_dumped && $$pre_dumped)
                ) {
                    $str="\\$self->{svn}[$idx]";
                } else {
                    #see the 'Many refs' tests in t\dump.t for
                    #why this is here. basically we need to
                    #ensure the ref is modifiable. If its two $'s
                    #then its modifable anyway, more and it wont be.
                    # $ref=\\$x; $ref=RW $$ref=RO $$$ref=$x=RW
                    unless ($self->{style}{purity}) {
                        $str="\\$self->{svn}[$idx]";
                    } else {
                        my $need_do=($name=~/^\$\$\$+/);
                        if ($need_do) {
                            $str.='do{my $f=';
                        }

                        $str.=!$self->{style}{verbose}
                               ? "'R'" : _quote($DEBUG ? 'SR: ' : 'R: ',
                                                "$self->{svn}[$idx]");
                        $ret=\do{my $nope=0};
                        $self->_add_fix('sv',$name,$idx,$ret);

                        $str.="}" if ($need_do)
                    }
                }
            } else {
                if ($depth==1) {
                    if ($self->{style}{declare}) {
                        $str.="my $name;\n";
                    }
                    #push @{$self->{out_names}},$name;
                    #push @{$self->{declare}},$name;
                    $str.="alias_ref(\\$name,\\$self->{svn}[$idx])";
                } elsif ($self->{style}{purity}) {
                    $str.=!$self->{style}{verbose} ? "'A'" : _quote("A: ",$self->{svn}[$idx]);
                    $ret=\$idx;
                } else {
                    $str.="alias_to($self->{svn}[$idx])";
                    $ret='';
                }
            }
            $self->{buf}+=length($str);
            $self->{buf}=length($1) if $str=~/\n([^\n]*)\s*\z/;
            $self->{fh}->print($str);
            return $ret ? $ret : ()
        } else {
            # we've never seen it before and we need to dump it.
            $self->{svdu}[$idx]||=$dumped;

            print "Defining Special:".$self->diag_sv_idx($idx)
                if $DEBUG and $self->{special}{$idx};

            $self->{svn}[$idx]=$name if $self->{special}{$idx};
            $self->{svd}[$idx]=$depth if $self->{special}{$idx};

        }
        $ro=$self->{svro}[$idx];
    } else {
        $ro=readonly $_[1] unless defined $ro;
    }
    print "sv_dump: Postindexed\n" if $DEBUG;
    if ($depth==1) {
        # root level object. declare it
        if ($name ne $clean_name and $name!~/^\*/ and $self->{svc}[$idx]>1) {

                print "Special $name\n" if $DEBUG;
                my $oidx=$self->{ref}{$self->{sva}[$idx]};
                if ($oidx) {
                    #theres a ref to us out there
                    my $name=$self->_build_name($self->{refn}[$oidx],'$');
                    $self->{svn}[$idx]=$name;
                    print "Oindex! $oidx $name\n" if $DEBUG;
                    #$self->{svd}[$idx]=$self->{refd}[$idx]+1;
                }

                #$self->{special}{$idx}++;
                $self->{svdu}[$idx]=undef;

                print $self->diag_sv_idx($idx,1) if $DEBUG;
        }
        #push @{$self->{out_names}},$name; #must
        #push @{$self->{declare}},$name;
        unless ($name=~/^\&/) { # XXX 
            my $str=(($self->{style}{declare} && $name!~/^\*/ 
                     && !$self->{lexicals}{added}{$name}
                     ) ? "my " : ""
                     )."$name = ";
            $self->{fh}->print($str);
            $indent=length($str);
            $self->{buf}=0;
        } else {
            $indent=0;
        }
        print "toplevel\n" if $DEBUG;
    }

    my $iaddr=refaddr $item;

    $self->{fh}->print("\\")
        if $is_ref;

    my $glob=globname $item;
    my $add_do=$self->{style}{purity} && !$ro && $is_ref
               && {''=>1,SCALAR=>1,REF=>0}->{reftype($_[1])}
               && !blessed($_[1]) && !$glob;


    if ($add_do) {
        #warn "\n!$ro && $is_ref && !blessed($_[1]) && !$glob";
        $self->{fh}->print('do { my $v = ');
        $self->{buf}+=13;
    }

    unless ($iaddr) {
        print "iaddr $glob\n" if $DEBUG;
        unless (defined $item) {
            $self->{fh}->print('undef');
            $self->{buf}+=5;
        } else {
            my $is_ro=($self->{style}{ro} && $ro && !$is_ref);
            if ($is_ro and !$self->{style}{purity}) {
                $self->{fh}->print("make_ro( ");
            }
            if ($glob) {
                if ($glob=~/^\*Symbol::GEN/) {
                    $self->_dump_symbol($_[1],$name,$glob,'deref',$depth);
                } else
                {
                    $self->{buf}+=length($glob);
                    $self->{fh}->print($glob);
                    if ($self->{style}{dumpglob} and
                        !$self->{sv_glob_du}{$glob}++) {
                        $self->_add_fix('glob',$_[1],$glob,$depth+1);
                    }
                }
            } else {
                my $quoted;
                if ($self->{style}{dualvars}) {
                    no warnings 'numeric';
                    if (_could_be_dualvar($item) && 0+$item ne $item && "$item" != $item ) {
                        $quoted="dualvar( ".join(", ",0+$item,_quote("$item"))." )";
                    } else {
                        $quoted=_quote($item);
                    }
                } else {
                    $quoted=_quote($item);
                }
                $self->{buf}+=length($quoted);
                $self->{buf}=length($1) if $quoted=~/\n([^\n]*)\s*\z/;
                $self->{fh}->print($quoted); #;
            }
            if ($is_ro && $self->{style}{purity}) {
                $self->_add_fix('sub call','make_ro',$name);
            } elsif ($is_ro) {
                $self->{fh}->print(' )');
            }
            #return
        }
        $self->{do_nl}=0;
    } else {
        $self->{do_nl}=1;
        $self->_dump_rv($item,$depth+1,$dumped,$name,$indent,$is_ref && !$add_do);
    }
    $self->{fh}->print(' }')
            if $add_do;
    $self->_add_fix('sub call','weaken',$name)
            if $self->{svw}{$addr};
    return
}

sub _brace {
    my ($self,$name,$type,$cond,$indent,$child)=@_;
    my $open=$type=~/[\{\[\(]/;

    my $brace=$name !~ /^[%@]/ ? $type : $type =~ /[\{\[\(]/ ? '(' : ')';
    $child=$child ? " " : "";
    if ($cond) {
        $_[-2]+=$open ? $self->{style}{indentcols} : -$self->{style}{indentcols};
        $self->{fh}->print($open ? "" : "\n".(" " x $_[-2]),
                           $brace,
                           $open ? "\n".(" " x $_[-2]) : "");
    } else {
        $self->{fh}->print($open ? "" : $child ,
                           $brace,
                           $open ? $child : "");
    }
    return
}

sub _dump_qr {
    my ($self,$pat,$mod)=@_;
    my %counts;
    $counts{$_}++ foreach split //,$pat;
    my ($quotes,$best)=('',length($pat)+1);
    foreach my $char (qw( / ! % & <> {} " ),'#') {
        my $bad=0;
        $bad+=$counts{$_}||0 for split //,$char;
        ($quotes,$best)=($char,$bad) if $bad<$best;
        last unless $best;
    }
    $pat=~s/(?!\\)([$quotes])/\\$1/g
        if $best;
    {
    use utf8;
    #$pat=~s/([^\x00-\x7f])/sprintf '\\x{%x}',ord $1/ge;
    $pat=~s/([^\040-\176])/sprintf "\\x{%x}", ord($1)/ge;
    }
    $self->{fh}->print('qr',substr($quotes,0,1),$pat,substr($quotes,-1),$mod);
    return
}




sub _get_keys {
    my ($self,$item,$addr,$class,$pass)=@_;

    #print Data::Dumper->Dump([$item,$addr,$class,$pass,@{$self->{style}}{qw(hashkeys sortkeys)}]) if $DEBUG;

    # object
    my $ak=$self->{style}{hashkeys}{objs}{$addr};
    # class
    my $ck=defined $class ? $self->{style}{hashkeys}{class}{$class}
                          : $self->{style}{hashkeys}{class}{''};

    # generic
    my $sk=$self->{style}{sortkeys};

    my $keys;
    CHECK_STYLE:
    for my $style ($ak,$ck,$sk) {
        {
        print "Style:".(defined($ak) ? "'$ak'" : 'undef')."\n"
            if $DEBUG;
        }
        $keys=undef;
        if ($style) {
            my $rtype=reftype($style);
            if ($rtype) {
                if ($rtype eq 'CODE') {
                    $keys =$style->($item,$addr,$class,$pass);
                    ($style = $keys),redo CHECK_STYLE
                       unless reftype($keys) eq 'ARRAY';
                } elsif ($rtype eq 'ARRAY') {
                    $keys=$style;
                } elsif ($rtype eq 'HASH') {
                    $keys=[map {
                                ($style->{$_})
                                 ? $_
                                 : (exists($item->{$_}))
                                    ? $_
                                    : ()
                              } keys %$style
                         ];
                } else {
                    Carp::confess "What kind of reference is an '$rtype' for HashKeys()/SortKeys()!?";
                }
                my %u;
                # remove any dupes.
                @$keys=grep defined($_)&&!$u{$_}++,@$keys
                    unless $rtype eq 'HASH';
            } elsif ($style=~/num/i) {
                $keys = [ sort {$a <=> $b} keys %$item ];
            } elsif ($style=~/alph|lex/i) {
                $keys = [ sort keys %$item ];
            } else {
                Carp::carp "Don't know '$style' SortKeys option, defaulting to 'smart'"
                    unless $style=~/smart/i;
                $keys=[
                            map { $_->[-1] }
                	    sort {
                	        ( !defined($a->[0]) cmp !defined($b->[0]) )
                	        ||
                	        (  defined($a->[0]) ? $a->[0] <=> $b->[0] || ($a->[1] cmp $b->[1])
                	                            : $a->[1] cmp $b->[1] )
                	        ||
                	        ( $a->[-1] cmp $b->[-1] )
                	    }
                	    map {
                	            (my $chars=lc($_))=~s/\A(-?(?:0|[1-9]\d{0,8})(?:\.\d{0,15})?)(?!\d)//;
                	            my $num=$1;
                        	    [ $num, $chars, $_, ]
                	    }  keys %$item
        	    ];
            }

        } else {
            $keys=$style;
        }
        last if defined $keys;
    }
    keys %$item unless $keys;
    return $keys ? $keys : undef;
}


sub _dump_hash {
    my ($self,$item,$depth,$dumped,$name,$indent,$addr,$class)=@_;

    #Carp::confess "$name" unless defined $self->{ref_hkcnt}{$addr};

    my $keys=$self->_get_keys($item,$addr,$class,1);

    warn "Keys: $keys : @$keys" if $keys and $DEBUG;

    my $full_indent=$self->{style}{indent}>1;
    my $ind=($self->{style}{indent}) &&
            (!defined($self->{ref_hkcnt}{$addr}) or $self->{ref_hkcnt}{$addr}>1);

    $self->_brace($name,'{',$ind,$indent,$self->{ref_hkcnt}{$addr}) ;

    my $indkey=($ind && $self->{style}{indentkeys}) ? $self->{ref_hklen}{$addr} : 0;

    my $cindent=$indent;
    my $sep=$self->{style}{hashsep};
    if ($indkey) {
        $cindent+=$indkey+length($sep);
    }
    $DEBUG==10 and print "Indent $ind $indkey $cindent\n";
    my ($kc,$ix)=(0,0);
    my $last_n=0;
    my $ind_str=" " x $indent;
    while (defined(my $k=defined $keys ? $keys->[$ix++] : each %$item)) {
       $last_n=0 if ref $item->{$k};
        if ( $kc ) {
            my $do_ind=$ind && !$last_n ;
            $self->{fh}->print(",", $do_ind ? "\n$ind_str" : " ");
            $self->{buf}++;
            $self->{buf}=0 if $do_ind;
        } else {
            #$self->{fh}->print("\n$ind_str") if !$last_n;
            $kc=1;
        }
        if ($indkey) {
            my $qk=_quotekey($k);
            my $str=$indkey>=length($qk) ? join "",$qk," " x ($indkey-length($qk)),$sep
                                         : join "",$qk,"\n$ind_str"," " x $indkey,$sep
            ;

            $self->{buf}+=length($str);
            $self->{fh}->print($str);
        } else {
            my $str=_quotekey($k).$sep;
            $self->{buf}+=length($str);
            $self->{fh}->print($str);
        }
        my $alias=$self->_dump_sv($item->{$k},$depth+1,$dumped,
                            $self->_build_name($name,'{',$k),
                            $cindent
        );
        if (!$full_indent and !$self->{do_nl} and $self->{buf}<60) {
            #warn "$self->{buf}\n";
            $last_n++;
        } else {
            #warn "$self->{buf}\n";
            $last_n=0;
        }
        if ($alias) {
            $self->_add_fix('sub call','alias_hv',
                            $self->_build_name($name,'%'),
                            _quote($k),
                            $alias
            );
        }
    }
    $self->_brace($name,'}',$ind,$indent,$self->{ref_hkcnt}{$addr});
    return
}

sub _dump_array {
    my ($self,$item,$depth,$dumped,$name,$indent)=@_;
    my $full_indent=$self->{style}{indent}>1;
    my $ind=$self->{style}{indent} && @$item>1;

    $self->_brace($name,'[',$ind,$indent,scalar @$item);
    my $last_n=0;
    my $ind_str=(" " x $indent);
    unless ($self->{style}{rle} ) {
        foreach my $k (0..$#$item) {
            my $do_ind=$ind && (!$last_n || ref $item->[$k]);
            $self->{fh}->print(",", $do_ind ? "\n$ind_str" : " ")
                if $k;
            $self->{buf}=0 if $do_ind and $k;

            my $alias=$self->_dump_sv($item->[$k],$depth+1,$dumped,
                                $self->_build_name($name,'[',$k),
                                $indent
            );

            if (!$full_indent and !$self->{do_nl} and $self->{buf}<60) {
                #warn "$last_n\n";
                $last_n++;
            } else {
                $last_n=0;
            }
            if ($alias) {
                $self->_add_fix('sub call','alias_av',
                                $self->_build_name($name,'@'),
                                $k,
                                $alias
                );
            }
        }
    } else {
        # this is evil and must be changed.
        # ... evil ... totally evil... blech
        for ( my $k = 0 ; $k <= $#$item ; ) {
            my $v     = $item->[$k];
            my $count = 1;
            if (!refaddr($item->[$k]) and !readonly($item->[$k])
                and (!$self->{sv}{refaddr(\$item->[$k])} or
                $self->{svt}[$self->{sv}{refaddr(\$item->[$k])}]==1)
            )
            {
                COUNT:while (
                        $k + $count <= $#$item

                    and !refaddr($item->[ $k + $count ])

                    and !readonly($item->[ $k + $count ])

                    and (!$self->{sv}{refaddr(\$item->[$k + $count])} or
                         $self->{svt}[$self->{sv}{refaddr(\$item->[$k + $count])}]==1)

                    and !$v == !$item->[ $k + $count ]
                )

                {
                    if (!defined( $item->[ $k + $count ] )) {
                        last COUNT if defined($v);
                    } else {
                        last COUNT if
                            $v ne overload::StrVal( $item->[ $k + $count ] )
                    }
                    $count++;
                }
            }

            my $do_ind=$ind && (!$last_n || ref $item->[$k]);
            $self->{fh}->print(",", $do_ind ? "\n$ind_str" : " ")
                if $k;
            $self->{buf}=0 if $do_ind and $k;
            if ($count>1){
                $self->{fh}->print("( ");
                $self->{buf}+=2;
            }
            my $alias=$self->_dump_sv($item->[$k],$depth+1,$dumped,
                                $self->_build_name($name,'[',$k),
                                $indent
            );
            if (!$full_indent and !$self->{do_nl} and $self->{buf}<60) {
                $last_n++;
            } else {
                $last_n=0;
            }
            if ($alias) {
                $self->_add_fix('sub call','alias_av',
                                $self->_build_name($name,'@'),
                                $k,
                                $alias
                );
            }
            if ($count>1) {
                my $str=" ) x $count";
                $self->{buf}+=length($str);
                $self->{fh}->print($str);
            }
            $k += $count;

        }
    }
    $self->_brace($name,']',$ind,$indent,scalar @$item);
    return
}

sub __vstr {
    my ($v,@v);
    unless (@_) {
        $v=$];
    } elsif (@_==1) {
        $v=shift;
    } else {
        @v=@_;
    }
    return join ".", @v ? (@v,(0) x 3)[0..2]
                        : map { $v * 1000**$_ % 1000 } 0..2
}

sub _dump_code {
    my ($self,$item,$name,$indent,$class)=@_;
    unless ($self->{style}{deparse}) {
        $self->{fh}->print($self->{style}{codestub});
    } else { #deparseopts
        my $cv=svref_2object($item);
        
        if (ref($cv->ROOT)=~/NULL/) {
            my $gv=$cv->GV;
            $self->{fh}->print("\\&",$gv->STASH->NAME,"::",$gv->SAFENAME);
            return;
        }  
            
        my $deparser=Data::Dump::Streamer::Deparser->new(@{$self->{style}{deparseopts}});
        
        my $used=get_lexicals($item);
        my %targ;
        foreach my $targ (keys %$used) {
            next if $targ=~/\D/;
            my $addr=refaddr($used->{$targ});
            $targ{$targ}=$self->{lexicals}{a2n}{$addr}
                if $self->{lexicals}{a2n}{$addr};
        }
        
        # we added this method, its not a normal method. see bottom of file.
        $deparser->dds_usenames(\%targ); 
        
        my $bless=undef;
        my $code;
        DEPARSE:{
            $bless=($class,bless($item,$bless))[0] if defined $bless;
            eval { $code=$deparser->coderef2text($item) };
            bless $item,$bless if defined $bless;
            if (!defined $bless and $@ and
                $@ =~ /^\QUsage: ->coderef2text(CODEREF)\E/)
            {
                $bless='CODE';
                redo DEPARSE;
            } elsif ($@) {
                warnings::warnif "Using CODE stub for $name as ".
                 "B::Deparse->coderef2text (v$B::Deparse::VERSION".
                 " on v@{[__vstr]}) failed. Message was:\n $@";
                $self->{fh}->print($self->{style}{codestub});
                return;
            }
        }
        
        #$self->{fh}->print("\n#",join " ",keys %$used,"\n");
        
        #$code=~s/^\s*(\([^)]+\)|)\s*/sub$1\n/;
        $code="sub".($code=~/^\s*\(/ ? "" : " ").$code;
        if ($self->{style}{indent}) {
            $code=~s/\n/"\n"." " x $indent/meg;
        }
        #warn $name;
        if ($name=~s/^\&//) {
            $code=~s/sub(\s)?/sub $name$1/;
        }
        $self->{fh}->print("$code");
    }
    return
}

sub _dump_format {
    # from link from [ysth]: http://groups.google.com/groups?selm=laUs8gzkgOlT092yn%40efn.org
    # translate arg (or reference to it) into a B::* object
    my ($self,$item,$name,$indent)=@_;


    if ($self->{style}{deparse}) {
        my $Bobj = B::svref_2object($item);
        # if passed a glob or globref, get the format
        $Bobj = B::GV::FORM($Bobj) if ref $Bobj eq 'B::GV';
        if (ref $Bobj eq 'B::FM') {
            my $format;
            eval {
              my $deparser = Data::Dump::Streamer::Deparser->new();
              $format=$deparser->indent($deparser->deparse_format($Bobj));
            };
            if ($@) {
                warnings::warnif "B::Deparse (v$B::Deparse::VERSION on v@{[__vstr]}) failed FORMAT ref deparse.\n";
                $format="B::Deparse (v$B::Deparse::VERSION on v@{[__vstr]}) failed FORMAT ref deparse.\n.\n";
            }
            my $ind=$self->{style}{indent} ? ' ' x $indent : '';
            $format="format F =\n$format";
            $format=~s/^/${ind}# /gm;

            my $end='_EOF_FORMAT_';
            $end=~s/T(\d*)_/sprintf "T%02d_",($1||0)+1/e
                    while $format=~/$end/;

            $self->{fh}->print("do{ local *F; my \$F=<<'$end'; \$F=~s/^\\s+# //mg; eval \$F; die \$F.\$@ if \$@; *F{FORMAT};\n$format\n$end\n$ind}");
            return
        }
    }

    $self->{fh}->print($self->{style}{formatstub});


}

sub _dump_symbol {
    my ($self,$item,$name,$glob,$deref,$depth)=@_;

    my $ret="Symbol::gensym";
    $ret="do{ require Symbol; $ret }"
        unless $self->{reqs}{Symbol}++;
    $ret="*{ $ret }"
        if $deref;
    $self->{fh}->print( $ret );
    if ($self->{style}{dumpglob} and !$self->{sv_glob_du}{$glob}++) {
        $self->_add_fix('glob',$_[1],$glob,$depth+1,$name);
    }
}

sub _dump_rv {
    my ($self,$item,$depth,$dumped,$name,$indent,$add_do)=@_;

    my ($addr,$idx,$type,$class,$frozen,$overloaded);
    GETITEM: {
        $addr=refaddr($item) or Carp::confess "$name : $item";
        $idx=$self->{ref}{$addr};
        $type=reftype($item);
        $class=blessed($item);
        $class=undef if $class and $class eq 'Regexp' and regex $item;

        $DEBUG and
        printf "_dump_rv %d %s %#x\n",$depth,$name,$addr;

        if ($self->{ref_fz}{$addr} and !$frozen) {
            $item=$self->{ref_fz}{$addr};
            $frozen=1;
            redo GETITEM;
        }
    }



    if (defined $class and $self->{style}{ignoreclass}{$class}) {
        my $str=_quote("Ignored Obj [".overload::StrVal($item)."]");
        $self->{buf}+=length($str);
        $self->{fh}->print($str);
        return
    }


    unless ($idx) {
        #Carp::confess "Unhandled address $addr $name\n";
        # this should only happen for localized globs.
        ($idx)=$self->_reg_ref($item,$depth,$name,refcount($item));
    }
    if ($idx) {
        my $pre_dumped=$self->{refdu}[$idx];
        my $str="";
        if ($pre_dumped and $$pre_dumped) {
            # its been dumped totally
            $DEBUG and print "  predumped $self->{refn}[$idx]\n";
            if ($self->{refn}[$idx]=~/^[\@\%\&]/) {
                if (SvREADONLY_ref($item)) {
                    my @hidden_keys=sort(hidden_keys(%$item));
                    $self->_add_fix('lock',$idx,\@hidden_keys);
                }
                $str=join "",($class ? 'bless( ' : ''),
                                   '\\'.$self->{refn}[$idx],
                                   ($class ? ', '._quote($class).' )' : '');
            } else {
                $str=$self->{refn}[$idx];
            }
            $self->{buf}+=length($str);
            $self->{fh}->print($str);
            return
        } elsif ($pre_dumped or $self->{refd}[$idx] < $depth) {
            $DEBUG and print "  inprocess or depth violation: $self->{refd}[$idx] < $depth\n";
            # we are in the process of dumping it
            # output a place holder and add a fix statement
            # XXX is this sigil test correct? why not $?
            if ($self->{refn}[$idx]=~/^[\@\%\&]/ and (!$self->{style}{declare})) {
                $str=join"",( $class ? 'bless( ' : '' ),
                               '\\'.$self->{refn}[$idx],
                               ( $class ? ', '._quote($class).' )' : '' );
            } else {
                if ($self->{style}{purity}) {
                    $str=join"",$add_do ? 'do { my $v = ' : '',
                        !$self->{style}{verbose} ? "'V'" : _quote("V: ",$self->{refn}[$idx]),
                        $add_do ? ' }' : '';

                    #Carp::cluck "$name $self->{refd}[$idx] < $depth" if $name=~/\*/;
                    $self->_add_fix('ref',$name,$idx,$class);
                } else {
                    $str=$self->{refn}[$idx];
                }
            }
            $self->{buf}+=length($str);
            $self->{fh}->print($str);
            return
        }
        $self->{refdu}[$idx]||=$dumped;
        #$name=$self->{refn}[$idx]; # override inherited names. ??? maybe not needed
    } else {
        Carp::confess "Unhandled object '$item'\n";
    }
    if (defined $class and overload::Overloaded($item)) {
        bless $item, 'Does::Not::Exist';
        $overloaded=$class;
    }
    $self->{do_nl}=1;
    my $add_lock=($type eq 'HASH') && SvREADONLY_ref($item);
    my $fix_lock=0;
    my @hidden_keys=$add_lock ? sort(hidden_keys(%$item)) : ();
    if ($add_lock) {
        #warn "$name\n";
        if ($name!~/^\$/) {
            $fix_lock=1;
            $add_lock=0;
        } else  {
            $self->{fh}->print("lock_ref_keys",
                           @hidden_keys ? '_plus' : '',
                           '( '
                          );
        }
    }


    my $add_bless=defined($class) && ($name!~/^[\@\%\&]/);
    if ($add_bless && !$overloaded) {
        $self->{fh}->print(substr($self->{style}{bless},0,-1)," ");
    }

    $DEBUG and print "  $type : Start typecheck\n";
    if ($type eq 'SCALAR' or $type eq 'REF' or $type eq 'GLOB') {
        my ($pat,$mod)=$type eq 'SCALAR' ? regex($item) : ();
        my $glob=$type eq 'GLOB' ? globname $$item : '';
        if ($glob=~/^\*Symbol::GEN/) {
            $self->_dump_symbol($_[1],$name,$glob,0,$depth);
        } elsif (defined $pat) {
            # its a regex
            $self->_dump_qr($pat,$mod);
        } else {
            my $ret=$self->_dump_sv($$item,$depth+1,$dumped,
                                $self->_build_name($name,'$'),
                                $indent,'is_ref'
            );
            $self->{refdu}[$idx]=$ret if $ret;
        }
    } elsif ($type eq 'ARRAY') {
        $self->_dump_array($item,$depth,$dumped,$name,$indent);
    } elsif ($type eq 'HASH') {
        $self->_dump_hash($item,$depth,$dumped,$name,$indent,$addr,$class);
    } elsif ($type eq 'CODE') {
        $self->_dump_code($item,$name,$indent,$class);
    } elsif ($type eq 'FORMAT') {
        #$self->_dump_code($item,$name,$indent,$class); #muwhahahah
        $self->_dump_format($item,$name,$indent);
    } else {
         Carp::confess "_dump_rv() can't handle '$type' objects yet\n :-(\n";
    }
    if ($add_bless) {
        unless ( defined $overloaded ) {
            $self->{fh}->print(", ",_quote($class)," ",substr($self->{style}{bless},-1))
        } else {
            $self->_add_fix('bless',$idx,$overloaded);
        }
        bless $item, $overloaded
            if defined $overloaded;
        if (my $meth=$self->{style}{thawclass}{$class}||$self->{style}{thaw}){
            my $now=$meth=~s/^->//;
            if ($item->can($meth)) {
                if ($now and !$overloaded) {
                    # XXX what should happen here when its overloaded?
                    # is this right???? Somehow i doubt it. 
                    $self->{fh}->print("->$meth()");
                } else {
                    $self->_add_fix('method call',$idx,$meth); #thaw
                }
                $item->$meth()
                    if ($self->{style}{freezeclass}{$class}||$self->{style}{freeze})
                       and !$frozen; # $frozen is only true f a replacement was used
            }
        }
    } elsif ($fix_lock && !defined($class)) {
        $self->_add_fix('lock',$idx,\@hidden_keys);
    }
    if ($add_lock) {
        if (@hidden_keys) {
            $self->{fh}->print(", ",join(", ",map {_quote($_)} @hidden_keys));
        }
        $self->{fh}->print(" )");
    }
    $self->{do_nl}=1;
   
    
    
    return
}

=item Names

=item Names LIST

=item Names ARRAYREF

Takes a list of strings or a reference to an array of strings to use for
var names for the objects dumped. The names may be prefixed by a * indicating
the variable is to be dumped as its dereferenced type if it is an array, hash
or code ref. Otherwise the star is ignored. Other sigils may be prefixed but
they will be silently converted to *'s.

If no names are provided then names are generated automatically based on the type
of object being dumped, with abreviations applied to compound class names.

If called with arguments then returns the object itself, otherwise in list context
returns the list of names in use, or in scalar context a reference or undef. In void
context with no arguments the names are cleared.

B<NOTE:>
Must be called before C<Data()> is called.

=cut

sub Names {
    my $self = shift->_safe_self;
    if (@_) {
        my $v=(@_==1 and reftype $_[0] eq 'ARRAY') ? shift @_ : \@_;
        $self->{unames} = [ map { ( my $s = $_ ) =~ s/^[\@\%\&\$]/*/;
                                    Carp::confess "Bad name '$_'" if $s!~/^\*?\w+$/;
                                    $s
                                } @$v ];
        return $self;
    } elsif (! defined wantarray ) {
        $self->{unames}=[];
    }
    return wantarray ? @{$self->{unames}||[]} : $self->{unames}
}

=for UEDIT
sub Purity {}

=item Purity

=item Purity BOOL

This option can be used to set the level of purity in the output. It defaults
to TRUE, which results in the module doing its best to ensure that the resulting
dump when eval()ed is precisely the same as the input. However, at times such as
debugging this can be tedius, resulting in extremely long dumps with many "fix"
statements involved.  By setting Purity to FALSE the resulting output won't
necessarily be legal Perl, but it will be more legible. In this mode the
output is boardly similar to that of the default setting of Data::Dumper (Purity(0)).
When set to TRUE the behaviour is likewise similar to Data::Dumper in Purity(1)
but more accurate.

When Purity() is set to FALSE aliases will be output with a function call wrapper
of 'alias_to' whose argument will be the value the item is an alias to. This wrapper
does nothing, and is only there as a visual cue. Likewise, 'make_ro' will be output
when the value was readonly, and again the effect is cosmetic only.

=item To

=item To STREAMER

Specifies the object to print to. Data::Dump::Streamer can stream its output to any
object supporting the print method. This is primarily meant for
streaming to a filehandle, however any object that supports the method
will do.

If a filehandle is specified then it is used until it is explicitly
changed, or the object is destroyed.

=cut

sub To {
    my $self = shift->_safe_self;
    if (@_) {
        $self->{fh} = shift;
        return $self;
    }
    return $self->{fh};
}

=for UEDIT
sub Declare     {}

=item Declare

=item Declare BOOL

If Declare is True then each object is dumped with 'my' declarations included,
and all rules that follow are obeyed. (Ie, not referencing an undeclared variable).
If Declare is False then all objects are expected to be previously defined and
references to top level objects can be made at any time.

Defaults to False.

=for UEDIT
sub Indent      {}

=item Indent

=item Indent INT

If Indent is True then data is output in an indented and fairly neat fashion.
If the value is 2 then hash key/value pairs and array values each on their own line.
If the value is 1 then a "smart" indenting mode is activated where multiple key/value
or values may be printed to the same line. The heuristics for this mode are still
experimental so it may occassional not indent very nicely.

Default is Indent(2)

If indent is False then no indentation is done.

Defaults to True.

Newlines are appended to each statement regardless of this value.

=for UEDIT
sub IndentKeys      {}

=item Indentkeys

=item Indentkeys BOOL

If Indent() and Indentkeys are True then hashes with more than one key value
pair are dumped such that the keys and values line up. Note however this means
each key has to be quoted twice. Not advised for very large data structures.
Additional logic may enhance this feature soon.

Defaults to True.

B<NOTE:>
Must be set before C<Data()> is called.

=for UEDIT
sub SortKeys      {}
sub Sortkeys      {}

=item SortKeys

=item SortKeys TYPE_OR_CODE

=item Sortkeys

=item Sortkeys TYPE_OR_CODE

If False then hashes are iterated using each(), and are output in whatever
order your particular instance of perl provides, which varies across OS,
architecture and version. This requires considerably less memory, and time.

If True then hashes are sorted before dumping. If the value matches
C</alph|lex/i> then a lexicographical sort order is imposed. If the
value matches C</num/i> then a numeric sort order is imposed, and if the
value matches C</smart/i> then a sort order akin to a dictionary sort is
imposed. This order is the default and probably will do the right thing
for most key sets.

A user may also provide a CODE ref to be used for sorting and
prefiltering the hash keys.  The hash to be sorted will be passed by
reference to the sub, and the sub is expected to return a reference to
an array of keys to dump, a string like above, or false for perls ordering.
Note that this subroutine will be called twice per hash per dump, with the
number of the pass (0 or 1) as the second parameter. The behaviour of returning
different values on each pass is not well defined, but it is likely that returning
less keys (but the same ordering) on the second pass will be viable. Returning
more keys or a different ordering probably wont be.

See L<"Controlling Hash Traversal and Display Order"> for more details.

B<Note> that C<Sortkeys()> is a synonym for C<SortKeys()> for compatibility with
expectations formed by Data::Dumper. Data::Dumper provides the former, but the latter
is consistant with the method naming scheme in this module. So in the spirit of TIMTOWTDI
you can use either. :-)

=for UEDIT
sub Hashkeys      {}

=item HashKeys

=item HashKeys LIST

=item Hashkeys

=item Hashkeys LIST

In addition to L<SortKeys> it is possible to further fine tune the traversal
and ordering of hashes by using HashKeys().  Using this method you may specify
either a specific ordering as in L<SortKeys>, or a coderef similar to that
used in L<SortKeys> based on the hashrefs specific identity or its class. The only
difference between the returns of the coderefs between the two methods is that if
a HashKeys() rule returns undef then a fallback occurs to the SortKeys() rule.
However if defined but false is returned then Perls internal ordering is used.

If provided a list it expects either $hash_refernce=>VALUE pairs or
'CLASS::NAME'=>VALUE pairs, and return $self. If called with no parameters in
list or scalar context returns the options currently set, and if called with no
parameters in void context clears all HashKeys() settings.

See L<"Controlling Hash Traversal and Display Order"> and L<"SortKeys"> for more
details.

B<Note> that C<Hashkeys()> is a synonym for C<HashKeys()> for compatibility with
expectations formed by Data::Dumper with regard to the method Sortkeys(). See L<SortKeys>
for details of this method and the reason behind the synonym.

=for UEDIT
sub Verbose      {}

=item Verbose

=item Verbose BOOL

If Verbose is True then when references that cannot be resolved in a single
statement are encountered the reference is substituted for a descriptive tag
saying what type of forward reference it is, and to what is being referenced.
The type is provided through a prefix, "R:" for reference, and "A:" for alias,
"V:" for a value and then the name of the var in a string. Automatically
generated var names are also reduced to the shortest possible unique abbreviation,
with some tricks thrown in for Long::Class::Names::Like::This (which would
abbreviate most likely to LCNLT1)

If Verbose if False then a simple placeholder saying 'A' or 'R' is provided.
(In most situations perl requires a placeholder, and as such one is always
provided, even if technically it could be omitted.)

This setting does not change the followup statements that fix up the structure,
and does not result in a loss of accuracy, it just makes it a little harder to
read. OTOH, it means dumps can be quite a bit smaller and less noisy.

Defaults to True.

B<NOTE:>
Must be set before C<Data()> is called.

=for UEDIT
sub DumpGlob {}

=item DumpGlob

=item DumpGlob BOOL

If True then globs will be followed and fully defined, otherwise the globs will
still be referenced but their current value will not be set.

Defaults to True

B<NOTE:>
Must be set before C<Data()> is called.

=for UEDIT
sub Deparse {}

=item Deparse

=item Deparse BOOL

If True then CODE refs will be deparsed use L<B::Deparse|B::Deparse> and included
in the dump. If it is False the a stub subroutine reference will be output as per
the setting of C<CodeStub()>.

Caveat Emptor, dumping subroutine references is hardly a secure act, and it is
provided here only for convenience.

Note using this routine is at your own risk as of DDS 1.11, how it interacts with
the newer advanced closure dumping process is undefined.

=for UEDIT
sub EclipseName {}

=item EclipseName

=item EclipseName SPRINTF_FORMAT

When necessary DDS will rename vars output during deparsing with this value.
It is a sprintf format string that should contain only and both of "%s" and a "%d" 
in any order along with any other part of the string. No checks are performed on 
the validity of this value. It defaults to

  "%s_eclipse_%d"
  
where the %s represents the name of the var being eclipsed, and the %d a counter
to ensure all such mappings are unique.  

=for UEDIT
sub DeparseOpts {}

=item DeparseOpts

=item DeparseOpts LIST

=item DeparseOpts ARRAY

If Deparse is True then these options will be passed to B::Deparse->new()
when dumping a CODE ref. If passed a list of scalars the list is used as
the arguments. If passed an array reference then this array is assumed to
contain a list of arguments. If no arguments are provided returns a
an array ref of arguments in scalar context, and a list of arguments in
list context.

Note using this routine is at your own risk as of DDS 1.11, how it interacts with
the newer advanced closure dumping process is undefined.

=for UEDIT
sub CodeStub {}

=item CodeStub

=item CodeStub STRING

If Deparse is False then this string will be used in place of CODE
references. Its the users responsibility to make sure its compilable
and blessable.

Defaults to 'sub { Carp::confess "Dumped code stub!" }'

=for UEDIT
sub FormatStub {}

=item FormatStub

=item FormatStub STRING

If Deparse is False then this string will be used in place of FORMAT
references. Its the users responsibility to make sure its compilable
and blessable.

Defaults to 'do{ local *F; eval "format F =\nFormat Stub\n.\n"; *F{FORMAT} }'

=for UEDIT
sub DeparseGlob {}

=item DeparseGlob

=item DeparseGlob BOOL

If Deparse is TRUE then this style attribute will determine if subroutines
and FORMAT's contained in globs that are dumped will be deparsed or not.

Defaults to True.

=for UEDIT
sub DualVars {}
sub Dualvars {}

=item Dualvars

=item Dualvars BOOL

=item Dualvars

=item Dualvars BOOL

If TRUE then dualvar checking will occur and the required statements emitted
to recreate dualvars when they are encountered, otherwise items will be dumped
in their stringified form always. It defaults to TRUE.

=for UEDIT
sub Rle {}
sub RLE {}

=item Rle

=item Rle BOOL

=item RLE

=item RLE BOOL

If True then arrays will be run length encoded using the C<x> operator.
What this means is that if an array contains repeated elements then instead
of outputting each and every one a list multiplier will be output. This means
that considerably less space is taken to dump redundant data.

=for UEDIT
sub Freeze {}

=item Freeze

=item Freeze METHOD

If set to a string then this method will be called on ALL objects before
they are dumped. This method may either, change the internal contents of
the reference to something suitable for dumping, or may alter $_[0] and
have that used _instead_ of the real object reference.

B<NOTE:>
Must be set before C<Data()> is called.

=for UEDIT
sub Thaw {}

=item Thaw

=item Thaw METHOD

If set to a string then this method will be called on ALL objects after they
are dumped.

B<NOTE:>
Must be set before C<Data()> is called.

=for UEDIT
sub FreezeClass {}

=item FreezeClass

=item FreezeClass CLASS

=item FreezeClass CLASS, METHOD

=item FreezeClass LIST

Defines methods to be used to freeze specific classes. These settings override
Freeze.  If one argument is provided then it returns the method for that class.
If two arguments are provided then it sets the dump method for the given class.
If more than two arguments are provided then it is assumed it is a list of
CLASS, METHOD pairs and sets the entire list, discarding any existing settings.
Called with no arguments in void setting clears the overall set of CLASS/METHOD
pairs. Called with no arguments in list context returns all CLASS/METHOD pairs.
Called with no arguments in scalar content returns a reference to the hash.

B<NOTE:>
Must be set before C<Data()> is called.

=for UEDIT
sub ThawClass   {}


=item ThawClass

=item ThawClass CLASS

=item ThawClass CLASS, METHOD

=item ThawClass LIST

Similar to FreezeClass, but called when evaling the data structure back into
existance. Has the same calling semantics as FreezeClass.

B<NOTE:>
Must be set before C<Data()> is called.



=item FreezeThaw CLASS, FREEZE_METHOD, THAW_METHOD

=item FreezeThaw LIST

FreezeThaw merges the features of FreezeClass and ThawClass into a single method.
It takes a list of triplets and then calls those method as necessary. Purely a
bit of syntactitc sugar because I realized the original interface was a bit clunky
to use.

FreezeThaw does not currently support 'get' semantics and cannot be used to clear
both options. This will probably come in a later release.

B<NOTE:>
Must be set before C<Data()> is called.

=for UEDIT
sub IgnoreClass {}

=item IgnoreClass

=item IgnoreClass CLASS

=item IgnoreClass CLASS, METHOD

=item IgnoreClass LIST

Similar to FreezeClass, but instead of changing how the object is dumped, causes
the object to be outright ignored if is an instance of barred class. The position
in the data structure will be filled with a string containing the name of the class
ignored. Has the same calling semantics as FreezeClass.

B<NOTE:>
Must be set before C<Data()> is called.

=cut

# weird styling here deliberate.
sub 
DeparseOpts 
{
    my $self=shift;
    if (@_) {
        if (ref $_[0]) {
            $self->{style}{deparseopts}=shift;
        } else {
            $self->{style}{deparseopts}=[@_];
        }
        return $self;
    } else {
        return wantarray ? @{$self->{style}{deparseopts}}
                         : $self->{style}{deparseopts};
    }
}

sub FreezeThaw {
    my $self=shift;
    if (@_) {
        Carp::confess("Argument to FreezeThaw must be in (Class Freeze Thaw) triplets")
            if @_ % 3;
        while (@_) {
            my $class =shift @_;
            my $freeze=shift @_;
            my $thaw  =shift @_;
            $self->FreezeClass($class,$freeze);
            $self->ThawClass($class,$thaw);
        }
        return $self
    } else {
        Carp::confess("FreezeThaw with no arguments is undefined.")
    }
}

sub HashKeys {
    my $self=shift;
    if (@_) {
        while (@_) {
            my $obj=shift;
            if (ref $obj) {
                $self->{style}{hashkeys}{refs}{refaddr($obj)}=$obj;
                $self->{style}{hashkeys}{objs}{refaddr($obj)}=shift;
            } else {
                $self->{style}{hashkeys}{class}{$obj}=shift;
            }
        }
        print Data::Dumper->Dump([$self->{style}{hashkeys}]) if $DEBUG;
        return $self;
    } elsif (defined wantarray) {
        return (%{$self->{style}{hashkeys}{class}},
                map { $self->{style}{hashkeys}{objs}{$_} =>
                      $self->{style}{hashkeys}{refs}{$_} }
                keys %{$self->{style}{hashkeys}{refs}} )
    } else {
        delete $self->{style}{hashkeys};
    }
}
*Hashkeys=*HashKeys;

my %scalar_meth=map{ $_ => lc($_)}
      qw(Declare Indent IndentCols SortKeys Sortkeys IndentKeys
        Verbose DumpGlob Deparse DeparseGlob DeparseFormat CodeStub
        FormatStub Rle RLE Purity DualVars Dualvars Freeze Thaw
        EclipseName);
my %hash_meth=map {$_ => lc($_)} qw(FreezeClass ThawClass IgnoreClass);

sub AUTOLOAD {
    (my $meth=$AUTOLOAD)=~s/^((?:\w+::)+)//;
    my $name;
    if (defined($name=$scalar_meth{$meth})) {
        $DEBUG and print "AUTLOADING scalar meth $meth ($name)\n";
        eval '
        sub '.$meth.' {
            my $self=shift->_safe_self();
            if (@_) {
                $self->{style}{'.$name.'}=shift;
                return $self
            } else {
                return $self->{style}{'.$name.'}
            }
        }
        ';
        $@ and die "$meth:$@\n";
        goto &$meth;
    } elsif (defined($name=$hash_meth{$meth})) {
        $DEBUG and print "AUTLOADING hash meth $meth ($name)\n";
        eval '
        sub '.$meth.' {
            my $self=shift->_safe_self();
            if (@_==1) {
                my $class=shift;
                return $self->{style}{'.$name.'}{$class};
            } elsif (@_==2) {
                my $class=shift;
                $self->{style}{'.$name.'}{$class}=shift;
                return $self
            } elsif (@_ >2 ) {
                $self->{style}{'.$name.'}={@_};
                return $self;
            } elsif (defined wantarray) {
                return wantarray ? %{$self->{style}{'.$name.'}||{}}
                                 : $self->{style}{'.$name.'};
            } else {
                $self->{style}{'.$name.'}={};
            }
        }
        ';
        $@ and die "$meth:$@\n";
        goto &$meth;
    } elsif ($meth=~/[^A-Z]/) {
        Carp::confess "Unhandled method/subroutine call $AUTOLOAD";
    }
}

sub get_lexicals {
    my $cv=shift;
    
    my $svo=svref_2object($cv);
    my @pl_array = $svo->PADLIST->ARRAY;
    
    my @name_obj = $pl_array[0]->ARRAY;
    
    my %named;
    for my $i ( 0..$#name_obj ) {
        if ( ref($name_obj[$i])!~/SPECIAL/) {
            $named{$i} = "${ $name_obj[$i]->object_2svref }";
        }
    }
    
    my %inited;
    my %used;
    walkoptree_filtered( 
            $svo->ROOT,
            sub { opgrep { name => [ qw[ padsv padav padhv ] ] }, @_ }, 
            sub { 
                my ( $op, @items )=@_;
                my $targ = $op->targ;
                my $name = $named{$targ} 
                    or return;
                
                $inited{$name}++
                    if $op->private & 128;

                if ( !$inited{$name} ) {
                    $used{$name} = $pl_array[1]->ARRAYelt($targ)->object_2svref;
                    $used{$targ} = $used{$name};
                    $inited{$name}++;
                }
            }
    );
    return \%used;
}    





package Data::Dump::Streamer::Deparser;
use B::Deparse;
our @ISA=qw(B::Deparse);
my %cache;

sub dds_usenames {
    my $self=shift;
    my $names=shift;
    $cache{$self}=$names;
}

sub padname {
    my $self = shift;
    my $targ = shift;
    if ( $cache{$self} and $cache{$self}{$targ} ) {
        return $cache{$self}{$targ}
    } 
    return $self->padname_sv($targ)->PVX;
}

sub DESTROY {
    my $self=shift;
    delete $cache{$self};
}

unless (B::AV->can('ARRAYelt')) {
    eval <<'    EOF_EVAL';
        sub B::AV::ARRAYelt {
            my ($obj,$idx)=@_;
            my @array=$obj->ARRAY;
            return $array[$idx];
        }
    EOF_EVAL
}

1;
__END__

=back

=head2 Reading the Output

As mentioned in L<Verbose> there is a notation used to make understanding the output easier.
However at first glance it can probably be a bit confusing. Take the following example:

    my $x=1;
    my $y=[];
    my $array=sub{\@_ }->( $x,$x,$y );
    push @$array,$y,1;
    unshift @$array,\$array->[-1];
    Dump($array);

Which prints (without the comments of course):

    $ARRAY1 = [
                'R: $ARRAY1->[5]',        # resolved by fix 1
                1,
                'A: $ARRAY1->[1]',        # resolved by fix 2
                [],
                'V: $ARRAY1->[3]',        # resolved by fix 3
                1
              ];
    $ARRAY1->[0] = \$ARRAY1->[5];         # fix 1
    alias_av(@$ARRAY1, 2, $ARRAY1->[1]);  # fix 2
    $ARRAY1->[4] = $ARRAY1->[3];          # fix 3

The first entry, C<< 'R: $ARRAY1->[5]' >> indicates that this slot in the array holds a reference
to the currently undefined C<< $ARRAY1->[5] >>, and as such the value will have to be provided
later in what the author calls 'fix' statements. The third entry C<< 'A: $ARRAY1->[1]' >> indicates
that is element of the array is in fact the exact same scalar as exists in C<< $ARRAY1->[1] >>, or is
in other words, an alias to that variable. Again, this cannot be expressed in a single statment
and so generates another, different, fix statement. The fifth entry C<< 'V: $ARRAY1->[3]' >> indicates
that this slots holds a value (actually a reference value) that is identical to one elsewhere,
but is currently undefined.  In this case it is because the value it needs is the reference
returned by the anonymous array constructer in the fourth element (C<< $ARRAY1->[3] >>). Again this
results in yet another different fix statement.  If Verbose() is off then only a 'R' 'A' or 'V'
tag is emitted as a marker of some form is necessary.

All of this specialized behaviour can be bypassed by setting Purity() to FALSE, in which case the
output will look very similar to what Data::Dumper outputs in low Purity setting.

In a later version I'll try to expand this section with more examples.

=head2 A Note About Speed

Data::Dumper is much faster than this module for many things. However IMO it is
less readable, and definately less accurate. YMMV.

=head1 EXPORT

By default exports the Dump() command. Or may export on request the same command
as Stream(). A Data::Dumper::Dumper compatibility routine is provided via
requesting Dumper and access to the real Data::Dumper::Dumper routine is provided
via DDumper. The later two are exported together with the :Dumper tag.

Additionally there are a set of internally used routines that
are exposed. These are mostly direct copies of routines from Array::RefElem,
Lexical::Alias and Scalar::Util, however some where marked have had their
semantics slightly changed, returning defined but false instead of undef
for negative checks, or throwing errors on failure.

The following XS subs (and tagnames for various groupings) are exportable
on request.

  :Dumper
        Dumper
        DDumper

  :undump          # Collection of routines needed to undump something
        alias_av              # aliases a given array value to a scalar
        alias_hv              # aliases a given hashes value to a scalar
        alias_ref             # aliases a scalar to another scalar
        make_ro               # makes a scalar read only
        lock_keys             # pass through to Hash::Util::lock_keys
        lock_keys_plus        # like lock_keys, but adds keys to those present
        lock_ref_keys         # like lock_keys but operates on a hashref
        lock_ref_keys_plus    # like lock_keys_plus but operates on a hashref
        dualvar               # make a variable with different string/numeric
                              # representation
        alias_to              # pretend to return an alias, used in low
                              # purity mode to indicate a value is actually
                              # an alias to something else.

  :alias           # all croak on failure
     alias_av(@Array,$index,$var);
     alias_hv(%hash,$key,$var);
     alias_ref(\$var1,\$var2);
     push_alias(@array,$var);

  :util
     blessed($var)           #undef or a class name.
     isweak($var)            #returns true if $var contains a weakref
     reftype($var)           #the underlying type or false but defined.
     refaddr($var)           #a references address
     refcount($var)          #the number of times a reference is referenced
     sv_refcount($var)       #the number of times a scalar is referenced.
     weak_refcount($var)     #the number of weakrefs to an object. 
                             #sv_refcount($var)-weak_refcount($var) is the true
                             #SvREFCOUNT() of the var.
     looks_like_number($var) #if perl will think this is a number.

     regex($var)     # In list context returns the pattern and the modifiers,
                     # in scalar context returns the pattern in (?msix:) form.
                     # If not a regex returns false.
     readonly($var)  # returns whether the $var is readonly
     weaken($var)    # cause the reference contained in var to become weak.
     make_ro($var)   # causes $var to become readonly, returns the value of $var.
     reftype_or_glob # returns the reftype of a reference, or if its not
                     # a reference but a glob then the globs name
     refaddr_or_glob # similar to reftype_or_glob but returns an address
                     # in the case of a reference.
     globname        # returns an evalable string to represent a glob, or
                     # the empty string if not a glob.
  :all               # (Dump() and Stream() and Dumper() and DDumper()
                     #  and all of the XS)
  :bin               # (not Dump() but all of the rest of the XS)

By default exports only the Dump() subroutine. Tags are provided for exporting
'all' subroutines, as well as 'bin' (not Dump()), 'util' (only introspection
utilities) and 'alias' for the aliasing utilities. If you need to ensure that
you can eval the results (undump) then use the 'undump' tag.

=head1 BUGS

Code with this many debug statements is certain to have errors. :-)

Please report them with as much of the error output as possible.

Be aware that to a certain extent this module is subject to whimsies of
your local perl. The same code may not produce the same dump on two different
installs and versions. Luckily these dont seem to pop up often.

=head1 AUTHOR AND COPYRIGHT

Yves Orton, E<lt>demerphq at hotmail dot comE<gt>

Copyright (C) 2003 Yves Orton

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

Contains code derived from works by Gisle Aas, Graham Barr,
Jeff Pinyan, Richard Clamp, and Gurusamy Sarathy.

Thanks to Dan Brook (broquaint) for testing and moral support. Without his
encouragement the 1.0 release would never have been written.

Thanks to Yitzchak Scott-Thoennes for the format dumping code and other input.
And to eric256 for noticing something... :-)

=head1 SEE ALSO

L<perl>. L<Perlmonks|http://www.perlmonks.org>.

And of course www.perlmonks.org

=cut


# DDS XS migration evidence

## Source baseline

The initial DDS inspection used commit afe97cc, version 2.42. Existing
user edits to Changes, META.json, and META.yml were preserved. Release
notes now include the conversion, and regenerated metadata preserves the
author and version while updating dependencies and the Perl floor.

Relevant DDS code:

| Source | Role |
| --- | --- |
| lib/Data/Dump/Streamer.pm, _make_args | Retains references to caller scalar slots and captures root metadata |
| lib/Data/Dump/Streamer.pm, Data | Breadth-first graph analysis, freezer handling, and closure-root discovery |
| lib/Data/Dump/Streamer.pm, _reg_scalar and _reg_ref | Separate scalar/referent identity catalogues, names, depths, and counts |
| lib/Data/Dump/Streamer.pm, Out and _dump_sv/_dump_rv | Depth-first emission informed by that catalogue |
| lib/Data/Dump/Streamer.pm, _add_fix and _dump_apply_fix | Deferred reconstruction and finalization |
| lib/Data/Dump/Streamer.pm, _dump_code and _get_lexicals | Deparsing and closure identity integration |
| lib/Data/Dump/Streamer.xs | Existing introspection and reconstruction primitives |
| lib/Data/Dump/Streamer/_/Printers.pm | Scalar/list output accumulation |

DDS already requires XS. The conversion moves its Perl engine and
bookkeeping into C; it is not the addition of an optional accelerator.

## Perl build

On 2026-11-09, CPAN's source index listed 5.45.4 as the latest development
release: https://www.cpan.org/src/ .

The successful perlbrew command was:

```sh
perlbrew install --as perl-5.45.4-rc-stack-debug --debug -j 8 -Dusedevel -Accflags='-DPERL_RC_STACK -DDEBUG_LEAKING_SCALARS' perl-5.45.4
```

The installed interpreter reports version 5.45.4, compiler flags including
PERL_RC_STACK, DEBUGGING, and DEBUG_LEAKING_SCALARS, and a value of 1 from
Internals::stack_refcounted(). The core suite passed 2,960 files and
1,395,709 tests. The initial perlbrew log is
~/perl5/perlbrew/build.perl-5.45.4.log; subsequent perlbrew builds can
overwrite that log.

The refcounted argument-stack documentation was inspected in
~/git_tree/perldev/blead/pod/perlguts.pod, along with rpp_invoke_xs,
xs_wrap, and related stack APIs in inline.h and pp_hot.c. This confirms
the distinction between interpreter stack ownership and ordinary
descendant traversal through C pointers. Boundary bookkeeping belongs
at entry/capture points; it does not require per-child Perl calls.

The public sv_get_backrefs API was inspected in sv.c. Hash referents can
store backreferences in HV auxiliary storage, whereas other SVs use
backreference magic. The new predicate must cover both.

## Tracking probe

A temporary C probe built against Perl 5.45.4 walked ordinary AV/HV/RV
graphs without creating references to inspected children. It used
SvREFCNT greater than one or non-null sv_get_backrefs to decide whether
to consult an address table. Immortal SVs were handled separately.

| Fixture | Visits | Identity lookups | Inserts | Repeated encounters |
| --- | ---: | ---: | ---: | ---: |
| Binary tree of depth 12 with three scalar leaves per terminal array | 28670 | 1 | 1 | 0 |
| Shared child array | 9 | 3 | 2 | 1 |
| Self-referential array | 4 | 3 | 2 | 1 |
| Two aliased scalar slots | 4 | 3 | 2 | 1 |
| One strong and one weak reference | 7 | 3 | 2 | 1 |
| Single occurrence with external owners | 7 | 2 | 2 | 0 |

The sole tree lookup was for the root argument scalar. In every fixture,
the measured root and root-referent refcounts were unchanged across
the walk. The weak case required tracking a target whose strong refcount
was one. The external-owner case demonstrates conservative registration
without an actual repeated encounter.

This probe used a recursive traversal and a Perl HV as its temporary
address table. It did not implement DDS breadth-first names, fixups,
callbacks, or code generation and did not measure DDS throughput.
Temporary sources were left in /tmp/dds-xs-analysis; they are disposable,
not a build dependency or a permanent test harness.

## Data Dumper references

The local blead checkout inspected initially was at 20aecbd6fc.
Relevant code is under ~/git_tree/perldev/blead/dist/Data-Dumper/.

- Dumper.xs, Style: configuration grouped for C traversal.
- Dumper.xs, DD_dump: direct SV/AV/HV traversal, output append operations,
  identity records, and deferred statements.
- Dumper.xs, scalar Sparseseen branch: refcount-one scalars can avoid
  registration. Reference referents have a separate tracking path.
- Dumper.xs, quoting and regexp helpers: current C implementations of
  Perl source formatting, subject to DDS's own compatibility rules.

Data::Dumper's DFS/seen/post-statement organization does not replace
DDS's minimum-depth and canonical-name analysis.

## Sereal references

Relevant code is under ~/git_tree/Sereal/Perl/.

| Source | Useful pattern |
| --- | --- |
| shared/ptable.h | Native pointer-to-value table and explicit allocation lifecycle |
| Encoder/srl_encoder.c, CALL_SRL_DUMP_SV | Direct ordinary-scalar fast path |
| Encoder/srl_encoder.c, srl_dump_sv | Conservative counts, weak-backreference exception, and identity encounters |
| Encoder/srl_encoder.h, srl_encoder_t | Persistent options, operational flags, reusable buffers, and tables |
| Encoder/Encoder.xs and Encoder/typemap | C-backed objects and a small XS binding layer |
| Encoder/srl_encoder.c, srl_prepare_encoder | Reentrant working context and save-stack cleanup |
| Encoder/srl_encoder.c, srl_callback_begin/end | Keepalives and validating live traversal state across callbacks |
| Encoder/t/043_alias_scalar_refs.t | Distinct scalar referents versus aliases and equal values |
| Encoder/t/171_weakref_rewrites.t | Weak slots, references to those slots, and replacement behavior |

Sereal's byte-offset rewriting and weak-only promotion policy belong to
its binary format. DDS needs expression names, dependency ordering, and
its own reconstruction policy. Reuse low-level mechanisms selectively.

## Local DDS test report

After resolving a stale CPAN configuration, the user reported that DDS
passes its tests. The inspected Build script selected Perl 5.44.0.
The implementation subsequently captured an isolated baseline: 26 files
and 375 tests passed on 5.44.0. The RC baseline had three raw-refcount
assertions affected by argument-stack ownership; generated dumps passed.
The refcount tests now include the reported stack contribution rather
than changing counts inside the introspection primitive.

## Planning isolation checks

The planning setup was checked with Module::Build 0.4234 and
ExtUtils::Manifest. Both skip files match the planning directory and
nested representative paths. MANIFEST has no planning entries, and
Git's ignore rules do not ignore the directory or its documents.

In a disposable source copy, sentinel PM, POD, XS, PL, and test files
inside planning were excluded from all six corresponding Module::Build
discovery methods. A staged distribution created with the distdir
action contained no planning directory. Planning document links,
whitespace, and code fences also passed local checks.

## Final correctness results

The final source archive, Data-Dump-Streamer-2.42.tar.gz, was extracted
into a separate clean directory for each interpreter. Every build was
configured using that interpreter, compiled from shipped C/XS sources,
and tested without another build's blib directory.

| Interpreter | Files | Tests | Result |
| --- | ---: | ---: | --- |
| Perl 5.22.4 | 30 | 4118 | PASS |
| Perl 5.44.0 | 30 | 4118 | PASS |
| Perl 5.45.4 DEBUGGING, RC stack, leaking-scalar debug | 30 | 4118 | PASS |

Two ithread-specific assertions skip on these nonthreaded local builds.
CI adds a separate threaded 5.44 job. The full stable matrix from 5.22
through 5.44 is configured but has not been executed remotely.

The repository's own rebuilt 5.44 working tree also passes all 4118
assertions. Optional PadWalker, Algorithm::Diff, and Cpanel::JSON::XS are
installed for the local suites; closure/JSON cases are exercised.

Temporary logs and extracted builds:

- /tmp/dds-xs-verified-dist-522-test.log
- /tmp/dds-xs-verified-dist-544-test.log
- /tmp/dds-xs-verified-dist-rc-test.log
- /tmp/dds-xs-repo-test.log
- /tmp/dds-xs-verified-dist-{522,544,rc}/Data-Dump-Streamer-2.42/
- /tmp/dds-xs-release-final/Data-Dump-Streamer-2.42.tar.gz

These files are disposable evidence, not dependencies of the repository.
Reproduce a build with its selected interpreter:

```sh
CCACHE_DISABLE=1 /path/to/perl Build.PL DDS
CCACHE_DISABLE=1 /path/to/perl Build test
```

RC checks additionally use PERL_DESTRUCT_LEVEL=2. Confirm
`Internals::stack_refcounted() & 1` before calling a run an RC-stack test.

## Memory and lifetime checks

Valgrind ran the four added suites under the RC debug interpreter with
`--trace-children=yes --leak-check=full --errors-for-leak-kinds=definite
--error-exitcode=77`. All 3733 assertions passed at that checkpoint and
every per-process Valgrind log was empty. Child tracing matters because
Test::Harness launches separate Perl processes.

After the final ownership fixes, t/xs_engine.t ran directly under
Valgrind from the final extracted archive. Its 69 assertions passed
(two ithread skips), exit status was zero, and
/tmp/dds-xs-valgrind-verified.log was empty. This includes recursive Data
and delayed Dump, input-destructor recursion, printer failures during
ordinary and deferred-glob emission, removed referents/address reuse,
weak-first sharing, tied containers, and 1200 nested arrays.

No memory-error or definite-leak diagnostic was reported by these runs.
This is targeted lifetime coverage, not a claim that every Perl callback
or platform has been checked.

## Distribution and installation

The actual archive contains all four linked C sources and the engine
header. It contains no planning or .github directory. Planning sentinels
were again excluded by Module::Build test, PM, POD, XS, PL, and script
discovery, and by a staged distribution.

Installing the extracted 5.44 build with
`perl Build install --destdir /tmp/dds-xs-install-verified` installed only
Perl modules, the shared library/bootstrap file, and manpages. A Perl
process launched from /tmp loaded that installation and independently
verified an ASCII-only Unicode scalar round trip.

POD checking, workflow YAML parsing and matrix membership, and
`git diff --check HEAD` pass. Planning remains Git-tracked, with no
MANIFEST or install entries. All new files were staged upon creation.

## Measured performance

Compare the original working-tree engine with the C engine on the same
nondebug Perl 5.44.0, compiled with -O2. Fixtures are built once, outside
timing. Each iteration calls Data and then one Out; time these calls
separately using Time::HiRes. Repeat for at least two seconds. Options:
Indent(0), Rle(0), Names('data'). Output is consumed in scalar context.
The original engine's repeated Out alone retains completion state, so a
benchmark that times repeated Out without fresh Data is invalid.

Fixtures:

- Flat: [1 .. 10000].
- Tree: 1000 independent [number, string, {x => number}] children.
- DAG: 3000 RV slots cycling through 100 distinct {value => number} hashes.

Peak RSS is the process high-water mark from /usr/bin/time, including
interpreter, fixtures, and repeated operations. It does not isolate the
engine allocator. Results are local samples, not a cross-platform
performance guarantee.

| Fixture | Old analysis ms | XS analysis ms | Old emission ms | XS emission ms | Total speedup | Old / XS peak RSS KiB |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| flat | 48.6398 | 0.1241 | 79.1490 | 2.7798 | 44.0x | 19756 / 20328 |
| tree | 36.8302 | 1.1329 | 64.8926 | 2.9416 | 25.0x | 21092 / 20356 |
| dag | 25.9872 | 1.0695 | 42.3944 | 3.2058 | 16.0x | 20992 / 19816 |

Output byte counts agree with the original engine for these fixtures.
Flat and tree inputs have one candidate record; the DAG has 101. Peak RSS
is similar in these small fixtures, so these measurements support a
throughput improvement, not a broad memory-reduction claim.

Timing script used (run once with each engine's built library paths):

```perl
use strict; use warnings; use Time::HiRes qw(time); use Data::Dump::Streamer qw(Dump);
my $which=shift || 'tree';
my $data = $which eq 'flat' ? [1..10000] : $which eq 'tree' ? [map { [$_, "value$_", {x=>$_}] } 1..1000] : do { my @pool=map { {value=>$_} } 1..100; [map { $pool[$_%100] } 1..3000] };
my $d=Dump()->Indent(0)->Rle(0)->Names('data');
my ($analysis,$emission,$bytes,$runs)=(0,0,0,0); my $start=time;
do {
 my $t=time; $d->Data($data); my $middle=time; my $text=$d->Out; my $end=time;
 $analysis += $middle-$t; $emission += $end-$middle; $bytes=length($text); ++$runs;
} while time-$start<2;
printf "%s data=%.4f out=%.4f total=%.4f ms bytes=%d runs=%d",$which,1000*$analysis/$runs,1000*$emission/$runs,1000*($analysis+$emission)/$runs,$bytes,$runs;
if($d->can('_xs_stats')) { my $s=$d->_xs_stats; print " candidates=$s->{candidates} lookups=$s->{lookups}" } print "\n";
```

Local benchmark logs are /tmp/dds-benchmark-final.log (original engine)
and /tmp/dds-benchmark-verified-new.log (final engine). These measurements
include normal allocation and output-string construction. Fixture
creation is excluded. No debug-versus-release comparison is used.

## Native facade checkpoint results

After removing most of the remaining Perl facade, the full suite passes
31 files and 4180 tests on Perl 5.22.4, 5.44.0, and the RC-stack 5.45.4
build. Logs: /tmp/dds-api-test.log, /tmp/dds-api-522-test.log, and
/tmp/dds-api-rc-test.log. These supersede the earlier 4118-test totals.

The added API suite contains 62 assertions. A direct RC-debug Valgrind run
passed its preceding 59-assertion version without diagnostics. The final
API and engine suites passed all 131 assertions under Valgrind with child
tracing enabled, and every per-process diagnostic log was empty; see
/tmp/dds-api-checkpoint-valgrind-test.log and the corresponding
/tmp/dds-api-checkpoint-valgrind.*.log files.

Applied the local perl_core_skillz installed-Perl XS, memory, and callback
skills, together with the core SV/magic guidance. This prompted explicit
argument-slot retention before stack growth, retained exception/results
through cleanup, receiver ownership across option FETCH, and a bounded
regression for hash iterator preservation.

The performance table above measures the first XS checkpoint, before the
additional native facade cleanup. It establishes the graph conversion's
benefit; it has not been rerun for this final facade revision. Remote CI
and wider real-world benchmarks remain release review work.

## Review: traversal temporary lifetimes

Claude's larger fixtures exposed temporary strings and sorting workspaces
that survived until the entire Data or Out call returned. The traversal
now frees scratch values after each breadth-first item and emission step.
Suspended emission frames own their names, class strings, key arrays,
hidden-key arrays, postdump snapshots, and any temporary tied-fetch SV.
Ordinary graph values remain borrowed during callback-free traversal.
Processed queue entries release their names immediately.

The repository-only `planning/memory-benchmark.pl` reproduces the wide
record fixture and the deep callback fixture. It excludes fixture creation
from timing, uses Indent(0), and reports Linux peak RSS growth in KiB.
Run it against each checkout's own blib and installed Perl. Single-run
measurements on the same non-debug Perl 5.44.0 build on this Linux host:

| Fixture | Engine | Seconds | Peak RSS growth (KiB) |
| --- | --- | ---: | ---: |
| 100,000 records | original Perl engine | 18.169 | 124,380 |
| 100,000 records | XS before temporary cleanup | 1.002 | 601,956 |
| 100,000 records | XS after temporary cleanup | 0.964 | 21,496 |
| 300,000 records | XS after temporary cleanup | 2.557 | 64,356 |
| 2,000 levels, no callback | XS after temporary cleanup | 0.061 | 36,268 |
| 2,000 levels, SortKeys callback | XS after temporary cleanup | 0.282 | 48,936 |

The 100,000-record output is 3,880,954 bytes in all three versions.
The original and revised engines also have the same output SHA-256:
`0161d77e333bf375d80808d835eabb2d12dd2a6d7d037ff16c284715a228d71a`.
Full path names still impose a depth-dependent cost for suspended frames;
these changes remove the accumulation across already completed work.
Timings are host-specific observations, not release performance promises.

All 4270 tests pass on Perl 5.22.4, 5.44.0, and the RC-stack debug 5.45.4
build. In particular, the tied-container regression exercises temporary
fetch values crossing an emission-step cleanup boundary. Logs are
`/tmp/dds-fix-temporaries-{522,544,rc}.log`.
The engine, freezer-semantics, and output-handle suites also pass all 112
assertions under Valgrind with child tracing enabled; all per-process
diagnostic logs are empty (`/tmp/dds-temporaries-valgrind.*.log`).

## Review: opaque catalogue addresses

The catalogue now stores integer addresses as lookup keys. No weak RVs
are created for identity records, and callback guards never scan the
catalogue to retain input SVs. Rehashing, cleanup and diagnostics inspect
metadata only. Deferred glob emission owns its glob, and freezer proxies
are explicitly registered even when one proxy RV represents several
occurrences of the original object.

`t/catalogue_lifetimes.t` checks magic flags and reference counts before
and after analysis/output for shared scalars, arrays, hashes and reference
slots, including read-only values. It also checks existing weak references
and callback output after catalogued descendants have been removed. The
address-reuse guarantee was deliberately dropped: callers must avoid
mutation during dumping and call Data again after structural changes.
Otherwise reconstruction is best effort.

All 4344 tests in 34 files pass on Perl 5.22.4, 5.44.0 and the RC-stack
debug 5.45.4 build (`/tmp/dds-fix-opaque-catalogue-{522,544,rc}.log`).
The catalogue, engine, freezer and output-handle suites pass 186 assertions
under Valgrind on the RC-stack debug build, with child tracing and
PERL_DESTRUCT_LEVEL=2; all per-process diagnostic logs are empty
(`/tmp/dds-opaque-valgrind.*.log`). The 100,000-record benchmark retains
the same output hash and about 21 MiB peak RSS growth.

## Glob slot reconstruction paths

Further probing found that array/hash elements in glob slots could get
bare-glob fixup paths such as `*Package::name[1]`, which are invalid Perl.
Emission now uses the slot's reference path for container children.
Anonymous globs use their reconstruction expression during analysis,
and nested glob expressions are braced consistently.

The 48 assertions in `t/glob_roundtrip.t` cover named and anonymous globs,
top-level and nested glob references, and both array and hash slots with
shared referents and scalar aliases. All eight cases failed to evaluate
before the fix on Perl 5.22.4. All 4392 tests in 35 files pass after the
fix on Perl 5.22.4, 5.44.0 and RC-stack debug 5.45.4. Existing glob and
overload output fixtures are unchanged.

## Sharing between glob slots and ordinary graph locations

Claude's follow-up found that a hash shared between a glob slot and an
ordinary array element could be emitted inline under the element while
its canonical name still referred to the uninitialized glob slot.
Glob slots could enter the FIFO queue at depth 7 before a subsequently
enqueued ordinary path at depth 6. The first path then won incorrectly.

The analyzer now uses numeric-depth buckets with FIFO ties. Queue indices
remain stable when storage grows, and callback retention visits pending
entries only. The emitter compares actual and canonical expressions
instead of treating glob frame depths as equivalent to analysis depths.
Noncanonical glob slots wait on the canonical construction. Glob lookup
also follows owned freezer proxies before choosing the effective role.

The 520 assertions in `t/glob_sharing.t` cover named and anonymous globs,
different depths and orders, competing equal-depth deferred globs,
freezer replacements, descendant sharing, scalar/code slots, and cycles.
The pre-fix Perl 5.22 build fails the new regression; see
`/tmp/dds-glob-canonical-before-522.log`. Old named-glob fixtures now use
the shallower ordinary scalar path as canonical, and scalar glob paths
spell out their SCALAR slot explicitly.

All 4912 tests in 36 files pass on Perl 5.22.4, 5.44.0 and RC-stack debug
5.45.4 (`/tmp/dds-fix-glob-canonical-{522,544,rc}.log`). A read-only Terra
follow-up found no queue-ordering or ownership defect and passed a
disposable 600-case shared hash/glob graph probe. It separately identified
a postdump-hook gap for distinct objects sharing one freezer proxy;
the follow-up below fixes that separately.

The wide 100,000-record fixture still has the same output SHA-256 and
about 23 MiB peak RSS growth with the depth-bucket queue. The change adds
one integer link per queued item and a small per-depth bucket array.
The focused eight-file suite passes 780 assertions under Valgrind on
RC-stack debug Perl with child tracing and PERL_DESTRUCT_LEVEL=2; all
per-process diagnostic logs are empty
(`/tmp/dds-glob-canonical-final-valgrind.*.log`).

## Postdump lifecycle for shared and chained proxies

Distinct original objects can share a single freezer proxy. The prior
early-return and noncanonical-glob paths then omitted some original
objects' postdump hooks. Calling them immediately would instead risk
restoring a proxy before its canonical occurrence was serialized, and
keeping only one frame snapshot would lose earlier proxy-chain hooks.

Postdump tasks now collect every eligible original once while traversing
live values. Each task owns a new RV to the already-fetched referent and
borrows its stable catalogue record. This avoids an extra tied FETCH that
could otherwise select a different object. After graph serialization and
fixups finish, the queue drains through the guarded callback boundary.
Pending snapshots are also released when an operation fails. Ordinary
catalogue records acquire no new input references or magic.

`t/freezer_semantics.t` now has 99 assertions, adding shared-proxy hooks
through elements, globs and mixed paths; repeated occurrences and Out;
reentry; exceptions and reuse; restoration timing; chained proxies; and
a changing-FETCH identity regression. Terra reproduced the original gaps
and verified their correction in the working tree, finding no further
concrete ownership or cleanup issue. That follow-up did not independently
rerun the complete suite or Valgrind.

All 4988 tests in 36 files pass on Perl 5.22.4, 5.44.0 and RC-stack debug
5.45.4 (`/tmp/dds-fix-shared-postdump-{522,544,rc}.log`).
The focused eight-file suite passes 856 assertions under Valgrind on
RC-stack debug Perl with child tracing and PERL_DESTRUCT_LEVEL=2; all
per-process diagnostic logs are empty
(`/tmp/dds-shared-postdump-final-valgrind.*.log`).

# DDS XS validation plan

## Interpreter and build isolation

The installed development interpreter is:

```text
perl-5.45.4-rc-stack-debug
/home/demerphq/perl5/perlbrew/perls/perl-5.45.4-rc-stack-debug/bin/perl
```

Verify its configuration and runtime stack flag before a baseline run:

```sh
perlbrew exec --with perl-5.45.4-rc-stack-debug perl -V:version -V:ccflags
perlbrew exec --with perl-5.45.4-rc-stack-debug perl -e 'print Internals::stack_refcounted(), "\n"'
```

Use a separate clean build copy for each interpreter. Run Makefile.PL
with that interpreter, then make and make test. MakeMaker records the
interpreter in the generated Makefile. Do not reuse blib or object files across
Perl versions or configurations.

Install dependencies with the intended interpreter. Makefile.PL declares
configure, runtime and test dependencies to the CPAN installer. Dependency
installation is setup, not evidence that DDS itself passes.

Local checks use Perl 5.22.4, 5.44.0, and 5.45.4 with the refcounted
stack. The full stable-series matrix and threaded build run in CI only.
XPerl class support additionally uses isolated builds of installed XPerl
5.45.5 with ordinary and refcounted stacks. Its tests skip on interpreters
without the snapshot builtins. Local XPerl checks do not replace stable
Perl CI or change the historical compatibility probe policy.
A 32-bit build has not been exercised locally. Keep correctness and
performance builds distinct: debug instrumentation changes timing.

## Existing coverage

| Area | Existing starting points under t |
| --- | --- |
| Calling conventions and names | usage.t, dump.t, names.t, as.t |
| Scalar/reference utilities | refcount.t, refaddr.t, reftype.t, refelem.t, xs_subs.t |
| Sharing and complicated graphs | madness.t, hardrefs.t, dogpound.t, tree.t |
| Weak references | madness_w.t |
| Objects and hooks | blessed.t, overload.t, filter.t |
| Readonly and restricted hashes | readonly.t, locked.t |
| Code, closures, and symbol tables | lexicals.t, globtest.t, stash.t |
| Formatting and output variants | sortkeys.t, terse.t, impure_madness.t, madness_json.t |

Read the assertions and skips before assigning a feature to a test.
Record optional dependency availability. A skipped closure or weakref
case is not coverage of the corresponding XS implementation.

## Graph correctness

Use two kinds of checks: compatible emitted text, and reconstruction
semantics after evaluating that text. Test helpers may use addresses
and mutation to establish identity; do not rely solely on Data::Dumper
output or is_deeply to detect scalar aliases.

Apply full graph reconstruction assertions to modes that promise that
fidelity. Preserve the documented limitations of impure and terse
representations rather than silently changing their output contract.

Required graph families:

- Ordinary trees containing unshared scalar values and containers.
- Distinct equal-valued scalars; distinct equal-valued scalar referents.
- Two independent RV scalars pointing at the same array/hash/scalar.
- One scalar SV aliased into several array/hash slots and roots.
- References to those scalar slots, including RV-containing slots.
- DAG diamonds, multiple roots in different orders, and shallow canonical
  locations first reached through deeper DFS paths.
- Self-cycles and mutual cycles through arrays, hashes, and scalar RVs.
- Strong-first and weak-first sharing, weak-only reachable objects,
  weak backedges to the root, and references to weak scalar slots.
- External strong/weak owners that do not appear inside the dumped graph.
- Constants, immortals, readonly values, holes, explicit undef values,
  restricted-hash placeholders, and blessed scalar referents.

After reconstruction, compare alias classes and referent-sharing
relationships. Assign through one member of an alias class and verify
the other changes; replace one of two merely shared RV scalars and
verify the other scalar remains independent.

For weak links, test isweak as well as address equality. Release strong
owners where the test can do so deterministically and check the
expected destruction/clearing behavior.

## API and operation lifetime

Exercise scalar/list/void contexts, To destinations, method and function
calls, delayed Dump capture, Data with and without new arguments,
Names before/after capture, repeated Out, reset, and destruction.

Test changes to captured root variables and their descendants between
Data and Out. Define the required behavior from the baseline before
implementing catalogue reuse. Include removal/replacement of previously
catalogued nodes and new allocations, not only value changes.

Test failures during construction, analysis, sorting, freezing,
deparsing, printing, and postdump processing. Check that active state
is cleaned and the object either remains reusable or reports its
documented invalid state. Exercise callback recursion on the same object,
callbacks releasing the owner variable, and thread behavior.

## Source and scalar fidelity

Cover bytes and UTF-8 strings and keys, embedded NULs, control characters,
quotes and interpolation characters, empty strings, numeric-looking
strings, negative zero, large signed/unsigned integers, floating-point
round trips, dualvars, booleans, and vstrings.

Verify regex modifiers, delimiter escaping, blessings, code stubs,
formats, closure behavior, shared captured variables, and name collisions.
Preserve intentional quoting differences from Data::Dumper.

## Instrumentation and benchmarks

Use development-only counters for visits, table lookups and insertions,
repeated identities, queue/record capacity, fixups queued/resolved, and
bytes emitted. Prove that ordinary refcount-one leaves avoid lookups;
also verify unique containers containing shared children.

Separate benchmark fixture creation from timing. Keep external-reference
counts stable between runs. Include a wide tree to expose BFS frontier
cost, deep chains, flat scalar arrays, shared DAGs, cycles, alias-heavy
graphs, long strings, mixed keys, and objects with actual callbacks.

Compare analysis time, emission time, total time, output size, candidate
records, peak memory, and repeated-object allocation behavior. Report
Perl build flags, options, fixture size, repetitions, and measurement
method. Data::Dumper can provide context but is not an equivalent-work
baseline when its options or fidelity differ.

## Planning directory isolation

Keep planning files out of MANIFEST and match the directory in both
skip files. Verify MANIFEST.SKIP using ExtUtils::Manifest's maniskip
function, including representative nested paths.

Inspect MakeMaker's explicit PM map, XS input and test glob. Build a
release archive and verify that neither planning files nor generated
native build copies appear. Stage DDS and NODDS installations under
DESTDIR and load the installed module to verify the actual payload.
Confirm Git does not ignore planning/ or its documents.

## Completion criteria

The complete supported suite passes on the declared interpreter matrix;
graph-semantic assertions pass independently of textual comparisons;
memory/cleanup checks pass on the refcounted-stack debug build; and
the distribution builds and loads in isolation without the old engine.

A performance claim requires comparable measured runs. The investigation
probe described in evidence.md establishes a tracking opportunity, not
an implemented DDS speedup.

## Added regression suites

- `t/xs_engine.t`: independent alias/sharing mutation checks, sparse
  catalogue counters, weak order, delayed roots, repeated output, removed
  removed identities, explicit reanalysis, callback failure/reentrancy/mutation, dualvars,
  Unicode sorting, tied scalars/containers, deep traversal, diagnostics,
  and thread cloning (skipped without ithreads).
- `t/xs_graphs.t`: 50 deterministic mixed cyclic graphs, with an independent
  BFS topology representation for aliases, referents, and weak flags.
- `t/regexp_roundtrip.t`: supplied sigil/bracket strategy, delimiter and
  backslash runs, interpolation counter, output-record-separator trap,
  match equivalence, and extended/control/wide patterns.
- `t/utf8_roundtrip.t`: byte/Unicode counterparts over all Latin-1 code
  points, numeric strings, controls, empty and long values, mixed arrays,
  and Unicode dualvars. Checks values and UTF-8 flags separately.

For Valgrind under Test::Harness, use `--trace-children=yes`; otherwise
only the harness process is instrumented. Use per-process log names and
inspect every log, as well as the harness exit status.

## Native API coverage

`t/xs_api.t` asserts that the public facade methods are XSUBs and tests
subclass construction, option contexts, name normalization, explicit key
arrays, DumpVars sharing/aliases, PadWalker caller names, filehandle output,
localized print globals, each iteration with and without deletion, callback
stack growth, false exception objects, and option FETCH releasing the
receiver. Hash iterator cases are bounded so a restart regression fails.

The minimum local Perl ships ExtUtils::ParseXS 3.28; the development build
uses 3.65. Both build the distribution through its normal generator. Native
API registration uses traditional XSUB calling conventions, including the
RC-stack compatibility wrapper provided by the development interpreter.

## Historical CI assessment after the 5.6 gate probe

The results below are observed at d2fef33 in PR run 42676464037, rather
than predictions from API introduction dates. All stable lanes from 5.22
through 5.44, threaded 5.44, and development RC-stack Perl passed.

| Perl | Observed stopping point |
| --- | --- |
| 5.6 | setup-perl's cpanm cannot load Scalar::Util before DDS configuration |
| 5.8, 5.10 | DDS native compilation: SVt_REGEXP is unavailable |
| 5.12, 5.14, 5.16 | Streamer.xs compilation: PADNAMELIST and PADNAME are unavailable |
| 5.18, 5.20 | Native library builds, but loading fails with unresolved HvHasAUX |

Older compile logs also report missing APIs such as cBOOL, isDIGIT_A,
utf8_to_uvchr_buf, isWORDCHAR_uni and HvNAMEUTF8. These first failures do
not establish the complete compatibility effort. Optional JSON dependency
test failures no longer prevent DDS configuration in those jobs.

Downloaded logs are /tmp/dds-old-ci-{56,58,510,512,514,516,518,520}.log.
Jobs 129359028710, 129359028470, 129359028315, 129359028660,
129359028679, 129359028636, 129359028222 and 129359028422 correspond
respectively to those versions. No older-version implementation changes
were made during this assessment, as requested. The provisional 5.6
metadata remains a release decision: 5.22 is the demonstrated working
floor and the current declaration must not be mistaken for passing 5.6.

## Astra review follow-up

Dynamic policy keys now use hv_fetch_ent through dds_hget_sv; fixed ASCII
configuration keys retain dds_hget. Regression tests cover Latin-1 and wide
class names for freezer selection, selected/disabled freezer caches,
ignore policies, and key ordering. Option keys, named selectors and primary
method registration now derive from one definition list. Aliases refer to
named selectors.

A separate reference-emitter refactor follows the build conversion.

The final follow-up passes 5000 tests each on stock Perl 5.22.4, 5.44.0,
5.44.0 with ithreads, and RC-stack debug 5.45.4. XPerl 5.45.5 passes
5173 tests with the refcounted stack and 5172 with the ordinary stack;
optional module availability differs. All runs cover 37 test files.
Logs: /tmp/dds-astra-followup-{522,dev,thread,xrc,xplain}.log and
/tmp/dds-option-enum-test.log (stock 5.44).

## Perl 5.6 dependency bootstrap

The 5.6 CI lane now builds Scalar-List-Utils 1.70 directly with the target
Perl's MakeMaker before invoking cpanm. The pinned release's Makefile.PL
requires Perl 5.006. The downloaded archive is checked against SHA-256
before extraction. PREFIX and LIB keep installation in RUNNER_TEMP, and
PERL5LIB exposes both its module and architecture directories to later
steps. A smoke check loads Scalar::Util and List::Util and exercises each.
This breaks the cpanm/Scalar::Util bootstrap dependency without changing DDS
compatibility. The bootstrap does not run the upstream test suite, whose
Test::More dependency is not yet installed. The full historical build and
execution check remains in CI, as requested. Local validation checked the
archive, upstream build requirement, workflow YAML, and shell syntax only.

## Restoring compatibility paths after the conversion

Regexp detection now distinguishes magic-backed PVMG values from native
regexp SVs, and pattern extraction uses Data::Dump::Streamer::regex. Old
Perls keep the existing XS extractor; later ones use re::regexp_pattern.
The historical 5.13.6–5.13.9 modifier filtering is retained. Modern tests
add compatibility API, shared identity and reblessed-regexp assertions.

Closure inspection now handles AV-backed pads before 5.18. The pre-5.22
hash backreference path tests SvOOK for auxiliary storage. Devel::PPPort
3.73 regenerated ppport.h, now included through the common engine header
instead of only Streamer.xs. The native modules request their required
local compatibility implementations. Older stash and pad flag accessors
have guarded fallbacks. This addresses known compilation/load failures;
it does not establish passing historical suites before CI executes them.

Full suites pass 5009 assertions on stock 5.22.4, unthreaded/threaded
5.44.0 and RC-stack debug 5.45.4, and 5182 on XPerl RC. The logs are
/tmp/dds-compat-ppport-{522,544,thread,dev,xrc}.log. Historical builds and
tests remain in CI only. Regeneration used the installed Perl 5.44.0
Devel::PPPort; the generated compatibility header is committed verbatim.

The separate 5.6 bootstrap succeeded at d0a9a0a. Its next failure is the
setup-perl cpanm's bundled JSON::PP 4.18 requiring Perl 5.8 while loading
metadata machinery. This is an installer dependency issue before DDS,
not a Scalar::Util installation failure. Source inspection identifies the
bundled JSON/PP.pm package at cpanm line 1673. Job 129414015357 records
this result; its downloaded log is /tmp/dds-56-bootstrap-next.log.

## MakeMaker and Perl 5.18 release floor

The user selected Perl 5.18 after run 42693449034 passed all stable
series from 5.18 through 5.44, threaded 5.44 and development RC Perl.
The older probe lanes and their 5.6 bootstrap have been removed. The
historical results above remain investigation records, not current policy.

Makefile.PL now owns configuration, native compilation, dependency
metadata, tests and installation. Streamer.xs is at the repository root.
Native C sources remain under src/ and have generated root build copies
so MakeMaker can use its platform compiler rules. Explicit dependencies
cover the source, engine header and ppport.h. The former Build.PL and
custom builder are removed, as are Module::Build and CBuilder dependencies.
DDS/NODDS, saved alias preferences and interactive selection are preserved.

Fresh MakeMaker builds pass 5009 tests on Perl 5.22.4, unthreaded and
threaded 5.44.0 and development RC Perl, and 5182 on XPerl RC. NODDS
passes 5009 tests. Staged DDS and NODDS installs load and dump correctly.
Touching the engine header schedules all six objects for recompilation.
The release archive contains Makefile.PL and Streamer.xs, excludes planning
and generated native copies, and declares Perl 5.18. MakeMaker disttest
passes all 5009 tests from the staged distribution. Logs are
/tmp/dds-mm-{test,522-test,dev-test,thread-test,xrc-test,nodds-test,disttest}.log.

Version 3.00 repeats those MakeMaker checks with the release version in
both the main module and Deparser. Preserve the version string instead of
evaluating it numerically: evaluating 3.00 produced 3 and broke the strict
component-version check. All interpreter suites and the distribution's
5009-test disttest pass after the version update. Staged DDS and NODDS
installs report 3.00 and dump correctly. INSTALL.SKIP is retained because
ExtUtils::Install uses it under MakeMaker. The final archive is
Data-Dump-Streamer-3.00.tar.gz; versioned logs are /tmp/dds-mm-300-*.log.

## DDS alias review follow-up

Claude's 3f3c7bd packaging review passed four interpreter suites and
staged installation, with no open ordinary-Perl findings. It noted that
the generated alias had lost its POD and a scannable version assignment.
Makefile.PL now generates the shortcut with NAME/SYNOPSIS/DESCRIPTION POD,
a literal version obtained from the main module at configuration time,
and an explicit DDS package declaration. The latter keeps the generated
VERSION assignment out of the caller's package.

All 5009 tests pass. MM->parse_version reads 3.00 without loading DDS,
the alias loads without changing a caller's VERSION, podchecker accepts
the stub, and a staged installation retains the documentation and loads
version 3.00. This follow-up is separate from the reference-emitter refactor.

A final alias check sets the already-loaded main module to a distinct
version before loading DDS. The literal generated file version is assigned
to DDS before the runtime stash alias is installed, so loading the shortcut
does not overwrite the main module's version. The shortcut then follows
the loaded module's version, while file scanners retain the generated
literal. Caller VERSION isolation and all 5009 assertions pass.

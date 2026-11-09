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

Use a separate clean build copy for each interpreter. Regenerate Build
with the selected interpreter, build, then run its tests explicitly
with that interpreter. An existing Build shebang can select a different
Perl from the active shell. Do not reuse blib or object files across
Perl versions or configurations.

Install dependencies with the intended interpreter. cpanm has its own
working-directory configuration; Module::Build's installdeps action
defaults to the CPAN client unless cpan_client is changed. Dependency
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

Inspect Module::Build's discovered tests, PM/POD/XS files, scripts, and
generated PL files. No discovered source should start with planning/.
Run the same discovery checks in a disposable copy containing sentinel
files named planning/lib/PlanningSentinel.pm,
planning/lib/PlanningSentinel.pod, planning/PlanningSentinel.xs,
planning/PlanningSentinel.PL, and planning/t/PlanningSentinel.t.

Stage a distribution from the real MANIFEST in that disposable copy and
verify no planning path is copied. The installed payload is built from
the discovered lib sources and blib, not directly from INSTALL.SKIP.
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

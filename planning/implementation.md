# DDS XS implementation sequence

Implement reviewable slices with explicit invariants and focused checks.
Do not change expected output merely to make a new implementation pass.
Use the existing engine as a development reference; remove it from the
final runtime once the XS implementation covers the supported features.

## Task 1 Capture the compatibility baseline

Record the configured Perl, dependencies, optional features, test
results, and existing failures for Perl 5.44.0 and the installed 5.45.4
refcounted-stack debug build. Use isolated build directories so XS
objects compiled for one interpreter cannot leak into another run.

Add focused characterization cases for root capture, repeated `Out()`,
mutation between calls, Names ordering, output contexts, custom printers,
and relevant subclass behavior. Inventory object-hash fields read by
tests and diagnostics.

Record the minimum supported Perl decision. Keep inherited defects
separate from conversion regressions. Completion requires reproducible
baseline commands and a feature-to-test map.

## Task 2 Define and implement engine ownership

Define C context structures, root ownership, catalogue ownership,
operation cleanup, and the facade attachment. Implement construction,
reset, destruction, and a working-context guard for recursive calls.
Decide and test thread-cloning behavior.

Exercise constructor failure, explicit reset, repeated destruction paths,
and callbacks that release the Perl variable holding the dumper. Keep
the old emitter active while validating this foundation.

Completion requires clear lifetime contracts and tests showing partial
operations can unwind without invalid memory access or a corrupt reusable
object.

## Task 3 Implement the pointer table and ordinary analyzer

Adapt Sereal's pointer table with attribution. Define candidate records
for scalar-slot and referent roles. Implement the conservative candidate
predicate, weak-backreference exception, and singleton handling.

Implement depth-ordered traversal with FIFO ties for scalars, references, arrays, and
hashes. Register canonical names and minimum depths. Preserve built-in
key order. Keep transient queue paths distinct from persistent candidate
records.

Provide development-only counters and a catalogue inspection view for
comparison with the existing analyzer. Counters cover values visited,
identity lookups, candidates inserted, repeat encounters, queue high
water mark, and fixups.

Completion requires agreement on canonical locations for roots,
diamonds, aliases, forward references, and cycles. Ordinary scalar leaves
with refcount one must produce no identity lookup. Elevated external
ownership must produce candidates without inventing sharing.

## Task 4 Implement one complete emission slice

Add internal output buffering, basic quoting and names, ordinary
scalar/array/hash emission, explicit construction states, and typed
assignment/alias fixes. Wire the slice through an internal development
entry point before replacing the public path.

This slice must round-trip ordinary trees, DAGs, scalar aliases, references
to array/hash slots, multiple roots, self-cycles, and mutual cycles.
It must preserve canonical breadth-first locations rather than defining
everything at its first DFS occurrence.

Completion requires exact output checks and independent identity/mutation
checks. Review this slice before dividing the remaining implementation
into smaller tasks: it establishes the engine's actual interfaces.

## Task 5 Port syntax and formatting features

Port indentation modes, key alignment, built-in sorting, separators,
terse/declaration/purity options, readonly values, dualvars, numeric and
string edge cases, regexes, and array run-length encoding.

Port compression integration and scalar/list/direct output contexts.
Implement custom-printer behavior after its callback and chunking
contract is covered by characterization tests.

Completion requires the relevant existing expected-output tests and
semantic round trips, including mutation checks that prevent run-length
encoding or formatting from merging scalar identities.

## Task 6 Port finalization and callback behavior

Add complete weak-reference handling, blessing and overload-safe
inspection, restricted hashes and hidden keys, custom sorters,
freezer/proxy/thaw/postdump behavior, Ignore, and output callbacks.

Integrate keepalive and container-revalidation handling only at actual
callback boundaries. Include croak cleanup and recursive use of the
same dumper object. Define how catalogue state is kept valid across
separate calls and live-root changes.

Completion requires callback failures and mutations to leave memory
safe and the object reusable according to the specified API. Weak-only
graphs and referenced weak slots must follow DDS's policy.

## Task 7 Port globs code and closure handling

Port named/anonymous glob reconstruction and scalar, array, hash, CODE,
FORMAT, and IO slot handling. Preserve localized-glob behavior and
relevant symbol-table interactions.

Integrate B::Deparse and the DDS lexical-name override. Discover closure
roots with direct C inspection where practical; preserve shared pad
values, collisions, nested closures, and analysis restarts. Retain
DumpLex's optional PadWalker behavior.

Completion requires the glob, stash, lexical, code-stub, and overload
cases, plus independently evaluated reconstructed closure behavior.

## Task 8 Switch the public engine and remove duplication

Route Dump/Data/Out through XS while preserving context and method
chaining. Complete diagnostic projections or adapt repository tests
deliberately if internal fields change.

Remove the old Perl traversal and emission implementation and dependencies
that are no longer used. Keep documented reconstruction helpers and
source-generation helpers. Update POD, build metadata, and distributed
source lists; regenerate generated C using the normal XS toolchain.

Completion requires the full DDS suite on the supported interpreter
matrix, loading from an isolated built distribution, and execution of
generated dumps using the documented imports. No runtime fallback to
the old engine remains.

## Task 9 Measure and optimize

Compare the old and new DDS engines on the same non-debug interpreter,
options, and data. Use the refcounted-stack debug build for correctness
and memory checks. Report analysis and emission separately as well as
end-to-end throughput, output size, identity-table size, and memory use.

Investigate the measured costs of persistent names, BFS queue paths,
table entry allocation, key sorting, buffer growth, and fixup flushing.
Make each optimization independently reviewable and rerun the affected
semantic cases.

Completion requires measurements that support the claimed improvements
without reducing identity fidelity or changing unsupported portions of
the public API.

## Implementation ownership

The user chose one primary implementer. No delegated implementation was
started. The interfaces and tests were developed together in reviewable
slices rather than splitting ownership across agents.

## Review boundaries

The initial plan was reviewed before implementation. The implementation
sequence remains above as the review checklist; current evidence and
remaining limits are recorded below and in evidence.md.

Keep planning updates in this directory. Put product documentation and
tests in their normal distributed locations. Use the repository's
`git-compose-commit` workflow when commits are requested or created.

## Implemented status

| Task | Implementation and evidence |
| --- | --- |
| 1 Baseline | Isolated original builds on 5.44.0 and RC development Perl |
| 2 Ownership | Magic-owned C context, save-stack cleanup, callback guards, reentrant working facade, CLONE_SKIP |
| 3 Analysis | Native pointer table, weak backreference exception, depth buckets with FIFO ties, sparse leaf counters |
| 4 Emission | Heap continuation frames, separate slot/referent roles, marks and typed fixes |
| 5 Syntax | Existing formatting suite plus regexp interpolation and Unicode flag regressions |
| 6 Finalization | Deferred weakening, freezer/thaw/postdump, overload-safe inspection, tied values, callback failures |
| 7 Code/globs | Existing closure/glob/stash suite, direct CV pad inspection, B::Deparse helpers |
| 8 Public switch | Mandatory XS path; old Perl engine and unused dependencies removed; lazy diagnostics |
| 9 Measurements | Same-interpreter paired analysis/output timing; fixtures and peak RSS recorded in evidence.md |

Unicode scalar flags are included in this release, independently tested
in t/utf8_roundtrip.t. They are no longer a deferred TODO.

CI is configured for stable series 5.22 through 5.44, development RC stack,
and ithreads. Remote CI and platforms not present locally remain checks
for the eventual pushed branch; do not report them as locally executed.

## Follow-up: remove the remaining Perl facade

Implemented at the user's request on xs-engine. src/dds_api.c owns the
native facade. Removed the Perl constructor, option/accessor/autoload
implementations, Dump/Data/Out wrappers, duplicate built-in sorters,
freezer/compressor/postdump dispatch wrappers, and printer module.

Retained Perl is limited to loading/exports, external-module integration,
and developer diagnostics. The public API's context and subclass behavior
have dedicated native API tests. The existing overload test now loads its
own overload dependency at compile time; it no longer relies on DDS doing
that as a side effect before the test's BEGIN block runs.

Before release, review the C changes and remote CI results, broaden
benchmarks to realistic large graphs and callback-heavy inputs, and
consider additional adversarial magic/reentry coverage. This is a pushed
implementation checkpoint rather than a published CPAN release.

## XPerl class storage

Use `class_object_to_hash` only when the public helper and class-storage
magic are present. The owned shallow snapshot shares field slots with the
source object. Remove its `$self` entry and keep the ordinary payload in
the existing graph traversal. Catalogue addresses remain lookup keys.

Each snapshot gets a private variable whose name avoids all named roots,
including closure roots after the analysis restart. Analyse snapshot
fields with the ordinary breadth-first queue and emit them after the
public roots. An inner lexical block contains helpers and graph fixups.
Declare requested roots outside that block so they remain accessible.
Terse class dumps use an enclosing do expression to return the roots.

Class hydration is a typed fixup. It waits for pending scalar/reference
replacements and alias_av/alias_hv calls affecting its snapshot or the
canonical object's path. Alias calls retain an explicit destination path,
separate from their printable argument expression. A pending-alias counter
avoids scanning the fixup queue when no replacements remain. Ordinary
thaws can complete prerequisite fields. Thaws on class payloads wait for
that payload's storage to be restored. Weakening follows graph fixups.

The helper restores storage onto the reconstructed blessed payload, which
preserves object identity and bypasses constructors. Field snapshots leave
scope after fixups. Explicit freezer replacements retain precedence.
The public HvENTRY_* traversal macros adapt key collection and hidden-key
collection to XPerl's compact hashes without disturbing each() state.

## Reference emitter readability follow-up

Completed the requested pre-release split of dds_step_ref. Resolution and
canonical selection operate on borrowed step-local state; construction
and completion share the existing frame fields across child traversal.
The coordinator handles initial dispatch and scalar-reference completion
without restoring a second set of local continuation variables. Existing
ownership and generated statement order are preserved.

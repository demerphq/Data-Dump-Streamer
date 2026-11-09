# DDS XS engine design

## Required behavior

DDS emits Perl code that reconstructs values and graph relationships.
Its engine must distinguish equal values, two scalar slots containing
the same reference, and two locations that alias the same scalar SV.
Cycles and forward references require construction to happen in a
valid order, followed by explicit fixups where a single expression
cannot express the original relationship.

Preserve the public calling conventions and context behavior of `Dump`,
`Data`, and `Out`, including delayed analysis after scalar-context
`Dump($value)`. Preserve naming, declaration, purity, formatting, sorting,
and reconstruction helpers unless a specific compatibility change is
identified and agreed separately.

An isolated copy of the original Perl engine is the migration oracle.
The final distribution has one mandatory XS engine. It may retain Perl
export glue, option wrappers, and source-generation helpers, but graph
traversal, identity decisions, formatting loops, and fixup scheduling
belong in C.

## Engine boundaries

Use a small XS binding layer over C functions that receive interpreter
context explicitly. Follow Sereal's `PERL_NO_GET_CONTEXT` and
`pTHX_`/`aTHX_` conventions where supported by the chosen Perl floor.

Source responsibilities are split between the XS entry point and native C files:

| Component | Responsibility |
| --- | --- |
| `Streamer.xs` | Entry points, magic attachment, lexical inspection, and reconstruction utilities |
| `dds_engine.h` | C structures, state enums, ownership contracts, and interfaces |
| `dds_analyze.c` | Breadth-first scan, candidate registration, and closure-root discovery |
| `dds_emit.c` | Depth-first emission and dependency-driven fixups |
| `dds_format.c` | Names, quoting, numeric output, layout, and built-in key ordering |
| `dds_engine.c` | Owned context, native pointer table, callback guards, and operation cleanup |
| `dds_api.c` | Native construction, option methods, entry points, and reentrant facade selection |

Keep C sources in src/. MakeMaker copies translation units into the build
directory and uses its standard platform compiler rules. Explicit object
dependencies include the shared engine and portability headers.

## Object ownership and lifetime

Preserve the blessed hash facade initially. Attach one owned C context
through extension magic, with one authoritative free hook. The context
owns configuration, captured roots, a completed catalogue, and reusable
allocation capacity. Keep any diagnostic hash projections separate from
the engine's working state.

Define these lifetimes explicitly:

| State | Lifetime |
| --- | --- |
| Configuration and output destination | Until replaced or the object is destroyed |
| Captured root scalar identities | Until new input replaces them, reset, or destruction |
| Candidate catalogue and proxy relationships | Through analysis and subsequent emission, until invalidated or reset |
| BFS queue and temporary paths | One analysis operation |
| Construction states and pending fixes | One emission operation |
| Temporary one-shot object | The complete functional call |

Capturing roots must retain the original scalar SVs, not copies of their
values: root aliases and later changes to a captured variable are
observable in the existing API. Perform any boundary bookkeeping in one
place. Do not compensate for Perl calls by changing input refcounts.

Borrow descendant pointers in the ordinary traversal. References owned
for root retention, proxies, callbacks, and output are normal lifetime
management, and must be documented separately from refcount-based
classification.

Register per-operation cleanup with Perl's save stack before allocating
operation-owned resources. Successful cleanup must preserve a catalogue
that `Data()` has committed for a later `Out()` call. A callback croaking
must release partial state without leaving an object apparently ready
to emit an incomplete graph.

Use a separate working context for recursive use of the same object,
following Sereal's busy-context pattern. The outer operation's catalogue,
queue, and output must remain intact. Specify destruction and thread
cloning behavior before enabling opaque C ownership; a raw pointer must
never be copied into a second interpreter as an independent owner.
`CLONE_SKIP` excludes dumper objects; child interpreters construct their own.

## Sparse identity tracking

The candidate predicate is a conservative filter, not a sharing test:

```text
ordinary candidate = strong refcount > 1 OR has weak backreferences
```

Roots, magic, globs, freezer replacements, and closure roots can need
explicit registration independently of that fast path. Treat Perl's
immortal singleton values separately so shared constants do not become
accidental user-visible aliases.

Read counts before creating references or keepalives for the inspected
item. The ordinary analyzer queue borrows live C pointers without
incrementing the input SV's refcount. The identity table stores integer
addresses as opaque keys and never converts them back to SV pointers.

For an ordinary value excluded by the predicate, neither pass needs an
address lookup. The analyzer must still follow references and visit the
children of containers: an unshared array can contain shared scalars or
cycles elsewhere in the graph.

Refcount greater than one remains sufficient to register a candidate
even if it is encountered only once. External references and interpreter
ownership can produce these false positives safely. Repeated addresses
within the scan establish the relationships the emitter must preserve.

Use a native pointer table mapping `SV *` to compact C records. Allocate
the table lazily. Start with Sereal's tested pointer-table implementation
and preserve attribution. Optimize insertion or entry allocation only
after measuring the actual DDS workload.

Each candidate record needs identity, canonical depth, canonical name,
encounter information, and role flags. Emission adds explicit construction
state and pending-fix dependencies. Read type, class, scalar flags, and
ordinary values directly rather than reproducing the current parallel
Perl arrays as a C-shaped copy.

Scalar-slot and referent identities are logically distinct roles, even
though both are SV pointers and the same SV can appear in both roles.
A single pointer table may serve both if its record represents both
roles correctly. Specify the expression and depth rules for each role
before implementation; do not infer aliasing merely from equal `SvRV`
pointers.

## Breadth-first analysis

Register all roots in caller order, including supplied names and
dereferenced-root requests. Seed a worklist ordered by numeric depth,
with FIFO ordering within each depth. Glob slots add a different depth
offset from ordinary container children, so a single FIFO can visit a
deeper path first. Depth buckets preserve linear worklist operations.
Queued container
descriptors carry the pointer, depth, and path information needed to
name any interesting children.

For each item:

1. Apply the appropriate ordinary or special inspection path.
2. Register a candidate's first canonical occurrence, or record its
   repeated occurrence. Expand an already expanded identity only once.
3. Enumerate its outgoing edges in the same effective order as DDS.
4. Construct persistent child names only when registration requires
   them; retain enough transient path information to traverse unshared
   containers that have interesting descendants.
5. Queue children without copying their values or creating Perl RVs.

Minimum depths use DDS's existing scalar and referent depth convention,
not a newly invented graph distance. Breadth-first discovery determines
the shallowest eligible definition location. Preserve root preferences,
ties, glob paths, supplied names, and closure-name collision rules.

Emission compares the current expression with the canonical expression
to decide which occurrence constructs a referent. Glob analysis depths
and deferred-emission frame depths differ, so numeric emission depth
cannot substitute for that comparison. A noncanonical glob slot waits
for the canonical construction and is assigned from it; it must never
emit a body under another slot's name.

Store canonical names at registration in the first implementation.
Do not select them later in the DFS pass. Compact path descriptors can
replace stored strings later if they preserve those choices and reduce
measured cost.

Built-in key ordering belongs in C. User sorters remain explicit
callbacks with the existing analysis/emission pass argument. Preserve
their ability to return different orderings for the two passes.

Closure discovery may add roots and rename existing roots. Retain DDS's
discovery/reanalysis behavior until the closure tests demonstrate an
equivalent smaller algorithm. The overall design has two main passes;
that does not prohibit an analysis restart after additional roots are
discovered.

## Depth-first emission and fixups

Emit from the live graph using the completed catalogue. Use explicit
states such as `UNSTARTED`, `BUILDING`, and `AVAILABLE`; define availability
precisely for each construction form. An allocated container can accept
some assignments before all of its contents are complete.

Use an explicit C frame stack for potentially deep traversal, with a
defined depth limit and useful failure behavior. Moving Perl recursion
into unbounded C recursion would introduce a new stack-overflow risk.
The emitter now uses reusable heap continuations for scalar, referent,
array, and hash visits. A depth above 16384 croaks; scalar and referent
visits each count toward that bound. A 1200-container chain exercises
this independently of Perl parser depth during eval.

When visiting an occurrence, determine separately whether it is the
canonical definition location, a scalar alias, a reference to a scalar,
or a reference sharing an existing referent. Emit the canonical form or
a placeholder and typed fixup as required by purity and declaration
options.

Represent fixes as typed C records. Initial fix kinds correspond to the
existing engine: root declarations, scalar assignments, referent
assignments, array/hash aliases, root aliases, glob slots, readonly or
locking operations, blessing, weakening, and thaw actions.

Each fix records the construction events it requires. Flush ready fixes
after the same root boundaries as DDS and during glob expansion.
Preserve stable ordering when multiple fixes become ready together.
Validate ordering against current expected output before replacing the
existing scheduler with a more aggressive dependency implementation.

Assignment availability, alias installation, and finalization are
different events. Some alias operations require the destination
container, some require a source scalar, and readonly or locking actions
must not prevent later required writes. Blessing and thaw hooks can run
user code and therefore require their established ordering.

Weakening occurs only after the needed identity and ownership links
exist. In particular, test weak-first, strong-first, and weak-only graph
shapes. Preserve DDS's established weak-only result; Sereal's choice to
promote an unsupported weak-only referent is a separate binary-format
policy, not an instruction for DDS.

Finish emission with either every required fix resolved or a useful
diagnostic. Do not silently return code containing unresolved placeholders.

## Output and scalar fidelity

Use C buffer append functions for internal string/list output. Keep list
return behavior and direct output behavior compatible with `Out()`.
For `To($object)`, preserve its `print` method contract and observable
chunking until tests establish that larger writes are compatible.

Adapt current DDS quoting rules and use Data::Dumper's C implementation
as a reference. Preserve byte lengths, UTF-8 flags, embedded NULs, key
quoting, integer signedness, floating-point precision, and dualvars.
Include modern boolean and vstring behavior in characterization tests
before deciding whether their representation needs a deliberate change.

Port indentation, key alignment, built-in key ordering, array run-length
encoding, and compression hooks without merging distinct scalar
identities. Equal text is not evidence that values can be aliased.

## Magic and callbacks

The ordinary fast path does not invoke Perl. Magic, ties, sorters,
freezers, deparsing, compressors, custom printers, and thaw/postdump hooks
cross an explicit callback boundary.

Borrowed pointers and hash iteration state cannot be assumed valid
across callbacks. Study Sereal's keepalive and traversal-frame handling;
retain values where required and reacquire container storage after user
code. Classification based on raw counts must account for DDS-owned
keepalives rather than treating them as new graph edges.

Characterize mutation between `Data()` and `Out()` separately from
mutation during a callback. Preserve live-root behavior; never claim
that retained roots create an immutable snapshot. Decide catalogue
validation/reanalysis rules from those tests. Callers must not mutate the
graph during dumping and must call Data again after structural changes.
Otherwise reconstruction is best effort. A stale catalogue address must
never be dereferenced, including in callback guards or diagnostics.

Inspect blessed values directly without temporarily reblessing them
to suppress overload. Use explicit stringification only where the
selected feature calls for it.

## Code references and reconstruction utilities

Retain B::Deparse as a helper for CODE and FORMAT source generation.
Port closure identity and captured-value discovery to C where practical;
avoid carrying the old `op_private & 128` assumption into the new
scanner. Keep lexical naming and shared pad state compatible with DDS's
deparser integration.

Audit and retain the XS utilities used by generated dumps, including
`alias_av`, `alias_hv`, `alias_ref`, `make_ro`, locking helpers, `dualvar`,
and `weaken`. Replacing the dumper does not remove the need to execute
its generated reconstruction code correctly on supported Perls.

## Decisions to settle before the relevant task

| Decision | Timing |
| --- | --- |
| Minimum supported Perl and compiler baseline | Before selecting C APIs and simplifying compatibility code |
| Supported direct access to object hash fields | Before changing facade layout or diagnostics |
| Mutation and repeated `Out()` semantics | During baseline characterization, before committing the catalogue lifecycle |
| Thread cloning policy | Before exposing C-backed object ownership |
| Custom-printer chunk compatibility | Before output batching |
| Exact output changes needed for correctness fixes | Individually, with regression cases and an explanation |

These are review points within the implementation sequence, not a reason
to begin a different serializer or to postpone writing the ordinary
two-pass engine once its contracts are settled.

## Decisions confirmed during implementation

Candidate records store opaque integer addresses, never borrowed or weak
references to the corresponding input SV. A traversal obtains an SV from
the live graph and looks up its address. Catalogue cleanup, rehashing and
diagnostics use stored metadata without touching input values. Callback
guards retain active frames and pending work, never catalogue entries.
Deferred glob fixups own their glob explicitly. Freezer replacements have
explicit registration because a single owned proxy RV may represent
multiple occurrences of the original object.

The earlier weak-witness design was rejected: it attached back-reference
magic to input values, including read-only scalars and arrays. Mutation
between analysis and output receives best-effort handling, not a promise
that SV address reuse or changed canonical locations will be detected.

Callbacks pin active values and pending work before Perl code can mutate
the graph. Native frame storage remains protected by operation cleanup
when a callback croaks. Custom key lists are copied before printing can
invoke further callbacks. Tied scalar FETCH and tied container access use
the callback path; ordinary containers do not.

Finalization drains newly ready fixups to a fixed point. Weakening is a
separate final phase after strong links, thawing, and other construction
fixups. The existing weak-output snapshot changes only the placement of
three weaken calls, with independent weak-first/strong-first assertions.

Root replacement triggers reanalysis. After changing sharing or moving
canonical descendant locations between calls, callers should explicitly
run Data again. Unresolved fixups raise an actionable exception. Repeated
Out starts fresh construction state instead of retaining completion marks
from a preceding output operation.

Diagnostics project the native table lazily through `_xs_catalogue`; the
engine does not rebuild the former Perl catalogue during normal dumping.
`_xs_stats` exposes visits, lookups, candidates, repeats, queue peak, and
fixups for development tests and benchmarks.

Unicode output is included in the conversion release, with a separate
regression file. ASCII/Latin-1 Unicode values use an initial `\N{U+...}`;
an empty Unicode scalar uses `substr("\N{U+100}",0,0)`. Wide character
escapes already force Unicode. The dualvar reconstruction helper also
copies the string's UTF-8 flag.

Regexp output follows the supplied Data::Dumper sigil patch: interpolate
the complete unescaped sigil plus opening-bracket run as a quoted literal.
Backslash pairs stay together. Also protect a dollar before backslash,
which otherwise interpolates Perl's output record separator. Extended
patterns containing control whitespace compile a separately quoted pattern
string so `/x` comment and whitespace semantics remain intact.

## Native API and callback boundary

Public constructor, options, Dump/DumpVars/DumpLex/Dumper, Data, and Out
are XSUBs registered by dds_api_boot. Method aliases carry small integer
selectors in CvXSUBANY, with no interpreter pointer stored in a CV. Class
calls respect overridden subclass constructors. Optional PadWalker name
lookup and B::Deparse source generation stay behind small Perl helpers.

The ordinary built-in key order has one C implementation. Custom sorters
retain their CV or explicit array; the engine calls them directly through
call_sv. Freezer discovery, caching, fallback, compression callback
invocation, and postdump dispatch also use the C callback boundary.

Postdump work is queued once per original object encountered by the live
traversal, including each link in a proxy chain. Each task owns an RV to
the already-fetched referent; copying the source scalar would invoke get
magic again and could select a different object. Tasks borrow stable C
catalogue records and never recover an SV from an address key. Drain the
queue after graph serialization and fixups, so restoration hooks cannot
change a proxy that is still awaiting output. Normal draining and error
cleanup release every task's invocant before catalogue destruction.

Callback arguments are retained before stack extension can relocate their
slots. Returned values and trapped exception objects have ownership that
survives temporary cleanup. Option setters retain their receiver while a
magical option value can release the caller variable. Out localizes print
globals on the save stack and restores them after callback completion.

Untied key collection walks native buckets without invoking callbacks,
so it preserves a caller's each iterator and pending lazy deletion.
Tied hashes still use their own FIRSTKEY/NEXTKEY protocol. No attempt is
made to save and restore a tied hash's externally implemented iterator.

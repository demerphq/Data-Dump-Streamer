# DDS XS migration planning

Convert DDS's graph analysis and Perl code generation to a mandatory XS
engine. Preserve its breadth-first analysis followed by depth-first
emission, while using direct C inspection to avoid unnecessary identity
tracking and Perl calls during traversal.

The implementation is on branch `xs-engine`. This directory records
the design, implementation decisions, and validation evidence. New source
and test files are staged immediately so git clean cannot remove them.

## Documents

| Document | Purpose |
| --- | --- |
| [design.md](design.md) | Architecture, traversal invariants, names, fixups, and object lifetime |
| [implementation.md](implementation.md) | Ordered implementation tasks, dependencies, and completion criteria |
| [validation.md](validation.md) | Compatibility coverage, graph correctness, and performance measurements |
| [evidence.md](evidence.md) | Existing source references, interpreter setup, and investigation results |

Read the design first, then the implementation sequence. The validation
document defines the evidence required before replacing the existing
engine.

## Agreed direction

- Keep DDS's two passes and its choice of canonical locations based on
  breadth-first discovery.
- Treat refcount greater than one as eligibility for tracking. Repeated
  addresses within the traversal establish actual sharing; external
  owners can raise a count without introducing sharing inside the dump.
- Record identity, minimum depth, and canonical name for candidates.
  Include targets with weak backreferences even when their strong
  refcount is one.
- Avoid address hashing for ordinary unshared values. Reading an SV
  through a borrowed C pointer does not increment its refcount.
- Preserve the distinction between scalar aliases and shared referents.
- Use Sereal as a reference for pointer tables, XS object structure,
  cleanup, and direct traversal. Use Data::Dumper as a reference for
  emitting Perl syntax. DDS retains its own code-generation algorithm.
- Deliver one XS dumping engine, with no pure-Perl fallback. Perl API
  glue and helpers such as B::Deparse remain possible.

## Implementation decisions

Store canonical names when candidates are registered. Defer compact path
descriptors until measurements show a reason to add them.

Preserve the blessed hash object initially and attach an owned C context.
This preserves the public object shape while construction, options, and
dumping entry points run in XS. Engine traversal must not use the Perl
hash as its working catalogue.

Keep Module::Build, with C sources under `src/` and XS bindings under
`lib/Data/Dump/`. Perl 5.22 is the supported minimum. CI covers each stable
series from 5.22 through 5.44, development Perl 5.45.4 with the refcounted
stack, and a separate threaded 5.44 build. The version matrix runs in CI;
local correctness checks use 5.22.4, 5.44.0, and the development build.

## Current validation status

- Perl 5.45.4 was built and installed through perlbrew with
  `PERL_RC_STACK`, `DEBUGGING`, and `DEBUG_LEAKING_SCALARS`.
- Its core suite passed: 2,960 files and 1,395,709 tests.
- The original engine passed 375 tests on Perl 5.44.0. On the development
  interpreter, three raw-refcount assertions needed the stack contribution;
  dump behavior passed.
- The new engine has separate graph, randomized topology, regexp, and
  Unicode regression tests. See evidence.md for final counts and commands.
- Ordinary singleton descendants avoid identity-table lookups. A tree
  containing 100 unshared arrays has fewer than 10 candidate records.
- Unicode round trips belong to this release. Their independent suite
  checks contents and UTF-8 flags, including empty strings and dualvars.

## Repository boundaries

`planning/` is intended to be tracked in Git. It is excluded by
`MANIFEST.SKIP` and is not listed in `MANIFEST`, so it is outside release
distributions. `INSTALL.SKIP` also names it explicitly; the actual
Module::Build installation boundary comes from discovering modules, POD,
and XS under `lib/` and installing from `blib/`.

Module::Build's default test discovery uses `t/` and an optional root
`test.pl`. The planning directory is outside those paths. Keep runnable
regression tests under `t/`, Perl/XS modules under `lib/`, and linked C
sources under `src/`. The C sources ship but are not installed as modules.

Do not add planning files to `MANIFEST`, `.gitignore`, installation maps,
or test globs. Keep this directory documentation-only so repository-wide
Perl formatting commands also leave it alone.

## Native facade checkpoint

The user requested removing most of the remaining Perl implementation.
Construction, option methods, Dump/DumpVars/DumpLex/Dumper, Data/Out,
built-in sorting, and callback dispatch now run in C. Perl retains loading
and exports, B::Deparse/PadWalker integration, compression helpers, and
small diagnostic routines. There is no PP dumping backend, duplicated
Perl sorter implementation, or Perl printer class.

The main module has about 360 lines before POD, down from 1123 non-POD
lines at the first XS checkpoint. The current suite contains 4180 tests
and passes locally on 5.22.4, 5.44.0, and the RC development interpreter.

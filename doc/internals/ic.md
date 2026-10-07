# Incremental compilation

`nimony c` runs two `nifmake` builds over files in the nimcache: the
frontend (parse, sem) and the backend (hexer, DCE, codegen, C compiler,
link). Everything that makes a rebuild cheap follows from a handful of rules
about which files a node writes and which files a node depends on. This
document states them.

## How nifmake decides

nifmake knows files, nothing else. A node runs when

* one of its outputs is missing, or
* one of its inputs is newer than its *freshest* output.

It never looks at a tool's flags or at file contents. Two consequences
shape everything below: what a tool does to an output's mtime is the only
way it can tell nifmake "this did not change", and an option that is not a
file is invisible to it.

## Rule 1: a node always writes its outputs

Every node rewrites its main output on every run, even when the bytes come
out the same. That is what records "this node ran since its inputs last
changed": the node is up to date afterwards, whatever it produced.

The opposite policy, keeping the old mtime when the content is unchanged,
looks like it saves work downstream, and it does, once. But the node's input
is now newer than its output for good, so the node itself runs again on
every later build, finds the same content again, and keeps the old mtime
again. A backend switch, an option change or a `touch` was enough to leave
nodes in that state indefinitely.

## Rule 2: sparing goes through interface files

Not rerunning the modules that use a changed module is the job of
**interface files**: small files next to a main output that hold exactly the
part of it that OTHER modules read. They are the one exception to rule 1:
written only when they change, so their old mtime spares everything that
depends on them. A node that writes one also writes a main output, so by
rule 1 it is still up to date after it ran (nifmake compares against the
freshest output).

The other modules depend on the interface, never on the main output.

| main output | interface | written by | read by other modules' |
|---|---|---|---|
| `M.s.nif` | `M.s.idx.nif` | `nimsem` | `nimsem`, `hexer` |
| `<main>.dce.nif` | `M.live.nif` (one per module) | `dceLive` | `dceEmit` of that module |
| `M.c.nif` | `M.c.idx.nif` | `dceEmit` | `lengc`, `arkham` |
| `M.oc.nif` | `M.oc.idx.nif` | `optimize` (Shoggoth) | `lengc`, `arkham` |

* `M.s.idx.nif` is the module's index plus a checksum of its interface and
  of the bodies of its `.inline` procs (`nifindexes.createIndex`). A changed
  non-inline proc body leaves it alone, so no importer is re-semmed.
* `M.live.nif` is the module's share of the whole-program DCE result.
  `dceLive` reads every module's `.x.nif` and runs whenever one of them
  changed; the live files it writes change only for the modules whose live
  set did. `<main>.dce.nif`, a summary written on every run, is the node's
  main output.
* `M.c.idx.nif` and `M.oc.idx.nif` hold a digest (`lib/lengiface.nim`) of
  everything in the Leng file except the bodies of non-inline procs: types,
  globals, constants, string literals, proc signatures and the bodies of
  `.inline` procs, which `lengc` emits into every translation unit that calls
  them. That is what `lengc` and `arkham` read from another module, by
  loading the declaration of a foreign symbol out of its owner's file. Every
  codegen node depends on every module's interface (`(inputsof dceEmit
  ".c.idx.nif")`), because the owner of a symbol, a deduplicated string
  literal for example, is not predictable from the import graph.

Everything else is a main output and always written: `.p.nif`,
`.p.deps.nif`, `.s.nif`, `.s.deps.nif`, `.x.nif`, `.c.nif`, `.oc.nif`, `.c`,
`.ll`, `.asm.nif`, objects, executables.

What a change costs, then:

* `touch`, or an edit that changes nothing sem sees: the module's own chain
  runs once (sem, hexer, emit, codegen, C compiler, link); its interface
  files spare every other module.
* A changed non-inline proc body: the same, one translation unit.
* A changed `.inline` body or interface: every importer is re-semmed, and
  every module that reads the changed declaration is compiled again.

## Rule 3: the driver's own files are sources

Files that nimony writes *before* it runs a build, as opposed to files a node
writes, are sources to nifmake. They are written only when they change, like
a programmer saves a file only when it was edited: the link manifest
(`<main>.linkmanifest.nif`) and Nim's imported configuration (`.cfg.nif`).

The driver also parses each module before the frontend build, to discover
the import graph. It compares mtimes at nanosecond resolution, as nifmake
does; in whole seconds, a source written in the same second as its `.p.nif`
would look older than it and be parsed on every build.

## Rule 4: options are memos

Defines, `--opt`, `--cc`, the target and so on change what the tools
produce, and nifmake cannot see them. The driver keeps a **memo** of the
options a stage's artifacts were made with and passes `--rerun` to that
stage's nifmake when they differ. `--rerun` runs every node of the build but
leaves mtimes to the tools, so unchanged interface files still spare their
dependents.

* `<nimcache>/config.nif` is the frontend's memo: everything sem reads
  (defines, `--mm`, cycles, bits, cpu, os, `--app`, the C compiler's name,
  the check flags, `--inlineframes`). It is written before the frontend
  runs, because the sub-compiles sem starts (compile-time evaluation, the
  `writenif` helper) share the nimcache and must see the configuration the
  outer build is producing. If a build that reruns everything fails, the memo
  is emptied, so the next build reruns everything again instead of trusting
  half-remade artifacts.
* `<nimcache>/<backend>/<main>/config.nif` is the backend's memo: the
  frontend's key plus `--opt`, the C compiler and linker, `--passC`/`--passL`
  and `.passc` pragmas, `--layout`, `--browser`. Since it contains the
  frontend's key, a changed define reruns the backend too — even when the sem
  results came out the same, and even when another main module's build was
  the one that changed them. It is written after the backend succeeded.

Neither memo is an input of any node: an mtime cannot express "already built
against this", and a file that every node depended on kept nodes stale
whenever their outputs legitimately came out the same.

## The nimcache layout

```
<nimcache>/
  config.nif               frontend memo
  M.p.nif, M.p.deps.nif    nifler (shared by every main and backend)
  M.s.nif, M.s.idx.nif,    nimsem (shared by every main and backend)
  M.s.deps.nif
  <main>.build.nif         the frontend build
  c/  l/  n/  w/  j/       one directory per backend, named by its command
    M.x.nif                hexer, for every module but the main one
    <main>.final.build.nif the backend build
    <main>/                everything specific to this main module:
      config.nif           backend memo
      <main>.x.nif         hexer with --isMain
      <main>.dce.nif,      dceLive
      M.live.nif
      M.c.nif, M.c.idx.nif dceEmit
      M.oc.nif, ...        optimize, lengc/arkham, objects, executable
```

A backend's artifacts depend on the backend itself — hexer runs with or
without `--native` — and nifmake cannot see that. With one directory per
backend, switching from `nimony c` to `nimony n` and back finds each
backend's files as it left them, and a sub-compile with another backend in
the same nimcache (`nimony n` builds compile-time helpers with the C
backend) cannot replace the outer build's files.

## Known cost: the optimizer

Shoggoth's inter-module inliner and its function summaries read any proc
body of any module, so an `optimize` node depends on every module's full
`.c.nif`, not on its interface. With `--opt:speed`/`--opt:size` (and
`-d:release`), a changed proc body therefore reruns every `optimize` node,
and by rule 1 every module's `lengc` and C compiler after it.

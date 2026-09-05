## Which tests fold into a directory's single joined program, and what that
## program is expected to print. Running one is `runner.joinedTest`.

import std / [syncio, os, strutils, algorithm]
import category

const
  JoinedPrefix* = "_hastur_"
    ## Generated files carry this prefix; they are never tests themselves.
  JoinedDriver* = JoinedPrefix & "joined"
    ## Basename of the per-directory joined-group driver module.
  MinJoinGroup* = 2
    ## A group of one saves nothing and only obscures the reporting.

var joinTests* = true
  ## `--joined:off` restores one process per test (bisecting a group, or
  ## working on the joining itself). See the `joined tests` section below.

proc isGeneratedTestFile*(file: string): bool {.inline.} =
  file.splitFile.name.startsWith(JoinedPrefix)

proc joinable*(file: string; cat: Category): bool =
  ## A test folds into its directory's joined group when nothing about it needs
  ## a process of its own: it must be a plain compile-run-diff-output test (no
  ## expected diagnostics, no golden C or NIF to diff, no non-zero exit code, no
  ## valgrind run), it must not care about being the main module, and it must
  ## not opt out with a `.nojoin` sidecar. Everything else keeps its own run.
  ## Whether a group is actually formed is `joinMembers`' call — this is only
  ## about the test itself.
  if cat != Normal: return false
  if isGeneratedTestFile(file): return false
  for ext in [".msgs", ".nim.c", ".nif", ".exitcode", ".valgrind", ".nojoin"]:
    if file.changeFileExt(ext).fileExists(): return false
  # `isMainModule` is false for an imported member, so such a test would
  # silently exercise the other branch.
  result = not readFile(file).contains("isMainModule")

# ── joined tests ("megatest") ────────────────────────────────────────────────
# The dominant cost of the suite is not compiling test code, it is the process
# tree each test drags along: nimony spawns nifler/nimsem/hexer/lengc/nifmake,
# nifmake spawns a C compiler and the linker, and then the executable itself
# runs. That is a dozen-odd process creations per test — cheap on Unix, brutal
# on Windows, and paid ~500 times over.
#
# So a directory's plain tests are compiled into ONE program: a generated
# driver imports each of them, and module initialization (which is where a test
# file's top-level code lives) runs in import order. The program's output is
# therefore the members' outputs concatenated, which is exactly what the
# members' `.output` files already spell out — no new golden files, and every
# test stays individually runnable via `hastur test <file>`.
#
# Same idea as Nim testament's `megatest`, with one deliberate difference:
# testament interleaves marker modules (`echo "megatest:processing: ..."`)
# between the members to attribute output. Here a marker module would double
# the module count — and modules, not tests, are what the process spawns are
# proportional to — so instead a group that diverges *at all* is re-run
# member-by-member, which pins the blame precisely and costs nothing when green.

proc joinMembers*(dir: string; cat: Category; overwrite: bool): seq[string] =
  ## The joinable tests of `dir`, sorted — empty when this directory forms no
  ## group. Both the parent (planning the run) and the `joined` worker derive
  ## the member list this way, so they agree on what a group contains without
  ## having to pass it along.
  ##
  ## `--overwrite` never joins: regenerating a member's `.output` means seeing
  ## that member's output on its own.
  result = @[]
  if cat != Normal or not joinTests or overwrite: return
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim") and joinable(x.path, cat):
      result.add x.path
  sort result

proc joinedExpectedOutput*(files: openArray[string]): string =
  ## What the joined program must print: each member's `.output` in import
  ## order. A member without one is expected to print nothing — the same
  ## contract `testFile` enforces is absent there, made explicit here.
  result = ""
  for f in items files:
    let o = f.changeFileExt(".output")
    if o.fileExists():
      let spec = readFile(o).strip
      if spec.len > 0:
        if result.len > 0: result.add '\n'
        result.add spec

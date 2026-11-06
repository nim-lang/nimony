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

proc memberOutputSpec*(file: string): string =
  ## The raw contents of `file`'s `.output`, or "" without one: a member with
  ## no `.output` is expected to print nothing, as `testFile` checks.
  let o = file.changeFileExt(".output")
  result = if o.fileExists(): readFile(o) else: ""

type
  JoinedDiff* = object
    ## Where the joined program's output first parts from its members'
    ## `.output` files. `matchJoinedOutput` fills it.
    ok*: bool
    member*: int
      ## The member whose expected segment holds the difference; -1 when the
      ## output goes on past the last non-empty segment.
    lastSegment*: int
      ## The last member with a non-empty `.output` before the difference.
    silent*: seq[int]
      ## Members with no expected output just before the difference: any of
      ## them may have printed the unexpected text.
    expectedLine*, gotLine*: string
    expectedLineNo*: int  ## 1-based, in the member's `.output` file
    gotLineNo*: int       ## 1-based, in the program's output
    atEnd*: bool          ## the program's output ended at the difference

proc lineAround(s: string; pos: int): (string, int) =
  ## The line of `s` holding offset `pos`, and its 1-based number.
  var a = min(pos, s.len)
  while a > 0 and s[a-1] != '\n': dec a
  var b = min(pos, s.len)
  while b < s.len and s[b] != '\n': inc b
  var n = 1
  for i in 0 ..< a:
    if s[i] == '\n': inc n
  result = (s.substr(a, b-1).strip(leading = false, chars = {'\r'}), n)

proc matchJoinedOutput*(specs: openArray[string]; output: string): JoinedDiff =
  ## Whether `output`, what the joined program printed, is what its members
  ## print one after another, judged as `testFile` judges each member alone:
  ## member `i` passes when what it prints, stripped, equals `specs[i]` (its
  ## raw `.output`) stripped. So leading and trailing whitespace of each
  ## member's share is free; each stripped spec must appear verbatim, in
  ## order, with only whitespace between. A stripped spec starts with
  ## non-whitespace, so skipping whitespace before it finds exactly where it
  ## starts: the group passes exactly when `output` splits into consecutive
  ## shares that each pass their member alone. Concatenating the stripped
  ## specs instead loses a member's leading indentation, and a trailing blank
  ## line, on one side only.
  result = JoinedDiff(ok: true, member: -1, lastSegment: -1)
  var p = 0
  var silent: seq[int] = @[]
  for i in 0 ..< specs.len:
    let raw = specs[i]
    let spec = raw.strip
    if spec.len == 0:
      silent.add i
      continue
    while p < output.len and output[p] in Whitespace: inc p
    var q = 0
    while q < spec.len and p < output.len and output[p] == spec[q]:
      inc p
      inc q
    if q < spec.len:
      let lead = raw.len - raw.strip(trailing = false).len
      let (el, en) = lineAround(raw, lead + q)
      let (gl, gn) = lineAround(output, p)
      return JoinedDiff(ok: false, member: i, lastSegment: result.lastSegment,
        silent: (if q == 0: silent else: @[]),
        expectedLine: el, expectedLineNo: en, gotLine: gl, gotLineNo: gn,
        atEnd: p >= output.len)
    result.lastSegment = i
    silent.setLen 0
  while p < output.len and output[p] in Whitespace: inc p
  if p < output.len:
    let (gl, gn) = lineAround(output, p)
    result = JoinedDiff(ok: false, member: -1, lastSegment: result.lastSegment,
      silent: silent, gotLine: gl, gotLineNo: gn)

proc describe*(d: JoinedDiff; files: openArray[string]): string =
  ## Names the member whose expected segment holds the first difference, for
  ## the line before a group's per-test rerun.
  let got =
    if d.atEnd: "the end of the output"
    else: d.gotLine & "    (line " & $d.gotLineNo & " of the program's output)"
  if d.member >= 0:
    result = "  first difference: line " & $d.expectedLineNo & " of " &
      files[d.member].changeFileExt(".output") & "\n" &
      "    expected: " & d.expectedLine & "\n" &
      "    got:      " & got
  else:
    result = "  unexpected output after " &
      (if d.lastSegment >= 0: "the expected output of " & files[d.lastSegment]
       else: "nothing: no member has an .output") & "\n" &
      "    got:      " & got
  if d.silent.len > 0:
    result.add "\n  or printed by a member with no .output:"
    for i in d.silent: result.add " " & files[i]

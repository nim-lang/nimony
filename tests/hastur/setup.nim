## hastur's joined groups: joining must not change a member's verdict. A group
## whose members pass alone passes joined, without a rerun; a member that fails
## alone fails joined too.
##
## `matchJoinedOutput` is checked directly, then a freshly built hastur runs
## small groups end to end (`hastur joined <dir>`), so the test exercises these
## sources rather than whatever `bin/hastur` is.
import std / [os, osproc, strutils]
import "../../src/hastur/joined"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

var failures = 0

proc check(cond: bool; msg: string; detail = "") =
  if cond:
    echo "  ok: ", msg
  else:
    echo "  FAIL: ", msg
    if detail.len > 0: echo detail
    inc failures

# ---- the matcher ----------------------------------------------------------

check matchJoinedOutput(["first\n", "  indented\nsecond\n"],
                        "first\n  indented\nsecond\n"),
  "a member's leading indentation"
check matchJoinedOutput(["a\nb\n\n", "c\n"], "a\nb\n\nc\n"),
  "a member printing a trailing blank line"
check matchJoinedOutput(["a\nb\n\n", "c\n"], "a\nb\nc\n"),
  "a trailing blank line in a member's .output it does not print"
check matchJoinedOutput(["", "a\n", ""], "a\n"),
  "members without .output that print nothing"
check matchJoinedOutput(["a", "b"], "ab"),
  "a member that ends without a newline"
check matchJoinedOutput([], "\n"), "an empty group printing a blank line"
check(not matchJoinedOutput(["first\n", "x\ny\n"], "first\nx\nY\n"), "a wrong line")
check(not matchJoinedOutput(["first\n", ""], "first\nx\n"),
      "output from a member without .output")
check(not matchJoinedOutput(["a\n", "b\n"], "a\n"), "missing output")

# ---- `hastur joined <dir>` ------------------------------------------------

let bindir = absolutePath(if arg("bindir").len > 0: arg("bindir") else: "bin")
let cachedir = if arg("cachedir").len > 0: arg("cachedir") else: "nimcache"
let forward = arg("forward")
let work = absolutePath(cachedir / "hasturjoined")
removeDir work
createDir work

let hastur = work / "hastur".addFileExt(ExeExt)
let buildCmd = "nim c --hints:off --warningAsError:ProveInit:off" &
  " --warningAsError:Uninit:off --nimcache:" & quoteShell(work / "host") &
  " -o:" & quoteShell(hastur) & " src/hastur/hastur.nim"
let (buildOut, buildCode) = execCmdEx(buildCmd)
if buildCode != 0:
  quit "FAILURE: " & buildCmd & "\n" & buildOut

proc group(name: string; files: openArray[(string, string)]): string =
  result = work / name
  createDir result
  for (f, content) in files: writeFile(result / f, content)

proc runJoined(dir: string): (string, int) =
  var cmd = quoteShell(hastur) & " --bindir:" & quoteShell(bindir) &
    " --cachedir:" & quoteShell(dir & "-cache")
  if forward.len > 0: cmd.add " --forward:" & quoteShell(forward)
  cmd.add " joined " & quoteShell(dir)
  result = execCmdEx(cmd)

const
  ta = ("ta.nim", "import std/syncio\necho \"first\"\n")
  taOut = ("ta.output", "first\n")

block:
  # report 62, reproduction 1: an `.output` starting with indentation
  let g = group("indented", [ta, taOut,
    ("tb.nim", "import std/syncio\necho \"  indented\"\necho \"second\"\n"),
    ("tb.output", "  indented\nsecond\n")])
  let (o, code) = runJoined(g)
  check(code == 0 and "re-running" notin o,
        "indented .output: the group passes without a rerun", o)

block:
  # a member that is not the last prints a trailing blank line
  let g = group("blankline", [
    ("ta.nim", "import std/syncio\necho \"first\"\necho \"\"\n"),
    ("ta.output", "first\n\n"),
    ("tb.nim", "import std/syncio\necho \"second\"\n"),
    ("tb.output", "second\n")])
  let (o, code) = runJoined(g)
  check(code == 0 and "re-running" notin o,
        "trailing blank line: the group passes without a rerun", o)

block:
  # report 62, reproduction 2: a member that prints and has no `.output`
  let tb = group("missing", [ta, taOut,
    ("tb.nim", "import std/syncio\necho \"x\"\n")]) / "tb.nim"
  let (o, code) = runJoined(tb.parentDir)
  check(code == 1 and tb & " ---" in o and "has no tb.output" in o,
        "a member printing without .output fails", o)

block:
  let g = group("wrong", [ta, taOut,
    ("tb.nim", "import std/syncio\necho \"x\"\necho \"y\"\n"),
    ("tb.output", "x\nz\n"),
    ("tc.nim", "import std/syncio\necho \"last\"\n"),
    ("tc.output", "last\n")])
  let (o, code) = runJoined(g)
  check(code == 1 and (g / "tb.nim") & " ---" in o and
        "SUCCESS " & (g / "tc.nim") in o,
        "a wrong .output fails its member", o)

if failures > 0:
  quit "FAILURE: " & $failures & " hastur check(s) failed"
removeDir work
echo "hastur: joined groups passed"

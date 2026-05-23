## Integration test for `pnak fetch` end-to-end.
##
## Stays offline by spinning up a tiny local git repository as the
## "upstream" and pointing at it via a `file://` URL — so the test does
## not depend on network reachability or the cached packages.json. The
## assertions cover the two behaviours that broke historically:
##
##   1. The dependency BFS produces a `nimony.paths` file next to the
##      input `.nimble`, with the resolved package's `src/` directory
##      listed inside the pnak-managed block.
##   2. Re-running pnak is idempotent and preserves any user content
##      outside the managed block (the original `nim.cfg` patcher had a
##      regression where the marker line was clobbered).
##
## Run manually:
##   hastur build pnak                          # builds bin/pnak
##   nim c -r tests/pnak/tpaths_basic.nim       # builds and runs this test

import std / [os, osproc, strutils, syncio, assertions]

const
  PnakBin = "bin" / "pnak"

proc sh(cmd: string; cwd = "") =
  let final = if cwd.len > 0: "cd " & quoteShell(cwd) & " && " & cmd else: cmd
  if execShellCmd(final) != 0:
    quit "FAILURE: " & final

proc shCapture(cmd: string; cwd = ""): (string, int) =
  let final = if cwd.len > 0: "cd " & quoteShell(cwd) & " && " & cmd else: cmd
  result = execCmdEx(final)

proc setupFakeUpstream(dir: string) =
  ## Create a minimal git repository that pnak can clone via file://.
  removeDir dir
  createDir dir
  writeFile dir / "fakepkg.nimble", """
version = "0.1.0"
license = "MIT"
srcDir = "src"
"""
  createDir dir / "src"
  writeFile dir / "src" / "fakepkg.nim", "## fakepkg main module\n"
  # `-c user.*` keeps the test independent of the developer's global
  # git identity; `-b master` pins the default branch so gitSync's
  # `origin/<default>` lookup is deterministic.
  sh "git init -q -b master", dir
  sh "git -c user.email=t@example.com -c user.name=t add -A", dir
  sh "git -c user.email=t@example.com -c user.name=t commit -q -m init", dir

proc setupProject(projectDir, upstreamDir: string) =
  removeDir projectDir
  createDir projectDir
  let url = "file://" & upstreamDir.absolutePath
  writeFile projectDir / "myapp.nimble", """
version = "0.1.0"
license = "MIT"
requires "$1"
""" % [url]

proc readManagedBlock(path: string): seq[string] =
  ## Extract the lines between pnak's begin/end sentinels.
  result = @[]
  doAssert fileExists(path), "nimony.paths file not found at " & path
  var inBlock = false
  for line in lines(path):
    if line.startsWith("# >>> pnak begin"):
      inBlock = true
    elif line.startsWith("# <<< pnak end"):
      inBlock = false
    elif inBlock and line.strip().len > 0:
      result.add line.strip()

proc main() =
  let pnak = absolutePath(PnakBin.addFileExt(ExeExt))
  doAssert fileExists(pnak),
    "pnak binary not found at " & pnak &
    "; build it first with `hastur build pnak`"

  let testRoot = absolutePath("tests" / "pnak" / "_workdir")
  removeDir testRoot
  createDir testRoot
  defer: removeDir testRoot

  let upstream = testRoot / "fakepkg"
  let project = testRoot / "project"
  setupFakeUpstream upstream
  setupProject(project, upstream)

  # ── 1. First fetch ────────────────────────────────────────────────
  let (out1, code1) = shCapture(
    quoteShell(pnak) & " --offline --depsdir:deps fetch myapp.nimble",
    project)
  doAssert code1 == 0, "pnak fetch exit " & $code1 & "\n" & out1

  doAssert dirExists(project / "deps" / "fakepkg"),
    "deps/fakepkg was not created"
  doAssert fileExists(project / "deps" / "fakepkg" / "src" / "fakepkg.nim"),
    "src/fakepkg.nim missing from clone"

  let pathsFile = project / "nimony.paths"
  doAssert fileExists(pathsFile), "nimony.paths was not created"

  let managed = readManagedBlock(pathsFile)
  doAssert managed.len == 1,
    "expected exactly one path entry, got " & $managed
  doAssert managed[0] == "deps/fakepkg/src",
    "wrong path entry (srcDir should be honored): " & managed[0]

  # ── 2. Re-run preserves user content outside the managed block ────
  const
    userBefore = "# my pinned flags\n--mm:orc\n\n"
    userAfter = "\n# trailing user note\n"
  writeFile pathsFile,
    userBefore &
    "# >>> pnak begin (managed block — do not edit) <<<\n" &
    "deps/fakepkg/src\n" &
    "# <<< pnak end >>>\n" &
    userAfter

  let (out2, code2) = shCapture(
    quoteShell(pnak) & " --offline --depsdir:deps fetch myapp.nimble",
    project)
  doAssert code2 == 0, "pnak re-run exit " & $code2 & "\n" & out2

  let final = readFile(pathsFile)
  doAssert "--mm:orc" in final, "user content before block was clobbered"
  doAssert "trailing user note" in final,
    "user content after block was clobbered"
  doAssert readManagedBlock(pathsFile) == @["deps/fakepkg/src"],
    "managed block content drifted on re-run"

  echo "OK: pnak nimony.paths integration test passed"

when isMainModule:
  main()

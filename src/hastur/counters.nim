## Per-run test tallies and the failure reporting every suite funnels into.

import std / [os, osproc, strutils]

type
  TestCounters* = object
    total*: int
    failures*: int
    failed*: seq[string]
      ## Names of the tests that failed, in failure order. A run of several
      ## hundred tests streams far too much output for a bare "N / M" to be
      ## actionable — and under `--jobs` the child's own FAILURE block sits
      ## thousands of lines up, interleaved with every other worker's.
      ## `reportFailures` replays this list right above the summary.

proc noteFailure*(c: var TestCounters; file: string) =
  inc c.failures
  c.failed.add file

proc failure*(c: var TestCounters; file, expected, given: string) =
  noteFailure c, file
  var m = file & " --------------------------------------\nFAILURE: expected:\n"
  m.add expected
  m.add "\nbut got\n"
  m.add given
  m.add "\n"
  echo m

proc failure*(c: var TestCounters; file, msg: string) =
  noteFailure c, file
  let m = file & " --------------------------------------\nFAILURE: " & msg & "\n"
  echo m

proc reportFailures*(c: TestCounters) =
  if c.failed.len == 0: return
  echo "\nFAILED (", c.failed.len, "):"
  for f in c.failed: echo "  ", f
  echo ""

proc diffFiles*(c: var TestCounters; file, a, b: string; overwrite: bool) =
  if not os.sameFileContent(a, b):
    if overwrite:
      copyFile(b, a)
    else:
      let gitCmd = "git diff --no-index $1 $2" % [a.quoteShell, b.quoteShell]
      let (diff, diffExitCode) = execCmdEx(gitCmd)
      if diffExitCode <= 1:
        failure c, file, diff
      else:
        failure c, file, gitCmd & "\n" & diff

proc echoTestSuccess*(file: string) =
  echo "SUCCESS ", file

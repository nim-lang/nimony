## CLI and process-reaping regressions. This runner doubles as its own child
## command, avoiding shell-specific job scripts or rebuilding shared tools.
import std / [os, osproc, strutils, monotimes, times]
when defined(posix):
  import std/posix

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

proc require(cond: bool; message: string) =
  if not cond: quit "FAILURE: " & message

proc waitForFile(path: string) =
  let deadline = getMonoTime() + initDuration(seconds = 15)
  while not fileExists(path):
    require(getMonoTime() < deadline, "timed out waiting for " & path)
    sleep 10

proc runJob(job, dir: string) =
  case job
  of "second":
    require(fileExists(dir / "first"), "dependent ran before its input")
  of "slow":
    writeFile(dir / "slow.started", "")
    waitForFile(dir / "dependent")
  of "fast":
    waitForFile(dir / "slow.started")
  of "dependent":
    require(fileExists(dir / "fast"), "dependent ran before fast job")
  of "running":
    writeFile(dir / "running.started", "")
    waitForFile(dir / "failure.started")
    sleep 100 # leave a job running while the other child fails
  of "failure", "signal":
    waitForFile(dir / "running.started")
    writeFile(dir / "failure.started", "")
    if job == "failure": quit 7
    when defined(posix):
      discard posix.kill(posix.getpid(), SIGTERM)
    quit "FAILURE: signal job survived"
  of "groupA", "groupB":
    # Writes `x` and `y`; notes whether the other build's node ran meanwhile.
    let other = dir / (if job == "groupA": "groupB" else: "groupA") & ".started"
    writeFile(dir / job & ".started", "")
    let deadline = getMonoTime() + initDuration(seconds = 1)
    while not fileExists(other) and getMonoTime() < deadline: sleep 10
    if fileExists(other): writeFile(dir / "overlap", "")
    writeFile(dir / "x", "")
    writeFile(dir / "y", "")
  of "first", "blocked", "after": discard
  else: quit "FAILURE: unknown job " & job
  writeFile(dir / job, "done\n")

# Child invocations must exit before running the suite itself.
let job = arg("job")
if job.len > 0:
  runJob(job, arg("dir"))
  quit 0

proc nifString(s: string): string =
  result = "\""
  for ch in s:
    if ch < ' ' or ch in {'"', '\\'}:
      result.add '\\'
      result.add toHex(ord(ch), 2)
    else:
      result.add ch
  result.add '"'

proc addJob(graph: var string; dir, job: string; dependency = "") =
  graph.add "(cmd :" & job & " " & nifString(getAppFilename()) & " " &
    nifString("--job:" & job) & " " & nifString("--dir:" & dir) & ")\n"
  graph.add "(do " & job & " (input"
  if dependency.len > 0: graph.add " " & nifString(dir / dependency)
  graph.add ") (output " & nifString(dir / job) & "))\n"

let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
let nifmake = (bindir / "nifmake".addFileExt(ExeExt)).quoteShell

let (expected, _) = execCmdEx(nifmake & " version")
let (actual, _) = execCmdEx(nifmake & " --base: version")
if actual == expected:
  echo "nifmake: an empty --base: does not swallow the command after it"
else:
  quit "FAILURE: `nifmake --base: version` did not print what `nifmake version` " &
       "prints; got: " & actual

let root = currentSourcePath().parentDir.parentDir.parentDir
let cache = root / "nimcache_static" / "nifmake-jobs"
createDir(cache)

for scenario in ["serial", "parallel", "failure", "signal", "keepgoing"]:
  when not defined(posix):
    if scenario == "signal": continue
  let dir = cache / scenario
  if dirExists(dir): removeDir(dir)
  createDir(dir)
  var graph = "(.nif27)\n(stmts\n"
  case scenario
  of "serial":
    graph.addJob(dir, "first")
    graph.addJob(dir, "second", "first")
  of "parallel":
    # The slow job cannot finish until the fast job's dependent has run.
    # This checks dataflow scheduling without fragile timing thresholds.
    graph.addJob(dir, "slow")
    graph.addJob(dir, "fast")
    graph.addJob(dir, "dependent", "fast")
  else:
    # `after` becomes ready only once the failure was seen: `running` waits for
    # the failing child to start.
    let failing = if scenario == "keepgoing": "failure" else: scenario
    graph.addJob(dir, "running")
    graph.addJob(dir, failing)
    graph.addJob(dir, "blocked", failing)
    graph.addJob(dir, "after", "running")
  graph.add ")\n"
  let graphFile = dir / "build.nif"
  writeFile(graphFile, graph)
  let parallel = case scenario
    of "serial": ""
    of "keepgoing": " --parallel:2 --keepgoing"
    else: " --parallel:2"
  let command = nifmake & parallel & " --profile --report run " & graphFile.quoteShell
  let (output, code) = execCmdEx(command)
  writeFile(dir / "result.log", output)
  if scenario in ["serial", "parallel"]:
    require(code == 0, scenario & " build failed:\n" & output)
    let count = if scenario == "serial": 2 else: 3
    require(output.contains("total=" & $count), "missing completed jobs:\n" & output)
    require(output.contains("nifmake profile"), "missing profile:\n" & output)
    require(fileExists(dir / (if scenario == "serial": "second" else: "slow")),
            "build returned before children finished")
    let (cached, cachedCode) = execCmdEx(command)
    require(cachedCode == 0 and cached.contains("total=0"),
            "up-to-date build launched commands:\n" & cached)
  else:
    let expectedExit = if scenario == "signal": 143 else: 7
    require(code != 0 and output.contains("exit code " & $expectedExit),
            "wrong child failure status:\n" & output)
    require(fileExists(dir / "running"),
            "did not drain the other running child:\n" & output)
    require(not fileExists(dir / "blocked"), "launched dependent of failed child")
    if scenario == "keepgoing":
      require(output.contains("total=3") and fileExists(dir / "after"),
              "--keepgoing stopped an independent job:\n" & output)
    else:
      require(output.contains("total=2") and not fileExists(dir / "after"),
              "launched a job after a failure:\n" & output)
  echo "nifmake: ", scenario, " execution passed"

block:
  # `--lock`: two builds whose nodes write the same files but list them in
  # other orders, as a `{.cyclic.}` group entered through different members
  # does, build them once and never at the same time.
  let dir = cache / "lock"
  if dirExists(dir): removeDir(dir)
  createDir(dir)
  var builds: seq[Process] = @[]
  for (job, first, second) in [("groupA", "x", "y"), ("groupB", "y", "x")]:
    let graphFile = dir / job & ".nif"
    writeFile(graphFile, "(.nif27)\n(stmts\n(cmd :" & job & " " &
      nifString(getAppFilename()) & " " & nifString("--job:" & job) & " " &
      nifString("--dir:" & dir) & ")\n(do " & job & " (input) (output " &
      nifString(dir / first) & ") (output " & nifString(dir / second) & ")))\n")
    builds.add startProcess(nifmake & " --lock:" & quoteShell(dir / "lock") &
      " run " & graphFile.quoteShell,
      options = {poEvalCommand, poStdErrToStdOut, poParentStreams})
  for p in builds:
    require(waitForExit(p) == 0, "a build sharing the lock failed")
    close p
  require(not fileExists(dir / "overlap"),
          "two builds wrote the same outputs at the same time")
  require(fileExists(dir / "groupA") xor fileExists(dir / "groupB"),
          "the build that waited did not find the outputs current")
  echo "nifmake: builds sharing outputs under --lock passed"

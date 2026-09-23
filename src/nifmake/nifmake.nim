#       Nifmake tool
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nifmake is a make-like tool that is used by Nimony to implement parallel
## and incremental compilation. Nifmake can run a dependency graph as specified
## by a .nif file or it can translate this file to a Makefile.

import std/[assertions, os, strutils, sequtils, tables, hashes, times, monotimes, sets, parseopt, syncio, osproc, algorithm, terminal]
import ".." / lib / [bitabs, lineinfos, nifreader, tooldirs, argsfinder, vfs, nifpools, nimversion]
when defined(windows):
  import std/winlean
else:
  import std/posix

# Inspired by https://gittup.org/tup/build_system_rules_and_algorithms.pdf
#[
build_partial_DAG(DAG, change_list)
   foreach file_changed in change_list {
     add_node(DAG, file_changed)
   }

add_node(DAG, node)
  add node to DAG
  dependency_list = get_dependencies(node)
  foreach dependency d in dependency_list {
    if d is not in DAG { add_node(DAG, d) }
    add link (node -> d) to DAG
  }

update(DAG)
  file_list = topological_sort(DAG)
  foreach file in file_list {
    perform command to update file
  }

Example for a .nif file:

```nif
(stmts
  (cmd :nifler "nifler" (input) (output))
  (do nifler
    (input "a.nim")
    (output "a.p.nif")
  )
)
```

]#

type
  NodeState = enum
    nsUnvisited
    nsInStack
    nsVisited

  Command* = object
    name*: string
    tokens*: TokenBuf
    ext*: string

  Node* = object
    cmdIdx*: int      # index into Dag.commands
    inputs*: seq[string]
    outputs*: seq[string]
    args*: seq[string]
    deps*: seq[int]   # node IDs this depends on
    inputsOf*: seq[string]
      ## commands whose every output is an input of this node; expanded into
      ## `inputs` once the whole file is parsed (see `expandInputsOf`)
    state*: NodeState
    depth*: int       # depth in the DAG for parallel execution

  Dag* = object
    nodes*: seq[Node]
    nameToId*: Table[string, int]
    maxDepth*: int    # maximum depth in the DAG
    commands*: seq[Command]  # bidirectional mapping of commands
    baseDir*: string

  CliCommand = enum
    cmdRun, cmdMakefile, cmdHelp, cmdVersion

  CliOption = enum
    Parallel, Force, Rerun, Verbose, Profile, Report, Progress

  CmdStats = object
    sec: float
    count: int
    peakKiB, sumKiB: int  ## peak RSS of the hungriest invocation / all summed
    peakLabel: string     ## the node that hit `peakKiB`

  ProfileData* = object
    parseTime: float
    dagSetupTime: float
    cmdStats: Table[string, CmdStats]
    heaviest: seq[tuple[kib: int, cmdName, label: string]]
      ## the `HeaviestShown` hungriest nodes, descending
    execWallTime: float

proc addSpace(result: var string) {.inline.} =
  if result.len > 0 and result[^1] != ' ': result.add ' '

proc addFilename(result: var string; filename, prefix, suffix: string) =
  if filename.len > 0:
    result.addSpace()
    if prefix.len > 0: result.add prefix
    # This is not a bug, a suffix is always assumed to be part of the filename
    # and so also subject to quoting:
    result.add (suffix & filename).quoteShell

proc expandCommand(cmd: Command; inputs, outputs, args: seq[string]; baseDir: string): string =
  result = ""
  if cmd.tokens.len == 0:
    quit "undeclared command: " & cmd.name

  var n = readonlyCursorAt(cmd.tokens, 0)
  var toolArgs: seq[string] = @[]
  if n.kind == StrLit:
    let tool = findTool(n.strVal)
    result.add quoteShell(tool)
    inc n
    if baseDir.len > 0 and cmd.ext.len > 0:
      let argsFile = findArgs(baseDir, extractArgsKey(tool) & cmd.ext)
      processArgsFile argsFile, toolArgs

  while n.hasMore:
    if n.kind == StrLit:
      addSpace(result)
      # each StrLit is one argument; without quoting an argument
      # containing a space (e.g. a forwarded `--define:key=a b` or a path)
      # splits into several (tool names and filenames are quoted already)
      result.add quoteShell(n.strVal)
      inc n
    elif n.isTagLit:
      let tag = globalTags.tags[n.cursorTagId]
      if tag == "args":
        # Add explicit arguments from the .nif file
        for i in 0..<args.len:
          addSpace(result)
          result.add quoteShell(args[i])
        # Add tool-specific arguments (from .args files)
        for arg in toolArgs:
          addSpace(result)
          result.add arg
        skip n  # advance past the (args …) subtree
      else:
        let L = if tag == "output": outputs.len else: inputs.len
        var a = 0
        var b = 0
        var prefix = ""
        var suffix = ""
        n.into:
          if n.hasMore and n.kind == StrLit:
            prefix = n.strVal
            inc n
          if n.hasMore and n.kind == IntLit:
            a = int n.intVal
            if a < 0: a = L + a
            b = a
            inc n
          if n.hasMore and n.kind == IntLit:
            b = int n.intVal
            if b < 0: b = L + b
            inc n
          if n.hasMore and n.kind == StrLit:
            suffix = n.strVal
            inc n
          while n.hasMore: skip n
        case tag
        of "input":
          for i in a..b:
            if i >= 0 and i < inputs.len:
              addFilename(result, inputs[i], prefix, suffix)
        of "output":
          for i in a..b:
            if i >= 0 and i < outputs.len:
              addFilename(result, outputs[i], prefix, suffix)
        else:
          raiseAssert "unsupported tag in `cmd` definition: " & tag
    else:
      raiseAssert "unsupported token in `cmd` definition: " & $n.kind

proc registerCommand(dag: var Dag; cmdName: string; ext: string): int =
  for i in 0..<dag.commands.len:
    if dag.commands[i].name == cmdName:
      return i
  result = dag.commands.len
  dag.commands.add Command(name: cmdName, ext: ext)

proc addNode(dag: var Dag; cmdName: string;
             inputs, outputs, args: sink seq[string]; ext: string;
             inputsOf: sink seq[string] = @[]): int =
  ## Add a build node to the DAG and return its ID
  result = dag.nodes.len
  let cmdIdx = registerCommand(dag, cmdName, ext)
  let node = Node(
    cmdIdx: cmdIdx,
    inputs: inputs,
    outputs: outputs,
    args: args,
    deps: @[],
    inputsOf: inputsOf,
    state: nsUnvisited,
    depth: 0
  )
  dag.nodes.add(node)

  # Map outputs to this node
  for output in outputs:
    dag.nameToId[output] = result

proc expandInputsOf(dag: var Dag) =
  ## Turn each node's `(inputsof <cmd>)` entries into ordinary inputs: every
  ## output of every node that runs `<cmd>`. Done as a post-pass because a
  ## `do` rule may name a command whose nodes appear later in the file.
  var byCommand = initTable[string, seq[string]]()
  for node in dag.nodes:
    let name = dag.commands[node.cmdIdx].name
    if name notin byCommand: byCommand[name] = @[]
    for output in node.outputs:
      byCommand[name].add output
  for nodeId in 0 ..< dag.nodes.len:
    if dag.nodes[nodeId].inputsOf.len == 0: continue
    var seen = initHashSet[string]()
    for input in dag.nodes[nodeId].inputs: seen.incl input
    for cmdName in dag.nodes[nodeId].inputsOf:
      if cmdName notin byCommand:
        quit "`inputsof` names a command with no nodes: " & cmdName
      for output in byCommand[cmdName]:
        if not seen.containsOrIncl(output):
          dag.nodes[nodeId].inputs.add output

proc findDependencies(dag: var Dag; nodeId: int) =
  ## Find dependencies for a node and link them
  var node = addr dag.nodes[nodeId]

  for input in node.inputs:
    if input in dag.nameToId:
      let depId = dag.nameToId[input]
      if depId != nodeId and depId notin node.deps:
        node.deps.add(depId)

proc touchOutputs(node: Node; opt: set[CliOption]) =
  ## `--force` gives every output a fresh mtime, including those a tool left
  ## untouched via `OnlyIfChanged`. Done *after* the node ran, never by
  ## deleting the outputs first: a const-eval or plugin sub-compile shares the
  ## nimcache and may be reading them right now (writes are atomic renames,
  ## so replacing a file under a reader is fine; removing it is not).
  let now = getTime()
  for output in node.outputs:
    if vfsExists(output):
      try:
        setLastModificationTime(output, now)
      except:
        if Verbose in opt:
          stderr.writeLine "Warning: Could not touch artifact: ", output

type
  StatEntry = tuple[exists: bool, mtime: int64]
  StatCache = object
    ## Memoizes one `stat` per path for the length of a build. A node names
    ## every module's Leng file (see `addInlineSourceInputs` in Nimony's
    ## `deps.nim`), so the same path is asked about once per node: on a no-op
    ## `nimsem` rebuild that was 88k `newfstatat` calls for ~1.3k distinct
    ## files. Only a node writing its outputs can change what a `stat` says,
    ## and `invalidate` is called exactly there.
    entries: Table[string, StatEntry]

proc stat(sc: var StatCache; path: string): StatEntry =
  sc.entries.withValue(path, e):
    return e[]
  let exists = vfsExists(path)
  result = (exists, if exists: vfsMtime(path) else: low(int64))
  sc.entries[path] = result

proc invalidate(sc: var StatCache; node: Node) =
  for output in node.outputs:
    sc.entries.del output

proc needsRebuild(sc: var StatCache; node: Node): bool =
  ## Check if a node needs to be rebuilt
  result = false

  # Nodes with no outputs are side-effectful (e.g. idetools printing to stdout);
  # always run them.
  if node.outputs.len == 0:
    return true

  # Use the *freshest* output as the staleness reference (max instead of
  # min). Tools may write some outputs OnlyIfChanged — when the content
  # didn't change those preserve their old mtime. Using min would treat
  # "preserved old" as the floor and re-fire the node forever even though
  # some other output (always written) is fresh enough to prove "we ran
  # since the inputs last changed".
  var freshestOutput = low(int64)
  for output in node.outputs:
    let o = sc.stat(output)
    # A missing output is the one unconditional reason to run.
    if not o.exists:
      return true
    if o.mtime > freshestOutput:
      freshestOutput = o.mtime

  for input in node.inputs:
    let i = sc.stat(input)
    if i.exists and i.mtime > freshestOutput:
      return true

proc visit(nodes: var seq[Node]; nodeId: int; sortedNodes: var seq[int]; maxDepth: var int): bool =
  case nodes[nodeId].state
  of nsInStack:
    # Cycle detected
    result = false
  of nsVisited:
    result = true
  of nsUnvisited:
    nodes[nodeId].state = nsInStack
    var nodeDepth = 0
    for depId in nodes[nodeId].deps:
      if not visit(nodes, depId, sortedNodes, maxDepth):
        result = false
        return
      nodeDepth = max(nodeDepth, nodes[depId].depth)
    nodes[nodeId].depth = nodeDepth + 1
    maxDepth = max(maxDepth, nodes[nodeId].depth)
    nodes[nodeId].state = nsVisited
    sortedNodes.add(nodeId)
    result = true

proc topologicalSort(dag: var Dag): seq[int] =
  ## Perform topological sort on the DAG, then re-order by depth. The DFS
  ## post-order is already a valid topological order; sorting by depth on top
  ## of it is what makes the order a useful dispatch PRIORITY for `runDag` —
  ## among the nodes that are ready at the same moment, the one whose
  ## dependency chain is shortest starts first, which keeps a parallel run
  ## deterministic and puts the widest fan-out in front.
  result = @[]
  dag.maxDepth = 0

  for i in 0..<dag.nodes.len:
    if dag.nodes[i].state == nsUnvisited:
      if not visit(dag.nodes, i, result, dag.maxDepth):
        quit "Circular dependency detected in build graph"

  let nodes = addr dag.nodes
  result.sort proc(a, b: int): int = cmp(nodes[a].depth, nodes[b].depth)

proc failed(command: string; exitCode = 0) =
  ## Name the command that failed. A build that fans out over many processes is
  ## only debuggable if the output that a child left behind can be tied back to
  ## the command that produced it -- a bare "SIGSEGV" in a CI log names neither
  ## the tool nor the module it was working on.
  stdout.write "nifmake: command failed"
  if exitCode != 0:
    stdout.write " (exit code "
    stdout.write $exitCode
    stdout.write ")"
  stdout.write ": "
  stdout.writeLine command

proc toSeconds(d: Duration): float =
  float(d.inNanoseconds) / 1e9

const HeaviestShown = 10

proc recordCmd(profile: var ProfileData; cmdName, label: string;
               sec: float; peakKiB: int) =
  let e = addr profile.cmdStats.mgetOrPut(cmdName, CmdStats())
  e.sec += sec
  inc e.count
  e.sumKiB += peakKiB
  if peakKiB > e.peakKiB:
    e.peakKiB = peakKiB
    e.peakLabel = label
  if peakKiB > 0:
    var i = profile.heaviest.len
    while i > 0 and profile.heaviest[i-1].kib < peakKiB: dec i
    if i < HeaviestShown:
      profile.heaviest.insert((peakKiB, cmdName, label), i)
      if profile.heaviest.len > HeaviestShown: profile.heaviest.setLen HeaviestShown

type
  CmdStatus = enum
    Enqueued, Running, Finished, Failed

  Progressor = object
    ## Live percentage indicator. `total` is the number of nodes we expect to
    ## (re)build this run; `done` counts the ones that actually ran. The shown
    ## percentage is remapped into `[lo, hi]` so a caller that runs nifmake in
    ## several phases (Nimony's frontend/backend split) can hand each phase a
    ## sub-range and present one continuous 0..100% bar across processes.
    active: bool
    done, total, lo, hi: int

proc nodeLabel(dag: Dag; node: Node): string =
  ## Short human-facing label for the artifact a node produces.
  if node.outputs.len > 0: extractFilename(node.outputs[0])
  else: dag.commands[node.cmdIdx].name

proc countToBuild(dag: var Dag; sortedNodes: seq[int]; opt: set[CliOption];
                  sc: var StatCache): int =
  ## Estimate how many nodes will run, propagating staleness along the DAG:
  ## a node rebuilds if it is stale itself or any dependency will rebuild.
  ## `sortedNodes` is depth-ordered (deps first), so a single forward pass
  ## suffices. This is an upper bound — a dependency written `OnlyIfChanged`
  ## may not actually re-trigger its dependents — so the bar can finish a hair
  ## early; the caller forces the final reading to `hi`.
  result = 0
  var willBuild = newSeq[bool](dag.nodes.len)
  for nodeId in sortedNodes:
    var w = Force in opt or Rerun in opt or needsRebuild(sc, dag.nodes[nodeId])
    if not w:
      for depId in dag.nodes[nodeId].deps:
        if willBuild[depId]: w = true; break
    willBuild[nodeId] = w
    if w: inc result

proc draw(p: Progressor; label: string) =
  if not p.active: return
  let frac = if p.total <= 0: 100 else: clamp(p.done * 100 div p.total, 0, 100)
  let pct = p.lo + frac * (p.hi - p.lo) div 100
  # `\r` rewinds to column 0, `\e[K` clears any leftover from a longer label.
  # The trailing `\r` parks the cursor back at column 0 so a child process that
  # streams its own output (a compile error, a warning) overwrites the bar in
  # place instead of getting glued onto the end of the parked bar line.
  stdout.write "\r[" & align($pct, 3) & "%] " & label & "\e[K\r"
  stdout.flushFile()

proc finish(p: Progressor) =
  if not p.active: return
  p.draw(if p.hi >= 100: "done" else: "")
  # Only the final phase (`hi == 100`) closes the line; earlier phases leave the
  # cursor parked so the next nifmake process overwrites the same line via `\r`.
  if p.hi >= 100:
    stdout.write "\n"
    stdout.flushFile()

type
  Scheduler = object
    ## Ready-queue state for `runDag`'s parallel path. `pending[n]` counts the
    ## dependencies of `n` that have not completed yet; `successors[n]` is the
    ## reverse edge that decrements them. `rank` is a node's position in the
    ## topological order and is the dispatch priority, so a run stays
    ## deterministic and deps-first.
    pending: seq[int]
    successors: seq[seq[int]]
    rank: seq[int]
    ready: seq[int]
    done: int

  RunningJob = object
    process: Process
    nodeId: int
    command: string   # kept for the failure message
    cmdName: string
    label: string
    start: MonoTime

proc initScheduler(dag: Dag; sortedNodes: seq[int]): Scheduler =
  result = Scheduler(
    pending: newSeq[int](dag.nodes.len),
    successors: newSeq[seq[int]](dag.nodes.len),
    rank: newSeq[int](dag.nodes.len),
    ready: @[],
    done: 0)
  for r, nodeId in sortedNodes:
    result.rank[nodeId] = r
  for nodeId in sortedNodes:
    # `deps` is already deduplicated by `findDependencies`, so one edge per
    # producer and the count matches the number of releases.
    for depId in dag.nodes[nodeId].deps:
      result.successors[depId].add nodeId
      inc result.pending[nodeId]
  for nodeId in sortedNodes:
    if result.pending[nodeId] == 0: result.ready.add nodeId

proc takeReady(s: var Scheduler): int =
  ## Remove and return the ready node that comes first in topological order.
  var best = 0
  for k in 1 ..< s.ready.len:
    if s.rank[s.ready[k]] < s.rank[s.ready[best]]: best = k
  result = s.ready[best]
  s.ready.del best  # order within `ready` does not matter, `rank` decides

proc complete(s: var Scheduler; nodeId: int) =
  inc s.done
  for succId in s.successors[nodeId]:
    dec s.pending[succId]
    if s.pending[succId] == 0:
      s.ready.add succId

when defined(windows):
  type
    ProcessMemoryCounters = object
      cb, pageFaultCount: DWORD
      peakWorkingSetSize, workingSetSize: uint
      quotaPeakPagedPoolUsage, quotaPagedPoolUsage: uint
      quotaPeakNonPagedPoolUsage, quotaNonPagedPoolUsage: uint
      pagefileUsage, peakPagefileUsage: uint

  proc getProcessMemoryInfo(process: Handle; counters: ptr ProcessMemoryCounters;
                            cb: DWORD): WINBOOL {.
    stdcall, dynlib: "kernel32", importc: "K32GetProcessMemoryInfo".}

  proc peakKiBOf(p: Process): int =
    ## Peak working set of a child that has just exited, 0 if Windows won't
    ## say. The pid still resolves at this point ONLY because osproc's process
    ## handle is still open: it is the last handle to the kernel object, and
    ## `peekExitCode` (not just `close`) drops it. Call this before either of
    ## them -- see `waitForAnyJob`. Wine (10.0) answers this only for the
    ## calling process and denies it for any other, so under Wine the memory
    ## column stays empty.
    result = 0
    let h = openProcess(PROCESS_QUERY_LIMITED_INFORMATION or PROCESS_VM_READ,
                        0, DWORD(p.processID))
    if h != 0:
      var c = ProcessMemoryCounters(cb: DWORD(sizeof(ProcessMemoryCounters)))
      if getProcessMemoryInfo(h, addr c, c.cb) != 0:
        result = int(c.peakWorkingSetSize div 1024)
      discard closeHandle(h)

proc waitForAnyJob(pool: seq[RunningJob]; exitCode, peakKiB: var int): int =
  ## Index of the first job in `pool` that has exited, with its exit code and
  ## peak memory use (resident set, KiB).
  result = 0
  when defined(windows):
    # A 1ms tick is far below the cheapest node in a real build (a `nifler`
    # parse is ~2.5ms).
    while true:
      for idx in 0 ..< pool.len:
        if not osproc.running(pool[idx].process):
          # Order matters: `peekExitCode` closes the process handle (it calls
          # osproc's `closeThreadAndProcessHandle`), that handle is the last
          # one alive, and once the kernel object goes so does the pid --
          # `openProcess` then fails with ERROR_INVALID_PARAMETER and the
          # memory column reads 0. Sample the memory first.
          peakKiB = peakKiBOf(pool[idx].process)
          exitCode = try: peekExitCode(pool[idx].process) except CatchableError: -1
          return idx
      sleep 1
  else:
    # Reaps with `wait4` rather than through osproc because only `wait4` hands
    # back the child's `ru_maxrss`, which covers the `sh -c` wrapper's own
    # children too. Waiting on any child is fine: `pool` holds all of them.
    # osproc never learns of the exit, so the caller must not ask it
    # (`running`, `peekExitCode`); `close` does not wait and stays safe.
    while true:
      var status: cint = 0
      var ru = default(Rusage)
      # Unlike waitpid, illumos/Solaris wait4 uses 0 for any child;
      # -1 selects process group 1 there and normally fails with ECHILD.
      const anyChild = when defined(sunos) or defined(solaris) or defined(illumos): Pid(0)
                       else: Pid(-1)
      let pid = wait4(anyChild, addr status, 0, addr ru)
      if pid < 0:
        if errno == EINTR: continue
        raiseOSError(osLastError())
      for idx in 0 ..< pool.len:
        if pool[idx].process.processID == int(pid):
          exitCode = exitStatusLikeShell(status)
          # macOS reports bytes, other supported implementations KiB.
          # illumos leaves ru_maxrss unimplemented (0): no memory column.
          peakKiB = when defined(macosx): int(ru.ru_maxrss) div 1024
                    else: int(ru.ru_maxrss)
          return idx

var gMaxJobs = 0
  ## Concurrency cap for `--parallel:N` / `-j:N` (0 = use all cores). Set
  ## during option parsing, read in `runDag`.

proc runDag(dag: var Dag; opt: set[CliOption]; profile: ptr ProfileData = nil;
            progressLo = 0; progressHi = 100): bool =
  ## Execute the DAG in topological order
  result = true
  let sortStart = if profile != nil: getMonoTime() else: MonoTime()
  let sortedNodes = topologicalSort(dag)
  if profile != nil:
    profile[].dagSetupTime = toSeconds(getMonoTime() - sortStart)

  var sc = StatCache(entries: initTable[string, StatEntry]())

  # The live bar is routed only where it makes sense: it needs an interactive
  # terminal, and it must not corrupt `--verbose`'s line output or `--report`'s
  # machine-readable stdout.
  var prog = Progressor(
    active: Progress in opt and Verbose notin opt and Report notin opt and isatty(stdout),
    done: 0, total: 0, lo: progressLo, hi: progressHi)
  if prog.active:
    prog.total = countToBuild(dag, sortedNodes, opt, sc)
    prog.draw("")  # paint the starting reading (lo%) right away

  # Dataflow scheduling: a node starts as soon as *its own* dependencies are
  # done, not when every node of its DAG depth is. The depth-barrier version
  # this replaces made one slow node block every unrelated node one level
  # below it — measured on a cold `nimsem` build, ~40 stdlib modules each
  # waited 0.96s behind a single `nimversion` const-eval sub-compile they do
  # not import. Staleness is still evaluated at dispatch time, after the
  # dependencies have actually been written, so `OnlyIfChanged` outputs keep
  # pruning their dependents exactly as before. A sequential run is this with
  # a single slot: `rank` dispatches in topological order.
  let jobs = if Parallel notin opt: 1
             elif gMaxJobs > 0: gMaxJobs
             else: countProcessors()
  var sched = initScheduler(dag, sortedNodes)
  var pool: seq[RunningJob] = @[]
  var aborted = false
  let execStart = if profile != nil: getMonoTime() else: MonoTime()

  while sched.done < sortedNodes.len:
    # Dispatch everything that fits.
    while not aborted and pool.len < jobs and sched.ready.len > 0:
      let nodeId = sched.takeReady()
      let node = addr dag.nodes[nodeId]
      if Force in opt or Rerun in opt or needsRebuild(sc, node[]):
        if Verbose in opt:
          echo "Building: ", node.outputs.join(", ")
        let expandedCmd = expandCommand(dag.commands[node.cmdIdx], node.inputs,
                                        node.outputs, node.args, dag.baseDir)
        if Verbose in opt:
          echo "Command: ", expandedCmd
        pool.add RunningJob(
          process: startProcess(expandedCmd,
                                options = {poStdErrToStdOut, poParentStreams, poEvalCommand}),
          nodeId: nodeId,
          command: expandedCmd,
          cmdName: dag.commands[node.cmdIdx].name,
          label: nodeLabel(dag, node[]),
          start: (if profile != nil: getMonoTime() else: MonoTime()))
      else:
        if Verbose in opt:
          echo "Up to date: ", node.outputs.join(", ")
        sched.complete nodeId

    if pool.len == 0:
      # Nothing running and nothing ready: either the DAG is finished, or a
      # failure left the rest unreachable.
      break

    var exitCode = 0
    var peakKiB = 0
    let k = waitForAnyJob(pool, exitCode, peakKiB)
    let job = pool[k]
    pool.del k
    close job.process
    if profile != nil:
      profile[].recordCmd(job.cmdName, job.label,
                          toSeconds(getMonoTime() - job.start), peakKiB)
    inc prog.done
    prog.draw job.label
    if exitCode == 0:
      if Force in opt: touchOutputs(dag.nodes[job.nodeId], opt)
      sc.invalidate dag.nodes[job.nodeId]
      sched.complete job.nodeId
    else:
      if prog.active:
        stdout.write "\n"
        stdout.flushFile()
      failed job.command, exitCode
      aborted = true
      inc sched.done

  if profile != nil:
    profile[].execWallTime += toSeconds(getMonoTime() - execStart)
  if aborted:
    return false

  prog.finish()

proc mescape(p: string): string =
  when defined(windows):
    result = p.replace("\\", "/")
  else:
    result = p.replace(":", "\\:") # Rule separators
  result = result.multiReplace({
    " ": "\\ ",   # Spaces
    "#": "\\#",   # Comments
    "$": "$$",    # Variables
    "(": "\\(",   # Function calls
    ")": "\\)",
    "*": "\\*",   # Wildcards
    "[": "\\[",   # Pattern matching
    "]": "\\]"
  })

proc generateMakefile(dag: Dag; filename: string) =
  ## Generate a Makefile from the DAG
  var content = "# Generated by nifmake\n\n"
  content.add ".PHONY: all clean\n\n"

  # Add all target
  content.add "all:"
  for node in dag.nodes:
    for output in node.outputs:
      content.add " " & mescape(output)
  content.add "\n\n"

  # Add rules for each node
  for node in dag.nodes:
    # Target line
    content.add node.outputs.map(mescape).join(" ")
    content.add ":"
    for input in node.inputs:
      content.add " " & mescape(input)
    content.add "\n"

    # Command line
    let expandedCmd = expandCommand(dag.commands[node.cmdIdx], node.inputs, node.outputs, node.args, dag.baseDir)
    content.add "\t" & mescape(expandedCmd) & "\n\n"

  # Add clean target
  content.add "clean:\n"
  content.add "\trm -f"
  for node in dag.nodes:
    for output in node.outputs:
      content.add " " & mescape(output)
  content.add "\n"

  writeFile(filename, content)

proc parseCommandDefinition(n: var Cursor; dag: var Dag) =
  if n.kind == SymbolDef:
    let cmdName = pool.symString(n.symId)
    inc n

    var tokens = createTokenBuf(4)
    var argsext = ".args"
    while n.hasMore:
      if n.kind == StrLit:
        tokens.addStrLit n.strVal
        inc n
      elif n.isTagLit:
        let tag = globalTags.tags[n.cursorTagId]
        if tag == "argsext":
          n.into:
            if n.hasMore and n.kind == StrLit:
              argsext = n.strVal
              inc n
        elif tag in ["input", "output", "args"]:
          # Bulk-copy the (input/output/args …) subtree into `tokens`. The
          # source's sealed jumps are preserved; `tokens.openTags` is left
          # alone because the subtree is internally balanced.
          tokens.addSubtree n
          skip n
        else:
          quit "unsupported tag in `cmd` definition: " & tag
      else:
        quit "unsupported token in `cmd` definition: " & $n.kind
    # No terminator token needed: `expandCommand`'s cursor is bounded by
    # the buffer, so `hasMore` ends the loop.
    let cmdIdx = registerCommand(dag, cmdName, argsext)
    dag.commands[cmdIdx].tokens = tokens
  else:
    quit "expected symbol definition in `cmd` definition"

proc parseDoRule(n: var Cursor; dag: var Dag) =
  var cmdName: string
  if n.kind == Symbol:
    cmdName = pool.symString(n.symId)
    inc n
  elif n.kind == Ident:
    cmdName = n.strVal
    inc n
  else:
    quit "expected symbol or identifier in `do` rule"

  var inputs: seq[string] = @[]
  var outputs: seq[string] = @[]
  var args: seq[string] = @[]
  var inputsOf: seq[string] = @[]

  # Parse imports and results
  while n.hasMore:
    if n.isTagLit:
      let tag = globalTags.tags[n.cursorTagId]
      n.into:
        if tag == "input":
          if n.hasMore and n.kind == StrLit:
            inputs.add(n.strVal)
            inc n
        elif tag == "output":
          if n.hasMore and n.kind == StrLit:
            outputs.add(n.strVal)
            inc n
        elif tag == "args":
          while n.hasMore:
            if n.kind == StrLit:
              args.add(n.strVal)
            inc n
        elif tag == "inputsof":
          # "every output of every node running <cmd>" — the whole-program
          # dependency a codegen node has on a phase that precedes it, written
          # once instead of naming N files per node (which is N*N strings for
          # N modules and dominated the .build.nif).
          while n.hasMore:
            if n.kind == Ident:
              inputsOf.add(n.strVal)
            elif n.kind == Symbol:
              inputsOf.add(pool.symString(n.symId))
            inc n
        else:
          quit "unsupported tag in `do` definition: " & tag
        # Body must consume all children — mop up anything we didn't recognise.
        while n.hasMore: skip n
    else:
      quit "expected `input` or `output` in `do` definition, but found: " & $n.kind

  discard addNode(dag, cmdName, inputs, outputs, args, ".args", inputsOf)

proc parseNifFile(filename: string; baseDir: sink string): Dag =
  ## Parse a .nif file and build the DAG
  result = Dag(baseDir: baseDir)

  if not vfsExists(filename):
    quit "File not found: " & filename

  var buf = parseFromFile(filename)
  var n = beginRead(buf)

  # Parse (.nif27)(stmts ...)
  if n.isTagLit:
    n.into:  # enter the (stmts ...) wrapper
      while n.hasMore:
        if n.isTagLit:
          case globalTags.tags[n.cursorTagId]
          of "cmd":
            n.into:
              parseCommandDefinition(n, result)
          of "do":
            n.into:
              parseDoRule(n, result)
          else:
            quit "unknown statement: " & globalTags.tags[n.cursorTagId]
        else:
          quit "expected statement in .nif file, but found: " & $n.kind

  # `(inputsof cmd)` names a whole build phase; resolve it now that every node
  # is known, then let `findDependencies` turn the filenames into edges as it
  # does for any other input.
  expandInputsOf(result)

  # Find dependencies between nodes
  for i in 0..<result.nodes.len:
    findDependencies(result, i)

proc writeHelp() =
  echo """nifmake - Nimony build system

Usage:
  nifmake [options] <command> [file]

Commands:
  run <file.nif>        Execute the build graph
  makefile <file.nif>   Generate Makefile from build graph
  help                  Show this help
  version               Show version

Options:
  -j, --parallel[:N]    Parallel builds (for 'run'); :N caps at N processes
  --makefile:<name>     Output Makefile name (default: Makefile)
  --force               Force rebuild of all targets
  --rerun               Run every command regardless of staleness, but KEEP the
                        existing outputs, so a tool writing OnlyIfChanged can
                        still report "unchanged" and spare everything
                        downstream. For a caller that knows the results are
                        stale for a reason no input mtime can express — e.g.
                        nimony when the compilation options changed.
  --verbose             Show verbose output
  --base:<dir>          Use <dir> as base directory for `.args` files.
                        If not set, no `.args` files are processed.
  --progress[:LO:HI]    Show a live percentage indicator while building (only
                        on an interactive terminal; ignored with --verbose and
                        --report). The optional LO:HI range remaps the bar so a
                        caller running several builds can show one continuous
                        0..100% bar across them.
  --profile             Print time and peak memory of executed commands to stderr.
  --report              Print machine-readable per-command invocation
                        counts to stdout, e.g.
                          nifmake-report nimsem=2 hexer=1 total=3
                        Used by the incremental-build regression test.

Examples:
  nifmake run build.nif
  nifmake makefile build.nif
  nifmake --makefile:build.mk makefile build.nif
"""
  quit(0)

proc writeVersion() =
  echo "nifmake " & Version
  quit(0)

proc printReport(profile: ProfileData) =
  ## Machine-readable summary of which commands actually executed during
  ## this nifmake invocation. One line on stdout, sorted by command name:
  ##   nifmake-report dceEmit=126 hexer=16 nifc=126 nimsem=121
  ## Zero-invocation runs print just `nifmake-report` (no entries) — that
  ## is the up-to-date signal used by the incremental-build regression
  ## test. Adds `total` as the sum across all commands.
  var entries = newSeq[(string, int)](profile.cmdStats.len)
  var i = 0
  var total = 0
  for cmd, data in profile.cmdStats.pairs:
    entries[i] = (cmd, data.count)
    inc i
    total += data.count
  entries.sort(proc(a, b: (string, int)): int = cmp(a[0], b[0]))
  stdout.write "nifmake-report"
  for (cmd, count) in entries:
    stdout.write " "
    stdout.write cmd
    stdout.write "="
    stdout.write $count
  stdout.write " total="
  stdout.write $total
  stdout.write "\n"

proc fmtKiB(kib: int): string =
  if kib >= 1024 * 1024: formatFloat(kib / (1024 * 1024), ffDecimal, 1) & " GiB"
  else: $(kib div 1024) & " MiB"

proc printProfile(profile: ProfileData) =
  stderr.writeLine "\n--- nifmake profile ---"
  stderr.writeLine "  parse .nif:     ", profile.parseTime.formatFloat(ffDecimal, 3), "s"
  stderr.writeLine "  DAG setup:      ", profile.dagSetupTime.formatFloat(ffDecimal, 3), "s"
  stderr.writeLine "  executed commands:"
  var entries: seq[(string, CmdStats)] = @[]
  for cmd, data in profile.cmdStats.pairs:
    entries.add (cmd, data)
  entries.sort(proc(a, b: (string, CmdStats)): int = cmp(b[1].sec, a[1].sec))
  for (cmd, data) in entries:
    var line = "    " & cmd.alignLeft(12) & " " & data.sec.formatFloat(ffDecimal, 3).align(8) &
      "s  (" & $data.count & " invocations)"
    if data.peakKiB > 0:
      line.add "  peak " & fmtKiB(data.peakKiB) & " (" & data.peakLabel & "), avg " &
        fmtKiB(data.sumKiB div data.count)
    stderr.writeLine line
  let execTotal = profile.cmdStats.values.toSeq.foldl(a + b.sec, 0.0)
  stderr.writeLine "  exec total:     ", execTotal.formatFloat(ffDecimal, 3), "s"
  stderr.writeLine "  wall time:      ", profile.execWallTime.formatFloat(ffDecimal, 3), "s"
  if profile.heaviest.len > 0:
    stderr.writeLine "  heaviest nodes (peak RSS):"
    for h in profile.heaviest:
      stderr.writeLine "    ", fmtKiB(h.kib).align(9), "  ", h.cmdName.alignLeft(12), " ", h.label
  stderr.writeLine "---"

proc main() =
  var
    cmd = cmdHelp
    inputFile = ""
    outputMakefile = "Makefile"
    opt: set[CliOption] = {}
    baseDir = ""
    progressLo = 0
    progressHi = 100

  var p = initOptParser(allowWhitespaceAfterColon = false)
  for kind, key, val in p.getopt():
    case kind
    of cmdArgument:
      case key.normalize
      of "help", "h": cmd = cmdHelp
      of "version", "v": cmd = cmdVersion
      of "run": cmd = cmdRun
      of "makefile": cmd = cmdMakefile
      else:
        if inputFile == "":
          inputFile = key
        else:
          quit "Too many arguments"

    of cmdLongOption, cmdShortOption:
      case key.normalize
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      of "parallel", "j":
        opt.incl Parallel
        # `--parallel:N` / `-j:N` caps the scheduler at N live processes;
        # bare `--parallel` (no value) keeps the all-cores default. Without this
        # the value was discarded and the build ran on all cores, which OOMs
        # large projects (e.g. nimbus under `nim ic -d:icJobs:N`).
        if val.len > 0:
          try:
            gMaxJobs = parseInt(val)
          except ValueError:
            quit "invalid value for --parallel: " & val
          if gMaxJobs < 1: quit "--parallel value must be >= 1"
      of "makefile": outputMakefile = val
      of "force": opt.incl Force
      of "rerun": opt.incl Rerun
      of "verbose": opt.incl Verbose
      of "base": baseDir = val
      of "profile": opt.incl Profile
      of "report": opt.incl Report
      of "progress":
        opt.incl Progress
        # Optional `--progress:LO:HI` remaps the bar into a sub-range so a
        # multi-phase caller gets one continuous 0..100% indicator.
        if val.len > 0:
          let parts = val.split(':')
          if parts.len == 2:
            try:
              progressLo = parseInt(parts[0])
              progressHi = parseInt(parts[1])
            except ValueError:
              quit "invalid --progress range: " & val
          else:
            quit "invalid --progress range: " & val
      else:
        echo "Unknown option: --", key
        quit(1)

    of cmdEnd: discard

  case cmd
  of cmdHelp: writeHelp()
  of cmdVersion: writeVersion()
  of cmdRun:
    if inputFile == "":
      quit "Input file required for 'run' command"

    if Profile in opt or Report in opt:
      var profile = ProfileData()
      let parseStart = getMonoTime()
      var dag = parseNifFile(inputFile, baseDir)
      profile.parseTime = toSeconds(getMonoTime() - parseStart)
      let ok = runDag(dag, opt, addr profile, progressLo, progressHi)
      if Profile in opt: printProfile(profile)
      if Report in opt: printReport(profile)
      if not ok: quit 1
    else:
      var dag = parseNifFile(inputFile, baseDir)
      if not runDag(dag, opt, nil, progressLo, progressHi):
        quit 1

  of cmdMakefile:
    if inputFile == "":
      quit "Input file required for 'makefile' command"

    let dag = parseNifFile(inputFile, baseDir)
    generateMakefile(dag, outputMakefile)
    echo "Generated: ", outputMakefile

when isMainModule:
  main()
  dumpVfsProfile("nifmake")

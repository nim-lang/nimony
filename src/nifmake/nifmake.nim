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
    Parallel, Force, Verbose, Profile, Report, Progress

  ProfileData* = object
    parseTime: float
    dagSetupTime: float
    cmdTime: Table[string, tuple[sec: float, count: int]]
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
             inputs, outputs, args: sink seq[string]; ext: string): int =
  ## Add a build node to the DAG and return its ID
  result = dag.nodes.len
  let cmdIdx = registerCommand(dag, cmdName, ext)
  let node = Node(
    cmdIdx: cmdIdx,
    inputs: inputs,
    outputs: outputs,
    args: args,
    deps: @[],
    state: nsUnvisited,
    depth: 0
  )
  dag.nodes.add(node)

  # Map outputs to this node
  for output in outputs:
    dag.nameToId[output] = result

proc findDependencies(dag: var Dag; nodeId: int) =
  ## Find dependencies for a node and link them
  var node = addr dag.nodes[nodeId]

  for input in node.inputs:
    if input in dag.nameToId:
      let depId = dag.nameToId[input]
      if depId != nodeId and depId notin node.deps:
        node.deps.add(depId)

proc removeOutdatedArtifacts(node: Node; opt: set[CliOption]) =
  ## Remove outdated build artifacts for a node. Only used with --force;
  ## removing before normal incremental builds breaks tools that use OnlyIfChanged.
  for output in node.outputs:
    if vfsExists(output):
      try:
        vfsRemove(output)
        if Verbose in opt:
          echo "Removed outdated artifact: ", output
      except:
        stderr.writeLine "Warning: Could not remove outdated artifact: ", output

proc needsRebuild(node: Node): bool =
  ## Check if a node needs to be rebuilt
  result = false

  # Nodes with no outputs are side-effectful (e.g. idetools printing to stdout);
  # always run them.
  if node.outputs.len == 0:
    return true

  # Check if any output is missing
  for output in node.outputs:
    if not vfsExists(output):
      return true

  # Use the *freshest* output as the staleness reference (max instead of
  # min). Tools may write some outputs OnlyIfChanged — when the content
  # didn't change those preserve their old mtime. Using min would treat
  # "preserved old" as the floor and re-fire the node forever even though
  # some other output (always written) is fresh enough to prove "we ran
  # since the inputs last changed".
  var freshestOutput = low(int64)
  for output in node.outputs:
    let outputTime = vfsMtime(output)
    if outputTime > freshestOutput:
      freshestOutput = outputTime

  for input in node.inputs:
    if vfsExists(input):
      let inputTime = vfsMtime(input)
      if inputTime > freshestOutput:
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
  ## Perform topological sort on the DAG, then re-order by depth so the
  ## scheduler in `runDag` can group same-depth nodes contiguously and
  ## dispatch them via `execProcesses` in parallel. The DFS post-order is
  ## already a valid topological order, but it interleaves depths — and
  ## the `currentDepth`-batching loop downstream then sees one node per
  ## batch and serializes the build.
  result = @[]
  dag.maxDepth = 0

  for i in 0..<dag.nodes.len:
    if dag.nodes[i].state == nsUnvisited:
      if not visit(dag.nodes, i, result, dag.maxDepth):
        quit "Circular dependency detected in build graph"

  let nodes = addr dag.nodes
  result.sort proc(a, b: int): int = cmp(nodes[a].depth, nodes[b].depth)

proc executeCommand(command: string): bool =
  ## Execute a shell command and return success status
  try:
    let exitCode = execShellCmd(command)
    result = exitCode == 0
  except:
    result = false

proc failed(arg: string) =
  stdout.write "nifmake: "
  stdout.writeLine arg

proc toSeconds(d: Duration): float =
  float(d.inNanoseconds) / 1e9

proc recordCmdTime(profile: var ProfileData; cmdName: string; sec: float) =
  if cmdName notin profile.cmdTime:
    profile.cmdTime[cmdName] = (0.0, 0)
  var e = profile.cmdTime[cmdName]
  e.sec += sec
  e.count += 1
  profile.cmdTime[cmdName] = e

type
  CmdStatus = enum
    Enqueued, Running, Finished

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

proc countToBuild(dag: var Dag; sortedNodes: seq[int]; opt: set[CliOption]): int =
  ## Estimate how many nodes will run, propagating staleness along the DAG:
  ## a node rebuilds if it is stale itself or any dependency will rebuild.
  ## `sortedNodes` is depth-ordered (deps first), so a single forward pass
  ## suffices. This is an upper bound — a dependency written `OnlyIfChanged`
  ## may not actually re-trigger its dependents — so the bar can finish a hair
  ## early; the caller forces the final reading to `hi`.
  result = 0
  var willBuild = newSeq[bool](dag.nodes.len)
  for nodeId in sortedNodes:
    var w = Force in opt or needsRebuild(dag.nodes[nodeId])
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

var gMaxJobs = 0
  ## Concurrency cap for `--parallel:N` / `-j:N` (0 = use all cores, the
  ## `execProcesses` default). Set during option parsing, read in `runDag`.

proc runDag(dag: var Dag; opt: set[CliOption]; profile: ptr ProfileData = nil;
            progressLo = 0; progressHi = 100): bool =
  ## Execute the DAG in topological order
  result = true
  let sortStart = if profile != nil: getMonoTime() else: MonoTime()
  let sortedNodes = topologicalSort(dag)
  if profile != nil:
    profile[].dagSetupTime = toSeconds(getMonoTime() - sortStart)

  # The live bar is routed only where it makes sense: it needs an interactive
  # terminal, and it must not corrupt `--verbose`'s line output or `--report`'s
  # machine-readable stdout.
  var prog = Progressor(
    active: Progress in opt and Verbose notin opt and Report notin opt and isatty(stdout),
    done: 0, total: 0, lo: progressLo, hi: progressHi)
  if prog.active:
    prog.total = countToBuild(dag, sortedNodes, opt)
    prog.draw("")  # paint the starting reading (lo%) right away

  if Parallel in opt:
    var i = 0
    while i < sortedNodes.len:
      let currentDepth = dag.nodes[sortedNodes[i]].depth
      var commands: seq[string] = @[]
      var nodeIds: seq[int] = @[]
      var cmdNames: seq[string] = @[]
      var labels: seq[string] = @[]  # captured by afterRunEvent (can't capture `dag`)

      # Collect all commands at the current depth
      while i < sortedNodes.len and dag.nodes[sortedNodes[i]].depth == currentDepth:
        let node = addr dag.nodes[sortedNodes[i]]
        if Force in opt or needsRebuild(node[]):
          if Force in opt:
            removeOutdatedArtifacts(node[], opt)
          if Verbose in opt:
            echo "Building: ", node.outputs.join(", ")
          let expandedCmd = expandCommand(dag.commands[node.cmdIdx], node.inputs, node.outputs, node.args, dag.baseDir)
          if Verbose in opt:
            echo "Command: ", expandedCmd
          commands.add(expandedCmd)
          nodeIds.add(sortedNodes[i])
          cmdNames.add(dag.commands[node.cmdIdx].name)
          labels.add(nodeLabel(dag, node[]))
        inc i

      # Execute all commands at this depth in parallel
      if commands.len > 0:
        var progress = newSeq[CmdStatus](commands.len)
        var startTimes = if profile != nil: newSeq[MonoTime](commands.len) else: @[]
        if profile != nil: startTimes.setLen(commands.len)
        let depthStart = if profile != nil: getMonoTime() else: MonoTime()

        proc beforeRunEvent(idx: int) =
          progress[idx] = Running
          if profile != nil: startTimes[idx] = getMonoTime()

        proc afterRunEvent(idx: int; p: Process) =
          progress[idx] = Finished
          inc prog.done
          prog.draw(labels[idx])
          if profile != nil:
            let sec = toSeconds(getMonoTime() - startTimes[idx])
            profile[].recordCmdTime(cmdNames[idx], sec)

        let maxExitCode =
          if gMaxJobs > 0:
            execProcesses(commands, n = gMaxJobs,
                          beforeRunEvent = beforeRunEvent, afterRunEvent = afterRunEvent)
          else:
            execProcesses(commands,
                          beforeRunEvent = beforeRunEvent, afterRunEvent = afterRunEvent)
        if profile != nil:
          profile[].execWallTime += toSeconds(getMonoTime() - depthStart)
        if maxExitCode != 0:
          if prog.active:
            stdout.write "\n"
            stdout.flushFile()
          for i, p in pairs(progress):
            if p == Running:
              failed commands[i]
          return false
  else:
    # Sequential execution
    for nodeId in sortedNodes:
      let node = addr dag.nodes[nodeId]
      if Force in opt or needsRebuild(node[]):
        if Force in opt:
          removeOutdatedArtifacts(node[], opt)
        if Verbose in opt:
          echo "Building: ", node.outputs.join(", ")
        let expandedCmd = expandCommand(dag.commands[node.cmdIdx], node.inputs, node.outputs, node.args, dag.baseDir)
        if Verbose in opt:
          echo "Command: ", expandedCmd
        let cmdName = dag.commands[node.cmdIdx].name
        let start = if profile != nil: getMonoTime() else: MonoTime()
        if not executeCommand(expandedCmd):
          if profile != nil:
            profile[].recordCmdTime(cmdName, toSeconds(getMonoTime() - start))
          if prog.active:
            stdout.write "\n"
            stdout.flushFile()
          failed expandedCmd
          return false
        inc prog.done
        prog.draw(nodeLabel(dag, node[]))
        if profile != nil:
          let sec = toSeconds(getMonoTime() - start)
          profile[].recordCmdTime(cmdName, sec)
          profile[].execWallTime += sec
      else:
        if Verbose in opt:
          echo "Up to date: ", node.outputs.join(", ")

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
    let cmdName = pool.syms[n.symId]
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
    cmdName = pool.syms[n.symId]
    inc n
  elif n.kind == Ident:
    cmdName = n.strVal
    inc n
  else:
    quit "expected symbol or identifier in `do` rule"

  var inputs: seq[string] = @[]
  var outputs: seq[string] = @[]
  var args: seq[string] = @[]

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
        else:
          quit "unsupported tag in `do` definition: " & tag
        # Body must consume all children — mop up anything we didn't recognise.
        while n.hasMore: skip n
    else:
      quit "expected `input` or `output` in `do` definition, but found: " & $n.kind

  discard addNode(dag, cmdName, inputs, outputs, args, ".args")

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
  --makefile <name>     Output Makefile name (default: Makefile)
  --force               Force rebuild of all targets
  --verbose             Show verbose output
  --base:<dir>          Use <dir> as base directory for `.args` files.
                        If not set, no `.args` files are processed.
  --progress[:LO:HI]    Show a live percentage indicator while building (only
                        on an interactive terminal; ignored with --verbose and
                        --report). The optional LO:HI range remaps the bar so a
                        caller running several builds can show one continuous
                        0..100% bar across them.
  --profile             Print timing profile of executed commands to stderr.
  --report              Print machine-readable per-command invocation
                        counts to stdout, e.g.
                          nifmake-report nimsem=2 hexer=1 total=3
                        Used by the incremental-build regression test.

Examples:
  nifmake run build.nif
  nifmake makefile build.nif
  nifmake --makefile build.mk makefile build.nif
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
  var entries = newSeq[(string, int)](profile.cmdTime.len)
  var i = 0
  var total = 0
  for cmd, data in profile.cmdTime.pairs:
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

proc printProfile(profile: ProfileData) =
  stderr.writeLine "\n--- nifmake profile ---"
  stderr.writeLine "  parse .nif:     ", profile.parseTime.formatFloat(ffDecimal, 3), "s"
  stderr.writeLine "  DAG setup:      ", profile.dagSetupTime.formatFloat(ffDecimal, 3), "s"
  stderr.writeLine "  executed commands:"
  var entries = newSeq[(string, float, int)](profile.cmdTime.len)
  var i = 0
  for cmd, data in profile.cmdTime.pairs:
    entries[i] = (cmd, data.sec, data.count)
    inc i
  entries.sort(proc(a, b: (string, float, int)): int = cmp(b[1], a[1]))
  for (cmd, sec, count) in entries:
    stderr.writeLine "    ", cmd.alignLeft(12), " ", sec.formatFloat(ffDecimal, 3).align(8), "s  (", count, " invocations)"
  let execTotal = profile.cmdTime.values.toSeq.foldl(a + b.sec, 0.0)
  stderr.writeLine "  exec total:     ", execTotal.formatFloat(ffDecimal, 3), "s"
  stderr.writeLine "  wall time:      ", profile.execWallTime.formatFloat(ffDecimal, 3), "s"
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

  for kind, key, val in getopt():
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
        # `--parallel:N` / `-j:N` caps the per-depth fan-out at N processes;
        # bare `--parallel` (no value) keeps the all-cores default. Without this
        # the value was discarded and every DAG depth ran on all cores, which
        # OOMs large projects (e.g. nimbus under `nim ic -d:icJobs:N`).
        if val.len > 0:
          try:
            gMaxJobs = parseInt(val)
          except ValueError:
            quit "invalid value for --parallel: " & val
          if gMaxJobs < 1: quit "--parallel value must be >= 1"
      of "makefile": outputMakefile = val
      of "force": opt.incl Force
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
      var profile = ProfileData(cmdTime: initTable[string, tuple[sec: float, count: int]]())
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

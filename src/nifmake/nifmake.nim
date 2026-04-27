#       Nifmake tool
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nifmake is a make-like tool that is used by Nimony to implement parallel
## and incremental compilation. Nifmake can run a dependency graph as specified
## by a .nif file or it can translate this file to a Makefile.

import std/[assertions, os, strutils, sequtils, tables, hashes, times, monotimes, sets, parseopt, syncio, osproc, algorithm, deques]
import ".." / lib / [nifstreams, nifcursors, bitabs, lineinfos, nifreader, tooldirs, argsfinder]

when defined(posix):
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
    Parallel, Force, Verbose, Profile

  ProfileData* = object
    parseTime: float
    dagSetupTime: float
    cmdTime: Table[string, tuple[sec: float, count: int]]
    execWallTime: float

proc skipParRi(n: var Cursor) =
  ## Helper to skip a closing parenthesis
  if n.kind == ParRi:
    inc n
  else:
    #writeStackTrace()
    #echo toString([n.load()])
    quit "Expected ')' but found: " & $n.kind

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
  if n.kind == StringLit:
    let tool = findTool(pool.strings[n.litId])
    result.add quoteShell(tool)
    inc n
    if baseDir.len > 0 and cmd.ext.len > 0:
      let argsFile = findArgs(baseDir, extractArgsKey(tool) & cmd.ext)
      processArgsFile argsFile, toolArgs

  while true:
    case n.kind
    of ParRi:
      break
    of StringLit:
      addSpace(result)
      result.add pool.strings[n.litId]
      inc n
    of ParLe:
      let tag = pool.tags[n.tag]
      if tag == "args":
        # Add explicit arguments from the .nif file
        for i in 0..<args.len:
          addSpace(result)
          result.add args[i]
        # Add tool-specific arguments (from .args files)
        for arg in toolArgs:
          addSpace(result)
          result.add arg

        inc n
        skipParRi n
      else:
        let L = if tag == "output": outputs.len else: inputs.len
        var a = 0
        var b = 0
        inc n
        var prefix = ""
        if n.kind == StringLit:
          prefix = pool.strings[n.litId]
          inc n
        if n.kind == IntLit:
          a = int pool.integers[n.intId]
          if a < 0: a = L + a
          b = a
          inc n
        if n.kind == IntLit:
          b = int pool.integers[n.intId]
          if b < 0: b = L + b
          inc n
        var suffix = ""
        if n.kind == StringLit:
          suffix = pool.strings[n.litId]
          inc n
        skipParRi n
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
    if fileExists(output):
      try:
        removeFile(output)
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
    if not fileExists(output):
      return true

  # Check if any input is newer than any output
  var oldestOutput = getTime()
  for output in node.outputs:
    let outputTime = getLastModificationTime(output)
    if outputTime < oldestOutput:
      oldestOutput = outputTime

  for input in node.inputs:
    if fileExists(input):
      let inputTime = getLastModificationTime(input)
      if inputTime >= oldestOutput:
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
  ## Perform topological sort on the DAG
  result = @[]
  dag.maxDepth = 0

  for i in 0..<dag.nodes.len:
    if dag.nodes[i].state == nsUnvisited:
      if not visit(dag.nodes, i, result, dag.maxDepth):
        quit "Circular dependency detected in build graph"

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
  TaskId = distinct int
  WorkerId = distinct int

  Task = object
    nodeId: int
    p: Process
    cmd, cmdName: string
    startTime: MonoTime

  WorkerPool = object
    tasks: seq[Task] # uses TaskId for it
    workerIds: seq[WorkerId]
    wokerDeques: seq[Deque[TaskId]]
    active: seq[TaskId]

  Scheduler = object
    pool: WorkerPool
    pending: seq[int]
    done: seq[bool]
    remaining: int
    todo: seq[int]

proc `==`(a, b: WorkerId): bool {.borrow.}

proc steal(p: var WorkerPool, fromWorkerId: WorkerId): TaskId =
  result = TaskId(-1)
  for id in p.workerIds:
    if id != fromWorkerId and p.wokerDeques[id.int].len > 0:
      return p.wokerDeques[id.int].popLast()

proc eval(s: var Scheduler; dag: var Dag; dependents: seq[seq[int]];
          opt: set[CliOption]; profile: ptr ProfileData): bool =
  result = true

  for id in s.pool.workerIds:
    while s.todo.len > 0:
      let cur = s.todo.pop()
      if s.done[cur]: continue
      # code borrowed from old implementation
      let node = addr dag.nodes[cur]
      if Force in opt or needsRebuild(node[]):
        if Force in opt:
          removeOutdatedArtifacts(node[], opt)
        if Verbose in opt:
          echo "Building: ", node.outputs.join(", ")
        let expandedCmd = expandCommand(dag.commands[node.cmdIdx],
          node.inputs, node.outputs, node.args, dag.baseDir)
        if Verbose in opt:
          echo "Command: ", expandedCmd
        let t = TaskId(s.pool.tasks.len)
        s.pool.tasks.add Task(
          nodeId: cur, cmd: expandedCmd,
          cmdName: dag.commands[node.cmdIdx].name)
        s.pool.wokerDeques[id.int].addLast(t)
        break # we playing ping-pong (next resolve on next iteration)
      else:
        if Verbose in opt:
          echo "Up to date: ", node.outputs.join(", ")
        s.done[cur] = true
        dec s.remaining
        for d in dependents[cur]:
          dec s.pending[d]
          if s.pending[d] == 0:
            s.todo.add d

    if s.pool.active[id.int].int >= 0:
      let task = addr s.pool.tasks[s.pool.active[id.int].int]
      let exit = peekExitCode(task.p)
      if exit == 0:
        # Process finished point: success

        if profile != nil:
          profile[].recordCmdTime(
            task.cmdName,
            toSeconds(getMonoTime() - task.startTime))

        s.pool.active[id.int] = TaskId(-1)
        s.done[task.nodeId] = true
        dec s.remaining

        for d in dependents[task.nodeId]:
          dec s.pending[d]
          if s.pending[d] == 0:
            s.todo.add d

        close(task.p)
      elif exit > 0:
        # Process finished point: error

        if profile != nil:
          profile[].recordCmdTime(
            task.cmdName,
            toSeconds(getMonoTime() - task.startTime))

        s.pool.active[id.int] = TaskId(-1)
        failed task.cmd
        close(task.p)
        return false

    if s.pool.active[id.int].int < 0:
      let taskId =
        if s.pool.wokerDeques[id.int].len > 0:
          s.pool.wokerDeques[id.int].popFirst().int
        else:
          s.pool.steal(id).int

      if taskId >= 0:
        if profile != nil:
          s.pool.tasks[taskId].startTime = getMonoTime()

        s.pool.active[id.int] = TaskId(taskId)
        s.pool.tasks[taskId].p = startProcess(
          s.pool.tasks[taskId].cmd,
          options = {poStdErrToStdOut, poParentStreams, poUsePath, poEvalCommand})

  # Yielding core until work appears
  for id in s.pool.workerIds:
    if s.pool.active[id.int].int >= 0:
      when defined(posix):
        var info = default(SigInfo)
        discard waitid(cint(0), Id(0), info, WEXITED or WNOWAIT)  # P_ALL = 0
      else:
        discard waitForExit(s.pool.tasks[s.pool.active[id.int].int].p)

      break

proc runDag(dag: var Dag; opt: set[CliOption]; profile: ptr ProfileData = nil): bool =
  ## Execute the DAG in topological order
  result = true
  let sortStart = if profile != nil: getMonoTime() else: MonoTime()
  let sortedNodes = topologicalSort(dag)
  if profile != nil:
    profile[].dagSetupTime = toSeconds(getMonoTime() - sortStart)

  if Parallel in opt:
    # Work-stealing scheduler: one worker owns one process.
    let n = dag.nodes.len
    let nWorkers = max(1, countProcessors())
    var s = Scheduler(
      pending: newSeq[int](n),
      done: newSeq[bool](n),
      remaining: n,
    )
    var dependents = newSeq[seq[int]](n)
    for nodeId in 0..<n:
      s.pending[nodeId] = dag.nodes[nodeId].deps.len
      for d in dag.nodes[nodeId].deps:
        dependents[d].add nodeId

    s.pool.workerIds = newSeq[WorkerId](nWorkers)
    s.pool.wokerDeques = newSeq[Deque[TaskId]](nWorkers)
    s.pool.active = newSeq[TaskId](nWorkers)
    for i in 0..<nWorkers:
      s.pool.workerIds[i] = WorkerId(i)
      s.pool.wokerDeques[i] = initDeque[TaskId]()
      s.pool.active[i] = TaskId(-1)

    # It's first layer for start:
    for nodeId in sortedNodes:
      if s.pending[nodeId] == 0:
        s.todo.add nodeId

    let schedulerStart =
      if profile != nil:
        getMonoTime()
      else:
        MonoTime()

    while s.remaining > 0:
      if not s.eval(dag, dependents, opt, profile):
        return false

    if profile != nil:
      profile[].execWallTime += toSeconds(getMonoTime() - schedulerStart)
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
          failed expandedCmd
          return false
        if profile != nil:
          let sec = toSeconds(getMonoTime() - start)
          profile[].recordCmdTime(cmdName, sec)
          profile[].execWallTime += sec
      else:
        if Verbose in opt:
          echo "Up to date: ", node.outputs.join(", ")

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
    while n.kind != ParRi:
      case n.kind
      of StringLit:
        tokens.add n.load()
        inc n
      of ParLe:
        let tag = pool.tags[n.tag]
        if tag == "argsext":
          inc n
          if n.kind == StringLit:
            argsext = pool.strings[n.litId]
            inc n
          skipParRi n
        elif tag in ["input", "output", "args"]:
          var nested = 0
          while true:
            if n.kind == ParLe:
              inc nested
            elif n.kind == ParRi:
              dec nested
            tokens.add n.load()
            inc n
            if nested == 0: break
        else:
          quit "unsupported tag in `cmd` definition: " & tag
      else:
        quit "unsupported token in `cmd` definition: " & $n.kind
    tokens.addParRi()
    let cmdIdx = registerCommand(dag, cmdName, argsext)
    freeze tokens
    dag.commands[cmdIdx].tokens = tokens
  else:
    quit "expected symbol definition in `cmd` definition"

proc parseDoRule(n: var Cursor; dag: var Dag) =
  var cmdName: string
  if n.kind == Symbol:
    cmdName = pool.syms[n.symId]
    inc n
  elif n.kind == Ident:
    cmdName = pool.strings[n.litId]
    inc n
  else:
    quit "expected symbol or identifier in `do` rule"

  var inputs: seq[string] = @[]
  var outputs: seq[string] = @[]
  var args: seq[string] = @[]

  # Parse imports and results
  while n.kind != ParRi:
    if n.kind == ParLe:
      let tag = pool.tags[n.tag]
      inc n # skip opening paren

      if tag == "input":
        if n.kind == StringLit:
          inputs.add(pool.strings[n.litId])
          inc n
      elif tag == "output":
        if n.kind == StringLit:
          outputs.add(pool.strings[n.litId])
          inc n
      elif tag == "args":
        while n.kind != ParRi:
          if n.kind == StringLit:
            args.add(pool.strings[n.litId])
          inc n
      else:
        quit "unsupported tag in `do` definition: " & tag

      skipParRi n
    else:
      quit "expected `input` or `output` in `do` definition, but found: " & $n.kind

  discard addNode(dag, cmdName, inputs, outputs, args, ".args")

proc parseNifFile(filename: string; baseDir: sink string): Dag =
  ## Parse a .nif file and build the DAG
  result = Dag(baseDir: baseDir)

  if not fileExists(filename):
    quit "File not found: " & filename

  var stream = nifstreams.open(filename)
  defer: nifstreams.close(stream)

  discard processDirectives(stream.r)

  var buf = fromStream(stream)
  var n = beginRead(buf)
  defer: endRead(buf)

  # Parse (.nif24)(stmts ...)
  if n.kind == ParLe:
    inc n # skip opening paren
    while n.kind != ParRi:
      if n.kind == ParLe:
        case pool.tags[n.tag]
        of "cmd":
          inc n
          parseCommandDefinition(n, result)
        of "do":
          inc n
          parseDoRule(n, result)
        else:
          quit "unknown statement: " & pool.tags[n.tag]
        skipParRi n
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
  -j, --parallel        Enable parallel builds (for 'run' command)
  --makefile <name>     Output Makefile name (default: Makefile)
  --force               Force rebuild of all targets
  --verbose             Show verbose output
  --base:<dir>          Use <dir> as base directory for `.args` files.
                        If not set, no `.args` files are processed.
  --profile             Print timing profile of executed commands to stderr.

Examples:
  nifmake run build.nif
  nifmake makefile build.nif
  nifmake --makefile build.mk makefile build.nif
"""
  quit(0)

proc writeVersion() =
  echo "nifmake 0.2.0"
  quit(0)

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
      of "parallel", "j": opt.incl Parallel
      of "makefile": outputMakefile = val
      of "force": opt.incl Force
      of "verbose": opt.incl Verbose
      of "base": baseDir = val
      of "profile": opt.incl Profile
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

    var profile: ProfileData
    if Profile in opt:
      profile = ProfileData(cmdTime: initTable[string, tuple[sec: float, count: int]]())
      let parseStart = getMonoTime()
      var dag = parseNifFile(inputFile, baseDir)
      profile.parseTime = toSeconds(getMonoTime() - parseStart)
      if not runDag(dag, opt, addr profile):
        printProfile(profile)
        quit 1
      printProfile(profile)
    else:
      var dag = parseNifFile(inputFile, baseDir)
      if not runDag(dag, opt):
        quit 1

  of cmdMakefile:
    if inputFile == "":
      quit "Input file required for 'makefile' command"

    let dag = parseNifFile(inputFile, baseDir)
    generateMakefile(dag, outputMakefile)
    echo "Generated: ", outputMakefile

when isMainModule:
  main()

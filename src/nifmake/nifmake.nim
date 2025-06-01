#       Nifmake tool
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nifmake is a make-like tool that is used by Nimony to implement parallel
## and incremental compilation. Nifmake can run a dependency graph as specified
## by a .nif file or it can translate this file to a Makefile.

import std/[os, strutils, sequtils, tables, hashes, times, sets, parseopt, syncio, osproc]
import ".." / lib / [nifstreams, nifcursors, bitabs, lineinfos, nifreader]

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
    (output "a.1.nif")
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

  Node* = object
    cmdIdx*: int      # index into Dag.commands
    inputs*: seq[string]
    outputs*: seq[string]
    deps*: seq[int]   # node IDs this depends on
    state*: NodeState
    depth*: int       # depth in the DAG for parallel execution

  Dag* = object
    nodes*: seq[Node]
    nameToId*: Table[string, int]
    changeList*: HashSet[string]
    maxDepth*: int    # maximum depth in the DAG
    commands*: seq[Command]  # bidirectional mapping of commands

  CliCommand = enum
    cmdRun, cmdMakefile, cmdHelp, cmdVersion

proc skipParRi(n: var Cursor) =
  ## Helper to skip a closing parenthesis
  if n.kind == ParRi:
    inc n
  else:
    quit "Expected ')' but found: " & $n.kind

proc expandCommand(cmd: Command; inputs, outputs: seq[string]): string =
  result = ""
  var n = readonlyCursorAt(cmd.tokens, 0)
  while n.kind != ParRi:
    if result.len > 0 and result[^1] != ' ':
      result.add ' '
    case n.kind
    of StringLit:
      result.add pool.strings[n.litId]
    of ParLe:
      let tag = pool.tags[n.tag]
      var a = 0
      var b = 0
      inc n
      if n.kind == IntLit:
        a = pool.integers[n.intId]
        if a < 0: a = inputs.len + a
        inc n
      if n.kind == IntLit:
        b = pool.integers[n.intId]
        if b < 0: b = outputs.len + b
        inc n
      skipParRi n
      case tag
      of "input":
        for i in a..b:
          if i >= 0 and i < inputs.len:
            if result.len > 0 and result[^1] != ' ':
              result.add ' '
            result.add inputs[i].quoteShell
      of "output":
        for i in a..b:
          if i >= 0 and i < outputs.len:
            if result.len > 0 and result[^1] != ' ':
              result.add ' '
            result.add outputs[i].quoteShell
      else:
        result.add tag
    else:
      discard

proc registerCommand(dag: var Dag; cmdName: string): int =
  for i in 0..<dag.commands.len:
    if dag.commands[i].name == cmdName:
      return i
  result = dag.commands.len
  dag.commands.add Command(name: cmdName)

proc addNode(dag: var Dag; cmdName: string;
             inputs, outputs: sink seq[string]): int =
  ## Add a build node to the DAG and return its ID
  result = dag.nodes.len
  let cmdIdx = registerCommand(dag, cmdName)
  let node = Node(
    cmdIdx: cmdIdx,
    inputs: inputs,
    outputs: outputs,
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

proc addChangedFile(dag: var Dag; filename: string) =
  ## Add a changed file to the change list and mark for rebuild
  dag.changeList.incl(filename)

proc needsRebuild(node: Node): bool =
  ## Check if a node needs to be rebuilt
  result = false

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

type
  CmdStatus = enum
    Enqueued, Running, Finished

proc runDag(dag: var Dag; parallel: bool): bool =
  ## Execute the DAG in topological order
  result = true
  let sortedNodes = topologicalSort(dag)

  if parallel:
    # Group nodes by depth for parallel execution
    var nodesByDepth = newSeq[seq[int]](dag.maxDepth + 1)
    for nodeId in sortedNodes:
      let node = dag.nodes[nodeId]
      if needsRebuild(node) or node.outputs.anyIt(it in dag.changeList):
        nodesByDepth[node.depth].add(nodeId)

    # Execute each depth level in parallel
    for depth in 0..dag.maxDepth:
      var commands: seq[string] = @[]
      var nodeIds: seq[int] = @[]

      for nodeId in nodesByDepth[depth]:
        let node = dag.nodes[nodeId]
        echo "Building: ", node.outputs.join(", ")
        let expandedCmd = expandCommand(dag.commands[node.cmdIdx], node.inputs, node.outputs)
        echo "Command: ", expandedCmd
        commands.add(expandedCmd)
        nodeIds.add(nodeId)

      if commands.len > 0:
        var progress = newSeq[CmdStatus](commands.len)
        proc beforeRunEvent(idx: int) = progress[idx] = Running
        proc afterRunEvent(idx: int, p: Process) = progress[idx] = Finished

        let maxExitCode = execProcesses(commands, beforeRunEvent = beforeRunEvent, afterRunEvent = afterRunEvent)
        if maxExitCode != 0:
          for i, p in pairs(progress):
            if p == Running:
              echo "Error: Command failed: ", commands[i]
            return false
  else:
    # Sequential execution
    for nodeId in sortedNodes:
      let node = dag.nodes[nodeId]
      if needsRebuild(node) or node.outputs.anyIt(it in dag.changeList):
        echo "Building: ", node.outputs.join(", ")
        let expandedCmd = expandCommand(dag.commands[node.cmdIdx], node.inputs, node.outputs)
        echo "Command: ", expandedCmd
        if not executeCommand(expandedCmd):
          echo "Error: Command failed: ", expandedCmd
          return false
      else:
        echo "Up to date: ", node.outputs.join(", ")

proc generateMakefile(dag: Dag; filename: string) =
  ## Generate a Makefile from the DAG
  var content = "# Generated by nifmake\n\n"
  content.add ".PHONY: all clean\n\n"

  # Add all target
  content.add "all:"
  for node in dag.nodes:
    for output in node.outputs:
      content.add " " & output.quoteShell
  content.add "\n\n"

  # Add rules for each node
  for node in dag.nodes:
    # Target line
    content.add node.outputs.join(" ")
    content.add ":"
    for input in node.inputs:
      content.add " " & input.quoteShell
    content.add "\n"

    # Command line
    content.add "\t" & dag.commands[node.cmdIdx].name & "\n\n"

  # Add clean target
  content.add "clean:\n"
  content.add "\trm -f"
  for node in dag.nodes:
    for output in node.outputs:
      content.add " " & output.quoteShell
  content.add "\n"

  writeFile(filename, content)

proc parseCommandDefinition(n: var Cursor; dag: var Dag) =
  if n.kind == SymbolDef:
    let cmdName = pool.syms[n.symId]
    inc n

    var tokens = createTokenBuf(4)
    while n.kind != ParRi:
      case n.kind
      of StringLit:
        tokens.add n.load()
        inc n
      of ParLe:
        let tag = pool.tags[n.tag]
        if tag in ["input", "output"]:
          discard
        else:
          quit "unsupported tag in `cmd` definition: " & tag
        while true:
          let atEnd = n.kind == ParRi
          tokens.add n.load()
          inc n
          if atEnd: break
      else:
        quit "unsupported token in `cmd` definition: " & $n.kind
    let cmdIdx = registerCommand(dag, cmdName)
    freeze tokens
    dag.commands[cmdIdx].tokens = tokens
  else:
    quit "expected symbol definition in `cmd` definition"

proc parseDoRule(n: var Cursor; dag: var Dag) =
  if n.kind == Symbol:
    let cmdName = pool.syms[n.symId]
    inc n

    var inputs: seq[string] = @[]
    var outputs: seq[string] = @[]

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
        else:
          quit "unsupported tag in `do` definition: " & tag

        # Skip to closing paren
        while n.kind != ParRi:
          inc n
        inc n
      else:
        inc n

    if outputs.len > 0:
      discard addNode(dag, cmdName, inputs, outputs)

proc parseNifFile(filename: string): Dag =
  ## Parse a .nif file and build the DAG
  result = Dag()

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
  run <file.nif>      - Execute the build graph
  makefile <file.nif> - Generate Makefile from build graph
  help                - Show this help
  version             - Show version

Options:
  -j, --parallel      - Enable parallel builds (for 'run' command)
  --makefile <name>   - Output Makefile name (default: Makefile)
  --changed <file>    - Mark file as changed for incremental builds

Examples:
  nifmake run build.nif
  nifmake makefile build.nif
  nifmake --makefile build.mk makefile build.nif
"""
  quit(0)

proc writeVersion() =
  echo "nifmake 0.1.0"
  quit(0)

proc main() =
  var
    cmd = cmdHelp
    inputFile = ""
    outputMakefile = "Makefile"
    parallel = false
    changedFiles: seq[string] = @[]

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
      of "parallel", "j": parallel = true
      of "makefile": outputMakefile = val
      of "changed": changedFiles.add(val)
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

    var dag = parseNifFile(inputFile)

    # Mark changed files
    for file in changedFiles:
      addChangedFile(dag, file)

    if not runDag(dag, parallel):
      quit "Build failed"

  of cmdMakefile:
    if inputFile == "":
      quit "Input file required for 'makefile' command"

    let dag = parseNifFile(inputFile)
    generateMakefile(dag, outputMakefile)
    echo "Generated: ", outputMakefile

when isMainModule:
  main()

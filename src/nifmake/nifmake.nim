#       Nifmake tool
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nifmake is a make-like tool that is used by Nimony to implement parallel
## and incremental compilation. Nifmake can run a dependency graph as specified
## by a .nif file or it can translate this file to a Makefile.

import std/[os, strutils, sequtils, tables, hashes, times, sets, parseopt, syncio]
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
  (do "nifler $input $output"
    (import "a.nim")
    (incl "a_include.nim")
    (result "a.1.nif")
  )
)
```

]#

type
  Node* = object
    id*: int
    command*: string
    inputs*: seq[string]
    outputs*: seq[string]
    deps*: seq[int] # node IDs this depends on
    visited*: bool
    inStack*: bool

  Dag* = object
    nodes*: seq[Node]
    nameToId*: Table[string, int]
    changeList*: HashSet[string]

  Command = enum
    cmdRun, cmdMakefile, cmdHelp, cmdVersion

proc skipParRi(cursor: var Cursor) =
  ## Helper to skip a closing parenthesis
  if cursor.kind == ParRi:
    inc cursor
  else:
    quit "Expected ')' but found: " & $cursor.kind

proc expandVariables(command: string; inputs, outputs: seq[string]): string =
  ## Expand $input and $output variables in command strings
  result = command
  if inputs.len > 0:
    result = result.replace("$input", inputs[0].quoteShell)
  if outputs.len > 0:
    result = result.replace("$output", outputs[0].quoteShell)

  # Replace $inputs and $outputs with space-separated lists
  if "$inputs" in result:
    let inputList = inputs.map(quoteShell).join(" ")
    result = result.replace("$inputs", inputList)
  if "$outputs" in result:
    let outputList = outputs.map(quoteShell).join(" ")
    result = result.replace("$outputs", outputList)

proc addNode(dag: var Dag; command: string;
             inputs, outputs: sink seq[string]): int =
  ## Add a build node to the DAG and return its ID
  result = dag.nodes.len
  let expandedCommand = expandVariables(command, inputs, outputs)
  let node = Node(
    id: result,
    command: expandedCommand,
    inputs: inputs,
    outputs: outputs,
    deps: @[],
    visited: false,
    inStack: false
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

proc topologicalSort(dag: var Dag): seq[int] =
  ## Perform topological sort on the DAG
  var sortedNodes: seq[int] = @[]

  proc visit(nodes: var seq[Node]; nodeId: int): bool =
    if nodes[nodeId].inStack:
      # Cycle detected
      return false

    if nodes[nodeId].visited:
      return true

    nodes[nodeId].inStack = true

    for depId in nodes[nodeId].deps:
      if not visit(nodes, depId):
        return false

    nodes[nodeId].inStack = false
    nodes[nodeId].visited = true
    sortedNodes.add(nodeId)
    return true

  for i in 0..<dag.nodes.len:
    if not dag.nodes[i].visited:
      if not visit(dag.nodes, i):
        quit "Circular dependency detected in build graph"

  return sortedNodes

proc executeCommand(command: string): bool =
  ## Execute a shell command and return success status
  try:
    let exitCode = execShellCmd(command)
    result = exitCode == 0
  except:
    result = false

proc runDAG(dag: var Dag; parallel: bool = false): bool =
  ## Execute the DAG in topological order
  result = true
  let sortedNodes = topologicalSort(dag)

  for nodeId in sortedNodes:
    let node = dag.nodes[nodeId]

    if needsRebuild(node) or node.outputs.anyIt(it in dag.changeList):
      echo "Building: ", node.outputs.join(", ")
      echo "Command: ", node.command

      if not executeCommand(node.command):
        echo "Error: Command failed: ", node.command
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
    content.add "\t" & node.command & "\n\n"

  # Add clean target
  content.add "clean:\n"
  content.add "\trm -f"
  for node in dag.nodes:
    for output in node.outputs:
      content.add " " & output.quoteShell
  content.add "\n"

  writeFile(filename, content)

# Simple NIF parsing without heavy dependencies
proc parseNifFile(filename: string): Dag =
  ## Parse a .nif file and build the DAG
  result = Dag()

  if not fileExists(filename):
    quit "File not found: " & filename

  var stream = nifstreams.open(filename)
  defer: nifstreams.close(stream)

  discard processDirectives(stream.r)

  # Parse using cursor API
  var buf = fromStream(stream)
  var cursor = beginRead(buf)
  defer: endRead(buf)

  # Parse (.nif24)(stmts ...)
  if cursor.kind == ParLe:
    inc cursor # skip opening paren

    while cursor.kind != ParRi:
      if cursor.kind == ParLe:
        # Check if this is a "do" statement
        if pool.tags[cursor.tag] == "do":
          inc cursor # skip opening paren

          # Get command string
          if cursor.kind == StringLit:
            let command = pool.strings[cursor.litId]
            inc cursor

            var inputs: seq[string] = @[]
            var outputs: seq[string] = @[]

            # Parse imports and results
            while cursor.kind != ParRi:
              if cursor.kind == ParLe:
                let tag = pool.tags[cursor.tag]
                inc cursor # skip opening paren

                if tag == "import" or tag == "incl":
                  if cursor.kind == StringLit:
                    inputs.add(pool.strings[cursor.litId])
                    inc cursor
                elif tag == "result":
                  if cursor.kind == StringLit:
                    outputs.add(pool.strings[cursor.litId])
                    inc cursor

                # Skip to closing paren
                while cursor.kind != ParRi:
                  inc cursor
                inc cursor
              else:
                inc cursor

            if outputs.len > 0:
              discard addNode(result, command, inputs, outputs)
        else:
          # Skip non-do statements
          while cursor.kind != ParRi:
            inc cursor

        # Skip closing paren
        if cursor.kind == ParRi:
          inc cursor
      else:
        inc cursor

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

    if not runDAG(dag, parallel):
      quit("Build failed", 1)

  of cmdMakefile:
    if inputFile == "":
      quit "Input file required for 'makefile' command"

    let dag = parseNifFile(inputFile)
    generateMakefile(dag, outputMakefile)
    echo "Generated: ", outputMakefile

when isMainModule:
  main()


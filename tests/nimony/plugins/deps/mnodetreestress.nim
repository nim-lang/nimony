import plugins

proc addEcho(t: var NifBuilder; info: LineInfo; value: string) =
  t.withTree CallS, info:
    t.addIdent "echo"
    t.addStrLit value

proc addEcho(t: var NifBuilder; info: LineInfo; value: NifCursor) =
  t.withTree CallS, info:
    t.addIdent "echo"
    t.addSubtree(value)

proc makeProgram(info: LineInfo): NifCursor =
  var tree = createTree()
  tree.withTree StmtsS, info:
    tree.addEcho(info, "alpha")
    tree.addEcho(info, "beta")
    tree.addEcho(info, "gamma")
  result = snapshot(tree)

proc tr(n: NifCursor): NifBuilder =
  var input = pluginCallArgs(n)

  let info = input.info
  var source = makeProgram(info)

  var first = firstChild(source)              # at "alpha"

  var second = firstChild(source)
  skip second                                 # at "beta"

  var beta = second

  var empty: NifCursor
  empty = beta

  var third = firstChild(source)
  skip third
  skip third                                  # at "gamma"
  # Reassigning an existing reader should release the old lease and acquire the new one.
  second = third

  var copiedTree = createTree()
  copiedTree.withTree StmtsS, info:
    copiedTree.addEcho(info, input)
    copiedTree.takeTree(first)
    copiedTree.takeTree(empty)
    copiedTree.takeTree(second)

  var copied = snapshot(copiedTree)

  var scratchTree = createTree()
  scratchTree.withTree StmtsS, info:
    scratchTree.addEcho(info, "delta")

  var scratch = firstChild(snapshot(scratchTree))

  var reusableTree = createTree()
  reusableTree.addEcho(info, "epsilon")

  var earlierEcho = snapshot(reusableTree)
  reusableTree.addEcho(info, "zeta")

  var laterEcho = snapshot(reusableTree)
  skip laterEcho

  var resultTree = createTree()
  resultTree.withTree StmtsS, info:
    var it = firstChild(copied)
    resultTree.takeTree(it)
    resultTree.takeTree(it)
    resultTree.takeTree(it)
    resultTree.takeTree(it)
    resultTree.takeTree(scratch)
    resultTree.takeTree(earlierEcho)
    resultTree.takeTree(laterEcho)
  result = resultTree

var inp = loadPluginInput()
saveTree tr(inp)

import nimonyplugins

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
  var input = n
  if input.stmtKind == StmtsS:
    inc input

  let info = input.info
  var source = makeProgram(info)

  var first = source
  inc first

  var second = source
  inc second
  skip second

  var beta = second

  var empty: NifCursor
  empty = beta

  var third = source
  inc third
  skip third
  skip third
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

  var scratch = snapshot(scratchTree)
  inc scratch

  var reusableTree = createTree()
  reusableTree.addEcho(info, "epsilon")

  var earlierEcho = snapshot(reusableTree)
  reusableTree.addEcho(info, "zeta")

  var laterEcho = snapshot(reusableTree)
  skip laterEcho

  var resultTree = createTree()
  resultTree.withTree StmtsS, info:
    var it = copied
    inc it
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

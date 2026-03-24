import nimonyplugins

proc addEcho(t: Tree; info: LineInfo; value: string) =
  t.withTree CallS, info:
    t.addIdent "echo"
    t.addStrLit value

proc addEcho(t: Tree; info: LineInfo; value: Node) =
  t.withTree CallS, info:
    t.addIdent "echo"
    var copy = value
    t.takeTree(copy)

proc makeProgram(info: LineInfo): Node =
  var tree = createTree()
  tree.withTree StmtsS, info:
    tree.addEcho(info, "alpha")
    tree.addEcho(info, "beta")
    tree.addEcho(info, "gamma")
  result = freeze(tree)

proc tr(n: Node): Tree =
  var input = n
  if input.stmtKind == StmtsS:
    inc input
  input = input

  let info = input.info
  var source = makeProgram(info)

  var first = source
  inc first
  first = first

  var second = source
  inc second
  skip second

  var beta = second
  beta = beta

  var empty: Node
  empty = empty
  empty = beta

  var third = source
  inc third
  skip third
  skip third
  second = third
  third = third

  var copiedTree = createTree()
  copiedTree.withTree StmtsS, info:
    copiedTree.addEcho(info, input)
    copiedTree.takeTree(first)
    copiedTree.takeTree(empty)
    copiedTree.takeTree(second)

  var copied = freeze(copiedTree)
  copied = copied

  var scratchTree = createTree()
  scratchTree.withTree StmtsS, info:
    scratchTree.addEcho(info, "delta")

  var scratch = freeze(scratchTree)
  inc scratch
  scratch = scratch

  var resultTree = createTree()
  resultTree.withTree StmtsS, info:
    var it = copied
    inc it
    resultTree.takeTree(it)
    resultTree.takeTree(it)
    resultTree.takeTree(it)
    resultTree.takeTree(it)
    resultTree.takeTree(scratch)
  result = resultTree

var inp = loadTree()
saveTree tr(inp)

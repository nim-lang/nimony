import nimonyplugins

proc makeFrozen(info: LineInfo): Node =
  var t = createTree()
  t.withTree StmtsS, info:
    t.addStrLit "before"
    t.addStrLit "after"
  result = freeze(t)

proc tr(n: Node): Tree =
  let info = n.info
  var cursor = makeFrozen(info)
  inc cursor
  inc cursor

  result = createTree()
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.takeTree cursor

var inp = loadTree()
saveTree tr(inp)

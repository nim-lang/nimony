import nimonyplugins

proc tr(n: Node): Tree =
  let tree = n.treeRepr
  let lispFlat = n.lispRepr
  let lispIndented = n.lispRepr(indented = true)

  result = createTree()
  let info = n.info
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit tree
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit lispFlat
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit lispIndented

var inp = loadTree()
saveTree tr(beginRead inp)

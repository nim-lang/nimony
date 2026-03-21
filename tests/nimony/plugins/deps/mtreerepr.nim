import nimonyplugins

proc tr(n: Node): Tree =
  let raw = n.toString(false)
  let tree = n.treeRepr

  result = createTree()
  let info = n.info
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit raw
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit tree

var inp = loadTree()
saveTree tr(beginRead inp)

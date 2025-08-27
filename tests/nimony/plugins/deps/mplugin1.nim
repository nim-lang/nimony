
import nimonyplugins

proc tr(n: Node): Tree =
  result = createTree()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.takeTree n

var inp = loadTree()
saveTree tr(beginRead inp)

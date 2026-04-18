
import nimonyplugins

proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.takeTree n

var inp = loadPluginInput()
saveTree tr(inp)

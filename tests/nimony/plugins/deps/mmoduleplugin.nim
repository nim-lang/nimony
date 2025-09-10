
include ".." / ".." / ".." / ".." / src / lib / nifprelude
import nimonyplugins

proc tr(n: Node): Tree =
  result = createTree()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.withTree StmtsS, info:
    while n.kind != ParRi:
      case n.kind
      of ParLe:
        case n.stmtKind
        of BlockS:
          skip n
        else:
          result.takeTree n
      else:
        result.takeTree n

var inp = loadTree()
saveTree tr(beginRead inp)

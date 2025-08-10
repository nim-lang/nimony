
import std / os

include ".." / ".." / ".." / ".." / src / lib / nifprelude
import ".." / ".." / ".." / ".." / src / nimony / nimony_model

proc tr(n: Cursor): TokenBuf =
  result = createTokenBuf()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.addParLe StmtsS, info
  
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

  result.addParRi()

let input = os.paramStr(1)
let output = os.paramStr(2)
var inp = nifstreams.open(input)
var buf = fromStream(inp)

let outp = tr(beginRead buf)

writeFile output, toString(outp)

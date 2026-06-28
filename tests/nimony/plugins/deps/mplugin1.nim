import plugins
import std / assertions

proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  var head = callArgs(n)
  assert head.kind == StrLit
  assert head.info.isValid
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.takeTree head

var inp = loadPluginInput()
saveTree tr(inp)

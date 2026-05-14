# Nimony-compiled mirror of `mplugin1.nim`. Identical logic, just the
# `import` switched. Selected via `.plugin: ("path", "nimony")`.

import nim3plugins

proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  var head = if n.stmtKind == StmtsS: firstChild(n) else: n
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.takeTree head

var inp = loadPluginInput()
saveTree tr(inp)

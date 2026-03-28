import std / [os, syncio]
import nimonyplugins

proc tr(n: Node): Tree =
  var arg = n
  if arg.stmtKind == StmtsS:
    inc arg
  result = errorTree("plugin-authored error", arg)
  discard errorTree("synthetic error", default(Node))

var inp = loadPluginInput()
writeFile os.paramStr(2), renderTree(tr(inp))

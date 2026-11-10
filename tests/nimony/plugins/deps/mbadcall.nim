import plugins

# Builds `takesInt("not an int")`, every node at the input's location.
proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "takesInt"
      result.addStrLit "not an int"

var inp = loadPluginInput()
saveTree tr(inp)

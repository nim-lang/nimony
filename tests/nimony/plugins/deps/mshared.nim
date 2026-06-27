import plugins

# A single shared plugin that dispatches on which template invoked it.
proc dispatch(n: NifCursor): NifBuilder =
  let info = n.info
  let mode = pluginName(n)
  var args = pluginCallArgs(n)
  result = createTree()
  result.withTree StmtsS, info:
    case mode
    of "sayConst":
      result.withTree ConstS, info:
        result.addIdent "Produced"
        result.addEmptyNode3()
        result.addIntLit 42
    of "sayEcho":
      result.withTree CallS, info:
        result.addIdent "echo"
        result.addStrLit "dispatched sayEcho"
    else:
      result = errorTree("unknown template name: " & mode)

let input = loadPluginInput()
saveTree dispatch(input)

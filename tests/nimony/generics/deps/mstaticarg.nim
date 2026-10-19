import plugins

proc transform(root: NifCursor): NifBuilder =
  var arg = callArgs(root)
  result = createTree()
  result.withTree StmtsS, root.info:
    result.withTree CallS, root.info:
      result.addIdent "echo"
      result.addStrLit "got: " & renderNode(arg)

let input = loadPluginInput()
saveTree transform(input)

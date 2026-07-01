import plugins

proc expand(n: NifCursor): NifBuilder =
  let info = n.info
  var arg = callArgs(n)
  let generated = genSym()
  let copied = genSym()

  result = createTree()
  result.withTree StmtsS, info:
    result.withTree LetS, info:
      result.addSymDef generated, info
      result.addEmptyNode3(info)
      result.takeTree arg
    result.withTree LetS, info:
      result.addSymDef copied, info
      result.addEmptyNode3(info)
      result.addSymUse generated, info
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addSymUse copied, info

let input = loadPluginInput()
saveTree expand(input)

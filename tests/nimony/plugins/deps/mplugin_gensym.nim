import plugins

proc expand(n: NifCursor): NifBuilder =
  let info = n.info
  var arg = callArgs(n)
  # The compiler resolves this handle independently for every insertion.
  let generated = genSym("generated")

  result = createTree()
  result.withTree StmtsS, info:
    result.withTree LetS, info:
      result.addSymDef generated, info
      result.addEmptyNode3(info)
      result.takeTree arg
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addSymUse generated, info

let input = loadPluginInput()
saveTree expand(input)

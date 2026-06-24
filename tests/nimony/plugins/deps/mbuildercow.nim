import plugins

proc addEcho(t: var NifBuilder; info: LineInfo; value: string) =
  t.withTree CallS, info:
    t.addIdent "echo"
    t.addStrLit value

proc tr(n: NifCursor): NifBuilder =
  var input = templateArgs(n)

  let info = input.info

  var original = createTree()
  original.addEcho(info, "original")

  var copy = original
  copy.addEcho(info, "copy")

  var originalNode = snapshot(original)
  var copyNode = snapshot(copy)

  result = createTree()
  result.withTree StmtsS, info:
    result.addEcho(info, "input")
    result.takeTree(originalNode)
    result.takeTree(copyNode)
    result.takeTree(copyNode)

var inp = loadPluginInput()
saveTree tr(inp)

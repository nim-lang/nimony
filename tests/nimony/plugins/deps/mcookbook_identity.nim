import plugins

proc transform(input: NifCursor): NifBuilder =
  var expression = callArgs(input)
  result = createTree()
  result.takeTree expression

saveTree transform(loadPluginInput())

import plugins

# A module plugin that returns the module unchanged.
proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  var n = n
  result.takeTree n

saveTree tr(loadPluginInput())

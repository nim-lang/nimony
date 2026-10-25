import plugins

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  result.withTree StmtsS, n.info:
    var c = firstChild(n)
    while c.hasMore:
      result.takeTree c

saveTree transform(loadPluginInput())

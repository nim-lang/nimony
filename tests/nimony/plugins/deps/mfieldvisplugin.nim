import plugins

# Echoes the call-site argument back inside a `discard`. The argument is the
# caller's own code; if it names a private field of another module's type it
# must still be rejected. Regression plugin for issue #1988.
proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  var args = callArgs(n)
  result.withTree StmtsS, info:
    result.withTree DiscardS, info:
      result.addSubtree args

var inp = loadPluginInput()
saveTree transform(inp)

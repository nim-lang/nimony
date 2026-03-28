import std / [os, syncio]
import nimonyplugins

proc tr(n: Node): Tree =
  discard n
  result = nif("""(add "oops" +1 +2)""")

var inp = loadPluginInput()
writeFile os.paramStr(2), renderTree(tr(inp))

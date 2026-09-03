# Plugin for the incremental suite's dependency-tracking phases: it reads a
# data file NO part of its input mentions, and reports the read with
# `dependsOn` so the compiler knows to re-run it when that file changes
# (nim-lang/nimony#1378).
import plugins
import std / [syncio, os]

proc tr(n: NifCursor): NifBuilder =
  var content = "plugin-data-missing"
  try:
    let path = getCurrentDir() / "tests" / "incremental" / "plugindata.txt"
    dependsOn path
    content = readFile(path)
  except:
    discard
  result = createTree()
  result.withTree StmtsS, n.info:
    result.withTree CallS, n.info:
      result.addIdent "echo"
      result.addStrLit content

var inp = loadPluginInput()
saveTree tr(inp)

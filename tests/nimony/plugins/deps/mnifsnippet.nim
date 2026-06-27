# Plugin that emits four `echo` calls — exercises the basic builder API
# (the prior version used the now-removed `~` / `%~` string-template DSL).

import plugins

proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  let head = pluginCallArgs(n)

  result.withTree StmtsS, info:
    # echo <input>
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addSubtree head
    # echo "seen"
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit "seen"
    # echo 17
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addIntLit 17
    # echo "done"
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit "done"

var inp = loadPluginInput()
saveTree tr(inp)

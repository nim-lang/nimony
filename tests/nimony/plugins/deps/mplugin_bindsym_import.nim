# Binds symbols of a module that only the plugin imports (#2639).
import std / syncio
import plugins
import mbindsym_helper

proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.bindSym "echo"
      result.withTree CallX, info:
        result.bindSym "hello"
    result.withTree CallS, info:
      result.bindSym "echo"
      result.withTree CallX, info:
        result.bindSym "=="
        result.addStrLit "x"
        result.addStrLit "x"

saveTree tr(loadPluginInput())

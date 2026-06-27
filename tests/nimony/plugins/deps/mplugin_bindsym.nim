# Plugin that uses `bindSym` to emit a hygienic reference to `echo` from
# `std/syncio`. At plugin sem-time (when nimony compiles this), `bindSym`
# resolves "echo" against the plugin module's def-scope (which imports
# `std/syncio`) and folds to the full symbol name. The plugin then writes
# that name out via `addSymUse`, so the resulting NIF carries a resolved
# `Symbol` token — the user's call site can't shadow it.

import std / syncio                       # makes `echo` visible at def-site
import plugins

proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  var head = callArgs(n)
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.bindSym "echo"
      result.takeTree head

var inp = loadPluginInput()
saveTree tr(inp)

# Plugin that uses `bindSym` against an overloaded name. `add` has multiple
# overloads in `system` (string+char, string+string, seq+T) — bindSym at
# plugin sem time emits a `(cchoice add.0.X add.1.X …)` subtree which is
# parsed at runtime and appended into the output via `bindSymHelper`. Sem at
# the call site then resolves the choice against the actual argument types.

import std / [syncio, assertions]
import plugins

# The plugin's input is `(call buf "literal")` — we rewrite it to an `add`
# call against the bound choice and prepend the user's `echo buf` for the
# round-trip check.
proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  # Input shape: (stmts <template-name> <bufRef> <strLit>) — the two template
  # args flattened into a stmts container, preceded by the invoked template's
  # name. Re-emit as
  #   (stmts (asgn <bufRef> (call (cchoice & & &) <bufRef> <strLit>)))
  # where the cchoice resolves to system's `&(string, string)` at call-site
  # sem. We use `&` so all three overloads live in `system` — overloads from
  # the plugin's def-site that the user's compile can't import would
  # otherwise leak into the choice and break cross-module resolution.
  assert n.exprKind == CallX
  var args = pluginCallArgs(n)
  result.withTree StmtsS, info:
    result.withTree AsgnS, info:
      result.takeTree args  # bufRef as the asgn lhs
      args = pluginCallArgs(n)  # rewind to read both args again for the rhs
      result.withTree CallS, info:
        result.bindSym "&"
        result.takeTree args
        result.takeTree args

var inp = loadPluginInput()
saveTree tr(inp)

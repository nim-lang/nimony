import plugins
import std / assertions

# `NifBuilder` is an alias for `TokenBuf`, so a builder `createTree()` did not
# mint has nil pools. Both shapes below used to die as a bare
# `[Assertion Failure]` — no file, no line — inside `openTagEscaped`, because
# `nifcore.ensurePools` minted a PRIVATE pool for them and a fresh pool
# declares no escape tag. A plugin process must declare its own pools as the
# nifcore default the way the compiler's `nifpools` does; `plugins.nim` never
# did.

type Ctx = object
  outBuf: NifBuilder      ## an object field: never passed through createTree
  depth: int

proc tr(n: NifCursor): NifBuilder =
  let info = n.info
  var head = callArgs(n)

  # shape 1: an object field
  var c = Ctx(depth: 0)
  c.outBuf.withTree StmtsS, info:
    c.outBuf.withTree CallS, info:
      c.outBuf.addIdent "echo"
      c.outBuf.takeTree head
  assert not c.outBuf.isEmpty

  # shape 2: `default(...)`, and the raw appenders re-exported from nifcore
  var d = default(NifBuilder)
  d.addIntLit 7
  assert renderTree(d) == "7"

  result = ensureMove c.outBuf

var inp = loadPluginInput()
saveTree tr(inp)

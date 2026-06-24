# Plugin that exercises the various `add*Lit` overloads on `NifBuilder`.
# Replaces the prior `~` / `%~` tilde-DSL version, which inflated the API
# surface and obscured line-info handling.

import plugins

proc tr(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  let head = templateArgs(n)

  result.withTree StmtsS, info:
    # echo "seen"   — built via an explicit ident head and a string literal arg
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit "seen"
    # echo true
    result.withTree CallS, info:
      result.addIdent "echo"
      result.withTree TrueX, info:
        discard
    # echo 'Z'
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addCharLit 'Z'
    # echo 17 (BiggestInt)
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addIntLit BiggestInt(17)
    # echo 18'u32
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addUIntLit BiggestUInt(18'u32)
    # echo 2.5'f32
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addFloatLit BiggestFloat(2.5'f32)
    # echo "prepared"
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addStrLit "prepared"
    # echo <input>
    result.withTree CallS, info:
      result.addIdent "echo"
      result.addSubtree head

var inp = loadPluginInput()
saveTree tr(inp)

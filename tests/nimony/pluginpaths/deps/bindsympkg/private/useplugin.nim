import plugins
import runtime

proc generate*(input: NifCursor): NifBuilder =
  let arg = templateArgs(input)
  result = createTree()
  result.withTree CallX, NoLineInfo:
    result.bindSym("accept")
    result.addSubtree(arg)

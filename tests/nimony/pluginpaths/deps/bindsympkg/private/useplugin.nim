import plugins
import runtime

proc generate*(input: NifCursor): NifBuilder =
  let arg = firstChild(input)
  result = createTree()
  result.withTree CallX, NoLineInfo:
    result.bindSym("accept")
    result.addSubtree(arg)

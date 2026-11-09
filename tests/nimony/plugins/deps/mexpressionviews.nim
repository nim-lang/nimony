import std / assertions
import plugins

proc transform(input: NifCursor): NifBuilder =
  var argument = callArgs(input)
  let before = renderNode(argument)
  let call = asCall(argument)
  assert call.fn.hasMore
  var count = 0
  for a in args(call):
    inc count
    if a.exprKind == DotX:
      let dot = asDot(a)
      assert dot.obj.hasMore
      assert dot.field.kind == Symbol
      assert isValid(dot.field.info)
  let name = if call.fn.kind == Symbol: symBasename(call.fn.symId) else: "field"
  case name
  of "zero": assert count == 0
  of "one": assert count == 1
  of "two": assert count == 2
  of "field":
    assert call.fn.exprKind == DotX
    assert symBasename(asDot(call.fn).field.symId) == "fn"
    assert count == 1
  else: assert false, "unexpected callee"
  assert renderNode(argument) == before
  # Visiting the arguments a second time must give the same sequence
  var secondCount = 0
  for a in args(call): inc secondCount
  assert count == secondCount
  result = createTree()
  result.takeTree argument

saveTree transform(loadPluginInput())

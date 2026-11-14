import plugins

proc rejected(n: NifCursor): NifBuilder =
  ## A small syntactic rule: reject a direct call whose resolved name is blocked
  if n.exprKind == CallX:
    let call = asCall(n)
    if call.fn.kind == Symbol and symBasename(call.fn.symId) == "blocked":
      return errorTree("checked block may not call blocked", n)
  for child in n.children:
    let err = rejected(child)
    if not err.isEmpty: return err
  result = createTree()

proc transform(input: NifCursor): NifBuilder =
  var body = callArgs(input)
  let err = rejected(body)
  if not err.isEmpty: return err
  result = createTree()
  result.takeTree body

saveTree transform(loadPluginInput())

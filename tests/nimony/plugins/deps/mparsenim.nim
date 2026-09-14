import plugins, nimparser

proc tr(n: NifCursor): NifBuilder =
  var args = callArgs(n)
  let mode = identText(args)   # `stmt` or `expr`
  inc args
  var lit = args
  if lit.kind == TagLit: lit = firstChild(lit)   # `(suf "..." "T")`
  let code = stringValue(lit)
  var parsed = if mode == "stmt": parseStmt(code, args.info)
               else: parseExpr(code, args.info)
  result = createTree()
  result.takeTree parsed

var inp = loadPluginInput()
saveTree tr(inp)

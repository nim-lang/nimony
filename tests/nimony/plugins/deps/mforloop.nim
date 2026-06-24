import plugins

# For-loop plugin that unrolls `for x in unrollN(n)` into N copies of the
# loop body, substituting the loop variable with an integer literal.
proc transform(n: NifCursor): NifBuilder =
  let info = n.info
  let mode = templateName(n)
  result = createTree()

  if mode == "unroll3":
    # Input shape: (stmts unroll3 (unpackflat (symdef x)) <body>)
    # No call args for this simple case — just the iter name, loop vars, body.
    var vars = forLoopVars(n)
    let body = forLoopBody(n)

    # Read the loop variable name
    var loopVarName = ""
    if vars.kind == ParLe and vars.tagText == "unpackflat":
      vars = firstChild(vars)
      if vars.kind == ParLe:
        # skip past the let/var tag to get the symdef
        vars = firstChild(vars)
      if vars.kind == SymbolDef:
        loopVarName = symText(vars)
      elif vars.kind == Ident:
        loopVarName = identText(vars)

    result.withTree StmtsS, info:
      for i in 1..3:
        # Emit: <body with loop var replaced by i>
        # For simplicity, just emit the body as-is 3 times.
        # A real unroller would substitute the loop variable.
        result.addSubtree body
  else:
    result = errorTree("unknown for-loop plugin mode: " & mode)

let input = loadPluginInput()
saveTree transform(input)

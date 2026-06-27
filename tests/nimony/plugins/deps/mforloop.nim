import plugins

# For-loop plugin that unrolls `for x in unroll3()` into 3 copies of the
# loop body, substituting the loop variable with the literals 1, 2, 3.
#
# The substitution works on the *resolved loop-var symbol*: it matches a
# `Symbol` token by its `SymId`. That symbol only exists because the for-loop
# plugin now receives a fully typed body (the loop var is sem-checked against
# the iterator's element type before the plugin runs). If the body were still
# untyped, `x` would be a bare `Ident` with no `SymId` to match and the
# substitution would not fire.

proc emitSubst(o: var NifBuilder; n: var NifCursor; loopSym: SymId; val: int) =
  ## Copies `n` to `o`, replacing every use of `loopSym` with the literal `val`.
  if n.kind == ParLe:
    o.copyInto(n):
      while n.kind != ParRi:
        emitSubst(o, n, loopSym, val)
  elif n.kind == Symbol and n.symId == loopSym:
    o.addIntLit(val)
    skip n
  else:
    o.addSubtree(n)
    skip n

proc loopVarSym(n: NifCursor): SymId =
  ## Extracts the loop variable's symbol from the for-loop plugin input.
  ## Input shape: `(stmts <iter> <args...> (unpackflat (let (symdef x) …)) <body>)`.
  var vars = forLoopVars(n)
  if vars.kind == ParLe and vars.tagText == "unpackflat":
    vars = firstChild(vars)        # (let …)
    if vars.kind == ParLe:
      vars = firstChild(vars)      # (symdef x)
    if vars.kind == SymbolDef:
      return vars.symId
  result = default(SymId)

proc transform(n: NifCursor): NifBuilder =
  let info = n.info
  let mode = pluginName(n)
  result = createTree()

  if mode == "unroll3":
    let loopSym = loopVarSym(n)
    result.withTree StmtsS, info:
      for i in 1..3:
        var body = forLoopBody(n)
        emitSubst(result, body, loopSym, i)
  else:
    result = errorTree("unknown for-loop plugin mode: " & mode)

let input = loadPluginInput()
saveTree transform(input)

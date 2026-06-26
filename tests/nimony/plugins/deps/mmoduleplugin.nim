import plugins

proc trAux(t: var Replacer) =
  if t.isAtom:
    keep t, Any
  else:
    case t.stmtKind
    of BlockS:
      drop t, BlockS
    else:
      loopKeepTag t:
        trAux t

var t = loadReplacer()
loopKeepTag t:
  if t.kind == Ident:
    drop t, Any # skip the module name prefix
  else:
    trAux t
saveReplacer(t)

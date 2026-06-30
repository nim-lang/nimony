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
replaceHead t, StmtsS, t.info:
  while t.getCursor.hasMore:
    trAux t
saveReplacer(t)

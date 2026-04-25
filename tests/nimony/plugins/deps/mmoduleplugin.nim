
import nimonyplugins

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
  trAux t
saveReplacer(t)

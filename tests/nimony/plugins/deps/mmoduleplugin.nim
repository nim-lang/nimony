
import nimonyplugins

proc trAux(t: var Replacer) =
  if t.isAtom:
    keep t, Any
  else:
    case t.stmtKind
    of BlockS:
      drop t, BlockS
    else:
      intoLoop t:
        trAux t

var t = loadReplacer()
into t:
  loop t:
    trAux t
saveReplacer(t)

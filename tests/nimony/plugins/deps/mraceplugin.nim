# A plugin of its own, used by nothing else on purpose: `tpluginrace` needs a
# plugin executable that does NOT exist yet when the test starts building, so
# that its importers all reach the "build this plugin" path at once. Sharing
# `mmoduleplugin` would not do — an earlier test in the directory has already
# built that one, and there would be nothing left to race over.
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

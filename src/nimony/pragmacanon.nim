import std / [sets]
include nifprelude
import nimony_model, decls, asthelpers

type
  CheckedPragmas* = object
    pragmas: set[NimonyPragma]
    callconv: CallConv = NoCallConv
    idents: HashSet[StrId]  # idents of user pragmas and macro pragmas

proc isChecked*(c: var CheckedPragmas; n: Cursor; kind: SymKind): bool =
  ## Prevents duplicated pragmas by returning true when `CheckedPragmas` get the pragma that is given previously.
  result = false
  if not kind.isRoutine and callConvKind(n) != NoCallConv:
    result = true
  else:
    var n2 = n
    if n2.substructureKind == KvU or n2.exprKind == CallX:
      inc n2
    if (let pk = pragmaKind(n2); pk != NoPragma):
      if pk == CallConvP:
        # callConv: X is a calling convention wrapper, treat like a calling convention
        if not kind.isRoutine:
          result = true
        else:
          result = c.callconv != NoCallConv
          if not result:
            # peek at the value to get the actual calling convention
            var n3 = n2
            inc n3
            let cc = callConvKind(n3)
            if cc != NoCallConv:
              c.callconv = cc
      else:
        result = pk in c.pragmas
        if not result:
          c.pragmas.incl pk
    elif kind.isRoutine and (let cc = callConvKind(n2); cc != NoCallConv):
      result = c.callconv != NoCallConv
      if not result:
        c.callconv = cc
    elif (let ident = getIdent(n2); ident != StrId(0)):
      result = c.idents.containsOrIncl(ident)

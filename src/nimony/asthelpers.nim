import std / [tables, sets, syncio, formatfloat, assertions]
include ".." / lib / nifprelude
import nimony_model
import ".." / lib / symparser

proc takeUnquotedAtom(r: var string; c: var Cursor) =
  ## Append one leaf token's text (build-agnostic).
  if c.isIdent or c.isStringLit: r.add pool.strings[c.litId]
  elif c.isIntLit: r.addInt pool.integers[c.intId]
  elif c.isCharLit: r.add char(c.uoperand)
  elif c.isUIntLit: r.add $pool.uintegers[c.uintId]
  elif c.isFloatLit: r.addFloat pool.floats[c.floatId]
  else: r.add "<unexpected token>"
  inc c

proc takeUnquoted*(c: var Cursor): StrId =
  var r = ""
  when defined(useNifcore):
    # quoted content is flat in practice: read the atoms of the current subtree
    if c.isTagLit:
      c.into:
        while c.hasMore: takeUnquotedAtom(r, c)
    else:
      takeUnquotedAtom(r, c)
  else:
    var scopes: seq[Cursor] = @[]
    while true:
      case c.kind
      of ParLe:
        scopes.add c; c = sub(c)
      of ParRi:
        # the first close ends the walk (quoted content is flat in practice)
        if scopes.len > 0:
          c = scopes.pop; skip c
        else:
          inc c
        break
      of EofToken:
        r.add "<unexpected eof>"
        break
      of Ident, StringLit:
        r.add pool.strings[c.litId]
        inc c
      of IntLit:
        r.addInt pool.integers[c.intId]
        inc c
      of CharLit:
        let ch = char(c.uoperand)
        r.add ch
        inc c
      of UIntLit:
        r.add $pool.uintegers[c.uintId]
        inc c
      of FloatLit:
        r.addFloat pool.floats[c.floatId]
        inc c
      of UnknownToken, DotToken, Symbol, SymbolDef:
        r.add "<unexpected token>: " & $c.kind
        inc c
  assert r.len > 0
  result = getOrIncl(pool.strings, r)

proc takeIdent*(n: var Cursor): StrId =
  if n.isIdent:
    result = n.litId
    inc n
  elif n.isSymbol or n.isSymbolDef:
    let sym = pool.syms[n.symId]
    var isGlobal = false
    result = pool.strings.getOrIncl(extractBasename(sym, isGlobal))
    inc n
  elif n.isTagLit:
    if exprKind(n) in {OchoiceX, CchoiceX}:
      result = StrId(0)
      n.peekInto:
        if n.hasMore:
          result = takeIdent(n)
    elif exprKind(n) == QuotedX:
      result = takeUnquoted(n)
    else:
      result = StrId(0)
  else:
    result = StrId(0)

proc getIdent*(n: Cursor): StrId =
  var n = n
  result = takeIdent(n)

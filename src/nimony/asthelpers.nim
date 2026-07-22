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
  elif c.isFloatLit: r.addFloat c.floatVal
  else: r.add "<unexpected token>"
  inc c

proc takeUnquoted*(c: var Cursor): StrId =
  var r = ""
  # quoted content is flat in practice: read the atoms of the current subtree
  if c.isTagLit:
    c.into:
      while c.hasMore: takeUnquotedAtom(r, c)
  else:
    takeUnquotedAtom(r, c)
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

#
#
#        Lengc type navigator — nifcore port
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A type navigator that recomputes the type of a Leng expression, over
## **nifcore** cursors. This is the nifcore port of `lengc/typenav.nim`: same
## algorithm, same shared `models/leng_tags` enums, but built on `nifcoreparse`
## / `nifcdecl` and the nifcore `MainModule` (`shoggoth/nifmodules`) so symbol
## resolution is fully cross-module — `MainModule.getDeclOrNil` lazily loads
## foreign declarations on demand.

import std / [assertions, tables]
import ".." / "lib" / nifcoreparse        # re-exports nifcore + parseFromBuffer
import ".." / "lib" / nifcdecl              # stmtKind/exprKind/typeKind, tag enums
import ".." / "models" / tags               # *TagId ordinals for synthesis
import nifmodules                                   # MainModule, getDeclOrNil

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc isImportC*(m: var MainModule; n: Cursor): bool =
  if n.kind in {Symbol, SymbolDef}:
    let d = m.getDeclOrNil(n.symId)
    result = d != nil and d.isImport
  else:
    result = false

proc registerParams*(c: var MainModule; params: Cursor) =
  ## Register a routine's parameters in the current scope (their types feed
  ## `getType`). `params` is the `(params (param :name pragmas type) …)` node, or
  ## `.` for a parameterless routine.
  if params.kind != TagLit: return
  var p = params
  p.loopInto:
    if p.substructureKind == ParamU:
      let d = takeParamDecl(p)
      if d.name.kind == SymbolDef:
        registerLocal(c, d.name.symId, d.typ)
    else:
      skip p

# ---- synthesized types ----------------------------------------------------

proc createIntegralType*(c: var MainModule; name: string): Cursor =
  result = c.builtinTypes.getOrDefault(name, default(Cursor))
  if cursorIsNil(result):
    var buf = parseFromBuffer(name, "<builtin>", 8, c.pool, c.tags)
    c.mem.add ensureMove(buf)
    result = cursorAt(c.mem[c.mem.len-1], 0)
    c.builtinTypes[name] = result

proc ptrTypeOf(c: var MainModule; elem: Cursor): Cursor =
  var buf = createTokenBuf(4, c.pool, c.tags)
  buf.openTag TagId(ord(PtrTagId))
  buf.addSubtree elem
  buf.closeTag()
  c.mem.add ensureMove(buf)
  result = cursorAt(c.mem[c.mem.len-1], 0)

# ---- field lookup ---------------------------------------------------------

type
  FieldSelector* = enum
    FieldType, FieldPragmas

proc typeOfField*(c: var MainModule; n: var Cursor; fld: SymId;
                  sel = FieldType): Cursor =
  if n.substructureKind == FldU:
    let decl = takeFieldDecl(n)
    if decl.name.kind == SymbolDef and decl.name.symId == fld:
      result = if sel == FieldType: decl.typ else: decl.pragmas
    else:
      result = default(Cursor)
  else:
    result = default(Cursor)
    let tk = n.typeKind
    if tk in {ObjectT, UnionT}:
      n.into:
        var hasBase = false
        var baseSym = default(SymId)
        if tk == ObjectT:
          if n.kind == Symbol:
            hasBase = true
            baseSym = n.symId
          skip n  # inheritance reference
        var done = false
        while n.hasMore and not done:
          result = typeOfField(c, n, fld, sel)
          if not cursorIsNil(result): done = true
        while n.hasMore: skip n  # mop up if we broke early
        if cursorIsNil(result) and hasBase:
          let d = c.getDeclOrNil(baseSym)
          if d != nil and d.pos.stmtKind == TypeS:
            var baseBody = asTypeDecl(d.pos).body
            result = typeOfField(c, baseBody, fld, sel)

proc navigateToObjectBody*(c: var MainModule; n: Cursor): Cursor =
  var counter = 20
  result = n
  while counter > 0 and result.kind == Symbol:
    dec counter
    let d = c.getDeclOrNil(result.symId)
    if d != nil and d.pos.stmtKind == TypeS:
      result = asTypeDecl(d.pos).body
    else:
      break

# ---- the navigator --------------------------------------------------------

proc getTypeImpl(c: var MainModule; n: Cursor): Cursor =
  case n.kind
  of DotToken, Ident, SymbolDef:
    result = createIntegralType(c, "(err)")
  of Symbol:
    var it {.cursor.} = c.current
    while it != nil:
      let res = it.locals.getOrDefault(n.symId, default(Cursor))
      if not cursorIsNil(res):
        return res
      it = it.parent
    let d = c.getDeclOrNil(n.symId)
    if d != nil:
      result = getTypeImpl(c, d.pos)
    else:
      # importC types are not defined
      result = createIntegralType(c, "(err)")
  of IntLit:
    result = createIntegralType(c, "(i -1)")
  of UIntLit:
    result = createIntegralType(c, "(u -1)")
  of FloatLit:
    result = createIntegralType(c, "(f +64)")
  of StrLit:
    result = createIntegralType(c, "(aptr (c +8))")
  of CharLit:
    result = createIntegralType(c, "(c +8)")
  of ExtendedSuffix, LineInfoLit:
    result = createIntegralType(c, "(err)")
  of TagLit:
    case n.exprKind
    of SizeofC, AlignofC, OffsetofC:
      result = createIntegralType(c, "(i +8)")
    of InfC, NegInfC, NanC:
      result = createIntegralType(c, "(f +64)")
    of TrueC, FalseC, AndC, OrC, NotC, EqC, NeqC, LeC, LtC, ErrvC, OvfC:
      result = createIntegralType(c, "(bool)")
    of CallC:
      var procType = navigateToObjectBody(c, getTypeImpl(c, firstChild(n)))
      if procType.typeKind == ProctypeT or procType.symKind == ProcY:
        inc procType
        skip procType  # name
      if procType.typeKind == ParamsT:
        result = procType
        skip result  # skip the parameters, return type follows
      else:
        result = createIntegralType(c, "(err)")
    of AtC, PatC:
      var arrayType = navigateToObjectBody(c, getTypeImpl(c, firstChild(n)))
      # Descend to the element type only when the base really is an indexable
      # type. Otherwise the base did not resolve (an unresolved `Symbol` or the
      # `(err)` sentinel), and a blind `inc` would run the cursor off the end of
      # its buffer — a later `kind`/`typeKind` on that exhausted cursor crashes.
      if arrayType.typeKind in {ArrayT, FlexarrayT, PtrT, AptrT}:
        result = arrayType
        inc result  # into the element type (first child of (arr …)/(ptr …))
      else:
        result = createIntegralType(c, "(err)")
    of DotC:
      var a = firstChild(n)
      var objType = navigateToObjectBody(c, getTypeImpl(c, a))
      skip a  # skip the object
      let fld = a.symId
      if objType.typeKind in {ObjectT, UnionT}:
        result = typeOfField(c, objType, fld)
        if cursorIsNil(result):
          result = createIntegralType(c, "(err)")
      else:
        result = createIntegralType(c, "(err)")
    of DerefC:
      let x = getTypeImpl(c, firstChild(n))
      if x.typeKind == PtrT:
        result = firstChild(x)
      else:
        result = createIntegralType(c, "(err)")
    of AddrC:
      let x = getTypeImpl(c, firstChild(n))
      result = ptrTypeOf(c, x)
    of ConvC, CastC, AconstrC, OconstrC, BaseobjC:
      result = firstChild(n)
    of NegC, AddC, SubC, MulC, DivC, ModC, ShrC, ShlC,
       BitandC, BitorC, BitxorC, BitnotC:
      result = firstChild(n)
    of ParC:
      result = getTypeImpl(c, firstChild(n))
    of NilC:
      result = createIntegralType(c, "(ptr (void))")
    of SufC:
      result = createIntegralType(c, "(err)")
      var a = firstChild(n)
      skip a
      if a.kind in {StrLit, Ident}:
        let s = strVal(a, c.pool)
        if s.len > 0:
          if s[0] == 'i':
            result = createIntegralType(c, "(i " & s.substr(1) & ")")
          elif s[0] == 'u':
            result = createIntegralType(c, "(u " & s.substr(1) & ")")
          elif s[0] == 'f':
            result = createIntegralType(c, "(f " & s.substr(1) & ")")
    of NoExpr:
      case n.stmtKind
      of ProcS:
        result = n
        inc result  # ProcS token
        skip result # skip the name
      of GvarS, TvarS, ConstS, VarS:
        result = n
        inc result  # token
        skip result # skip the name
        skip result # skip the pragmas
      else:
        if n.substructureKind in {ParamU, FldU}:
          result = n
          inc result  # token
          skip result # skip the name
          skip result # skip the pragmas
        else:
          result = createIntegralType(c, "(err)")

proc getType*(c: var MainModule; n: Cursor; skipAliases = true): Cursor =
  result = getTypeImpl(c, n)
  if skipAliases:
    result = navigateToObjectBody(c, result)

proc getNominalType*(c: var MainModule; n: Cursor): Cursor =
  ## Arrays are nominal types in NIFC too, so this does not skip aliases.
  result = getTypeImpl(c, n)

proc lookupField*(c: var MainModule; typ: Cursor; fld: SymId): Cursor =
  var body = navigateToObjectBody(c, typ)
  result = typeOfField(c, body, fld)

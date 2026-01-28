#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A type navigator can recompute the type of an expression.

import std / [tables, assertions]
include "../lib" / nifprelude

import nifc_model, mangler

proc isImportC*(m: MainModule; n: Cursor): bool =
  result = n.kind in {Symbol, SymbolDef} and m.defs.getOrDefault(n.symId).extern != StrId(0)

proc createIntegralType*(m: var MainModule; name: string): Cursor =
  result = m.builtinTypes.getOrDefault(name)
  if cursorIsNil(result):
    var buf = nifcursors.parseFromBuffer(name, "<invalid>", 3)
    result = cursorAt(buf, 0)
    m.mem.add buf
    m.builtinTypes[name] = result

type
  FieldSelector* = enum
    FieldType, FieldPragmas

proc typeOfField*(m: var MainModule; n: var Cursor; fld: SymId; sel = FieldType): Cursor =
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
      inc n
      if tk == ObjectT:
        skip n # inheritance
      while n.kind != ParRi:
        result = typeOfField(m, n, fld, sel)
        if not cursorIsNil(result): break
      inc n

proc getTypeImpl(m: var MainModule; n: Cursor): Cursor =
  case n.kind
  of DotToken, Ident, SymbolDef:
    result = createIntegralType(m, "(err)")
  of Symbol:
    var it {.cursor.} = m.current
    while it != nil:
      let res = it.locals.getOrDefault(n.symId)
      if not cursorIsNil(res):
        return res
      it = it.parent
    let d = m.defs.getOrDefault(n.symId)
    if d.kind != NoSym:
      result = getTypeImpl(m, d.pos)
    else:
      # importC types are not defined
      result = createIntegralType(m, "(err)")
  of ParRi:
    bug "typenav: unexpected ParRi"
  of IntLit:
    result = createIntegralType(m, "(i -1)")
  of UIntLit:
    result = createIntegralType(m, "(u -1)")
  of FloatLit:
    result = createIntegralType(m, "(f +64)")
  of StringLit: result = createIntegralType(m, "(aptr (c +8))")
  of CharLit: result = createIntegralType(m, "(c +8)")
  of ParLe:
    case n.exprKind
    of SizeofC, AlignofC, OffsetofC: result = createIntegralType(m, "(i +8)")
    of InfC, NegInfC, NanC: result = createIntegralType(m, "(f +64)")
    of TrueC, FalseC, AndC, OrC, NotC, EqC, NeqC, LeC, LtC, ErrvC, OvfC:
      result = createIntegralType(m, "(bool)")
    of CallC:
      var procType = getTypeImpl(m, n.firstSon)
      if procType.typeKind == ProctypeT or procType.symKind == ProcY:
        inc procType
        skip procType # name
      if procType.typeKind == ParamsT:
        result = procType
        skip result # skip the parameters, return type follows!
      else:
        result = createIntegralType(m, "(err)")
    of AtC, PatC:
      let a = n.firstSon
      let arrayType = getTypeImpl(m, a)
      result = arrayType
      # array type is an alias
      if result.kind == Symbol:
        let d = m.defs.getOrDefault(result.symId)
        if d.kind != NoSym:
          let dd = d.pos
          if dd.stmtKind == TypeS:
            let decl = asTypeDecl(dd)
            result = decl.body
      inc result # (arr ...)
    of DotC:
      var a = n.firstSon
      var objType = getTypeImpl(m, a)
      skip a # skip the object
      let fld = a.symId
      var counter = 20
      while counter > 0 and objType.kind == Symbol:
        dec counter
        let d = m.defs.getOrDefault(objType.symId)
        if d.kind != NoSym:
          let dd = d.pos
          if dd.stmtKind == TypeS:
            let decl = asTypeDecl(dd)
            objType = decl.body

      if objType.typeKind in {ObjectT, UnionT}:
        result = typeOfField(m, objType, fld)
        if cursorIsNil(result):
          result = createIntegralType(m, "(err)")
      else:
        result = createIntegralType(m, "(err)")
    of DerefC:
      let x = getTypeImpl(m, n.firstSon)
      assert x.typeKind == PtrT
      result = x.firstSon
    of AddrC:
      let x = getTypeImpl(m, n.firstSon)
      var buf = createTokenBuf(4)
      buf.add parLeToken(PtrT, x.info)
      buf.addSubtree x
      buf.addParRi()
      result = cursorAt(buf, 0)
      m.mem.add ensureMove buf
    of ConvC, CastC, AconstrC, OconstrC, BaseobjC:
      result = n.firstSon
    of NegC, AddC, SubC, MulC, DivC, ModC, ShrC, ShlC, BitandC, BitorC, BitxorC, BitnotC:
      result = n.firstSon
    of ParC:
      result = getTypeImpl(m, n.firstSon)
    of NilC:
      result = createIntegralType(m, "(ptr (void))")
    of SufC:
      result = createIntegralType(m, "(err)")
      var a = n.firstSon
      skip a
      let s = pool.strings[a.litId]
      if s.len > 0:
        if s[0] == 'i':
          result = createIntegralType(m, "(i " & s.substr(1) & ")")
        elif s[0] == 'u':
          result = createIntegralType(m, "(u " & s.substr(1) & ")")
        elif s[0] == 'f':
          result = createIntegralType(m, "(f " & s.substr(1) & ")")

    of NoExpr:
      case n.stmtKind
      of ProcS:
        result = n
        inc result # ProcS token
        skip result # skip the name
      of GvarS, TvarS, ConstS, VarS:
        result = n
        inc result # token
        skip result # skip the name
        skip result # skip the pragmas
      else:
        if n.substructureKind in {ParamU, FldU}:
          result = n
          inc result # token
          skip result # skip the name
          skip result # skip the pragmas
        elif n.substructureKind == EfldU:
          # skip to its outer Enum declaration which is its type:
          result = n
          unsafeDec result
          while result.typeKind != EnumT: unsafeDec result
        else:
          bug "typenav: cannot get type of construct: " & $n.stmtKind
  else:
    result = createIntegralType(m, "(err)")

proc navigateToObjectBody*(m: var MainModule; n: Cursor): Cursor =
  var counter = 20
  result = n
  while counter > 0 and result.kind == Symbol:
    dec counter
    let d = m.defs.getOrDefault(result.symId)
    if d.kind != NoSym:
      let dd = d.pos
      if dd.stmtKind == TypeS:
        let decl = asTypeDecl(dd)
        result = decl.body
      else:
        break
    else:
      raiseAssert "could not load: " & pool.syms[result.symId]

proc getType*(m: var MainModule; n: Cursor; skipAliases = true): Cursor =
  result = getTypeImpl(m, n)
  if skipAliases:
    result = navigateToObjectBody(m, result)

proc getNominalType*(m: var MainModule; n: Cursor): Cursor =
  # arrays are nominal types in NIFC too! so we must not skip "aliases" here and
  # also exploit this to be able to look at its pragmas. Importc'ed arrays
  # are very special in that they don't have the `a` struct wrapper.
  result = getTypeImpl(m, n)

#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A type navigator can recompute the type of an expression.

import std / [strutils, tables, assertions]
include "../lib" / nifprelude

import nifc_model, mangler

proc isImportC*(m: Module; typ: Cursor): bool =
  result = typ.kind == Symbol and pool.syms[typ.symId].isImportC

proc createIntegralType*(m: var Module; name: string): Cursor =
  result = m.builtinTypes.getOrDefault(name)
  if cursorIsNil(result):
    var buf = nifcursors.parse(name, 3)
    result = cursorAt(buf, 0)
    m.mem.add buf
    m.builtinTypes[name] = result

proc getType*(m: var Module; n: Cursor): Cursor =
  case n.kind
  of DotToken, Ident, SymbolDef:
    result = createIntegralType(m, "(err)")
  of Symbol:
    let d = m.defs.getOrDefault(n.symId)
    if d.pos != 0:
      result = getType(m, m.code.cursorAt(d.pos))
    else:
      # importC types are not defined
      result = n
  of ParRi:
    bug "typenav: unexpected ParRi"
  of IntLit:
    result = createIntegralType(m, "(i -1)")
  of UIntLit:
    result = createIntegralType(m, "(u -1)")
  of FloatLit:
    result = createIntegralType(m, "(f 64)")
  of StrLit: result = createIntegralType(m, "(aptr (c 8))")
  of CharLit: result = createIntegralType(m, "(c 8)")
  of ParLe:
    case n.exprKind
    of SizeofC, AlignofC, OffsetofC: result = createIntegralType(m, "(i 8)")
    of InfC, NegInfC, NanC: result = createIntegralType(m, "(f 64)")
    of TrueC, FalseC, AndC, OrC, NotC, EqC, NeqC, LeC, LtC, ErrC:
      result = createIntegralType(m, "(bool)")
    of CallC:
      let procType = getType(m, t, n.firstSon)
      if procType.p != NodePos(0):
        assert t[procType.p].kind == ProcC
        result = typeFromPos asProcDecl(t, n).returnType
      else:
        result = procType # propagate error
    of AtC, PatC:
      let (a, _) = sons2(t, n)
      let arrayType = getType(m, t, a)
      result = elemType(t, arrayType)
    of DotC:
      let (_, fld, _) = sons3(t, n)
      result = getType(m, t, fld)
    of DerefC:
      let x = getType(m, t, n.firstSon)
      assert kind(t, x) == PtrC
      result = elemType(t, x)
    of AddrC:
      let x = getType(m, t, n.firstSon)
      result = makePtrType(m, x)
    of ConvC, CastC, AconstrC, OconstrC:
      result = typeFromPos n.firstSon
    of ParC:
      result = getType(m, t, n.firstSon)
    of NilC:
      result = makePtrType(m, createIntegralType(m.lits, VoidC, ""))
    of SufC:
      result = errorType()
      let (_, suffix) = sons2(t, n)
      let s = m.lits.strings[t[suffix].litId]
      if s.len > 0:
        if s[0] == 'i':
          result = createIntegralType(m.lits, IntC, s.substr(1))
        elif s[0] == 'u':
          result = createIntegralType(m.lits, UIntC, s.substr(1))
        elif s[0] == 'f':
          result = createIntegralType(m.lits, FloatC, s.substr(1))

    else:
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
        else:
          bug "typenav: cannot get type of construct: " & $n.stmtKind
  of EfldC:
    # skip to its outer Enum declaration which is its type:
    var i = int(n)
    while i > 0 and t[NodePos(i)].kind != EnumC: dec i
    result = typeFromPos NodePos(i)
  of NegC, AddC, SubC, MulC, DivC, ModC, ShrC, ShlC,
     BitandC, BitorC, BitxorC, BitnotC:
    result = typeFromPos n.firstSon
  of AsgnC, RetC, BreakC, WhileC, StmtsC, KvC,
     RangeC, RangesC, EmitC, IfC, ElifC, ElseC, CaseC,
     OfC, LabC, JmpC,  ParamsC, UnionC, ObjectC, EnumC,
     ProctypeC, AtomicC, RoC, RestrictC, IntC, UIntC, FloatC, CharC, BoolC,
     VoidC, PtrC, ArrayC, FlexarrayC, APtrC, TypeC, CdeclC,
     StdcallC, SafecallC, SyscallC, FastcallC, ThiscallC, NoconvC, MemberC,
     AttrC, InlineC, NoinlineC, VarargsC, WasC, SelectanyC,
     PragmasC, AlignC, BitsC, VectorC, ImpC, NodeclC, InclC, ScopeC, DiscardC,
     TryC, RaiseC, OnErrC, RaisesC, ErrsC, StaticC:
    result = errorType()

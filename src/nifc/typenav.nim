#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A type navigator can recompute the type of an expression.

import std / [strutils, tables, assertions]
import bitabs, packedtrees

import nifc_model, mangler

type
  TypeDesc* {.acyclic.} = object
    p: NodePos
    a: PackedNode[NifcKind]
    down, next: ref TypeDesc
  TypeDescRef = ref TypeDesc

proc typeFromPos*(n: NodePos): TypeDesc {.inline.} =
  TypeDesc(p: n)

proc rawPos*(t: TypeDesc): NodePos {.inline.} =
  assert t.p != NodePos(0)
  result = t.p

proc errorType(): TypeDesc = TypeDesc(p: NodePos(0), a: createAtom(Empty))
proc isError*(t: TypeDesc): bool = t.p == NodePos(0) and t.a.kind == Empty

proc createAtom(k: NifcKind; value: StrId): PackedNode[NifcKind] {.inline.} =
  createAtom(k, uint32(value))

proc createIntegralType(lits: var Literals; integral: NifcKind; bits: string): TypeDesc =
  result = TypeDesc(p: NodePos(0), a: createAtom(integral))
  result.down = TypeDescRef(p: NodePos(0), a: createAtom(NifcKind.IntLit, lits.strings.getOrIncl(bits)))

proc atomType(lits: var Literals; name: NifcKind): TypeDesc =
  result = TypeDesc(p: NodePos(0), a: createAtom(name))

proc kind*(tree: Tree; t: TypeDesc): NifcKind =
  if t.p != NodePos(0):
    tree[t.p].kind
  else:
    t.a.kind

proc copyType*(dest: var Tree; t: Tree; typ: TypeDesc) =
  if typ.p != NodePos(0):
    copyTree dest, t, typ.p
  else:
    if isAtom(typ.a.kind):
      dest.copyNode typ.a
    else:
      copyInto dest, typ.a.kind, typ.a.info:
        var it {.cursor.} = typ.down
        while it != nil:
          copyType(dest, t, it[])
          it = it.next

proc elemType*(t: Tree; typ: TypeDesc): TypeDesc =
  if typ.p != NodePos(0):
    result = TypeDesc(p: ithSon(t, typ.p, 1))
  else:
    result = typ.down[]

proc bits*(m: Module; typ: TypeDesc): int =
  var lit: StrId
  if typ.p != NodePos(0):
    lit = m.code[typ.p.firstSon].litId
  else:
    lit = typ.down.a.litId
  result = parseInt(m.lits.strings[lit])

proc makePtrType(m: var Module; typ: TypeDesc): TypeDesc =
  result = atomType(m.lits, PtrC)
  result.down = TypeDescRef(p: typ.p, a: typ.a, down: typ.down)

proc isImportC*(m: Module; typ: TypeDesc): bool =
  assert m.code.kind(typ) == Sym
  let litId = if typ.p != NodePos(0):
                m.code[typ.rawPos].litId
              else:
                typ.a.litId
  m.lits.strings[litId].isImportC

proc getType*(m: var Module; t: Tree; n: NodePos): TypeDesc =
  case t[n].kind
  of Empty, Ident, SymDef, Err:
    result = errorType()
  of Sym:
    let d = m.defs.getOrDefault(t[n].litId)
    if d.pos != NodePos(0):
      result = getType(m, t, d.pos)
    else:
      # importC types are not defined
      if m.lits.strings[t[n].litId].isImportC:
        result = TypeDesc(p: n)
      else:
        result = errorType()
  of ProcC:
    result = TypeDesc(p: n)
  of GvarC, TvarC, ConstC, VarC:
    let v = asVarDecl(t, n)
    result = TypeDesc(p: v.typ)
  of ParamC:
    let v = asParamDecl(t, n)
    result = TypeDesc(p: v.typ)
  of FldC:
    let v = asFieldDecl(t, n)
    result = TypeDesc(p: v.typ)
  of IntLit, SizeofC, AlignofC, OffsetofC: result = createIntegralType(m.lits, IntC, "-1")
  of UIntLit: result = createIntegralType(m.lits, UIntC, "-1")
  of FloatLit, InfC, NegInfC, NanC: result = createIntegralType(m.lits, FloatC, "64")
  of CharLit: result = createIntegralType(m.lits, CharC, "8")
  of StrLit: result = makePtrType(m, createIntegralType(m.lits, CharC, "8"))
  of TrueC, FalseC, AndC, OrC, NotC, EqC, NeqC, LeC, LtC, ErrC:
    result = createIntegralType(m.lits, BoolC, "8")
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

#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Collect useful information for a native code generator.

import std / [assertions, syncio, tables, sets, intsets, strutils]

import bitabs, packedtrees
import .. / nifc_model
import slots

## Records how often every local variable is used
## and whether its address has been taken.

type
  VarInfo* = object
    defs*, usages*: int # how often the variable is defined&used.
    weight*: int # similar to `usages` but takes into consideration
                 # whether the variable is used within a loop.
    props*: set[VarProp]
  ProcBodyProps* = object
    vars*: Table[StrId, VarInfo]
    inlineStructs*: bool # candidate for struct inlining

  Scope = object
    vars: seq[StrId]
    hasCall: bool

  Context = object
    inLoop, inAddr, inAsgnTarget: int
    res: ProcBodyProps
    scopes: seq[Scope]

proc openScope(c: var Context) =
  c.scopes.add Scope()

proc closeScope(c: var Context) =
  let finished = c.scopes.pop()
  if not finished.hasCall:
    for v in finished.vars:
      c.res.vars[v].props.incl AllRegs
  elif c.scopes.len > 0:
    # a scope has a call if some inner scope has a call:
    c.scopes[^1].hasCall = true

const
  LoopWeight = 3 # assume that the usual loop runs 3 times. This is used
                 # to make the register allocator keep variables that are
                 # used in loops more important.

proc analyseProcBody(m: Module; t: Tree; n: NodePos; c: var Context) =
  case t[n].kind
  of Empty, Ident, SymDef, IntLit, UIntLit, FloatLit, CharLit, StrLit, Err,
     NilC, FalseC, TrueC, SizeofC:
    discard
  of StmtsC, ScopeC:
    c.openScope()
    for ch in sons(t, n):
      analyseProcBody(m, t, ch, c)
    c.closeScope()
  of CallC:
    # XXX Special case `cold` procs like `raiseIndexError` in order
    # to produce better code for the common case.
    for ch in sons(t, n):
      analyseProcBody(m, t, ch, c)
    c.scopes[^1].hasCall = true
  of VarC, GvarC, TvarC, ConstC:
    let v = asVarDecl(t, n)
    assert t[v.name].kind == SymDef
    let vn = t[v.name].litId
    let hasValue = t[v.value].kind != Empty
    c.res.vars[vn] = VarInfo(defs: ord(hasValue))
    c.scopes[^1].vars.add vn
    if hasValue:
      analyseProcBody(m, t, v.value, c)
  of ParamC:
    let v = asParamDecl(t, n)
    assert t[v.name].kind == SymDef
    let vn = t[v.name].litId
    c.res.vars[vn] = VarInfo(defs: 1) # it is a parameter, it has a value
    c.scopes[^1].vars.add vn
  of Sym:
    let vn = t[n].litId
    if c.res.vars.hasKey(vn):
      let entry = addr(c.res.vars[vn])
      if c.inAsgnTarget > 0:
        inc entry.defs
      else:
        inc entry.usages
      inc entry.weight, c.inLoop*LoopWeight
      if c.inAddr > 0:
        entry.props.incl AddrTaken
  of EmitC:
    for ch in sons(t, n):
      analyseProcBody(m, t, ch, c)
  of WhileC:
    inc c.inLoop
    for ch in sons(t, n):
      analyseProcBody(m, t, ch, c)
    dec c.inLoop
  of AtC, PatC:
    let (a, idx) = sons2(t, n)
    analyseProcBody(m, t, a, c)
    # don't pessimize array indexes:
    let oldAddr = c.inAddr
    let oldTarget = c.inAsgnTarget
    c.inAddr = 0
    c.inAsgnTarget = 0
    analyseProcBody(m, t, idx, c)
    c.inAddr = oldAddr
    c.inAsgnTarget = oldTarget
  of DerefC:
    let oldTarget = c.inAsgnTarget
    c.inAsgnTarget = 0
    analyseProcBody(m, t, n.firstSon, c)
    c.inAsgnTarget = oldTarget
  of AddrC:
    inc c.inAddr
    analyseProcBody(m, t, n.firstSon, c)
    dec c.inAddr
  of AsgnC:
    let (dest, src) = sons2(t, n)
    inc c.inAsgnTarget
    analyseProcBody(m, t, dest, c)
    dec c.inAsgnTarget
    analyseProcBody(m, t, src, c)
  of DotC, ParC, AndC, OrC, NotC, NegC, OconstrC, AconstrC, KvC,
     AddC, SubC, MulC, DivC, ModC, ShrC, ShlC, BitandC, BitorC, BitxorC, BitnotC,
     EqC, NeqC, LeC, LtC, CastC, ConvC, RangeC, RangesC, IfC, ElifC, ElseC,
     BreakC, CaseC, OfC, LabC, JmpC, RetC, ParamsC:
    for ch in sons(t, n):
      analyseProcBody(m, t, ch, c)
  of ProcC, FldC,
     UnionC, ObjectC, EfldC, EnumC, ProctypeC, AtomicC, RoC, RestrictC,
     IntC, UIntC, FloatC, CharC, BoolC, VoidC, PtrC, ArrayC, FlexarrayC,
     APtrC, TypeC, CdeclC, StdcallC, SafecallC, SyscallC, FastcallC, ThiscallC,
     NoconvC, MemberC, AttrC, InlineC, NoinlineC, VarargsC, WasC, SelectanyC,
     PragmasC, AlignC, BitsC, VectorC, ImpC, NodeclC, InclC, SufC:
    discard "do not traverse these"

proc analyseProcBody*(m: Module; t: Tree; n: NodePos): ProcBodyProps =
  var c = Context()
  analyseProcBody m, t, n, c
  result = ensureMove(c.res)
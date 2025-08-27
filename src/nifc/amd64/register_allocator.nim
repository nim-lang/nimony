#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from genpreasm.nim

#[

Optimistic Scope Based Register Allocation
------------------------------------------

The used strategy is simple and effective. It is not covered by the literature
very well, probably because ultimately it's inferior to a graph based register
allocation strategy. But how can we know without trying...

We traverse the proc body keeping track precisely of the involved scopes. For
every variable that can possibly be kept in a register, we do so until we run
out of registers. If we run out of registers we search the current scope and all
of its parents scopes for a variable that has been assigned a register but has
a lower "weight" (as computed in analyser.nim) as the current one. If so, we
instead put this other variable to the stack and use its register for the current
one.

Notice that this form of "spilling" does not produce any code at all. We simply
undo the register assignment. All this happens before we generate code. There
are 3 passes over a proc body:

1. "Analyse" the variables and their usages, compute their "weights".
2. Perform register allocation.
3. Generate code.

]#

proc openScope(c: var GeneratedCode) =
  c.scopes.add Scope()

proc closeScope(c: var GeneratedCode) =
  let finished = c.scopes.pop()
  for v in finished.vars:
    freeScope(c.rega, c.locals[v])

proc stealFrom*(c: var GeneratedCode; current: LitId; loc: Location; weights: Table[LitId, VarInfo]) =
  let lookFor = if loc.typ.kind == AFloat: InRegFp else: InReg
  for i in 0..<c.scopes.len:
    for v in c.scopes[i].vars:
      if c.locals[v].kind == lookFor:
        if weights[current].weight > weights[v].weight:
          c.locals[current] = c.locals[v]
          c.locals[v] = loc
          break

proc allocRegsForProc(c: var GeneratedCode; t: Tree; n: NodePos; weights: Table[LitId, VarInfo]) =
  # Step 2.
  let k = t[n].kind
  case k
  of Empty, Ident, SymDef, Sym, IntLit, UIntLit, FloatLit, CharLit, StrLit, Err,
     NilC, FalseC, TrueC, SizeofC, AlignofC, OffsetofC, InfC, NegInfC, NanC:
    discard
  of StmtsC, ScopeC:
    c.openScope()
    for ch in sons(t, n):
      allocRegsForProc(c, t, ch, weights)
    c.closeScope()
  of VarC:
    let v = asVarDecl(t, n)
    assert t[v.name].kind == SymDef
    let vn = t[v.name].litId

    var typ = typeToSlot(c, v.typ)
    let w = weights[vn]
    let loc = allocVar(c.rega, typ, w.props)

    c.locals[vn] = loc
    # did register allocation fail?
    if AddrTaken notin w.props:
      if typ.kind == AFloat:
        if loc.kind != InRegFp:
          stealFrom c, vn, loc, weights
      elif typ.size <= WordSize:
        if loc.kind != InReg:
          stealFrom c, vn, loc, weights

    c.scopes[^1].vars.add vn
    let hasValue = t[v.value].kind != Empty
    if hasValue:
      allocRegsForProc(c, t, v.value, weights)
  of ParamC:
    # XXX Params have dedicated in the stack anyway...
    when false:
      let v = asParamDecl(t, n)
      assert t[v.name].kind == SymDef
      let vn = t[v.name].litId
  of AsgnC, AddrC, DerefC,  AtC, PatC, WhileC, EmitC,
     ParC, AndC, OrC, NotC, NegC, OconstrC, AconstrC, KvC,
     AddC, SubC, MulC, DivC, ModC, ShrC, ShlC, BitandC, BitorC, BitxorC, BitnotC,
     EqC, NeqC, LeC, LtC, CastC, ConvC, RangeC, RangesC, IfC, ElifC, ElseC,
     BreakC, CaseC, OfC, LabC, JmpC, RetC, ParamsC, CallC, OnErrC, DiscardC,
     TryC, RaiseC:
    for ch in sons(t, n):
      allocRegsForProc(c, t, ch, weights)
  of DotC, GvarC, TvarC, ConstC, ProcC, FldC,
     UnionC, ObjectC, EfldC, EnumC, ProctypeC, AtomicC, RoC, RestrictC,
     IntC, UIntC, FloatC, CharC, BoolC, VoidC, PtrC, ArrayC, FlexarrayC,
     APtrC, TypeC, CdeclC, StdcallC, SafecallC, SyscallC, FastcallC, ThiscallC,
     NoconvC, MemberC, AttrC, InlineC, NoinlineC, VarargsC, WasC, SelectanyC,
     PragmasC, AlignC, BitsC, VectorC, ImpC, NodeclC, InclC, SufC, RaisesC, ErrsC,
     StaticC, ErrC:
    discard "do not traverse these"

proc allocateVars*(c: var GeneratedCode; t: Tree; n: NodePos) =
  let props = analyseVarUsages(t, n)
  c.locals.clear()
  allocRegsForProc c, t, n, props.vars

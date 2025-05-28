#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Collect useful information for a native code generator.

import std / [assertions, syncio, tables, sets, intsets, strutils]

import bitabs, nifstreams, nifcursors
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
    vars*: Table[SymId, VarInfo]
    inlineStructs*: bool # candidate for struct inlining
    hasCall*: bool

  Scope = object
    vars: seq[SymId]
    hasCall: bool

  Context = object
    inLoops, inAddr, inAsgnTarget, inArrayIndex: int
    res: ProcBodyProps
    scopes: seq[Scope]

proc openScope(c: var Context) =
  c.scopes.add Scope()

proc closeScope(c: var Context) =
  let finished = c.scopes.pop()
  if not finished.hasCall:
    for v in finished.vars:
      c.res.vars[v].props.incl AllRegs
  else:
    assert c.scopes.len > 0
    # a scope has a call if some inner scope has a call:
    c.scopes[^1].hasCall = true

const
  LoopWeight = 3 # assume that the usual loop runs 3 times. This is used
                 # to make the register allocator keep variables that are
                 # used in loops more important.

proc analyseVarUsages(c: var Context; n: var Cursor) =
  # Step 1. Analyse variable usages.
  case n.kind
  of Symbol:
    let vn = n.symId
    if c.res.vars.hasKey(vn):
      let entry = addr(c.res.vars[vn])
      if c.inAsgnTarget > 0:
        inc entry.defs
      else:
        inc entry.usages
      inc entry.weight, c.inLoops*LoopWeight
      if (c.inAddr + c.inArrayIndex) > 0:
        # arrays on the stack cannot be in registers either as registers
        # cannot be aliased!
        entry.props.incl AddrTaken

  of UnknownToken, EofToken,
     DotToken, Ident, SymbolDef,
     StringLit, CharLit, IntLit, UIntLit, FloatLit:
    inc n
  else:
    case n.stmtKind
    of NoStmt:
      let k = n.exprKind
      case k
      of AtC, PatC:
        inc n
        if k == AtC: inc c.inArrayIndex
        analyseVarUsages(c, n)
        if k == AtC: dec c.inArrayIndex
        # don't pessimize array indexes:
        let oldAddr = c.inAddr
        let oldTarget = c.inAsgnTarget
        c.inAddr = 0
        c.inAsgnTarget = 0
        analyseVarUsages(c, n)
        c.inAddr = oldAddr
        c.inAsgnTarget = oldTarget
        skipParRi n
      of DerefC:
        inc n
        let oldTarget = c.inAsgnTarget
        c.inAsgnTarget = 0
        analyseVarUsages(c, n)
        c.inAsgnTarget = oldTarget
        skipParRi n
      of AddrC:
        inc n
        inc c.inAddr
        analyseVarUsages(c, n)
        dec c.inAddr
        skipParRi n
      of DotC:
        inc n
        let inStackFrame = n.exprKind != DerefC
        if inStackFrame: inc c.inArrayIndex
        analyseVarUsages(c, n)
        if inStackFrame: dec c.inArrayIndex
        skipParRi n
      of NoExpr, SufC, NilC, InfC, NeginfC, NanC, FalseC, TrueC, SizeofC, AlignofC,
         OffsetofC, ErrvC:
        skip n
      of ParC, AndC, OrC, NotC, NegC, OconstrC, AconstrC, OvfC,
         AddC, SubC, MulC, DivC, ModC, ShrC, ShlC, BitandC, BitorC, BitxorC, BitnotC,
         EqC, NeqC, LeC, LtC, CastC, ConvC, BaseobjC:
        inc n
        while n.kind != ParRi:
          analyseVarUsages(c, n)
        inc n
      of CallC:
        # XXX Special case `cold` procs like `raiseIndexError` in order
        # to produce better code for the common case.
        inc n
        while n.kind != ParRi:
          analyseVarUsages(c, n)
        inc n
        c.scopes[^1].hasCall = true
    of StmtsS, ScopeS:
      c.openScope()
      while n.kind != ParRi:
        analyseVarUsages(c, n)
      inc n
      c.closeScope()
    of CallS, OnerrS:
      inc n
      while n.kind != ParRi:
        analyseVarUsages(c, n)
      inc n
      c.scopes[^1].hasCall = true
    of VarS, GvarS, TvarS, ConstS:
      let v = takeVarDecl(n)
      assert v.name.kind == SymbolDef
      let vn = v.name.symId
      let hasValue = v.value.kind != DotToken
      c.res.vars[vn] = VarInfo(defs: ord(hasValue))
      c.scopes[^1].vars.add vn
      if hasValue:
        var n = v.value
        analyseVarUsages(c, n)
    of AsgnS:
      inc n
      inc c.inAsgnTarget
      analyseVarUsages(c, n)
      dec c.inAsgnTarget
      analyseVarUsages(c, n)
      skipParRi n
    of KeepovfS:
      inc n
      analyseVarUsages(c, n)
      inc c.inAsgnTarget
      analyseVarUsages(c, n)
      dec c.inAsgnTarget
      skipParRi n
    of ProcS, TypeS, BreakS, ImpS, InclS, LabS, JmpS:
      skip n
    of WhileS:
      inc n
      inc c.inLoops
      while n.kind != ParRi:
        analyseVarUsages(c, n)
      dec c.inLoops
      skipParRi n
    of EmitS, IfS, CaseS, RetS, DiscardS, TryS, RaiseS:
      inc n
      while n.kind != ParRi:
        analyseVarUsages(c, n)
      inc n

proc analyseVarUsages*(n: Cursor): ProcBodyProps =
  var c = Context()
  c.scopes.add Scope() # there is always one scope
  var n = n
  if n.stmtKind == ProcS:
    var prc = takeProcDecl(n)
    var params = prc.params
    if params.kind != DotToken:
      inc params
      while params.kind != ParRi:
        var p = takeParamDecl(params)
        assert p.name.kind == SymbolDef
        let vn = p.name.symId
        c.res.vars[vn] = VarInfo(defs: 1) # it is a parameter, it has a value
        c.scopes[^1].vars.add vn
    analyseVarUsages c, prc.body
  else:
    analyseVarUsages c, n
  c.res.hasCall = c.scopes[0].hasCall
  result = ensureMove(c.res)

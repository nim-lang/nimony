#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Prepare for dead code elimination and generic instance merging.

import std / [assertions, tables, sets]
include nifprelude
import ".." / nifc / [nifc_model]

import symparser

type
  ModuleAnalysis* = object
    uses*: Table[SymId, HashSet[SymId]]
    roots*: HashSet[SymId]
    offers*: HashSet[SymId] # generic instances that are offered by this module

proc tr(n: var Cursor; a: var ModuleAnalysis; owner: SymId) =
  case n.kind
  of ParLe:
    case n.stmtKind
    of ProcS, TypeS, VarS, ConstS, GvarS, TvarS:
      inc n
      var newOwner = owner
      let symName = pool.syms[n.symId]
      if n.kind == SymbolDef:
        if isInstantiation(symName):
          a.offers.incl(n.symId)
        if not isLocalName(symName):
          newOwner = n.symId

      while n.kind != ParRi:
        tr n, a, newOwner
      inc n
    else:
      if n.substructureKind == FldU:
        inc n
        let symName = pool.syms[n.symId]
        if n.kind == SymbolDef:
          if isInstantiation(symName):
            a.offers.incl(n.symId)
      else:
        inc n
      while n.kind != ParRi:
        tr n, a, owner
      inc n
  of Symbol:
    if not isLocalName(pool.syms[n.symId]):
      if owner == SymId(0):
        a.roots.incl(n.symId)
      else:
        if not a.uses.hasKey(owner): a.uses[owner] = initHashSet[SymId]()
        a.uses[owner].incl(n.symId)
    inc n
  of SymbolDef, UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit: inc n
  of ParRi: raiseAssert "ParRi should not be encountered here"

const
  depName = "uses"
  offerName = "offers"
  rootName = "roots"

proc prepDce(outputFilename: string; n: Cursor) =
  var n = n
  var a = ModuleAnalysis()
  tr n, a, SymId(0)

  var b = nifbuilder.open(outputFilename)
  b.withTree "stmts":
    b.withTree rootName:
      for root in a.roots:
        b.addSymbol pool.syms[root]
    for owner, uses in mpairs(a.uses):
      b.withTree depName:
        b.addSymbol pool.syms[owner]
        for dep in uses:
          b.addSymbol pool.syms[dep]
    b.withTree offerName:
      for offer in a.offers:
        b.addSymbol pool.syms[offer]
  b.close()

proc readModuleAnalysis*(infile: string): ModuleAnalysis =
  var buf = parseFromFile(infile)
  var n = beginRead(buf)
  result = ModuleAnalysis()
  if n.stmtKind == StmtsS:
    inc n
    let depTag = pool.tags.getOrIncl(depName)
    let offerTag = pool.tags.getOrIncl(offerName)
    let rootTag = pool.tags.getOrIncl(rootName)
    while n.kind != ParRi:
      if n.kind == ParLe:
        if n.tag == rootTag:
          inc n
          while n.kind != ParRi:
            if n.kind == Symbol:
              result.roots.incl(n.symId)
              inc n
            else:
              raiseAssert infile & ": expected Symbol"
        elif n.tag == depTag:
          inc n
          let key = n.symId
          result.uses[key] = initHashSet[SymId]()
          inc n
          while n.kind != ParRi:
            if n.kind == Symbol:
              result.uses[key].incl(n.symId)
              inc n
            else:
              raiseAssert infile & ": expected Symbol"
        elif n.tag == offerTag:
          inc n
          while n.kind != ParRi:
            if n.kind == Symbol:
              result.offers.incl(n.symId)
              inc n
            else:
              raiseAssert infile & ": expected Symbol"
        else:
          raiseAssert infile & ": expected (roots|uses|offers)"
        inc n
      else:
        raiseAssert infile & ": expected ParLe"

proc writeDceOutput*(buf: var TokenBuf; outfile: string) =
  ## Direct overload that works on an already-parsed token buffer,
  ## avoiding the file read + parse step.
  let n = beginRead(buf)
  prepDce(outfile, n)
  endRead(buf)

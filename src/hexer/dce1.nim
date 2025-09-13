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
    deps*: Table[SymId, HashSet[SymId]]
    offers*: HashSet[SymId] # generic instances that are offered by this module

proc tr(n: var Cursor; a: var ModuleAnalysis; owner: SymId) =
  case n.kind
  of ParLe:
    case n.stmtKind
    of ProcS, TypeS, VarS, ConstS, GvarS, TvarS:
      inc n
      let newOwner = if n.kind == SymbolDef: n.symId else: owner
      if n.kind == SymbolDef and isInstantiation(pool.syms[n.symId]):
        a.offers.incl(n.symId)

      while n.kind != ParRi:
        tr n, a, newOwner
      inc n
    else:
      inc n
      while n.kind != ParRi:
        tr n, a, owner
      inc n
  of Symbol:
    if not isLocalName(pool.syms[n.symId]):
      if not a.deps.hasKey(owner): a.deps[owner] = initHashSet[SymId]()
      a.deps[owner].incl(n.symId)
    inc n
  of SymbolDef, UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit: inc n
  of ParRi: raiseAssert "ParRi should not be encountered here"

const
  depName = "dep"
  offerName = "offer"
  RootSym* = "`root.0"

proc prepDce(outputFilename: string; n: Cursor) =
  var n = n
  var a = ModuleAnalysis()
  tr n, a, pool.syms.getOrIncl(RootSym)

  var b = nifbuilder.open(outputFilename)
  b.withTree "stmts":
    for owner, deps in mpairs(a.deps):
      b.withTree depName:
        b.addSymbol pool.syms[owner]
        for dep in deps:
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
    while n.kind != ParRi:
      if n.kind == ParLe:
        if n.tag == depTag:
          inc n
          let key = n.symId
          result.deps[key] = initHashSet[SymId]()
          inc n
          while n.kind != ParRi:
            if n.kind == Symbol:
              result.deps[key].incl(n.symId)
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
        raiseAssert infile & ": expected ParLe"

proc writeDceOutput*(infile, outfile: string) =
  var buf = parseFromFile(infile)
  let n = beginRead(buf)
  prepDce(outfile, n)

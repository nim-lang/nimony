#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Prepare for dead code elimination and generic instance merging.

import std / [assertions, tables, hashes, sets, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lengc / [leng_model]

import ".." / lib / symparser

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
      n.into:
        var newOwner = owner
        if n.kind == SymbolDef:
          let symName = pool.syms[n.symId]
          if isInstantiation(symName):
            a.offers.incl(n.symId)
          if not isLocalName(symName):
            newOwner = n.symId
        while n.hasMore:
          tr n, a, newOwner
    else:
      if n.substructureKind == PragmasU:
        # Check if this pragma section contains exportc
        # If so, mark the owner as a root (exportc symbols are entry points)
        var hasExportc = false
        n.into:
          while n.hasMore:
            if n.kind == ParLe and n.pragmaKind == ExportcP:
              hasExportc = true
            tr n, a, owner
        if hasExportc and owner != SymId(0):
          a.roots.incl(owner)
      else:
        let isFld = n.substructureKind == FldU
        n.into:
          if isFld and n.kind == SymbolDef:
            let symName = pool.syms[n.symId]
            if isInstantiation(symName):
              a.offers.incl(n.symId)
          while n.hasMore:
            tr n, a, owner
  of Symbol:
    if not isLocalName(pool.syms[n.symId]):
      if owner == SymId(0):
        a.roots.incl(n.symId)
      else:
        if not a.uses.hasKey(owner): a.uses[owner] = initHashSet[SymId]()
        a.uses.getOrQuit(owner).incl(n.symId)
    inc n
  of SymbolDef, UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit: inc n
  of ParRi: raiseAssert "ParRi should not be encountered here"

const
  depName = "uses"
  offerName = "offers"
  rootName = "roots"

proc prepDce(outputFilename: string; n: Cursor; dottedSuffix: string) =
  var n = n
  var a = ModuleAnalysis()
  tr n, a, SymId(0)

  var b = nifbuilder.open(outputFilename, writeMode = OnlyIfChanged)
  b.withTree "stmts":
    b.withTree rootName:
      for root in a.roots:
        b.addSymbol pool.syms[root], dottedSuffix
    for owner, uses in mpairs(a.uses):
      b.withTree depName:
        b.addSymbol pool.syms[owner], dottedSuffix
        for dep in uses:
          b.addSymbol pool.syms[dep], dottedSuffix
    b.withTree offerName:
      for offer in a.offers:
        b.addSymbol pool.syms[offer], dottedSuffix
  b.close()

proc readModuleAnalysis*(infile: string): ModuleAnalysis =
  var buf = parseFromFile(infile)
  var n = beginRead(buf)
  result = ModuleAnalysis()
  if n.stmtKind == StmtsS:
    let depTag = pool.tags.getOrIncl(depName)
    let offerTag = pool.tags.getOrIncl(offerName)
    let rootTag = pool.tags.getOrIncl(rootName)
    n.into:                                     # (stmts ...)
      while n.hasMore:
        if n.kind != ParLe:
          raiseAssert infile & ": expected ParLe"
        if n.tag == rootTag:
          n.into:                               # (roots ...)
            while n.hasMore:
              if n.kind == Symbol:
                result.roots.incl(n.symId)
                skip n
              else:
                raiseAssert infile & ": expected Symbol"
        elif n.tag == depTag:
          n.into:                               # (uses ...)
            let key = n.symId
            result.uses[key] = initHashSet[SymId]()
            skip n
            while n.hasMore:
              if n.kind == Symbol:
                result.uses.getOrQuit(key).incl(n.symId)
                skip n
              else:
                raiseAssert infile & ": expected Symbol"
        elif n.tag == offerTag:
          n.into:                               # (offers ...)
            while n.hasMore:
              if n.kind == Symbol:
                result.offers.incl(n.symId)
                skip n
              else:
                raiseAssert infile & ": expected Symbol"
        else:
          raiseAssert infile & ": expected (roots|uses|offers)"

proc writeDceOutput*(buf: var TokenBuf; outfile, dottedSuffix: string) =
  ## Direct overload that works on an already-parsed token buffer,
  ## avoiding the file read + parse step.
  let n = beginRead(buf)
  prepDce(outfile, n, dottedSuffix)
  endRead(buf)

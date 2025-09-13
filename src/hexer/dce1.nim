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
import ".." / nimony / [nimony_model, decls]

import symparser

type
  DependencyGraph = Table[SymId, HashSet[SymId]]

  ModuleAnalysis = object
    deps: DependencyGraph
    offers: HashSet[SymId] # generic instances that are offered by this module

proc tr(n: var Cursor; a: var ModuleAnalysis; owner: SymId) =
  case n.kind
  of ParLe:
    case n.stmtKind
    of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS,
      VarS, LetS, ConstS, GvarS, TvarS, GletS, TletS, ResultS, CursorS:
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

proc prepDce(outputFilename: string; n: Cursor) =
  var n = n
  var a = ModuleAnalysis()
  tr n, a, NoSymId

  var b = nifbuilder.open(outputFilename)
  b.withTree "stmts":
    for owner, deps in mpairs(a.deps):
      b.withTree "dep":
        b.addSymbol pool.syms[owner]
        for dep in deps:
          b.addSymbol pool.syms[dep]
    for offer in a.offers:
      b.withTree "offer":
        b.addSymbol pool.syms[offer]
  b.close()

proc writeDceOutput*(infile, outfile: string) =
  var buf = parse(infile)
  let n = beginRead(buf)
  prepDce(outfile, n)

#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Prepare for dead code elimination and generic instance merging.
##
## The analysis rides in the module's own `.x.nif`, as the first statement of
## its `(stmts …)`. `withDceSection` puts it there, `readModuleAnalysis` reads
## it back by parsing that one subtree and nothing else.

import std / [assertions, tables, hashes, sets, syncio, algorithm]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lengc / [leng_model]

import ".." / lib / symparser
from ".." / lib / nifcoreparse import nil

type
  ModuleAnalysis* = object
    uses*: Table[SymId, HashSet[SymId]]
    roots*: HashSet[SymId]
    offers*: HashSet[SymId] # generic instances that are offered by this module

proc tr(n: var Cursor; a: var ModuleAnalysis; owner: SymId) =
  case n.kind
  of TagLit:
    case n.stmtKind
    of ProcS, TypeS, VarS, ConstS, GvarS, TvarS:
      n.into:
        var newOwner = owner
        if n.isSymbolDef:
          if pool.symIsInstantiation(n.symId):
            a.offers.incl(n.symId)
          if not pool.symIsLocal(n.symId):
            newOwner = n.symId
        while n.hasMore:
          tr n, a, newOwner
    else:
      if n.substructureKind == PragmasU:
        # Check if this pragma section contains exportc or interrupt.
        # If so, mark the owner as a root: both name entry points nothing in the
        # program calls. An `{.interrupt.}` handler is reached ONLY through the
        # interrupt table, which the back end builds after this pass runs — so
        # without this it is unreachable by construction, gets deleted, and the
        # failure is a device that silently never responds to the interrupt.
        var isEntryPoint = false
        n.into:
          while n.hasMore:
            if n.isTagLit and n.pragmaKind in {ExportcP, InterruptP}:
              isEntryPoint = true
            tr n, a, owner
        if isEntryPoint and owner != SymId(0):
          a.roots.incl(owner)
      else:
        let isFld = n.substructureKind == FldU
        n.into:
          if isFld and n.kind == SymbolDef:
            if pool.symIsInstantiation(n.symId):
              a.offers.incl(n.symId)
          while n.hasMore:
            tr n, a, owner
  of Symbol:
    if not pool.symIsLocal(n.symId):
      if owner == SymId(0):
        a.roots.incl(n.symId)
      else:
        if not a.uses.hasKey(owner): a.uses[owner] = initHashSet[SymId]()
        a.uses.getOrQuit(owner).incl(n.symId)
    inc n
  of SymbolDef, UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, DotToken, Ident, StrLit, CharLit, IntLit, UIntLit, FloatLit: inc n
  else: raiseAssert "ParRi should not be encountered here" # classic ParRi only

const
  depName = "uses"
  offerName = "offers"
  rootName = "roots"
  dceName* = "dce"
    ## Head of the analysis section hexer parks at the top of every `.x.nif`.

proc cmpSyms(a, b: SymId): int = cmpNames(pool.symString(a), pool.symString(b))

proc sortedSyms*(syms: HashSet[SymId]): seq[SymId] =
  ## A `SymId` is a pool index handed out in interning order, so an edit that
  ## interns one more symbol (a local, say) renumbers every one after it. The
  ## `.x.nif` carrying this section is written `OnlyIfChanged`, so its bytes
  ## must depend on the content alone — hence name order, not `SymId` order.
  result = newSeq[SymId](0)
  for s in syms: result.add s
  sort result, cmpSyms

proc sortedSymNames*(syms: HashSet[SymId]): seq[string] =
  ## `sortedSyms` for a writer that speaks strings (`dce2.writeLiveFile`).
  result = newSeq[string](0)
  for s in syms: result.add pool.symString(s)
  sort result, cmpNames

proc sortedKeys*[T](t: Table[string, T]): seq[string] =
  result = newSeq[string](0)
  for k in t.keys: result.add k
  sort result, cmpNames

proc addSymList(dest: var TokenBuf; tag: string; syms: HashSet[SymId]) =
  dest.addParLe globalTags.registerTag(tag)
  for s in sortedSyms(syms): dest.addSymUse s
  dest.addParRi()

proc addDceSection(dest: var TokenBuf; a: ModuleAnalysis) =
  ## `(dce (roots …) (uses owner dep…)* (offers …))`. No line info: the
  ## section describes symbols, not source positions, and the bytes it does
  ## not write are bytes that cannot shift.
  dest.addParLe globalTags.registerTag(dceName)
  addSymList dest, rootName, a.roots

  var owners = newSeq[SymId](0)
  for owner in a.uses.keys: owners.add owner
  sort owners, cmpSyms
  for owner in owners:
    dest.addParLe globalTags.registerTag(depName)
    dest.addSymUse owner
    for dep in sortedSyms(a.uses.getOrQuit(owner)): dest.addSymUse dep
    dest.addParRi()

  addSymList dest, offerName, a.offers
  dest.addParRi()

proc withDceSection*(buf: var TokenBuf): TokenBuf =
  ## Returns the module's `(stmts …)` with its DCE analysis spliced in as the
  ## first statement, which is where `readModuleAnalysis` expects to find it.
  ## Riding along in the `.x.nif` rather than in a `.dce.nif` of its own: the
  ## analysis is a projection of this very buffer, so a second file would be a
  ## second write per module for information the reader can reach by parsing
  ## one subtree.
  var probe = beginRead(buf)
  var a = ModuleAnalysis()
  tr probe, a, SymId(0)

  result = createTokenBuf(buf.len + 64)
  var n = beginRead(buf)
  result.addParLe n.cursorTagId, n.info
  n.into:
    addDceSection result, a
    while n.hasMore:
      result.takeTree n
  result.addParRi()

proc readModuleAnalysis*(infile: string): ModuleAnalysis =
  ## Reads the `(dce …)` section back out of a module's `.x.nif`. Only that
  ## one subtree is parsed: the body behind it is the bulk of the file and no
  ## business of the liveness fixpoint.
  var r = nifreader.open(infile)
  discard nifreader.processDirectives(r)
  var tok = default(nifreader.ExpandedToken)
  nifreader.next(r, tok)                        # `(stmts`
  var buf = createTokenBuf(64)
  if tok.tk == ParLe:
    nifcoreparse.parse(r, buf)                  # one subtree: `(dce …)`
  nifreader.close(r)

  result = ModuleAnalysis()
  var n = beginRead(buf)
  if not (n.isTagLit and n.cursorTagId == globalTags.registerTag(dceName)):
    # The only way to get here is a cache an older hexer filled, which wrote
    # the analysis to a `.dce.nif` of its own and nothing into the `.x.nif`.
    raiseAssert infile & ": no (dce …) section; the nifcache predates this " &
                "hexer and must be deleted"
  let depTag = globalTags.registerTag(depName)
  let offerTag = globalTags.registerTag(offerName)
  let rootTag = globalTags.registerTag(rootName)
  n.into:                                       # (dce ...)
    while n.hasMore:
      if not n.isTagLit:
        raiseAssert infile & ": expected ParLe"
      if n.cursorTagId == rootTag:
        n.into:                                 # (roots ...)
          while n.hasMore:
            if n.kind == Symbol:
              result.roots.incl(n.symId)
              skip n
            else:
              raiseAssert infile & ": expected Symbol"
      elif n.cursorTagId == depTag:
        n.into:                                 # (uses ...)
          let key = n.symId
          result.uses[key] = initHashSet[SymId]()
          skip n
          while n.hasMore:
            if n.kind == Symbol:
              result.uses.getOrQuit(key).incl(n.symId)
              skip n
            else:
              raiseAssert infile & ": expected Symbol"
      elif n.cursorTagId == offerTag:
        n.into:                                 # (offers ...)
          while n.hasMore:
            if n.kind == Symbol:
              result.offers.incl(n.symId)
              skip n
            else:
              raiseAssert infile & ": expected Symbol"
      else:
        raiseAssert infile & ": expected (roots|uses|offers)"

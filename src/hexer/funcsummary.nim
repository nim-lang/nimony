#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Alias-aware function summaries shared by late Hexer passes and the Leng
## optimizer (CSE). A summary partitions the parameters, the result and an
## implicit "outside" world, plus per-element may-read / may-write effects.
## It is *bounded in space by the arity* of the proc: independent of the body
## size.
##
## Effects are directed: `obj.field = x` writes `obj` and reads `x` (it does
## not by itself write `x`). Identity still joins partition classes, including
## when the dest is a field or deref: after `x.obj = y` with `obj` a pointer,
## `x.obj.a = 3` mutates `y`, so `x` and `y` must share a class. A projection
## that is not an identity (`return obj.field` of a value, `x = f(a, b)`)
## does not join. A `cast` to a pointer-bearing type from a value that does
## not carry pointer identity (`cast[ptr T](4000)`, `cast[ptr T](intParam)`)
## forges an untracked pointee: the proc is `writeGlobal`/`readGlobal`.
## Pointer-to-pointer casts, `cast[ptr T](addr x)`, and value puns
## (`cast[byte](v)`) are not forging.
##
## The wire format is NIF, for example:
## `(smry raises (param 0 0 writes slot) (param 1 0 reads) result 2)`
## means: may raise; params 0 and 1 are in the same partition class 0 (they may
## alias via identity); param 0 is written and its slot reassigned; param 1 is
## read; the result is its own fresh class (`2 == params.len`). See `doc/tags.md`.

import std / [assertions, tables]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lengc / [leng_model, nifmodules, typenav, pointerbearing]
import pointertypes                # imported types: resolve via the Nimony decl

type
  ParamEffect* = object
    cls*: uint32        ## partition class id; params sharing a `cls` may alias
                        ## via pointer/object identity (including identity stored
                        ## into a field/deref). Canonical = smallest interface
                        ## index in the class.
    reads*: bool        ## call may read through this param's reachable graph
    writes*: bool       ## call may write through this param's reachable graph
    slotWritten*: bool  ## `var` param whose own binding is reassigned (not just
                        ## its pointee)
    escapes*: bool      ## param's graph is stored into a global or passed to a
                        ## callee with no summary

  FunctionSummary* = object
    writesGlobal*: bool
    readsGlobal*: bool
    callsUnknown*: bool
    raises*: bool
    resultCls*: uint32   ## class the result joins; `== params.len` ⇒ own fresh class
    resultEscapes*: bool
    params*: seq[ParamEffect]

  FunctionSummaryTable* = Table[SymId, FunctionSummary]

# ---- queries used by the optimizer ----------------------------------------

proc isReadOnly*(s: FunctionSummary): bool {.inline.} =
  if s.writesGlobal or s.callsUnknown: return false
  for p in s.params:
    if p.writes or p.slotWritten: return false
  result = true

proc paramMayWrite*(s: FunctionSummary; idx: int): bool {.inline.} =
  ## Conservative: an out-of-range index (e.g. a varargs actual) or an unknown
  ## callee is assumed to write.
  if s.callsUnknown: return true
  if idx < 0 or idx >= s.params.len: return true
  result = s.params[idx].writes or s.params[idx].slotWritten

proc paramDirectEscapes*(s: FunctionSummary; idx: int): bool {.inline.} =
  if s.callsUnknown: return true
  result = idx >= 0 and idx < s.params.len and s.params[idx].escapes

# ---------------------------------------------------------------------------
# Type context.
#
# Leng is nominal — `(param :x.0 . Point.0.M)`, never an inline object — so an
# analysis that cannot follow a `Symbol` in a type slot has to call *every*
# aggregate pointer-bearing, and then every copy of one merges two alias
# classes. That resolution is exactly what `lengc/typenav` does, so this pass
# uses it rather than a second navigator: `nifmodules.fromBuffer` builds a
# `MainModule` over the very buffer being annotated (it carries every
# `(type …)`, every global and every proc signature of the module), and
# `getType` / `pointerBearing` answer from it — the same two calls the alias
# pass in shoggoth makes.
#
# The one thing that context deliberately cannot do here is load a *sibling*
# module: those `.c.nif`s are later outputs of the same build, so whether one is
# on disk depends on scheduling (hence `noForeign`). Imported symbols are the
# common case, not an exotic one — `string` alone accounts for most type slots
# in a typical module — so they are not guessed at either: `pointertypes` reads
# the symbol's *Nimony* declaration through the same on-demand `.s.nif` loader
# every other hexer pass uses. That covers imported types, imported globals and
# fields of imported objects alike, and it is the answer, not a fallback — there
# is no case in which this pass shrugs and calls something pointer-bearing
# because it could not find a declaration.
# ---------------------------------------------------------------------------

proc buildTypeEnv*(buf: var TokenBuf): MainModule =
  ## The module being annotated, as its own type context. `buf` is borrowed:
  ## it must outlive the result and must not be reallocated meanwhile, which
  ## holds because summarizing only reads it — the pragmas are spliced into a
  ## fresh buffer afterwards.
  fromBuffer(buf, "hexer")

proc exprMayHoldPointer(env: var MainModule; n: Cursor): bool {.inline.} =
  ## True unless the *value* of `n` provably cannot contain a pointer. This is
  ## the type question — "can copying this value create a path into another
  ## graph?" — and is what gates class merging. It is deliberately NOT the
  ## identity question `sourceMayBePointer` answers.
  transfersPointer(addr env, n, nimonyMayHoldPointer, nimonyFieldMayHoldPointer)

proc sourceMayBePointer(env: var MainModule; n: Cursor): bool {.inline.} =
  ## True unless `n` is provably a value carrying no pointer *identity*.
  ## `(cast[uint](p))` and `(add (cast[uint](p)) n)` still carry `p`'s identity
  ## even though their static type is an integer, so the operands are followed
  ## rather than the type — pointer arithmetic is not forging.
  pointerbearing.sourceMayBePointer(addr env, n, nimonyMayHoldPointer,
                                    nimonyFieldMayHoldPointer)

proc isForgingCast(env: var MainModule; n: Cursor): bool {.inline.} =
  pointerbearing.isForgingCast(addr env, n, nimonyMayHoldPointer,
                              nimonyFieldMayHoldPointer)

# ---------------------------------------------------------------------------
# Extraction: directed (Andersen-style) flow into a bounded interface.
# Interface elements are indexed 0..nParams: params are 0..nParams-1 and the
# result is `nParams`. `cls` values are the canonical (smallest) element index
# of a class; the result element being the largest index can never become a
# class root, so a param's `cls` is always a param index. Classes merge on
# identity copies (`p = q`, `return p`, and `obj.field = p` / `p[] = q` of
# an identity — a pointer cell then names that graph). Value projections
# (`return obj.field`, `x = f(a, b)`) do not join.
# ---------------------------------------------------------------------------

const
  OutsideElem = -1                 ## untracked symbol: a global / unknown graph

type
  CallFact = object
    callee: SymId
    argRoots: seq[seq[int]]   ## per actual: the interface elements it may root at
    argCarries: seq[bool]     ## per actual: whether its value can carry a
                              ## pointer. A pointer-free actual is passed *by
                              ## value*, so the callee gets no path back into
                              ## the caller's graph through it — it cannot be
                              ## written through, cannot leak, and cannot make
                              ## two actuals alias.

  ProcAnalysis = object
    nParams: int
    uf: seq[int]                       ## union-find parent array, length nParams+1
    paramLookup: Table[SymId, int]
    localRoots: Table[SymId, seq[int]] ## local sym -> interface elements it aliases
    reads, writes, slotW, escapes: seq[bool]  ## per element, length nParams+1
    writesGlobal, readsGlobal, callsUnknown, raises: bool
    calls: seq[CallFact]

proc ufFind(u: var seq[int]; x: int): int =
  var r = x
  while u[r] != r: r = u[r]
  var i = x
  while u[i] != r:
    let nxt = u[i]
    u[i] = r
    i = nxt
  result = r

proc ufUnion(u: var seq[int]; a, b: int) =
  if a < 0 or b < 0 or a >= u.len or b >= u.len: return
  let ra = ufFind(u, a)
  let rb = ufFind(u, b)
  if ra != rb:
    if ra < rb: u[rb] = ra      # keep the smaller index as the canonical root
    else: u[ra] = rb

proc markReadElem(a: var ProcAnalysis; e: int) =
  if e < 0:
    a.readsGlobal = true        # reading a global / unknown graph
  elif e < a.nParams:
    a.reads[e] = true
  # e == nParams: reading this proc's result local, not a global

proc markWriteElem(a: var ProcAnalysis; e: int; slot = false) =
  if e < 0:
    a.writesGlobal = true       # write through a global / unknown graph
  elif e == a.nParams:
    discard                     # write through this proc's result — not a global
  else:
    a.writes[e] = true
    if slot: a.slotW[e] = true

proc markEscapeElem(a: var ProcAnalysis; e: int) =
  if e < 0: a.callsUnknown = true
  elif e <= a.nParams: a.escapes[e] = true


proc unwrapConv(env: var MainModule; n: Cursor): Cursor =
  ## Strip `(conv)`/`(baseobj)`/`(par)` wrappers, and non-forging `(cast)`,
  ## so the head is the underlying value. A forging cast is not identity.
  result = n
  while result.kind == TagLit:
    case result.exprKind
    of ConvC:
      inc result
      skip result                # type
    of CastC:
      if isForgingCast(env, result): break
      inc result
      skip result
    of BaseobjC:
      inc result
      skip result                # type
      skip result                # inheritance-depth
    of ParC:
      result = result.childCursor
    else:
      break

proc isIdentityExpr(env: var MainModule; n: Cursor): bool =
  ## True when `n` names an existing graph (`p`, `addr p`) rather than a
  ## projection or computed value. Used to decide whether a copy should join
  ## partition classes.
  var n = unwrapConv(env, n)
  while n.kind == TagLit and n.exprKind in {AddrC, HaddrC}:
    n = unwrapConv(env, n.childCursor)
  result = n.kind == Symbol

proc transfersGraph(env: var MainModule; n: Cursor): bool {.inline.} =
  ## Whether copying `n` should join the two sides' partition classes: it must
  ## *name* an existing graph AND carry a value that can actually hold a
  ## pointer. `p = intParam` names `intParam`, but copying a machine word
  ## creates no path between the two graphs, so merging their classes would
  ## only lose CSE. This is the summary-side twin of the type test in
  ## `shoggoth/aliasing.connect`.
  isIdentityExpr(env, n) and exprMayHoldPointer(env, n)

proc isTrackedSym(a: ProcAnalysis; s: SymId): bool {.inline.} =
  s != SymId(0) and (a.paramLookup.hasKey(s) or a.localRoots.hasKey(s))

proc baseSymOf(env: var MainModule; n: Cursor): SymId =
  ## Innermost symbol of an lvalue path, or `SymId(0)` if the path is computed.
  var n = unwrapConv(env, n)
  while n.kind == TagLit:
    case n.exprKind
    of DotC, DerefC, PatC, AtC, AddrC, HaddrC, ParC:
      n = unwrapConv(env, n.childCursor)
    else:
      return SymId(0)
  if n.kind == Symbol: result = n.symId
  else: result = SymId(0)

proc collectParamSyms(params: Cursor): seq[SymId] =
  result = @[]
  if not params.isTagLit: return
  var p = params
  p.into:
    while p.hasMore:
      if p.substructureKind == ParamU:
        var q = p
        inc q
        if q.kind == SymbolDef:
          result.add q.symId
      skip p

proc exprRoots(a: var ProcAnalysis; env: var MainModule; n: Cursor): seq[int] =
  ## The interface elements the value of `n` may be rooted at. Constructors,
  ## literals and unrecognised forms yield the empty set (a "fresh" value).
  result = @[]
  var n = n
  case n.kind
  of Symbol:
    if a.paramLookup.hasKey(n.symId):
      result.add a.paramLookup.getOrQuit(n.symId)
    elif a.localRoots.hasKey(n.symId):
      result = a.localRoots.getOrQuit(n.symId)
    else:
      result.add OutsideElem     # global / unknown symbol
  of TagLit:
    case n.exprKind
    of DotC, DerefC, PatC, AtC, AddrC, HaddrC:
      result = exprRoots(a, env, n.childCursor)
    of ConvC:
      var r = n
      inc r
      skip r                 # skip the type operand
      result = exprRoots(a, env, r)
    of CastC:
      if isForgingCast(env, n):
        result = @[]         # untracked pointee, not the inner integer
      else:
        var r = n
        inc r
        skip r
        result = exprRoots(a, env, r)
    of BaseobjC:
      var r = n
      inc r
      skip r                 # type
      skip r                 # inheritance-depth intlit
      result = exprRoots(a, env, r)
    of CallC:
      # Conservative "select": without the callee's summary yet, a pointer
      # result may be one of the arguments. Value results (bool, int) still
      # get these roots, but `handleRet` / slot rebind refuse to *join classes*
      # on a non-identity expression, so `return f(a, b)` does not make the
      # result alias `a`.
      var r = n
      r = sub(r)  # throwaway copy; no leaveScope needed
      skip r                 # callee
      while r.hasMore:
        for e in exprRoots(a, env, r): result.add e
        skip r
    else:
      discard
  else:
    discard

proc walkStmt(a: var ProcAnalysis; env: var MainModule; n: var Cursor)

proc lastChildStart(n: Cursor): Cursor =
  ## Returns a cursor to the last child of the `(tag ...)` node `n`.
  result = n
  var c = n
  c = sub(c)  # throwaway copy; no leaveScope needed
  while c.hasMore:
    result = c
    skip c

proc registerDeclType(env: var MainModule; n: Cursor) =
  ## Put a local declaration's type into the typenav scope, so a later use of
  ## the symbol has one. Every local kind has to be registered, not just `var`:
  ## an unregistered symbol has no type to recompute, and this pass does not
  ## have a "then assume the worst" branch to fall into.
  var c = n
  inc c
  if c.kind != SymbolDef: return
  let sym = c.symId
  var t = c
  if t.hasMore: skip t             # name
  if t.hasMore: skip t             # pragmas
  if t.hasMore and t.kind != DotToken:
    registerLocal(env, sym, t)

proc handleLocalDecl(a: var ProcAnalysis; env: var MainModule; n: var Cursor) =
  var c = n
  inc c
  let sym = if c.kind == SymbolDef: c.symId else: SymId(0)
  # Register the local's declared type before anything asks about a use of it
  # (mirrors how the C backend and the alias pass drive the typenav scopes).
  registerDeclType(env, n)
  let initStart = lastChildStart(n)
  let roots = exprRoots(a, env, initStart)
  if sym != SymId(0):
    # Only an identity init (`var x = p`) lets x name p's graph. A field
    # load, call, or computed value is a fresh copy.
    if transfersGraph(env, initStart): a.localRoots[sym] = roots
    else: a.localRoots[sym] = @[]
  for e in roots: markReadElem(a, e)
  var ic = initStart
  walkStmt(a, env, ic)            # capture nested calls / addr in the initializer
  skip n

proc handleAssign(a: var ProcAnalysis; env: var MainModule; n: var Cursor; reversed: bool) =
  var c = n
  inc c
  let firstStart = c
  skip c
  let secondStart = c
  let destStart = if reversed: secondStart else: firstStart
  let valStart = if reversed: firstStart else: secondStart

  let valRoots = exprRoots(a, env, valStart)
  let destBareSym = destStart.kind == Symbol

  if destBareSym and a.paramLookup.hasKey(destStart.symId):
    # Slot rebind of a param: `p = …`. Writes p's binding, not p's old pointee
    # graph. Join classes only on identity (`p = q`), never on `p = obj.field`.
    let e = a.paramLookup.getOrQuit(destStart.symId)
    markWriteElem(a, e, slot = true)
    if transfersGraph(env, valStart):
      for r in valRoots: ufUnion(a.uf, e, r)
  elif destBareSym:
    # Copy into a local. Only an identity (`x = y`) rebinds x to y's graph;
    # `x = obj.field` / `x = f(a, b)` is a fresh copy — otherwise returning
    # that local would join every argument of f via the result element.
    if not a.localRoots.hasKey(destStart.symId):
      a.writesGlobal = true
    if transfersGraph(env, valStart): a.localRoots[destStart.symId] = valRoots
    else: a.localRoots[destStart.symId] = @[]
  else:
    # Store through dest: `obj.field = x`, `p[] = x`, `a[i] = x`.
    # Writes dest's object; reads x. If x is an identity, the cell now
    # names x's graph: `x.obj = y` then `x.obj.a = 3` mutates y.
    let destRoots = exprRoots(a, env, destStart)
    if destRoots.len == 0:
      # Empty roots: a fresh stack local (result, or a not-yet-aliased var)
      # vs. a computed / untracked address. Only the latter is a global write.
      if not isTrackedSym(a, baseSymOf(env, destStart)):
        a.writesGlobal = true
    else:
      for e in destRoots:
        markWriteElem(a, e)
      if transfersGraph(env, valStart):
        for e in destRoots:
          for r in valRoots: ufUnion(a.uf, e, r)
    var dc = destStart
    walkStmt(a, env, dc)                            # index exprs / nested calls in dest

  for r in valRoots: markReadElem(a, r)
  var vc = valStart
  walkStmt(a, env, vc)                              # nested calls in the value
  skip n

proc handleRet(a: var ProcAnalysis; env: var MainModule; n: var Cursor) =
  var c = n
  c = sub(c)  # throwaway copy; no leaveScope needed
  if c.hasMore and c.kind != DotToken:
    let roots = exprRoots(a, env, c)
    if transfersGraph(env, c):
      for r in roots:
        ufUnion(a.uf, a.nParams, r)            # result aliases a returned identity
    for r in roots:
      markReadElem(a, r)
    var rc = c
    walkStmt(a, env, rc)
  skip n

proc handleCall(a: var ProcAnalysis; env: var MainModule; n: var Cursor) =
  var c = n
  c = sub(c)  # throwaway copy; no leaveScope needed
  let calleeStart = c
  var fact = CallFact(callee: SymId(0), argRoots: @[], argCarries: @[])
  if calleeStart.kind == Symbol:
    fact.callee = calleeStart.symId
  skip c                                       # past callee
  while c.hasMore:
    let roots = exprRoots(a, env, c)
    let carries = exprMayHoldPointer(env, c)
    fact.argRoots.add roots
    fact.argCarries.add carries
    for r in roots: markReadElem(a, r)
    if fact.callee == SymId(0) and carries:
      for r in roots:
        markWriteElem(a, r)                    # unknown callee may write & leak
        markEscapeElem(a, r)
    var ac = c
    walkStmt(a, env, ac)
    skip c
  if fact.callee != SymId(0):
    a.calls.add ensureMove fact
  else:
    a.callsUnknown = true
  skip n

proc walkStmt(a: var ProcAnalysis; env: var MainModule; n: var Cursor) =
  if n.kind == Symbol:
    if a.paramLookup.hasKey(n.symId):
      markReadElem(a, a.paramLookup.getOrQuit(n.symId))
    inc n
    return
  if not n.isTagLit:
    inc n
    return
  case n.stmtKind
  of VarS:
    handleLocalDecl(a, env, n)
  of AsgnS:
    handleAssign(a, env, n, reversed = false)
  of StoreS:
    handleAssign(a, env, n, reversed = true)        # `(store value dest)`
  of RetS:
    handleRet(a, env, n)
  of CallS:
    handleCall(a, env, n)
  of RaiseS:
    a.raises = true
    n.loopInto:
      walkStmt(a, env, n)
  of GvarS, TvarS, ConstS:
    # Not routed through `handleLocalDecl` — a global's effects are the `else`
    # walk's business — but its type still has to reach the scope.
    registerDeclType(env, n)
    n.loopInto:
      walkStmt(a, env, n)
  of ScopeS:                        # explicit lexical scope: its locals end here
    openScope(env)
    n.loopInto:
      walkStmt(a, env, n)
    closeScope(env)
  else:
    if n.exprKind in AddrKinds:
      let escRoots = exprRoots(a, env, n.childCursor)
      for r in escRoots: markEscapeElem(a, r)
    elif n.exprKind == CastC and isForgingCast(env, n):
      a.writesGlobal = true
      a.readsGlobal = true
    n.loopInto:
      walkStmt(a, env, n)

proc markAllUnknown(a: var ProcAnalysis) =
  a.callsUnknown = true
  a.writesGlobal = true
  a.readsGlobal = true
  for i in 0 ..< a.nParams:
    a.reads[i] = true
    a.writes[i] = true
    a.slotW[i] = true
    a.escapes[i] = true

proc computeProcAnalysis(procDecl: Cursor; env: var MainModule): ProcAnalysis =
  var p = procDecl
  let d = takeProcDecl(p)
  let paramSyms = collectParamSyms(d.params)
  result = ProcAnalysis(nParams: paramSyms.len)
  result.uf = newSeq[int](paramSyms.len + 1)
  for i in 0 .. paramSyms.len: result.uf[i] = i
  result.reads = newSeq[bool](paramSyms.len + 1)
  result.writes = newSeq[bool](paramSyms.len + 1)
  result.slotW = newSeq[bool](paramSyms.len + 1)
  result.escapes = newSeq[bool](paramSyms.len + 1)
  for i, s in paramSyms: result.paramLookup[s] = i
  if d.body.isTagLit:
    if d.name.kind == SymbolDef: setPointerTypeContext(pool.syms[d.name.symId])
    # The params' types live in a scope of their own, exactly as `optdriver`
    # opens one around each body it optimizes.
    openScope(env)
    registerParams(env, d.params)
    var body = d.body
    walkStmt(result, env, body)
    closeScope(env)
  else:
    markAllUnknown result

proc finalizeSummary(a: var ProcAnalysis): FunctionSummary =
  result = FunctionSummary(
    writesGlobal: a.writesGlobal, readsGlobal: a.readsGlobal,
    callsUnknown: a.callsUnknown, raises: a.raises)
  result.params = newSeq[ParamEffect](a.nParams)
  for i in 0 ..< a.nParams:
    result.params[i] = ParamEffect(
      cls: uint32(ufFind(a.uf, i)),
      reads: a.reads[i], writes: a.writes[i],
      slotWritten: a.slotW[i], escapes: a.escapes[i])
  result.resultCls = uint32(ufFind(a.uf, a.nParams))
  result.resultEscapes = a.escapes[a.nParams]

proc computeFunctionSummary*(procDecl: Cursor; env: var MainModule): FunctionSummary =
  ## Intra-procedural summary only (no callee summaries available). `env` is the
  ## enclosing module as a type context (`buildTypeEnv`).
  var a = computeProcAnalysis(procDecl, env)
  result = finalizeSummary(a)

proc applyCallee(a: var ProcAnalysis; call: CallFact; callee: FunctionSummary) =
  a.raises = a.raises or callee.raises
  a.writesGlobal = a.writesGlobal or callee.writesGlobal
  a.readsGlobal = a.readsGlobal or callee.readsGlobal
  a.callsUnknown = a.callsUnknown or callee.callsUnknown
  var byCls = initTable[uint32, int]()        # callee class -> a representative caller element
  for k in 0 ..< callee.params.len:
    let pe = callee.params[k]
    let roots = if k < call.argRoots.len: call.argRoots[k] else: newSeq[int]()
    # A pointer-free actual is a by-value copy: the callee can read it, but it
    # holds no path back into our graphs, so it cannot be written through, it
    # cannot leak, and it cannot make two of our actuals alias each other.
    let carries = k >= call.argCarries.len or call.argCarries[k]
    if pe.reads:
      for r in roots: markReadElem(a, r)
    if carries:
      if pe.writes or pe.slotWritten:
        for r in roots: markWriteElem(a, r, pe.slotWritten)
      if pe.escapes:
        if roots.len == 0: a.callsUnknown = true
        else:
          for r in roots: markEscapeElem(a, r)
      # Union caller actuals the callee placed in one class (identity aliases
      # only — the callee no longer merges classes on field store/load).
      for r in roots:
        if r < 0: continue
        if byCls.hasKey(pe.cls): ufUnion(a.uf, byCls.getOrQuit(pe.cls), r)
        else: byCls[pe.cls] = r

proc applyExternalCall(a: var ProcAnalysis; call: CallFact) =
  a.callsUnknown = true
  for k in 0 ..< call.argRoots.len:
    if k < call.argCarries.len and not call.argCarries[k]: continue
    for r in call.argRoots[k]:
      markWriteElem(a, r)
      markEscapeElem(a, r)

proc resolveSummaries(analyses: var Table[SymId, ProcAnalysis]): FunctionSummaryTable =
  ## Least fixpoint over the in-module call graph. Effects are monotone (bits
  ## only get set, classes only merge), so iteration converges; the cap is a
  ## safety net.
  const MaxIters = 10
  result = initTable[SymId, FunctionSummary]()
  for sym, a in analyses.mpairs:
    result[sym] = finalizeSummary(a)
  for _ in 0 ..< MaxIters:
    var changed = false
    for sym, a in analyses.mpairs:
      for call in a.calls:
        if analyses.hasKey(call.callee):
          applyCallee(a, call, result.getOrDefault(call.callee))
        else:
          applyExternalCall(a, call)
      let s = finalizeSummary(a)
      if s != result.getOrDefault(sym):
        result[sym] = s
        changed = true
    if not changed: break

proc collectProcAnalyses(buf: var TokenBuf; env: var MainModule): Table[SymId, ProcAnalysis] =
  result = initTable[SymId, ProcAnalysis]()
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.isTagLit and n.stmtKind == ProcS:
          let p = n
          var d = n
          inc d
          if d.isSymbolDef:
            result[d.symId] = computeProcAnalysis(p, env)
        skip n

# ---- serialization --------------------------------------------------------

proc readParamSummary(n: var Cursor; outSummary: var FunctionSummary) =
  # no early `return` here: it would skip `into`'s epilogue and leave the
  # caller's cursor mid-scope
  n.into:
    var idx = -1
    if n.kind == IntLit:
      idx = int(n.intVal)
      inc n
    if idx < 0:
      while n.hasMore: skip n
    else:
      while outSummary.params.len <= idx:
        outSummary.params.add ParamEffect()
      var cls = uint32(idx)
      if n.kind == IntLit:
        cls = uint32(n.intVal)
        inc n
      outSummary.params[idx].cls = cls
      while n.hasMore:
        if n.kind == Ident:
          case pool.strings[n.strId]
          of "reads": outSummary.params[idx].reads = true
          of "writes": outSummary.params[idx].writes = true
          of "slot": outSummary.params[idx].slotWritten = true
          of "escapes": outSummary.params[idx].escapes = true
          else: discard
          inc n
        else:
          skip n

proc readSummary(n: var Cursor; outSummary: var FunctionSummary): bool =
  result = true
  outSummary = FunctionSummary()
  var sawResult = false
  n.into:
    while n.hasMore:
      if n.kind == Ident:
        case pool.strings[n.strId]
        of "writeGlobal": outSummary.writesGlobal = true; inc n
        of "readGlobal": outSummary.readsGlobal = true; inc n
        of "callsUnknown": outSummary.callsUnknown = true; inc n
        of "raises": outSummary.raises = true; inc n
        of "resultEscapes": outSummary.resultEscapes = true; inc n
        of "result":
          inc n
          if n.kind == IntLit:
            outSummary.resultCls = uint32(n.intVal)
            sawResult = true
            inc n
        else: inc n
      elif n.isTagLit and n.substructureKind == ParamU:
        readParamSummary(n, outSummary)
      else:
        skip n
  if not sawResult:
    outSummary.resultCls = uint32(outSummary.params.len)

proc readSummaryPragma*(pragmas: Cursor; outSummary: var FunctionSummary): bool =
  if not pragmas.isTagLit: return false
  var p = pragmas
  p.into:
    while p.hasMore:
      if p.isTagLit and p.pragmaKind == SmryP:
        return readSummary(p, outSummary)
      skip p
  result = false

proc collectFunctionSummaries*(buf: var TokenBuf): FunctionSummaryTable =
  result = initTable[SymId, FunctionSummary]()
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.isTagLit and n.stmtKind == ProcS:
          let d = takeProcDecl(n)
          var summary = FunctionSummary()
          if d.name.isSymbolDef and readSummaryPragma(d.pragmas, summary):
            result[d.name.symId] = summary
        else:
          skip n

proc addSummaryPragma(dest: var TokenBuf; summary: FunctionSummary; info: NifLineInfo) =
  dest.addParLe(TagId(SmryP), info)
  if summary.writesGlobal: dest.addIdent "writeGlobal", info
  if summary.readsGlobal: dest.addIdent "readGlobal", info
  if summary.callsUnknown: dest.addIdent "callsUnknown", info
  if summary.raises: dest.addIdent "raises", info
  for i, p in summary.params:
    dest.addParLe(TagId(ParamU), info)
    dest.addIntLit i, info
    dest.addIntLit int(p.cls), info
    if p.reads: dest.addIdent "reads", info
    if p.writes: dest.addIdent "writes", info
    if p.slotWritten: dest.addIdent "slot", info
    if p.escapes: dest.addIdent "escapes", info
    dest.addParRi()
  if int(summary.resultCls) != summary.params.len or summary.resultEscapes:
    dest.addIdent "result", info
    dest.addIntLit int(summary.resultCls), info
    if summary.resultEscapes: dest.addIdent "resultEscapes", info
  dest.addParRi()

proc writePragmasWithSummary(dest: var TokenBuf; pragmas: Cursor;
                             summary: FunctionSummary; info: NifLineInfo) =
  var p = pragmas
  if p.isDotToken:
    dest.addParLe(TagId(PragmasU), info)
    addSummaryPragma(dest, summary, info)
    dest.addParRi()
  elif p.isTagLit:
    dest.addParLe(p.cursorTagId, p.info)
    var hadSummary = false
    p.into:
      while p.hasMore:
        if p.isTagLit and p.pragmaKind == SmryP:
          addSummaryPragma(dest, summary, p.info)
          skip p
          hadSummary = true
        else:
          dest.takeTree p
    if not hadSummary:
      addSummaryPragma(dest, summary, info)
    dest.addParRi()
  else:
    dest.addSubtree p

proc annotateSummaries(dest: var TokenBuf; n: var Cursor;
                       summaries: FunctionSummaryTable) =
  case n.kind
  of TagLit:
    if n.stmtKind == ProcS:
      let tag = n.cursorTagId
      let info = n.info
      let d = takeProcDecl(n)
      dest.addParLe(tag, info)
      dest.addSubtree d.name
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      if d.name.isSymbolDef and summaries.hasKey(d.name.symId):
        writePragmasWithSummary(dest, d.pragmas,
                                summaries.getOrQuit(d.name.symId), info)
      else:
        dest.addSubtree d.pragmas
      dest.addSubtree d.body
      dest.addParRi()
    else:
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          annotateSummaries(dest, n, summaries)
      dest.addParRi()
  else:
    dest.takeTree n

proc annotateFunctionSummaries*(buf: var TokenBuf) =
  # The buffer is its own type context: one pre-pass over it registers the
  # nominal types every signature is written in (see `buildTypeEnv`).
  var env = buildTypeEnv(buf)
  var analyses = collectProcAnalyses(buf, env)
  let summaries = resolveSummaries(analyses)

  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len + buf.len div 16)
  annotateSummaries(dest, n, summaries)
  buf = ensureMove(dest)

when isMainModule:
  proc summaryIn(src: string; name: string): FunctionSummary =
    ## The module path: `src` is a whole `(stmts …)` body, so its `(type …)`
    ## declarations become the type environment — this is what
    ## `annotateFunctionSummaries` does.
    var buf = parseFromBuffer("(stmts " & src & ")", "M")
    var env = buildTypeEnv(buf)
    var analyses = collectProcAnalyses(buf, env)
    let summaries = resolveSummaries(analyses)
    let s = pool.syms.getOrIncl(name)
    doAssert summaries.hasKey(s), "no summary for " & name
    result = summaries.getOrQuit(s)

  const PairTypes = """
    (type :Cell.0 . (object . (fld :a.0 . (i +32))))
    (type :Node.0 . (object . (fld :fld.0 . (ptr Cell.0))))"""

  proc twoParams(body: string): FunctionSummary =
    ## `obj` is an object with a pointer field and `x` a pointer of that field's
    ## type — the shape the identity and effect rules below are about. Types are
    ## declared, not omitted: an untyped slot is not valid Leng, and the pass
    ## reports one rather than guessing what it might have held.
    summaryIn(PairTypes & """
      (proc :f.0
        (params
          (param :obj.0 . Node.0)
          (param :x.0 . (ptr Cell.0)))
        . . (stmts """ & body & "))", "f.0")

  const ScalarTypes = """
    (type :Point.0 . (object . (fld :px.0 . (f +64)) (fld :py.0 . (f +64))))
    (type :Alias.0 . Point.0)
    (type :Boxed.0 . (object . (fld :bp.0 . (ptr (i +32)))))
    (type :Base.0 . (object . (fld :vt.0 . (ptr (i +32)))))
    (type :Derived.0 . (object Base.0 (fld :dx.0 . (i +32))))
    (type :Grid.0 . (array (f +64) +16))
    (type :Ptrs.0 . (array (ptr (i +32)) +16))"""

  proc typedPair(t1, t2, body: string; name = "f.0"): FunctionSummary =
    ## Two params of the named types, so the type environment decides whether a
    ## copy between them can alias.
    summaryIn(ScalarTypes & """
      (proc :""" & name & """
        (params
          (param :obj.0 . """ & t1 & """)
          (param :x.0 . """ & t2 & """))
        . . (stmts """ & body & "))", name)

  block scalar_object_copy_does_not_join:
    # `obj = x` for a nominal object of two floats: an identity copy, but the
    # value carries no pointer, so the two graphs stay separate. Before the
    # type environment every nominal type was assumed pointer-bearing and this
    # merged the classes.
    let s = typedPair("Point.0", "Point.0", "(asgn obj.0 x.0)")
    doAssert s.params[0].slotWritten
    doAssert s.params[1].reads
    doAssert s.params[0].cls != s.params[1].cls

  block type_alias_chain_resolves:
    let s = typedPair("Alias.0", "Alias.0", "(asgn obj.0 x.0)")
    doAssert s.params[0].cls != s.params[1].cls

  block pointer_field_object_copy_joins:
    let s = typedPair("Boxed.0", "Boxed.0", "(asgn obj.0 x.0)")
    doAssert s.params[0].cls == s.params[1].cls

  block inherited_pointer_field_joins:
    # `Derived` has only an int of its own; the pointer sits in its base.
    let s = typedPair("Derived.0", "Derived.0", "(asgn obj.0 x.0)")
    doAssert s.params[0].cls == s.params[1].cls

  block scalar_array_copy_does_not_join:
    let s = typedPair("Grid.0", "Grid.0", "(asgn obj.0 x.0)")
    doAssert s.params[0].cls != s.params[1].cls

  block pointer_array_copy_joins:
    let s = typedPair("Ptrs.0", "Ptrs.0", "(asgn obj.0 x.0)")
    doAssert s.params[0].cls == s.params[1].cls

  # There is no `unloadable type` case to test: an imported symbol is resolved
  # from its own module's `.s.nif` (`pointertypes`), and one that cannot be is a
  # hard error, not a conservative answer. The imported path needs a real sem
  # output to exercise, so it is covered by running the pass over every module
  # of the test suite rather than by a snippet here.

  block scalar_field_store_does_not_join:
    # `obj.px = x` writes obj and reads the float x, but stores no identity.
    let s = typedPair("Ptrs.0", "(f +64)", "(asgn (dot obj.0 px.0) x.0)")
    doAssert s.params[0].writes
    doAssert s.params[1].reads
    doAssert s.params[0].cls != s.params[1].cls

  block pointer_field_store_joins:
    let s = typedPair("Boxed.0", "(ptr (i +32))", "(asgn (dot obj.0 bp.0) x.0)")
    doAssert s.params[0].cls == s.params[1].cls

  block field_load_of_pointer_type_joins:
    # `obj = x.bp` is a projection, not an identity — it must not join even
    # though the loaded value is a pointer.
    let s = typedPair("(ptr (i +32))", "Boxed.0", "(asgn obj.0 (dot x.0 bp.0))")
    doAssert s.params[0].cls != s.params[1].cls

  block return_scalar_object_does_not_join_result:
    let s = typedPair("Point.0", "Point.0", "(ret obj.0)")
    doAssert int(s.resultCls) == 2

  block return_pointer_object_joins_result:
    let s = typedPair("Boxed.0", "Boxed.0", "(ret obj.0)")
    doAssert int(s.resultCls) == 0

  block forging_cast_from_typed_int_is_detected:
    # `cast[ptr T](intParam)` forges a pointer to untracked memory. Without a
    # type for `x.0` this looked like a pointer pun and went unnoticed.
    let s = typedPair("Boxed.0", "(i +64)", "(asgn obj.0 (cast (ptr (i +32)) x.0))")
    doAssert s.writesGlobal
    doAssert s.readsGlobal

  block cast_from_typed_pointer_is_a_pun:
    let s = typedPair("Boxed.0", "(ptr (i +32))",
                      "(asgn obj.0 (cast (ptr (i +32)) x.0))")
    doAssert not s.writesGlobal
    doAssert s.params[0].cls == s.params[1].cls

  block scalar_arg_to_unknown_callee_is_not_written:
    # An unknown callee receiving a by-value float cannot write through it.
    let s = typedPair("Ptrs.0", "(f +64)", "(call (deref obj.0) x.0)")
    doAssert s.callsUnknown
    doAssert not s.params[1].writes
    doAssert not s.params[1].escapes

  block pointer_arg_to_unknown_callee_escapes:
    let s = typedPair("Ptrs.0", "(ptr (i +32))", "(call (deref obj.0) x.0)")
    doAssert s.params[1].writes
    doAssert s.params[1].escapes

  block scalar_arg_to_known_writer_is_not_written:
    # `g` writes through its param; passing a pointer-free value by value still
    # gives it no path back into the caller.
    let s = summaryIn(ScalarTypes & """
      (proc :g.0 (params (param :p.0 . (ptr (i +32)))) . .
        (stmts (asgn (deref p.0) +1)))
      (proc :f.0 (params (param :obj.0 . Point.0)) . .
        (stmts (call g.0 obj.0)))""", "f.0")
    doAssert not s.params[0].writes

  block field_store_writes_obj_not_x:
    # `obj.field = x` writes obj, reads x. x is not itself written, but an
    # identity in a field cell joins the graphs (`x.obj = y` then `x.obj.a`
    # mutates y).
    let s = twoParams("(asgn (dot obj.0 fld.0) x.0)")
    doAssert not s.writesGlobal
    doAssert not s.readsGlobal
    doAssert s.params.len == 2
    doAssert s.params[0].writes
    doAssert s.params[0].reads
    doAssert not s.params[0].slotWritten
    doAssert s.params[1].reads
    doAssert not s.params[1].writes
    doAssert s.params[0].cls == s.params[1].cls
    doAssert int(s.resultCls) == 2

  block pointer_field_store_then_write_through:
    # `x.obj = y; x.obj.a = 3` writes y's graph (same class as x).
    let s = twoParams("""
      (asgn (dot obj.0 fld.0) x.0)
      (asgn (dot (deref (dot obj.0 fld.0)) a.0) 3)""")
    doAssert s.params[0].writes
    doAssert s.params[0].cls == s.params[1].cls

  block field_load_copies_into_slot:
    # `x = obj.field` reads obj, rebinds x's slot, does not write obj, does
    # not put them in the same class.
    let s = twoParams("(asgn x.0 (dot obj.0 fld.0))")
    doAssert not s.writesGlobal
    doAssert s.params[0].reads
    doAssert not s.params[0].writes
    doAssert s.params[1].slotWritten
    doAssert not s.params[1].writes or s.params[1].slotWritten
    doAssert s.params[0].cls != s.params[1].cls

  block field_load_into_local_is_readonly:
    let s = twoParams("""
      (var :tmp.0 . . . .)
      (asgn tmp.0 (dot obj.0 fld.0))""")
    doAssert not s.writesGlobal
    doAssert s.params[0].reads
    doAssert not s.params[0].writes
    doAssert not s.params[1].writes
    doAssert not s.params[1].slotWritten
    doAssert isReadOnly(s)

  block identity_copy_joins_classes:
    let s = twoParams("(asgn obj.0 x.0)")
    doAssert s.params[0].slotWritten
    doAssert s.params[1].reads
    doAssert s.params[0].cls == s.params[1].cls

  block result_field_store_is_not_global:
    let s = summaryIn(PairTypes & """
      (proc :f.0
        (params (param :x.0 . (ptr Cell.0)))
        . . (stmts
          (var :result.0 . Node.0 .)
          (asgn (dot result.0 fld.0) x.0)
          (ret result.0)))""", "f.0")
    doAssert not s.writesGlobal
    doAssert s.params[0].reads
    doAssert not s.params[0].writes
    doAssert int(s.resultCls) == 1

  block return_field_does_not_join_result:
    let s = twoParams("(ret (dot obj.0 fld.0))")
    doAssert s.params[0].reads
    doAssert not s.params[0].writes
    doAssert int(s.resultCls) == 2

  block return_identity_joins_result:
    let s = twoParams("(ret obj.0)")
    doAssert s.params[0].reads
    doAssert int(s.resultCls) == 0

  block return_call_does_not_join_args:
    # `return f(obj, x)` must not put obj and x in one class just because both
    # were arguments of a (possibly bool-returning) call.
    let s = twoParams("(ret (call g.0 obj.0 x.0))")
    doAssert s.params[0].cls != s.params[1].cls
    doAssert int(s.resultCls) == 2

  block forging_int_to_ptr_is_alias_unsafe:
    # `cast[ptr T](4000)` forges a pointer to untracked memory.
    let s = twoParams("(asgn obj.0 (cast (ptr (i +32)) 4000))")
    doAssert s.writesGlobal
    doAssert s.readsGlobal

  block cast_byte_is_not_unsafe:
    let s = twoParams("(asgn x.0 (cast (u +8) obj.0))")
    doAssert not s.writesGlobal
    doAssert not s.readsGlobal

  block cast_ptr_from_addr_is_not_unsafe:
    let s = twoParams("(asgn obj.0 (cast (ptr (i +32)) (addr x.0)))")
    doAssert not s.writesGlobal
    doAssert s.params[0].cls == s.params[1].cls

  block cast_ptr_pun_joins:
    let s = twoParams("(asgn obj.0 (cast (ptr (i +32)) x.0))")
    doAssert not s.writesGlobal
    doAssert s.params[0].cls == s.params[1].cls

  block cast_ptr_arith_is_not_unsafe:
    # `cast[ptr T](cast[uint](p) + n)` is pointer arithmetic, not a forge.
    let s = twoParams(
      "(asgn obj.0 (cast (ptr (i +32)) (add (u +64) (cast (u +64) x.0) 8)))")
    doAssert not s.writesGlobal

  echo "ok"

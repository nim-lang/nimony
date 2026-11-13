#
#
#           Leng alias facts
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## `lengaa`: the one place that answers "may I skip this copy, or could
## something observe the location I am about to reuse?" for Leng. The
## shoggoth optimizer, hexer's Leng passes and arkham all ask that question;
## they used to answer it each with their own scan.
##
## The answer rests on the prover's notion of an *owned* variable
## (doc/language.md, "Aliasing"), restated for Leng: a parameter or a local
## declared by a `(var …)` in the body, whose address is never taken (`(addr …)`
## with it at the root of its operand) and that no `(emit …)` names. A `var`
## parameter is a pointer in Leng, so what it points to is reached by `deref`
## and never owned. Nothing but a
## write that names an owned local can change it, and nothing but a read that
## names it can see it. `(haddr x)` does not expose `x`: it hands a callee the
## location for the duration of one call, and that call names `x`.
##
## So "can evaluating E observe D?" is: is D's root an owned local, and does E
## not name it? Two cheap facts, no alias classes. Everything else, writes
## through pointers, globals, parameters, is answered "yes, it may", and the
## caller keeps its copy. That is deliberate: those are not the cases worth
## optimizing, and a precise answer for them is what made each site grow its
## own analysis.
##
## The facts are recomputed per proc (`analyseLocals` is one walk) rather than
## carried as a mark in the IR: later passes keep creating addresses, and a mark
## would be an invariant every one of them has to maintain.
##
## Also here is the callee half of the hidden-result-pointer contract
## (`nrvoLocal`): the buffer a caller passes for a by-reference result never
## aliases anything the callee can reach, which is the C ABI's rule too.
## A caller that cannot promise that for its destination, as told by
## `destMayBeObserved`, passes a temporary instead.

import std / sets
import nifcore
import nifcdecl     # the tag decoders; not re-exported, so no clash with `leng_model`

type
  LocalFacts* = object
    ## The per-proc facts `isOwned` reads.
    locals: HashSet[SymId]    ## the parameters and the `(var …)`s of the body
    exposed: HashSet[SymId]   ## address taken or named by an `(emit …)`

const NoSym = SymId(0)

proc isTag(n: Cursor): bool {.inline.} = n.hasMore and n.kind == TagLit

proc lvalueRoot*(n: Cursor): SymId =
  ## The variable whose storage the lvalue `n` is part of: `x` for `x`,
  ## `(dot x f)`, `(at x i)` and conversions of those. `SymId(0)` when the
  ## location is reached through a pointer (`deref`, `pat`) or is no lvalue
  ## this recognizes; callers read that as "anything may observe it".
  result = NoSym
  var n = n
  while true:
    if not n.hasMore: return NoSym
    case n.kind
    of Symbol: return n.symId
    of TagLit:
      case n.exprKind
      of DotC, AtC, ParC:
        inc n                                   # the base is the first child
      of ConvC, CastC:
        inc n
        skip n                                  # the target type
      else: return NoSym
    else: return NoSym

proc fieldSlot(n: Cursor): int =
  ## The child index of `n` that names a FIELD, -1 when none. A field is a
  ## symbol too, and it can be spelled exactly like a variable (`x.0` for a
  ## field `x` and for the first local `x`), so a use count that walks into it
  ## counts a variable that is not there.
  if n.exprKind in {DotC, OffsetofC}: result = 1   # (dot obj fld depth), (offsetof T fld)
  elif n.substructureKind == KvU: result = 0       # (kv fld value depth)
  else: result = -1

proc countUses(n: Cursor; s: SymId; limit: int; count: var int) =
  if count >= limit or not n.hasMore: return
  case n.kind
  of Symbol:
    if n.symId == s: inc count
  of TagLit:
    let f = fieldSlot(n)
    var i = 0
    var it = n
    it.into:
      while it.hasMore:
        if i != f: countUses(it, s, limit, count)
        skip it
        inc i
  else: discard

proc countUses*(n: Cursor; s: SymId): int =
  ## How often the subtree `n` uses the variable `s`: reads and writes, not its
  ## declaration (`SymbolDef`) and not a field that happens to share its name.
  result = 0
  countUses(n, s, high(int), result)

proc mentions*(n: Cursor; s: SymId): bool =
  ## Does the subtree `n` use the variable `s` at all? See `countUses`.
  var count = 0
  countUses(n, s, 1, count)
  result = count > 0

proc exposeAll(f: var LocalFacts; n: Cursor) =
  ## Every symbol under `n` (an `(emit …)`) is exposed.
  if not n.hasMore: return
  case n.kind
  of Symbol: f.exposed.incl n.symId
  of TagLit:
    var it = n
    it.loopInto:
      exposeAll(f, it)
      skip it
  else: discard

proc walk(f: var LocalFacts; n: Cursor) =
  if not n.isTag: return
  case n.stmtKind
  of ProcS, TypeS: return                       # a different routine / no code
  of VarS:
    let name = n.childCursor
    if name.hasMore and name.kind == SymbolDef: f.locals.incl name.symId
  of EmitS:
    exposeAll(f, n)
    return
  else: discard
  if n.exprKind == AddrC:
    let r = lvalueRoot(n.childCursor)
    if r != NoSym: f.exposed.incl r
  var it = n
  it.loopInto:
    walk(f, it)
    skip it

proc analyseLocals*(procDecl: Cursor): LocalFacts =
  ## The owned-ness facts for one `(proc …)` (or any statement subtree).
  result = LocalFacts(locals: initHashSet[SymId](), exposed: initHashSet[SymId]())
  if procDecl.stmtKind == ProcS:
    var it = procDecl.childCursor
    skip it                                     # name
    if it.isTag:                                # (params (param :x …) …)
      var p = it
      p.loopInto:
        let nm = p.childCursor
        if nm.hasMore and nm.kind == SymbolDef: result.locals.incl nm.symId
        skip p
    while it.hasMore:
      walk(result, it)
      skip it
  else:
    walk(result, procDecl)

proc isOwned*(f: LocalFacts; s: SymId): bool {.inline.} =
  ## A parameter or local that only a read or write naming it can observe.
  s in f.locals and s notin f.exposed

proc addLocal*(f: var LocalFacts; s: SymId) {.inline.} =
  ## For a pass that introduces a local the facts were not computed with (an
  ## inliner splicing a callee's locals in under fresh names).
  f.locals.incl s

proc expose*(f: var LocalFacts; s: SymId) {.inline.} = f.exposed.incl s

iterator localSyms*(f: LocalFacts): SymId =
  for s in f.locals: yield s

iterator exposedSyms*(f: LocalFacts): SymId =
  for s in f.exposed: yield s

proc declares*(f: LocalFacts; s: SymId): bool {.inline.} =
  ## Is `s` a parameter or local the facts were computed with? A back end's
  ## own temporaries are not, and nothing but the back end can name those.
  s in f.locals

proc symMayBeObserved*(f: LocalFacts; s: SymId; e: Cursor): bool =
  ## `destMayBeObserved` for a destination that is the whole variable `s`.
  result = s == NoSym or not f.isOwned(s) or mentions(e, s)

proc destMayBeObserved*(f: LocalFacts; dest, e: Cursor): bool =
  ## Can evaluating `e` (typically a call) read or write the location `dest`
  ## other than through its own result? When it cannot, `dest` may serve as
  ## the buffer `e` builds its result in, which is what makes the copy from a
  ## temporary unnecessary.
  ##
  ## For an assignment. A declaration's own initializer needs no question: no
  ## pointer can reach storage before its declaration runs (one kept from a
  ## previous loop iteration is dangling).
  result = symMayBeObserved(f, lvalueRoot(dest), e)

# ── the returned local ────────────────────────────────────────────────────

proc scanRets(n: Cursor; sym: var SymId; ok: var bool) =
  if not ok or not n.isTag: return
  case n.stmtKind
  of ProcS: return
  of RetS:
    let v = n.childCursor
    if not v.hasMore or v.kind != Symbol: ok = false
    elif sym == NoSym: sym = v.symId
    elif sym != v.symId: ok = false
    return
  else: discard
  var it = n
  it.into:
    while it.hasMore and ok:
      scanRets(it, sym, ok)
      skip it
    while it.hasMore: skip it

proc returnedLocal*(body: Cursor): SymId =
  ## The symbol every `(ret X)` in `body` returns, when they all return the
  ## same bare symbol: nimsem's `result` after lowering. `SymId(0)` when there
  ## is no `ret` or one returns anything else (a literal, a call, nothing).
  ## Whether that symbol is a local or a parameter is the caller's question.
  var sym = NoSym
  var ok = true
  scanRets(body, sym, ok)
  result = if ok: sym else: NoSym

proc hasRaise(n: Cursor): bool =
  if not n.isTag: return false
  if n.stmtKind in {RaiseS, TryS, OnerrS}: return true
  result = false
  var it = n
  it.into:
    while it.hasMore and not result:
      if hasRaise(it): result = true
      skip it
    while it.hasMore: skip it

proc findUninitDecl(n: Cursor; s, typ: SymId; count: var int; ok: var bool) =
  ## Count the `(var :s …)` decls under `n`; `ok` stays true iff each has the
  ## named type `typ` and no initializer.
  if not n.isTag: return
  if n.stmtKind == VarS:
    var it = n.childCursor
    if it.hasMore and it.kind == SymbolDef and it.symId == s:
      inc count
      skip it                                   # name
      skip it                                   # pragmas
      if not it.hasMore or it.kind != Symbol or it.symId != typ: ok = false
      else:
        skip it                                 # type
        if it.hasMore and it.kind != DotToken: ok = false
      return
  var it = n
  it.loopInto:
    findUninitDecl(it, s, typ, count, ok)
    skip it

proc nrvoLocal*(procDecl: Cursor): SymId =
  ## The local a `(proc …)` may build its result in, directly in the caller's
  ## result buffer: every `(ret X)` returns it, it is declared once, without an
  ## initializer, with the proc's (named) return type, and no `raise` can leave
  ## the proc with the buffer half-written. `SymId(0)` otherwise.
  ##
  ## Sound only together with the caller half: a caller passes a buffer that
  ## nothing the callee reaches aliases (`destMayBeObserved`).
  if procDecl.stmtKind != ProcS: return NoSym
  var it = procDecl.childCursor
  skip it                                       # name
  skip it                                       # params
  if not it.hasMore or it.kind != Symbol: return NoSym
  let retTyp = it.symId
  skip it                                       # return type
  skip it                                       # pragmas
  if not it.isTag or it.stmtKind != StmtsS: return NoSym
  let body = it
  if hasRaise(body): return NoSym
  let r = returnedLocal(body)
  if r == NoSym: return NoSym
  var count = 0
  var ok = true
  findUninitDecl(body, r, retTyp, count, ok)
  result = if ok and count == 1: r else: NoSym

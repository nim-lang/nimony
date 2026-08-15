#
#
#        Lengc intra-procedural alias analysis
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Field-insensitive intra-procedural alias analysis for a single Leng proc
## body, over **nifcore** cursors. A copy joins may-alias classes when the
## transferred value can carry a pointer — including through a field or deref:
## after `x.obj = y` with `obj` a pointer, `x.obj.a = 3` mutates `y`, so `x`
## and `y` must be in the same class. Pointer-free copies (`obj.n = 3`,
## `outparam.x = intParam`) do not join: those write the dest object without
## making the source an alias of it. Two memory accesses may-alias iff the
## base symbols of their access paths land in the same class.
##
## This is the intra-procedural counterpart of the inter-procedural alias
## information carried by `(smry …)` function summaries; value-CSE uses both to
## invalidate only the cached loads a store or call can actually affect, instead
## of dropping the whole cache.
##
## **Type precision.** An assignment only needs to join the two sides when the
## value it transfers can actually carry a pointer: copying a machine-word scalar
## (`outparam.x = intParam`) creates no path between the two graphs. So `connect`
## consults the type navigator (`typenav.getType`) for the transferred value's
## type and skips the union when that type is pointer-free. Leng is nominal —
## nearly every declared type is a `Symbol` — so the navigator is what makes this
## test worth anything: it follows the symbol to the object body and walks the
## fields, rather than calling every named type pointer-bearing. When the
## navigator cannot recompute a type at all it answers with its `(err)`
## sentinel, and `transfersPointer` falls back to the expression's own shape.
## This is sound — we only ever skip when *provably* no pointer flows; anything
## still unresolved is treated as pointer-bearing, so we never miss an
## invalidation (under-merging would). Over-merging the rest only loses CSE.
##
## **`cast` vs `conv`.** `(conv)` is a typed conversion of the same value and
## is always unwrapped. `(cast)` is bit reinterpretation: pointer-to-pointer
## (and `addr` → ptr) still names the same graph, so those follow the inner
## like `conv`. A cast *to* a pointer-bearing type *from* a value that does
## not carry pointer identity (`cast[ptr T](4000)`, `cast[ptr T](intParam)`)
## forges a pointer to untracked memory. That sets `forged`: through-pointer
## stores in this body are treated as unknown (same as `writesGlobal` on the
## summary). Narrowing casts (`cast[byte](v)`) and ptr punning are not
## forging — marking every proc that uses `cast` would poison string/alloc.

import std / tables
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind, tag enums
import ".." / nifmodules                      # MainModule (type context)
import ".." / typenav                         # getType / navigateToObjectBody / scopes
import ".." / pointerbearing                  # the shared "can this hold a pointer?" test

type
  Aliasing* = object
    parent: Table[SymId, SymId]   ## union-find; a symbol absent here is its own root
    forged*: bool                 ## a pointer-forging `cast` appears in the body

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result


proc find*(a: var Aliasing; x: SymId): SymId =
  var r = x
  while true:
    let p = a.parent.getOrDefault(r, r)
    if p == r: break
    r = p
  var i = x                       # path compression
  while true:
    let p = a.parent.getOrDefault(i, i)
    if p == i or p == r: break
    a.parent[i] = r
    i = p
  result = r

proc union*(a: var Aliasing; x, y: SymId) =
  let rx = find(a, x)
  let ry = find(a, y)
  if rx != ry: a.parent[rx] = ry

proc mayAlias*(a: var Aliasing; x, y: SymId): bool {.inline.} =
  find(a, x) == find(a, y)

# ---- access roots ---------------------------------------------------------

proc accessRoots*(c: Cursor; roots: var seq[SymId]; m: ptr MainModule = nil) =
  ## The base symbols whose memory the value/location at `c` may be rooted at.
  ## Memory accesses contribute their base; calls/constructors the union of
  ## their operands; an index expression does NOT contribute (it selects within
  ## the base, not a separate object). A forging `cast` contributes nothing:
  ## its pointee is untracked, not the inner integer.
  case c.kind
  of Symbol, SymbolDef:
    roots.add symId(c)
  of TagLit:
    case c.exprKind
    of DotC, AtC, DerefC, PatC, AddrC, HaddrC:
      accessRoots(firstChild(c), roots, m)     # the base / addressed lvalue
    of ConvC:
      var r = c
      inc r
      skip r                                    # type
      accessRoots(r, roots, m)
    of CastC:
      var r = c
      inc r
      skip r                                    # type
      if not isForgingCast(m, c):
        accessRoots(r, roots, m)
    of BaseobjC:
      var r = c
      inc r
      skip r                                    # type
      skip r                                    # inheritance-depth intlit
      accessRoots(r, roots, m)
    of CallC:
      var r = c
      r.into:                                   # bounded to the call's children
        if r.hasMore: skip r                    # callee
        while r.hasMore:
          accessRoots(r, roots, m)
          skip r
    else:
      # constructors / aggregates / arithmetic: union of operand roots.
      var r = c
      r.loopInto:
        accessRoots(r, roots, m)
        skip r
  else:
    discard

# ---- build ----------------------------------------------------------------

proc noteForging(a: var Aliasing; m: ptr MainModule; n: Cursor) =
  if n.kind != TagLit: return
  if isForgingCast(m, n): a.forged = true
  var c = n
  c.loopInto:
    noteForging(a, m, c)
    skip c

proc connect(a: var Aliasing; m: ptr MainModule; destStart, srcStart: Cursor) =
  # Skip the union when the transferred value provably carries no pointer.
  # A pointer-bearing copy joins even through a field/deref: `x.obj = y`
  # (obj a pointer) makes `x.obj.a = 3` a write of `y`. Skipping those
  # dests was the value-copy rule, and it is unsound once a pointer flows.
  # A forging cast invents a pointer to untracked memory: do not pretend
  # the inner integer is the pointee.
  noteForging(a, m, destStart)
  noteForging(a, m, srcStart)
  if isForgingCast(m, srcStart):
    return
  if m != nil and not transfersPointer(m, srcStart):
    return
  var dr, sr: seq[SymId] = @[]
  accessRoots(destStart, dr, m)
  accessRoots(srcStart, sr, m)
  for x in dr:
    for y in sr: union(a, x, y)

proc walk(a: var Aliasing; m: ptr MainModule; n: var Cursor) =
  if not n.hasMore: return
  if n.kind != TagLit:
    inc n
    return
  case n.stmtKind
  of AsgnS:                        # (asgn dest src)
    var c = n
    var destStart, srcStart: Cursor
    c.into:
      destStart = c
      skip c
      srcStart = c
      while c.hasMore: skip c
    connect(a, m, destStart, srcStart)
    skip n
  of StoreS:                       # (store src dest) — reversed operands
    var c = n
    var srcStart, destStart: Cursor
    c.into:
      srcStart = c
      skip c
      destStart = c
      while c.hasMore: skip c
    connect(a, m, destStart, srcStart)
    skip n
  of VarS, GvarS, TvarS, ConstS:   # (var name pragmas type init)
    var c = n
    var nameStart: Cursor
    var typeStart = default(Cursor)
    var initStart = default(Cursor)
    var haveInit = false
    c.into:
      nameStart = c
      skip c                       # name
      if c.hasMore: skip c         # pragmas
      if c.hasMore:
        typeStart = c
        skip c                     # type
      if c.hasMore:
        initStart = c
        # An omitted initializer is a `.` in the value slot: `var x: T` binds no
        # value at all, so there is nothing to connect. Treating it as an
        # initializer used to hand `connect` a DotToken, whose type is the
        # `(err)` sentinel — a "may hold a pointer" verdict on a copy that does
        # not exist. It joined nothing (a `.` contributes no roots), but it was
        # the single largest source of conservative answers in the pass.
        haveInit = initStart.kind != DotToken
        while c.hasMore: skip c
    # Register the local's type so later `getType` can resolve uses of it
    # (mirrors how the C backend drives the typenav scopes).
    if m != nil and nameStart.kind == SymbolDef and not cursorIsNil(typeStart):
      m[].registerLocal(nameStart.symId, typeStart)
    if haveInit: connect(a, m, nameStart, initStart)
    skip n
  of ScopeS:                       # explicit lexical scope
    if m != nil: m[].openScope()
    n.loopInto:
      walk(a, m, n)
    if m != nil: m[].closeScope()
  else:
    if isForgingCast(m, n): a.forged = true
    n.loopInto:
      walk(a, m, n)

proc computeAliasing*(buf: var TokenBuf; m: ptr MainModule = nil): Aliasing =
  ## Build the alias partition for a proc body (flow-insensitive single pass).
  ## `m` is the module type context (with the proc's params already registered
  ## in the current scope by the caller); pass `nil` to fall back to the coarse,
  ## type-agnostic partition.
  result = Aliasing(parent: initTable[SymId, SymId](), forged: false)
  var n = beginRead(buf)
  walk(result, m, n)

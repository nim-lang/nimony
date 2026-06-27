#
#
#        Lengc intra-procedural alias analysis (Steensgaard)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Field-insensitive intra-procedural alias analysis for a single Leng proc
## body, over **nifcore** cursors. A flow-insensitive Steensgaard-style
## union-find partitions the body's symbols into *may-alias classes*: two
## symbols share a class when a chain of assignments may make them reach
## overlapping memory. Two memory accesses may-alias iff the base symbols of
## their access paths land in the same class.
##
## This is the intra-procedural counterpart of the inter-procedural alias
## information carried by `(smry …)` function summaries; value-CSE uses both to
## invalidate only the cached loads a store or call can actually affect, instead
## of dropping the whole cache.
##
## **Type precision.** An assignment only needs to unify the two sides when the
## value it transfers can actually carry a pointer: copying a machine-word scalar
## (`outparam.x = intParam`) creates no path between the two graphs. So `connect`
## consults the type navigator (`typenav.getType`) for the transferred value's
## type and skips the union when that type is pointer-free. This is sound — we
## only ever skip when *provably* no pointer flows; an unresolved/unknown type is
## treated conservatively as pointer-bearing, so we still never miss an
## invalidation (under-merging would). Over-merging the rest only loses CSE.

import std / [tables, sets]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind, tag enums
import ".." / nifmodules                      # MainModule (type context)
import ".." / typenav                         # getType / navigateToObjectBody / scopes

type
  Aliasing* = object
    parent: Table[SymId, SymId]   ## union-find; a symbol absent here is its own root

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

# ---- type-based pointer test ----------------------------------------------

proc pointerBearing(m: var MainModule; typ: Cursor; depth = 0): bool =
  ## True if a value of `typ` may hold a pointer, so copying it can create
  ## aliasing. Pointer-free scalars return false; an unresolved/unknown type
  ## returns true (conservative — never miss an alias).
  if depth > 12 or cursorIsNil(typ): return true
  case typ.typeKind
  of IT, UT, FT, CT, BoolT, EnumT, VoidT:
    result = false
  of PtrT, AptrT, ProctypeT, FlexarrayT:
    result = true
  of ArrayT:
    result = pointerBearing(m, firstChild(typ), depth+1)   # element type
  of ObjectT, UnionT:
    result = false
    var body = typ
    body.into:
      if typ.typeKind == ObjectT and body.kind == Symbol:   # base type
        if pointerBearing(m, body, depth+1): return true
        inc body
      while body.hasMore:
        if body.substructureKind == FldU:
          let fld = takeFieldDecl(body)
          if pointerBearing(m, fld.typ, depth+1): return true
        else:
          skip body
  of NoType:
    if typ.kind == Symbol:
      let nb = navigateToObjectBody(m, typ)
      result = nb.kind == Symbol or pointerBearing(m, nb, depth+1)  # unresolved -> true
    else:
      result = true
  else:
    result = true   # err / params / anything unexpected -> conservative

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

proc accessRoots*(c: Cursor; roots: var seq[SymId]) =
  ## The base symbols whose memory the value/location at `c` may be rooted at.
  ## Memory accesses contribute their base; calls/constructors the union of
  ## their operands; an index expression does NOT contribute (it selects within
  ## the base, not a separate object).
  case c.kind
  of Symbol, SymbolDef:
    roots.add symId(c)
  of TagLit:
    case c.exprKind
    of DotC, AtC, DerefC, PatC, AddrC:
      accessRoots(firstChild(c), roots)        # the base / addressed lvalue
    of ConvC, CastC:
      var r = c
      inc r
      skip r                                    # type
      accessRoots(r, roots)
    of BaseobjC:
      var r = c
      inc r
      skip r                                    # type
      skip r                                    # inheritance-depth intlit
      accessRoots(r, roots)
    of CallC:
      var r = c
      r.into:                                   # bounded to the call's children
        if r.hasMore: skip r                    # callee
        while r.hasMore:
          accessRoots(r, roots)
          skip r
    else:
      # constructors / aggregates / arithmetic: union of operand roots.
      var r = c
      r.loopInto:
        accessRoots(r, roots)
        skip r
  else:
    discard

# ---- build ----------------------------------------------------------------

proc connect(a: var Aliasing; m: ptr MainModule; destStart, srcStart: Cursor) =
  # Skip the union when the transferred value provably carries no pointer.
  if m != nil and not pointerBearing(m[], getType(m[], srcStart)):
    return
  var dr, sr: seq[SymId] = @[]
  accessRoots(destStart, dr)
  accessRoots(srcStart, sr)
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
        haveInit = true
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
    n.loopInto:
      walk(a, m, n)

proc computeAliasing*(buf: var TokenBuf; m: ptr MainModule = nil): Aliasing =
  ## Build the alias partition for a proc body (flow-insensitive single pass).
  ## `m` is the module type context (with the proc's params already registered
  ## in the current scope by the caller); pass `nil` to fall back to the coarse,
  ## type-agnostic partition.
  result = Aliasing(parent: initTable[SymId, SymId]())
  var n = beginRead(buf)
  walk(result, m, n)

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
## It is deliberately a coarse over-approximation: any assignment unifies the
## graphs of its two sides (no type filtering yet), so scalars assigned from a
## struct field join that struct's class. Over-merging only ever loses CSE
## opportunities; it never misses an invalidation.

import std / [tables, sets]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind, tag enums

type
  Aliasing* = object
    parent: Table[SymId, SymId]   ## union-find; a symbol absent here is its own root

proc symId(c: Cursor): SymId {.inline.} =
  c.pool.syms.getOrIncl(symName(c))

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

proc connect(a: var Aliasing; destStart, srcStart: Cursor) =
  var dr, sr: seq[SymId] = @[]
  accessRoots(destStart, dr)
  accessRoots(srcStart, sr)
  for x in dr:
    for y in sr: union(a, x, y)

proc walk(a: var Aliasing; n: var Cursor) =
  if not n.hasMore: return
  if n.kind != TagLit:
    inc n
    return
  case n.stmtKind
  of AsgnS:                        # (asgn dest src)
    var m = n
    var destStart, srcStart: Cursor
    m.into:
      destStart = m
      skip m
      srcStart = m
      while m.hasMore: skip m
    connect(a, destStart, srcStart)
    skip n
  of StoreS:                       # (store src dest) — reversed operands
    var m = n
    var srcStart, destStart: Cursor
    m.into:
      srcStart = m
      skip m
      destStart = m
      while m.hasMore: skip m
    connect(a, destStart, srcStart)
    skip n
  of VarS, GvarS, TvarS, ConstS:   # (var name pragmas type init)
    var m = n
    var nameStart: Cursor
    var initStart = default(Cursor)
    var haveInit = false
    m.into:
      nameStart = m
      skip m                       # name
      if m.hasMore: skip m         # pragmas
      if m.hasMore: skip m         # type
      if m.hasMore:
        initStart = m
        haveInit = true
        while m.hasMore: skip m
    if haveInit: connect(a, nameStart, initStart)
    skip n
  else:
    n.loopInto:
      walk(a, n)

proc computeAliasing*(buf: var TokenBuf): Aliasing =
  ## Build the alias partition for a proc body (flow-insensitive single pass).
  result = Aliasing(parent: initTable[SymId, SymId]())
  var n = beginRead(buf)
  walk(result, n)

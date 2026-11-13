#
#
#           Nimony Final IR: the versions of a routine's variables
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Which version every variable of a routine has while `finalir.nim` walks its
## body (doc/internals/multi_join_ir.md, "Versions").
##
## A `Versions` knows the current version of every variable and keeps a log of
## the changes. A `Mark` is a point of the walk; what the versions were at a
## mark is read back from the log, so a `jmp` and the end of a branch only
## have to remember a `Mark`, and a join asks for the versions at its marks.

import std / [tables, assertions]
include ".." / lib / nifprelude

type
  Mark* = distinct int32 ## a point of the walk over a routine's body

  Change = object
    sym: SymId
    before: int32 ## the version the change replaced

  Variable = object
    current: int32    ## 0: not defined yet
    handedOut: int32  ## the highest version a definition got

  Versions* = object
    vars: Table[SymId, Variable]
    log: seq[Change] ## every change of a current version, in order

  Sources* = object ## what a join of one variable merges
    sym*: SymId
    olds*: seq[int] ## its version at each of the marks asked for

const
  NoMark* = Mark(-1)

proc `==`*(a, b: Mark): bool {.borrow.}
proc `<`*(a, b: Mark): bool {.borrow.}

proc mark*(v: Versions): Mark {.inline.} =
  ## The point the walk is at.
  Mark(v.log.len)

proc current*(v: Versions; s: SymId): int =
  int v.vars.getOrDefault(s).current

proc change(v: var Versions; s: SymId; val: int32) =
  v.log.add Change(sym: s, before: v.vars.getOrDefault(s).current)
  v.vars.mgetOrPut(s, Variable()).current = val

proc define*(v: var Versions; s: SymId): int =
  ## A definition of `s`: it gets the next version that was never used.
  let n = v.vars.getOrDefault(s).handedOut + 1'i32
  v.vars.mgetOrPut(s, Variable()).handedOut = n
  change v, s, n
  result = int n

proc changedBetween*(v: Versions; a, b: Mark): seq[SymId] =
  ## The variables whose version changed between the marks `a` and `b`, in
  ## the order they first did.
  result = @[]
  for i in int32(a) ..< int32(b):
    let s = v.log[i].sym
    if s notin result: result.add s

proc changedSince*(v: Versions; m: Mark): seq[SymId] =
  changedBetween(v, m, mark(v))

proc find(x: seq[Sources]; s: SymId): int =
  result = -1
  for i in 0 ..< x.len:
    if x[i].sym == s: return i

proc sourcesAt*(v: Versions; since: Mark; marks: openArray[Mark]): seq[Sources] =
  ## For every variable whose version changed since `since`, in the order it
  ## first did: the version it had at each of `marks`, none of which lies
  ## before `since`.
  result = @[]
  var state: seq[int] = @[] # the versions at `pos`, walking the log backwards
  for i in int32(since) ..< int32(v.log.len):
    let s = v.log[i].sym
    if find(result, s) < 0:
      result.add Sources(sym: s, olds: newSeq[int](marks.len))
      state.add current(v, s)
  # the marks from the latest to the earliest:
  var order: seq[int] = @[]
  for k in 0 ..< marks.len:
    assert not (marks[k] < since)
    var at = order.len
    order.add k
    while at > 0 and marks[order[at-1]] < marks[k]:
      order[at] = order[at-1]
      dec at
    order[at] = k
  var pos = v.log.len
  for k in order:
    while pos > int(int32(marks[k])):
      dec pos
      state[find(result, v.log[pos].sym)] = int v.log[pos].before
    for i in 0 ..< result.len:
      result[i].olds[k] = state[i]

proc restore*(v: var Versions; m: Mark) =
  ## Entering another branch: every variable is back at the version it had at
  ## `m`. Nothing is forgotten: the marks taken so far stay valid.
  let changed = sourcesAt(v, m, [m])
  for c in changed:
    if c.olds[0] != current(v, c.sym): change v, c.sym, int32(c.olds[0])

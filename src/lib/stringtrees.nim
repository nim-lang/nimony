#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Helper logic for an efficient compile-time string to value/action mapping.

import std/assertions

# We split the set of strings into 2 sets of roughly the same size.
# The condition used for splitting is a (position, char) tuple.
# Every string of length > position for which s[position] <= char is in one
# set else it is in the other set.

type
  Key* = (string, string) # 2nd component is the enum field name as a string

func addUnique*[T](s: var seq[T]; x: T) =
  for y in items(s):
    if y == x: return
  s.add x

proc splitValue(a: openArray[Key]; position: int): (char, float) =
  var cand: seq[char] = @[]
  for t in items a:
    if t[0].len > position: cand.addUnique t[0][position]

  result = ('\0', -1.0)
  for disc in items cand:
    var hits = 0
    for t in items a:
      if t[0].len > position and t[0][position] <= disc:
        inc hits
    # the split is the better, the more `hits` is close to `a.len / 2`:
    let grade = 100000.0 - abs(hits.float - a.len.float / 2.0)
    if grade > result[1]:
      result = (disc, grade)

proc tryAllPositions(a: openArray[Key]): (char, int) =
  var m = 0
  for t in items a:
    m = max(m, t[0].len)

  result = ('\0', -1)
  var best = -1.0
  for i in 0 ..< m:
    let current = splitValue(a, i)
    if current[1] > best:
      best = current[1]
      result = (current[0], i)

type
  SearchKind* = enum
    LinearSearch, ForkedSearch
  SearchNode* = object
    case kind*: SearchKind
    of LinearSearch:
      choices*: seq[Key]
    of ForkedSearch:
      span: int
      best: (char, int)

  ForkedResult* = object
    thenA*: int
    elseA*: int
    best*: (char, int)

proc forked*(s: seq[SearchNode]; i: int): ForkedResult =
  let thenA = i+1
  let elseA = thenA + (if s[thenA].kind == LinearSearch: 1 else: s[thenA].span)
  result = ForkedResult(thenA: thenA, elseA: elseA, best: s[i].best)

proc emitLinearSearch(a: openArray[Key]; dest: var seq[SearchNode]) =
  var d = SearchNode(kind: LinearSearch, choices: @[])
  for x in a: d.choices.add x
  dest.add d

proc splitImpl(a: openArray[Key]; dest: var seq[SearchNode]) =
  if a.len <= 2:
    emitLinearSearch a, dest
  else:
    let best = tryAllPositions(a)
    var groupA: seq[Key] = @[]
    var groupB: seq[Key] = @[]
    for t in items a:
      if t[0].len > best[1] and t[0][best[1]] <= best[0]:
        groupA.add t
      else:
        groupB.add t
    if groupA.len == 0 or groupB.len == 0:
      emitLinearSearch a, dest
    else:
      let toPatch = dest.len
      dest.add SearchNode(kind: ForkedSearch, span: 1, best: best)
      splitImpl groupA, dest
      splitImpl groupB, dest
      let dist = dest.len - toPatch
      assert dist > 0
      dest[toPatch].span = dist

proc createSearchTree*(a: openArray[Key]): seq[SearchNode] =
  result = @[]
  splitImpl a, result

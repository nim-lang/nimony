#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / assertions
include ".." / lib / nifprelude

import ".." / models / [tags, njvl_tags]
export njvl_tags

template tagEnum(c: Cursor): TagEnum = cast[TagEnum](tag(c))

template tagEnum(c: PackedToken): TagEnum = cast[TagEnum](tag(c))

proc njvlKind*(c: PackedToken): NjvlKind {.inline.} =
  if c.kind == ParLe and rawTagIsNjvlKind(tagEnum(c)):
    result = cast[NjvlKind](tagEnum(c))
  else:
    result = NoVTag

proc njvlKind*(c: Cursor): NjvlKind {.inline.} =
  result = njvlKind(c.load())

proc parLeToken*(kind: NjvlKind; info = NoLineInfo): PackedToken =
  parLeToken(cast[TagId](kind), info)

template copyIntoKind*(dest: var TokenBuf; kind: NjvlKind;
                       info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(kind, info)
  body
  dest.addParRi()

type
  IteTracker*[T] = object
    ## Seq-backed tracker for ite-branch traversal.
    ## Both branches inherit the outer state (items are readable across checkpoints),
    ## but changes in one branch don't survive to the other: use `rollback` to reset
    ## to the saved checkpoint before entering the next branch and after the merge.
    data: seq[T]

proc checkpoint*[T](t: IteTracker[T]): int {.inline.} =
  ## Save current length; pass to `rollback` to undo additions since this point.
  t.data.len

proc rollback*[T](t: var IteTracker[T]; cp: int) {.inline.} =
  ## Discard everything added since `cp`.
  t.data.shrink cp

proc add*[T](t: var IteTracker[T]; item: sink T) {.inline.} =
  t.data.add item

proc contains*[T](t: IteTracker[T]; item: T): bool {.inline.} =
  item in t.data

iterator since*[T](t: IteTracker[T]; cp: int): lent T =
  ## Iterate over items added since checkpoint `cp`.
  for i in cp ..< t.data.len:
    yield t.data[i]

type
  SplitPoint*[T] = object
    ## Returned by `split`; passed to `thenDone` and `join`.
    cp: int
    thenData: seq[T]

proc split*[T](t: IteTracker[T]): SplitPoint[T] {.inline.} =
  ## Begin an ite traversal. Snapshot the current position.
  SplitPoint[T](cp: t.checkpoint())

proc thenDone*[T](t: var IteTracker[T]; s: var SplitPoint[T]) =
  ## Call after the then-branch: capture its items and roll the tracker back so
  ## the else-branch starts from the same state as the then-branch did.
  for item in t.since(s.cp): s.thenData.add item
  t.rollback(s.cp)

proc join*[T](t: var IteTracker[T]; s: SplitPoint[T]; thenNoReturn, elseNoReturn: bool) =
  ## Call after the else-branch: merge both branches into the tracker.
  ## If thenNoReturn, keep only else items (then never reaches the join point).
  ## If elseNoReturn, keep only then items. Otherwise keep the intersection.
  var elseData: seq[T] = @[]
  for item in t.since(s.cp): elseData.add item
  t.rollback(s.cp)
  if thenNoReturn:
    for item in elseData: t.add item
  elif elseNoReturn:
    for item in s.thenData: t.add item
  else:
    for item in s.thenData:
      if item in elseData: t.add item

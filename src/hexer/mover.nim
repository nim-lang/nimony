#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Move analyser.
import std / [assertions]

include nifprelude
import ".." / nimony / [nimony_model, decls, controlflow, programs]

proc prepare(buf: var TokenBuf): seq[PackedLineInfo] =
  result = newSeq[PackedLineInfo](buf.len)
  for i in 0..<buf.len:
    result[i] = buf[i].info
    buf[i].info = toPayload(i.uint32)

proc restore(buf: var TokenBuf; infos: seq[PackedLineInfo]) =
  for i in 0..<buf.len:
    buf[i].info = infos[i]

proc isMarked(n: Cursor): bool {.inline.} =
  result = n.info == NoLineInfo

proc doMark(n: Cursor) {.inline.} =
  n.setInfo(NoLineInfo)

proc testOrSetMark(n: Cursor): bool {.inline.} =
  if isMarked(n):
    result = true
  else:
    doMark(n)
    result = false

proc containsUsage(tree: var Cursor; x: Cursor): bool =
  result = false # TODO
  skip tree

proc containsRoot(tree: var Cursor; x: Cursor): bool =
  result = false # TODO
  skip tree

proc findStart(c: TokenBuf; idx: PackedLineInfo; n: var Cursor): bool =
  for i in 0..<c.len:
    if c[i].info == idx:
      n = c.readonlyCursorAt(i)
      return true
  return false

proc singlePath(pc, x: Cursor; pcs: var seq[Cursor]; otherUsage: var PackedLineInfo): bool =
  var nested = 0
  var pc = pc
  while true:
    case pc.kind
    of GotoInstr:
      let diff = pc.getInt28
      if diff < 0:
        # jump backwards:
        let back = pc +! diff
        if not testOrSetMark(back):
          pc = back
        else:
          # finished traversing this path:
          break
      else:
        # ordinary goto, simply follow it:
        pc = pc +! diff
    of ParRi:
      if nested == 0:
        raiseAssert "BUG: unpaired ')'"
      dec nested
    of Symbol:
      if x.kind == Symbol and pc.symId == x.symId:
        otherUsage = pc.info
        return false
      inc pc
    of SymbolDef, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
      inc pc
    of ParLe:
      if pc.cfKind == IteF:
        inc pc
        if containsUsage(pc, x):
          otherUsage = pc.info
          return false
        # now 2 goto instructions follow:
        let a = pc +! pc.getInt28
        inc pc
        let b = pc +! pc.getInt28
        # we follow the second goto and remember the first one:
        if not isMarked(a):
          pcs.add a
        pc = b
      elif pc.stmtKind == AsgnS:
        inc pc
        if sameTrees(pc, x):
          # the path leads to a redefinition of 's' --> sink 's'.
          break
        skip pc # skip left-hand-side
        # right-hand-side is a simple use expression:
        if containsUsage(pc, x):
          # only partially writes to 's' --> can't sink 's', so this def reads 's'
          # or maybe writes to 's' --> can't sink 's'
          otherUsage = pc.info
          return false
        skipParRi pc
      elif pc.stmtKind == RetS:
        break
      else:
        if containsRoot(pc, x):
          otherUsage = pc.info
          return false
  return true

proc isLastReadImpl(c: TokenBuf; idx: uint32; otherUsage: var PackedLineInfo): bool =
  var n = default Cursor
  otherUsage = NoLineInfo
  if not findStart(c, toPayload(idx), n):
    return true
  # we are interested in what comes after this node:
  let x = n
  skip n
  var pcs = @[n]
  while pcs.len > 0:
    var pc = pcs.pop()
    if not isMarked(pc):
      if not singlePath(pc, x, pcs, otherUsage):
        return false
      doMark pc
  return true

proc isLastUse*(n: Cursor; buf: var TokenBuf; otherUsage: var PackedLineInfo): bool =
  # XXX Todo: only transform&traverse the innermost scope the variable was declared in.
  let oldInfos = prepare(buf)
  let idx = cursorToPosition(buf, n)
  assert idx >= 0
  var cf = toControlflow(n)
  result = isLastReadImpl(cf, idx.uint32, otherUsage)
  if otherUsage.isPayload:
    otherUsage = oldInfos[otherUsage.getPayload]
  restore(buf, oldInfos)

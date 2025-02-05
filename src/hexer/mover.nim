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

const
  PayloadOffset = 1'u32 # so that we don't use 0 as a payload

proc prepare(buf: var TokenBuf): seq[PackedLineInfo] =
  result = newSeq[PackedLineInfo](buf.len)
  for i in 0..<buf.len:
    result[i] = buf[i].info
    buf[i].info = toPayload(i.uint32 + PayloadOffset)

proc restore(buf: var TokenBuf; infos: seq[PackedLineInfo]) =
  for i in 0..<buf.len:
    buf[i].info = infos[i]

proc isMarked(n: Cursor): bool {.inline.} =
  result = n.info == toPayload(0'u32)

proc doMark(n: Cursor) {.inline.} =
  n.setInfo(toPayload(0'u32))

proc testOrSetMark(n: Cursor): bool {.inline.} =
  if isMarked(n):
    result = true
  else:
    doMark(n)
    result = false

proc rootOf(n: Cursor): SymId =
  var n = n
  while true:
    case n.exprKind
    of DotX, TupAtX, AtX, ArrAtX, DerefX, AddrX, HderefX, HaddrX:
      inc n
    of ConvKinds:
      inc n
      skip n # type part
    else:
      break
  if n.kind == Symbol:
    result = n.symId
  else:
    result = NoSymId

proc containsUsage(tree: var Cursor; x: Cursor): bool =
  result = false
  var nested = 0
  while true:
    if sameTrees(tree, x):
      result = true
    case tree.kind
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else:
      discard
    inc tree
    if nested == 0: break

proc containsRoot(tree: var Cursor; x: Cursor): bool =
  let r = rootOf(x)
  # scan loop also correct for `r == NoSymId`:
  var nested = 0
  result = false
  while true:
    case tree.kind
    of Symbol:
      if tree.symId == r:
        # MUST continue here as we must `skip tree` properly
        result = true
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else:
      discard
    inc tree
    if nested == 0: break

proc findStart(c: TokenBuf; idx: PackedLineInfo; n: var Cursor): bool =
  for i in 0..<c.len:
    if c[i].info == idx:
      n = c.readonlyCursorAt(i)
      return true
  return false

proc singlePath(pc, x: Cursor; pcs: var seq[Cursor]; otherUsage: var Cursor): bool =
  var nested = 0
  var pc = pc
  let root = rootOf(x)
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
      inc pc
    of Symbol:
      if x.kind == Symbol and pc.symId == x.symId:
        otherUsage = pc
        return false
      inc pc
    of SymbolDef:
      if root != NoSymId and pc.symId == root:
        # found the definition, so it gets a new value:
        break
      inc pc
    of EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
      inc pc
    of ParLe:
      if pc.cfKind == IteF:
        inc pc
        if containsUsage(pc, x):
          otherUsage = pc
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
        if (pc.kind == Symbol and pc.symId == root) or sameTrees(pc, x):
          # the path leads to a redefinition of 's' --> sink 's'.
          break
        skip pc # skip left-hand-side
        # right-hand-side is a simple use expression:
        if containsUsage(pc, x):
          # only partially writes to 's' --> can't sink 's', so this def reads 's'
          # or maybe writes to 's' --> can't sink 's'
          otherUsage = pc
          return false
        skipParRi pc
      elif pc.stmtKind == RetS:
        break
      else:
        if containsRoot(pc, x):
          otherUsage = pc
          return false
  return true

proc isLastReadImpl(c: TokenBuf; idx: uint32; otherUsage: var Cursor): bool =
  var n = default Cursor
  if not findStart(c, toPayload(idx + PayloadOffset), n):
    return true
  # we are interested in what comes after this node:
  let x = n
  skip n
  while n.kind == ParRi: inc n
  var pcs = @[n]
  while pcs.len > 0:
    let pc = pcs.pop()
    #echo "Looking at: ", toString(pc, false)
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
  var cf = toControlflow(beginRead buf)
  freeze cf
  #echo "CF IS ", codeListing(cf)
  var other = default Cursor
  result = isLastReadImpl(cf, idx.uint32, other)
  endRead buf
  restore(buf, oldInfos)
  if other.cursorIsNil:
    otherUsage = NoLineInfo
  else:
    otherUsage = other.info

when isMainModule:
  proc findX(n: Cursor): Cursor =
    result = n
    var nested = 0
    while true:
      case result.kind
      of ParLe:
        if result.exprKind == EnsureMoveX:
          inc result
          return result
        inc nested
      of ParRi:
        dec nested
      else:
        discard
      if nested == 0: break
      inc result
    raiseAssert "BUG: no 'ensureMove' found"

  proc test(s: string; expected: bool) =
    var input = parse(s)
    var otherUsage = NoLineInfo
    let n = findX(beginRead(input))
    let res = isLastUse(n, input, otherUsage)
    if res != expected:
      echo "FAILED Test case: ", s

  const BasicTest1 = """(stmts
  (let :my.var . . (array (i +8) +6) .)
  (var :i.0 . . (i -1) +0)
  (asgn (arrat my.var i.0) +56)

  (discard (emove my.var))
  (call use my.var)

  )
  """

  const BasicTest2 = """(stmts
  (let :my.var . . (array (i +8) +6) .)
  (var :i.0 . . (i -1) +0)
  (asgn (arrat my.var i.0) +56)

  (discard (emove my.var))

  )
  """

  test BasicTest1, false
  test BasicTest2, true

  const LoopTest = """(stmts
    (var :my.var . . (array (i +8) +6) .)
    (while (true)
      (discard (emove my.var))
    )

  )"""
  test LoopTest, false

  const LoopTestB = """(stmts
    (while (true) (stmts
      (var :my.var . . (array (i +8) +6) .)
      (discard (emove my.var))
    ))

  )"""
  test LoopTestB, true

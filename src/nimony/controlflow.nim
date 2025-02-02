#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Helper to translate control flow into `goto` based code
## which can be easier to analyze, depending on the used algorithm.

import std/[assertions, intsets]
include nifprelude

import nimony_model, sembasics

const
  GotoInstr = InlineInt

type
  Label = distinct int
  ControlFlow = object
    dest: TokenBuf

proc codeListing(c: TokenBuf, start = 0; last = -1): string =
  # for debugging purposes
  # first iteration: compute all necessary labels:
  var jumpTargets = initIntSet()
  let last = if last < 0: c.len-1 else: min(last, c.len-1)
  for i in start..last:
    if c[i].kind == GotoInstr:
      jumpTargets.incl(i+c[i].getInt32)
  # second iteration: generate string representation:
  var i = start
  var b = nifbuilder.open(1000)
  while i <= last:
    if i in jumpTargets:
      b.addTree "lab"
      b.addSymbolDef("L" & $i)
      b.endTree()
    case c[i].kind
    of GotoInstr:
      b.addTree "goto"
      b.addIdent "L" & $(i+c[i].getInt32())
      b.endTree()
    of Symbol:
      b.addSymbol pool.syms[c[i].symId]
    of SymbolDef:
      b.addSymbolDef pool.syms[c[i].symId]
    of EofToken:
      b.addRaw "\n<unexptected EOF>\n"
    of DotToken: b.addEmpty
    of Ident: b.addIdent pool.strings[c[i].litId]
    of StringLit: b.addStrLit pool.strings[c[i].litId]
    of CharLit: b.addCharLit c[i].charLit
    of IntLit: b.addIntLit pool.integers[c[i].intId]
    of UIntLit: b.addUIntLit pool.uintegers[c[i].uintId]
    of FloatLit: b.addFloatLit pool.floats[c[i].floatId]
    of ParLe: b.addTree pool.tags[c[i].tagId]
    of ParRi: b.endTree()
    inc i
  if i in jumpTargets: b.addRaw("L" & $i & ": End\n")
  result = b.extract()

proc genLabel(c: ControlFlow): Label = Label(c.dest.len)

proc jmpBack(c: var ControlFlow, p: Label; info: PackedLineInfo) =
  c.dest.add int32Token(p.int32 - c.dest.len.int32, info)

proc jmpForw(c: var ControlFlow; info: PackedLineInfo): Label =
  result = Label(c.dest.len)
  c.dest.add int32Token(0, info) # destination will be patched later

proc patch(c: var ControlFlow; p: Label) =
  # patch with current index
  c.dest[p.int].patchInt32Token int32(c.dest.len - p.int)

proc tr(c: var ControlFlow; n: var Cursor)

proc trAnd(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n
  c.dest.addParLe(IfS, info)
  tr c, n
  # (if (first condition) (goto L1) (goto L1))
  # (lab :L1) (second condition) (goto End))
  # (lab :L2) (false)
  # (lab :End)
  let l1 = c.jmpForw(info)
  let l2 = c.jmpForw(info)
  c.dest.addParRi()
  c.patch l1
  tr c, n
  let lend = c.jmpForw(info)
  c.patch l2
  c.dest.addParLe(FalseX, info)
  c.dest.addParRi()
  skipParRi n
  c.patch lend

proc trOr(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n
  c.dest.addParLe(IfS, info)
  tr c, n
  # (if (first condition) (goto L1) (goto L1))
  # (lab :L1) (true) (goto End))
  # (lab :L2) (second condition)
  # (lab :End)
  let l1 = c.jmpForw(info)
  let l2 = c.jmpForw(info)
  c.dest.addParRi()
  c.patch l1
  c.dest.addParLe(TrueX, info)
  c.dest.addParRi()
  let lend = c.jmpForw(info)
  c.patch l2
  tr c, n
  skipParRi n
  c.patch lend

proc trWhile(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n

  let loopStart = c.genLabel()

  # Generate if with goto
  c.dest.addParLe(IfS, info)
  tr(c, n) # transform condition
  let loopBody = c.jmpForw(info)
  let afterLoop = c.jmpForw(info)
  c.dest.addParRi()

  # Generate loop body
  c.patch loopBody
  tr(c, n) # transform body
  c.jmpBack(loopStart, info)

  c.patch afterLoop
  skipParRi n

proc trIf(c: var ControlFlow; n: var Cursor) =
  var endings: seq[Label] = @[]
  inc n # if
  while true:
    let info = n.info
    let k = n.substructureKind
    if k == ElifS:
      inc n
      c.dest.addParLe(IfS, info)
      tr c, n # condition
      let thenSection = c.jmpForw(info)
      let elseSection = c.jmpForw(info)
      c.dest.addParRi()
      c.patch thenSection
      tr c, n # action
      c.patch elseSection
      skipParRi n
    elif k == ElseS:
      inc n
      tr c, n
      skipParRi n
    else:
      break
  skipParRi n
  for i in countdown(endings.high, 0):
    c.patch(endings[i])

proc tr(c: var ControlFlow; n: var Cursor) =
  case n.kind
  of Symbol, SymbolDef, IntLit, UIntLit, FloatLit, StringLit, CharLit,
     Ident, DotToken, EofToken, UnknownToken:
    c.dest.add n
    inc n
  of ParRi:
    raiseAssert "unreachable"
  of ParLe:
    case n.stmtKind
    of IfS:
      trIf(c, n)
    of WhileS:
      trWhile(c, n)
    of BreakS, ForS, ContinueS, RetS, RaiseS:
      raiseAssert "not implemented"
    else:
      case n.exprKind
      of AndX:
        trAnd(c, n)
      of OrX:
        trOr(c, n)
      else:
        # Replace copyTree with recursive transformation
        c.dest.add n
        inc n
        while n.kind != ParRi:
          tr(c, n)
        c.dest.addParRi()
        inc n

proc toControlflow*(n: Cursor): TokenBuf =
  var c = ControlFlow()
  assert n.stmtKind == StmtsS
  var n = n
  c.dest.add n
  inc n
  while n.kind != ParRi:
    tr c, n
  c.dest.addParRi()
  result = ensureMove c.dest

when isMainModule:
  proc test(s: string) =
    var input = parse(s)
    var cf = toControlflow(beginRead(input))
    echo codeListing(cf)

  test """(stmts
(if (elif (eq +1 +1) (call echo "true")))

(if
  (elif (eq +1 +1) (call echo "true"))
  (elif (and (eq +2 +3) (eq +4 +5)) (call echo "elif"))
  (else (call echo "false"))
)

(while (eq +1 +1) (call echo "while"))

(while (or (eq +1 +1) (eq +4 +5)) (call echo "while 2"))
)

"""

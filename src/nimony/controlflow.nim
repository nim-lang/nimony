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
  TempVar = distinct int
  ControlFlow = object
    dest: TokenBuf
    stmtBegin: int
    nextVar: int

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

proc trExpr(c: var ControlFlow; n: var Cursor)
proc trStmt(c: var ControlFlow; n: var Cursor)

type
  FixupList = seq[Label]

proc trCondOp2(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  # Handles the `b` part of `a and b` or `a or b`. Simply translates it
  # to `(ite b tjmp fjmp)`.
  let info = n.info
  c.dest.addParLe(IteF, info)
  trExpr c, n # second condition
  tjmp.add c.jmpForw(info)
  fjmp.add c.jmpForw(info)
  c.dest.addParRi()
  skipParRi n

proc trAnd(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  # (ite (first-condition) L1 fjmp)
  # (lab :L1) (ite second-condition tjmp fjmp)
  let info = n.info
  c.dest.addParLe(IteF, info)
  inc n
  trExpr c, n # first-condition
  let l1 = c.jmpForw(info)
  fjmp.add c.jmpForw(info)
  c.dest.addParRi()
  c.patch l1
  trCondOp2 c, n, tjmp, fjmp

proc trOr(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  # (ite (first-condition) tjmp L1)
  # (lab :L1) (ite second-condition tjmp fjmp)
  let info = n.info
  c.dest.addParLe(IteF, info)
  inc n
  trExpr c, n # first-condition
  tjmp.add c.jmpForw(info)
  let l1 = c.jmpForw(info)
  c.dest.addParRi()
  c.patch l1
  trCondOp2 c, n, tjmp, fjmp

proc trIte(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  case n.exprKind
  of AndX:
    trAnd(c, n, tjmp, fjmp)
  of OrX:
    trOr(c, n, tjmp, fjmp)
  of NotX:
    # reverse the jump targets:
    trIte c, n, fjmp, tjmp
  of ParX:
    inc n
    trIte c, n, tjmp, fjmp
    skipParRi n
  else:
    # cannot exploit a special case here:
    let info = n.info
    c.dest.addParLe(IteF, info)
    trExpr c, n
    tjmp.add c.jmpForw(info)
    fjmp.add c.jmpForw(info)
    c.dest.addParRi()

proc defineTemp(c: var ControlFlow; tmp: TempVar; info: PackedLineInfo) =
  c.dest.addSymDef pool.syms.getOrIncl("`cf." & $int(tmp)), info

proc useTemp(c: var ControlFlow; tmp: TempVar; info: PackedLineInfo) =
  c.dest.copyIntoSymUse pool.syms.getOrIncl("`cf." & $int(tmp)), info

proc declareBool(c: var ControlFlow; info: PackedLineInfo): TempVar =
  result = TempVar(c.nextVar)
  inc c.nextVar
  c.dest.addParLe VarS, info
  c.defineTemp result, info
  c.dest.addEmpty2 info # no export marker, no pragmas
  c.dest.addParPair(BoolT, info)
  c.dest.addDotToken() # no value
  c.dest.addParRi()

proc rollbackToStmtBegin(c: var ControlFlow): TokenBuf =
  result = createTokenBuf(40)
  for i in c.stmtBegin ..< c.dest.len:
    result.add c.dest[i]
  c.dest.shrink c.stmtBegin

proc trStandaloneAndOr(c: var ControlFlow; n: var Cursor; opc: ExprKind) =
  assert opc == AndX or opc == OrX
  # The control flow graph has no `and`/`or` operators and we might already be in deeply
  # nested expression based code here. The solution is to "repair" the AST:
  # `(call (add x (add y z)) (and a b)` is rewritten to
  # `(var :tmp (bool)) (asgn tmp a) (ite tmp L1 L2)
  # L1: (asgn tmp b)
  # L2:
  # (call (add x (add y z)) tmp)`.
  # For this we stored the beginning of the stmt in `c.stmtBegin`.
  let fullExpr = rollbackToStmtBegin c
  let info = n.info
  let temp = declareBool(c, info)
  var tjmp: seq[Label] = @[]
  var fjmp: seq[Label] = @[]
  trIte c, n, tjmp, fjmp
  for t in tjmp:
    c.patch t
  c.dest.copyIntoKind(AsgnS, info):
    c.useTemp temp, info
    c.dest.addParPair TrueX, info
  let lend = c.jmpForw(info)
  # patch the false jump targets:
  for f in fjmp:
    c.patch f
  c.dest.copyIntoKind(AsgnS, info):
    c.useTemp temp, info
    c.dest.addParPair FalseX, info
  c.patch lend

  for i in 0 ..< fullExpr.len:
    c.dest.add fullExpr[i]
  c.useTemp temp, info

when false:
  proc trAnd(c: var ControlFlow; n: var Cursor) =
    let info = n.info
    inc n
    c.dest.addParLe(IteF, info)
    tr c, n
    # (if (first condition) (goto L1) (goto L2))
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
    c.dest.addParPair(FalseX, info)
    skipParRi n
    c.patch lend

  proc trOr(c: var ControlFlow; n: var Cursor) =
    let info = n.info
    inc n
    c.dest.addParLe(IteF, info)
    tr c, n
    # (if (first condition) (goto L1) (goto L2))
    # (lab :L1) (true) (goto End))
    # (lab :L2) (second condition)
    # (lab :End)
    let l1 = c.jmpForw(info)
    let l2 = c.jmpForw(info)
    c.dest.addParRi()
    c.patch l1
    c.dest.addParPair(TrueX, info)
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
  var tjmp: seq[Label] = @[]
  var fjmp: seq[Label] = @[]
  trIte c, n, tjmp, fjmp # transform condition

  # loop body is about to begin:
  for t in tjmp: c.patch t

  trStmt(c, n) # transform body
  c.jmpBack(loopStart, info)

  for f in fjmp: c.patch f
  skipParRi n

proc trIf(c: var ControlFlow; n: var Cursor) =
  var endings: seq[Label] = @[]
  inc n # if
  while true:
    let info = n.info
    let k = n.substructureKind
    if k == ElifS:
      inc n
      var tjmp: seq[Label] = @[]
      var fjmp: seq[Label] = @[]
      trIte c, n, tjmp, fjmp # condition
      for t in tjmp: c.patch t
      trStmt c, n # action
      endings.add c.jmpForw(info)
      for f in fjmp: c.patch f
      skipParRi n
    elif k == ElseS:
      inc n
      trStmt c, n
      skipParRi n
    else:
      break
  skipParRi n
  for i in countdown(endings.high, 0):
    c.patch(endings[i])

proc trStmt(c: var ControlFlow; n: var Cursor) =
  c.stmtBegin = c.dest.len
  case n.stmtKind
  of IfS:
    trIf(c, n)
  of WhileS:
    trWhile(c, n)
  of StmtsS:
    inc n
    while n.kind != ParRi:
      trStmt c, n
    inc n
  of BreakS, ForS, ContinueS, RetS, RaiseS:
    raiseAssert "not implemented"
  else:
    trExpr c, n

proc trExpr(c: var ControlFlow; n: var Cursor) =
  case n.kind
  of Symbol, SymbolDef, IntLit, UIntLit, FloatLit, StringLit, CharLit,
     Ident, DotToken, EofToken, UnknownToken:
    c.dest.add n
    inc n
  of ParRi:
    raiseAssert "unreachable"
  of ParLe:
    case n.exprKind
    of AndX:
      trStandaloneAndOr(c, n, AndX)
    of OrX:
      trStandaloneAndOr(c, n, OrX)
    else:
      # Replace copyTree with recursive transformation
      c.dest.add n
      inc n
      while n.kind != ParRi:
        trExpr c, n
      c.dest.addParRi()
      inc n

proc toControlflow*(n: Cursor): TokenBuf =
  var c = ControlFlow()
  assert n.stmtKind == StmtsS
  var n = n
  c.dest.add n
  inc n
  while n.kind != ParRi:
    trStmt c, n
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

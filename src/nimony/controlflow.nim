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
    nextLabel: int
    toPatch: seq[(Label, int)] # (label, position to patch)

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
    if i in jumpTargets: b.addRaw("L" & $i & ":\n")
    case c[i].kind
    of GotoInstr:
      b.addRaw "goto L"
      b.addRaw $(i+c[i].getInt32())
      b.addRaw "\n"
    of Symbol:
      b.addSymbol pool.syms[c[i].symId]
    of SymbolDef:
      b.addSymbolDef pool.syms[c[i].symId]
    of EofToken:
      b.addRaw "\n <unexptected EOF>\n"
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

proc genLabel(c: var ControlFlow): Label =
  result = Label(c.nextLabel)
  inc c.nextLabel

proc patch(c: var ControlFlow; p: Label) =
  # Find all positions to patch for this label and write the current position
  let target = c.dest.len
  for i in 0..<c.toPatch.len:
    if c.toPatch[i][0].int == p.int:
      c.dest[c.toPatch[i][1]].patchInt32Token(int32(target - c.toPatch[i][1]))

proc addGoto(c: var ControlFlow; target: Label; info: PackedLineInfo) =
  c.toPatch.add((target, c.dest.len))
  c.dest.add int32Token(0, info) # Will be patched later

proc addLabelDef(c: var ControlFlow; l: Label; info: PackedLineInfo) =
  c.patch(l)

proc tr(c: var ControlFlow; n: var Cursor)

proc trIf(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n # skip if header

  while true:
    let afterBranch = c.genLabel()

    let k = n.substructureKind
    if k == ElifS:
      inc n
      let thenLabel = c.genLabel()

      # Generate if with goto
      c.dest.addParLe(IfS, info)
      tr(c, n) # transform condition
      c.addGoto(thenLabel, info)
      c.addGoto(afterBranch, info)
      c.dest.addParRi()

      # Generate then branch
      c.addLabelDef(thenLabel, info)
      tr(c, n) # transform then body
      c.addGoto(afterBranch, info)
      skipParRi n
    elif k == ElseS:
      # Transform else body
      inc n
      tr(c, n)
      skipParRi n
      break
    else:
      break

    c.addLabelDef(afterBranch, info)
  skipParRi n

proc trWhile(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n

  let loopStart = c.genLabel()
  let loopBody = c.genLabel()
  let afterLoop = c.genLabel()

  c.addLabelDef(loopStart, info)

  # Generate if with goto
  c.dest.addParLe(IfS, info)
  tr(c, n) # transform condition
  c.addGoto(loopBody, info)
  c.addGoto(afterLoop, info)
  c.dest.addParRi()

  # Generate loop body
  c.addLabelDef(loopBody, info)
  tr(c, n) # transform body
  c.addGoto(loopStart, info)

  c.addLabelDef(afterLoop, info)
  skipParRi n

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
    else:
      # Replace copyTree with recursive transformation
      c.dest.add n
      inc n
      while n.kind != ParRi:
        tr(c, n)
      c.dest.addParRi()
      inc n


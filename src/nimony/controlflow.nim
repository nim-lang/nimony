#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Helper to translate control flow into `goto` based code
## which can be easier to analyze, depending on the used algorithm.

import std/assertions
include nifprelude

import nimony_model

const
  GotoInstr = InlineInt

type
  Label = distinct int
  ControlFlow = object
    dest: TokenBuf
    nextLabel: int
    toPatch: seq[(Label, int)] # (label, position to patch)

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

    if n.substructureKind != ElseS:
      let thenLabel = c.genLabel()

      # Generate if with goto
      c.dest.addParLe(IfS, info)
      tr(c, n) # transform condition recursively
      c.addGoto(thenLabel, info)
      c.addGoto(afterBranch, info)
      c.dest.addParRi()

      # Generate then branch
      c.addLabelDef(thenLabel, info)
      tr(c, n) # transform then body recursively
      c.addGoto(afterBranch, info)
    else:
      # Transform else body recursively
      tr(c, n)
      break

    if n.substructureKind notin {ElifS, ElseS}:
      break

    c.addLabelDef(afterBranch, info)
  skipPar(n)

proc trWhile(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n

  let loopStart = c.genLabel()
  let loopBody = c.genLabel()
  let afterLoop = c.genLabel()

  c.addLabelDef(loopStart, info)

  # Generate if with goto
  c.dest.addParLe(IfS, info)
  tr(c, n) # transform condition recursively
  c.addGoto(loopBody, info)
  c.addGoto(afterLoop, info)
  c.dest.addParRi()

  # Generate loop body
  c.addLabelDef(loopBody, info)
  tr(c, n) # transform body recursively
  c.addGoto(loopStart, info)

  c.addLabelDef(afterLoop, info)

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
      c.dest.addParLe(n.stmtKind, n.info)
      inc n
      while n.kind != ParRi:
        tr(c, n)
      c.dest.addParRi()
      inc n


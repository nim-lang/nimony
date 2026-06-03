#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Small type-compatibility helpers shared between the sem core (sem.nim) and
## separately-compiled handler modules such as `semmagics`. They were factored
## out of sem.nim so that `semmagics` can call them directly (they are leaves
## that do not recurse back into the dispatcher).

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
import std / [tables, sets]
include ".." / lib / nifprelude
include ".." / lib / compat2
import nimony_model, builtintypes, renderer, typeprops, semdata, sembasics

proc typeMismatch*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; got, expected: TypeCursor) =
  c.buildErr dest, info, "type mismatch: got: " & typeToString(got) & " but wanted: " & typeToString(expected)

proc typecheck*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; got, expected: TypeCursor) =
  if sameTrees(expected, got):
    discard "fine"
  elif isVoidType(expected) and isVoidType(got):
    discard "fine"
  elif isVoidType(expected) and typeKind(got) == UntypedT:
    # untyped is compatible with void in template contexts
    discard "fine"
  else:
    c.typeMismatch dest, info, got, expected

proc combineType*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; d: var Cursor; src: Cursor) =
  if typeKind(d) == AutoT:
    d = src
  elif sameTrees(d, src):
    discard "fine"
  else:
    c.typeMismatch dest, info, src, d

proc producesVoid*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; d: var Cursor) =
  if typeKind(d) in {AutoT, VoidT}:
    combineType c, dest, info, d, c.types.voidType
  else:
    c.typeMismatch dest, info, c.types.voidType, d

proc producesNoReturn*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; d: var Cursor) =
  if typeKind(d) in {AutoT, VoidT}:
    combineType c, dest, info, d, c.types.voidType
  else:
    # allowed in expression context
    discard

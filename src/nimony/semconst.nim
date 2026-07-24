#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Compile-time constant-expression evaluation: `semConstExpr` / `semBoolExpr`
## and the `evalConst*` helpers that fold an expression to a value.
##
## Extracted from sem.nim. The only re-entry into the sem core is `semExpr`
## (already a `SemContext` callback), so this module imports no part of the
## engine — modules that need const-eval (sem itself, sempragmas) import this
## directly instead of routing through extra callbacks.

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
import std / [tables, sets]
include ".." / lib / nifprelude
include ".." / lib / compat2
import nimony_model, builtintypes, xints, semdata, sembasics,
  renderer, expreval, programs

# --- thin shim forwarding into the sem core via the SemContext callback ---

proc semExpr(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag] = {}) =
  c.semExprCB(c, dest, it, flags)

# --- const-eval (moved verbatim from sem.nim) ---

proc semBoolExprBody(c: var SemContext; dest: var TokenBuf; n: var Cursor; start: int): Item =
  ## Shared core of `semBoolExpr` / `semConstBoolExpr`: sems an expression and
  ## appends a type-mismatch error iff no better error was produced already.
  let origInfo = n.info
  result = Item(n: n, typ: c.types.autoType)
  semExpr c, dest, result
  let t = skipModifier(result.typ)
  if classifyType(c, t) != BoolT and not hasErrorSince(dest, start):
    combineErr c, dest, start, origInfo,
      "expected `bool` but got: " & typeToString(t)

proc semBoolExpr*(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  let start = dest.len
  let it = semBoolExprBody(c, dest, n, start)
  n = it.n

proc semConstBoolExpr*(c: var SemContext; dest: var TokenBuf; n: var Cursor; allowUnresolved = false) =
  let start = dest.len
  let it = semBoolExprBody(c, dest, n, start)
  n = it.n
  var e = cursorAt(dest, start)
  var valueBuf = evalExpr(c, e)
  endRead e
  expectUnique dest
  let value = cursorAt(valueBuf, 0)
  if not isConstBoolValue(value):
    if allowUnresolved:
      discard
    elif value.isTagLit and value.cursorTagId == nifpools.ErrT:
      dest.shrink start
      dest.add valueBuf
    else:
      dest.shrink start
      buildErr c, dest, it.n.info, "expected constant bool value but got: " & asNimCode(value)
  else:
    dest.shrink start
    dest.add valueBuf

proc semConstStrExpr*(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  let start = dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, dest, it
  n = it.n
  let t = skipModifier(it.typ)
  if not isStringType(t):
    dest.shrink start
    buildErr c, dest, it.n.info, "expected `string` but got: " & typeToString(t)
  var e = cursorAt(dest, start)
  var valueBuf = evalExpr(c, e)
  endRead e
  expectUnique dest
  let value = cursorAt(valueBuf, 0)
  if not isConstStringValue(value):
    if value.isTagLit and value.cursorTagId == nifpools.ErrT:
      dest.add valueBuf
    else:
      buildErr c, dest, it.n.info, "expected constant string value but got: " & asNimCode(value)
  else:
    dest.shrink start
    dest.add valueBuf

proc semConstStrExprIgnoreTopLevel*(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  case c.phase
  of SemcheckTopLevelSyms:
    # XXX `const`s etc are not evaluated yet
    dest.takeTree n
  of SemcheckSignaturesInProgress, SemcheckSignatures,
     SemcheckBodiesInProgress, SemcheckBodies:
    semConstStrExpr(c, dest, n)

proc semConstIntExpr*(c: var SemContext; dest: var TokenBuf; n: var Cursor; phaseOverride: SemPhase) =
  let start = dest.len
  var it = Item(n: n, typ: c.types.autoType)
  let oldPhase = c.phase
  c.phase = phaseOverride
  semExpr c, dest, it
  c.phase = oldPhase
  n = it.n
  let t = skipModifier(it.typ)
  if classifyType(c, t) != IntT:
    dest.shrink start
    buildErr c, dest, it.n.info, "expected `int` but got: " & typeToString(t)
  var e = cursorAt(dest, start)
  var valueBuf = evalExpr(c, e)
  endRead e
  expectUnique dest
  let value = cursorAt(valueBuf, 0)
  if not isConstIntValue(value):
    if value.isTagLit and value.cursorTagId == nifpools.ErrT:
      dest.add valueBuf
    else:
      dest.shrink start
      buildErr c, dest, it.n.info, "expected constant integer value but got: " & asNimCode(value)
  else:
    dest.shrink start
    dest.add valueBuf

proc semConstExpr*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let start = dest.len
  var phase = SemcheckBodies
  swap c.phase, phase
  semExpr c, dest, it
  swap c.phase, phase
  # XXX future note: consider when the expression depends on a generic param
  var e = cursorAt(dest, start)
  var valueBuf = evalExpr(c, e, it.typ)
  endRead e
  expectUnique dest
  dest.shrink start
  var value = beginRead(valueBuf)
  annotateConstantType dest, it.typ, value

proc addXint*(c: var SemContext; dest: var TokenBuf; x: xint; info: PackedLineInfo) =
  var err = false
  let val = asSigned(x, err)
  if not err:
    dest.addIntLit(val, info)
  else:
    let val = asUnsigned(x, err)
    if not err:
      dest.addUIntLit(val, info)
    else:
      c.buildErr dest, info, "enum value not a constant expression"

proc evalConstExpr*(c: var SemContext; dest: var TokenBuf; n: var Cursor; expected: TypeCursor): TokenBuf =
  let beforeExpr = dest.len
  var x = Item(n: n, typ: expected)
  semExpr c, dest, x
  n = x.n
  var e = cursorAt(dest, beforeExpr)
  result = evalExpr(c, e)
  endRead e

proc evalConstIntExpr*(c: var SemContext; dest: var TokenBuf; n: var Cursor; expected: TypeCursor): xint =
  let info = n.info
  var valueBuf = evalConstExpr(c, dest, n, expected)
  let value = beginRead(valueBuf)
  result = getConstOrdinalValue(value)
  if result.isNaN:
    if value.isTagLit and value.cursorTagId == nifpools.ErrT:
      dest.add valueBuf
    else:
      buildErr c, dest, info, "expected constant integer value but got: " & asNimCode(value)

proc evalConstStrExpr*(c: var SemContext; dest: var TokenBuf; n: var Cursor; expected: TypeCursor): StrId =
  let info = n.info
  var valueBuf = evalConstExpr(c, dest, n, expected)
  let value = beginRead(valueBuf)
  result = getConstStringValue(value)
  if result == StrId(0):
    if value.isTagLit and value.cursorTagId == nifpools.ErrT:
      dest.add valueBuf
    else:
      buildErr c, dest, info, "expected constant string value but got: " & asNimCode(value)

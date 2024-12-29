#
#
#           Gear3 Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A type navigator can recompute the type of an expression.

include nifprelude
import std/assertions

import ".." / nimony / [nimony_model, builtintypes, decls, programs]

type
  TypeCache* = object
    builtins: BuiltinTypes
    mem: seq[TokenBuf]

proc createTypeCache*(): TypeCache =
  TypeCache(builtins: createBuiltinTypes())

proc firstSon(n: Cursor): Cursor {.inline.} =
  result = n
  inc result

proc getTypeImpl(c: var TypeCache; n: Cursor): Cursor =
  result = c.builtins.autoType # to indicate error
  case exprKind(n)
  of NoExpr:
    if n.kind == Symbol:
      let res = tryLoadSym(n.symId)
      if res.status == LacksNothing:
        let local = asLocal(res.decl)
        if local.kind.isLocal:
          result = local.typ
        else:
          if isRoutine(symKind(res.decl)):
            result = res.decl
      else:
        quit "gear3:could not find symbol: " & pool.syms[n.symId]
  of AtX, PatX:
    result = getTypeImpl(c, n.firstSon)
    if typeKind(result) == ArrayT:
      inc result # to the element type
    else:
      result = c.builtins.autoType # still an error
  of ExprX:
    var n = n
    inc n # skip "expr"
    while n.kind != ParRi:
      let prev = n
      skip n
      if n.kind == ParRi:
        result = getTypeImpl(c, prev)
  of CallX, CallStrLitX, InfixX, PrefixX, CmdX:
    result = getTypeImpl(c, n.firstSon)
    if isRoutine(symKind(result)):
      let routine = asRoutine(result)
      result = routine.retType
    elif typeKind(result) in {IterT, ProcT}:
      inc result
      inc result # dot token
      skip result # parameters
  of FalseX, TrueX, AndX, OrX, NotX, DefinedX, DeclaredX, IsMainModuleX, EqX, NeqX, LeX, LtX,
     CompilesX:
    result = c.builtins.boolType
  of NegX, NegInfX, NanX, InfX:
    result = c.builtins.floatType
  of EnumToStrX, DefaultObjX, DefaultTupX:
    result = c.builtins.stringType
  of SizeofX:
    result = c.builtins.intType
  of AddX, SubX, MulX, DivX, ModX, ShlX, ShrX, AshrX, BitandX, BitorX, BitxorX, BitnotX,
     CastX, ConvX, OconvX, HconvX, DconvX, OconstrX:
    result = n.firstSon
  of ParX:
    result = getTypeImpl(c, n.firstSon)
  of NilX:
    result = c.builtins.nilType
  of DotX:
    result = n
    skip result # obj
    result = getTypeImpl(c, result) # typeof(obj.field) == typeof field
  of DerefX, HderefX:
    result = getTypeImpl(c, n.firstSon)
    if typeKind(result) in {RefT, PtrT}:
      inc result
    else:
      result = c.builtins.autoType # still an error
  of RangesX, RangeX:
    result = getTypeImpl(c, n.firstSon)
  of QuotedX, OchoiceX, CchoiceX, UnpackX, TypeofX, LowX, HighX:
    discard "keep the error type"
  of KvX:
    var n = n
    inc n # skip "kv"
    skip n # skip key
    result = getTypeImpl(c, n)
  of AddrX, HaddrX:
    let elemType = getTypeImpl(c, n.firstSon)
    var buf = createTokenBuf(4)
    buf.add parLeToken(PtrT, n.info)
    buf.addSubtree elemType
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of SetX:
    let elemType = getTypeImpl(c, n.firstSon)
    var buf = createTokenBuf(4)
    buf.add parLeToken(SetT, n.info)
    buf.addSubtree elemType
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of TupleConstrX:
    var buf = createTokenBuf(4)
    buf.add parLeToken(TupleT, n.info)
    var n = n
    inc n
    while n.kind != ParRi:
      buf.addSubtree getTypeImpl(c, n)
      skip n
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of AconstrX:
    let elemType = getTypeImpl(c, n.firstSon)
    var buf = createTokenBuf(4)
    buf.add parLeToken(ArrayT, n.info)
    buf.addSubtree elemType
    var n = n
    var arrayLen = 0
    while n.kind != ParRi:
      skip n
      inc arrayLen
    buf.addIntLit(arrayLen, n.info)
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of SufX:
    var n = n
    inc n # tag
    skip n # expr
    if n.kind in {Ident, StringLit}:
      case pool.strings[n.litId]
      of "i": result = c.builtins.intType
      of "i8": result = c.builtins.int8Type
      of "i16": result = c.builtins.int16Type
      of "i32": result = c.builtins.int32Type
      of "i64": result = c.builtins.int64Type
      of "u": result = c.builtins.uintType
      of "u8": result = c.builtins.uint8Type
      of "u16": result = c.builtins.uint16Type
      of "u32": result = c.builtins.uint32Type
      of "u64": result = c.builtins.uint64Type
      of "f": result = c.builtins.floatType
      of "f32": result = c.builtins.float32Type
      of "f64": result = c.builtins.float64Type
      of "R": result = c.builtins.stringType
      else: result = c.builtins.autoType

proc getType*(c: var TypeCache; n: Cursor): Cursor =
  getTypeImpl c, n


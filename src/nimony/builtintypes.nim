#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

include ".." / lib / nifprelude
import nimony_model

type
  BuiltinTypes* = object
    mem: TokenBuf
    autoType*, stringType*, intType*, uintType*, floatType*, boolType*, charType*: Cursor
    voidType*, nilType*: Cursor
    int8Type*, int16Type*, int32Type*, int64Type*: Cursor
    uint8Type*, uint16Type*, uint32Type*, uint64Type*: Cursor
    float32Type*, float64Type*: Cursor
    emptyTupleType*: Cursor
    untypedType*: Cursor
    cstringType*: Cursor
    vtableType*: Cursor # UncheckedArray[pointer]
    continuationType*: Cursor

const
  sso* = true ## set to true to enable SSO string implementation

const
  SystemModuleSuffix* = "sysvq0asl" # "sys9azlf"
  StringName* = "string.0." & SystemModuleSuffix

when sso:
  const
    LongStringName* = "LongString.0." & SystemModuleSuffix
    StringBytesField* = "bytes.0"
    StringMoreField* = "more.0"
    LongStringFullLenField* = "fullLen.0"
    LongStringRcField* = "rc.0"
    LongStringCapImplField* = "capImpl.0"
    LongStringDataField* = "data.0"
else:
  const
    StringAField* = "a.0"
    StringIField* = "i.0"

const
  ErrorCodeName* = "ErrorCode.0." & SystemModuleSuffix
  SuccessName* = "Success.0." & SystemModuleSuffix
  FailureName* = "Failure.0." & SystemModuleSuffix
  ExceptionName* = "Exception.0." & SystemModuleSuffix
  ExcThreadVarName* = "exc.0." & SystemModuleSuffix
  ContinuationName* = "Continuation.0." & SystemModuleSuffix
  OpenArrayHeadName* = "openArray.0." & SystemModuleSuffix

proc createBuiltinTypes*(bits: int): BuiltinTypes =
  # Positions are recorded while building rather than hardcoded, so the
  # layout stays correct when `-d:virtualParRi` elides the `)` tokens.
  result = BuiltinTypes(mem: createTokenBuf(30))

  let autoPos = result.mem.len
  result.mem.addParLe "auto"
  result.mem.addParRi()

  let stringPos = result.mem.len
  when true:
    result.mem.addSymUse(pool.syms.getOrIncl(StringName), NoLineInfo)
    result.mem.addDotToken()
  else:
    result.mem.addParLe "string"
    result.mem.addParRi()

  let boolPos = result.mem.len
  result.mem.addParLe "bool"
  result.mem.addParRi()

  let configBits = bits
  let intPos = result.mem.len
  result.mem.addParLe "i"
  result.mem.addIntLit(configBits, NoLineInfo)
  result.mem.addParRi()

  let uintPos = result.mem.len
  result.mem.addParLe "u"
  result.mem.addIntLit(configBits, NoLineInfo)
  result.mem.addParRi()

  let floatPos = result.mem.len
  result.mem.addParLe "f"
  result.mem.addIntLit(64, NoLineInfo)
  result.mem.addParRi()

  let charPos = result.mem.len
  result.mem.addParLe "c"
  result.mem.addIntLit(8, NoLineInfo)
  result.mem.addParRi()

  let voidPos = result.mem.len
  result.mem.addDotToken(NoLineInfo)

  let niltPos = result.mem.len
  result.mem.addParLe "nilt"
  result.mem.addParRi()

  template addBitsType(tag: string, bits: int): int =
    let pos = result.mem.len
    result.mem.addParLe(tag)
    result.mem.addIntLit(bits, NoLineInfo)
    result.mem.addParRi()
    pos

  let int8Pos = addBitsType("i", 8)
  let int16Pos = addBitsType("i", 16)
  let int32Pos = addBitsType("i", 32)
  let int64Pos = addBitsType("i", 64)

  let uint8Pos = addBitsType("u", 8)
  let uint16Pos = addBitsType("u", 16)
  let uint32Pos = addBitsType("u", 32)
  let uint64Pos = addBitsType("u", 64)

  let float32Pos = addBitsType("f", 32)
  let float64Pos = addBitsType("f", 64)

  let tuplePos = result.mem.len
  result.mem.addParLe "tuple"
  result.mem.addParRi()

  let untypedPos = result.mem.len
  result.mem.addParLe "untyped"
  result.mem.addParRi()

  let cstringPos = result.mem.len
  result.mem.addParLe "cstring"
  result.mem.addParLe "notnil"
  result.mem.addParRi() # close notnil
  result.mem.addParRi() # close cstring

  # UncheckedArray[pointer] = (uarray (ptr (void)))
  let vtablePos = result.mem.len
  result.mem.addParLe "uarray"
  result.mem.addParLe "ptr"
  result.mem.addParLe "void"
  result.mem.addParRi() # close void
  result.mem.addParRi() # close ptr
  result.mem.addParRi() # close uarray

  let continuationPos = result.mem.len
  result.mem.addSymUse(pool.syms.getOrIncl(ContinuationName), NoLineInfo)


  result.autoType = result.mem.cursorAt(autoPos)
  result.stringType = result.mem.cursorAt(stringPos)
  result.boolType = result.mem.cursorAt(boolPos)
  result.intType = result.mem.cursorAt(intPos)
  result.uintType = result.mem.cursorAt(uintPos)
  result.floatType = result.mem.cursorAt(floatPos)
  result.charType = result.mem.cursorAt(charPos)
  result.voidType = result.mem.cursorAt(voidPos)
  result.nilType = result.mem.cursorAt(niltPos)
  result.int8Type = result.mem.cursorAt(int8Pos)
  result.int16Type = result.mem.cursorAt(int16Pos)
  result.int32Type = result.mem.cursorAt(int32Pos)
  result.int64Type = result.mem.cursorAt(int64Pos)
  result.uint8Type = result.mem.cursorAt(uint8Pos)
  result.uint16Type = result.mem.cursorAt(uint16Pos)
  result.uint32Type = result.mem.cursorAt(uint32Pos)
  result.uint64Type = result.mem.cursorAt(uint64Pos)
  result.float32Type = result.mem.cursorAt(float32Pos)
  result.float64Type = result.mem.cursorAt(float64Pos)
  result.emptyTupleType = result.mem.cursorAt(tuplePos)
  result.untypedType = result.mem.cursorAt(untypedPos)
  result.cstringType = result.mem.cursorAt(cstringPos)
  result.vtableType = result.mem.cursorAt(vtablePos)
  result.continuationType = result.mem.cursorAt(continuationPos)

proc isStringType*(a: Cursor): bool {.inline.} =
  result = a.isSymbol and a.symId == pool.syms.getOrIncl(StringName)
  #a.typeKind == StringT: StringT now unused!

proc isSomeStringType*(a: Cursor): bool {.inline.} =
  result = a.typeKind == CstringT or isStringType(a)

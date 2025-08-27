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

proc tagToken(tag: string; info: PackedLineInfo = NoLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

const
  SystemModuleSuffix* = "sysvq0asl" # "sys9azlf"
  StringName* = "string.0." & SystemModuleSuffix
  StringAField* = "a.0." & SystemModuleSuffix
  StringIField* = "i.0." & SystemModuleSuffix
  ErrorCodeName* = "ErrorCode.0." & SystemModuleSuffix
  SuccessName* = "Success.0." & SystemModuleSuffix

proc createBuiltinTypes*(): BuiltinTypes =
  result = BuiltinTypes(mem: createTokenBuf(30))

  result.mem.add tagToken"auto" # 0
  result.mem.addParRi() # 1

  when true:
    result.mem.add symToken(pool.syms.getOrIncl(StringName), NoLineInfo) # 2
    result.mem.addDotToken() # 3
  else:
    result.mem.add tagToken"string" # 2
    result.mem.addParRi() # 3

  result.mem.add tagToken"bool" # 4
  result.mem.addParRi() # 5

  let minusOne = pool.integers.getOrIncl(-1)
  result.mem.add tagToken"i" # 6
  result.mem.add intToken(minusOne, NoLineInfo) # 7
  result.mem.addParRi() # 8

  result.mem.add tagToken"u" # 9
  result.mem.add intToken(minusOne, NoLineInfo) # 10
  result.mem.addParRi() # 11

  result.mem.add tagToken"f" # 12
  result.mem.add intToken(pool.integers.getOrIncl(64), NoLineInfo) # 13
  result.mem.addParRi() # 14

  result.mem.add tagToken"c" # 15
  result.mem.add intToken(pool.integers.getOrIncl(8), NoLineInfo) # 16
  result.mem.addParRi() # 17

  result.mem.add dotToken(NoLineInfo) # 18

  result.mem.add tagToken"nilt" # 19
  result.mem.addParRi() # 20

  template addBitsType(tag: string, bits: int) =
    # adds 3
    result.mem.add tagToken(tag) # +1
    result.mem.add intToken(pool.integers.getOrIncl(bits), NoLineInfo) # +2
    result.mem.addParRi() # +3

  addBitsType "i", 8 # 21
  addBitsType "i", 16 # 24
  addBitsType "i", 32 # 27
  addBitsType "i", 64 # 30

  addBitsType "u", 8 # 33
  addBitsType "u", 16 # 36
  addBitsType "u", 32 # 39
  addBitsType "u", 64 # 42

  addBitsType "f", 32 # 45
  addBitsType "f", 64 # 48

  result.mem.add tagToken"tuple" # 51
  result.mem.addParRi() # 52

  result.mem.add tagToken"untyped" # 53
  result.mem.addParRi() # 54

  result.mem.add tagToken"cstring" # 55
  result.mem.addParRi() # 56

  result.mem.freeze()

  result.autoType = result.mem.cursorAt(0)
  result.stringType = result.mem.cursorAt(2)
  result.boolType = result.mem.cursorAt(4)
  result.intType = result.mem.cursorAt(6)
  result.uintType = result.mem.cursorAt(9)
  result.floatType = result.mem.cursorAt(12)
  result.charType = result.mem.cursorAt(15)
  result.voidType = result.mem.cursorAt(18)
  result.nilType = result.mem.cursorAt(19)
  result.int8Type = result.mem.cursorAt(21)
  result.int16Type = result.mem.cursorAt(24)
  result.int32Type = result.mem.cursorAt(27)
  result.int64Type = result.mem.cursorAt(30)
  result.uint8Type = result.mem.cursorAt(33)
  result.uint16Type = result.mem.cursorAt(36)
  result.uint32Type = result.mem.cursorAt(39)
  result.uint64Type = result.mem.cursorAt(42)
  result.float32Type = result.mem.cursorAt(45)
  result.float64Type = result.mem.cursorAt(48)
  result.emptyTupleType = result.mem.cursorAt(51)
  result.untypedType = result.mem.cursorAt(53)
  result.cstringType = result.mem.cursorAt(55)

proc isStringType*(a: Cursor): bool {.inline.} =
  result = a.kind == Symbol and a.symId == pool.syms.getOrIncl(StringName)
  #a.typeKind == StringT: StringT now unused!

proc isSomeStringType*(a: Cursor): bool {.inline.} =
  result = a.typeKind == CstringT or isStringType(a)

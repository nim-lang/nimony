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

proc addSuccessTupleType*(dest: var TokenBuf; retType: Cursor; info: NifLineInfo) =
  ## The type a `.raises` routine's result actually travels in: `ErrorCode`
  ## alone when the routine returns nothing, `(tuple ErrorCode T)` when it
  ## returns a `T`. Prefer `addLengReturnType` unless the caller has already
  ## established that the routine raises.
  if retType.isDotToken or retType.typeKind == VoidT:
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
  else:
    dest.addParLe TupleT, info
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
    dest.addSubtree retType
    dest.addParRi()

proc addLengReturnType*(dest: var TokenBuf; retType, pragmas: Cursor;
                        info: NifLineInfo) =
  ## THE mapping from a routine's Nimony return type to its Leng one, and the
  ## only place it is decided.
  ##
  ## Hexer keeps asking the Nimony type system what things are, and that answer
  ## stays in Nimony's terms: a `.raises` routine returns `T` and says so with
  ## a pragma. Leng has no exceptions, so at the boundary that becomes a value
  ## and a code travelling together. Whoever crosses the boundary applies this,
  ## and everyone who does agrees by construction — which is the whole
  ## requirement, because the two type systems are allowed to differ and are
  ## not allowed to differ INCONSISTENTLY. A signature rewritten one way and a
  ## proctype rewritten the other is a function pointer that does not match its
  ## function, and nothing but the C compiler is going to notice.
  ##
  ## `raiselowering` is the one pass that BAKES the answer into the
  ## declarations it emits, because `cps` runs after it and builds a
  ## coroutine's frame and result slot out of the return type. Everything that
  ## still queries Nimony afterwards — `cps` for a callee, `lengcgen` for a
  ## foreign proctype, which is pulled in as a type and never as code — has to
  ## come back here for the same answer.
  if hasPragma(pragmas, RaisesP):
    addSuccessTupleType(dest, retType, info)
  else:
    dest.addSubtree retType

proc addPragmasWithoutRaises*(dest: var TokenBuf; pragmas: Cursor) =
  ## Copy a pragma list minus `(raises)`. Used where the raising-ness of a
  ## routine has already been spent — `raiselowering` has put the success tuple
  ## in the signature, so leaving the pragma on would have `lengcgen` wrap the
  ## return type a second time — and on the procs `cps` generates FOR a
  ## coroutine, which return a `Continuation` and never fail.
  if pragmas.isDotToken or not pragmas.isTagLit:
    dest.addDotToken()
    return
  var n = pragmas
  dest.addParLe(n.cursorTagId, n.info)
  n = sub(n)
  while n.hasMore:
    if n.pragmaKind == RaisesP: skip n
    else: dest.takeTree n
  dest.addParRi()

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

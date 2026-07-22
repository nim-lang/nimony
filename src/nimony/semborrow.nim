#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Implements the transformation that the `.borrow` pragma requires.

import std / [tables, sets, hashes, syncio, formatfloat, assertions]
include ".." / lib / nifprelude
include ".." / lib / compat2
import nimony_model, symtabs, builtintypes, decls,
  programs, sigmatch, magics, reporters,
  semdata, sembasics, typeprops
import ".." / lib / symparser

proc genBorrowedProcBody*(c: var SemContext; fn: StrId; signature: Cursor; info: PackedLineInfo): TokenBuf =
  #[Consider:

  type
    VarId* = distinct int

  proc `+`*(x, y: VarId): VarId {.borrow.}

  Implementation: Generate a body for this proc in phase 3.
  The body is a single call of the current proc name, converted
  to the distinct type, if the return type is one. The type could also
  be generic, so generate the parmeter type properly. It also needs to
  skip modifiers first.]#
  var n = signature
  result = createTokenBuf(10)
  result.addParLe(StmtsS, info)
  result.addParLe(CallS, info)
  result.addIdent(fn, info)
  var distinctParams = 0
  n.into ParamsU:
    while n.hasMore:
      let param = asLocal(n)
      if param.kind == ParamY and param.name.isSymbolDef:
        var isDistinct = false
        let destType = skipDistinct(param.typ, isDistinct)
        if isDistinct:
          result.addParLe(DconvX, info)
          result.copyTree destType
          result.addSymUse(param.name.symId, info)
          result.addParRi(info)
          inc distinctParams
        else:
          result.addSymUse(param.name.symId, info)
      skip n
  if n.isDotToken:
    discard "fine: no return type"
  else:
    var isDistinct = false
    discard skipDistinct(n, isDistinct)
    if isDistinct:
      var finalConv = createTokenBuf(4)
      finalConv.addParLe(DconvX, info)
      finalConv.copyTree n
      result.insert finalConv, 1
      result.addParRi(info)

  result.addParRi(info)
  result.addParRi(info)
  if distinctParams == 0:
    result.shrink 0
    result.buildLocalErr info, "cannot .borrow: no parameter of a `distinct` type"

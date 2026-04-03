#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Generation for enum to string conversions.

include nifprelude

import decls, nimony_model, semdata, sembasics, symtabs

proc tagToken(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

proc genEnumToStrProcCase(c: var SemContext; dest: var TokenBuf; enumDecl: var Cursor; symId, enumSymId: SymId) =
  dest.add tagToken("case", enumDecl.info)
  dest.add symToken(symId, enumDecl.info)
  inc enumDecl # skips enum
  skip enumDecl # skips base type
  while enumDecl.kind != ParRi:
    let enumDeclInfo = enumDecl.info
    dest.add tagToken("of", enumDeclInfo)

    dest.add tagToken("ranges", enumDeclInfo)

    inc enumDecl
    let symId = enumDecl.symId
    let symInfo = enumDecl.info
    inc enumDecl
    skip enumDecl
    skip enumDecl
    skip enumDecl

    inc enumDecl # skips tupleConstr
    inc enumDecl # skips counter field
    var fieldValue = enumDecl
    inc enumDecl # skips fieldValue
    inc enumDecl # Skips ParRi

    inc enumDecl # Skips ParRi

    while enumDecl.kind == ParLe and enumDecl.tagId == ErrT:
      skip enumDecl

    dest.add symToken(symId, symInfo)
    dest.addParRi() # set

    dest.add tagToken("stmts", enumDeclInfo)
    dest.add tagToken("ret", enumDeclInfo)
    if fieldValue.kind == StringLit:
      dest.add strToken(fieldValue.litId, enumDeclInfo)
    else:
      # handle errors
      dest.addSubtree fieldValue

    dest.addParRi() # ret
    dest.addParRi() # stmts

    dest.addParRi() # of

  dest.addParRi() # case

proc genEnumToStrProc*(c: var SemContext; dest: var TokenBuf; typeDecl: var Cursor) =
  let decl = asTypeDecl(typeDecl)
  let enumSymId = decl.name.symId
  let enumSymInfo = decl.name.info
  let dollorName = "dollar`." & pool.syms[enumSymId]
  let dollorSymId = pool.syms.getOrIncl(dollorName)

  dest.add tagToken("proc", enumSymInfo)
  dest.add symdefToken(dollorSymId, enumSymInfo)

  # TODO: defaults to (nodecl)
  # TODO: static for local functions
  if c.currentScope.kind == ToplevelScope:
    let exportIdent = pool.strings.getOrIncl("x")
    dest.add identToken(exportIdent, enumSymInfo)
  else:
    dest.addDotToken() # exportIdent
  dest.addDotToken()
  dest.addDotToken()

  var paramName = "e"
  c.makeLocalSym(paramName)
  let paramSymId = pool.syms.getOrIncl(paramName)
  dest.add tagToken("params", enumSymInfo)
  dest.add tagToken("param", enumSymInfo)
  dest.add symdefToken(paramSymId, enumSymInfo)
  dest.addDotToken()
  dest.addDotToken()
  dest.add symToken(enumSymId, enumSymInfo)
  dest.addDotToken()
  dest.addParRi() # param
  dest.addParRi() # params

  dest.addSubtree c.types.stringType

  dest.addDotToken()
  dest.addDotToken()

  dest.add tagToken("stmts", enumSymInfo)
  var body = decl.body
  genEnumToStrProcCase(c, dest, body, paramSymId, enumSymId)
  dest.addParRi() # stmts

  dest.addParRi() # proc

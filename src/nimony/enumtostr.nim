#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Generation for enum to string conversions.

include nifprelude

import decls, nimony_model, semdata, sembasics

proc tagToken(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

proc genEnumToStrProcCase(c: var SemContext; enumDecl: var Cursor; symId, enumSymId: SymId) =
  c.dest.add tagToken("case", enumDecl.info)
  c.dest.add symToken(symId, enumDecl.info)
  inc enumDecl # skips enum
  skip enumDecl # skips base type
  while enumDecl.kind != ParRi:
    let enumDeclInfo = enumDecl.info
    c.dest.add tagToken("of", enumDeclInfo)

    c.dest.add tagToken("ranges", enumDeclInfo)

    inc enumDecl
    let symId = enumDecl.symId
    let symInfo = enumDecl.info
    inc enumDecl
    skip enumDecl
    skip enumDecl
    skip enumDecl

    inc enumDecl # skips tupleConstr
    inc enumDecl # skips counter field
    let fieldValue = enumDecl.litId
    inc enumDecl # skips fieldValue
    inc enumDecl # Skips ParRi

    inc enumDecl # Skips ParRi

    c.dest.add symToken(symId, symInfo)
    c.dest.addParRi() # set

    c.dest.add tagToken("stmts", enumDeclInfo)
    c.dest.add tagToken("ret", enumDeclInfo)
    c.dest.add strToken(fieldValue, enumDeclInfo)
    c.dest.addParRi() # ret
    c.dest.addParRi() # stmts

    c.dest.addParRi() # of

  c.dest.addParRi() # case

proc genEnumToStrProc*(c: var SemContext; typeDecl: var Cursor) =
  let decl = asTypeDecl(typeDecl)
  let enumSymId = decl.name.symId
  let enumSymInfo = decl.name.info
  let dollorName = "dollar`." & pool.syms[enumSymId]
  let dollorSymId = pool.syms.getOrIncl(dollorName)

  c.dest.add tagToken("proc", enumSymInfo)
  c.dest.add symdefToken(dollorSymId, enumSymInfo)

  # TODO: defaults to (nodecl)
  # TODO: static for local functions
  let exportIdent = pool.strings.getOrIncl("x")
  c.dest.add identToken(exportIdent, enumSymInfo)
  c.dest.addDotToken()
  c.dest.addDotToken()

  var paramName = "e"
  c.makeLocalSym(paramName)
  let paramSymId = pool.syms.getOrIncl(paramName)
  c.dest.add tagToken("params", enumSymInfo)
  c.dest.add tagToken("param", enumSymInfo)
  c.dest.add symdefToken(paramSymId, enumSymInfo)
  c.dest.addDotToken()
  c.dest.addDotToken()
  c.dest.add symToken(enumSymId, enumSymInfo)
  c.dest.addDotToken()
  c.dest.addParRi() # param
  c.dest.addParRi() # params

  c.dest.addSubtree c.types.stringType

  c.dest.addDotToken()
  c.dest.addDotToken()

  c.dest.add tagToken("stmts", enumSymInfo)
  var body = decl.body
  genEnumToStrProcCase(c, body, paramSymId, enumSymId)
  c.dest.addParRi() # stmts

  c.dest.addParRi() # proc

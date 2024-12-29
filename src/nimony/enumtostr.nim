include nifprelude

import decls, nimony_model


proc tagToken(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

proc genEnumToStrProcCase(dest: var TokenBuf; enumDecl: var Cursor; symId: SymId) =
  dest.add tagToken("case", enumDecl.info)
  dest.add symToken(symId, enumDecl.info)
  inc enumDecl # skips enum
  skip enumDecl # skips base type
  while enumDecl.kind != ParRi:
    let enumDeclInfo = enumDecl.info
    dest.add tagToken("of", enumDeclInfo)

    dest.add tagToken("set", enumDeclInfo)

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

    dest.add symToken(symId, symInfo)
    dest.addParRi() # set

    dest.add tagToken("stmts", enumDeclInfo)
    dest.add tagToken("ret", enumDeclInfo)
    dest.add strToken(fieldValue, enumDeclInfo)
    dest.addParRi() # ret
    dest.addParRi() # stmts

    dest.addParRi() # of

  dest.addParRi() # case

proc genEnumToStrProc*(dest: var TokenBuf, typeDecl: var Cursor; stringType: Cursor) =
  let decl = asTypeDecl(typeDecl)
  let enumSymId = decl.name.symId
  let enumSymInfo = decl.name.info
  let dollorName = "dollar`." & pool.syms[enumSymId]
  let dollorSymId = pool.syms.getOrIncl(dollorName)

  dest.add tagToken("proc", enumSymInfo)
  dest.add symdefToken(dollorSymId, enumSymInfo)

  # Todo: defaults to (nodecl)
  let exportIdent = pool.strings.getOrIncl("x")
  dest.add identToken(exportIdent, enumSymInfo)
  dest.addDotToken()
  dest.addDotToken()

  let paramSymId = pool.syms.getOrIncl("e")
  dest.add tagToken("params", enumSymInfo)
  dest.add tagToken("param", enumSymInfo)
  dest.add symdefToken(paramSymId, enumSymInfo)
  dest.addDotToken()
  dest.addDotToken()
  dest.add symToken(enumSymId, enumSymInfo)
  dest.addDotToken()
  dest.addParRi() # param
  dest.addParRi() # params

  dest.add stringType
  dest.addParRi()

  dest.addDotToken()
  dest.addDotToken()


  dest.add tagToken("stmts", enumSymInfo)
  var body = decl.body
  genEnumToStrProcCase(dest, body, paramSymId)
  dest.addParRi() # stmts

  dest.addParRi() # proc

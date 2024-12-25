include nifprelude

import decls, nimony_model


proc tagToken(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

proc genEnumToStrProcCase(dest: var TokenBuf; enumDecl: var Cursor; symId: SymId) =
  dest.add tagToken("case", enumDecl.info)
  dest.add symToken(symId, enumDecl.info)
  inc enumDecl # skips enum
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

proc genEnumToStrProc*(typeDecl: var Cursor; stringType: var Cursor): TokenBuf =
  result = createTokenBuf()
  let decl = asTypeDecl(typeDecl)
  let enumSymId = decl.name.symId
  let dollorName = "dollar." & pool.syms[enumSymId]
  let dollorSymId = pool.syms.getOrIncl(dollorName)

  result.add tagToken("proc", typeDecl.info)
  result.add symdefToken(dollorSymId, typeDecl.info)

  # Todo: defaults to (nodecl)
  result.addDotToken()
  result.addDotToken()
  result.addDotToken()

  let paramSymId = pool.syms.getOrIncl("e")
  result.add tagToken("params", typeDecl.info)
  result.add tagToken("param", typeDecl.info)
  result.add symdefToken(paramSymId, typeDecl.info)
  result.addDotToken()
  result.addDotToken()
  result.add symToken(enumSymId, typeDecl.info)
  result.addDotToken()
  result.addParRi() # param
  result.addParRi() # params

  result.add stringType
  result.addParRi()

  result.addDotToken()
  result.addDotToken()


  result.add tagToken("stmts", typeDecl.info)
  var body = decl.body
  genEnumToStrProcCase(result, body, paramSymId)
  result.addParRi() # stmts

  result.addParRi() # proc

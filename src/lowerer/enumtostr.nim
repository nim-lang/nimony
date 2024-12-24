include nifprelude

import ".." / nimony / [decls]
import utils

proc genEnumToStrProcCase(dest: var TokenBuf; enumDecl: var Cursor; symId: SymId) =
  dest = createTokenBuf()
  dest.add tagToken("case", enumDecl.info)
  dest.add symToken(symId, enumDecl.info)
  inc enumDecl # skips enum
  while enumDecl.kind != ParRi:
    dest.add tagToken("of", enumDecl.info)

    dest.add tagToken("set", enumDecl.info)
    let field = asEnumField(enumDecl)
    let symId = field.name.symId
    var val = field.val
    inc val # skips tupleConstr
    inc val # skips counter field
    let fieldValue = val.litId
    dest.add symToken(symId, field.name.info)
    dest.addParRi() # set

    dest.add tagToken("stmts", enumDecl.info)
    dest.add tagToken("ret", enumDecl.info)
    dest.add strToken(fieldValue, enumDecl.info)
    dest.addParRi() # stmts

    dest.addParRi() # of

  dest.addParRi() # case

proc genEnumToStrProc*(typeDecl: var Cursor): TokenBuf =
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

  result.add tagToken("string", NoLineInfo)

  result.addDotToken()
  result.addDotToken()


  result.add tagToken("stmts", typeDecl.info)
  var body = decl.body
  genEnumToStrProcCase(result, body, paramSymId)
  result.addParRi()

  result.addParRi()
  result.addParRi()

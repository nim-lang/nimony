#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Generation for enum to string conversions.

import std / [tables, sets, hashes, assertions]
include ".." / lib / nifprelude
include ".." / lib / compat2

import decls, nimony_model, semdata, sembasics, symtabs, programs

proc genEnumToStrProcCase(c: var SemContext; dest: var TokenBuf; enumDecl: var Cursor; symId, enumSymId: SymId) =
  dest.addParLe("case", enumDecl.info)
  dest.addSymUse(symId, enumDecl.info)
  enumDecl.into: # enum
    skip enumDecl # skips base type
    while enumDecl.hasMore:
      let enumDeclInfo = enumDecl.info
      dest.addParLe("of", enumDeclInfo)

      dest.addParLe("ranges", enumDeclInfo)

      var symId: SymId
      var symInfo: PackedLineInfo
      var fieldValue: Cursor
      enumDecl.into: # efld
        symId = enumDecl.symId
        symInfo = enumDecl.info
        inc enumDecl                # past name (SymbolDef)
        skip enumDecl, SkipExport   # export marker
        skip enumDecl, SkipPragmas  # pragmas
        skip enumDecl, SkipType     # type

        enumDecl.into: # tupleConstr
          skip enumDecl # skips counter field
          fieldValue = enumDecl
          skip enumDecl # skips string value

      while enumDecl.hasMore and enumDecl.isTagLit and enumDecl.tagId == nifpools.ErrT:
        skip enumDecl

      dest.addSymUse(symId, symInfo)
      dest.addParRi() # ranges

      dest.addParLe("stmts", enumDeclInfo)
      dest.addParLe("ret", enumDeclInfo)
      if fieldValue.isStringLit:
        dest.addStrLit(fieldValue.litId, enumDeclInfo)
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

  dest.addParLe("func", enumSymInfo)
  dest.addSymDef(dollorSymId, enumSymInfo)

  # TODO: defaults to (nodecl)
  # TODO: static for local functions
  if c.currentScope.kind == ToplevelScope:
    let exportIdent = pool.strings.getOrIncl("x")
    dest.addIdent(exportIdent, enumSymInfo)
  else:
    dest.addDotToken() # exportIdent
  dest.addDotToken()
  dest.addDotToken()

  var paramName = "e"
  c.makeLocalSym(paramName)
  let paramSymId = pool.syms.getOrIncl(paramName)
  dest.addParLe("params", enumSymInfo)
  dest.addParLe("param", enumSymInfo)
  dest.addSymDef(paramSymId, enumSymInfo)
  dest.addDotToken()
  dest.addDotToken()
  dest.addSymUse(enumSymId, enumSymInfo)
  dest.addDotToken()
  dest.addParRi() # param
  dest.addParRi() # params

  dest.addSubtree c.types.stringType

  dest.addDotToken()
  dest.addDotToken()

  dest.addParLe("stmts", enumSymInfo)
  var body = decl.body
  genEnumToStrProcCase(c, dest, body, paramSymId, enumSymId)
  dest.addParRi() # stmts

  dest.addParRi() # func

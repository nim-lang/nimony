# NIF <-> NimNode conversion (host-Nim only).
# Included from lib/std/macros.nim under `when not defined(nimony):`.
# All forward-declared procs in that file are defined here.

# Mapping from NIF tags to NimNodeKind
proc tagToNimNodeKind(tag: string): NimNodeKind =
  case tag
  of "call": nnkCall
  of "cmd": nnkCommand
  of "callstrlit": nnkCallStrLit
  of "infix": nnkInfix
  of "prefix": nnkPrefix
  of "par": nnkPar
  of "bracket": nnkBracket
  of "curly": nnkCurly
  of "tupconstr": nnkTupleConstr
  of "oconstr": nnkObjConstr
  of "tabconstr": nnkTableConstr
  of "at": nnkBracketExpr
  of "curlyat": nnkCurlyExpr
  of "dot": nnkDotExpr
  of "deref": nnkDerefExpr
  of "hderef": nnkHiddenDeref
  of "addr": nnkAddr
  of "haddr": nnkHiddenAddr
  of "cast": nnkCast
  of "conv", "hconv", "dconv": nnkConv
  of "kv": nnkExprColonExpr
  of "vv": nnkExprEqExpr
  of "quoted": nnkAccQuoted
  of "cchoice": nnkClosedSymChoice
  of "ochoice": nnkOpenSymChoice
  of "nil": nnkNilLit
  of "true", "false": nnkIdent  # handled specially
  of "do": nnkDo
  of "range": nnkRange
  # Statements
  of "stmts": nnkStmtList
  of "asgn": nnkAsgn
  of "var": nnkVarSection
  of "let": nnkLetSection
  of "const": nnkConstSection
  of "gvar", "tvar": nnkVarSection
  of "glet", "tlet": nnkLetSection
  of "if": nnkIfStmt
  of "when": nnkWhenStmt
  of "elif": nnkElifBranch
  of "else": nnkElse
  of "case": nnkCaseStmt
  of "of": nnkOfBranch
  of "while": nnkWhileStmt
  of "for": nnkForStmt
  of "try": nnkTryStmt
  of "except": nnkExceptBranch
  of "fin": nnkFinally
  of "ret": nnkReturnStmt
  of "break": nnkBreakStmt
  of "continue": nnkContinueStmt
  of "yld": nnkYieldStmt
  of "raise": nnkRaiseStmt
  of "discard": nnkDiscardStmt
  of "block": nnkBlockStmt
  of "staticstmt": nnkStaticStmt
  of "defer": nnkDefer
  of "asm": nnkAsm
  of "comment": nnkCommentStmt
  # Declarations
  of "func": nnkFuncDef
  of "method": nnkMethodDef
  of "iterator": nnkIteratorDef
  of "macro": nnkMacroDef
  of "template": nnkTemplateDef
  of "converter": nnkConverterDef
  of "type": nnkTypeSection
  of "params": nnkFormalParams
  of "param": nnkIdentDefs
  of "typevars": nnkGenericParams
  of "typevar": nnkIdentDefs
  of "pragmas": nnkPragma
  of "pragmax": nnkPragmaExpr
  # Types
  of "object": nnkObjectTy
  of "tuple": nnkTupleTy
  of "enum": nnkEnumTy
  of "efld": nnkEnumFieldDef
  of "fld": nnkIdentDefs
  of "distinct": nnkDistinctTy
  of "ref": nnkRefTy
  of "ptr": nnkPtrTy
  of "mut": nnkVarTy
  of "proctype": nnkProcTy
  of "itertype": nnkIteratorTy
  # Imports/Exports
  of "import": nnkImportStmt
  of "export": nnkExportStmt
  of "include": nnkIncludeStmt
  of "fromimport": nnkFromImport
  of "importexcept": nnkImportExcept
  of "bind": nnkBind
  of "mixin": nnkMixin
  of "using": nnkUsing
  # Errors
  of "err": nnkError
  else: nnkNone

proc nimNodeKindToTag(k: NimNodeKind): string =
  case k
  of nnkNone, nnkEmpty: "."  # dot token
  of nnkIdent: ""  # handled as ident token
  of nnkSym: ""    # handled as symbol token
  of nnkClosedSymChoice: "cchoice"
  of nnkOpenSymChoice: "ochoice"
  of nnkIntLit..nnkUInt64Lit: ""  # handled as literal
  of nnkFloatLit..nnkFloat64Lit: ""
  of nnkStrLit, nnkRStrLit, nnkTripleStrLit: ""
  of nnkCharLit: ""
  of nnkNilLit: "nil"
  of nnkCall: "call"
  of nnkCommand: "cmd"
  of nnkCallStrLit: "callstrlit"
  of nnkInfix: "infix"
  of nnkPrefix: "prefix"
  of nnkPostfix: "prefix"  # no direct equivalent
  of nnkHiddenCallConv: "hcall"
  of nnkExprEqExpr: "vv"
  of nnkExprColonExpr: "kv"
  of nnkPar: "par"
  of nnkBracket: "bracket"
  of nnkCurly: "curly"
  of nnkTupleConstr: "tupconstr"
  of nnkObjConstr: "oconstr"
  of nnkTableConstr: "tabconstr"
  of nnkBracketExpr: "at"
  of nnkCurlyExpr: "curlyat"
  of nnkDotExpr: "dot"
  of nnkDerefExpr: "deref"
  of nnkHiddenDeref: "hderef"
  of nnkAddr: "addr"
  of nnkHiddenAddr: "haddr"
  of nnkCast: "cast"
  of nnkConv, nnkHiddenStdConv, nnkHiddenSubConv: "conv"
  of nnkIfExpr, nnkIfStmt: "if"
  of nnkWhenExpr, nnkWhenStmt: "when"
  of nnkStmtList, nnkStmtListExpr: "stmts"
  of nnkBlockExpr, nnkBlockStmt: "block"
  of nnkAsgn, nnkFastAsgn: "asgn"
  of nnkVarSection: "var"
  of nnkLetSection: "let"
  of nnkConstSection: "const"
  of nnkIdentDefs, nnkVarTuple: "param"  # close enough
  of nnkElifBranch: "elif"
  of nnkElse: "else"
  of nnkCaseStmt: "case"
  of nnkOfBranch: "of"
  of nnkWhileStmt: "while"
  of nnkForStmt: "for"
  of nnkTryStmt: "try"
  of nnkExceptBranch: "except"
  of nnkFinally: "fin"
  of nnkReturnStmt: "ret"
  of nnkBreakStmt: "break"
  of nnkContinueStmt: "continue"
  of nnkYieldStmt: "yld"
  of nnkRaiseStmt: "raise"
  of nnkDiscardStmt: "discard"
  of nnkProcDef: "func"
  of nnkFuncDef: "func"
  of nnkMethodDef: "method"
  of nnkIteratorDef: "iterator"
  of nnkMacroDef: "macro"
  of nnkTemplateDef: "template"
  of nnkConverterDef: "converter"
  of nnkFormalParams: "params"
  of nnkGenericParams: "typevars"
  of nnkPragma: "pragmas"
  of nnkPragmaExpr: "pragmax"
  of nnkTypeSection: "type"
  of nnkTypeDef: "type"
  of nnkObjectTy: "object"
  of nnkTupleTy: "tuple"
  of nnkEnumTy: "enum"
  of nnkEnumFieldDef: "efld"
  of nnkRecList, nnkRecCase, nnkRecWhen: "stmts"
  of nnkDistinctTy: "distinct"
  of nnkRefTy: "ref"
  of nnkPtrTy: "ptr"
  of nnkVarTy: "mut"
  of nnkProcTy: "proctype"
  of nnkIteratorTy: "itertype"
  of nnkRange: "range"
  of nnkImportStmt: "import"
  of nnkExportStmt: "export"
  of nnkIncludeStmt: "include"
  of nnkFromImport: "fromimport"
  of nnkImportExcept: "importexcept"
  of nnkBind: "bind"
  of nnkMixin: "mixin"
  of nnkUsing: "using"
  of nnkCommentStmt: "comment"
  of nnkStaticStmt: "staticstmt"
  of nnkDefer: "defer"
  of nnkAsm: "asm"
  of nnkDo: "do"
  of nnkAccQuoted: "quoted"
  of nnkError: "err"
  of nnkType: "typedesc"

proc fromNifToken(r: var Reader; t: ExpandedToken): NimNode =
  ## Helper to convert a single already-read token
  case t.tk
  of EofToken:
    result = nil
  of DotToken:
    result = newEmptyNode()
  of Ident:
    result = newIdentNode(r.decodeStr(t))
  of Symbol, SymbolDef:
    result = newNimNode(nnkSym)
    result.strValField = r.decodeStr(t)
  of IntLit:
    result = newIntLitNode(decodeInt(t))
  of UIntLit:
    result = newNimNode(nnkUIntLit)
    result.intValField = BiggestInt(decodeUInt(t))
  of FloatLit:
    result = newFloatLitNode(decodeFloat(t))
  of StringLit:
    result = newStrLitNode(r.decodeStr(t))
  of CharLit:
    result = newNimNode(nnkCharLit)
    result.intValField = BiggestInt(decodeChar(t))
  of ParLe:
    let tag = $t.data
    # Special cases for keywords that are leaf nodes
    if tag == "true":
      result = newIdentNode("true")
      discard r.next()  # skip ParRi
    elif tag == "false":
      result = newIdentNode("false")
      discard r.next()  # skip ParRi
    elif tag == "nil":
      result = newNilLit()
      discard r.next()  # skip ParRi
    elif tag == "inf":
      result = newFloatLitNode(1.0 / 0.0)
      discard r.next()
    elif tag == "neginf":
      result = newFloatLitNode(-1.0 / 0.0)
      discard r.next()
    elif tag == "nan":
      result = newFloatLitNode(0.0 / 0.0)
      discard r.next()
    else:
      let kind = tagToNimNodeKind(tag)
      result = newNimNode(kind)
      # Read children until ParRi
      while true:
        let child = r.next()
        if child.tk == ParRi:
          break
        elif child.tk == EofToken:
          break
        else:
          result.add fromNifToken(r, child)
  of ParRi:
    result = newEmptyNode()
  of UnknownToken:
    result = newNimNode(nnkError)

proc fromNif*(r: var Reader): NimNode =
  ## Convert NIF stream to NimNode tree
  let t = r.next()
  result = fromNifToken(r, t)

proc toNif*(b: var Builder; n: NimNode) =
  ## Convert NimNode tree to NIF stream
  if isNilNode(n):
    b.addEmpty()
    return

  case n.kind
  of nnkNone, nnkEmpty:
    b.addEmpty()
  of nnkIdent:
    b.addIdent(n.strValField)
  of nnkSym:
    b.addSymbol(n.strValField)
  of nnkIntLit:
    b.addIntLit(n.intValField)
  of nnkInt8Lit, nnkInt16Lit, nnkInt32Lit, nnkInt64Lit:
    b.addIntLit(n.intValField)
  of nnkUIntLit, nnkUInt8Lit, nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit:
    b.addUIntLit(uint(n.intValField))
  of nnkFloatLit, nnkFloat32Lit, nnkFloat64Lit:
    b.addFloatLit(n.floatValField)
  of nnkStrLit, nnkRStrLit, nnkTripleStrLit:
    b.addStrLit(n.strValField)
  of nnkCharLit:
    b.addCharLit(char(n.intValField))
  of nnkNilLit:
    b.withTree "nil":
      discard
  else:
    let tag = nimNodeKindToTag(n.kind)
    if tag.len > 0:
      b.withTree tag:
        for c in n.kids:
          b.toNif(c)
    else:
      # Fallback: use stmts
      b.withTree "stmts":
        for c in n.kids:
          b.toNif(c)

# ============================================================================
# Macro plugin support
# ============================================================================

proc loadInput*(): NimNode =
  ## Load the input NIF file (first command line argument)
  var r = nifreader.open(paramStr(1))
  result = fromNif(r)
  nifreader.close(r)

proc saveOutput*(n: NimNode) =
  ## Save the output NIF file (second command line argument)
  var b = nifbuilder.open(paramStr(2))
  b.toNif(n)
  nifbuilder.close(b)

# Convenience template for macro plugins
template macroMain*(body: untyped) =
  when isMainModule:
    let input = loadInput()
    let output = body
    saveOutput(output)

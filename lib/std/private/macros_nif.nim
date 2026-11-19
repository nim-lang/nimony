# NIF <-> NimNode conversion, included from lib/std/macros.nim.

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
  of "expr": nnkStmtListExpr
  of "unpackflat": nnkVarTuple
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
  of "proc": nnkProcDef
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
  of nnkStmtList: "stmts"
  of nnkStmtListExpr: "expr"
  of nnkBlockExpr, nnkBlockStmt: "block"
  of nnkAsgn, nnkFastAsgn: "asgn"
  of nnkVarSection: "var"
  of nnkLetSection: "let"
  of nnkConstSection: "const"
  of nnkIdentDefs: "param"  # close enough — unwrapped in var/let parents
  of nnkVarTuple: "unpackflat"
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
  of nnkProcDef: "proc"
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
  of nnkRecList: "stmts"
  of nnkRecCase: "case"
  of nnkRecWhen: "when"
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

proc litSuffixKind(lit: NimNodeKind; suffix: string): NimNodeKind =
  ## The literal kind of Nifler's `(suf <lit> "<suffix>")`; `nnkNone` for a
  ## suffix that has no kind of its own (a user-defined one).
  result = nnkNone
  case lit
  of nnkStrLit:
    if suffix == "R": result = nnkRStrLit
    elif suffix == "T": result = nnkTripleStrLit
  of nnkIntLit, nnkUIntLit:
    case suffix
    of "i8": result = nnkInt8Lit
    of "i16": result = nnkInt16Lit
    of "i32": result = nnkInt32Lit
    of "i64": result = nnkInt64Lit
    of "u": result = nnkUIntLit
    of "u8": result = nnkUInt8Lit
    of "u16": result = nnkUInt16Lit
    of "u32": result = nnkUInt32Lit
    of "u64": result = nnkUInt64Lit
    else: discard
  of nnkFloatLit:
    if suffix == "f32": result = nnkFloat32Lit
    elif suffix == "f64": result = nnkFloat64Lit
  else: discard

proc litSuffix(k: NimNodeKind): string =
  ## The Nifler suffix that keeps a typed literal's type; `""` for none.
  case k
  of nnkInt8Lit: "i8"
  of nnkInt16Lit: "i16"
  of nnkInt32Lit: "i32"
  of nnkInt64Lit: "i64"
  of nnkUInt8Lit: "u8"
  of nnkUInt16Lit: "u16"
  of nnkUInt32Lit: "u32"
  of nnkUInt64Lit: "u64"
  of nnkFloat32Lit: "f32"
  of nnkFloat64Lit: "f64"
  else: ""

proc fromSuffixed(n: NimNode): NimNode =
  ## `(suf "x" "T")` is the literal `"""x"""`, `(suf 3 "u8")` is `3'u8`, ...
  result = n
  if n.len == 2 and n.kids[1].kind == nnkStrLit:
    let lit = n.kids[0]
    let k = litSuffixKind(lit.kind, n.kids[1].strValField)
    if k != nnkNone:
      result = newNimNode(k)
      result.strValField = lit.strValField
      result.intValField = lit.intValField
      result.floatValField = lit.floatValField

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
  of TagLit, ExtendedSuffix, LineInfoLit:
    # in-memory-only kinds; the reader's lexical token stream never yields them
    result = nil
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
      result.tagField = tag
      # Read children until ParRi
      while true:
        let child = r.next()
        if child.tk == ParRi:
          break
        elif child.tk == EofToken:
          break
        else:
          result.add fromNifToken(r, child)
      if tag == "suf":
        result = fromSuffixed(result)
  of ParRi:
    result = newEmptyNode()
  of UnknownToken:
    result = newNimNode(nnkError)

proc fromNif*(r: var Reader): NimNode =
  ## Convert NIF stream to NimNode tree
  let t = r.next()
  result = fromNifToken(r, t)

proc treeTag(n: NimNode; inObject: bool): string =
  ## The tag `n` was read with while its kind still fits it, else the tag of
  ## its kind; a macro-built `nnkIdentDefs` is a field inside an object type.
  if n.tagField.len > 0 and tagToNimNodeKind(n.tagField) == n.kind:
    result = n.tagField
  elif n.kind == nnkIdentDefs and inObject:
    result = "fld"
  else:
    result = nimNodeKindToTag(n.kind)

proc toNif(b: var Builder; n: NimNode; inObject: bool)

proc toNifKids(b: var Builder; n: NimNode; tag: string; inObject: bool) =
  # Fields only live directly in an object or tuple type, or in the
  # `case`/`when`/`stmts` nested inside one; anything else starts over.
  let kidsInObject =
    case tag
    of "object", "tuple": true
    of "case", "of", "when", "elif", "else", "stmts", "fld": inObject
    else: false
  b.withTree tag:
    for i in 0 ..< n.kids.len:
      b.toNif(n.kids[i], kidsInObject)

proc toNif(b: var Builder; n: NimNode; inObject: bool) =
  if isNilNode(n):
    b.addEmpty()
    return

  case n.kind
  of nnkNone, nnkEmpty:
    if n.tagField.len > 0:
      # a tag without a `NimNodeKind` of its own, such as `ranges`
      b.toNifKids(n, n.tagField, inObject)
    else:
      b.addEmpty()
  of nnkIdent:
    b.addIdent(n.strValField)
  of nnkSym:
    b.addSymbol(n.strValField)
  of nnkIntLit:
    b.addIntLit(n.intValField)
  of nnkUIntLit:
    b.addUIntLit(uint(n.intValField))
  of nnkFloatLit:
    b.addFloatLit(n.floatValField)
  of nnkInt8Lit, nnkInt16Lit, nnkInt32Lit, nnkInt64Lit:
    b.withTree "suf":
      b.addIntLit(n.intValField)
      b.addStrLit(litSuffix(n.kind))
  of nnkUInt8Lit, nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit:
    b.withTree "suf":
      b.addUIntLit(uint(n.intValField))
      b.addStrLit(litSuffix(n.kind))
  of nnkFloat32Lit, nnkFloat64Lit:
    b.withTree "suf":
      b.addFloatLit(n.floatValField)
      b.addStrLit(litSuffix(n.kind))
  of nnkStrLit, nnkRStrLit, nnkTripleStrLit:
    b.addStrLit(n.strValField)
  of nnkCharLit:
    b.addCharLit(char(n.intValField))
  of nnkNilLit:
    b.withTree "nil":
      discard
  of nnkVarSection, nnkLetSection, nnkConstSection:
    # NIF expects var/let/const children to be inline:
    #   (var :name . . type value)        -- 5 slots: name, export, pragma, type, value
    # not Nim's wrapped form:
    #   (var (identdefs name type value)) -- 3 slots in IdentDefs: name, type, value
    # Unwrap each nnkIdentDefs and pad with two empty slots (export marker
    # and pragma) so the result matches what nifler emits.
    let tag = nimNodeKindToTag(n.kind)
    b.withTree tag:
      for i in 0 ..< n.kids.len:
        let c = n.kids[i]
        if c.kind == nnkIdentDefs and c.len == 3:
          # name
          b.toNif(c.kids[0], false)
          # export marker (none)
          b.addEmpty()
          # pragmas (none)
          b.addEmpty()
          # type
          b.toNif(c.kids[1], false)
          # value
          b.toNif(c.kids[2], false)
        else:
          b.toNif(c, false)
  else:
    let tag = treeTag(n, inObject)
    # an empty tag is a kind without a tree of its own: fall back to `stmts`
    b.toNifKids(n, if tag.len > 0: tag else: "stmts", inObject)

proc toNif*(b: var Builder; n: NimNode) =
  ## Convert NimNode tree to NIF stream
  b.toNif(n, false)

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

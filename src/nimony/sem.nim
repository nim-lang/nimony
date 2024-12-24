#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Semantic checking:
## Most important task is to turn identifiers into symbols and to perform
## type checking.

import std / [tables, sets, syncio, formatfloat, assertions, packedsets]
include nifprelude
import nimony_model, symtabs, builtintypes, decls, symparser, asthelpers,
  programs, sigmatch, magics, reporters, nifconfig, nifindexes,
  intervals, xints, typeprops,
  semdata, sembasics, semos, expreval, semborrow

import ".." / gear2 / modnames

# ------------------ include/import handling ------------------------

proc semStmt(c: var SemContext; n: var Cursor)

proc typeMismatch(c: var SemContext; info: PackedLineInfo; got, expected: TypeCursor) =
  c.buildErr info, "type mismatch: got: " & typeToString(got) & " but wanted: " & typeToString(expected)

proc typecheck(c: var SemContext; info: PackedLineInfo; got, expected: TypeCursor) =
  if sameTrees(expected, got):
    discard "fine"
  else:
    c.typeMismatch info, got, expected

proc combineType(c: var SemContext; info: PackedLineInfo; dest: var Cursor; src: Cursor) =
  if typeKind(dest) == AutoT:
    dest = src
  elif sameTrees(dest, src):
    discard "fine"
  else:
    c.typeMismatch info, src, dest

proc implicitlyDiscardable(n: Cursor, noreturnOnly = false): bool =
  template checkBranch(branch) =
    if not implicitlyDiscardable(branch, noreturnOnly):
      return false

  var it = n
  #const
  #  skipForDiscardable = {nkStmtList, nkStmtListExpr,
  #    nkOfBranch, nkElse, nkFinally, nkExceptBranch,
  #    nkElifBranch, nkElifExpr, nkElseExpr, nkBlockStmt, nkBlockExpr,
  #    nkHiddenStdConv, nkHiddenSubConv, nkHiddenDeref}
  while it.kind == ParLe and stmtKind(it) in {StmtsS, BlockS}:
    inc it
    var last = it
    while true:
      skip it
      if it.kind == ParRi:
        it = last
        break
      else:
        last = it

  if it.kind != ParLe: return false
  case stmtKind(it)
  of IfS:
    inc it
    while it.kind != ParRi:
      case it.substructureKind
      of ElifS:
        inc it
        skip it # condition
        checkBranch(it)
        skip it
        skipParRi it
      of ElseS:
        inc it
        checkBranch(it)
        skip it
        skipParRi it
      else:
        error "illformed AST: `elif` or `else` inside `if` expected, got ", it
    # all branches are discardable
    result = true
  of CaseS:
    inc it
    while it.kind != ParRi:
      case it.substructureKind
      of OfS:
        inc it
        skip it # ranges
        checkBranch(it)
        skip it
        skipParRi it
      of ElifS:
        inc it
        skip it # condition
        checkBranch(it)
        skip it
        skipParRi it
      of ElseS:
        inc it
        checkBranch(it)
        skip it
        skipParRi it
      else:
        error "illformed AST: `of`, `elif` or `else` inside `case` expected, got ", it
    # all branches are discardable
    result = true
  #of TryS:
  #  checkBranch(it[0])
  #  for i in 1 ..< it.len:
  #    let branch = it[i]
  #    if branch.kind != nkFinally:
  #      checkBranch(branch[^1])
  #  # all branches are discardable
  #  result = true
  of CallS, CmdS:
    inc it
    if it.kind == Symbol:
      let sym = tryLoadSym(it.symId)
      if sym.status == LacksNothing:
        var decl = sym.decl
        if isRoutine(symKind(decl)):
          inc decl
          skip decl # name
          skip decl # exported
          skip decl # pattern
          skip decl # typevars
          skip decl # params
          skip decl # retType
          # decl should now be pragmas:
          inc decl
          let accepted =
            if noreturnOnly: {NoReturn}
            else: {Discardable, NoReturn}
          while decl.kind != ParRi:
            if pragmaKind(decl) in accepted:
              return true
            skip decl
    result = false
  of RetS, BreakS, ContinueS: # XXX also `raise`
    result = true
  else:
    result = false

proc isNoReturn(n: Cursor): bool {.inline.} =
  result = implicitlyDiscardable(n, noreturnOnly = true)

proc commonType(c: var SemContext; it: var Item; argBegin: int; expected: TypeCursor) =
  if typeKind(expected) == AutoT:
    return
  elif typeKind(it.typ) == AutoT:
    it.typ = expected
    return

  let info = it.n.info
  var m = createMatch(addr c)
  var arg = Item(n: cursorAt(c.dest, argBegin), typ: it.typ)
  if typeKind(arg.typ) == VoidT and isNoReturn(arg.n):
    # noreturn allowed in expression context
    # maybe use sem flags to restrict this to statement branches
    discard
  else:
    typematch m, expected, arg
  endRead(c.dest)
  if m.err:
    c.typeMismatch info, it.typ, expected
  else:
    shrink c.dest, argBegin
    c.dest.add m.args
    it.typ = expected

proc producesVoid(c: var SemContext; info: PackedLineInfo; dest: var Cursor) =
  if typeKind(dest) in {AutoT, VoidT}:
    combineType c, info, dest, c.types.voidType
  else:
    c.typeMismatch info, c.types.voidType, dest

proc producesNoReturn(c: var SemContext; info: PackedLineInfo; dest: var Cursor) =
  if typeKind(dest) in {AutoT, VoidT}:
    combineType c, info, dest, c.types.voidType
  else:
    # allowed in expression context
    discard

proc semInclude(c: var SemContext; it: var Item) =
  var files: seq[ImportedFilename] = @[]
  var hasError = false
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `include`
  while x.kind != ParRi:
    filenameVal(x, files, hasError, allowAs = false)

  if hasError:
    c.buildErr info, "wrong `include` statement"
  else:
    for f1 in items(files):
      let f2 = resolveFile(c.g.config.paths, getFile(info), f1.path)
      c.meta.includedFiles.add f2
      # check for recursive include files:
      var isRecursive = false
      for a in c.includeStack:
        if a == f2:
          isRecursive = true
          break

      if not isRecursive:
        var buf = parseFile(f2, c.g.config.paths)
        c.includeStack.add f2
        #c.m.includes.add f2
        var n = cursorAt(buf, 0)
        semStmt(c, n)
        c.includeStack.setLen c.includeStack.len - 1
      else:
        var m = ""
        for i in 0..<c.includeStack.len:
          m.add shortenDir c.includeStack[i]
          m.add " -> "
        m.add shortenDir f2
        c.buildErr info, "recursive include: " & m

  producesVoid c, info, it.typ

type
  ImportModeKind = enum
    ImportAll, FromImport, ImportExcept

  ImportMode = object
    kind: ImportModeKind
    list: PackedSet[StrId] # `from import` or `import except` symbol list

proc importSingleFile(c: var SemContext; f1: ImportedFilename; origin: string; mode: ImportMode; info: PackedLineInfo) =
  let f2 = resolveFile(c.g.config.paths, origin, f1.path)
  let suffix = moduleSuffix(f2, c.g.config.paths)
  if not c.processedModules.containsOrIncl(suffix):
    c.meta.importedFiles.add f2
    if c.canSelfExec and needsRecompile(f2, suffix):
      selfExec c, f2

    let moduleName = pool.strings.getOrIncl(f1.name)
    let moduleSym = identToSym(c, moduleName, ModuleY)
    let s = Sym(kind: ModuleY, name: moduleSym, pos: ImportedPos)
    c.currentScope.addOverloadable(moduleName, s)
    var moduleDecl = createTokenBuf(2)
    moduleDecl.addParLe(ModuleY, info)
    moduleDecl.addParRi()
    publish moduleSym, moduleDecl
    var module = ImportedModule()
    var marker = mode.list
    loadInterface suffix, module.iface, moduleSym, c.importTab,
      marker, negateMarker = mode.kind == FromImport
    c.importedModules[moduleSym] = module

proc cyclicImport(c: var SemContext; x: var Cursor) =
  c.buildErr x.info, "cyclic module imports are not implemented"

proc doImportMode(c: var SemContext; files: seq[ImportedFilename]; mode: ImportMode; info: PackedLineInfo) =
  let origin = getFile(info)
  for f in files:
    importSingleFile c, f, origin, mode, info

proc semImport(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `import`

  if x.kind == ParLe and x == "pragmax":
    inc x
    var y = x
    skip y
    if y.substructureKind == PragmasS:
      inc y
      if y.kind == Ident and pool.strings[y.litId] == "cyclic":
        cyclicImport(c, x)
        return

  var files: seq[ImportedFilename] = @[]
  var hasError = false
  while x.kind != ParRi:
    filenameVal(x, files, hasError, allowAs = true)
  if hasError:
    c.buildErr info, "wrong `import` statement"
  else:
    doImportMode c, files, ImportMode(kind: ImportAll, list: initPackedSet[StrId]()), info

  producesVoid c, info, it.typ

proc semImportExcept(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `importexcept`

  if x.kind == ParLe and x == "pragmax":
    inc x
    var y = x
    skip y
    if y.substructureKind == PragmasS:
      inc y
      if y.kind == Ident and pool.strings[y.litId] == "cyclic":
        cyclicImport(c, x)
        return

  var files: seq[ImportedFilename] = @[]
  var hasError = false
  filenameVal(x, files, hasError, allowAs = true)
  if hasError:
    c.buildErr info, "wrong `import except` statement"
  else:
    var excluded = initPackedSet[StrId]()
    while x.kind != ParRi:
      excluded.incl getIdent(x)
    doImportMode c, files, ImportMode(kind: ImportExcept, list: excluded), info

  producesVoid c, info, it.typ

proc semFromImport(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `from`

  if x.kind == ParLe and x == "pragmax":
    inc x
    var y = x
    skip y
    if y.substructureKind == PragmasS:
      inc y
      if y.kind == Ident and pool.strings[y.litId] == "cyclic":
        cyclicImport(c, x)
        return

  var files: seq[ImportedFilename] = @[]
  var hasError = false
  filenameVal(x, files, hasError, allowAs = true)
  if hasError:
    c.buildErr info, "wrong `from import` statement"
  else:
    var included = initPackedSet[StrId]()
    while x.kind != ParRi:
      if x.kind == ParLe and x == $NilX:
        # from a import nil
        discard
      else:
        included.incl getIdent(x)
    doImportMode c, files, ImportMode(kind: FromImport, list: included), info

  producesVoid c, info, it.typ

# -------------------- declare `result` -------------------------

proc classifyType(c: var SemContext; n: Cursor): TypeKind =
  result = typeKind(n)

proc declareResult(c: var SemContext; info: PackedLineInfo): SymId =
  if c.routine.kind in {ProcY, FuncY, ConverterY, MethodY, MacroY} and
      classifyType(c, c.routine.returnType) != VoidT:
    let name = pool.strings.getOrIncl("result")
    result = identToSym(c, name, ResultY)
    let s = Sym(kind: ResultY, name: result,
                pos: c.dest.len)
    discard c.currentScope.addNonOverloadable(name, s)
    c.routine.resId = result

    let declStart = c.dest.len
    buildTree c.dest, ResultS, info:
      c.dest.add symdefToken(result, info) # name
      c.dest.addDotToken() # export marker
      c.dest.addDotToken() # pragmas
      # XXX ^ pragma should be `.noinit` if the proc decl has it
      c.dest.copyTree(c.routine.returnType) # type
      c.dest.addDotToken() # value
    publish c, result, declStart
  else:
    result = SymId(0)

# -------------------- generics ---------------------------------

proc newSymId(c: var SemContext; s: SymId): SymId =
  var isGlobal = false
  var name = extractBasename(pool.syms[s], isGlobal)
  if isGlobal:
    c.makeGlobalSym(name)
  else:
    c.makeLocalSym(name)
  result = pool.syms.getOrIncl(name)

type
  SubsContext = object
    newVars: Table[SymId, SymId]
    params: ptr Table[SymId, Cursor]

proc subs(c: var SemContext; dest: var TokenBuf; sc: var SubsContext; body: Cursor) =
  var nested = 0
  var n = body
  while true:
    case n.kind
    of UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
      dest.add n
    of Symbol:
      let s = n.symId
      let arg = sc.params[].getOrDefault(s)
      if arg != default(Cursor):
        dest.addSubtree arg
      else:
        let nv = sc.newVars.getOrDefault(s)
        if nv != SymId(0):
          dest.add symToken(nv, n.info)
        else:
          dest.add n # keep Symbol as it was
    of SymbolDef:
      let s = n.symId
      let newDef = newSymId(c, s)
      sc.newVars[s] = newDef
      dest.add symdefToken(newDef, n.info)
    of ParLe:
      dest.add n
      inc nested
    of ParRi:
      dest.add n
      dec nested
    if nested == 0: break
    inc n

include templates

proc produceInvoke(c: var SemContext; dest: var TokenBuf; req: InstRequest;
                   typeVars: Cursor; info: PackedLineInfo) =
  dest.buildTree InvokeT, info:
    dest.add symToken(req.origin, info)
    var typeVars = typeVars
    if typeVars.substructureKind == TypevarsS:
      inc typeVars
      while typeVars.kind != ParRi:
        if typeVars.symKind == TypeVarY:
          var tv = typeVars
          inc tv
          dest.copyTree req.inferred[tv.symId]
        skip typeVars

proc subsGenericType(c: var SemContext; dest: var TokenBuf; req: InstRequest) =
  #[
  What we need to do is rather simple: A generic instantiation is
  the typical (type :Name ex generic_params pragmas body) tuple but
  this time the generic_params list the used `Invoke` construct for the
  instantiation.
  ]#
  let info = req.requestFrom[^1]
  let decl = getTypeSection(req.origin)
  dest.buildTree TypeS, info:
    dest.add symdefToken(req.targetSym, info)
    dest.addDotToken() # export
    produceInvoke c, dest, req, decl.typevars, info
    # take the pragmas from the origin:
    dest.copyTree decl.pragmas
    var sc = SubsContext(params: addr req.inferred)
    subs(c, dest, sc, decl.body)

proc subsGenericProc(c: var SemContext; dest: var TokenBuf; req: InstRequest) =
  let info = req.requestFrom[^1]
  let decl = getProcDecl(req.origin)
  dest.buildTree decl.kind, info:
    dest.add symdefToken(req.targetSym, info)
    if decl.exported.kind == ParLe:
      # magic?
      dest.copyTree decl.exported
    else:
      dest.addDotToken()
    dest.copyTree decl.pattern
    produceInvoke c, dest, req, decl.typevars, info

    var sc = SubsContext(params: addr req.inferred)
    subs(c, dest, sc, decl.params)
    subs(c, dest, sc, decl.retType)
    subs(c, dest, sc, decl.effects)
    subs(c, dest, sc, decl.pragmas)
    subs(c, dest, sc, decl.body)

template withFromInfo(req: InstRequest; body: untyped) =
  let oldLen = c.instantiatedFrom.len
  for jnfo in items(req.requestFrom):
    pushErrorContext c, jnfo
  body
  shrink c.instantiatedFrom, oldLen

type
  TypeDeclContext = enum
    InLocalDecl, InTypeSection, InObjectDecl, InParamDecl, InInheritanceDecl, InReturnTypeDecl, AllowValues,
    InGenericConstraint

proc semLocalTypeImpl(c: var SemContext; n: var Cursor; context: TypeDeclContext)

proc semLocalType(c: var SemContext; n: var Cursor; context = InLocalDecl): TypeCursor =
  let insertPos = c.dest.len
  semLocalTypeImpl c, n, context
  assert c.dest.len > insertPos
  result = typeToCursor(c, insertPos)

proc semTypeSection(c: var SemContext; n: var Cursor)
proc instantiateGenericType(c: var SemContext; req: InstRequest) =
  var dest = createTokenBuf(30)
  withFromInfo req:
    subsGenericType c, dest, req
    var n = beginRead(dest)
    semTypeSection c, n

type
  PassKind = enum checkSignatures, checkBody, checkGenericInst, checkConceptProc

proc semProc(c: var SemContext; it: var Item; kind: SymKind; pass: PassKind)
proc instantiateGenericProc(c: var SemContext; req: InstRequest) =
  var dest = createTokenBuf(40)
  withFromInfo req:
    subsGenericProc c, dest, req
    var it = Item(n: beginRead(dest), typ: c.types.autoType)
    #echo "now in generic proc: ", toString(it.n)
    semProc c, it, it.n.symKind, checkGenericInst

proc instantiateGenerics(c: var SemContext) =
  while c.typeRequests.len + c.procRequests.len > 0:
    # This way with `move` ensures it is safe even though
    # the semchecking of generics can add to `c.typeRequests`
    # or to `c.procRequests`. This is subtle!
    let typeReqs = move(c.typeRequests)
    for t in typeReqs: instantiateGenericType c, t
    let procReqs = move(c.procRequests)
    for p in procReqs: instantiateGenericProc c, p

# -------------------- sem checking -----------------------------

type
  SemFlag = enum
    KeepMagics
    PreferIterators
    AllowUndeclared
    AllowModuleSym

proc semExpr(c: var SemContext; it: var Item; flags: set[SemFlag] = {})

proc fetchSym(c: var SemContext; s: SymId): Sym =
  # yyy find a better solution
  var name = pool.syms[s]
  extractBasename name
  let identifier = pool.strings.getOrIncl(name)
  var it {.cursor.} = c.currentScope
  while it != nil:
    for sym in it.tab.getOrDefault(identifier):
      if sym.name == s:
        return sym
    it = it.up

  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    result = Sym(kind: symKind(res.decl), name: s, pos: ImportedPos)
  else:
    result = Sym(kind: NoSym, name: s, pos: InvalidPos)

proc semBoolExpr(c: var SemContext; n: var Cursor) =
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  if classifyType(c, it.typ) != BoolT:
    buildErr c, it.n.info, "expected `bool` but got: " & typeToString(it.typ)

proc semConstBoolExpr(c: var SemContext; n: var Cursor) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  if classifyType(c, it.typ) != BoolT:
    buildErr c, it.n.info, "expected `bool` but got: " & typeToString(it.typ)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstBoolValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, it.n.info, "expected constant bool value but got: " & toString(value, false)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstStrExpr(c: var SemContext; n: var Cursor) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  if classifyType(c, it.typ) != StringT:
    buildErr c, it.n.info, "expected `string` but got: " & typeToString(it.typ)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstStringValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, it.n.info, "expected constant string value but got: " & toString(value, false)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstIntExpr(c: var SemContext; n: var Cursor) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  if classifyType(c, it.typ) != IntT:
    buildErr c, it.n.info, "expected `int` but got: " & typeToString(it.typ)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstIntValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, it.n.info, "expected constant integer value but got: " & toString(value, false)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstExpr(c: var SemContext; it: var Item) =
  let start = c.dest.len
  semExpr c, it
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  c.dest.shrink start
  c.dest.add valueBuf

proc isLastSon(n: Cursor): bool =
  var n = n
  skip n
  result = n.kind == ParRi

proc semStmtsExprImpl(c: var SemContext; it: var Item) =
  while it.n.kind != ParRi:
    if not isLastSon(it.n):
      semStmt c, it.n
    else:
      semExpr c, it
  wantParRi c, it.n

proc semStmtsExpr(c: var SemContext; it: var Item) =
  takeToken c, it.n
  semStmtsExprImpl c, it

proc semProcBody(c: var SemContext; itB: var Item) =
  let beforeBodyPos = c.dest.len
  let info = itB.n.info
  var it = Item(n: itB.n, typ: c.types.autoType)
  semStmtsExprImpl c, it
  if c.routine.kind == TemplateY:
    if c.routine.returnType.typeKind == UntypedT:
      discard "ok"
    else:
      typecheck(c, info, it.typ, c.routine.returnType)
  elif classifyType(c, it.typ) == VoidT:
    discard "ok"
  else:
    typecheck(c, info, it.typ, c.routine.returnType)
    # transform `expr` to `result = expr`:
    if c.routine.resId != SymId(0):
      var prefix = [
        parLeToken(pool.tags.getOrIncl($AsgnS), info),
        symToken(c.routine.resId, info)]
      c.dest.insert prefix, beforeBodyPos
      c.dest.addParRi()
  itB.n = it.n

proc semStmt(c: var SemContext; n: var Cursor) =
  let info = n.info
  var it = Item(n: n, typ: c.types.autoType)
  let exPos = c.dest.len
  semExpr c, it
  if classifyType(c, it.typ) in {NoType, VoidT, AutoT}:
    discard "ok"
  else:
    # analyze the expression that was just produced:
    let ex = cursorAt(c.dest, exPos)
    let discardable = implicitlyDiscardable(ex)
    endRead(c.dest)
    if not discardable:
      buildErr c, info, "expression of type `" & typeToString(it.typ) & "` must be discarded"
  n = it.n

template emptyNode(): Cursor =
  # XXX find a better solution for this
  c.types.voidType

template skipToLocalType(n) =
  inc n # skip ParLe
  inc n # skip name
  skip n # skip export marker
  skip n # skip pragmas

template skipToParams(n) =
  inc n # skip ParLe
  skip n # skip name
  skip n # skip export marker
  skip n # skip pattern
  skip n # skip generics

proc fetchType(c: var SemContext; n: Cursor; s: Sym): TypeCursor =
  if s.kind == NoSym:
    c.buildErr n.info, "undeclared identifier"
    result = c.types.autoType
  else:
    let res = declToCursor(c, s)
    if res.status == LacksNothing:
      var d = res.decl
      if s.kind.isLocal:
        skipToLocalType d
      elif s.kind.isRoutine:
        skipToParams d
      else:
        # XXX enum field, object field?
        assert false, "not implemented"
      result = d
    else:
      c.buildErr n.info, "could not load symbol: " & pool.syms[s.name] & "; errorCode: " & $res.status
      result = c.types.autoType

proc pickBestMatch(c: var SemContext; m: openArray[Match]): int =
  result = -1
  var other = -1
  for i in 0..<m.len:
    if not m[i].err:
      if result < 0:
        result = i
      else:
        case cmpMatches(m[result], m[i])
        of NobodyWins:
          other = i
        of FirstWins:
          discard "result remains the same"
        of SecondWins:
          result = i
          other = -1
  if other >= 0: result = -2 # ambiguous

const
  ConceptProcY = CchoiceY

type MagicCallKind = enum
  NonMagicCall, MagicCall, MagicCallNeedsSemcheck

proc addFn(c: var SemContext; fn: FnCandidate; fnOrig: Cursor; args: openArray[Item]): MagicCallKind =
  result = NonMagicCall
  if fn.kind in RoutineKinds:
    assert fn.sym != SymId(0)
    let res = tryLoadSym(fn.sym)
    if res.status == LacksNothing:
      var n = res.decl
      inc n # skip the symbol kind
      if n.kind == SymbolDef:
        inc n # skip the SymbolDef
        if n.kind == ParLe:
          if n.exprKind in {DefinedX, DeclaredX, CompilesX, TypeofX,
              SizeofX, LowX, HighX, AddrX}:
            # magic needs semchecking after overloading
            result = MagicCallNeedsSemcheck
          else:
            result = MagicCall
          # ^ export marker position has a `(`? If so, it is a magic!
          copyKeepLineInfo c.dest[c.dest.len-1], n.load # overwrite the `(call` node with the magic itself
          inc n
          if n.kind == IntLit:
            if pool.integers[n.intId] == TypedMagic:
              c.dest.addSubtree args[0].typ
            else:
              c.dest.add n
            inc n
          if n.kind != ParRi:
            error "broken `magic`: expected ')', but got: ", n
    if result == NonMagicCall:
      c.dest.add symToken(fn.sym, fnOrig.info)
  elif fn.kind == ConceptProcY and fn.sym != SymId(0):
    c.dest.add identToken(symToIdent(fn.sym), fnOrig.info)
  else:
    c.dest.addSubtree fnOrig

proc semTemplateCall(c: var SemContext; it: var Item; fnId: SymId; beforeCall: int;
                     m: Match) =
  var expandedInto = createTokenBuf(30)

  let s = fetchSym(c, fnId)
  let res = declToCursor(c, s)
  if res.status == LacksNothing:
    let args = cursorAt(c.dest, beforeCall + 2)
    let firstVarargMatch = cursorAt(c.dest, beforeCall + 2 + m.firstVarargPosition)
    expandTemplate(c, expandedInto, res.decl, args, firstVarargMatch, addr m.inferred)
    # We took 2 cursors, so we have to do the `endRead` twice too:
    endRead(c.dest)
    endRead(c.dest)
    shrink c.dest, beforeCall
    var a = Item(n: cursorAt(expandedInto, 0), typ: c.types.autoType)
    semExpr c, a
    it.typ = a.typ
    it.kind = a.kind
  else:
    c.buildErr it.n.info, "could not load symbol: " & pool.syms[fnId] & "; errorCode: " & $res.status

proc sameIdent(sym: SymId; str: StrId): bool =
  # XXX speed this up by using the `fieldCache` idea
  var name = pool.syms[sym]
  extractBasename(name)
  result = pool.strings.getOrIncl(name) == str

proc sameIdent(a, b: SymId): bool =
  # XXX speed this up by using the `fieldCache` idea
  var x = pool.syms[a]
  extractBasename(x)
  var y = pool.syms[b]
  extractBasename(y)
  result = x == y

type
  FnCandidates = object
    a: seq[FnCandidate]
    s: HashSet[SymId]

proc addUnique(c: var FnCandidates; x: FnCandidate) =
  if not containsOrIncl(c.s, x.sym):
    c.a.add x

proc maybeAddConceptMethods(c: var SemContext; fn: StrId; typevar: SymId; cands: var FnCandidates) =
  let res = tryLoadSym(typevar)
  assert res.status == LacksNothing
  let local = asLocal(res.decl)
  if local.kind == TypevarY and local.typ.kind == Symbol:
    let concpt = local.typ.symId
    let section = getTypeSection concpt

    var ops = section.body
    inc ops  # (concept
    skip ops # .
    skip ops # .
    skip ops #   (typevar Self ...)
    if ops == "stmts":
      inc ops
      while ops.kind != ParRi:
        let sk = ops.symKind
        if sk in RoutineKinds:
          var prc = ops
          inc prc # (proc
          if prc.kind == SymbolDef and sameIdent(prc.symId, fn):
            var d = ops
            skipToParams d
            cands.addUnique FnCandidate(kind: ConceptProcY, sym: prc.symId, typ: d)
        skip ops

proc considerTypeboundOps(c: var SemContext; m: var seq[Match]; candidates: FnCandidates; args: openArray[Item], genericArgs: Cursor) =
  for candidate in candidates.a:
    m.add createMatch(addr c)
    sigmatch(m[^1], candidate, args, genericArgs)

proc requestRoutineInstance(c: var SemContext; origin: SymId; m: var Match;
                            info: PackedLineInfo): ProcInstance =
  let key = typeToCanon(m.typeArgs, 0)
  var targetSym = c.instantiatedProcs.getOrDefault(key)
  if targetSym == SymId(0):
    let targetSym = newSymId(c, origin)
    var signature = createTokenBuf(30)
    let decl = getProcDecl(origin)
    assert decl.typevars == "typevars", pool.syms[origin]
    buildTree signature, decl.kind, info:
      signature.add symdefToken(targetSym, info)
      signature.addDotToken() # a generic instance is not exported
      signature.copyTree decl.pattern
      # InvokeT for the generic params:
      signature.buildTree InvokeT, info:
        signature.add symToken(origin, info)
        signature.add m.typeArgs
      var sc = SubsContext(params: addr m.inferred)
      subs(c, signature, sc, decl.params)
      let beforeRetType = signature.len
      subs(c, signature, sc, decl.retType)
      subs(c, signature, sc, decl.pragmas)
      subs(c, signature, sc, decl.effects)
      signature.addDotToken() # no body

    result = ProcInstance(targetSym: targetSym, procType: cursorAt(signature, 0),
      returnType: cursorAt(signature, beforeRetType))
    publish targetSym, ensureMove signature

    c.instantiatedProcs[key] = targetSym
    var req = InstRequest(
      origin: origin,
      targetSym: targetSym,
      inferred: move(m.inferred)
    )
    for ins in c.instantiatedFrom: req.requestFrom.add ins
    req.requestFrom.add info

    c.procRequests.add ensureMove req
  else:
    let res = tryLoadSym(targetSym)
    assert res.status == LacksNothing
    var n = res.decl
    skipToParams n
    skip n
    result = ProcInstance(targetSym: targetSym, procType: res.decl,
      returnType: n)
  assert result.returnType.kind != UnknownToken

proc typeofCallIs(c: var SemContext; it: var Item; beforeCall: int; returnType: TypeCursor) {.inline.} =
  let expected = it.typ
  it.typ = returnType
  commonType c, it, beforeCall, expected

proc getFnIdent(c: var SemContext): StrId =
  var n = beginRead(c.dest)
  result = getIdent(n)
  endRead(c.dest)

proc containsGenericParams(n: TypeCursor): bool =
  var n = n
  var nested = 0
  while true:
    case n.kind
    of Symbol:
      let res = tryLoadSym(n.symId)
      if res.status == LacksNothing and res.decl == $TypevarY:
        return true
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else: discard
    if nested == 0: break
    inc n
  return false

type
  DotExprState = enum
    MatchedDotField ## matched a dot field, i.e. result is a dot expression
    MatchedDotSym ## matched a qualified identifier
    FailedDot
    InvalidDot

proc tryBuiltinDot(c: var SemContext; it: var Item; lhs: Item; fieldName: StrId; info: PackedLineInfo; flags: set[SemFlag]): DotExprState

proc semBuiltinSubscript(c: var SemContext; lhs: Item; it: var Item)

type
  TransformedCallSource = enum
    RegularCall, MethodCall, DotCall, SubscriptCall

  CallState = object
    beforeCall: int
    fn: Item
    fnKind: SymKind
    callNode: PackedToken
    dest, genericDest: TokenBuf
    args: seq[Item]
    hasGenericArgs: bool
    candidates: FnCandidates
    source: TransformedCallSource
      ## type of expression the call was transformed from

proc untypedCall(c: var SemContext; it: var Item; cs: CallState) =
  c.dest.add cs.callNode
  c.dest.addSubtree cs.fn.n
  for a in cs.args:
    c.dest.addSubtree a.n
  typeofCallIs c, it, cs.beforeCall, c.types.autoType
  wantParRi c, it.n

proc semConvFromCall(c: var SemContext; it: var Item; cs: CallState) =
  const
    IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT}
    StringTypes = {StringT, CstringT}
  let beforeExpr = c.dest.len
  let info = cs.callNode.info
  c.dest.add parLeToken(ConvX, info)
  var destType = cs.fn.typ
  if destType.typeKind == TypedescT: inc destType
  c.dest.copyTree destType

  var srcType = skipModifier(cs.args[0].typ)

  # distinct type conversion?
  var isDistinct = false
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)

  if (destBase.typeKind in IntegralTypes and srcBase.typeKind in IntegralTypes) or
     (destBase.typeKind in StringTypes and srcBase.typeKind in StringTypes) or
     (destBase.containsGenericParams or srcBase.containsGenericParams):
    discard "ok"
    # XXX Add hderef here somehow
    c.dest.addSubtree cs.args[0].n
  elif isDistinct:
    var arg = Item(n: cs.args[0].n, typ: srcBase)
    var m = createMatch(addr c)
    typematch m, destBase, arg
    if m.err:
      c.typeMismatch info, cs.args[0].typ, destType
    else:
      # distinct type conversions can also involve conversions
      # between different integer sizes or object types and then
      # `m.args` contains these so use them here:
      c.dest.add m.args
  else:
    # maybe object types with an inheritance relation?
    var arg = cs.args[0]
    var m = createMatch(addr c)
    typematch m, destType, arg
    if not m.err:
      c.dest.add m.args
    else:
      # also try the other direction:
      var m = createMatch(addr c)
      m.flipped = true
      arg.typ = destType
      typematch m, srcType, arg
      if not m.err:
        c.dest.add m.args
      else:
        c.typeMismatch info, cs.args[0].typ, destType

  wantParRi c, it.n
  commonType c, it, beforeExpr, destType

proc isCastableType(t: TypeCursor): bool =
  const IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, PointerT, CstringT, RefT, PtrT, NilT}
  result = t.typeKind in IntegralTypes or isEnumType(t)

proc semCast(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  let destType = semLocalType(c, it.n)
  var x = Item(n: it.n, typ: c.types.autoType)
  # XXX Add hderef here somehow
  semExpr c, x
  it.n = x.n
  wantParRi c, it.n

  var srcType = skipModifier(x.typ)

  # distinct type conversion?
  var isDistinct = false
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)
  if destBase.isCastableType and srcBase.isCastableType:
    commonType c, it, beforeExpr, destType
  elif containsGenericParams(srcType) or containsGenericParams(destType):
    commonType c, it, beforeExpr, destType
  else:
    c.dest.shrink beforeExpr
    c.buildErr info, "cannot `cast` between types " & typeToString(srcType) & " and " & typeToString(destType)

proc resolveOverloads(c: var SemContext; it: var Item; cs: var CallState) =
  let genericArgs =
    if cs.hasGenericArgs: cursorAt(cs.genericDest, 0)
    else: emptyNode()

  var m: seq[Match] = @[]
  if cs.fn.n.exprKind in {OchoiceX, CchoiceX}:
    var f = cs.fn.n
    inc f
    while f.kind != ParRi:
      if f.kind == Symbol:
        let sym = f.symId
        let s = fetchSym(c, sym)
        let candidate = FnCandidate(kind: s.kind, sym: sym, typ: fetchType(c, f, s))
        m.add createMatch(addr c)
        sigmatch(m[^1], candidate, cs.args, genericArgs)
      else:
        buildErr c, cs.fn.n.info, "`choice` node does not contain `symbol`"
      inc f
    considerTypeboundOps(c, m, cs.candidates, cs.args, genericArgs)
  elif cs.fn.n.kind == Ident:
    # error should have been given above already:
    # buildErr c, fn.n.info, "attempt to call undeclared routine"
    discard
  elif cs.fn.typ.typeKind == TypedescT and cs.args.len == 1:
    semConvFromCall c, it, cs
    return
  else:
    # Keep in mind that proc vars are a thing:
    let sym = if cs.fn.n.kind == Symbol: cs.fn.n.symId else: SymId(0)
    let candidate = FnCandidate(kind: cs.fnKind, sym: sym, typ: cs.fn.typ)
    m.add createMatch(addr c)
    sigmatch(m[^1], candidate, cs.args, genericArgs)
    considerTypeboundOps(c, m, cs.candidates, cs.args, genericArgs)
  let idx = pickBestMatch(c, m)

  if idx >= 0:
    c.dest.add cs.callNode
    let finalFn = m[idx].fn
    let isMagic = c.addFn(finalFn, cs.fn.n, cs.args)
    c.dest.add m[idx].args
    wantParRi c, it.n

    if finalFn.kind == TemplateY:
      typeofCallIs c, it, cs.beforeCall, m[idx].returnType
      if c.templateInstCounter <= MaxNestedTemplates:
        inc c.templateInstCounter
        withErrorContext c, cs.callNode.info:
          semTemplateCall c, it, finalFn.sym, cs.beforeCall, m[idx]
        dec c.templateInstCounter
      else:
        buildErr c, cs.callNode.info, "recursion limit exceeded for template expansions"
    elif isMagic == MagicCallNeedsSemcheck:
      # semcheck produced magic expression
      var magicExprBuf = createTokenBuf(c.dest.len - cs.beforeCall)
      magicExprBuf.addUnstructured cursorAt(c.dest, cs.beforeCall)
      endRead(c.dest)
      c.dest.shrink cs.beforeCall
      var magicExpr = Item(n: cursorAt(magicExprBuf, 0), typ: it.typ)
      semExpr c, magicExpr
      it.typ = magicExpr.typ
    elif c.routine.inGeneric == 0 and m[idx].inferred.len > 0 and isMagic == NonMagicCall:
      assert cs.fn.n.kind == Symbol
      let inst = c.requestRoutineInstance(cs.fn.n.symId, m[idx], cs.callNode.info)
      c.dest[cs.beforeCall+1].setSymId inst.targetSym
      typeofCallIs c, it, cs.beforeCall, inst.returnType
    else:
      typeofCallIs c, it, cs.beforeCall, m[idx].returnType

  else:
    skipParRi it.n
    var errorMsg: string
    if idx == -2:
      errorMsg = "ambiguous call"
    elif m.len > 0:
      errorMsg = "Type mismatch at [position]"
      for i in 0..<m.len:
        errorMsg.add "\n"
        addErrorMsg errorMsg, m[i]
    else:
      errorMsg = "undeclared identifier"
    var errored = createTokenBuf(4)
    # rebuild original call:
    case cs.source
    of RegularCall:
      errored.add cs.callNode
      errored.addSubtree cs.fn.n
      for a in cs.args:
        errored.addSubtree a.n
    of MethodCall:
      assert cs.args.len >= 1
      errored.add cs.callNode
      errored.addParLe(DotX, cs.callNode.info)
      errored.addSubtree cs.args[0].n
      errored.addParRi()
      errored.addSubtree cs.fn.n
      for i in 1 ..< cs.args.len:
        errored.addSubtree cs.args[i].n
    of DotCall:
      assert cs.args.len == 1
      errored.addParLe(DotX, cs.callNode.info)
      errored.addSubtree cs.args[0].n
      errored.addSubtree cs.fn.n
    of SubscriptCall:
      errored.addParLe(AtX, cs.callNode.info)
      for a in cs.args:
        errored.addSubtree a.n
    errored.addParRi()
    buildErr c, cs.callNode.info, errorMsg, cursorAt(errored, 0)

proc semCall(c: var SemContext; it: var Item; source: TransformedCallSource = RegularCall) =
  var cs = CallState(
    beforeCall: c.dest.len,
    callNode: it.n.load(),
    dest: createTokenBuf(16),
    source: source
  )
  inc it.n
  swap c.dest, cs.dest
  cs.fn = Item(n: it.n, typ: c.types.autoType)
  var fnName = StrId(0)
  var argIndexes: seq[int] = @[]
  if cs.fn.n.exprKind == AtX:
    inc cs.fn.n # skip tag
    var lhsBuf = createTokenBuf(4)
    var lhs = Item(n: cs.fn.n, typ: c.types.autoType)
    swap c.dest, lhsBuf
    semExpr c, lhs, {KeepMagics, AllowUndeclared}
    swap c.dest, lhsBuf
    cs.fn.n = lhs.n
    lhs.n = cursorAt(lhsBuf, 0)
    if lhs.n.kind == Symbol and isRoutine(lhs.kind):
      let res = tryLoadSym(lhs.n.symId)
      assert res.status == LacksNothing
      if isGeneric(asRoutine(res.decl)):
        cs.hasGenericArgs = true
        cs.genericDest = createTokenBuf(16)
        swap c.dest, cs.genericDest
        while cs.fn.n.kind != ParRi:
          # XXX semLocalType should build `static` types for values
          discard semLocalType(c, cs.fn.n)
        swap c.dest, cs.genericDest
        skipParRi cs.fn.n
        it.n = cs.fn.n
        c.dest.addSubtree lhs.n
        cs.fn.typ = lhs.typ
        cs.fn.kind = lhs.kind
        fnName = getFnIdent(c)
    if not cs.hasGenericArgs:
      semBuiltinSubscript(c, lhs, cs.fn)
      fnName = getFnIdent(c)
      it.n = cs.fn.n
  elif cs.fn.n.exprKind == DotX:
    let dotStart = c.dest.len
    let dotInfo = cs.fn.n.info
    # read through the dot expression first:
    inc cs.fn.n # skip tag
    var lhsBuf = createTokenBuf(4)
    var lhs = Item(n: cs.fn.n, typ: c.types.autoType)
    swap c.dest, lhsBuf
    semExpr c, lhs, {AllowModuleSym}
    swap c.dest, lhsBuf
    cs.fn.n = lhs.n
    lhs.n = cursorAt(lhsBuf, 0)
    let fieldNameCursor = cs.fn.n
    let fieldName = getIdent(cs.fn.n)
    # skip optional inheritance depth:
    if cs.fn.n.kind == IntLit:
      inc cs.fn.n
    skipParRi cs.fn.n
    it.n = cs.fn.n
    # now interpret the dot expression:
    let dotState = tryBuiltinDot(c, cs.fn, lhs, fieldName, dotInfo, {KeepMagics, AllowUndeclared})
    if dotState == FailedDot or
        # also ignore non-proc fields:
        (dotState == MatchedDotField and cs.fn.typ.typeKind != ProcT):
      cs.source = MethodCall
      # turn a.b(...) into b(a, ...)
      # first, delete the output of `tryBuiltinDot`:
      c.dest.shrink dotStart
      # sem b:
      cs.fn = Item(n: fieldNameCursor, typ: c.types.autoType)
      semExpr c, cs.fn, {KeepMagics, AllowUndeclared}
      fnName = getFnIdent(c)
      # add a as argument:
      let lhsIndex = c.dest.len
      c.dest.addSubtree lhs.n
      argIndexes.add lhsIndex
      # scope extension: If the type is Typevar and it has attached
      # a concept, use the concepts symbols too:
      if fnName != StrId(0) and lhs.typ.kind == Symbol:
        maybeAddConceptMethods c, fnName, lhs.typ.symId, cs.candidates
      # lhs.n escapes here, but is not read and will be set by argIndexes:
      cs.args.add lhs
  else:
    semExpr(c, cs.fn, {KeepMagics, AllowUndeclared})
    fnName = getFnIdent(c)
    it.n = cs.fn.n
  cs.fnKind = cs.fn.kind
  var skipSemCheck = false
  while it.n.kind != ParRi:
    var arg = Item(n: it.n, typ: c.types.autoType)
    argIndexes.add c.dest.len
    semExpr c, arg
    if arg.typ.typeKind == UntypedT:
      skipSemCheck = true
    # scope extension: If the type is Typevar and it has attached
    # a concept, use the concepts symbols too:
    if fnName != StrId(0) and arg.typ.kind == Symbol:
      maybeAddConceptMethods c, fnName, arg.typ.symId, cs.candidates
    it.n = arg.n
    cs.args.add arg
  assert cs.args.len == argIndexes.len
  swap c.dest, cs.dest
  cs.fn.n = beginRead(cs.dest)
  for i in 0 ..< cs.args.len:
    cs.args[i].n = cursorAt(cs.dest, argIndexes[i])
  if skipSemCheck:
    untypedCall c, it, cs
  else:
    resolveOverloads c, it, cs

proc findObjField(t: Cursor; name: StrId; level = 0): ObjField =
  assert t == "object"
  var n = t
  inc n # skip `(object` token
  let baseType = n
  skip n # skip basetype
  while n.kind == ParLe and n.substructureKind == FldS:
    inc n # skip FldS
    if n.kind == SymbolDef and sameIdent(n.symId, name):
      let symId = n.symId
      inc n # skip name
      skip n # export marker
      skip n # pragmas
      return ObjField(sym: symId, level: level, typ: n)
    skip n # skip name
    skip n # export marker
    skip n # pragmas
    skip n # type
    skip n # value
  if baseType.kind == Symbol:
    result = findObjField(objtypeImpl(baseType.symId), name, level+1)
  else:
    result = ObjField(level: -1)

proc findModuleSymbol(n: Cursor): SymId =
  result = SymId(0)
  if n.kind == Symbol:
    let res = tryLoadSym(n.symId)
    if res.status == LacksNothing and symKind(res.decl) == ModuleY:
      result = n.symId
  elif n.kind == ParLe and exprKind(n) in {OchoiceX, CchoiceX}:
    # if any sym in choice is module sym, count it as a module reference
    # this emulates behavior that was caused by sym order shenanigans before, could be removed
    var n = n
    inc n
    while n.kind != ParRi:
      result = findModuleSymbol(n)
      if result != SymId(0): break
      inc n

proc semQualifiedIdent(c: var SemContext; module: SymId; ident: StrId; info: PackedLineInfo): Sym =
  # mirrors semIdent
  let insertPos = c.dest.len
  # XXX does not handle the case where `module` is the current module
  let count = buildSymChoiceForForeignModule(c, c.importedModules[module], ident, info)
  if count == 1:
    let sym = c.dest[insertPos+1].symId
    c.dest.shrink insertPos
    c.dest.add symToken(sym, info)
    result = fetchSym(c, sym)
  else:
    result = Sym(kind: if count == 0: NoSym else: CchoiceY)

proc semExprSym(c: var SemContext; it: var Item; s: Sym; start: int; flags: set[SemFlag])

proc tryBuiltinDot(c: var SemContext; it: var Item; lhs: Item; fieldName: StrId;
                   info: PackedLineInfo; flags: set[SemFlag]): DotExprState =
  let exprStart = c.dest.len
  let expected = it.typ
  c.dest.addParLe(DotX, info)
  c.dest.addSubtree lhs.n
  result = FailedDot
  if fieldName == StrId(0):
    # fatal error
    c.buildErr info, "identifier after `.` expected"
    result = InvalidDot
  else:
    let t = skipModifier(lhs.typ)
    if t.kind == Symbol:
      let objType = objtypeImpl(t.symId)
      if objType.typeKind == ObjectT:
        let field = findObjField(objType, fieldName)
        if field.level >= 0:
          c.dest.add symToken(field.sym, info)
          c.dest.add intToken(pool.integers.getOrIncl(field.level), info)
          it.typ = field.typ # will be fit later with commonType
          it.kind = FldY
          result = MatchedDotField
        else:
          c.dest.add identToken(fieldName, info)
          c.buildErr info, "undeclared field: " & pool.strings[fieldName]
      else:
        c.dest.add identToken(fieldName, info)
        c.buildErr info, "object type expected"
    elif lhs.kind == ModuleY:
      # this is a qualified identifier, i.e. module.name
      # consider matched even if undeclared
      result = MatchedDotSym
      c.dest.shrink exprStart
      let module = findModuleSymbol(lhs.n)
      let s = semQualifiedIdent(c, module, fieldName, info)
      semExprSym c, it, s, exprStart, flags
      return
    elif t.typeKind == TupleT:
      var tup = t
      inc tup
      while tup.kind != ParRi:
        let field = asLocal(tup)
        if field.name.kind == SymbolDef and sameIdent(field.name.symId, fieldName):
          c.dest.add symToken(field.name.symId, info)
          it.typ = field.typ # will be fit later with commonType
          it.kind = FldY
          result = MatchedDotField
          break
        skip tup
      if result != MatchedDotField:
        c.dest.add identToken(fieldName, info)
        c.buildErr info, "undeclared field: " & pool.strings[fieldName]
      c.dest.add intToken(pool.integers.getOrIncl(0), info)
    else:
      c.dest.add identToken(fieldName, info)
      c.buildErr info, "object type expected"
  c.dest.addParRi()
  if result == MatchedDotField:
    commonType c, it, exprStart, expected

proc semDot(c: var SemContext, it: var Item; flags: set[SemFlag]) =
  let exprStart = c.dest.len
  let info = it.n.info
  let expected = it.typ
  # read through the dot expression first:
  inc it.n # skip tag
  var lhsBuf = createTokenBuf(4)
  var lhs = Item(n: it.n, typ: c.types.autoType)
  swap c.dest, lhsBuf
  semExpr c, lhs, {AllowModuleSym}
  swap c.dest, lhsBuf
  it.n = lhs.n
  lhs.n = cursorAt(lhsBuf, 0)
  let fieldNameCursor = it.n
  let fieldName = getIdent(it.n)
  # skip optional inheritance depth:
  if it.n.kind == IntLit:
    inc it.n
  skipParRi it.n
  # now interpret the dot expression:
  let state = tryBuiltinDot(c, it, lhs, fieldName, info, flags)
  if state == FailedDot:
    # attempt a dot call, i.e. build b(a) from a.b
    c.dest.shrink exprStart
    var callBuf = createTokenBuf(16)
    callBuf.addParLe(CallX, info)
    callBuf.add fieldNameCursor
    callBuf.addSubtree lhs.n # add lhs as first argument
    callBuf.addParRi()
    var call = Item(n: cursorAt(callBuf, 0), typ: expected)
    semCall c, call, DotCall
    it.typ = call.typ

proc semWhile(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  semBoolExpr c, it.n
  inc c.routine.inLoop
  withNewScope c:
    semStmt c, it.n
  dec c.routine.inLoop
  wantParRi c, it.n
  producesVoid c, info, it.typ

proc semBlock(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n

  inc c.routine.inBlock
  withNewScope c:
    if it.n.kind == DotToken:
      takeToken c, it.n
    else:
      let declStart = c.dest.len
      let delayed = handleSymDef(c, it.n, LabelY)
      c.addSym delayed
      publish c, delayed.s.name, declStart

    semStmt c, it.n
  dec c.routine.inBlock

  wantParRi c, it.n
  producesVoid c, info, it.typ

proc semBreak(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.inLoop+c.routine.inBlock == 0:
    buildErr c, it.n.info, "`break` only possible within a `while` or `block` statement"
  else:
    if it.n.kind == DotToken:
      wantDot c, it.n
    else:
      var a = Item(n: it.n, typ: c.types.autoType)
      semExpr(c, a)
      if a.kind != LabelY:
        buildErr c, it.n.info, "`break` needs a block label"
      it.n = a.n
  wantParRi c, it.n
  producesNoReturn c, info, it.typ

proc semContinue(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.inLoop == 0:
    buildErr c, it.n.info, "`continue` only possible within a `while` statement"
  else:
    wantDot c, it.n
  wantParRi c, it.n
  producesNoReturn c, info, it.typ

proc wantExportMarker(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    c.dest.add n
    inc n
  elif n.kind == Ident and pool.strings[n.litId] == "x":
    if c.currentScope.kind != ToplevelScope:
      buildErr c, n.info, "only toplevel declarations can be exported"
    else:
      c.dest.add n
    inc n
  elif n.kind == ParLe:
    # export marker could have been turned into a NIF tag
    takeTree c, n
  else:
    buildErr c, n.info, "expected '.' or 'x' for an export marker"

proc insertType(c: var SemContext; typ: TypeCursor; patchPosition: int) =
  let t = skipModifier(typ)
  c.dest.insert t, patchPosition

proc patchType(c: var SemContext; typ: TypeCursor; patchPosition: int) =
  let t = skipModifier(typ)
  c.dest.replace t, patchPosition

type
  CrucialPragma* = object
    magic: string
    bits: int
    hasVarargs: PackedLineInfo
    flags: set[PragmaKind]

proc semPragma(c: var SemContext; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  let pk = pragmaKind(n)
  case pk
  of NoPragma:
    if kind.isRoutine and (let cc = callConvKind(n); cc != NoCallConv):
      c.dest.add parLeToken(pool.tags.getOrIncl($cc), n.info)
      inc n
      c.dest.addParRi()
    else:
      buildErr c, n.info, "expected pragma"
      inc n
      c.dest.addParRi()
      #skip n
  of Magic:
    c.dest.add parLeToken(pool.tags.getOrIncl($pk), n.info)
    inc n
    if n.kind in {StringLit, Ident}:
      let m = parseMagic(pool.strings[n.litId])
      if m == mNone:
        buildErr c, n.info, "unknown `magic`"
      else:
        let (magicWord, bits) = magicToTag(m)
        crucial.magic = magicWord
        crucial.bits = bits
      takeToken c, n
    else:
      buildErr c, n.info, "`magic` pragma takes a string literal"
    c.dest.addParRi()
  of ImportC, ImportCpp, ExportC, Header:
    c.dest.add parLeToken(pool.tags.getOrIncl($pk), n.info)
    inc n
    if n.kind != ParRi:
      semConstStrExpr c, n
    c.dest.addParRi()
  of Align, Bits:
    c.dest.add parLeToken(pool.tags.getOrIncl($pk), n.info)
    inc n
    semConstIntExpr(c, n)
    c.dest.addParRi()
  of Nodecl, Selectany, Threadvar, Globalvar, Discardable, Noreturn, Borrow:
    crucial.flags.incl pk
    c.dest.add parLeToken(pool.tags.getOrIncl($pk), n.info)
    c.dest.addParRi()
    inc n
  of Varargs:
    crucial.hasVarargs = n.info
    c.dest.add parLeToken(pool.tags.getOrIncl($pk), n.info)
    c.dest.addParRi()
    inc n

proc semPragmas(c: var SemContext; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  if n.kind == DotToken:
    takeToken c, n
  elif n == "pragmas":
    takeToken c, n
    while n.kind != ParRi:
      let hasParRi = n.kind == ParLe
      if n == "kv":
        inc n
      semPragma c, n, crucial, kind
      if hasParRi:
        skipParRi n
    wantParRi c, n
  else:
    buildErr c, n.info, "expected '.' or 'pragmas'"

proc semIdentImpl(c: var SemContext; n: var Cursor; ident: StrId): Sym =
  let insertPos = c.dest.len
  let info = n.info
  let count = buildSymChoice(c, ident, info, InnerMost)
  if count == 1:
    let sym = c.dest[insertPos+1].symId
    c.dest.shrink insertPos
    c.dest.add symToken(sym, info)
    result = fetchSym(c, sym)
  else:
    result = Sym(kind: if count == 0: NoSym else: CchoiceY)

proc semIdent(c: var SemContext; n: var Cursor): Sym =
  result = semIdentImpl(c, n, n.litId)
  inc n

proc semQuoted(c: var SemContext; n: var Cursor): Sym =
  let nameId = unquote(n)
  result = semIdentImpl(c, n, nameId)

proc maybeInlineMagic(c: var SemContext; res: LoadResult) =
  if res.status == LacksNothing:
    var n = res.decl
    inc n # skip the symbol kind
    if n.kind == SymbolDef:
      inc n # skip the SymbolDef
      if n.kind == ParLe:
        # ^ export marker position has a `(`? If so, it is a magic!
        c.dest[c.dest.len-1] = n.load
        inc n
        while true:
          c.dest.add n
          if n.kind == ParRi: break
          inc n

proc semTypeSym(c: var SemContext; s: Sym; info: PackedLineInfo; start: int; context: TypeDeclContext) =
  if s.kind in {TypeY, TypevarY}:
    let res = tryLoadSym(s.name)
    let beforeMagic = c.dest.len
    maybeInlineMagic c, res
    let afterMagic = c.dest.len
    if s.kind == TypevarY:
      # likely was not magic
      # maybe substitution performed here?
      inc c.usedTypevars
    elif beforeMagic != afterMagic:
      # was magic, nothing to do
      discard
    else:
      let typ = asTypeDecl(res.decl)
      if typ.body.typeKind in {ObjectT, EnumT, DistinctT, ConceptT}:
        # types that should stay as symbols, see sigmatch.matchSymbol
        discard
      else:
        # remove symbol, inline type:
        c.dest.shrink c.dest.len-1
        var t = typ.body
        semLocalTypeImpl c, t, context
  elif context == AllowValues:
    # non type symbol, treat as expression
    # XXX should skip TypedescT and become StaticT/UnresolvedT otherwise
    var dummyBuf = createTokenBuf(1)
    dummyBuf.add dotToken(info)
    var it = Item(n: cursorAt(dummyBuf, 0), typ: c.types.autoType)
    semExprSym c, it, s, start, {}
  elif s.kind != NoSym:
    var orig = createTokenBuf(1)
    orig.add c.dest[c.dest.len-1]
    c.dest.shrink c.dest.len-1
    c.buildErr info, "type name expected, but got: " & pool.syms[s.name], cursorAt(orig, 0)
  else:
    var orig = createTokenBuf(1)
    orig.add c.dest[c.dest.len-1]
    c.dest.shrink c.dest.len-1
    c.buildErr info, "unknown type name", cursorAt(orig, 0)

proc semParams(c: var SemContext; n: var Cursor)
proc semLocal(c: var SemContext; n: var Cursor; kind: SymKind)

proc semObjectType(c: var SemContext; n: var Cursor) =
  takeToken c, n
  # inherits from?
  if n.kind == DotToken:
    takeToken c, n
  else:
    semLocalTypeImpl c, n, InLocalDecl
  if n.kind == DotToken:
    takeToken c, n
  else:
    # object fields:
    withNewScope c:
      while n.substructureKind == FldS:
        semLocal(c, n, FldY)
  wantParRi c, n

proc semTupleType(c: var SemContext; n: var Cursor) =
  takeToken c, n
  # tuple fields:
  withNewScope c:
    while n.substructureKind == FldS:
      semLocal(c, n, FldY)
  wantParRi c, n

type
  EnumTypeState = object
    isBoolType: bool # `bool` is a magic enum and needs special handling
    enumType: SymId
    thisValue: xint
    hasHole: bool

proc semEnumField(c: var SemContext; n: var Cursor; state: var EnumTypeState)

proc semEnumType(c: var SemContext; n: var Cursor; enumType: SymId; beforeExportMarker: int) =
  # XXX Propagate hasHole somehow
  takeToken c, n
  let magicToken = c.dest[beforeExportMarker]
  var state = EnumTypeState(enumType: enumType, thisValue: createXint(0'i64), hasHole: false,
    isBoolType: magicToken.kind == ParLe and pool.tags[magicToken.tagId] == $BoolT)
  while n.substructureKind == EfldS:
    semEnumField(c, n, state)
    inc state.thisValue
  wantParRi c, n

proc declareConceptSelf(c: var SemContext; info: PackedLineInfo) =
  let name = pool.strings.getOrIncl("Self")
  let result = identToSym(c, name, TypevarY)
  let s = Sym(kind: TypevarY, name: result,
              pos: c.dest.len)
  discard c.currentScope.addNonOverloadable(name, s)
  let declStart = c.dest.len
  buildTree c.dest, TypevarY, info:
    c.dest.add symdefToken(result, info) # name
    c.dest.addDotToken() # export marker
    c.dest.addDotToken() # pragmas
    c.dest.addDotToken() # typ
    c.dest.addDotToken() # value
  publish c, result, declStart

proc semConceptType(c: var SemContext; n: var Cursor) =
  takeToken c, n
  wantDot c, n
  wantDot c, n
  declareConceptSelf c, n.info
  skip n # skip dot or previous `Self` declaration
  if n != "stmts":
    error "(stmts) expected, but got: ", n
  takeToken c, n
  withNewScope c:
    while true:
      let k = n.symKind
      if k in RoutineKinds:
        var it = Item(n: n, typ: c.types.voidType)
        semProc(c, it, k, checkConceptProc)
        n = it.n
      else:
        break
  wantParRi c, n
  wantParRi c, n

proc instGenericType(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo;
                     origin, targetSym: SymId; decl: TypeDecl; args: Cursor) =
  #[
  What we need to do is rather simple: A generic instantiation is
  the typical (type :Name ex generic_params pragmas body) tuple but
  this time the generic_params list the used `Invoke` construct for the
  instantiation.
  ]#
  var inferred = initTable[SymId, Cursor]()
  var err = 0
  dest.buildTree TypeS, info:
    dest.add symdefToken(targetSym, info)
    dest.addDotToken() # export
    dest.buildTree InvokeT, info:
      dest.add symToken(origin, info)
      var a = args
      var typevars = decl.typevars
      inc typevars
      while a.kind != ParRi and typevars.kind != ParRi:
        var tv = typevars
        assert tv == "typevar"
        inc tv
        assert tv.kind == SymbolDef
        inferred[tv.symId] = a
        takeTree dest, a
        skip typevars
      if a.kind != ParRi:
        err = -1
      elif typevars.kind != ParRi:
        err = 1
    # take the pragmas from the origin:
    dest.copyTree decl.pragmas
    if err == 0:
      var sc = SubsContext(params: addr inferred)
      subs(c, dest, sc, decl.body)
    elif err == 1:
      dest.buildLocalErr info, "too few generic arguments provided"
    else:
      dest.buildLocalErr info, "too many generic arguments provided"

proc isRangeExpr(n: Cursor): bool =
  var n = n
  if n.exprKind notin {CallX, InfixX}:
    return false
  inc n
  let name = getIdent(n)
  result = name != StrId(0) and pool.strings[name] == ".."

proc addRangeValues(c: var SemContext; n: var Cursor) =
  # XXX AllowValues refactor would need this to handle StaticT/UnresolvedT
  var err: bool = false
  let first = asSigned(evalOrdinal(c, n), err)
  if err:
    c.buildErr n.info, "could not evaluate as ordinal", n
    err = false
  else:
    c.dest.addIntLit(first, n.info)
  skip n
  let last = asSigned(evalOrdinal(c, n), err)
  if err:
    c.buildErr n.info, "could not evaluate as ordinal", n
    err = false
  else:
    c.dest.addIntLit(last, n.info)

proc semRangeTypeFromExpr(c: var SemContext; n: var Cursor; info: PackedLineInfo) =
  inc n # call tag
  skip n # `..`
  c.dest.addParLe(RangeT, info)
  var it = Item(n: n, typ: c.types.autoType)
  var valuesBuf = createTokenBuf(4)
  swap c.dest, valuesBuf
  semExpr c, it
  semExpr c, it
  swap c.dest, valuesBuf
  n = it.n
  # insert base type:
  c.dest.addSubtree it.typ
  var values = cursorAt(valuesBuf, 0)
  addRangeValues c, values
  wantParRi c, n

proc semInvoke(c: var SemContext; n: var Cursor) =
  let typeStart = c.dest.len
  let info = n.info
  takeToken c, n # copy `at`
  semLocalTypeImpl c, n, InLocalDecl

  var headId: SymId = SymId(0)
  var decl = default TypeDecl
  var magicKind = NoType
  var ok = false
  if c.dest[typeStart+1].kind == Symbol:
    headId = c.dest[typeStart+1].symId
    decl = getTypeSection(headId)
    if decl.kind != TypeY:
      c.buildErr info, "cannot attempt to instantiate a non-type"
    if decl.typevars != "typevars":
      c.buildErr info, "cannot attempt to instantiate a concrete type"
    else:
      ok = true
  else:
    # symbol may have inlined into a magic
    let head = cursorAt(c.dest, typeStart+1)
    let kind = head.typeKind
    endRead(c.dest)
    if kind in {ArrayT, RangeT, VarargsT,
      PtrT, RefT, UncheckedArrayT, SetT, StaticT, TypedescT}:
      # magics that can be invoked
      magicKind = kind
      ok = true
    else:
      c.buildErr info, "cannot attempt to instantiate a non-type"

  var genericArgs = 0
  swap c.usedTypevars, genericArgs
  let beforeArgs = c.dest.len
  while n.kind != ParRi:
    semLocalTypeImpl c, n, AllowValues
  swap c.usedTypevars, genericArgs
  wantParRi c, n
  if genericArgs == 0 and ok:
    # we have to be eager in generic type instantiations so that type-checking
    # can do its job properly:
    let key = typeToCanon(c.dest, typeStart)
    if c.instantiatedTypes.hasKey(key):
      c.dest.add symToken(c.instantiatedTypes[key], info)
    else:
      var args = cursorAt(c.dest, beforeArgs)
      if magicKind != NoType:
        var magicExpr = createTokenBuf(8)
        magicExpr.addParLe(magicKind, info)
        # reorder invocation according to type specifications:
        case magicKind
        of ArrayT:
          # invoked as array[len, elem], but needs to become (array elem len)
          let indexPart = args
          skip args
          magicExpr.takeTree args # element type
          magicExpr.addSubtree indexPart
          skipParRi args
        of RangeT:
          # range types are invoked as `range[a..b]`
          if isRangeExpr(args):
            # don't bother calling semLocalTypeImpl, fully build type here
            magicExpr.shrink 0
            swap c.dest, magicExpr
            semRangeTypeFromExpr c, args, info
            swap c.dest, magicExpr
            c.dest.endRead()
            c.dest.shrink typeStart
            c.dest.add magicExpr
            return
          else:
            # error?
            discard
        of PtrT, RefT, UncheckedArrayT, SetT, StaticT, TypedescT:
          # unary invocations
          magicExpr.takeTree args
          skipParRi args
        of VarargsT:
          magicExpr.takeTree args
          if args.kind != ParRi:
            # optional varargs call
            magicExpr.takeTree args
          skipParRi args
        else:
          raiseAssert "unreachable" # see type kind check for magicKind
        magicExpr.addParRi()
        c.dest.endRead()
        c.dest.shrink typeStart
        var m = cursorAt(magicExpr, 0)
        semLocalTypeImpl c, m, InLocalDecl
        return
      let targetSym = newSymId(c, headId)
      var instance = createTokenBuf(30)
      instGenericType c, instance, info, headId, targetSym, decl, args
      c.dest.endRead()
      publish targetSym, ensureMove instance
      c.instantiatedTypes[key] = targetSym
      c.dest.shrink typeStart
      c.dest.add symToken(targetSym, info)

proc addVarargsParameter(c: var SemContext; paramsAt: int; info: PackedLineInfo) =
  const vanon = "vanon"
  var varargsParam = @[
    parLeToken(ParamS, info),
    identToken(pool.strings.getOrIncl(vanon), info),
    dotToken(info), # export marker
    dotToken(info), # pragmas
    parLeToken(VarargsT, info),
    parRiToken(info),
    dotToken(info), # value
    parRiToken(info)
  ]
  if c.dest[paramsAt].kind == DotToken:
    c.dest[paramsAt] = parLeToken(ParamsS, info)
    varargsParam.add parRiToken(info)
    c.dest.insert fromBuffer(varargsParam), paramsAt+1
  else:
    var n = cursorAt(c.dest, paramsAt)
    if n.substructureKind == ParamsS:
      inc n
      while n.kind != ParRi:
        if n.substructureKind == ParamS:
          inc n
          let lit = getIdent(n)
          if lit != StrId(0) and pool.strings[lit] == vanon:
            # already added:
            endRead(c.dest)
            return
          skipToEnd n
        else:
          break
      let insertPos = cursorToPosition(c.dest, n)
      endRead(c.dest)
      c.dest.insert fromBuffer(varargsParam), insertPos

proc semLocalTypeImpl(c: var SemContext; n: var Cursor; context: TypeDeclContext) =
  let info = n.info
  case n.kind
  of Ident:
    let start = c.dest.len
    let s = semIdent(c, n)
    semTypeSym c, s, info, start, context
  of Symbol:
    let start = c.dest.len
    let s = fetchSym(c, n.symId)
    c.dest.add n
    inc n
    semTypeSym c, s, info, start, context
  of ParLe:
    case typeKind(n)
    of NoType:
      if exprKind(n) == QuotedX:
        let start = c.dest.len
        let s = semQuoted(c, n)
        semTypeSym c, s, info, start, context
      elif context == AllowValues:
        # XXX should skip TypedescT and become StaticT/UnresolvedT otherwise
        var it = Item(n: n, typ: c.types.autoType)
        semExpr c, it
        n = it.n
      elif false and isRangeExpr(n):
        # a..b, interpret as range type but only without AllowValues
        # to prevent conflict with HSlice
        # disabled for now, array types special case range expressions
        semRangeTypeFromExpr c, n, info
      else:
        c.buildErr info, "not a type", n
        skip n
    of IntT, FloatT, CharT, BoolT, UIntT, VoidT, StringT, NilT, AutoT, SymKindT, UntypedT, TypedT, CstringT, PointerT:
      takeTree c, n
    of PtrT, RefT, MutT, OutT, LentT, SinkT, NotT, UncheckedArrayT, StaticT, TypedescT:
      takeToken c, n
      semLocalTypeImpl c, n, context
      wantParRi c, n
    of SetT:
      takeToken c, n
      let elemTypeStart = c.dest.len
      semLocalTypeImpl c, n, context
      wantParRi c, n
      let elemType = cursorAt(c.dest, elemTypeStart)
      # XXX allow unresolved types
      if not isOrdinalType(elemType, allowEnumWithHoles = true):
        c.dest.endRead()
        c.buildErr info, "set element type must be ordinal"
      else:
        let length = lengthOrd(c, elemType)
        c.dest.endRead()
        if length.isNaN or length > MaxSetElements:
          c.buildErr info, "type " & typeToString(elemType) & " is too large to be a set element type"
    of OrT, AndT:
      takeToken c, n
      semLocalTypeImpl c, n, context
      semLocalTypeImpl c, n, context
      wantParRi c, n
    of TupleT:
      semTupleType c, n
    of ArrayT:
      takeToken c, n
      semLocalTypeImpl c, n, context
      # index type, possibilities are:
      # 1. length as integer
      # 2. range expression i.e. `a..b`
      # 3. full ordinal type i.e. `uint8`, `Enum`, `range[a..b]`
      # 4. standalone unresolved expression/type variable, could resolve to 1 or 3
      if isRangeExpr(n):
        semRangeTypeFromExpr c, n, info
      else:
        var indexBuf = createTokenBuf(4)
        swap c.dest, indexBuf
        semLocalTypeImpl c, n, AllowValues
        swap c.dest, indexBuf
        var index = cursorAt(indexBuf, 0)
        if index.typeKind == RangeT:
          # direct range type
          c.dest.addSubtree index
        elif isOrdinalType(index):
          # ordinal type, turn it into a range type
          c.dest.addParLe(RangeT, index.info)
          c.dest.addSubtree index # base type
          var err = false
          let first = asSigned(firstOrd(c, index), err)
          if err:
            c.buildErr index.info, "could not get first index of ordinal type: " & typeToString(index)
          else:
            c.dest.addIntLit(first, index.info)
          err = false
          let last = asSigned(lastOrd(c, index), err)
          if err:
            c.buildErr index.info, "could not get last index of ordinal type: " & typeToString(index)
          else:
            c.dest.addIntLit(last, index.info)
          c.dest.addParRi()
        elif containsGenericParams(index):
          # unresolved types are left alone
          c.dest.addSubtree index
        elif index.typeKind != NoType:
          c.buildErr index.info, "unknown array index type: " & typeToString(index)
        else:
          # length expression
          var err = false
          let length = asSigned(evalOrdinal(c, index), err)
          if err:
            c.buildErr index.info, "invalid array index: " & typeToString(index)
          else:
            c.dest.addParLe(RangeT, info)
            c.dest.addSubtree c.types.intType
            c.dest.addIntLit 0, info
            c.dest.addIntLit length - 1, info
            c.dest.addParRi()
      wantParRi c, n
    of RangeT:
      takeToken c, n
      semLocalTypeImpl c, n, context
      var valuesBuf = createTokenBuf(4)
      swap c.dest, valuesBuf
      semLocalTypeImpl c, n, AllowValues
      semLocalTypeImpl c, n, AllowValues
      swap c.dest, valuesBuf
      var values = cursorAt(valuesBuf, 0)
      addRangeValues c, values
      wantParRi c, n
    of VarargsT:
      takeToken c, n
      if n.kind != ParRi:
        semLocalTypeImpl c, n, context
        if n.kind == DotToken:
          takeToken c, n
        else:
          var it = Item(n: n, typ: c.types.autoType)
          semExpr c, it
          # XXX Check the expression is a symchoice or a sym
          n = it.n
      wantParRi c, n
    of ObjectT:
      if context == InGenericConstraint:
        c.dest.takeTree n
      elif context != InTypeSection:
        c.buildErr info, "`object` type must be defined in a `type` section"
        skip n
      else:
        semObjectType c, n
    of EnumT:
      if context == InGenericConstraint:
        c.dest.takeTree n
      else:
        c.buildErr info, "`enum` type must be defined in a `type` section"
        skip n
    of ConceptT:
      if context != InTypeSection:
        c.buildErr info, "`concept` type must be defined in a `type` section"
        skip n
      else:
        semConceptType c, n
    of DistinctT:
      if context != InTypeSection:
        c.buildErr info, "`distinct` type must be defined in a `type` section"
        skip n
      else:
        takeToken c, n
        semLocalTypeImpl c, n, context
        wantParRi c, n
    of ProcT, IterT:
      takeToken c, n
      wantDot c, n # name
      let beforeParams = c.dest.len
      semParams c, n
      semLocalTypeImpl c, n, InReturnTypeDecl
      var crucial = default CrucialPragma
      semPragmas c, n, crucial, ProcY
      wantParRi c, n
      if crucial.hasVarargs.isValid:
        addVarargsParameter c, beforeParams, crucial.hasVarargs
    of InvokeT:
      semInvoke c, n
  of DotToken:
    if context in {InReturnTypeDecl, InGenericConstraint}:
      takeToken c, n
    else:
      c.buildErr info, "not a type", n
      inc n
  else:
    if context == AllowValues:
      # XXX should skip TypedescT and become StaticT/UnresolvedT otherwise
      var it = Item(n: n, typ: c.types.autoType)
      semExpr c, it
      n = it.n
    else:
      c.buildErr info, "not a type", n
      inc n

proc semReturnType(c: var SemContext; n: var Cursor): TypeCursor =
  result = semLocalType(c, n, InReturnTypeDecl)

proc exportMarkerBecomesNifTag(c: var SemContext; insertPos: int; crucial: CrucialPragma) =
  assert crucial.magic.len > 0
  let info = c.dest[insertPos].info

  if crucial.bits != 0:
    let nifTag = [
      parLeToken(pool.tags.getOrIncl(crucial.magic), info),
      intToken(pool.integers.getOrIncl(crucial.bits), info),
      parRiToken(info)
    ]
    c.dest.replace fromBuffer(nifTag), insertPos
  else:
    let nifTag = [
      parLeToken(pool.tags.getOrIncl(crucial.magic), info),
      parRiToken(info)
    ]
    c.dest.replace fromBuffer(nifTag), insertPos

proc semLocal(c: var SemContext; n: var Cursor; kind: SymKind) =
  let declStart = c.dest.len
  takeToken c, n
  let delayed = handleSymDef(c, n, kind) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1
  var crucial = default CrucialPragma
  semPragmas c, n, crucial, kind # 2
  if crucial.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, crucial
  case kind
  of TypevarY:
    discard semLocalType(c, n, InGenericConstraint)
    wantDot c, n
  of ParamY, LetY, VarY, ConstY, CursorY, ResultY, FldY:
    let beforeType = c.dest.len
    if n.kind == DotToken:
      # no explicit type given:
      inc n # 3
      var it = Item(n: n, typ: c.types.autoType)
      if false and kind == ConstY:
        withNewScope c:
          semConstExpr c, it # 4
      else:
        semExpr c, it # 4
      n = it.n
      insertType c, it.typ, beforeType
    else:
      let typ = semLocalType(c, n) # 3
      if n.kind == DotToken:
        # empty value
        takeToken c, n
      else:
        var it = Item(n: n, typ: typ)
        if false and kind == ConstY:
          withNewScope c:
            semConstExpr c, it # 4
        else:
          semExpr c, it # 4
        n = it.n
        patchType c, it.typ, beforeType
  else:
    assert false, "bug"
  c.addSym delayed
  wantParRi c, n
  publish c, delayed.s.name, declStart

proc semLocal(c: var SemContext; it: var Item; kind: SymKind) =
  let info = it.n.info
  semLocal c, it.n, kind
  producesVoid c, info, it.typ

proc addXint(c: var SemContext; x: xint; info: PackedLineInfo) =
  var err = false
  let val = asSigned(x, err)
  if not err:
    c.dest.add intToken(pool.integers.getOrIncl(val), info)
  else:
    let val = asUnsigned(x, err)
    if not err:
      c.dest.add uintToken(pool.uintegers.getOrIncl(val), info)
    else:
      c.buildErr info, "enum value not a constant expression"

proc evalConstIntExpr(c: var SemContext; n: var Cursor; expected: TypeCursor): xint =
  let beforeExpr = c.dest.len
  var x = Item(n: n, typ: expected)
  semExpr c, x
  n = x.n
  result = evalOrdinal(c, cursorAt(c.dest, beforeExpr))
  endRead c.dest

proc semEnumField(c: var SemContext; n: var Cursor; state: var EnumTypeState) =
  let declStart = c.dest.len
  takeToken c, n
  let delayed = handleSymDef(c, n, EfldY) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1
  var crucial = default CrucialPragma
  semPragmas c, n, crucial, EfldY # 2
  if state.isBoolType and crucial.magic.len == 0:
    # bool type, set magic to fields if unset
    if state.thisValue == zero():
      crucial.magic = "false"
    else:
      crucial.magic = "true"
  if crucial.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, crucial
  if n.kind == DotToken or n.kind == Symbol:
    if state.isBoolType:
      c.dest.addParLe(BoolT, n.info)
      c.dest.addParRi()
    else:
      c.dest.add symToken(state.enumType, n.info)
    inc n # 3
  else:
    c.buildErr n.info, "enum field's type must be empty"

  if n.kind == DotToken:
    # empty value
    c.addXint state.thisValue, c.dest[declStart].info
    inc n
  else:
    let explicitValue = evalConstIntExpr(c, n, c.types.autoType) # 4
    if explicitValue != state.thisValue:
      state.hasHole = true
      state.thisValue = explicitValue
  c.addSym delayed
  wantParRi c, n
  publish c, delayed.s.name, declStart

proc semGenericParam(c: var SemContext; n: var Cursor) =
  if n == "typevar":
    semLocal c, n, TypevarY
  else:
    buildErr c, n.info, "expected 'typevar'"

proc semGenericParams(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    takeToken c, n
  elif n == "typevars":
    inc c.routine.inGeneric
    takeToken c, n
    while n.kind != ParRi:
      semGenericParam c, n
    wantParRi c, n
  elif n == $InvokeT:
    takeTree c, n
  else:
    buildErr c, n.info, "expected '.' or 'typevars'"

proc semParam(c: var SemContext; n: var Cursor) =
  if n == "param":
    semLocal c, n, ParamY
  else:
    buildErr c, n.info, "expected 'param'"

proc semParams(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    takeToken c, n
  elif n == "params":
    takeToken c, n
    while n.kind != ParRi:
      semParam c, n
    wantParRi c, n
  else:
    buildErr c, n.info, "expected '.' or 'params'"

proc addReturnResult(c: var SemContext; resId: SymId; info: PackedLineInfo) =
  if resId != SymId(0):
    assert c.dest[c.dest.len-1].kind == ParRi
    c.dest.shrink c.dest.len-1 # remove the ParRi
    # maybe add `return result`:
    buildTree(c.dest, RetS, info):
      c.dest.addSymUse resId, info
    c.dest.addParRi() # add it back

proc semBorrow(c: var SemContext; fn: StrId; beforeParams: int) =
  let signature = cursorAt(c.dest, beforeParams)
  var procBody = genBorrowedProcBody(c, fn, signature, signature.info)
  endRead(c.dest)
  var n = cursorAt(procBody, 0)
  takeToken c, n # `(stmts`
  var it = Item(n: n, typ: c.types.autoType)
  semProcBody c, it

proc semProc(c: var SemContext; it: var Item; kind: SymKind; pass: PassKind) =
  let info = it.n.info
  let declStart = c.dest.len
  takeToken c, it.n
  let symId = declareOverloadableSym(c, it, kind)

  let beforeExportMarker = c.dest.len
  wantExportMarker c, it.n
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    buildErr c, it.n.info, "TR pattern not implemented"
    skip it.n
  c.routine = createSemRoutine(kind, c.routine)
  # 'break' and 'continue' are valid in a template regardless of whether we
  # really have a loop or not:
  if kind == TemplateY:
    inc c.routine.inLoop
    inc c.routine.inGeneric

  try:
    c.openScope() # open parameter scope
    semGenericParams c, it.n
    let beforeParams = c.dest.len
    semParams c, it.n
    c.routine.returnType = semReturnType(c, it.n)
    var crucial = default CrucialPragma
    semPragmas c, it.n, crucial, kind
    if crucial.hasVarargs.isValid:
      addVarargsParameter c, beforeParams, crucial.hasVarargs
    if crucial.magic.len > 0:
      exportMarkerBecomesNifTag c, beforeExportMarker, crucial
    if it.n.kind == DotToken:
      takeToken c, it.n
    else:
      buildErr c, it.n.info, "`effects` must be empty"
      skip it.n

    publishSignature c, symId, declStart
    if it.n.kind != DotToken:
      case pass
      of checkGenericInst:
        if it.n != "stmts":
          error "(stmts) expected, but got ", it.n
        c.openScope() # open body scope
        takeToken c, it.n
        semProcBody c, it
        c.closeScope() # close body scope
        c.closeScope() # close parameter scope
      of checkBody:
        if it.n != "stmts":
          error "(stmts) expected, but got ", it.n
        c.openScope() # open body scope
        takeToken c, it.n
        let resId = declareResult(c, it.n.info)
        semProcBody c, it
        c.closeScope() # close body scope
        c.closeScope() # close parameter scope
        addReturnResult c, resId, it.n.info
      of checkSignatures:
        c.takeTree it.n
        c.closeScope() # close parameter scope
      of checkConceptProc:
        c.closeScope() # close parameter scope
        if it.n.kind == DotToken:
          inc it.n
        else:
          c.buildErr it.n.info, "inside a `concept` a routine cannot have a body"
          skip it.n
    else:
      if Borrow in crucial.flags and pass in {checkGenericInst, checkBody}:
        if kind notin {ProcY, FuncY, ConverterY, TemplateY, MethodY}:
          c.buildErr it.n.info, ".borrow only valid for proc, func, converter, template or method"
        else:
          semBorrow(c, symToIdent(symId), beforeParams)
        inc it.n # skip DotToken
      else:
        takeToken c, it.n
      c.closeScope() # close parameter scope
  finally:
    c.routine = c.routine.parent
  wantParRi c, it.n
  producesVoid c, info, it.typ
  publish c, symId, declStart

proc semExprSym(c: var SemContext; it: var Item; s: Sym; start: int; flags: set[SemFlag]) =
  it.kind = s.kind
  let expected = it.typ
  if s.kind == NoSym:
    if AllowUndeclared notin flags:
      var orig = createTokenBuf(1)
      orig.add c.dest[c.dest.len-1]
      c.dest.shrink c.dest.len-1
      if pool.syms.hasId(s.name):
        c.buildErr it.n.info, "undeclared identifier: " & pool.syms[s.name], cursorAt(orig, 0)
      else:
        c.buildErr it.n.info, "undeclared identifier", cursorAt(orig, 0)
    it.typ = c.types.autoType
  elif s.kind == CchoiceY:
    if KeepMagics notin flags and c.routine.kind != TemplateY:
      c.buildErr it.n.info, "ambiguous identifier"
    it.typ = c.types.autoType
  elif s.kind in {TypeY, TypevarY}:
    let typeStart = c.dest.len
    c.dest.buildTree TypedescT, it.n.info:
      let symStart = c.dest.len
      c.dest.add symToken(s.name, it.n.info)
      semTypeSym c, s, it.n.info, symStart, InLocalDecl
    it.typ = typeToCursor(c, typeStart)
    c.dest.shrink typeStart
    commonType c, it, start, expected
  else:
    let res = declToCursor(c, s)
    if KeepMagics notin flags:
      maybeInlineMagic c, res
    if res.status == LacksNothing:
      var n = res.decl
      if s.kind.isLocal or s.kind == EfldY:
        skipToLocalType n
      elif s.kind.isRoutine:
        skipToParams n
      elif s.kind == LabelY:
        discard
      elif s.kind == ModuleY:
        if AllowModuleSym notin flags:
          c.buildErr it.n.info, "module symbol '" & pool.syms[s.name] & "' not allowed in this context"
      else:
        # XXX enum field?
        assert false, "not implemented"
      it.typ = n
      commonType c, it, start, expected
    else:
      c.buildErr it.n.info, "could not load symbol: " & pool.syms[s.name] & "; errorCode: " & $res.status
      it.typ = c.types.autoType

proc semLocalTypeExpr(c: var SemContext, it: var Item) =
  let val = semLocalType(c, it.n)
  let start = c.dest.len
  c.dest.buildTree TypedescT, it.n.info:
    c.dest.addSubtree val
  it.typ = typeToCursor(c, start)
  c.dest.shrink start

proc semAsgn(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  var a = Item(n: it.n, typ: c.types.autoType)
  semExpr c, a # infers type of `left-hand-side`
  semExpr c, a # ensures type compatibility with `left-hand-side`
  it.n = a.n
  wantParRi c, it.n
  producesVoid c, info, it.typ

proc semEmit(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  while it.n.kind != ParRi:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a
    it.n = a.n
  wantParRi c, it.n
  producesVoid c, info, it.typ

proc semDiscard(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a
    it.n = a.n
    if classifyType(c, it.typ) == VoidT:
      buildErr c, it.n.info, "expression of type `" & typeToString(it.typ) & "` must not be discarded"
  wantParRi c, it.n
  producesVoid c, info, it.typ

proc semStmtBranch(c: var SemContext; it: var Item) =
  # handle statements that could be expressions
  case classifyType(c, it.typ)
  of AutoT:
    semExpr c, it
  of VoidT:
    # performs discard check:
    semStmt c, it.n
  else:
    var ex = Item(n: it.n, typ: it.typ)
    let start = c.dest.len
    semExpr c, ex
    # this is handled by commonType, since it has to be done deeply:
    #if classifyType(c, ex.typ) == VoidT:
    #  # allow statement in expression context if it is noreturn
    #  let ignore = isNoReturn(cursorAt(c.dest, start))
    #  endRead(c.dest)
    #  if not ignore:
    #    typeMismatch(c, it.n.info, ex.typ, it.typ)
    commonType(c, ex, start, it.typ)
    it.n = ex.n

proc semIf(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if it.n.substructureKind == ElifS:
    while it.n.substructureKind == ElifS:
      takeToken c, it.n
      semBoolExpr c, it.n
      withNewScope c:
        semStmtBranch c, it
      wantParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `elif` inside `if` expected"
  if it.n.substructureKind == ElseS:
    takeToken c, it.n
    withNewScope c:
      semStmtBranch c, it
    wantParRi c, it.n
  wantParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semWhen(c: var SemContext; it: var Item) =
  let start = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var leaveUnresolved = false
  if it.n.substructureKind == ElifS:
    while it.n.substructureKind == ElifS:
      takeToken c, it.n
      let condStart = c.dest.len
      semConstBoolExpr c, it.n
      let condValue = cursorAt(c.dest, condStart).exprKind
      endRead(c.dest)
      if not leaveUnresolved:
        if condValue == TrueX:
          c.dest.shrink start
          semExpr c, it
          skipParRi it.n # finish elif
          skipToEnd it.n
          return
        elif condValue != FalseX:
          # erroring/unresolved condition, leave entire statement as unresolved
          leaveUnresolved = true
      takeTree c, it.n
      wantParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `elif` inside `if` expected"
  if it.n.substructureKind == ElseS:
    takeToken c, it.n
    if not leaveUnresolved:
      c.dest.shrink start
      semExpr c, it
      skipParRi it.n # finish else
      skipToEnd it.n
      return
    else:
      takeTree c, it.n
    wantParRi c, it.n
  wantParRi c, it.n
  if not leaveUnresolved:
    # none of the branches evaluated, output nothing
    c.dest.shrink start

proc semCaseOfValue(c: var SemContext; it: var Item; selectorType: TypeCursor;
                    seen: var seq[(xint, xint)]) =
  if it.n == "set":
    takeToken c, it.n
    while it.n.kind != ParRi:
      let info = it.n.info
      if isRangeExpr(it.n):
        inc it.n # call tag
        skip it.n # `..`
        c.dest.buildTree RangeX, it.n.info:
          let a = evalConstIntExpr(c, it.n, selectorType)
          let b = evalConstIntExpr(c, it.n, selectorType)
          if seen.doesOverlapOrIncl(a, b):
            buildErr c, info, "overlapping values"
        inc it.n # right paren of call
      elif it.n.exprKind == RangeX:
        takeToken c, it.n
        let a = evalConstIntExpr(c, it.n, selectorType)
        let b = evalConstIntExpr(c, it.n, selectorType)
        if seen.doesOverlapOrIncl(a, b):
          buildErr c, info, "overlapping values"
        wantParRi c, it.n
      else:
        let a = evalConstIntExpr(c, it.n, selectorType)
        if seen.containsOrIncl(a):
          buildErr c, info, "value already handled"
    wantParRi c, it.n
  else:
    buildErr c, it.n.info, "`set` within `of` expected"
    skip it.n

proc semCase(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  var selector = Item(n: it.n, typ: c.types.autoType)
  semExpr c, selector
  it.n = selector.n
  var seen: seq[(xint, xint)] = @[]
  if it.n.substructureKind == OfS:
    while it.n.substructureKind == OfS:
      takeToken c, it.n
      semCaseOfValue c, it, selector.typ, seen
      withNewScope c:
        semStmtBranch c, it
      wantParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `of` inside `case` expected"
  if it.n.substructureKind == ElseS:
    takeToken c, it.n
    withNewScope c:
      semStmtBranch c, it
    wantParRi c, it.n
  wantParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semForLoopVar(c: var SemContext; it: var Item; loopvarType: TypeCursor) =
  if stmtKind(it.n) == LetS:
    let declStart = c.dest.len
    takeToken c, it.n
    let delayed = handleSymDef(c, it.n, LetY)
    c.addSym delayed
    wantDot c, it.n # export marker must be empty
    wantDot c, it.n # pragmas
    copyTree c.dest, loopvarType
    skip it.n # skip over the type which might have been set already as we tend to re-sem stuff
    wantDot c, it.n # value
    wantParRi c, it.n
    publish c, delayed.s.name, declStart
  else:
    buildErr c, it.n.info, "illformed AST: `let` inside `unpackflat` expected"
    skip it.n

proc isIterator(c: var SemContext; s: SymId): bool =
  let sym = fetchSym(c, s)
  let res = declToCursor(c, sym)
  result = res.status == LacksNothing and res.decl == $IterY

proc semFor(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  var iterCall = Item(n: it.n, typ: c.types.autoType)
  let beforeCall = c.dest.len
  semExpr c, iterCall, {PreferIterators, KeepMagics}
  var isMacroLike = false
  if c.dest[beforeCall+1].kind == Symbol and c.isIterator(c.dest[beforeCall+1].symId):
    discard "fine"
  elif iterCall.typ.typeKind == UntypedT:
    isMacroLike = true
  else:
    buildErr c, it.n.info, "iterator expected"
  it.n = iterCall.n
  withNewScope c:
    case substructureKind(it.n)
    of UnpackFlatS:
      takeToken c, it.n
      if iterCall.typ.typeKind == TupleT:
        var tup = iterCall.typ
        inc tup
        while it.n.kind != ParRi and tup.kind != ParRi:
          semForLoopVar c, it, tup
          skip tup
        if it.n.kind == ParRi:
          inc it.n
          if tup.kind == ParRi:
            discard "all fine"
          else:
            buildErr c, it.n.info, "too few for loop variables"
        else:
          buildErr c, it.n.info, "too many for loop variables"
          skipToEnd it.n
      else:
        semForLoopVar c, it, iterCall.typ

      wantParRi c, it.n
    of UnpackTupS:
      # XXX To implement
      buildErr c, it.n.info, "`unpacktup` inside `for` not implemented"
      skip it.n
    else:
      buildErr c, it.n.info, "illformed AST: `unpackflat` inside `for` expected"
      skip it.n

    if isMacroLike and false:
      takeTree c.dest, it.n # don't touch the body
    else:
      inc c.routine.inLoop
      semStmt c, it.n
      dec c.routine.inLoop

  wantParRi c, it.n
  producesNoReturn c, info, it.typ

proc semReturn(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.kind == NoSym:
    buildErr c, it.n.info, "`return` only allowed within a routine"
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.routine.returnType)
    # `return` within a template refers to the caller, so
    # we allow any type here:
    if c.routine.kind == TemplateY:
      a.typ = c.types.autoType
    semExpr c, a
    it.n = a.n
  wantParRi c, it.n
  producesNoReturn c, info, it.typ

proc semYield(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.kind != IterY:
    buildErr c, it.n.info, "`yield` only allowed within an `iterator`"
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.routine.returnType)
    semExpr c, a
    it.n = a.n
  wantParRi c, it.n
  producesVoid c, info, it.typ

proc semTypePragmas(c: var SemContext; n: var Cursor; beforeExportMarker: int) =
  var crucial = default CrucialPragma
  semPragmas c, n, crucial, TypeY # 2
  if crucial.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, crucial

proc semTypeSection(c: var SemContext; n: var Cursor) =
  let declStart = c.dest.len
  takeToken c, n
  # name, export marker, generic params, pragmas, body
  let delayed = handleSymDef(c, n, TypeY) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1

  if c.phase == SemcheckSignatures or (delayed.status == OkNew and c.phase != SemcheckTopLevelSyms):
    var isGeneric: bool
    if n.kind == DotToken:
      takeToken c, n
      isGeneric = false
    else:
      openScope c
      semGenericParams c, n
      isGeneric = true

    semTypePragmas c, n, beforeExportMarker

    # body:
    if n.kind == DotToken:
      takeToken c, n
    else:
      if n.typeKind == EnumT:
        semEnumType c, n, delayed.s.name, beforeExportMarker
      else:
        semLocalTypeImpl c, n, InTypeSection
    if isGeneric:
      closeScope c
  else:
    c.takeTree n # generics
    semTypePragmas c, n, beforeExportMarker
    c.takeTree n # body

  c.addSym delayed
  wantParRi c, n
  publish c, delayed.s.name, declStart

proc semTypedBinaryArithmetic(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  semExpr c, it
  semExpr c, it
  wantParRi c, it.n
  commonType c, it, beforeExpr, typ

proc semCmp(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  var operand = Item(n: it.n, typ: typ)
  semExpr c, operand
  semExpr c, operand
  it.n = operand.n
  wantParRi c, it.n
  commonType c, it, beforeExpr, c.types.boolType

proc literal(c: var SemContext; it: var Item; literalType: TypeCursor) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let expected = it.typ
  it.typ = literalType
  commonType c, it, beforeExpr, expected

proc literalB(c: var SemContext; it: var Item; literalType: TypeCursor) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  wantParRi c, it.n
  let expected = it.typ
  it.typ = literalType
  commonType c, it, beforeExpr, expected

proc semNil(c: var SemContext; it: var Item) =
  literalB c, it, c.types.nilType

proc semTypedUnaryArithmetic(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  semExpr c, it
  wantParRi c, it.n
  commonType c, it, beforeExpr, typ

proc semArrayConstr(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  takeToken c, it.n
  if it.n.kind == ParRi:
    # empty array
    if it.typ.typeKind in {AutoT, VoidT}:
      buildErr c, it.n.info, "empty array needs a specified type"
    wantParRi c, it.n
    return
  var elem = Item(n: it.n, typ: c.types.autoType)
  case it.typ.typeKind
  of ArrayT: # , SeqT, OpenArrayT
    var arr = it.typ
    inc arr
    elem.typ = arr
  of AutoT: discard
  else:
    buildErr c, it.n.info, "invalid expected type for array constructor: " & typeToString(it.typ)
  # XXX index types, `index: value` etc not implemented
  semExpr c, elem
  var count = 1
  while elem.n.kind != ParRi:
    semExpr c, elem
    inc count
  it.n = elem.n
  wantParRi c, it.n
  let typeStart = c.dest.len
  c.dest.buildTree ArrayT, it.n.info:
    c.dest.addSubtree elem.typ
    c.dest.addParLe(RangeT, it.n.info)
    c.dest.addSubtree c.types.intType
    c.dest.addIntLit(0, it.n.info)
    c.dest.addIntLit(count - 1, it.n.info)
    c.dest.addParRi()
  let expected = it.typ
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  commonType c, it, exprStart, expected

proc semSetConstr(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  takeToken c, it.n
  if it.n.kind == ParRi:
    # empty set
    if it.typ.typeKind in {AutoT, VoidT}:
      buildErr c, it.n.info, "empty set needs a specified type"
    wantParRi c, it.n
    return
  var elem = Item(n: it.n, typ: c.types.autoType)
  case it.typ.typeKind
  of SetT:
    var t = it.typ
    inc t
    elem.typ = t
  of AutoT: discard
  else:
    buildErr c, it.n.info, "invalid expected type for set constructor: " & typeToString(it.typ)
  var elemStart = c.dest.len
  var elemInfo = elem.n.info
  while elem.n.kind != ParRi:
    if isRangeExpr(elem.n):
      inc elem.n # call tag
      skip elem.n # `..`
      c.dest.buildTree RangeX, elem.n.info:
        elemStart = c.dest.len
        elemInfo = elem.n.info
        semExpr c, elem
        semExpr c, elem
      inc elem.n # right paren of call
    elif elem.n.exprKind == RangeX:
      skip elem.n # resem elements?
    else:
      semExpr c, elem
  # XXX allow unresolved types
  if not isOrdinalType(elem.typ, allowEnumWithHoles = true):
    c.buildErr elemInfo, "set element type must be ordinal"
  #elif elem.typ.typeKind == IntT and c.dest[elemStart].kind == IntLit:
  #  set to range of 0..<DefaultSetElements
  else:
    let length = lengthOrd(c, elem.typ)
    if length.isNaN or length > MaxSetElements:
      c.buildErr elemInfo, "type " & typeToString(elem.typ) & " is too large to be a set element type"
  it.n = elem.n
  wantParRi c, it.n
  let typeStart = c.dest.len
  c.dest.buildTree SetT, it.n.info:
    c.dest.addSubtree elem.typ
  let expected = it.typ
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  commonType c, it, exprStart, expected

proc semSuf(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  takeToken c, it.n
  var num = Item(n: it.n, typ: c.types.autoType)
  semExpr c, num
  it.n = num.n
  if it.n.kind != StringLit:
    c.buildErr it.n.info, "string literal expected for suf"
    skip it.n
    return
  let expected = it.typ
  case pool.strings[it.n.litId]
  of "i": it.typ = c.types.intType
  of "i8": it.typ = c.types.int8Type
  of "i16": it.typ = c.types.int16Type
  of "i32": it.typ = c.types.int32Type
  of "i64": it.typ = c.types.int64Type
  of "u": it.typ = c.types.uintType
  of "u8": it.typ = c.types.uint8Type
  of "u16": it.typ = c.types.uint16Type
  of "u32": it.typ = c.types.uint32Type
  of "u64": it.typ = c.types.uint64Type
  of "f": it.typ = c.types.floatType
  of "f32": it.typ = c.types.float32Type
  of "f64": it.typ = c.types.float64Type
  of "R": it.typ = c.types.stringType
  else:
    c.buildErr it.n.info, "unknown suffix: " & pool.strings[it.n.litId]
  takeToken c, it.n # suffix
  wantParRi c, it.n # right paren
  commonType c, it, exprStart, expected

proc semTupleConstr(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  let origExpected = it.typ
  takeToken c, it.n
  if it.n.kind == ParRi:
    wantParRi c, it.n
    it.typ = c.types.emptyTupletype
    commonType c, it, exprStart, origExpected
    return
  var expected = origExpected
  var doExpected = expected.typeKind == TupleT
  if doExpected:
    inc expected # skip tag, now at fields
  let named = it.n.exprKind == KvX
  var typ = createTokenBuf(32)
  typ.add parLeToken(pool.tags.getOrIncl($TupleT), it.n.info)
  var i = 0
  while it.n.kind != ParRi:
    typ.add parLeToken(pool.tags.getOrIncl($FldS), it.n.info) # start field
    if named:
      if it.n.exprKind != KvX:
        c.buildErr it.n.info, "expected field name for named tuple constructor"
      else:
        takeToken c, it.n
        typ.add it.n # add name
        takeToken c, it.n
    else:
      typ.add identToken(pool.strings.getOrIncl("Field" & $i), it.n.info)
      inc i
    typ.addDotToken() # export marker
    typ.addDotToken() # pragmas
    var elem = Item(n: it.n, typ: c.types.autoType)
    if doExpected:
      let fld = asLocal(expected)
      elem.typ = fld.typ
      skip expected
      if expected.kind == ParRi:
        # happens if expected tuple type has less fields than constructor
        doExpected = false
    semExpr c, elem
    it.n = elem.n
    if named:
      # should be KvX
      wantParRi c, it.n
    typ.addSubtree elem.typ # type
    typ.addDotToken() # value
    typ.addParRi() # end field
  wantParRi c, it.n
  typ.addParRi()
  let typeStart = c.dest.len
  var t = typ.cursorAt(0)
  semTupleType(c, t)
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  commonType c, it, exprStart, origExpected

proc getDottedIdent(n: var Cursor): string =
  let isError = n.kind == ParLe and n.tagId == ErrT
  if isError:
    inc n
  if n.kind == ParLe and n == $DotX:
    inc n
    result = getDottedIdent(n)
    let s = getIdent(n)
    if s == StrId(0) or result == "":
      result = ""
    else:
      result.add(pool.strings[s])
    skipParRi n
  else:
    # treat as atom
    let s = getIdent(n)
    if s == StrId(0):
      result = ""
    else:
      result = pool.strings[s]
  if isError:
    skipToEnd n

proc semDefined(c: var SemContext; it: var Item) =
  inc it.n
  let info = it.n.info
  let orig = it.n
  let name = getDottedIdent(it.n)
  skipParRi it.n
  if name == "":
    c.buildErr info, "invalid expression for defined: " & toString(orig, false), orig
  else:
    let isDefined = name in c.g.config.defines
    let beforeExpr = c.dest.len
    c.dest.addParLe(if isDefined: TrueX else: FalseX, it.n.info)
    c.dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, it, beforeExpr, expected

proc isDeclared(c: var SemContext; name: StrId): bool =
  var scope = c.currentScope
  while scope != nil:
    if name in scope.tab:
      return true
    scope = scope.up
  result = name in c.importTab

proc semDeclared(c: var SemContext; it: var Item) =
  inc it.n
  let info = it.n.info
  let orig = it.n
  # XXX maybe always type the argument and check for Symbol/errored Ident instead
  let isError = it.n.kind == ParLe and it.n.tagId == ErrT
  if isError:
    inc it.n
  # does not consider module quoted symbols for now
  let nameId = getIdent(it.n)
  if isError:
    skipToEnd it.n
  skipParRi it.n
  if nameId == StrId(0):
    c.buildErr info, "invalid expression for declared: " & toString(orig, false), orig
  else:
    let isDeclared = isDeclared(c, nameId)
    let beforeExpr = c.dest.len
    c.dest.addParLe(if isDeclared: TrueX else: FalseX, it.n.info)
    c.dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, it, beforeExpr, expected

proc semIsMainModule(c: var SemContext; it: var Item) =
  let info = it.n.info
  inc it.n
  skipParRi it.n
  let isMainModule = IsMain in c.moduleFlags
  let beforeExpr = c.dest.len
  c.dest.addParLe(if isMainModule: TrueX else: FalseX, info)
  c.dest.addParRi()
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, it, beforeExpr, expected

proc semBuiltinSubscript(c: var SemContext; lhs: Item; it: var Item) =
  # it.n is after lhs, at args
  if lhs.n.kind == Symbol and lhs.kind == TypeY and
      getTypeSection(lhs.n.symId).typevars == "typevars":
    # lhs is a generic type symbol, this is a generic invocation
    # treat it as a type expression to call semInvoke
    semLocalTypeExpr c, it
    return
  # XXX also check for proc generic instantiation, including symchoice

  # build call:
  var callBuf = createTokenBuf(16)
  callBuf.addParLe(CallX, lhs.n.info)
  callBuf.add identToken(pool.strings.getOrIncl("[]"), lhs.n.info)
  callBuf.addSubtree lhs.n
  while it.n.kind != ParRi:
    callBuf.takeTree it.n
  callBuf.addParRi()
  skipParRi it.n
  var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
  semCall c, call, SubscriptCall
  it.typ = call.typ

proc semSubscript(c: var SemContext; it: var Item) =
  var n = it.n
  inc n # tag
  var lhsBuf = createTokenBuf(4)
  swap c.dest, lhsBuf
  var lhs = Item(n: n, typ: c.types.autoType)
  semExpr c, lhs, {KeepMagics}
  swap c.dest, lhsBuf
  it.n = lhs.n
  lhs.n = cursorAt(lhsBuf, 0)
  semBuiltinSubscript(c, lhs, it)

proc semDconv(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var destType = semLocalType(c, it.n)
  var x = Item(n: it.n, typ: c.types.autoType)
  let beforeArg = c.dest.len
  semExpr c, x
  it.n = x.n

  var isDistinct = false
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(x.typ, isDistinct)
  if not isDistinct:
    shrink c.dest, beforeExpr
    c.buildErr info, "`dconv` operation only valid for type conversions involving `distinct` types"
  else:
    var arg = Item(n: cursorAt(c.dest, beforeArg), typ: srcBase)
    var m = createMatch(addr c)
    typematch m, destBase, arg
    endRead c.dest
    if m.err:
      when defined(debug):
        shrink c.dest, beforeExpr
        c.dest.addErrorMsg m
      else:
        c.typeMismatch info, x.typ, destType
    else:
      # distinct type conversions can also involve conversions
      # between different integer sizes or object types and then
      # `m.args` contains these so use them here:
      shrink c.dest, beforeArg
      c.dest.add m.args
  it.n = x.n
  wantParRi c, it.n
  commonType c, it, beforeExpr, destType

proc whichPass(c: SemContext): PassKind =
  result = if c.phase == SemcheckSignatures: checkSignatures else: checkBody

template toplevelGuard(c: var SemContext; body: untyped) =
  if c.phase == SemcheckBodies:
    body
  else:
    c.takeTree it.n

template procGuard(c: var SemContext; body: untyped) =
  if c.phase in {SemcheckSignatures, SemcheckBodies}:
    body
  else:
    c.takeTree it.n

proc semExpr(c: var SemContext; it: var Item; flags: set[SemFlag] = {}) =
  case it.n.kind
  of IntLit:
    literal c, it, c.types.intType
  of UIntLit:
    literal c, it, c.types.uintType
  of FloatLit:
    literal c, it, c.types.floatType
  of StringLit:
    literal c, it, c.types.stringType
  of CharLit:
    literal c, it, c.types.charType
  of Ident:
    let start = c.dest.len
    let s = semIdent(c, it.n)
    semExprSym c, it, s, start, flags
  of Symbol:
    let start = c.dest.len
    let s = fetchSym(c, it.n.symId)
    takeToken c, it.n
    semExprSym c, it, s, start, flags
  of ParLe:
    case exprKind(it.n)
    of QuotedX:
      let start = c.dest.len
      let s = semQuoted(c, it.n)
      semExprSym c, it, s, start, flags
    of NoExpr:
      case stmtKind(it.n)
      of NoStmt:
        case typeKind(it.n)
        of NoType:
          if pool.tags[it.n.tag] == "err":
            c.takeTree it.n
          else:
            buildErr c, it.n.info, "expression expected"
            skip it.n
        of ObjectT, EnumT, DistinctT, ConceptT:
          buildErr c, it.n.info, "expression expected"
          skip it.n
        of IntT, FloatT, CharT, BoolT, UIntT, VoidT, StringT, NilT, AutoT, SymKindT,
            PtrT, RefT, MutT, OutT, LentT, SinkT, UncheckedArrayT, SetT, StaticT, TypedescT,
            TupleT, ArrayT, RangeT, VarargsT, ProcT, IterT, UntypedT, TypedT, CstringT, PointerT:
          # every valid local type expression
          semLocalTypeExpr c, it
        of OrT, AndT, NotT, InvokeT:
          # should be handled in respective expression kinds
          discard
      of ProcS:
        procGuard c:
          semProc c, it, ProcY, whichPass(c)
      of FuncS:
        procGuard c:
          semProc c, it, FuncY, whichPass(c)
      of IterS:
        procGuard c:
          semProc c, it, IterY, whichPass(c)
      of ConverterS:
        procGuard c:
          semProc c, it, ConverterY, whichPass(c)
      of MethodS:
        procGuard c:
          semProc c, it, MethodY, whichPass(c)
      of TemplateS:
        procGuard c:
          semProc c, it, TemplateY, whichPass(c)
      of MacroS:
        procGuard c:
          semProc c, it, MacroY, whichPass(c)
      of WhileS:
        toplevelGuard c:
          semWhile c, it
      of VarS:
        toplevelGuard c:
          semLocal c, it, VarY
      of LetS:
        toplevelGuard c:
          semLocal c, it, LetY
      of CursorS:
        toplevelGuard c:
          semLocal c, it, CursorY
      of ResultS:
        toplevelGuard c:
          semLocal c, it, ResultY
      of ConstS:
        toplevelGuard c:
          semLocal c, it, ConstY
      of StmtsS: semStmtsExpr c, it
      of BreakS:
        toplevelGuard c:
          semBreak c, it
      of ContinueS:
        toplevelGuard c:
          semContinue c, it
      of CallS, CmdS:
        toplevelGuard c:
          semCall c, it
      of IncludeS: semInclude c, it
      of ImportS: semImport c, it
      of ImportExceptS: semImportExcept c, it
      of FromImportS: semFromImport c, it
      of AsgnS:
        toplevelGuard c:
          semAsgn c, it
      of EmitS:
        toplevelGuard c:
          semEmit c, it
      of DiscardS:
        toplevelGuard c:
          semDiscard c, it
      of IfS:
        toplevelGuard c:
          semIf c, it
      of WhenS:
        toplevelGuard c:
          semWhen c, it
      of RetS:
        toplevelGuard c:
          semReturn c, it
      of YieldS:
        toplevelGuard c:
          semYield c, it
      of TypeS:
        let info = it.n.info
        semTypeSection c, it.n
        producesVoid c, info, it.typ
      of BlockS:
        toplevelGuard c:
          semBlock c, it
      of CaseS:
        toplevelGuard c:
          semCase c, it
      of ForS:
        toplevelGuard c:
          semFor c, it
      of ExportS:
        # XXX ignored for now
        skip it.n
    of FalseX, TrueX:
      literalB c, it, c.types.boolType
    of InfX, NegInfX, NanX:
      literalB c, it, c.types.floatType
    of AndX, OrX:
      takeToken c, it.n
      semBoolExpr c, it.n
      semBoolExpr c, it.n
      wantParRi c, it.n
    of NotX:
      takeToken c, it.n
      semBoolExpr c, it.n
      wantParRi c, it.n
    of ParX:
      takeToken c, it.n
      semExpr c, it
      wantParRi c, it.n
    of CallX, CmdX, CallStrLitX, InfixX, PrefixX:
      toplevelGuard c:
        semCall c, it
    of DotX:
      toplevelGuard c:
        semDot c, it, flags
    of DconvX:
      toplevelGuard c:
        semDconv c, it
    of EqX, NeqX, LeX, LtX:
      semCmp c, it
    of AshrX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX:
      semTypedBinaryArithmetic c, it
    of BitnotX, NegX:
      semTypedUnaryArithmetic c, it
    of AconstrX:
      semArrayConstr c, it
    of SetX:
      semSetConstr c, it
    of SufX:
      semSuf c, it
    of TupleConstrX:
      semTupleConstr c, it
    of DefinedX:
      semDefined c, it
    of DeclaredX:
      semDeclared c, it
    of IsMainModuleX:
      semIsMainModule c, it
    of AtX:
      semSubscript c, it
    of UnpackX:
      takeToken c, it.n
      wantParRi c, it.n
    of OchoiceX, CchoiceX:
      takeTree c, it.n
    of HaddrX, HderefX:
      takeToken c, it.n
      # this is exactly what we need here as these operators have the same
      # type as the operand:
      semExpr c, it
      wantParRi c, it.n
    of CastX:
      semCast c, it
    of NilX:
      semNil c, it
    of DerefX, PatX, AddrX, SizeofX, OconstrX, KvX,
       ConvX, RangeX, RangesX,
       OconvX, HconvX,
       CompilesX, HighX, LowX, TypeofX:
      # XXX To implement
      buildErr c, it.n.info, "to implement: " & $exprKind(it.n)
      takeToken c, it.n
      wantParRi c, it.n

  of ParRi, EofToken, SymbolDef, UnknownToken, DotToken:
    buildErr c, it.n.info, "expression expected"

proc reportErrors(c: var SemContext): int =
  let errTag = pool.tags.getOrIncl("err")
  var i = 0
  var r = Reporter(verbosity: 2, noColors: not useColors())
  result = 0
  while i < c.dest.len:
    if c.dest[i].kind == ParLe and c.dest[i].tagId == errTag:
      inc result
      let info = c.dest[i].info
      inc i
      # original expression, optional:
      if c.dest[i].kind == DotToken:
        inc i
      else:
        let x = cursorAt(c.dest, i)
        inc i, span(x)
        endRead(c.dest)
      # instantiation contexts:
      while c.dest[i].kind == DotToken:
        r.trace infoToStr(c.dest[i].info), "instantiation from here"
        inc i
      # error message:
      assert c.dest[i].kind == StringLit
      r.error infoToStr(info), pool.strings[c.dest[i].litId]
      inc i
    else:
      inc i

proc writeOutput(c: var SemContext; outfile: string) =
  #var b = nifbuilder.open(outfile)
  #b.addHeader "nimony", "nim-sem"
  #b.addRaw toString(c.dest)
  #b.close()
  writeFile outfile, "(.nif24)\n" & toString(c.dest)
  createIndex outfile, true

proc phaseX(c: var SemContext; n: Cursor; x: SemPhase): TokenBuf =
  assert n == "stmts"
  c.phase = x
  var n = n
  takeToken c, n
  while n.kind != ParRi:
    semStmt c, n
  wantParRi c, n
  result = move c.dest

proc semcheck*(infile, outfile: string; config: sink NifConfig; moduleFlags: set[ModuleFlag];
               commandLineArgs: sink string; canSelfExec: bool) =
  var n0 = setupProgram(infile, outfile)
  var c = SemContext(
    dest: createTokenBuf(),
    types: createBuiltinTypes(),
    thisModuleSuffix: prog.main,
    moduleFlags: moduleFlags,
    g: ProgramContext(config: config),
    phase: SemcheckTopLevelSyms,
    routine: SemRoutine(kind: NoSym),
    commandLineArgs: commandLineArgs,
    canSelfExec: canSelfExec)
  c.currentScope = Scope(tab: initTable[StrId, seq[Sym]](), up: nil, kind: ToplevelScope)
  # XXX could add self module symbol here

  assert n0 == "stmts"
  #echo "PHASE 1"
  var n1 = phaseX(c, n0, SemcheckTopLevelSyms)
  #echo "PHASE 2: ", toString(n1)
  var n2 = phaseX(c, beginRead(n1), SemcheckSignatures)

  #echo "PHASE 3: ", toString(n2)
  var n = beginRead(n2)
  c.phase = SemcheckBodies
  takeToken c, n
  while n.kind != ParRi:
    semStmt c, n
  instantiateGenerics c
  for _, val in mpairs(c.instantiatedTypes):
    let s = fetchSym(c, val)
    let res = declToCursor(c, s)
    if res.status == LacksNothing:
      c.dest.copyTree res.decl
  wantParRi c, n
  if reportErrors(c) == 0:
    writeOutput c, outfile
  else:
    quit 1

#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Semantic checking:
## Most important task is to turn identifiers into symbols and to perform
## type checking.

import std / [tables, sets, syncio, formatfloat, assertions, strutils]
from std/os import changeFileExt, getCurrentDir
include nifprelude
import nimony_model, symtabs, builtintypes, decls, symparser, asthelpers,
  programs, sigmatch, magics, reporters, nifconfig, nifindexes,
  intervals, xints, typeprops,
  semdata, sembasics, semos, expreval, semborrow, enumtostr, derefs, sizeof, renderer,
  semuntyped, contracts, vtables_frontend, module_plugins

import ".." / gear2 / modnames
import ".." / models / [tags, nifindex_tags]

proc semStmt(c: var SemContext; n: var Cursor; isNewScope: bool)
proc semStmtBranch(c: var SemContext; it: var Item; isNewScope: bool)

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
      of ElifU:
        inc it
        skip it # condition
        checkBranch(it)
        skip it
        skipParRi it
      of ElseU:
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
      of OfU:
        inc it
        skip it # ranges
        checkBranch(it)
        skip it
        skipParRi it
      of ElifU:
        inc it
        skip it # condition
        checkBranch(it)
        skip it
        skipParRi it
      of ElseU:
        inc it
        checkBranch(it)
        skip it
        skipParRi it
      else:
        error "illformed AST: `of`, `elif` or `else` inside `case` expected, got ", it
    # all branches are discardable
    result = true
  of TryS:
    inc it # tag
    checkBranch(it)
    while it.substructureKind == ExceptU:
      inc it # tag
      skip it # `Exception as e` part
      checkBranch(it)
      skipParRi it
    # ignore finally part
    # all branches are discardable
    result = true
  of CallKindsS:
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
            if noreturnOnly: {NoreturnP}
            else: {DiscardableP, NoreturnP}
          while decl.kind != ParRi:
            if pragmaKind(decl) in accepted:
              return true
            skip decl
    result = false
  of RetS, BreakS, ContinueS, RaiseS:
    result = true
  else:
    result = false

proc isNoReturn(n: Cursor): bool {.inline.} =
  result = implicitlyDiscardable(n, noreturnOnly = true)

proc requestRoutineInstance(c: var SemContext; origin: SymId;
                            typeArgs: TokenBuf;
                            inferred: Table[SymId, Cursor];
                            info: PackedLineInfo): ProcInstance

proc tryConverterMatch(c: var SemContext; convMatch: var Match; f: TypeCursor, arg: CallArg): bool

type
  SemFlag = enum
    KeepMagics
    AllowOverloads
    PreferIterators
    AllowUndeclared
    AllowModuleSym
    AllowEmpty
    InTypeContext

  TransformedCallSource = enum
    RegularCall, MethodCall,
    DotCall, SubscriptCall,
    DotAsgnCall, SubscriptAsgnCall

proc semExpr(c: var SemContext; it: var Item; flags: set[SemFlag] = {})

proc semCall(c: var SemContext; it: var Item; flags: set[SemFlag]; source: TransformedCallSource = RegularCall)

proc commonType(c: var SemContext; it: var Item; argBegin: int; expected: TypeCursor) =
  if typeKind(expected) == AutoT:
    return

  var arg = Item(n: cursorAt(c.dest, argBegin), typ: it.typ)
  var done = false
  if typeKind(arg.typ) == VoidT and isNoReturn(arg.n):
    # noreturn allowed in expression context
    # maybe use sem flags to restrict this to statement branches
    done = true
  elif typeKind(arg.typ) == AutoT and not isEmptyContainer(arg.n):
    # auto is valid for empty container, will be handled below
    it.typ = expected
    done = true
  endRead(c.dest)
  if done:
    return

  var m = createMatch(addr c)
  let info = arg.n.info
  typematch m, expected, arg
  if m.err:
    # try converter
    var convMatch = default(Match)
    let convArg = CallArg(n: arg.n, typ: arg.typ)
    if tryConverterMatch(c, convMatch, expected, convArg):
      shrink c.dest, argBegin
      c.dest.add parLeToken(HcallX, info)
      c.dest.add symToken(convMatch.fn.sym, info)
      if convMatch.genericConverter:
        buildTypeArgs(convMatch)
        if convMatch.err:
          # adding type args errored
          buildErr c, info, getErrorMsg(convMatch)
        else:
          let inst = c.requestRoutineInstance(convMatch.fn.sym, convMatch.typeArgs, convMatch.inferred, arg.n.info)
          c.dest[c.dest.len-1].setSymId inst.targetSym
      # ignore checkEmptyArg case, probably environment is generic
      c.dest.add convMatch.args
      c.dest.addParRi()
      it.typ = expected
    else:
      shrink c.dest, argBegin
      c.typeMismatch info, it.typ, expected
  else:
    shrink c.dest, argBegin
    if m.checkEmptyArg and cursorAt(m.args, 0).exprKind in CallKinds:
      # empty seq call, semcheck
      var call = Item(n: beginRead(m.args), typ: c.types.autoType)
      semCall c, call, {}
    else:
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

# ------------------ include/import handling ------------------------

include semimport

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
      if NoinitP in c.routine.pragmas:
        c.dest.add parLeToken(PragmasU, info)
        c.dest.add parLeToken(NoinitP, info)
        c.dest.addParRi()
        c.dest.addParRi()
      else:
        c.dest.addDotToken() # pragmas
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

proc instToString(buf: TokenBuf; start: int): string =
  # canonicalized string of invocation
  # could directly build hash too but this is easier to debug
  var b = nifbuilder.open((buf.len - start) * 20, compact = true)
  for n in start ..< buf.len:
    let k = buf[n].kind
    case k
    of DotToken: b.addEmpty()
    of Ident: b.addIdent(pool.strings[buf[n].litId])
    of Symbol:
      # for nested instantiations i.e. `Foo[Bar[int]]`
      let s = pool.syms[buf[n].symId]
      if isInstantiation(s):
        b.addSymbol(removeModule(s))
      else:
        b.addSymbol(s)
    of IntLit: b.addIntLit(pool.integers[buf[n].intId])
    of UIntLit: b.addUIntLit(pool.uintegers[buf[n].uintId])
    of FloatLit: b.addFloatLit(pool.floats[buf[n].floatId])
    of SymbolDef: b.addSymbolDef(pool.syms[buf[n].symId])
    of CharLit: b.addCharLit char(buf[n].uoperand)
    of StringLit: b.addStrLit(pool.strings[buf[n].litId])
    of UnknownToken: b.addIdent "<unknown token>"
    of EofToken: b.addIntLit buf[n].soperand
    of ParRi: b.endTree()
    of ParLe: b.addTree(pool.tags[buf[n].tagId])
  result = b.extract()

proc instToSuffix(buf: TokenBuf, start: int): string =
  result = uhashBase36(instToString(buf, start))

proc newInstSymId(c: var SemContext; orig: SymId; suffix: string): SymId =
  # abc.123.Iabcdefgh.instmod
  var name = removeModule(pool.syms[orig])
  name.add(".I")
  name.add(suffix)
  name.add '.'
  name.add c.thisModuleSuffix
  result = pool.syms.getOrIncl(name)

type
  SubsContext = object
    newVars: Table[SymId, SymId]
    params: ptr Table[SymId, Cursor]
    instSuffix: string

proc addFreshSyms(c: var SemContext, sc: var SubsContext) =
  for _, newVar in sc.newVars:
    c.freshSyms.incl newVar

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
      let newDef =
        if sc.instSuffix != "":
          newInstSymId(c, s, sc.instSuffix)
        else:
          newSymId(c, s)
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
    if typeVars.substructureKind == TypevarsU:
      inc typeVars
      while typeVars.kind != ParRi:
        if typeVars.symKind == TypeVarY:
          var tv = typeVars
          inc tv
          dest.copyTree req.inferred[tv.symId]
        skip typeVars

proc subsGenericType(c: var SemContext; dest: var TokenBuf; req: InstRequest) {.used.} =
  # was used for `c.typeRequests` but types are instantiated eagerly now
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
    addFreshSyms(c, sc)

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
    subs(c, dest, sc, decl.pragmas)
    subs(c, dest, sc, decl.effects)
    subs(c, dest, sc, decl.body)
    addFreshSyms(c, sc)

template withFromInfo(req: InstRequest; body: untyped) =
  let oldLen = c.instantiatedFrom.len
  for jnfo in items(req.requestFrom):
    pushErrorContext c, jnfo
  body
  shrink c.instantiatedFrom, oldLen

type
  TypeDeclContext = enum
    InLocalDecl, InTypeSection, InReturnTypeDecl, AllowValues,
    InGenericConstraint, InInvokeHead

proc semLocalTypeImpl(c: var SemContext; n: var Cursor; context: TypeDeclContext)

proc semLocalType(c: var SemContext; n: var Cursor; context = InLocalDecl): TypeCursor =
  let insertPos = c.dest.len
  semLocalTypeImpl c, n, context
  assert c.dest.len > insertPos
  result = typeToCursor(c, insertPos)

proc semTypeSection(c: var SemContext; n: var Cursor)

proc instantiateType(c: var SemContext; typ: Cursor; bindings: Table[SymId, Cursor]): Cursor =
  var dest = createTokenBuf(30)
  var sc = SubsContext(params: addr bindings)
  subs(c, dest, sc, typ)
  var sub = beginRead(dest)
  var instDest = createTokenBuf(30)
  swap c.dest, instDest
  result = semLocalType(c, sub)
  swap c.dest, instDest

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
  while c.procRequests.len > 0:
    # This way with `move` ensures it is safe even though
    # the semchecking of generics can add to `c.procRequests`.
    # This is subtle!
    let procReqs = move(c.procRequests)
    for p in procReqs: instantiateGenericProc c, p

proc instantiateGenericHooks(c: var SemContext) =
  ## hooks are instantiated after all generics have been instantiated
  while c.procRequests.len > 0:
    let procReqs = move(c.procRequests)
    for p in procReqs: instantiateGenericProc c, p

# -------------------- sem checking -----------------------------

proc instantiateExprIntoBuf(c: var SemContext; buf: var TokenBuf; it: var Item; bindings: Table[SymId, Cursor]) =
  var dest = createTokenBuf(30)
  var sc = SubsContext(params: addr bindings)
  subs(c, dest, sc, it.n)
  var sub = beginRead(dest)
  let start = buf.len
  swap c.dest, buf
  it.n = sub
  semExpr(c, it)
  swap c.dest, buf
  it.n = cursorAt(buf, start)

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
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  let t = skipModifier(it.typ)
  if classifyType(c, t) != BoolT:
    c.dest.shrink start
    buildErr c, n.info, "expected `bool` but got: " & typeToString(t)
  n = it.n

proc semConstBoolExpr(c: var SemContext; n: var Cursor; allowUnresolved = false) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  let t = skipModifier(it.typ)
  if classifyType(c, t) != BoolT:
    c.dest.shrink start
    buildErr c, it.n.info, "expected `bool` but got: " & typeToString(t)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstBoolValue(value):
    if allowUnresolved:
      discard
    elif value.kind == ParLe and value.tagId == ErrT:
      c.dest.shrink start
      c.dest.add valueBuf
    else:
      c.dest.shrink start
      buildErr c, it.n.info, "expected constant bool value but got: " & asNimCode(value)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstStrExpr(c: var SemContext; n: var Cursor) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  let t = skipModifier(it.typ)
  if not isStringType(t):
    c.dest.shrink start
    buildErr c, it.n.info, "expected `string` but got: " & typeToString(t)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstStringValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, it.n.info, "expected constant string value but got: " & asNimCode(value)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstIntExpr(c: var SemContext; n: var Cursor) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  let t = skipModifier(it.typ)
  if classifyType(c, t) != IntT:
    c.dest.shrink start
    buildErr c, it.n.info, "expected `int` but got: " & typeToString(t)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstIntValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      c.dest.shrink start
      buildErr c, it.n.info, "expected constant integer value but got: " & asNimCode(value)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstExpr(c: var SemContext; it: var Item) =
  let start = c.dest.len
  var phase = SemcheckBodies
  swap c.phase, phase
  semExpr c, it
  swap c.phase, phase
  # XXX future note: consider when the expression depends on a generic param
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  c.dest.shrink start
  var value = beginRead(valueBuf)
  annotateConstantType c.dest, it.typ, value

proc semStmtsExpr(c: var SemContext; it: var Item; isNewScope: bool) =
  let before = c.dest.len
  takeToken c, it.n
  while it.n.kind != ParRi:
    if not isLastSon(it.n):
      semStmt c, it.n, false
    else:
      semExpr c, it
  takeParRi c, it.n
  let kind =
    if classifyType(c, it.typ) in {VoidT, AutoT}:
      (if isNewScope: ScopeTagId else: StmtsTagId)
    else: ExprTagId
  c.dest[before] = parLeToken(TagId(kind), c.dest[before].info)

proc semProcBody(c: var SemContext; itB: var Item) =
  var it = Item(n: itB.n, typ: c.types.autoType)
  var lastSonInfo = itB.n.info
  var beforeLastSon = c.dest.len
  var beforeLastSonCursor = default(Cursor)
  while it.n.kind != ParRi:
    if not isLastSon(it.n):
      semStmt c, it.n, false
    else:
      beforeLastSon = c.dest.len
      lastSonInfo = it.n.info
      beforeLastSonCursor = it.n
      semExpr c, it
  if c.routine.kind == TemplateY:
    case c.routine.returnType.typeKind
    of UntypedT:
      discard "ok"
    of VoidT:
      typecheck(c, lastSonInfo, it.typ, c.routine.returnType)
    else:
      commonType c, it, beforeLastSon, c.routine.returnType
  elif classifyType(c, it.typ) in {VoidT, UntypedT}:
    discard "ok"
  else:
    # transform `expr` to `result = expr`:
    if c.routine.resId != SymId(0):
      shrink c.dest, beforeLastSon
      var it = Item(n: beforeLastSonCursor, typ: c.routine.returnType)
      semExpr c, it

      var prefix = [
        parLeToken(AsgnS, lastSonInfo),
        symToken(c.routine.resId, lastSonInfo)]
      c.dest.insert prefix, beforeLastSon
      c.dest.addParRi()
    else:
      commonType c, it, beforeLastSon, c.routine.returnType
  takeParRi c, it.n # of (stmts)
  itB.n = it.n

proc semStmt(c: var SemContext; n: var Cursor; isNewScope: bool) =
  let info = n.info
  var it = Item(n: n, typ: c.types.autoType)
  let exPos = c.dest.len
  semExpr c, it
  if classifyType(c, it.typ) in {NoType, VoidT, AutoT, UntypedT}:
    discard "ok"
  else:
    # analyze the expression that was just produced:
    let ex = cursorAt(c.dest, exPos)
    let discardable = implicitlyDiscardable(ex)
    endRead(c.dest)
    if not discardable:
      buildErr c, info, "expression of type `" & typeToString(it.typ) & "` must be discarded"
  n = it.n

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

proc sameIdent(sym: SymId; str: StrId): bool =
  # XXX speed this up by using the `fieldCache` idea
  var name = pool.syms[sym]
  extractBasename(name)
  result = pool.strings.getOrIncl(name) == str

proc sameIdent(a, b: SymId): bool {.used.} =
  # not used yet
  # XXX speed this up by using the `fieldCache` idea
  var x = pool.syms[a]
  extractBasename(x)
  var y = pool.syms[b]
  extractBasename(y)
  result = x == y

proc requestRoutineInstance(c: var SemContext; origin: SymId;
                            typeArgs: TokenBuf;
                            inferred: Table[SymId, Cursor];
                            info: PackedLineInfo): ProcInstance =
  let key = typeToCanon(typeArgs, 0)
  var targetSym = c.instantiatedProcs.getOrDefault((origin, key))
  if targetSym == SymId(0):
    let targetSym = newSymId(c, origin)
    var signature = createTokenBuf(30)
    let decl = getProcDecl(origin)
    assert decl.typevars.substructureKind == TypevarsU, pool.syms[origin]
    var typeArgsStart = -1
    buildTree signature, decl.kind, info:
      signature.add symdefToken(targetSym, info)
      signature.addDotToken() # a generic instance is not exported
      signature.copyTree decl.pattern
      # InvokeT for the generic params:
      signature.buildTree InvokeT, info:
        signature.add symToken(origin, info)
        typeArgsStart = signature.len
        signature.add typeArgs
      var sc = SubsContext(params: addr inferred)
      subs(c, signature, sc, decl.params)
      let beforeRetType = signature.len
      subs(c, signature, sc, decl.retType)
      subs(c, signature, sc, decl.pragmas)
      subs(c, signature, sc, decl.effects)
      addFreshSyms(c, sc)
      signature.addDotToken() # no body

    result = ProcInstance(targetSym: targetSym, procType: cursorAt(signature, 0),
      returnType: cursorAt(signature, beforeRetType))

    # rebuild inferred as cursors to params in signature invocation
    var newInferred = initTable[SymId, Cursor](inferred.len)
    var typevar = decl.typevars
    inc typevar # skip tag
    var typeArg = cursorAt(signature, typeArgsStart)
    while typevar.kind != ParRi:
      assert typeArg.kind != ParRi
      let sym = asLocal(typevar).name.symId
      newInferred[sym] = typeArg
      skip typevar
      skip typeArg
    assert typeArg.kind == ParRi

    publish targetSym, ensureMove signature

    c.instantiatedProcs[(origin, key)] = targetSym
    var req = InstRequest(
      origin: origin,
      targetSym: targetSym,
      inferred: move(newInferred)
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

type
  DotExprState = enum
    MatchedDotField ## matched a dot field, i.e. result is a dot expression
    MatchedDotSym ## matched a qualified identifier
    FailedDot
    InvalidDot

proc tryBuiltinDot(c: var SemContext; it: var Item; lhs: Item; fieldName: StrId; info: PackedLineInfo; flags: set[SemFlag]): DotExprState

proc tryBuiltinSubscript(c: var SemContext; it: var Item; lhs: Item): bool
proc semBuiltinSubscript(c: var SemContext; it: var Item; lhs: Item)

proc semBaseobj(c: var SemContext; it: var Item) =
  inc it.n # baseobj
  let beforeExpr = c.dest.len
  let destType = semLocalType(c, it.n)
  if it.n.kind == IntLit:
    # we will recompute it via `typematch` below:
    inc it.n
  else:
    bug("expected integer literal in `baseobj` construct")
  var m = createMatch(addr c)

  var arg = Item(n: it.n, typ: c.types.autoType)
  var argBuf = createTokenBuf(16)
  swap c.dest, argBuf
  semExpr c, arg
  swap c.dest, argBuf
  it.n = arg.n
  arg.n = cursorAt(argBuf, 0)

  typematch m, destType, arg
  if not m.err and m.args[0].tagEnum == BaseobjTagId:
    c.dest.shrink beforeExpr
    c.dest.add m.args
  else:
    c.typeMismatch it.n.info, it.typ, destType
  skipParRi it.n
  commonType c, it, beforeExpr, destType

proc addMaybeBaseobjConv(c: var SemContext; m: var Match; beforeExpr: int) =
  if m.args[0].tagEnum == BaseobjTagId:
    c.dest.shrink beforeExpr
    c.dest.add m.args
    # remove the ')' as the caller will add one for us!
    c.dest.shrink c.dest.len - 1
  else:
    c.dest.add m.args

proc semConvArg(c: var SemContext; destType: Cursor; arg: Item; info: PackedLineInfo; beforeExpr: int) =
  const
    IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, EnumT, HoleyEnumT}

  var srcType = skipModifier(arg.typ)

  # distinct type conversion?
  var isDistinct = false
  # also skips to type body for symbols:
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)

  if (destBase.typeKind in IntegralTypes and srcBase.typeKind in IntegralTypes) or
     (destBase.isSomeStringType and srcBase.isSomeStringType) or
     (destBase.containsGenericParams or srcBase.containsGenericParams):
    discard "ok"
    # XXX Add hderef here somehow
    c.dest.addSubtree arg.n
  elif isDistinct:
    var matchArg = Item(n: arg.n, typ: srcBase)
    var m = createMatch(addr c)
    typematch m, destBase, matchArg
    if m.err:
      c.typeMismatch info, arg.typ, destType
    else:
      # distinct type conversions can also involve conversions
      # between different integer sizes or object types and then
      # `m.args` contains these so use them here:
      c.dest.add m.args
  else:
    # maybe object types with an inheritance relation?
    var matchArg = arg
    var m = createMatch(addr c)
    typematch m, destType, matchArg
    if not m.err:
      addMaybeBaseobjConv(c, m, beforeExpr)
    else:
      # also try the other direction:
      var m = createMatch(addr c)
      m.flipped = true
      matchArg.typ = destType
      typematch m, srcType, matchArg
      if not m.err:
        addMaybeBaseobjConv(c, m, beforeExpr)
      else:
        c.typeMismatch info, arg.typ, destType

proc isCastableType(t: TypeCursor): bool =
  const IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, PointerT, CstringT, RefT, PtrT, NiltT, EnumT, HoleyEnumT}
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
  takeParRi c, it.n

  var srcType = skipModifier(x.typ)

  # distinct type conversion?
  var isDistinct = false
  # also skips to type body for symbols:
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)
  if sameTrees(destBase, srcBase):
    commonType c, it, beforeExpr, destType
  elif destBase.isCastableType and srcBase.isCastableType:
    commonType c, it, beforeExpr, destType
  elif containsGenericParams(srcType) or containsGenericParams(destType):
    commonType c, it, beforeExpr, destType
  else:
    c.dest.shrink beforeExpr
    c.buildErr info, "cannot `cast` between types " & typeToString(srcType) & " and " & typeToString(destType)

proc semLocalTypeExpr(c: var SemContext, it: var Item)

proc semReturnType(c: var SemContext; n: var Cursor): TypeCursor =
  result = semLocalType(c, n, InReturnTypeDecl)

include semcall

proc objBody(td: TypeDecl): Cursor {.inline.} =
  # see also `objtypeImpl`
  result = td.body
  # old ref object types:
  if result.typeKind in {RefT, PtrT}:
    inc result

proc skipInvoke(t: var Cursor): Cursor {.inline.} =
  ## if `t` is an invocation, skips to root sym and returns start of args,
  ## otherwise returns `default(Cursor)`
  if t.typeKind == InvokeT:
    inc t
    result = t
    inc result
  else:
    result = default(Cursor)

proc genericRootSym(td: TypeDecl): SymId =
  result = td.name.symId
  if td.typevars.typeKind == InvokeT:
    var root = td.typevars
    inc root
    assert root.kind == Symbol
    result = root.symId

proc bindInvokeArgs(decl: TypeDecl; invokeArgs: Cursor): Table[SymId, Cursor] =
  ## returns a mapping of invocation arguments to typevars of a type
  result = initTable[SymId, Cursor]()
  if invokeArgs != default(Cursor):
    var typevar = decl.typevars
    assert typevar.substructureKind == TypevarsU
    inc typevar
    var arg = invokeArgs
    while arg.kind != ParRi:
      let tv = asLocal(typevar)
      assert tv.kind == TypevarY
      result[tv.name.symId] = arg
      skip typevar
      skip arg

proc bindSubsInvokeArgs(c: var SemContext; decl: TypeDecl; buf: var TokenBuf;
                        prevBindings: Table[SymId, Cursor];
                        invokeArgs: Cursor): Table[SymId, Cursor] =
  ## same as `bindInvokeArgs` but substitutes the given arguments
  ## based on `prevBindings`,
  ## substituted arguments are built into `buf` which must live as long as
  ## the returned bindings
  if invokeArgs != default(Cursor):
    buf = createTokenBuf(16)
    var sc = SubsContext(params: addr prevBindings)
    var arg = invokeArgs
    while arg.kind != ParRi:
      subs(c, buf, sc, arg)
      skip arg
    buf.addParRi()
    result = bindInvokeArgs(decl, beginRead(buf))
  else:
    result = initTable[SymId, Cursor]()

proc findObjFieldAux(c: var SemContext; t: Cursor; name: StrId; bindings: Table[SymId, Cursor]; level = 0): ObjField =
  assert t.typeKind == ObjectT
  var n = t
  inc n # skip `(object` token
  var baseType = n
  skip n # skip basetype
  var iter = initObjFieldIter()
  while nextField(iter, n):
    inc n # skip FldU
    if n.kind == SymbolDef and sameIdent(n.symId, name):
      let symId = n.symId
      inc n # skip name
      let exported = n.kind != DotToken
      skip n # export marker
      skip n # pragmas
      var typ = n
      if bindings.len != 0:
        # fields in generic type AST contain generic params of the type
        # for invoked object types, bindings are built from the given arguments
        # and the field type is instantiated based on them here
        typ = instantiateType(c, typ, bindings)
      return ObjField(sym: symId, level: level, typ: typ, exported: exported, rootOwner: SymId(0))
    skip n # skip name
    skip n # export marker
    skip n # pragmas
    skip n # type
    skip n # value
    skipParRi n
  if baseType.kind == DotToken:
    result = ObjField(level: -1)
  else:
    if baseType.typeKind in {RefT, PtrT}:
      inc baseType
    let baseInvokeArgs = skipInvoke(baseType)
    if baseType.kind == Symbol:
      let decl = getTypeSection(baseType.symId)
      if decl.kind != TypeY:
        error "invalid parent object type", baseType
      let objType = decl.objBody
      # build bindings for parent type:
      var newBindingBuf = default(TokenBuf)
      let newBindings = bindSubsInvokeArgs(c, decl, newBindingBuf, bindings, baseInvokeArgs)
      result = findObjFieldAux(c, objType, name, newBindings, level+1)
      if result.level == level+1:
        result.rootOwner = genericRootSym(decl)
    else:
      # maybe error
      result = ObjField(level: -1)

proc findObjFieldConsiderVis(c: var SemContext; decl: TypeDecl; name: StrId; bindings: Table[SymId, Cursor]): ObjField =
  let impl = decl.objBody
  result = findObjFieldAux(c, impl, name, bindings)
  if c.routine.inInst == 0:
    # only check visibility during first semcheck
    if result.level == 0:
      result.rootOwner = genericRootSym(decl)
    if result.level >= 0:
      # check visibility
      var visible = false
      if result.exported:
        visible = true
      else:
        let owner = result.rootOwner
        if owner == SymId(0):
          visible = true
        else:
          let ownerModule = extractModule(pool.syms[owner])
          visible = ownerModule == "" or ownerModule == c.thisModuleSuffix
      if not visible:
        # treat as undeclared
        result = ObjField(level: -1)

proc semQualifiedIdent(c: var SemContext; module: SymId; ident: StrId; info: PackedLineInfo): Sym =
  # mirrors semIdent
  let insertPos = c.dest.len
  let count =
    if module == c.selfModuleSym:
      buildSymChoiceForSelfModule(c, ident, info)
    else:
      buildSymChoiceForForeignModule(c, module, ident, info)
  if count == 1:
    let sym = c.dest[insertPos+1].symId
    c.dest.shrink insertPos
    c.dest.add symToken(sym, info)
    result = fetchSym(c, sym)
  else:
    result = Sym(kind: if count == 0: NoSym else: CchoiceY)

proc semExprSym(c: var SemContext; it: var Item; s: Sym; start: int; flags: set[SemFlag])

proc findEnumField(decl: EnumDecl; name: StrId): SymId =
  result = SymId(0)
  var f = decl.firstField
  while f.kind != ParRi:
    let field = takeLocal(f, SkipFinalParRi)
    let symId = field.name.symId
    var isGlobal = false
    let basename = extractBasename(pool.syms[symId], isGlobal)
    let strId = pool.strings.getOrIncl(basename)
    if name == strId:
      return symId

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
    var root = t
    var doDeref = false # maybe arbitrary number of derefs for compat mode
    if root.typeKind in {RefT, PtrT}:
      doDeref = true
      inc root
    let invokeArgs = skipInvoke(root)
    if root.kind == Symbol:
      let decl = getTypeSection(root.symId)
      if decl.kind == TypeY:
        var objType = decl.body
        # old ref object types:
        if objType.typeKind in {RefT, PtrT}:
          doDeref = true
          inc objType
        if objType.typeKind == ObjectT:
          # build bindings for invoked object type to get proper field type:
          let bindings = bindInvokeArgs(decl, invokeArgs)
          let field = findObjFieldConsiderVis(c, decl, fieldName, bindings)
          if field.level >= 0:
            result = MatchedDotField
            if doDeref:
              c.dest[exprStart] = parLeToken(DdotX, info)
            c.dest.add symToken(field.sym, info)
            c.dest.add intToken(pool.integers.getOrIncl(field.level), info)
            it.typ = field.typ # will be fit later with commonType
            it.kind = FldY
          else:
            c.dest.add identToken(fieldName, info)
        else:
          # typevars reach here, maybe return untyped state
          # though probably better to fail if this is a call, i.e. `x.int`
          c.dest.add identToken(fieldName, info)
      else:
        c.dest.add identToken(fieldName, info)
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
      var i = 0
      while tup.kind != ParRi:
        var field = asTupleField(tup)
        if field.kind == KvU:
          let name = takeIdent(field.name)
          if name == fieldName:
            c.dest[exprStart] = parLeToken(TupatX, info)
            c.dest.addIntLit(i, info)
            it.typ = field.typ # will be fit later with commonType
            result = MatchedDotField
            break
        skip tup
        inc i
      if result != MatchedDotField:
        c.dest.add identToken(fieldName, info)
        c.dest.add intToken(pool.integers.getOrIncl(0), info)
    elif t.typeKind == TypedescT:
      var tval = t
      inc tval
      if tval.kind == Symbol:
        let decl = getTypeSection(tval.symId)
        if decl.kind == TypeY:
          tval = decl.body
      if tval.typeKind in {EnumT, OnumT}:
        # check for qualified enum field i.e. Foo.Bar
        let field = findEnumField(asEnumDecl(tval), fieldName)
        if field != SymId(0):
          result = MatchedDotSym
          c.dest.shrink exprStart
          c.dest.add symToken(field, info)
          let s = Sym(kind: EfldY, name: field, pos: ImportedPos) # placeholder pos
          semExprSym c, it, s, exprStart, flags
          return
        else:
          c.dest.add identToken(fieldName, info)
      else:
        c.dest.add identToken(fieldName, info)
    else:
      c.dest.add identToken(fieldName, info)
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
  let fieldName = takeIdent(it.n)
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
    callBuf.addSubtree fieldNameCursor
    callBuf.addSubtree lhs.n # add lhs as first argument
    callBuf.addParRi()
    var call = Item(n: cursorAt(callBuf, 0), typ: expected)
    semCall c, call, flags, DotCall
    it.typ = call.typ

proc semWhile(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  semBoolExpr c, it.n
  inc c.routine.inLoop
  withNewScope c:
    semStmt c, it.n, true
  dec c.routine.inLoop
  takeParRi c, it.n
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
      let delayed = handleSymDef(c, it.n, BlockY)
      c.addSym delayed
      publish c, delayed.s.name, declStart

    semStmtBranch c, it, true
  dec c.routine.inBlock

  takeParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semBreak(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.inLoop+c.routine.inBlock == 0:
    buildErr c, info, "`break` only possible within a `while` or `block` statement"
    skip it.n
  else:
    if it.n.kind == DotToken:
      wantDot c, it.n
    else:
      let labelInfo = it.n.info
      var a = Item(n: it.n, typ: c.types.autoType)
      semExpr(c, a)
      if a.kind != BlockY:
        buildErr c, labelInfo, "`break` needs a block label"
      it.n = a.n
  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc semContinue(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.inLoop == 0:
    buildErr c, info, "`continue` only possible within a loop"
  else:
    wantDot c, it.n
  takeParRi c, it.n
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

proc semProposition(c: var SemContext; n: var Cursor; kind: PragmaKind) =
  let prevPhase = c.phase
  if prevPhase != SemcheckBodies:
    takeTree c, n
  else:
    c.phase = SemcheckBodies
    withNewScope c:
      if kind == EnsuresP:
        c.dest.add parLeToken(ExprX, n.info)
        discard declareResult(c, n.info)
      #let start = c.dest.len
      semBoolExpr c, n
      if kind == EnsuresP:
        c.dest.addParRi()
      # XXX More checking here: Expression can only use parameters and `result`
      # and consts. Function calls are not allowed either. The grammar is:
      # atom ::= const | param | result
      # arith ::= atom | arith `+` arith | arith `-` arith | arith `*` arith | arith `/` arith # etc.
      # expr ::= arith | expr `and` expr | expr `or` expr | `not` expr
    c.phase = prevPhase

type
  CrucialPragma* = object
    sym: SymId
    magic, externName: string
    bits: int
    hasVarargs: PackedLineInfo
    flags: set[PragmaKind]
    headerFileTok: PackedToken

proc semPragma(c: var SemContext; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  let hasParRi = n.kind == ParLe # if false, has no arguments
  if n.substructureKind == KvU:
    inc n
  let pk = pragmaKind(n)
  case pk
  of NoPragma:
    if kind.isRoutine and (let cc = callConvKind(n); cc != NoCallConv):
      c.dest.addParLe(cc, n.info)
      inc n
      c.dest.addParRi()
    else:
      let name = getIdent(n)
      if name != StrId(0) and name in c.userPragmas and not hasParRi:
        # custom pragma, cannot have arguments
        inc n
        var read = beginRead(c.userPragmas[name])
        while read.kind != ParRi:
          semPragma c, read, crucial, kind
        endRead(c.userPragmas[name])
      else:
        buildErr c, n.info, "expected pragma"
        inc n
        if hasParRi:
          while n.kind != ParRi: skip n # skip optional pragma arguments
  of MagicP:
    c.dest.add parLeToken(MagicP, n.info)
    inc n
    if hasParRi and n.kind in {StringLit, Ident}:
      let (magicWord, bits) = magicToTag(pool.strings[n.litId])
      if magicWord == "":
        buildErr c, n.info, "unknown `magic`"
      else:
        crucial.magic = magicWord
        crucial.bits = bits
      takeToken c, n
    elif n.exprKind == ErrX:
      c.dest.addSubtree n
    else:
      buildErr c, n.info, "`magic` pragma takes a string literal"
    c.dest.addParRi()
  of ErrorP, ReportP, DeprecatedP:
    crucial.flags.incl pk
    c.dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      semConstStrExpr c, n
    c.dest.addParRi()
  of ImportcP, ImportcppP, ExportcP, HeaderP, PluginP:
    crucial.flags.incl pk
    let info = n.info
    c.dest.add parLeToken(pk, info)
    inc n
    let strPos = c.dest.len
    if hasParRi and n.kind != ParRi:
      semConstStrExpr c, n
    elif crucial.sym != SymId(0):
      var name = pool.syms[crucial.sym]
      extractBasename name
      c.dest.add strToken(pool.strings.getOrIncl(name), info)
    else:
      c.buildErr info, "invalid import/export symbol"
      c.dest.addParRi()
      return
    if pk in {ImportcP, ImportcppP, ExportcP} and c.dest[strPos].kind == StringLit:
      crucial.externName = pool.strings[c.dest[strPos].litId]
    # Header pragma extra
    if pk == HeaderP:
      let idx = c.dest.len - 1
      let tok = c.dest[idx]
      var name = replaceSubs(pool.strings[tok.litId], info.getFile(), c.g.config)
      name = name.toRelativePath(c.g.config.nifcachePath)
      c.dest[idx] = strToken(pool.strings.getOrIncl(name), tok.info)
      crucial.headerFileTok = c.dest[idx]
    # Finalize expression
    c.dest.addParRi()
  of AlignP, BitsP:
    c.dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      semConstIntExpr(c, n)
    else:
      buildErr c, n.info, "expected int literal"
    c.dest.addParRi()
  of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoreturnP, BorrowP,
     NoSideEffectP, NodestroyP, BycopyP, ByrefP, InlineP, NoinlineP, NoinitP,
     InjectP, GensymP, UntypedP, SideEffectP, BaseP:
    crucial.flags.incl pk
    c.dest.add parLeToken(pk, n.info)
    c.dest.addParRi()
    inc n
  of ViewP, InheritableP, PureP, FinalP:
    if kind == TypeY:
      c.dest.add parLeToken(pk, n.info)
      inc n
    else:
      buildErr c, n.info, "pragma only allowed on types"
    c.dest.addParRi()
  of CursorP:
    if kind in {VarY, LetY, CursorY}:
      c.dest.add parLeToken(pk, n.info)
      inc n
    else:
      buildErr c, n.info, "pragma only allowed on local variables"
    c.dest.addParRi()
  of VarargsP:
    crucial.hasVarargs = n.info
    c.dest.add parLeToken(pk, n.info)
    c.dest.addParRi()
    inc n
  of RequiresP, EnsuresP:
    crucial.flags.incl pk
    c.dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      semProposition c, n, pk
    else:
      buildErr c, n.info, "`requires`/`ensures` pragma takes a bool expression"
    c.dest.addParRi()
  of TagsP:
    c.dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      takeTree c, n
    else:
      buildErr c, n.info, "expected tags/raises list"
    c.dest.addParRi()
  of RaisesP:
    crucial.flags.incl pk
    let oldLen = c.dest.len
    c.dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      var nn = n
      takeTree c, n
      c.dest.addParRi()
      if nn.exprKind == BracketX and nn.firstSon.kind == ParRi:
        # `raises: []` means "does not raise":
        crucial.flags.excl pk
        c.dest.shrink oldLen
    else:
      c.dest.addParRi()
  of EmitP, BuildP, StringP, AssumeP, AssertP, PragmaP:
    buildErr c, n.info, "pragma not supported"
    inc n
    if hasParRi:
      while n.kind != ParRi: skip n # skip optional pragma arguments
    c.dest.addParRi()
  of KeepOverflowFlagP:
    c.dest.add parLeToken(pk, n.info)
    inc n
    c.dest.addParRi()
  of SemanticsP:
    c.dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind in {StringLit, Ident}:
      takeToken c, n
    else:
      buildErr c, n.info, "`semantics` pragma takes a string literal"
    c.dest.addParRi()
  if hasParRi:
    if n.kind != ParRi:
      if n.exprKind != ErrX:
        buildErr c, n.info, "too many arguments for pragma"
      while n.kind != ParRi: skip n
    skipParRi n

proc semPragmas(c: var SemContext; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  if n.kind == DotToken:
    takeToken c, n
  elif n.substructureKind == PragmasU:
    takeToken c, n
    while n.kind != ParRi:
      if n.exprKind == ErrX:
        takeTree c, n
      else:
        semPragma c, n, crucial, kind
    takeParRi c, n
  else:
    buildErr c, n.info, "expected '.' or 'pragmas'"

proc semIdentImpl(c: var SemContext; n: var Cursor; ident: StrId; flags: set[SemFlag]): Sym =
  let mode =
    if AllowOverloads in flags: FindOverloads
    else: InnerMost
  let insertPos = c.dest.len
  let info = n.info
  let count = buildSymChoice(c, ident, info, mode)
  if count == 1:
    let sym = c.dest[insertPos+1].symId
    c.dest.shrink insertPos
    c.dest.add symToken(sym, info)
    result = fetchSym(c, sym)
  else:
    result = Sym(kind: if count == 0: NoSym else: CchoiceY)

proc semIdent(c: var SemContext; n: var Cursor; flags: set[SemFlag]): Sym =
  result = semIdentImpl(c, n, n.litId, flags)
  inc n

proc semQuoted(c: var SemContext; n: var Cursor; flags: set[SemFlag]): Sym =
  let nameId = takeUnquoted(n)
  result = semIdentImpl(c, n, nameId, flags)

proc maybeInlineMagic(c: var SemContext; res: LoadResult) =
  if res.status == LacksNothing:
    var n = res.decl
    inc n # skip the symbol kind
    if n.kind == SymbolDef:
      inc n # skip the SymbolDef
      if n.kind == ParLe:
        # ^ export marker position has a `(`? If so, it is a magic!
        let info = c.dest[c.dest.len-1].info
        var tag = n.tagId
        if cast[TagEnum](tag) == IsMainModuleTagId:
          if IsMain in c.moduleFlags:
            tag = TagId(TrueTagId)
          else:
            tag = TagId(FalseTagId)
        c.dest[c.dest.len-1] = parLeToken(tag, info)
        inc n
        while true:
          c.dest.add withLineInfo(n.load, info)
          if n.kind == ParRi: break
          inc n

proc exprToType(c: var SemContext; exprType: Cursor; start: int; context: TypeDeclContext; info: PackedLineInfo) =
  case exprType.typeKind
  of TypedescT:
    c.dest.shrink start
    var base = exprType
    inc base
    c.dest.addSubtree base
  of ErrT, AutoT:
    # propagate error
    discard
  else:
    # otherwise, is a static value
    if context != AllowValues:
      c.dest.shrink start
      c.buildErr info, "not a type"

proc semTypeExpr(c: var SemContext; n: var Cursor; context: TypeDeclContext; info: PackedLineInfo) =
  # expression needs to be fully evaluated, switch to body phase
  var phase = SemcheckBodies
  swap c.phase, phase
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  exprToType c, it.typ, start, context, info
  swap c.phase, phase

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
      # was magic symbol, may be typeclass, otherwise nothing to do
      if context != InInvokeHead:
        let magic = cursorAt(c.dest, start).typeKind
        endRead(c.dest)
        # magic types that are just symbols and not in the syntax:
        if magic in {ArrayT, SetT, RangetypeT, EnumT, HoleyEnumT}:
          var typeclassBuf = createTokenBuf(4)
          typeclassBuf.addParLe(TypeKindT, info)
          typeclassBuf.addParLe(magic, info)
          typeclassBuf.addParRi()
          typeclassBuf.addParRi()
          replace(c.dest, cursorAt(typeclassBuf, 0), start)
    elif res.status == LacksNothing:
      let typ = asTypeDecl(res.decl)
      if isGeneric(typ) or isNominal(typ.body.typeKind):
        # types that should stay as symbols, see sigmatch.matchSymbol
        # but see if it triggers a module plugin:
        let p = extractPragma(typ.pragmas, PluginP)
        if p != default(Cursor) and p.kind == StringLit:
          if p.litId notin c.pluginBlacklist:
            c.pendingTypePlugins[s.name] = p.litId
      else:
        # remove symbol, inline type:
        c.dest.shrink c.dest.len-1
        var t = typ.body
        semLocalTypeImpl c, t, context
  else:
    # non type symbol, treat as expression
    # mirror semTypeExpr but just call semExprSym
    var phase = SemcheckBodies
    swap c.phase, phase
    var dummyBuf = createTokenBuf(1)
    dummyBuf.add dotToken(info)
    var it = Item(n: cursorAt(dummyBuf, 0), typ: c.types.autoType)
    semExprSym c, it, s, start, {}
    exprToType c, it.typ, start, context, info
    swap c.phase, phase

proc semParams(c: var SemContext; n: var Cursor)
proc semLocal(c: var SemContext; n: var Cursor; kind: SymKind)

type WhenMode = enum
  NormalWhen
  ObjectWhen

proc semWhenImpl(c: var SemContext; it: var Item; mode: WhenMode)

type CaseMode = enum
  NormalCase
  ObjectCase

proc semCaseImpl(c: var SemContext; it: var Item; mode: CaseMode)

proc semExprMissingPhases(c: var SemContext; it: var Item; firstPhase: SemPhase) =
  if c.phase <= firstPhase:
    var lastBuf = default(TokenBuf)
    var usingBuf = false
    for phase in low(SemPhase) ..< c.phase:
      var buf = createTokenBuf()
      swap c.dest, buf
      var phase = phase
      swap c.phase, phase
      var it2 = Item(typ: it.typ)
      if usingBuf:
        it2.n = beginRead(lastBuf)
      else:
        it2.n = it.n
      semExpr c, it2
      if not usingBuf:
        it.n = it2.n
      swap c.phase, phase
      swap c.dest, buf
      lastBuf = buf
      usingBuf = true
    let lastN = it.n
    if usingBuf:
      it.n = beginRead(lastBuf)
    semExpr c, it
    if usingBuf:
      it.n = lastN
  else:
    semExpr c, it

proc addVarargsParameter(c: var SemContext; paramsAt: int; info: PackedLineInfo) =
  const vanon = "vanon"
  var varargsParam = @[
    parLeToken(ParamY, info),
    identToken(pool.strings.getOrIncl(vanon), info),
    dotToken(info), # export marker
    dotToken(info), # pragmas
    parLeToken(VarargsT, info),
    parRiToken(info),
    dotToken(info), # value
    parRiToken(info)
  ]
  if c.dest[paramsAt].kind == DotToken:
    c.dest[paramsAt] = parLeToken(ParamsT, info)
    varargsParam.add parRiToken(info)
    c.dest.insert fromBuffer(varargsParam), paramsAt+1
  else:
    var n = cursorAt(c.dest, paramsAt)
    if n.typeKind == ParamsT:
      inc n
      while n.kind != ParRi:
        if n.symKind == ParamY:
          inc n
          let lit = takeIdent(n)
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

include semtypes

proc semTypeof(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  inc it.n # typeof
  semExpr c, it
  var t = it.typ
  if t.typeKind == TypedescT: inc t
  c.dest.shrink beforeExpr
  c.dest.addParLe(TypedescT, t.info)
  c.dest.addSubtree t
  c.dest.addParRi()
  it.typ = typeToCursor(c, beforeExpr)
  #echo "CAME HERE! ", typeToString(t), " ", c.phase
  #writeStackTrace()
  skipParRi it.n

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

proc semLocalValue(c: var SemContext; it: var Item; crucial: CrucialPragma) =
  if ThreadvarP in crucial.flags:
    c.buildErr it.n.info, "a `threadvar` cannot have an init value"
    skip it.n
  else:
    semExpr c, it

proc semLocal(c: var SemContext; n: var Cursor; kind: SymKind) =
  let declStart = c.dest.len
  takeToken c, n
  var delayed = handleSymDef(c, n, kind) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1
  var crucial = CrucialPragma(sym: delayed.s.name)
  semPragmas c, n, crucial, kind # 2
  if crucial.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, crucial
  if delayed.status == OkExistingFresh and InjectP in crucial.flags:
    # symbol is injected, add it to scope
    delayed.status = OkNew
  case kind
  of TypevarY:
    discard semLocalType(c, n, InGenericConstraint)
    wantDot c, n
  of ParamY, LetY, VarY, ConstY, CursorY, ResultY, FldY, GletY, TletY, GvarY, TvarY:
    let beforeType = c.dest.len
    if n.kind == DotToken:
      # no explicit type given:
      inc n # 3
      var it = Item(n: n, typ: c.types.autoType)
      if kind == ConstY:
        withNewScope c:
          semConstExpr c, it # 4
      else:
        semLocalValue c, it, crucial # 4
      n = it.n
      let typ = skipModifier(it.typ)
      insertType c, typ, beforeType
    else:
      let typ = semLocalType(c, n) # 3
      if n.kind == DotToken:
        # empty value
        takeToken c, n
      else:
        var it = Item(n: n, typ: typ)
        if kind == ConstY:
          withNewScope c:
            semConstExpr c, it # 4
        else:
          semLocalValue c, it, crucial # 4
        n = it.n
        patchType c, it.typ, beforeType
  else:
    bug "semLocal"
  c.addSym delayed
  takeParRi c, n
  if kind == LetY:
    if ThreadvarP in crucial.flags:
      copyKeepLineInfo c.dest[declStart], parLeToken(TletS, NoLineInfo)
    elif GlobalP in crucial.flags or c.currentScope.kind == TopLevelScope:
      copyKeepLineInfo c.dest[declStart], parLeToken(GletS, NoLineInfo)
  elif kind == VarY:
    if ThreadvarP in crucial.flags:
      copyKeepLineInfo c.dest[declStart], parLeToken(TvarS, NoLineInfo)
    elif GlobalP in crucial.flags or c.currentScope.kind == TopLevelScope:
      copyKeepLineInfo c.dest[declStart], parLeToken(GvarS, NoLineInfo)
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

proc evalConstExpr(c: var SemContext; n: var Cursor; expected: TypeCursor): TokenBuf =
  let beforeExpr = c.dest.len
  var x = Item(n: n, typ: expected)
  semExpr c, x
  n = x.n
  var e = cursorAt(c.dest, beforeExpr)
  result = evalExpr(c, e)
  endRead(c.dest)

proc evalConstIntExpr(c: var SemContext; n: var Cursor; expected: TypeCursor): xint =
  let info = n.info
  var valueBuf = evalConstExpr(c, n, expected)
  let value = beginRead(valueBuf)
  result = getConstOrdinalValue(value)
  if result.isNaN:
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, info, "expected constant integer value but got: " & asNimCode(value)

proc evalConstStrExpr(c: var SemContext; n: var Cursor; expected: TypeCursor): StrId =
  let info = n.info
  var valueBuf = evalConstExpr(c, n, expected)
  let value = beginRead(valueBuf)
  result = getConstStringValue(value)
  if result == StrId(0):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, info, "expected constant string value but got: " & asNimCode(value)

proc semEnumField(c: var SemContext; n: var Cursor; state: var EnumTypeState) =
  let declStart = c.dest.len
  takeToken c, n
  var delayed = handleSymDef(c, n, EfldY) # 0
  if delayed.status == OkExistingFresh:
    # XXX original nim always injects enum fields regardless of the enum sym itself,
    # this does the same here
    delayed.status = OkNew
  let beforeExportMarker = c.dest.len
  if n.kind == DotToken:
    if state.isExported:
      # if enum type is exported, enum field is exported
      c.dest.add identToken(pool.strings.getOrIncl("x"), n.info)
    else:
      c.dest.add n
    inc n # 1
  else:
    wantExportMarker c, n # 1
  var crucial = CrucialPragma(sym: delayed.s.name)
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
    let info = c.dest[declStart].info
    c.dest.add parLeToken(TupX, info)
    c.addXint state.thisValue, info
    c.dest.add strToken(delayed.lit, info)
    c.dest.addParRi()
    inc n
  else:
    if n.kind == ParLe and n.exprKind == TupX:
      c.dest.add n
      inc n
      let explicitValue = evalConstIntExpr(c, n, c.types.autoType) # 4
      if explicitValue != state.thisValue:
        state.hasHole = true
        state.thisValue = explicitValue
      c.dest.add evalExpr(c, n)
      takeParRi c, n
    else:
      var ec = initEvalContext(addr c)
      var valueCursor = n
      let fieldValue = eval(ec, valueCursor)
      if fieldValue.kind == StringLit:
        c.dest.add parLeToken(TupX, n.info)
        c.addXint state.thisValue, n.info
        c.dest.add fieldValue
        c.dest.addParRi()
        n = valueCursor
      else:
        c.dest.add parLeToken(TupX, n.info)
        let explicitValue = evalConstIntExpr(c, n, c.types.autoType) # 4
        if explicitValue != state.thisValue:
          state.hasHole = true
          state.thisValue = explicitValue
        c.dest.add strToken(delayed.lit, n.info)
        c.dest.addParRi()
  c.addSym delayed
  takeParRi c, n
  publish c, delayed.s.name, declStart

proc semGenericParam(c: var SemContext; n: var Cursor) =
  if n.substructureKind == TypevarU:
    semLocal c, n, TypevarY
  else:
    buildErr c, n.info, "expected 'typevar'"

proc semGenericParams(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    takeToken c, n
  elif n.substructureKind == TypevarsU:
    inc c.routine.inGeneric
    takeToken c, n
    while n.kind != ParRi:
      semGenericParam c, n
    takeParRi c, n
  elif n.typeKind == InvokeT:
    inc c.routine.inInst
    takeTree c, n
  else:
    buildErr c, n.info, "expected '.' or 'typevars'"

proc semParam(c: var SemContext; n: var Cursor) =
  if n.substructureKind == ParamU:
    semLocal c, n, ParamY
  else:
    buildErr c, n.info, "expected 'param'"

proc semParams(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    takeToken c, n
  elif n.substructureKind == ParamsU:
    takeToken c, n
    while n.kind != ParRi:
      semParam c, n
    takeParRi c, n
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
  let resId = declareResult(c, it.n.info)
  semProcBody c, it
  addReturnResult c, resId, procBody[procBody.len - 1].info

proc getParamsType(c: var SemContext; paramsAt: int): seq[TypeCursor] =
  result = @[]
  if c.dest[paramsAt].kind != DotToken:
    var n = cursorAt(c.dest, paramsAt)
    if n.typeKind == ParamsT:
      inc n
      while n.kind != ParRi:
        if n.symKind == ParamY:
          var local = takeLocal(n, SkipFinalParRi)
          result.add local.typ
        else:
          break
      endRead(c.dest)

proc getObjSymId(c: var SemContext; obj: TypeCursor): SymId =
  var obj = skipModifier(obj)
  while true:
    if obj.typeKind == InvokeT:
      inc obj
    else:
      break
  if obj.kind == Symbol:
    result = obj.symId
  else:
    result = SymId(0)

proc checkTypeHook(c: var SemContext; params: seq[TypeCursor]; op: HookKind; info: PackedLineInfo) =
  var cond: bool
  case op
  of NoHook:
    return
  of DestroyH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 1
    if not cond:
      buildErr c, info, "signature for '=destroy' must be proc[T: object](x: T)"
  of TraceH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT and classifyType(c, params[1]) == PointerT
  of WasmovedH:
    cond = classifyType(c, c.routine.returnType) == VoidT and
        params.len == 1 and classifyType(c, params[0]) == MutT
  of CopyH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT
  of SinkhH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT
  of DupH:
    cond = params.len == 1 and sameTrees(params[0], c.routine.returnType)

  if cond:
    let obj = getObjSymId(c, params[0])

    if obj == SymId(0):
      cond = false
    else:
      let res = tryLoadSym(obj)
      assert res.status == LacksNothing
      if res.decl.symKind == TypeY:
        let typeDecl = asTypeDecl(res.decl)

        if not (classifyType(c, typeDecl.body) == ObjectT):
          cond = false
      else:
        cond = false

  if not cond:
    case op
    of NoHook:
      discard
    of DestroyH:
      buildErr c, info, "signature for '=destroy' must be proc[T: object](x: T)"
    of TraceH:
      buildErr c, info, "signature for '=trace' must be proc[T: object](x: var T; env: pointer)"
    of WasmovedH:
      buildErr c, info, "signature for '=wasMoved' must be proc[T: object](x: var T)"
    of CopyH:
      buildErr c, info, "signature for '=copy' must be proc[T: object](x: var T; y: T)"
    of SinkhH:
      buildErr c, info, "signature for '=sink' must be proc[T: object](x: var T; y: T)"
    of DupH:
      buildErr c, info, "signature for '=dup' must be proc[T: object](x: T): T"

proc hookToAttachedOp(op: HookKind): AttachedOp =
  case op
  of DestroyH: attachedDestroy
  of WasmovedH: attachedWasMoved
  of CopyH: attachedCopy
  of SinkhH: attachedSink
  of DupH: attachedDup
  of TraceH, NoHook: attachedTrace

proc registerHook(c: var SemContext; obj: SymId, symId: SymId, op: HookKind; isGeneric: bool) =
  let attachedOp = hookToAttachedOp(op)
  c.hookIndexLog[attachedOp].add HookIndexEntry(typ: obj, hook: symId, isGeneric: isGeneric)
  programs.registerHook(c.thisModuleSuffix, obj, attachedOp, symId, isGeneric)

proc getHookName(symId: SymId): string =
  result = pool.syms[symId]
  extractBasename(result)
  #result = result.normalize

proc semHook(c: var SemContext; name: string; beforeParams: int; symId: SymId, info: PackedLineInfo): TypeCursor =
  let params = getParamsType(c, beforeParams)
  case name
  of "=destroy":
    checkTypeHook(c, params, DestroyH, info)
    result = params[0]
  of "=wasMoved":
    checkTypeHook(c, params, WasmovedH, info)
    result = params[0]
  of "=trace":
    checkTypeHook(c, params, TraceH, info)
    result = params[0]
  of "=copy":
    checkTypeHook(c, params, CopyH, info)
    result = params[0]
  of "=sink":
    checkTypeHook(c, params, SinkhH, info)
    result = params[0]
  of "=dup":
    checkTypeHook(c, params, DupH, info)
    result = params[0]
  else:
    bug "unreachable"

proc hookToKind(name: string): HookKind =
  case name
  of "=destroy": DestroyH
  of "=wasMoved": WasmovedH
  of "=trace": TraceH
  of "=copy": CopyH
  of "=sink": SinkhH
  of "=dup": DupH
  else: NoHook

proc attachConverter(c: var SemContext; symId: SymId;
                     declStart, beforeExportMarker, beforeGenericParams: int; info: PackedLineInfo) =
  let root = nominalRoot(c.routine.returnType)
  if root == SymId(0) and not c.g.config.compat:
    var errBuf = createTokenBuf(16)
    swap c.dest, errBuf
    buildErr c, info, "cannot attach converter to type " & typeToString(c.routine.returnType)
    swap c.dest, errBuf
    c.dest.insert errBuf, declStart
  else:
    c.converters.mgetOrPut(root, @[]).add(symId)
    if c.dest[beforeExportMarker].kind != DotToken:
      # exported
      if not (c.dest[beforeGenericParams].kind == ParLe and
          pool.tags[c.dest[beforeGenericParams].tagId] == $InvokeT):
        # don't register instances
        c.converterIndexMap.add((root, symId))

proc attachMethod(c: var SemContext; symId: SymId;
                  declStart, beforeParams, beforeGenericParams: int; info: PackedLineInfo) =
  var params = cursorAt(c.dest, beforeParams)
  var root = SymId(0)
  var signature = StrId(0)
  if params.kind == ParLe:
    inc params
    if params.substructureKind == ParamU:
      inc params
      skip params # name
      skip params # export marker
      skip params # pragmas
      root = getClass(params) # can be a generic instance symbol
      var rest = params
      skip rest # type
      skip rest # default value
      skipParRi rest
      var methodName = pool.syms[symId]
      extractBasename methodName
      signature = pool.strings.getOrIncl(methodKey(methodName, rest))
  if root == SymId(0) or not isObjectType(root):
    let typ = typeToString(params)
    c.dest.endRead()
    var errBuf = createTokenBuf(16)
    swap c.dest, errBuf
    buildErr c, info, "cannot attach method to type " & typ
    swap c.dest, errBuf
    c.dest.insert errBuf, declStart
  else:
    c.dest.endRead()
    let methodIsInstance = c.dest[beforeGenericParams].kind == ParLe and c.dest[beforeGenericParams].tagId == TagId(InvokeT)
    var symToRegister = symId
    if methodIsInstance:
      symToRegister = c.dest[beforeGenericParams+1].symId
    c.methods.mgetOrPut(root, @[]).add(symToRegister)
    if not methodIsInstance:
      # don't register instances
      for i in 0..<c.classIndexMap.len:
        if c.classIndexMap[i].cls == root:
          c.classIndexMap[i].methods.add MethodIndexEntry(fn: symId, signature: signature)
          return
      c.classIndexMap.add ClassIndexEntry(cls: root, methods: @[MethodIndexEntry(fn: symId, signature: signature)])

proc hookThatShouldBeMethod(c: var SemContext; hk: HookKind; beforeParams: int): bool =
  case hk
  of DestroyH, TraceH:
    result = false
    var params = cursorAt(c.dest, beforeParams)
    if params.kind == ParLe:
      inc params
      if params.substructureKind == ParamU:
        inc params
        skip params # name
        skip params # export marker
        skip params # pragmas
        result = isInheritable(params, true)
    endRead(c.dest)
  else:
    result = false

proc semProc(c: var SemContext; it: var Item; kind: SymKind; pass: PassKind) =
  let info = it.n.info
  let declStart = c.dest.len
  takeToken c, it.n
  let beforeName = c.dest.len
  let (symId, status) = declareOverloadableSym(c, it, kind)

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
    let beforeGenericParams = c.dest.len
    semGenericParams c, it.n
    let beforeParams = c.dest.len
    semParams c, it.n
    c.routine.returnType = semReturnType(c, it.n)
    var crucial = CrucialPragma(sym: symId)
    semPragmas c, it.n, crucial, kind
    c.routine.pragmas = crucial.flags
    if crucial.hasVarargs.isValid:
      addVarargsParameter c, beforeParams, crucial.hasVarargs
    if crucial.magic.len > 0:
      exportMarkerBecomesNifTag c, beforeExportMarker, crucial
    if status == OkExistingFresh and InjectP in crucial.flags:
      # symbol is injected, add it to scope
      let s = Sym(kind: kind, name: symId, pos: beforeName)
      var name = pool.syms[symId]
      extractBasename(name)
      # go up a scope for the parameter scope:
      c.currentScope.up.addOverloadable(pool.strings.getOrIncl(name), s)
    if it.n.kind == DotToken:
      takeToken c, it.n
    else:
      buildErr c, it.n.info, "`effects` must be empty"
      skip it.n

    publishSignature c, symId, declStart
    let hookName = getHookName(symId)
    let hk = hookToKind(hookName)
    if status in {OkNew, OkExistingFresh}:
      if kind == ConverterY:
        attachConverter c, symId, declStart, beforeExportMarker, beforeGenericParams, info
      elif kind == MethodY:
        attachMethod c, symId, declStart, beforeParams, beforeGenericParams, info
      elif hookThatShouldBeMethod(c, hk, beforeParams):
        c.dest[declStart] = parLeToken(MethodS, info)
        attachMethod c, symId, declStart, beforeParams, beforeGenericParams, info
    if it.n.kind != DotToken:
      case pass
      of checkGenericInst:
        if it.n.stmtKind != StmtsS:
          bug "(stmts) expected, but got ", it.n
        c.openScope() # open body scope
        takeToken c, it.n
        var resId = SymId(0)
        if UntypedP in crucial.flags:
          # for untyped generic procs, need to add result symbol now
          resId = declareResult(c, it.n.info)
        semProcBody c, it
        c.closeScope() # close body scope
        c.closeScope() # close parameter scope
        if resId != SymId(0):
          addReturnResult c, resId, it.n.info

        if hk != NoHook:
          let params = getParamsType(c, beforeParams)
          assert params.len >= 1
          let obj = getObjSymId(c, params[0])
          registerHook(c, obj, symId, hk, false)

      of checkBody:
        if it.n.stmtKind != StmtsS:
          bug "(stmts) expected, but got ", it.n
        c.openScope() # open body scope
        var resId = SymId(0)
        if UntypedP in crucial.flags and c.routine.inGeneric > 0: # includes templates
          # should eventually be default for compat mode
          let mode = if kind == TemplateY: UntypedTemplate else: UntypedGeneric
          var ctx = createUntypedContext(addr c, mode)
          addParams(ctx, beforeGenericParams)
          addParams(ctx, beforeParams)
          semTemplBody ctx, it.n
        else:
          takeToken c, it.n
          resId = declareResult(c, it.n.info)
          semProcBody c, it
        c.closeScope() # close body scope
        c.closeScope() # close parameter scope
        addReturnResult c, resId, it.n.info
        let name = getHookName(symId)
        let hk = hookToKind(name)
        if hk != NoHook:
          let objCursor = semHook(c, hookName, beforeParams, symId, info)
          let obj = getObjSymId(c, objCursor)

          # because it's a hook for sure
          registerHook(c, obj, symId, hk, c.routine.inGeneric > 0)

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
      if ErrorP in crucial.flags and pass in {checkGenericInst, checkBody}:
        let name = getHookName(symId)
        let hk = hookToKind(name)
        if hk != NoHook:
          let objCursor = semHook(c, name, beforeParams, symId, info)
          let obj = getObjSymId(c, objCursor)
          registerHook(c, obj, symId, hk, c.routine.inGeneric > 0)
        takeToken c, it.n
      elif BorrowP in crucial.flags and pass in {checkGenericInst, checkBody}:
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
  takeParRi c, it.n
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
      let ident = cursorAt(orig, 0)
      if pool.syms.hasId(s.name):
        c.buildErr ident.info, "undeclared identifier: " & pool.syms[s.name], ident
      else:
        c.buildErr ident.info, "undeclared identifier", ident
    it.typ = c.types.autoType
  elif s.kind == CchoiceY:
    if KeepMagics notin flags and c.routine.kind != TemplateY:
      c.buildErr c.dest[start].info, "ambiguous identifier"
    it.typ = c.types.autoType
  elif s.kind == BlockY:
    it.typ = c.types.autoType
  elif s.kind in {TypeY, TypevarY}:
    let typeStart = c.dest.len
    let info = c.dest[c.dest.len-1].info
    c.dest.buildTree TypedescT, info:
      let symStart = c.dest.len
      c.dest.add symToken(s.name, info)
      semTypeSym c, s, info, symStart, InLocalDecl
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
      elif s.kind == ModuleY:
        if AllowModuleSym notin flags:
          c.buildErr c.dest[start].info, "module symbol '" & pool.syms[s.name] & "' not allowed in this context"
      else:
        assert false, "not implemented"
      it.typ = n
      commonType c, it, start, expected
    else:
      c.buildErr c.dest[start].info, "could not load symbol: " & pool.syms[s.name] & "; errorCode: " & $res.status
      it.typ = c.types.autoType

proc semLocalTypeExpr(c: var SemContext, it: var Item) =
  let info = it.n.info
  let val = semLocalType(c, it.n)
  let start = c.dest.len
  c.dest.buildTree TypedescT, info:
    c.dest.addSubtree val
  it.typ = typeToCursor(c, start)
  c.dest.shrink start

proc semSubscriptAsgn(c: var SemContext; it: var Item; info: PackedLineInfo) =
  # check if LHS is builtin subscript:
  var subscript = Item(n: it.n, typ: c.types.autoType)
  inc subscript.n # tag
  var subscriptLhsBuf = createTokenBuf(4)
  swap c.dest, subscriptLhsBuf
  var subscriptLhs = Item(n: subscript.n, typ: c.types.autoType)
  semExpr c, subscriptLhs, {KeepMagics}
  swap c.dest, subscriptLhsBuf
  let afterSubscriptLhs = subscriptLhs.n
  subscript.n = afterSubscriptLhs
  subscriptLhs.n = cursorAt(subscriptLhsBuf, 0)
  var subscriptBuf = createTokenBuf(8)
  swap c.dest, subscriptBuf
  let builtin = tryBuiltinSubscript(c, subscript, subscriptLhs)
  swap c.dest, subscriptBuf
  if builtin:
    # build regular assignment:
    c.dest.addParLe(AsgnS, info)
    c.dest.add subscriptBuf
    removeModifier(subscript.typ) # remove `var` for rhs
    semExpr c, subscript # use the type and position from the subscript
    it.n = subscript.n
    takeParRi c, it.n
    producesVoid c, info, it.typ
  else:
    # generate call to `[]=`:
    var callBuf = createTokenBuf(16)
    callBuf.addParLe(CallX, subscriptLhs.n.info)
    callBuf.add identToken(pool.strings.getOrIncl("[]="), subscriptLhs.n.info)
    callBuf.addSubtree subscriptLhs.n
    it.n = afterSubscriptLhs
    while it.n.kind != ParRi:
      # arguments of the subscript
      callBuf.takeTree it.n
    skipParRi it.n # end subscript expression
    callBuf.takeTree it.n # assignment value
    callBuf.addParRi()
    skipParRi it.n # end assignment
    var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
    semCall c, call, {}, SubscriptAsgnCall
    it.typ = call.typ

proc semDotAsgn(c: var SemContext; it: var Item; info: PackedLineInfo) =
  # check if LHS is builtin subscript:
  let dotInfo = it.n.info
  var dot = Item(n: it.n, typ: c.types.autoType)
  inc dot.n # tag
  var dotLhsBuf = createTokenBuf(4)
  swap c.dest, dotLhsBuf
  var dotLhs = Item(n: dot.n, typ: c.types.autoType)
  semExpr c, dotLhs, {KeepMagics}
  swap c.dest, dotLhsBuf
  dot.n = dotLhs.n
  dotLhs.n = cursorAt(dotLhsBuf, 0)
  let fieldName = takeIdent(dot.n)
  # skip optional inheritance depth:
  if dot.n.kind == IntLit:
    inc dot.n
  skipParRi dot.n
  var dotBuf = createTokenBuf(8)
  swap c.dest, dotBuf
  let builtin = tryBuiltinDot(c, dot, dotLhs, fieldName, dotInfo, {}) != FailedDot
  swap c.dest, dotBuf
  if builtin:
    # build regular assignment:
    c.dest.addParLe(AsgnS, info)
    c.dest.add dotBuf
    removeModifier(dot.typ) # remove `var` for rhs
    semExpr c, dot # use the type and position from the dot expression
    it.n = dot.n
    takeParRi c, it.n
    producesVoid c, info, it.typ
  else:
    # generate call to `field=`:
    var callBuf = createTokenBuf(16)
    callBuf.addParLe(CallX, dotLhs.n.info)
    callBuf.add identToken(pool.strings.getOrIncl(pool.strings[fieldName] & "="), dotLhs.n.info)
    callBuf.addSubtree dotLhs.n
    it.n = dot.n
    callBuf.takeTree it.n # assignment value
    callBuf.addParRi()
    skipParRi it.n
    var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
    semCall c, call, {}, DotAsgnCall
    # XXX original compiler also checks if the call fails and tries a dotcall for the LHS
    it.typ = call.typ

proc semAsgn(c: var SemContext; it: var Item) =
  let info = it.n.info
  inc it.n
  case it.n.exprKind
  of AtX:
    semSubscriptAsgn c, it, info
  of DotX, DdotX:
    semDotAsgn c, it, info
  else:
    c.dest.addParLe(AsgnS, info)
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a # infers type of `left-hand-side`
    removeModifier(a.typ) # remove `var` for rhs
    semExpr c, a # ensures type compatibility with `left-hand-side`
    it.n = a.n
    takeParRi c, it.n
    producesVoid c, info, it.typ

proc semEmit(c: var SemContext; it: var Item) =
  let info = it.n.info
  c.dest.add parLeToken(EmitS, info)
  inc it.n
  if it.n.exprKind == BracketX:
    inc it.n
    while it.n.kind != ParRi:
      var a = Item(n: it.n, typ: c.types.autoType)
      semExpr c, a
      it.n = a.n
    skipParRi it.n
  else:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a
    it.n = a.n
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semDiscard(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    let exInfo = it.n.info
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a
    it.n = a.n
    if classifyType(c, it.typ) == VoidT:
      buildErr c, exInfo, "expression of type `" & typeToString(it.typ) & "` must not be discarded"
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semStmtBranch(c: var SemContext; it: var Item; isNewScope: bool) =
  # handle statements that could be expressions
  case classifyType(c, it.typ)
  of AutoT:
    semExpr c, it
  of VoidT:
    # performs discard check:
    semStmt c, it.n, isNewScope
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
  if it.n.substructureKind == ElifU:
    while it.n.substructureKind == ElifU:
      takeToken c, it.n
      semBoolExpr c, it.n
      withNewScope c:
        semStmtBranch c, it, true
      takeParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `elif` inside `if` expected"
  if it.n.substructureKind == ElseU:
    takeToken c, it.n
    withNewScope c:
      semStmtBranch c, it, true
    takeParRi c, it.n
  takeParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semExceptionType(c: var SemContext; it: var Item): TypeCursor =
  let start = c.dest.len
  result = semLocalType(c, it.n)
  if result.kind != Symbol or pool.syms[result.symId] != ErrorCodeName:
    c.dest.shrink start
    buildErr c, it.n.info, "type in `except` must be `system.ErrorCode`"

proc semTry(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  withNewScope c:
    semStmtBranch c, it, true
  while it.n.substructureKind == ExceptU:
    openScope(c)
    takeToken c, it.n
    if it.n.kind == DotToken:
      takeToken c, it.n
    elif it.n.exprKind in CallKinds and it.n.firstSon.kind == Ident and pool.strings[it.n.firstSon.litId] == "as":
      # `Type as e`:
      inc it.n
      inc it.n
      let start = c.dest.len
      let etyp = semExceptionType(c, it)
      c.dest.shrink start
      var decl = createTokenBuf(8)
      decl.addParLe(LetS, info)
      decl.takeTree it.n # name
      skipParRi it.n
      decl.addEmpty() # export marker
      decl.addEmpty() # pragmas
      decl.addSubtree etyp
      decl.addEmpty() # value
      decl.addParRi()
      var dd = beginRead(decl)
      semLocal c, dd, LetY
    elif it.n.stmtKind == LetS:
      # resem the local declaration:
      semLocal(c, it.n, LetY)
    else:
      discard semExceptionType(c, it)
    semStmtBranch c, it, true
    takeParRi c, it.n
    closeScope(c)
  if it.n.substructureKind == FinU:
    takeToken c, it.n
    withNewScope c:
      semStmt c, it.n, true
    takeParRi c, it.n
  takeParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semWhenImpl(c: var SemContext; it: var Item; mode: WhenMode) =
  let start = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var leaveUnresolved = false
  if it.n.substructureKind == ElifU:
    while it.n.substructureKind == ElifU:
      takeToken c, it.n
      let condStart = c.dest.len
      var phase = SemcheckBodies
      swap c.phase, phase
      semConstBoolExpr c, it.n, allowUnresolved = c.routine.inGeneric > 0
      swap c.phase, phase
      let condValue = cursorAt(c.dest, condStart).exprKind
      endRead(c.dest)
      if not leaveUnresolved:
        if condValue == TrueX:
          c.dest.shrink start
          case mode
          of NormalWhen:
            semExprMissingPhases c, it, SemcheckSignatures
          of ObjectWhen:
            semObjectComponent c, it.n
          skipParRi it.n # finish elif
          skipToEnd it.n
          return
        elif condValue != FalseX:
          # erroring/unresolved condition, leave entire statement as unresolved
          leaveUnresolved = true
      if leaveUnresolved:
        # might have been set above
        var ctx = createUntypedContext(addr c, UntypedGeneric)
        semTemplBody ctx, it.n
      else:
        takeTree c, it.n
      takeParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `elif` inside `if` expected"
  if it.n.substructureKind == ElseU:
    takeToken c, it.n
    if not leaveUnresolved:
      c.dest.shrink start
      case mode
      of NormalWhen:
        semExprMissingPhases c, it, SemcheckSignatures
      of ObjectWhen:
        semObjectComponent c, it.n
      skipParRi it.n # finish else
      skipToEnd it.n
      return
    else:
      var ctx = createUntypedContext(addr c, UntypedGeneric)
      semTemplBody ctx, it.n
    takeParRi c, it.n
  takeParRi c, it.n
  if not leaveUnresolved:
    # none of the branches evaluated, output nothing
    c.dest.shrink start
    producesVoid c, info, it.typ
  else:
    it.typ = c.types.untypedType

proc semWhen(c: var SemContext; it: var Item) =
  case c.phase
  of SemcheckTopLevelSyms:
    # XXX `const`s etc are not evaluated yet, so we cannot compile the `when` conditions
    # so symbols inside of `when` are not defined until `SemcheckSignatures`
    # effectively this means types defined in `when` cannot be used before they are declared
    # but this was already not possible in original Nim
    c.takeTree it.n
    return
  of SemcheckSignatures, SemcheckBodies:
    discard

  semWhenImpl(c, it, NormalWhen)

proc semCaseOfValue(c: var SemContext; it: var Item; selectorType: TypeCursor;
                    seen: var seq[(xint, xint)]) =
  if it.n.substructureKind == RangesU:
    takeToken c, it.n
    while it.n.kind != ParRi:
      let info = it.n.info
      if isRangeExpr(it.n):
        inc it.n # call tag
        skip it.n # `..`
        c.dest.buildTree RangeU, it.n.info:
          let a = evalConstIntExpr(c, it.n, selectorType)
          let b = evalConstIntExpr(c, it.n, selectorType)
          if seen.doesOverlapOrIncl(a, b):
            buildErr c, info, "overlapping values"
        inc it.n # right paren of call
      elif it.n.substructureKind == RangeU:
        takeToken c, it.n
        let a = evalConstIntExpr(c, it.n, selectorType)
        let b = evalConstIntExpr(c, it.n, selectorType)
        if seen.doesOverlapOrIncl(a, b):
          buildErr c, info, "overlapping values"
        takeParRi c, it.n
      else:
        let a = evalConstIntExpr(c, it.n, selectorType)
        if seen.containsOrIncl(a):
          buildErr c, info, "value already handled"
    takeParRi c, it.n
  else:
    buildErr c, it.n.info, "`ranges` within `of` expected"
    skip it.n

proc semCaseOfValueString(c: var SemContext; it: var Item; selectorType: TypeCursor;
                          seen: var HashSet[StrId]) =
  if it.n.substructureKind == RangesU:
    takeToken c, it.n
    while it.n.kind != ParRi:
      let info = it.n.info
      let s = evalConstStrExpr(c, it.n, selectorType) # will error if range is given
      if s != StrId(0): # otherwise error
        # use literal id as value:
        if seen.containsOrIncl(s):
          buildErr c, info, "value already handled"
    takeParRi c, it.n
  else:
    buildErr c, it.n.info, "`ranges` within `of` expected"
    skip it.n

proc checkExhaustiveness(c: var SemContext; info: PackedLineInfo; selectorType: TypeCursor; seen: seq[(xint, xint)]) =
  var total = createXint(0'i32)
  for s in items(seen):
    total = total + s[1] - s[0] + createXint(1'i32)

  var typ = selectorType
  var counter = 20
  while typ.kind == Symbol:
    dec counter
    if counter <= 0: break
    let impl = getTypeSection(typ.symId)
    if impl.kind == TypeY and impl.body.typeKind in {EnumT, HoleyEnumT}:
      typ = impl.body
      break

  if typ.typeKind != HoleyEnumT:
    # quick check based on the `total` count:
    if total == lengthOrd(c, selectorType):
      return

  if typ.typeKind in {EnumT, HoleyEnumT}:
    # check if all values are handled:
    var field = asEnumDecl(typ).firstField
    var missing = ""
    while field.kind != ParRi:
      let f = takeLocal(field, SkipFinalParRi)
      let v: xint
      let vnode = f.val.firstSon # skip tuple tag
      case vnode.kind
      of IntLit:
        v = createXint pool.integers[vnode.intId]
      of UIntLit:
        v = createXint pool.uintegers[vnode.uintId]
      else:
        v = createNaN()
      if not seen.contains(v):
        if missing.len > 0: missing.add ", "
        var isGlobal = false
        missing.add extractBasename(pool.syms[f.name.symId], isGlobal)
    if missing.len > 0:
      buildErr c, info, "not all cases are covered; missing: {" & missing & "}"
  else:
    buildErr c, info, "not all cases are covered"

proc semObjectCaseBranch(c: var SemContext; it: var Item) =
  if it.n.stmtKind == StmtsS:
    takeToken c, it.n
    while it.n.kind != ParRi:
      semObjectComponent c, it.n
    takeParRi c, it.n
  else:
    c.dest.addParLe(StmtsS, it.n.info)
    semObjectComponent c, it.n
    c.dest.addParRi()

proc semCaseImpl(c: var SemContext; it: var Item; mode: CaseMode) =
  let info = it.n.info
  takeToken c, it.n
  var selectorType = default(Cursor)
  case mode
  of NormalCase:
    var selector = Item(n: it.n, typ: c.types.autoType)
    semExpr c, selector
    it.n = selector.n
    selectorType = skipModifier(selector.typ)
  of ObjectCase:
    let selectorStart = c.dest.len
    semLocal(c, it.n, FldY)
    let field = cursorAt(c.dest, selectorStart)
    let fieldType = asLocal(field).typ
    let fieldTypePos = cursorToPosition(c.dest, fieldType)
    endRead(c.dest)
    selectorType = typeToCursor(c, fieldTypePos)
    if not isOrdinalType(selectorType):
      buildErr c, info, "selector must be of an ordinal type"

  let isString = isSomeStringType(selectorType)
  var seen: seq[(xint, xint)] = @[]
  var seenStr = initHashSet[StrId]()
  if it.n.substructureKind == OfU:
    while it.n.substructureKind == OfU:
      takeToken c, it.n
      if isString:
        semCaseOfValueString c, it, selectorType, seenStr
      else:
        semCaseOfValue c, it, selectorType, seen
      case mode
      of NormalCase:
        withNewScope c:
          semStmtBranch c, it, true
      of ObjectCase:
        semObjectCaseBranch(c, it)
      takeParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `of` inside `case` expected"
  if it.n.substructureKind == ElseU:
    takeToken c, it.n
    case mode
    of NormalCase:
      withNewScope c:
        semStmtBranch c, it, true
    of ObjectCase:
      semObjectCaseBranch(c, it)
    takeParRi c, it.n
  elif not isString:
    checkExhaustiveness c, it.n.info, selectorType, seen

  takeParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semCase(c: var SemContext; it: var Item) =
  semCaseImpl(c, it, NormalCase)

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
    takeParRi c, it.n
    publish c, delayed.s.name, declStart
  else:
    buildErr c, it.n.info, "illformed AST: `let` inside `unpackflat` expected"
    skip it.n

proc isIterator(c: var SemContext; s: SymId): bool =
  let sym = fetchSym(c, s)
  let res = declToCursor(c, sym)
  result = res.status == LacksNothing and res.decl.symKind == IteratorY

proc semForLoopTupleVar(c: var SemContext; it: var Item; tup: TypeCursor) =
  var tup = tup
  inc tup
  while it.n.kind != ParRi and tup.kind != ParRi:
    let field = getTupleFieldType(tup)
    if it.n.substructureKind == UnpacktupU:
      takeToken c, it.n
      if field.typeKind == TupleT:
        semForLoopTupleVar c, it, field
      else:
        buildErr c, it.n.info, "tuple types expected, but got: " & $field
      takeParRi c, it.n
    else:
      semForLoopVar c, it, field
    skip tup
  if it.n.kind == ParRi:
    if tup.kind == ParRi:
      discard "all fine"
    else:
      buildErr c, it.n.info, "too few for loop variables"
  else:
    buildErr c, it.n.info, "too many for loop variables"
    skipToEnd it.n

include semfields

proc isIteratorCall(c: var SemContext; beforeCall: int): bool {.inline.} =
  result = c.dest.len > beforeCall+1
  if result:
    let callKind =
      if c.dest[beforeCall].kind == ParLe and rawTagIsNimonyExpr(tagEnum(c.dest[beforeCall])):
        cast[NimonyExpr](tagEnum(c.dest[beforeCall]))
      else:
        NoExpr
    result = callKind in CallKinds and
      c.dest[beforeCall+1].kind == Symbol and
      c.isIterator(c.dest[beforeCall+1].symId)

proc isIdentCall(c: var SemContext; beforeCall: int): bool {.inline.} =
  result = c.dest.len > beforeCall+1
  if result:
    let callKind =
      if c.dest[beforeCall].kind == ParLe and rawTagIsNimonyExpr(tagEnum(c.dest[beforeCall])):
        cast[NimonyExpr](tagEnum(c.dest[beforeCall]))
      else:
        NoExpr
    result = callKind in CallKinds and
      c.dest[beforeCall+1].kind == Ident

proc semFor(c: var SemContext; it: var Item) =
  let info = it.n.info
  let orig = it.n
  takeToken c, it.n
  var iterCall = Item(n: it.n, typ: c.types.autoType)
  let callInfo = iterCall.n.info
  let beforeCall = c.dest.len
  semExpr c, iterCall, {PreferIterators, KeepMagics}
  it.n = iterCall.n
  var isMacroLike = false
  if isIteratorCall(c, beforeCall):
    discard "fine"
  elif c.dest[beforeCall].kind == ParLe and
      (c.dest[beforeCall].tagId == TagId(FieldsTagId) or
       c.dest[beforeCall].tagId == TagId(FieldPairsTagId) or
       c.dest[beforeCall].tagId == TagId(InternalFieldPairsTagId)):
    var callBuf = createTokenBuf(c.dest.len - beforeCall)
    for tok in beforeCall ..< c.dest.len: callBuf.add c.dest[tok]
    c.dest.shrink beforeCall-1
    var call = beginRead(callBuf)
    semForFields c, it, call, orig
    return
  elif iterCall.typ.typeKind == UntypedT or
      # for iterators from concepts in generic context:
      isIdentCall(c, beforeCall):
    isMacroLike = true
  else:
    var vars = 0
    var varsCursor = it.n
    if varsCursor.substructureKind == UnpackflatU:
      inc varsCursor
      while varsCursor.kind != ParRi:
        inc vars
        skip varsCursor
    else:
      vars = 1
    var name = ""
    case vars
    of 1:
      name = "items"
    of 2:
      name = "pairs"
    else:
      c.dest.shrink beforeCall
      buildErr c, callInfo, "iterator expected"
    if name != "":
      # try implicit iterator call
      var callBuf = createTokenBuf(32)
      callBuf.addParLe(CallX, callInfo)
      swap c.dest, callBuf
      discard buildSymChoice(c, pool.strings.getOrIncl(name), info, FindAll)
      swap c.dest, callBuf
      for tok in beforeCall ..< c.dest.len: callBuf.add c.dest[tok]
      callBuf.addParRi()
      let argType = iterCall.typ
      iterCall = Item(n: beginRead(callBuf), typ: c.types.autoType)
      shrink c.dest, beforeCall
      semCall c, iterCall, {}
      if isIteratorCall(c, beforeCall):
        discard "fine"
      elif iterCall.typ.typeKind == UntypedT or
          # for iterators from concepts in generic context:
          isIdentCall(c, beforeCall):
        isMacroLike = true
      else:
        if c.dest[beforeCall].kind == ParLe and c.dest[beforeCall].tagId == ErrT:
          # original nim gives `items` overload errors so preserve them
          discard
        else:
          c.dest.shrink beforeCall
          buildErr c, callInfo, "no implicit `" & name & "` iterator found for type " & typeToString(argType)
  withNewScope c:
    case substructureKind(it.n)
    of UnpackflatU:
      takeToken c, it.n
      if iterCall.typ.typeKind == TupleT:
        semForLoopTupleVar c, it, iterCall.typ
      else:
        semForLoopVar c, it, iterCall.typ

      takeParRi c, it.n
    of UnpacktupU:
      takeToken c, it.n
      if iterCall.typ.typeKind == TupleT:
        semForLoopTupleVar c, it, iterCall.typ
      else:
        buildErr c, it.n.info, "tuple types expected, but got: " & $iterCall.typ
      takeParRi c, it.n
    else:
      buildErr c, it.n.info, "illformed AST: `unpackflat` or `unpacktup` inside `for` expected"
      skip it.n

    if isMacroLike and false:
      takeTree c.dest, it.n # don't touch the body
    else:
      inc c.routine.inLoop
      semStmt c, it.n, true
      dec c.routine.inLoop

  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc semReturn(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.kind == NoSym:
    buildErr c, info, "`return` only allowed within a routine"
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
  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc semRaise(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.kind == NoSym:
    buildErr c, info, "`raise` only allowed within a routine"
  elif not c.routine.pragmas.contains(RaisesP) and not c.g.config.compat:
    buildErr c, info, "`raise` only allowed within a routine with `raises` pragma"
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a
    if a.typ.kind == Symbol and pool.syms[a.typ.symId] == ErrorCodeName:
      discard "ok"
    else:
      buildErr c, info, "only type `system.ErrorCode` is allowed to be raised"
    it.n = a.n
  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc semYield(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.kind != IteratorY:
    buildErr c, info, "`yield` only allowed within an `iterator`"
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.routine.returnType)
    semExpr c, a
    it.n = a.n
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semTypePragmas(c: var SemContext; n: var Cursor; sym: SymId; beforeExportMarker: int): CrucialPragma =
  result = CrucialPragma(sym: sym)
  semPragmas c, n, result, TypeY # 2
  if result.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, result

proc fitTypeToPragmas(c: var SemContext; pragmas: CrucialPragma; typeStart: int) =
  if {ImportcP, ImportcppP} * pragmas.flags != {}:
    let typ = cursorAt(c.dest, typeStart)
    if isNominal(typ.typeKind):
      # ok
      endRead(c.dest)
    elif typ.typeKind in {IntT, UintT, FloatT, CharT}:
      let info = typ.info
      endRead(c.dest)
      let kind = if ImportcP in pragmas.flags: ImportcP else: ImportcppP
      var tokens = @[
        parLeToken(kind, info),
        strToken(pool.strings.getOrIncl(pragmas.externName), info),
        parRiToken(info)
      ]
      if HeaderP in pragmas.flags:
        assert pragmas.headerFileTok.kind == StringLit
        tokens.add [
          parLeToken(HeaderP, info),
          pragmas.headerFileTok,
          parRiToken(info)
        ]
      c.dest.insert tokens, typeStart+2
    else:
      let err = "cannot import type " & typeToString(typ)
      let info = typ.info
      endRead(c.dest)
      c.buildErr info, err

proc buildInnerObjDecl(c: var SemContext; decl: Cursor; sym: var SymId): TokenBuf =
  ## build inner object type declaration from full ref/ptr object decl
  result = createTokenBuf(64)

  # make anon object symbol from `sym` and set `sym` to it:
  var isGlobal = false
  let basename = extractBasename(pool.syms[sym], isGlobal)
  var objName = basename & ".Obj"
  if isGlobal: c.makeGlobalSym(objName)
  else: c.makeLocalSym(objName)
  sym = pool.syms.getOrIncl(objName)

  var n = decl
  result.add n # (type
  inc n
  result.add symdefToken(sym, n.info)
  skip n # name
  takeTree result, n # copy exported (?)
  takeTree result, n # copy typevars
  # ^ may need to build fresh identifiers
  takeTree result, n # copy pragmas
  assert n.typeKind in {RefT, PtrT}
  inc n # (ref/ptr
  takeTree result, n # copy (object)
  skipParRi n # ) from ref/ptr
  takeParRi result, n # ) from type

proc invokeInnerObj(c: var SemContext; genericsPos: int; objSym: SymId; info: PackedLineInfo) =
  var params = cursorAt(c.dest, genericsPos)
  if params.substructureKind == TypevarsU:
    # build an invocation of the inner object type
    inc params
    var invokeBuf = createTokenBuf(16)
    invokeBuf.buildTree InvokeT, info:
      invokeBuf.add symToken(objSym, info)
      while params.kind != ParRi:
        let typevar = asTypevar(params).name
        if typevar.kind == SymbolDef:
          invokeBuf.add symToken(typevar.symId, typevar.info)
        else:
          # assume it was left as an identifier
          invokeBuf.addSubtree typevar
        skip params
    endRead(c.dest)
    c.dest.add invokeBuf
  else:
    # enough to use object sym directly
    endRead(c.dest)
    c.dest.add symToken(objSym, info)

proc semTypeSection(c: var SemContext; n: var Cursor) =
  let startCursor = n
  let declStart = c.dest.len
  takeToken c, n
  # name, export marker, generic params, pragmas, body
  var delayed = handleSymDef(c, n, TypeY) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1

  var isEnumTypeDecl = false
  var isRefPtrObj = false
  var innerObjDecl = default(TokenBuf)

  let beforeGenerics = c.dest.len
  var isGeneric: bool
  let prevGeneric = c.routine.inGeneric
  let prevInst = c.routine.inInst
  if n.kind == DotToken:
    takeToken c, n
    isGeneric = false
  else:
    let oldScopeKind = c.currentScope.kind
    openScope c
    semGenericParams c, n
    # copy toplevel scope status for exported fields
    c.currentScope.kind = oldScopeKind
    isGeneric = true

  let crucial = semTypePragmas(c, n, delayed.s.name, beforeExportMarker)
  if delayed.status == OkExistingFresh and InjectP in crucial.flags:
    # symbol is injected, add it to scope
    delayed.status = OkNew

  if c.phase == SemcheckSignatures or
      (delayed.status in {OkNew, OkExistingFresh} and
        c.phase != SemcheckTopLevelSyms):
    # body:
    if n.kind == DotToken:
      takeToken c, n
    else:
      let typeStart = c.dest.len
      case n.typeKind
      of EnumT, HoleyEnumT:
        semEnumType(c, n, delayed.s.name, beforeExportMarker)
        isEnumTypeDecl = true
      of RefT, PtrT:
        var obj = n
        inc obj
        if obj.typeKind == ObjectT:
          isRefPtrObj = true
          var objSym = delayed.s.name
          innerObjDecl = buildInnerObjDecl(c, startCursor, objSym)
          takeToken c, n # ref/ptr tag
          invokeInnerObj(c, beforeGenerics, objSym, n.info)
          skip n
          takeParRi c, n
        else:
          semLocalTypeImpl c, n, InTypeSection
      else:
        semLocalTypeImpl c, n, InTypeSection
      fitTypeToPragmas(c, crucial, typeStart)
  else:
    if n.typeKind in {RefT, PtrT}:
      var obj = n
      inc obj
      if obj.typeKind == ObjectT:
        # handle these here too for better forward decls
        isRefPtrObj = true
        var objSym = delayed.s.name
        innerObjDecl = buildInnerObjDecl(c, startCursor, objSym)
        takeToken c, n # ref/ptr tag
        invokeInnerObj(c, beforeGenerics, objSym, n.info)
        skip n
        takeParRi c, n
    if not isRefPtrObj: # body not already handled
      if isGeneric:
        # capture typevars for instantiation of forward declared types to work
        var ctx = createUntypedContext(addr c, UntypedForwardGeneric)
        addParams(ctx, beforeGenerics)
        semTemplBody ctx, n # body
      else:
        c.takeTree n # body
  if isGeneric:
    closeScope c
    c.routine.inGeneric = prevGeneric # revert increase by semGenericParams
    c.routine.inInst = prevInst

  c.addSym delayed
  takeParRi c, n


  publish c, delayed.s.name, declStart

  if isEnumTypeDecl:
    var enumTypeDecl = tryLoadSym(delayed.s.name)
    assert enumTypeDecl.status == LacksNothing
    swap c.dest, c.pending
    genEnumToStrProc(c, enumTypeDecl.decl)
    swap c.dest, c.pending

  if isRefPtrObj:
    if c.phase != SemcheckTopLevelSyms:
      var topLevelDest = createTokenBuf(64)
      var topLevelRead = beginRead(innerObjDecl)
      var phase = SemcheckTopLevelSyms
      swap c.phase, phase
      swap c.dest, topLevelDest
      semTypeSection c, topLevelRead
      swap c.dest, topLevelDest
      swap c.phase, phase
      innerObjDecl = topLevelDest
    var decl = beginRead(innerObjDecl)
    semTypeSection c, decl

proc semTypedBinaryArithmetic(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  semExpr c, it
  semExpr c, it
  takeParRi c, it.n
  commonType c, it, beforeExpr, typ

proc semShift(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  semExpr c, it
  var shift = Item(n: it.n, typ: c.types.autoType)
  let shiftInfo = shift.n.info
  semExpr c, shift
  it.n = shift.n
  if shift.typ.typeKind notin {IntT, UIntT}:
    c.buildErr shiftInfo, "expected integer for shift operand"
  takeParRi c, it.n
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
  takeParRi c, it.n
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
  takeParRi c, it.n
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
  takeParRi c, it.n
  commonType c, it, beforeExpr, typ

proc semBracket(c: var SemContext, it: var Item; flags: set[SemFlag]) =
  let exprStart = c.dest.len
  let info = it.n.info
  inc it.n
  c.dest.addParLe(AconstrX, info)
  if it.n.kind == ParRi:
    # empty array
    if it.typ.typeKind == AutoT:
      if AllowEmpty in flags:
        # keep it.typ as auto
        c.dest.addSubtree it.typ
      else:
        buildErr c, info, "empty array needs a specified type"
    else:
      c.dest.addSubtree it.typ
    takeParRi c, it.n
    return

  let typeInsertPos = c.dest.len
  var elem = Item(n: it.n, typ: c.types.autoType)
  var freshElemType = true # whether the array element type is being inferred from the first element
  case it.typ.typeKind
  of ArrayT: # , SeqT, OpenArrayT
    var arr = it.typ
    inc arr
    elem.typ = arr
    freshElemType = false
  of AutoT: discard
  else:
    buildErr c, info, "invalid expected type for array constructor: " & typeToString(it.typ)
  # XXX index types, `index: value` etc not implemented
  semExpr c, elem
  if freshElemType:
    # do not save modifier in array type unless it was annotated as such
    # also do not expect it from subsequent elements
    removeModifier(elem.typ)
  var count = 1
  while elem.n.kind != ParRi:
    semExpr c, elem
    inc count
  it.n = elem.n
  takeParRi c, it.n
  let typeStart = c.dest.len
  c.dest.buildTree ArrayT, info:
    c.dest.addSubtree elem.typ
    c.dest.addParLe(RangetypeT, info)
    c.dest.addSubtree c.types.intType
    c.dest.addIntLit(0, info)
    c.dest.addIntLit(count - 1, info)
    c.dest.addParRi()
  let expected = it.typ
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  c.dest.insert it.typ, typeInsertPos
  commonType c, it, exprStart, expected

proc semCurly(c: var SemContext, it: var Item; flags: set[SemFlag]) =
  let exprStart = c.dest.len
  let info = it.n.info
  inc it.n
  c.dest.addParLe(SetConstrX, info)
  if it.n.kind == ParRi:
    # empty set
    if it.typ.typeKind == AutoT:
      if AllowEmpty in flags:
        # keep it.typ as auto
        c.dest.addSubtree it.typ
      else:
        buildErr c, info, "empty set needs a specified type"
    else:
      c.dest.addSubtree it.typ
    takeParRi c, it.n
    return

  let typeInsertPos = c.dest.len
  var elem = Item(n: it.n, typ: c.types.autoType)
  var freshElemType = true # whether the set element type is being inferred from the first element
  case it.typ.typeKind
  of SetT:
    var t = it.typ
    inc t
    elem.typ = t
    freshElemType = false
  of AutoT: discard
  else:
    buildErr c, info, "invalid expected type for set constructor: " & typeToString(it.typ)
  var elemStart = c.dest.len
  var elemInfo = elem.n.info
  while elem.n.kind != ParRi:
    if isRangeExpr(elem.n):
      inc elem.n # call tag
      skip elem.n # `..`
      c.dest.buildTree RangeU, elem.n.info:
        elemStart = c.dest.len
        elemInfo = elem.n.info
        semExpr c, elem
        if freshElemType:
          # do not save modifier in set type unless it was annotated as such
          # also do not expect it from subsequent elements
          removeModifier(elem.typ)
          freshElemType = false
        semExpr c, elem
      inc elem.n # right paren of call
    elif elem.n.substructureKind == RangeU:
      takeToken c, elem.n
      semExpr c, elem
      if freshElemType:
        # do not save modifier in set type unless it was annotated as such
        # also do not expect it from subsequent elements
        removeModifier(elem.typ)
        freshElemType = false
      semExpr c, elem
      takeParRi c, elem.n
    else:
      semExpr c, elem
      if freshElemType:
        # do not save modifier in set type unless it was annotated as such
        # also do not expect it from subsequent elements
        removeModifier(elem.typ)
        freshElemType = false
  if containsGenericParams(elem.typ):
    discard
  elif not isOrdinalType(elem.typ, allowEnumWithHoles = true):
    c.buildErr elemInfo, "set element type must be ordinal"
  #elif elem.typ.typeKind == IntT and c.dest[elemStart].kind == IntLit:
  #  set to range of 0..<DefaultSetElements
  else:
    let length = lengthOrd(c, elem.typ)
    if length.isNaN or length > MaxSetElements:
      c.buildErr elemInfo, "type " & typeToString(elem.typ) & " is too large to be a set element type"
  it.n = elem.n
  takeParRi c, it.n
  let typeStart = c.dest.len
  c.dest.buildTree SetT, info:
    c.dest.addSubtree elem.typ
  let expected = it.typ
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  c.dest.insert it.typ, typeInsertPos
  commonType c, it, exprStart, expected

proc semArrayConstr(c: var SemContext; it: var Item) =
  let start = c.dest.len
  let expected = it.typ
  let info = it.n.info
  takeToken c, it.n
  it.typ = semLocalType(c, it.n)
  # XXX type length not enforced
  var elem = Item(n: it.n, typ: c.types.autoType)
  let t = skipModifier(it.typ)
  if t.typeKind == ArrayT:
    elem.typ = t
    inc elem.typ
  else:
    c.buildErr info, "expected array type for array constructor, got: " & typeToString(t)
  while elem.n.kind != ParRi:
    semExpr c, elem
  it.n = elem.n
  takeParRi c, it.n
  commonType c, it, start, expected

proc semSetConstr(c: var SemContext; it: var Item) =
  let start = c.dest.len
  let expected = it.typ
  let info = it.n.info
  takeToken c, it.n
  it.typ = semLocalType(c, it.n)
  var elem = Item(n: it.n, typ: c.types.autoType)
  let t = skipModifier(it.typ)
  if t.typeKind == SetT:
    elem.typ = t
    inc elem.typ
  else:
    c.buildErr info, "expected set type for set constructor, got: " & typeToString(t)
  while elem.n.kind != ParRi:
    if elem.n.substructureKind == RangeU:
      takeToken c, elem.n
      semExpr c, elem
      semExpr c, elem
      takeParRi c, elem.n
    else:
      semExpr c, elem
  it.n = elem.n
  takeParRi c, it.n
  commonType c, it, start, expected

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
  of "R", "T": it.typ = c.types.stringType
  of "C": it.typ = c.types.cstringType
  else:
    c.buildErr it.n.info, "unknown suffix: " & pool.strings[it.n.litId]
  takeToken c, it.n # suffix
  takeParRi c, it.n # right paren
  commonType c, it, exprStart, expected

proc semTup(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  let origExpected = it.typ
  c.dest.add parLeToken(TupConstrX, it.n.info)
  inc it.n
  if it.n.kind == ParRi:
    it.typ = c.types.emptyTupleType
    c.dest.addSubtree it.typ
    takeParRi c, it.n
    commonType c, it, exprStart, origExpected
    return
  let typePos = c.dest.len
  var expected = origExpected
  var doExpected = expected.typeKind == TupleT
  if doExpected:
    inc expected # skip tag, now at fields
  let named = it.n.substructureKind == KvU
  var typ = createTokenBuf(32)
  typ.add parLeToken(TupleT, it.n.info)
  while it.n.kind != ParRi:
    if named:
      if it.n.substructureKind != KvU:
        c.buildErr it.n.info, "expected field name for named tuple constructor"
      else:
        typ.add it.n
        takeToken c, it.n
        let nameCursor = it.n
        let name = takeIdent(it.n)
        let nameStart = c.dest.len
        if name == StrId(0):
          c.buildErr nameCursor.info, "invalid tuple field name", nameCursor
        else:
          c.dest.add identToken(name, nameCursor.info)
        for tok in nameStart ..< c.dest.len:
          typ.add c.dest[tok]
    var elem = Item(n: it.n, typ: c.types.autoType)
    var freshElemType = true
    if doExpected:
      elem.typ = getTupleFieldType(expected)
      freshElemType = false
      skip expected
      if expected.kind == ParRi:
        # happens if expected tuple type has less fields than constructor
        doExpected = false
    semExpr c, elem
    it.n = elem.n
    if freshElemType:
      # do not save modifier in tuple type unless it was annotated as such
      removeModifier(elem.typ)
    typ.addSubtree elem.typ # type
    if named:
      # should be KvX
      takeParRi c, it.n
      typ.addParRi()
  takeParRi c, it.n
  typ.addParRi()
  let typeStart = c.dest.len
  var t = typ.cursorAt(0)
  semTupleType c, t
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  c.dest.insert it.typ, typePos
  commonType c, it, exprStart, origExpected

proc semTupleConstr(c: var SemContext, it: var Item) =
  let start = c.dest.len
  let expected = it.typ
  let info = it.n.info
  takeToken c, it.n
  it.typ = semLocalType(c, it.n)
  var t = skipModifier(it.typ)
  if t.typeKind != TupleT:
    c.dest.shrink start
    c.buildErr info, "expected tuple type for tuple constructor, got: " & typeToString(t)
    return
  inc t
  while it.n.kind != ParRi:
    let named = it.n.substructureKind == KvU
    if named:
      takeToken c, it.n
      takeTree c, it.n
    var elem = Item(n: it.n, typ: c.types.autoType)
    if t.kind != ParRi:
      elem.typ = getTupleFieldType(t)
      skip t
    else:
      c.buildErr info, "tuple type " & typeToString(it.typ) & " too short for tuple constructor"
    semExpr c, elem
    it.n = elem.n
    if named:
      takeParRi c, it.n
  if t.kind != ParRi:
    c.buildErr info, "tuple type " & typeToString(it.typ) & " too long for tuple constructor"
  takeParRi c, it.n
  commonType c, it, start, expected

proc callDefault(c: var SemContext; typ: Cursor; info: PackedLineInfo) =
  var callBuf = createTokenBuf(16)
  callBuf.addParLe(CallX, info)
  swap c.dest, callBuf
  discard buildSymChoice(c, pool.strings.getOrIncl("default"), info, FindAll)
  swap c.dest, callBuf
  callBuf.addSubtree typ
  callBuf.addParRi()
  var it = Item(n: cursorAt(callBuf, 0), typ: c.types.autoType)
  semCall c, it, {}

proc buildObjConstrField(c: var SemContext; field: Local;
                         setFields: Table[SymId, Cursor]; info: PackedLineInfo;
                         bindings: Table[SymId, Cursor]; depth: int) =
  let fieldSym = field.name.symId
  if fieldSym in setFields:
    c.dest.addSubtree setFields[fieldSym]
  else:
    c.dest.addParLe(KvU, info)
    c.dest.add symToken(fieldSym, info)
    var typ = field.typ
    if bindings.len != 0:
      # fields in generic type AST contain generic params of the type
      # for invoked object types, bindings are built from the given arguments
      # and the field type is instantiated based on them here
      typ = instantiateType(c, typ, bindings)
    callDefault c, typ, info
    if depth != 0:
      c.dest.addIntLit(depth, info)
    c.dest.addParRi()

proc fieldsPresentInInitExpr(c: var SemContext; n: Cursor; setFields: Table[SymId, Cursor]): bool =
  var n = n
  inc n
  result = false
  if n.substructureKind == NilU:
    return
  while n.kind != ParRi:
    let local = takeLocal(n, SkipFinalParRi)
    if local.name.symId in setFields:
      result = true
      break

proc fieldsPresentInBranch(c: var SemContext; n: var Cursor;
                setFields: Table[SymId, Cursor]; info: PackedLineInfo;
                bindings: Table[SymId, Cursor]; depth: int) =
  var branches = 0
  block matched:
    while n.kind != ParRi:
      case n.substructureKind
      of OfU:
        inc n
        skip n
        let hasField = fieldsPresentInInitExpr(c, n, setFields)
        if hasField:
          inc n # stmt
          while n.kind != ParRi:
            let field = takeLocal(n, SkipFinalParRi)
            buildObjConstrField(c, field, setFields, info, bindings, depth)
          skipParRi n
          inc branches
        else:
          skip n

        skipParRi n
      of ElseU:
        inc n
        let hasField = fieldsPresentInInitExpr(c, n, setFields)
        if hasField:
          inc n # stmt
          while n.kind != ParRi:
            let field = takeLocal(n, SkipFinalParRi)
            buildObjConstrField(c, field, setFields, info, bindings, depth)
          skipParRi n # stmt
          inc branches
        else:
          skip n
        skipParRi n
      else:
        error "illformed AST inside case object: ", n

proc buildObjConstrFields(c: var SemContext; n: var Cursor;
                          setFields: Table[SymId, Cursor]; info: PackedLineInfo;
                          bindings: Table[SymId, Cursor]; depth = 0) =
  # XXX for now counts each case object field as separate
  var iter = initObjFieldIter()
  while nextField(iter, n, keepCase = true):
    if n.substructureKind == CaseU:
      var body = n
      inc body
      # selector
      let field = takeLocal(body, SkipFinalParRi)
      buildObjConstrField(c, field, setFields, info, bindings, depth)

      fieldsPresentInBranch(c, body, setFields, info, bindings, depth)
      skip n
    else:
      let field = takeLocal(n, SkipFinalParRi)
      buildObjConstrField(c, field, setFields, info, bindings, depth)

proc buildDefaultObjConstr(c: var SemContext; typ: Cursor;
                           setFields: Table[SymId, Cursor]; info: PackedLineInfo;
                           prebuiltBindings = initTable[SymId, Cursor]()) =
  var constrKind = NoExpr
  var objImpl = typ
  if objImpl.typeKind == RefT:
    constrKind = NewobjX
    inc objImpl
  let invokeArgs = skipInvoke(objImpl)
  var objDecl = default(TypeDecl)
  if objImpl.kind == Symbol:
    objDecl = getTypeSection(objImpl.symId)
    if objDecl.kind == TypeY:
      objImpl = objDecl.objBody
    if objImpl.typeKind == ObjectT:
      if constrKind == NoExpr:
        constrKind = OconstrX
    else:
      c.buildErr info, "cannot build object constructor for type: " & typeToString(objImpl)
      return
  else:
    c.buildErr info, "cannot build object constructor for type: " & typeToString(objImpl)
    return
  c.dest.addParLe(constrKind, info)
  c.dest.addSubtree typ
  var obj = asObjectDecl(objImpl)
  # bindings for invoked object type to get proper types for fields:
  var bindings = prebuiltBindings
  if bindings.len == 0:
    # bindings weren't prebuilt, build here:
    bindings = bindInvokeArgs(objDecl, invokeArgs)
  # same field order as old nim VM: starting with most shallow base type
  if obj.parentType.kind != DotToken:
    # copy original bindings to bring back when iterating the original type:
    let origBindings = bindings
    var bindingBuf = default(TokenBuf) # to store subsequent parent args
    var parentType = obj.parentType
    var depth = 1
    while parentType.kind != DotToken:
      var parentImpl = parentType
      if parentImpl.typeKind in {RefT, PtrT}:
        inc parentImpl
      let parentInvokeArgs = skipInvoke(parentImpl)
      var parentDecl = default(TypeDecl)
      if parentImpl.kind == Symbol:
        parentDecl = getTypeSection(parentImpl.symId)
        if parentDecl.kind == TypeY:
          parentImpl = parentDecl.objBody
        else:
          error "invalid parent object type", parentImpl
      else:
        error "invalid parent object type", parentImpl

      # build bindings for parent type:
      var newBindingBuf = default(TokenBuf)
      let newBindings = bindSubsInvokeArgs(c, parentDecl, newBindingBuf, bindings, parentInvokeArgs)
      # set to current bindings so the next parent type can substitute based on them:
      bindingBuf = newBindingBuf
      bindings = newBindings

      let parent = asObjectDecl(parentImpl)
      var currentField = parent.firstField
      if currentField.kind != DotToken:
        buildObjConstrFields(c, currentField, setFields, info, bindings, depth)
      parentType = parent.parentType
      inc depth
    # bring back original bindings:
    bindings = origBindings
  var currentField = obj.firstField
  if currentField.kind != DotToken:
    buildObjConstrFields(c, currentField, setFields, info, bindings)
  c.dest.addParRi()

proc semObjConstr(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, it.n)
  c.dest.shrink exprStart
  var decl = default(TypeDecl)
  var objType = it.typ
  let isGenericObj = containsGenericParams(objType)
  if objType.typeKind in {RefT, PtrT}:
    inc objType
  let invokeArgs = skipInvoke(objType)
  if objType.kind == Symbol:
    decl = getTypeSection(objType.symId)
    if decl.kind != TypeY:
      # includes typevar case
      c.buildErr info, "expected type for object constructor"
      return
    objType = decl.objBody
    if objType.typeKind != ObjectT:
      c.buildErr info, "expected object type for object constructor"
      return
  # build bindings for invoked object type to get proper types for fields:
  let bindings = bindInvokeArgs(decl, invokeArgs)
  var fieldBuf = createTokenBuf(16)
  var setFieldPositions = initTable[SymId, int]()
  while it.n.kind != ParRi:
    if it.n.substructureKind != KvU:
      c.buildErr it.n.info, "expected key/value pair in object constructor"
      skip it.n
    else:
      let fieldStart = fieldBuf.len
      fieldBuf.add it.n
      inc it.n
      let fieldInfo = it.n.info
      let fieldNameCursor = it.n
      let fieldName = takeIdent(it.n)
      if fieldName == StrId(0):
        c.buildErr fieldInfo, "identifier expected for object field"
        skipUntilEnd it.n
      else:
        var hasFieldSym = false
        var field = ObjField(level: -1)
        if fieldNameCursor.kind == Symbol:
          let sym = fieldNameCursor.symId
          let res = tryLoadSym(sym)
          if res.status == LacksNothing and res.decl.substructureKind == FldU:
            # trust that it belongs to this object for now
            # level is either given or 0
            hasFieldSym = true
            field = ObjField(sym: sym, typ: asLocal(res.decl).typ, level: 0)
          else:
            field = findObjFieldConsiderVis(c, decl, fieldName, bindings)
        else:
          field = findObjFieldConsiderVis(c, decl, fieldName, bindings)
        if field.level >= 0:
          if field.sym in setFieldPositions:
            c.buildErr fieldInfo, "field already set: " & pool.strings[fieldName]
            skip it.n
          else:
            setFieldPositions[field.sym] = fieldStart
            if isGenericObj:
              # do not save generic field sym
              fieldBuf.add identToken(fieldName, fieldInfo)
            else:
              fieldBuf.add symToken(field.sym, fieldInfo)
            # maybe add inheritance depth too somehow?
            var val = Item(n: it.n, typ: field.typ)
            swap c.dest, fieldBuf
            semExpr c, val
            swap c.dest, fieldBuf
            it.n = val.n
        else:
          c.buildErr fieldInfo, "undeclared field: '" & pool.strings[fieldName] & "' for type " & typeToString(it.typ)
          skip it.n
        if it.n.kind != ParRi:
          # inheritance level, reuse if field already has a sym, otherwise set a new one
          if hasFieldSym:
            takeTree fieldBuf, it.n
          else:
            skip it.n
        if not hasFieldSym and field.level > 0:
          # add inheritance level
          fieldBuf.addIntLit(field.level, fieldInfo)
      fieldBuf.addParRi()
      skipParRi it.n
  skipParRi it.n
  var setFields = initTable[SymId, Cursor]()
  for field, pos in setFieldPositions:
    setFields[field] = cursorAt(fieldBuf, pos)
  buildDefaultObjConstr(c, it.typ, setFields, info, bindings)
  commonType c, it, exprStart, expected

proc semObjDefault(c: var SemContext; it: var Item) =
  let exprStart = c.dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, it.n)
  c.dest.shrink exprStart
  skipParRi it.n
  buildDefaultObjConstr(c, it.typ, initTable[SymId, Cursor](), info)
  commonType c, it, exprStart, expected

proc semNewref(c: var SemContext; it: var Item) =
  let exprStart = c.dest.len
  let expected = it.typ
  let info = it.n.info
  c.takeToken it.n
  let beforeTypeArg = c.dest.len
  it.typ = semLocalType(c, it.n)
  c.dest.shrink beforeTypeArg
  if it.typ.typeKind == TypedescT:
    inc it.typ
  c.dest.addSubtree it.typ
  assert it.typ.typeKind == RefT
  let typeForDefault = it.typ.firstSon
  callDefault c, typeForDefault, info
  skip it.n # type
  if it.n.kind != ParRi:
    skip it.n # existing `default(T)` call
  takeParRi c, it.n
  commonType c, it, exprStart, expected

proc buildDefaultTuple(c: var SemContext; typ: Cursor; info: PackedLineInfo) =
  c.dest.addParLe(TupConstrX, info)
  c.dest.addSubtree typ
  var currentField = typ
  inc currentField # skip tuple tag
  while currentField.kind != ParRi:
    let field = getTupleFieldType(currentField)
    callDefault c, field, info
    skip currentField
  c.dest.addParRi()

proc semTupleDefault(c: var SemContext; it: var Item) =
  let exprStart = c.dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, it.n)
  c.dest.shrink exprStart
  skipParRi it.n
  buildDefaultTuple(c, it.typ, info)
  commonType c, it, exprStart, expected

proc semTupAt(c: var SemContext; it: var Item) =
  # has already been semchecked but we do it again:
  let exprStart = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  var tup = Item(n: it.n, typ: c.types.autoType)
  let tupInfo = tup.n.info
  semExpr c, tup
  let tupleType = skipModifier(tup.typ)
  if tupleType.typeKind != TupleT:
    if tupleType.kind == Symbol and getTypeSection(tupleType.symId).kind == TypevarY:
      # for `T: tuple`
      var index = Item(n: tup.n, typ: c.types.autoType)
      semExpr c, index
      it.n = index.n
      takeParRi c, it.n
      it.typ = c.types.untypedType
    else:
      c.buildErr tupInfo, "expected tuple but got: " & typeToString(tupleType)
    return
  var idx = tup.n
  let idxStart = c.dest.len
  let idxInfo = idx.info
  semConstIntExpr c, idx
  var idxValue = evalOrdinal(c, cursorAt(c.dest, idxStart))
  endRead(c.dest)
  it.n = idx
  let zero = createXint(0'i64)
  if idxValue.isNaN or idxValue < zero:
    shrink c.dest, idxStart
    c.buildErr idxInfo, "must be a constant expression >= 0"
    takeParRi c, it.n
  else:
    it.typ = tupleType
    inc it.typ
    # navigate to the proper type within the tuple type:
    let one = createXint(1'i64)
    while true:
      if it.typ.kind == ParRi:
        shrink c.dest, idxStart
        c.buildErr idxInfo, "tuple index too large"
        break
      if idxValue > zero:
        skip it.typ
        idxValue = idxValue - one
      else:
        break
    if it.typ.kind != ParRi:
      it.typ = getTupleFieldType(it.typ)
    takeParRi c, it.n
    commonType c, it, exprStart, expected

proc getDottedIdent(n: var Cursor): string =
  let isError = n.kind == ParLe and n.tagId == ErrT
  if isError:
    inc n
  if n.kind == ParLe and n.exprKind == DotX:
    inc n
    result = getDottedIdent(n)
    let s = takeIdent(n)
    if s == StrId(0) or result == "":
      result = ""
    else:
      result.add(".")
      result.add(pool.strings[s])
    skipParRi n
  else:
    # treat as atom
    let s = takeIdent(n)
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
    c.buildErr info, "invalid expression for defined: " & asNimCode(orig), orig
  else:
    let isDefined = c.g.config.isDefined(name)
    let beforeExpr = c.dest.len
    c.dest.addParLe(if isDefined: TrueX else: FalseX, info)
    c.dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, it, beforeExpr, expected

proc semDeclared(c: var SemContext; it: var Item) =
  inc it.n
  let info = it.n.info
  let orig = it.n
  # XXX maybe always type the argument and check for Symbol/errored Ident instead
  let isError = it.n.kind == ParLe and it.n.tagId == ErrT
  if isError:
    inc it.n
  # does not consider module quoted symbols for now
  let nameId = takeIdent(it.n)
  if isError:
    skipToEnd it.n
  skipParRi it.n
  if nameId == StrId(0):
    c.buildErr info, "invalid expression for declared: " & asNimCode(orig), orig
  else:
    let isDeclared = isDeclared(c, nameId)
    let beforeExpr = c.dest.len
    c.dest.addParLe(if isDeclared: TrueX else: FalseX, info)
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

proc tryExplicitRoutineInst(c: var SemContext; syms: Cursor; it: var Item): bool =
  result = false
  let info = syms.info
  let exprStart = c.dest.len
  # build symchoice first so we can directly add the matching syms:
  c.dest.add parLeToken(AtX, info)
  c.dest.add parLeToken(CchoiceX, info)
  var argBuf = createTokenBuf(16)
  swap c.dest, argBuf
  var argRead = it.n
  while argRead.kind != ParRi:
    semLocalTypeImpl c, argRead, AllowValues
  takeParRi c, argRead
  swap c.dest, argBuf
  let args = cursorAt(argBuf, 0)
  var matches = 0
  var lastMatch = default(Match)
  var instLastMatch = false
  var syms = syms
  var nested = 0
  while true:
    # find matching syms
    case syms.kind
    of ParLe:
      if syms.exprKind in {CchoiceX, OchoiceX}:
        inc nested
        inc syms
      else:
        c.dest.shrink exprStart
        c.buildErr syms.info, "invalid tag in symchoice: " & pool.tags[syms.tagId]
        return
    of ParRi:
      dec nested
      inc syms
    of Symbol:
      let sym = syms.symId
      let routine = getProcDecl(sym)
      let candidate = FnCandidate(kind: routine.kind, sym: sym, typ: routine.params)
      var m = createMatch(addr c)
      m.fn = candidate
      matchTypevars m, candidate, args
      buildTypeArgs(m)
      if not m.err:
        # match
        c.dest.add symToken(sym, syms.info)
        inc matches
        lastMatch = m
        # mark if routine is suitable for instantiation:
        instLastMatch = routine.kind notin {TemplateY, MacroY} and routine.exported.kind != ParLe
      inc syms
    else:
      c.dest.shrink exprStart
      c.buildErr syms.info, "invalid token in symchoice: " & $syms.kind
      return
    if nested == 0: break
  c.dest.addParRi() # close symchoice
  if matches == 0:
    c.dest.shrink exprStart
    result = false
  elif matches == 1 and c.routine.inGeneric == 0 and instLastMatch:
    # can instantiate single match
    c.dest.shrink exprStart
    let inst = c.requestRoutineInstance(lastMatch.fn.sym, lastMatch.typeArgs, lastMatch.inferred, info)
    c.dest.add symToken(inst.targetSym, info)
    it.typ = asRoutine(inst.procType).params
    it.kind = lastMatch.fn.kind
    it.n = argRead
    result = true
  else:
    # multiple matches, leave as subscript of symchoice
    c.dest.add argBuf
    it.n = argRead
    result = true

proc isSinglePar(n: Cursor): bool =
  var n = n
  inc n
  result = n.kind == ParRi

proc tryBuiltinSubscript(c: var SemContext; it: var Item; lhs: Item): bool =
  # it.n is after lhs, at args
  result = false
  if (lhs.n.kind == Symbol and lhs.kind == TypeY and
        isGeneric(getTypeSection(lhs.n.symId))) or
      (lhs.n.typeKind in InvocableTypeMagics and isSinglePar(lhs.n)):
    # lhs is a generic type symbol, this is a generic invocation
    # treat it as a type expression to call semInvoke
    var typeExpr = createTokenBuf(16)
    typeExpr.addParLe(AtX, lhs.n.info)
    typeExpr.addSubtree lhs.n
    while it.n.kind != ParRi:
      takeTree typeExpr, it.n
    skipParRi it.n
    typeExpr.addParRi()
    var typeItem = Item(n: beginRead(typeExpr), typ: it.typ)
    semLocalTypeExpr c, typeItem
    it.typ = typeItem.typ
    return true
  var maybeRoutine = lhs.n
  if maybeRoutine.exprKind in {OchoiceX, CchoiceX}:
    inc maybeRoutine
  if maybeRoutine.kind == Symbol:
    let res = tryLoadSym(maybeRoutine.symId)
    if res.status == LacksNothing and isRoutine(res.decl.symKind):
      # check for explicit generic routine instantiation
      result = tryExplicitRoutineInst(c, lhs.n, it)
      if result: return

proc semBuiltinSubscript(c: var SemContext; it: var Item; lhs: Item) =
  # it.n is after lhs, at args
  if tryBuiltinSubscript(c, it, lhs):
    return

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
  semCall c, call, {}, SubscriptCall
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
  semBuiltinSubscript(c, it, lhs)

proc semTypedAt(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  let lhsInfo = it.n.info
  var lhs = Item(n: it.n, typ: c.types.autoType)
  semExpr c, lhs
  it.n = lhs.n
  var index = Item(n: it.n, typ: c.types.autoType)
  semExpr c, index
  it.n = index.n
  var typ = skipModifier(lhs.typ)
  if typ.typeKind == PtrT:
    inc typ
  case typ.typeKind
  of ArrayT:
    it.typ = typ
    inc it.typ
    # add array index information to the `ArrAtX` magic for easy
    # code generation of index checking:
    var t = it.typ # at element type
    skip t # now at the index type
    if t.typeKind == RangetypeT:
      inc t # tag
      skip t # skip base type
      let first = t
      skip t # now at last
      c.dest.addSubtree t
      var isZero: bool
      case first.kind
      of IntLit:
        isZero = pool.integers[first.intId] == 0
      of UIntLit:
        isZero = pool.uintegers[first.uintId] == 0
      else:
        isZero = true
      if not isZero:
        c.dest.addSubtree first
    # skip the index type information in case we re-semcheck this node
    while it.n.kind != ParRi:
      skip it.n
  of UarrayT:
    it.typ = typ
    inc it.typ
  of CstringT:
    it.typ = c.types.charType
  of SetT:
    it.typ = c.types.uint8Type
  else:
    c.buildErr lhsInfo, "invalid lhs type for typed index: " & typeToString(typ)
  takeParRi c, it.n
  commonType c, it, beforeExpr, expected

proc semConv(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var destType = semLocalType(c, it.n)
  var arg = Item(n: it.n, typ: c.types.autoType)
  var argBuf = createTokenBuf(16)
  swap c.dest, argBuf
  semExpr c, arg
  swap c.dest, argBuf
  it.n = arg.n
  arg.n = cursorAt(argBuf, 0)
  semConvArg(c, destType, arg, info, beforeExpr)
  takeParRi c, it.n
  let expected = it.typ
  it.typ = destType
  commonType c, it, beforeExpr, expected

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
  takeParRi c, it.n
  let expected = it.typ
  it.typ = destType
  commonType c, it, beforeExpr, expected

proc semEnumToStr(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var x = Item(n: it.n, typ: c.types.autoType)

  var exprTokenBuf = createTokenBuf()
  swap c.dest, exprTokenBuf
  semExpr c, x
  swap c.dest, exprTokenBuf
  it.n = x.n
  if containsGenericParams(x.typ):
    discard
  else:
    let typeSymId = x.typ.symId
    let typeName = pool.syms[typeSymId]
    let dollorName = "dollar`." & typeName
    let dollorSymId = pool.syms.getOrIncl(dollorName)
    shrink c.dest, beforeExpr
    c.dest.add parLeToken(CallX, info)
    c.dest.add symToken(dollorSymId, info)
  c.dest.add exprTokenBuf
  takeParRi c, it.n
  let expected = it.typ
  it.typ = c.types.stringType
  commonType c, it, beforeExpr, expected

proc buildLowValue(c: var SemContext; typ: Cursor; info: PackedLineInfo) =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    assert s.status == LacksNothing
    if s.decl.symKind != TypeY:
      c.buildErr typ.info, "cannot get low value of non-type"
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT:
      # first field
      var field = asEnumDecl(decl.body).firstField
      let first = asLocal(field)
      c.dest.add symToken(first.name.symId, info)
    else:
      c.buildErr info, "invalid type for low: " & typeToString(typ)
  of ParLe:
    case typ.typeKind
    of IntT:
      var bitsCursor = typ
      inc bitsCursor # skip int tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        c.dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: low(int8).int64
        of 16: low(int16).int64
        of 32: low(int32).int64
        else: low(int64)
      c.dest.add intToken(pool.integers.getOrIncl(value), info)
      if rawBits != -1:
        c.dest.add strToken(pool.strings.getOrIncl("i" & $rawBits), info)
        c.dest.addParRi()
    of UIntT:
      var bitsCursor = typ
      inc bitsCursor # skip uint tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        c.dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value = 0'u64
      c.dest.add uintToken(pool.uintegers.getOrIncl(value), info)
      if rawBits != -1:
        c.dest.add strToken(pool.strings.getOrIncl("u" & $rawBits), info)
        c.dest.addParRi()
    of CharT:
      c.dest.add charToken('\0', info)
    of RangetypeT:
      var first = typ
      inc first
      let base = first
      skip first
      c.dest.addParLe(ConvX, info)
      c.dest.addSubtree base
      c.dest.addSubtree first
      c.dest.addParRi()
    of ArrayT:
      var index = typ
      inc index # tag
      skip index # element
      buildLowValue(c, index, info)
    of BoolT:
      c.dest.addParLe(FalseX, info)
      c.dest.addParRi()
    of FloatT:
      c.dest.addParLe(NegInfX, info)
      c.dest.addParRi()
    else:
      c.buildErr info, "invalid type for low: " & typeToString(typ)
  else:
    c.buildErr info, "invalid type for low: " & typeToString(typ)

proc buildHighValue(c: var SemContext; typ: Cursor; info: PackedLineInfo) =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    assert s.status == LacksNothing
    if s.decl.symKind != TypeY:
      c.buildErr typ.info, "cannot get high value of non-type"
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT:
      # last field
      var field = asEnumDecl(decl.body).firstField
      var lastField = field
      while field.kind != ParRi:
        lastField = field
        skip field
      let last = asLocal(lastField)
      c.dest.add symToken(last.name.symId, info)
    else:
      c.buildErr info, "invalid type for high: " & typeToString(typ)
  of ParLe:
    case typ.typeKind
    of IntT:
      var bitsCursor = typ
      inc bitsCursor # skip int tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        c.dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: high(int8).int64
        of 16: high(int16).int64
        of 32: high(int32).int64
        else: high(int64)
      c.dest.add intToken(pool.integers.getOrIncl(value), info)
      if rawBits != -1:
        c.dest.add strToken(pool.strings.getOrIncl("i" & $rawBits), info)
        c.dest.addParRi()
    of UIntT:
      var bitsCursor = typ
      inc bitsCursor # skip uint tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        c.dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: high(uint8).uint64
        of 16: high(uint16).uint64
        of 32: high(uint32).uint64
        else: high(uint64)
      c.dest.add uintToken(pool.uintegers.getOrIncl(value), info)
      if rawBits != -1:
        c.dest.add strToken(pool.strings.getOrIncl("u" & $rawBits), info)
        c.dest.addParRi()
    of CharT:
      c.dest.add charToken(high(char), info)
    of RangetypeT:
      var last = typ
      inc last
      let base = last
      skip last
      skip last
      c.dest.addParLe(ConvX, info)
      c.dest.addSubtree base
      c.dest.addSubtree last
      c.dest.addParRi()
    of ArrayT:
      var index = typ
      inc index # tag
      skip index # element
      buildHighValue(c, index, info)
    of BoolT:
      c.dest.addParLe(TrueX, info)
      c.dest.addParRi()
    of FloatT:
      c.dest.addParLe(InfX, info)
      c.dest.addParRi()
    else:
      c.buildErr info, "invalid type for high: " & typeToString(typ)
  else:
    c.buildErr info, "invalid type for high: " & typeToString(typ)

proc semLow(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  let typ = semLocalType(c, it.n)
  takeParRi c, it.n
  if containsGenericParams(typ):
    discard
  else:
    c.dest.shrink beforeExpr
    buildLowValue(c, typ, info)
  let expected = it.typ
  var resultType = typ
  if resultType.typeKind == ArrayT:
    inc resultType # skip tag, get to range type
    skip resultType # skip element type, get to range type
    if resultType.typeKind == RangetypeT:
      inc resultType # skip range tag, get to base type
  elif resultType.typeKind == RangetypeT:
    inc resultType # skip tag, get to base type
  it.typ = resultType
  commonType c, it, beforeExpr, expected

proc semHigh(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  let typ = semLocalType(c, it.n)
  takeParRi c, it.n
  if containsGenericParams(typ):
    discard
  else:
    c.dest.shrink beforeExpr
    buildHighValue(c, typ, info)
  let expected = it.typ
  var resultType = typ
  if resultType.typeKind == ArrayT:
    inc resultType # skip tag
    skip resultType # skip element type, get to range type
    if resultType.typeKind == RangetypeT:
      inc resultType # skip range tag, get to base type
  elif resultType.typeKind == RangetypeT:
    inc resultType # skip tag, get to base type
  it.typ = resultType
  commonType c, it, beforeExpr, expected

proc semVoidHook(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  it.typ = c.types.autoType
  semExpr c, it
  if it.n.kind != ParRi:
    # hook has 2nd argument:
    it.typ = c.types.autoType
    semExpr c, it
  takeParRi c, it.n
  it.typ = c.types.voidType
  commonType c, it, beforeExpr, expected

proc semDupHook(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  it.typ = c.types.autoType
  semExpr c, it
  takeParRi c, it.n
  it.typ = skipModifier(it.typ)
  commonType c, it, beforeExpr, expected

proc semDeref(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  let expected = it.typ
  takeToken c, it.n
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, arg
  it.n = arg.n
  takeParRi c, it.n
  let t = skipModifier(arg.typ)
  case t.typeKind
  of RefT, PtrT:
    it.typ = t
    inc it.typ # get to base type
  else:
    c.buildErr info, "invalid type for deref: " & typeToString(t)
  commonType c, it, beforeExpr, expected

proc semFailed(c: var SemContext; it: var Item) =
  # It is not yet clear how this should work.
  let beforeExpr = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, arg
  it.n = arg.n
  takeParRi c, it.n
  it.typ = c.types.boolType
  commonType c, it, beforeExpr, expected

proc semAddr(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let info = it.n.info
  let expected = it.typ
  let beforeArg = c.dest.len
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, arg
  it.n = arg.n
  takeParRi c, it.n
  let a = cursorAt(c.dest, beforeArg)
  if isAddressable(a) or arg.typ.typeKind in {MutT, LentT}:
    endRead c.dest
  else:
    let asStr = asNimCode(a)
    endRead c.dest
    c.dest.shrink beforeArg
    c.buildErr info, "invalid expression for `addr` operation: " & asStr
    c.dest.addParRi()

  it.typ = ptrTypeOf(c, skipModifier(arg.typ))
  commonType c, it, beforeExpr, expected

proc semSizeof(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let expected = it.typ
  c.takeToken(it.n)
  # handle types
  semLocalTypeImpl c, it.n, InLocalDecl
  c.takeParRi(it.n)
  it.typ = c.types.intType
  commonType c, it, beforeExpr, expected

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

template constGuard(c: var SemContext; body: untyped) =
  if c.phase in {SemcheckSignatures, SemcheckBodies}:
    body
  else:
    c.takeTree it.n

template pragmaGuard(c: var SemContext; body: untyped) =
  if c.phase in {SemcheckSignatures, SemcheckBodies}:
    body
  else:
    c.takeTree it.n

proc semAssumeAssert(c: var SemContext; it: var Item; kind: StmtKind) =
  let info = it.n.info
  inc it.n
  c.dest.addParLe(kind, info)
  semBoolExpr c, it.n
  takeParRi c, it.n

proc semPragmaLine(c: var SemContext; it: var Item; isPragmaBlock: bool) =
  case it.n.pragmaKind
  of BuildP:
    let info = it.n.info
    inc it.n
    var args = newSeq[string]()
    while it.n.kind != ParRi:
      if it.n.kind != StringLit:
        buildErr c, it.n.info, "expected `string` but got: " & asNimCode(it.n)
        skip it.n
      else:
        args.add pool.strings[it.n.litId]
        inc it.n

    skipParRi it.n

    if args.len != 2 and args.len != 3:
      buildErr c, info, "build expected 2 or 3 parameters"

    # XXX: Relative paths in makefile are relative to current working directory, not the location of the makefile.
    let curWorkDir = os.getCurrentDir()
    let currentDir = absoluteParentDir(info.getFile)

    # Extract build pragma arguments
    let compileType = args[0]
    var name = replaceSubs(args[1], currentDir, c.g.config).toAbsolutePath(currentDir)
    let customArgs = if args.len == 3: replaceSubs(args[2], currentDir, c.g.config) else: ""

    if not semos.fileExists(name):
      buildErr c, info, "cannot find: " & name
    name = name.toRelativePath(curWorkDir)

    c.toBuild.buildTree TupX, info:
      c.toBuild.addStrLit compileType, info
      c.toBuild.addStrLit name, info
      c.toBuild.addStrLit customArgs, info
  of EmitP:
    semEmit c, it
  of AssumeP:
    semAssumeAssert c, it, AssumeS
  of AssertP:
    semAssumeAssert c, it, AssertS
  of KeepOverflowFlagP:
    if not isPragmaBlock:
      buildErr c, it.n.info, "`keepOverflowFlag` pragma must be used in a pragma block"
    else:
      c.dest.add parLeToken(KeepOverflowFlagP, it.n.info)
      c.dest.addParRi()
    skip it.n
  of PluginP:
    c.dest.add parLeToken(PragmasS, it.n.info)
    c.dest.add parLeToken(PluginP, it.n.info)
    inc it.n
    if it.n.kind == StringLit:
      if c.routine.inGeneric == 0 and it.n.litId notin c.pluginBlacklist:
        c.pendingModulePlugins.add it.n.litId
      c.dest.add it.n
      inc it.n
    else:
      buildErr c, it.n.info, "expected `string` but got: " & asNimCode(it.n)
      if it.n.kind != ParRi: skip it.n
    c.dest.addParRi()
  of PragmaP:
    c.dest.add parLeToken(PragmasS, it.n.info)
    c.dest.add parLeToken(PragmaP, it.n.info)
    inc it.n
    let name = takeIdent(it.n)
    if name == StrId(0):
      buildErr c, it.n.info, "expected identifier for pragma"
      takeParRi c, it.n
      while it.n.kind != ParRi:
        takeTree c, it.n
    else:
      var buf = createTokenBuf(16)
      c.dest.add identToken(name, it.n.info)
      takeParRi c, it.n
      # take remaining pragmas:
      while it.n.kind != ParRi:
        buf.addSubtree it.n
        takeTree c, it.n
      buf.addParRi() # extra ParRi to make reading easier
      c.userPragmas[name] = buf
    c.dest.addParRi()
  else:
    buildErr c, it.n.info, "unsupported pragma"
    skip it.n
    while it.n.kind != ParRi: skip it.n

proc semPragmasLine(c: var SemContext; it: var Item) =
  let info = it.n.info
  inc it.n
  while true:
    if it.n.kind == ParLe:
      if it.n.stmtKind in CallKindsS or
          it.n.substructureKind == KvU:
        inc it.n
      semPragmaLine c, it, false
    else:
      break
  skipParRi it.n
  producesVoid c, info, it.typ # in case it was not already produced

proc semInclExcl(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, op
  if op.typ.typeKind == SetT:
    inc op.typ
  else:
    c.buildErr info, "expected set type"
  semExpr c, op
  it.n = op.n
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semInSet(c: var SemContext; it: var Item) =
  let info = it.n.info
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, op
  if op.typ.typeKind == SetT:
    inc op.typ
  else:
    c.buildErr info, "expected set type"
  semExpr c, op
  it.n = op.n
  takeParRi c, it.n
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, it, beforeExpr, expected

proc semCardSet(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, op
  it.n = op.n
  takeParRi c, it.n
  let expected = it.typ
  it.typ = c.types.intType
  commonType c, it, beforeExpr, expected

proc addTupleAccess(buf: var TokenBuf; lvalue: SymId; i: int; info: PackedLineInfo) =
  buf.add parLeToken(TupatX, info)
  buf.add symToken(lvalue, info)
  buf.addIntLit(i, info)
  buf.addParRi()

proc semUnpackDecl(c: var SemContext; it: var Item) =
  case c.phase
  of SemcheckTopLevelSyms:
    c.takeTree it.n
    return
  of SemcheckSignatures:
    var kindTag = it.n
    while kindTag.stmtKind == UnpackdeclS:
      inc kindTag # unpackdecl tag
      skip kindTag # value
      assert kindTag.substructureKind == UnpacktupU
      inc kindTag # unpacktup tag
    let kind = kindTag.symKind
    if kind != ConstY:
      c.takeTree it.n
      return
  of SemcheckBodies: discard

  let info = it.n.info
  inc it.n # skip tag
  var tup = Item(n: it.n, typ: c.types.autoType)
  let tupInfo = tup.n.info
  var tupBuf = createTokenBuf(16)
  swap c.dest, tupBuf
  semExpr c, tup
  swap c.dest, tupBuf
  it.n = tup.n
  let tupleType = skipModifier(tup.typ)
  if tupleType.typeKind != TupleT:
    c.buildErr tupInfo, "expected tuple for tuple unpacking"
    skipToEnd it.n
    return
  if it.n.substructureKind != UnpacktupU:
    error "illformed AST: `unpacktup` inside `unpackdecl` expected, got ", it.n
  inc it.n # skip unpacktup tag
  var kindTag = it.n
  while kindTag.stmtKind == UnpackdeclS:
    # skip nested unpacks as well
    inc kindTag # unpackdecl tag
    skip kindTag # value
    assert kindTag.substructureKind == UnpacktupU
    inc kindTag # unpacktup tag
  let kind = kindTag.symKind
  let tmpName = identToSym(c, "`tmptup", kind)

  # build local for tuple:
  let tmpStart = c.dest.len
  c.dest.buildTree kind, info:
    c.dest.add symdefToken(tmpName, info) # 0: name
    c.dest.addDotToken() # 1: export
    c.dest.addDotToken() # 2: pragma
    c.dest.addSubtree tupleType # 3: type
    c.dest.add tupBuf # 4: value
  publish c, tmpName, tmpStart

  # iterate over unpacktup:
  var declBuf = createTokenBuf(32)
  var i = 0
  while it.n.kind != ParRi:
    let declInfo = it.n.info
    if it.n.stmtKind == UnpackdeclS:
      declBuf.add it.n
      inc it.n
      assert it.n.kind == DotToken # value
      inc it.n
      declBuf.addTupleAccess(tmpName, i, declInfo)
      takeTree declBuf, it.n
      takeParRi declBuf, it.n
    else:
      declBuf.add it.n
      inc it.n
      takeTree declBuf, it.n # 0: name
      takeTree declBuf, it.n # 1: export
      takeTree declBuf, it.n # 2: pragma
      takeTree declBuf, it.n # 3: type
      assert it.n.kind == DotToken # value
      inc it.n
      declBuf.addTupleAccess(tmpName, i, declInfo) # 4: value
      takeParRi declBuf, it.n
    var decl = cursorAt(declBuf, 0)
    semStmt c, decl, false
    endRead(declBuf)
    declBuf.shrink 0
    inc i
  skipParRi it.n # close unpacktup
  skipParRi it.n # close unpackdecl
  producesVoid c, info, it.typ

proc semPragmaExpr(c: var SemContext; it: var Item) =
  let info = it.n.info
  c.takeToken it.n
  assert it.n.stmtKind == PragmasS
  c.takeToken it.n
  while it.n.kind != ParRi:
    semPragmaLine c, it, true
  takeParRi c, it.n
  semStmt(c, it.n, false)
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semInstanceof(c: var SemContext; it: var Item) =
  type
    State = enum
      NoSubtype
      LacksRtti
      MaybeSubtype
      AlwaysSubtype

  let info = it.n.info
  let beforeExpr = c.dest.len
  let expected = it.typ
  c.takeToken(it.n)
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, arg
  it.n = arg.n
  # handle types
  let beforeType = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  var ok = MaybeSubtype
  if c.routine.inGeneric == 0:
    let t = cursorAt(c.dest, beforeType)
    if t.kind == Symbol and arg.typ.kind == Symbol:
      let xtyp = arg.typ.symId
      let targetSym = t.symId
      ok = NoSubtype
      if xtyp == targetSym:
        # XXX report "always true" here
        ok = AlwaysSubtype
      else:
        let targetBase = skipTypeInstSym(targetSym)
        for xsubtype in inheritanceChain(xtyp):
          let subBase = skipTypeInstSym(xsubtype)
          if subBase == targetBase:
            ok = AlwaysSubtype
            break
      if ok == NoSubtype:
        let xBase = skipTypeInstSym(xtyp)
        for subtype in inheritanceChain(targetSym):
          let subBase = skipTypeInstSym(subtype)
          if xBase == subBase:
            ok = MaybeSubtype
            break
        if not hasRtti(xtyp):
          ok = LacksRtti
    c.dest.endRead()
  c.takeParRi(it.n)
  case ok
  of MaybeSubtype, AlwaysSubtype:
    discard
  of NoSubtype, LacksRtti:
    let tstr = asNimCode(cursorAt(c.dest, beforeType))
    c.dest.endRead()
    c.dest.shrink beforeExpr
    if ok == NoSubtype:
      c.buildErr info, "type of " & asNimCode(arg.n) & " is never a subtype of " & tstr
    else:
      c.buildErr info, "base type of " & asNimCode(arg.n) & " is " & tstr & " which lacks RTTI and cannot be used in an `of` check"
  it.typ = c.types.boolType
  commonType c, it, beforeExpr, expected

proc semProccall(c: var SemContext; it: var Item) =
  c.takeToken(it.n)
  semExpr c, it
  c.takeParRi(it.n)

proc semInternalTypeName(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  let typ = semLocalType(c, it.n)
  if containsGenericParams(typ):
    discard
  else:
    let typeName = pool.syms[typ.symId]
    c.dest.shrink beforeExpr
    c.dest.addStrLit typeName, info
  takeParRi c, it.n
  let expected = it.typ
  it.typ = c.types.stringType
  commonType c, it, beforeExpr, expected

proc semIs(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  let orig = it.n
  inc it.n
  var lhs = Item(n: it.n, typ: c.types.autoType)
  semExpr c, lhs
  it.n = lhs.n
  if lhs.typ.typeKind == TypedescT:
    inc lhs.typ
  let rhs = semLocalType(c, it.n)
  skipParRi it.n
  c.dest.shrink beforeExpr # delete LHS and RHS
  if containsGenericParams(lhs.typ) or containsGenericParams(rhs):
    c.dest.add orig
    c.dest.addSubtree lhs.typ
    c.dest.addSubtree rhs
    c.dest.addParRi()
  else:
    var m = createMatch(addr c)
    typematch m, rhs, lhs
    if classifyMatch(m) >= SubtypeMatch:
      c.dest.addParPair(TrueX, info)
    else:
      c.dest.addParPair(FalseX, info)
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, it, beforeExpr, expected

proc semTableConstructor(c: var SemContext; it: var Item; flags: set[SemFlag]) =
  let info = it.n.info
  inc it.n
  var arrayBuf = createTokenBuf(16)
  arrayBuf.buildTree BracketX, info:
    while it.n.kind != ParRi:
      assert it.n.substructureKind == KvU
      let kvInfo = it.n.info
      inc it.n
      arrayBuf.buildTree TupX, kvInfo:
        arrayBuf.takeTree it.n
        assert it.n.kind != ParRi
        arrayBuf.takeTree it.n
      inc it.n

  var item = Item(n: beginRead(arrayBuf), typ: it.typ)
  semBracket c, item, flags
  it.typ = item.typ
  inc it.n

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
    let s = semIdentImpl(c, it.n, it.n.litId, flags)
    semExprSym c, it, s, start, flags
    inc it.n
  of Symbol:
    let start = c.dest.len
    let s = fetchSym(c, it.n.symId)
    takeToken c, it.n
    semExprSym c, it, s, start, flags
  of ParLe:
    case exprKind(it.n)
    of QuotedX:
      let start = c.dest.len
      let s = semQuoted(c, it.n, flags)
      semExprSym c, it, s, start, flags
    of NoExpr:
      case stmtKind(it.n)
      of NoStmt:
        case typeKind(it.n)
        of NoType:
          buildErr c, it.n.info, "expression expected; tag: " & pool.tags[it.n.tag]
          skip it.n
        of ErrT:
          c.takeTree it.n
        of ObjectT, EnumT, HoleyEnumT, DistinctT, ConceptT:
          buildErr c, it.n.info, "expression expected"
          skip it.n
        of IntT, FloatT, CharT, BoolT, UIntT, VoidT, NiltT, AutoT, SymKindT,
            PtrT, RefT, MutT, OutT, LentT, SinkT, UarrayT, SetT, StaticT, TypedescT,
            TupleT, ArrayT, RangetypeT, VarargsT, ProctypeT, IteratorT, UntypedT, TypedT,
            CstringT, PointerT, TypeKindT, OrdinalT, ParamsT, ItertypeT:
          # every valid local type expression
          semLocalTypeExpr c, it
        of OrT, AndT, NotT, InvokeT:
          # should be handled in respective expression kinds
          discard
      of ImportasS, StaticstmtS, BindS, MixinS, UsingS, AsmS, DeferS:
        buildErr c, it.n.info, "unsupported statement: " & $stmtKind(it.n)
        skip it.n
      of ProcS:
        procGuard c:
          semProc c, it, ProcY, whichPass(c)
      of FuncS:
        procGuard c:
          semProc c, it, FuncY, whichPass(c)
      of IteratorS:
        procGuard c:
          semProc c, it, IteratorY, whichPass(c)
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
      of GvarS:
        toplevelGuard c:
          semLocal c, it, GvarY
      of TvarS:
        toplevelGuard c:
          semLocal c, it, TvarY
      of LetS:
        toplevelGuard c:
          semLocal c, it, LetY
      of GletS:
        toplevelGuard c:
          semLocal c, it, GletY
      of TletS:
        toplevelGuard c:
          semLocal c, it, TletY
      of CursorS:
        toplevelGuard c:
          semLocal c, it, CursorY
      of ResultS:
        toplevelGuard c:
          semLocal c, it, ResultY
      of ConstS:
        constGuard c:
          semLocal c, it, ConstY
      of UnpackdeclS:
        semUnpackDecl c, it
      of StmtsS: semStmtsExpr c, it, false
      of ScopeS: semStmtsExpr c, it, true
      of BreakS:
        toplevelGuard c:
          semBreak c, it
      of ContinueS:
        toplevelGuard c:
          semContinue c, it
      of CallKindsS:
        toplevelGuard c:
          semCall c, it, flags
      of IncludeS: semInclude c, it
      of ImportS: semImport c, it
      of ImportExceptS: semImportExcept c, it
      of FromimportS: semFromImport c, it
      of ExportS: semExport c, it
      of ExportExceptS: semExportExcept c, it
      of AsgnS:
        toplevelGuard c:
          semAsgn c, it
      of DiscardS:
        toplevelGuard c:
          semDiscard c, it
      of IfS:
        toplevelGuard c:
          semIf c, it
      of WhenS:
        semWhen c, it
      of RetS:
        toplevelGuard c:
          semReturn c, it
      of YldS:
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
      of TryS:
        toplevelGuard c:
          semTry c, it
      of RaiseS:
        toplevelGuard c:
          semRaise c, it
      of CommentS:
        # XXX ignored for now
        skip it.n
      of EmitS:
        pragmaGuard c:
          semEmit c, it
      of PragmasS:
        pragmaGuard c:
          semPragmasLine c, it
      of InclS, ExclS:
        toplevelGuard c:
          semInclExcl c, it
      of AssumeS, AssertS:
        pragmaGuard c:
          semAssumeAssert c, it, it.n.stmtKind
    of FalseX, TrueX, OvfX:
      literalB c, it, c.types.boolType
    of InfX, NegInfX, NanX:
      literalB c, it, c.types.floatType
    of AndX, OrX, XorX:
      let start = c.dest.len
      takeToken c, it.n
      semBoolExpr c, it.n
      semBoolExpr c, it.n
      takeParRi c, it.n
      let expected = it.typ
      it.typ = c.types.boolType
      commonType c, it, start, expected
    of NotX:
      let start = c.dest.len
      takeToken c, it.n
      semBoolExpr c, it.n
      takeParRi c, it.n
      let expected = it.typ
      it.typ = c.types.boolType
      commonType c, it, start, expected
    of EmoveX:
      takeToken c, it.n
      semExpr c, it
      takeParRi c, it.n
    of FailedX:
      semFailed c, it
    of ParX:
      inc it.n
      semExpr c, it
      skipParRi it.n
    of CallX, CmdX, CallStrLitX, InfixX, PrefixX, HcallX:
      toplevelGuard c:
        semCall c, it, flags
    of ProccallX:
      toplevelGuard c:
        semProccall c, it
    of DotX, DdotX:
      toplevelGuard c:
        semDot c, it, flags
    of TupatX:
      toplevelGuard c:
        semTupAt c, it
    of DconvX:
      toplevelGuard c:
        semDconv c, it
    of EqX, NeqX, LeX, LtX, EqSetX, LeSetX, LtSetX:
      semCmp c, it
    of AddX, SubX, MulX, DivX, ModX, BitandX, BitorX, BitxorX, PlusSetX, MinusSetX, MulSetX, XorSetX:
      semTypedBinaryArithmetic c, it
    of AshrX, ShrX, ShlX:
      semShift c, it
    of BitnotX, NegX:
      semTypedUnaryArithmetic c, it
    of InSetX:
      semInSet c, it
    of CardX:
      semCardSet c, it
    of BracketX:
      semBracket c, it, flags
    of CurlyX:
      semCurly c, it, flags
    of TupX:
      semTup c, it
    of AconstrX:
      semArrayConstr c, it
    of SetConstrX:
      semSetConstr c, it
    of TupConstrX:
      semTupleConstr c, it
    of SufX:
      semSuf c, it
    of OconstrX, NewobjX:
      semObjConstr c, it
    of NewrefX:
      semNewref c, it
    of DefinedX:
      semDefined c, it
    of DeclaredX:
      semDeclared c, it
    of IsMainModuleX:
      semIsMainModule c, it
    of AtX:
      semSubscript c, it
    of ArrAtX, PatX:
      semTypedAt c, it
    of UnpackX:
      takeToken c, it.n
      takeParRi c, it.n
    of FieldsX, FieldpairsX, InternalFieldPairsX:
      takeTree c, it.n
    of OchoiceX, CchoiceX:
      takeTree c, it.n
    of HaddrX, HderefX:
      takeToken c, it.n
      # this is exactly what we need here as these operators have the same
      # type as the operand:
      semExpr c, it
      takeParRi c, it.n
    of CastX:
      semCast c, it
    of NilX:
      semNil c, it
    of ConvX, HconvX:
      semConv c, it
    of EnumToStrX:
      semEnumToStr c, it
    of DefaultObjX:
      semObjDefault c, it
    of DefaultTupX:
      semTupleDefault c, it
    of LowX:
      semLow c, it
    of HighX:
      semHigh c, it
    of ExprX:
      semStmtsExpr c, it, false
    of DerefX:
      semDeref c, it
    of AddrX:
      semAddr c, it
    of SizeofX:
      semSizeof c, it
    of TypeofX:
      semTypeof c, it
    of DestroyX, CopyX, WasMovedX, SinkhX, TraceX:
      semVoidHook c, it
    of DupX:
      semDupHook c, it
    of ErrX:
      takeTree c, it.n
    of PragmaxX:
      semPragmaExpr c, it
    of InstanceofX:
      semInstanceof c, it
    of BaseobjX:
      semBaseobj c, it
    of InternalTypeNameX:
      semInternalTypeName c, it
    of IsX:
      semIs c, it
    of TabconstrX:
      semTableConstructor c, it, flags
    of CurlyatX, DoX,
       CompilesX, AlignofX, OffsetofX:
      # XXX To implement
      buildErr c, it.n.info, "to implement: " & $exprKind(it.n)
      takeToken c, it.n
      takeParRi c, it.n

  of ParRi, EofToken, SymbolDef, UnknownToken, DotToken:
    buildErr c, it.n.info, "expression expected"

proc reportErrors(c: var SemContext): int =
  result = reporters.reportErrors(c.dest)

proc buildIndexExports(c: var SemContext): TokenBuf =
  if c.exports.len == 0:
    return default(TokenBuf)
  result = createTokenBuf(32)
  for m, ex in c.exports:
    let path = toAbsolutePath(c.importedModules[m].path)
    case ex.kind
    of ImportAll:
      result.addParLe(TagId(ExportIdx), NoLineInfo)
      result.add strToken(pool.strings.getOrIncl(path), NoLineInfo)
      result.addParRi()
    of FromImport:
      if ex.list.len != 0:
        result.addParLe(TagId(FromexportIdx), NoLineInfo)
        result.add strToken(pool.strings.getOrIncl(path), NoLineInfo)
        for s in ex.list:
          result.add identToken(s, NoLineInfo)
        result.addParRi()
    of ImportExcept:
      let kind = if ex.list.len == 0: ExportIdx else: ExportexceptIdx
      result.addParLe(TagId(kind), NoLineInfo)
      result.add strToken(pool.strings.getOrIncl(path), NoLineInfo)
      for s in ex.list:
        result.add identToken(s, NoLineInfo)
      result.addParRi()

proc writeOutput(c: var SemContext; outfile: string) =
  #var b = nifbuilder.open(outfile)
  #b.addHeader "nimony", "nim-sem"
  #b.addRaw toString(c.dest)
  #b.close()
  writeFile outfile, "(.nif24)\n" & toString(c.dest)
  let root = c.dest[0].info
  createIndex outfile, root, true,
    IndexSections(hooks: move c.hookIndexLog,
      converters: move c.converterIndexMap,
      classes: move c.classIndexMap,
      toBuild: move c.toBuild,
      exportBuf: buildIndexExports(c))

  # Update .2.deps.nif file that doesn't contain modules imported under `when false:`
  # so that Hexer and following phases doesn't read such modules.
  var deps = createTokenBuf(16)
  deps.buildTree StmtsS, NoLineInfo:
    if c.importedModules.len != 0:
      deps.buildTree ImportS, NoLineInfo:
        for _, i in c.importedModules:
          deps.addStrLit i.path.toAbsolutePath
  let depsFile = changeFileExt(outfile, ".deps.nif")
  writeFile depsFile, "(.nif24)\n" & toString(deps)

proc phaseX(c: var SemContext; n: Cursor; x: SemPhase): TokenBuf =
  assert n.stmtKind == StmtsS
  c.phase = x
  var n = n
  takeToken c, n
  while n.kind != ParRi:
    semStmt c, n, false
  takeParRi c, n
  result = move c.dest

proc requestHookInstance(c: var SemContext; decl: Cursor) =
  let decl = asTypeDecl(decl)
  var typevars = decl.typevars
  assert classifyType(c, typevars) == InvokeT
  inc typevars
  assert typevars.kind == Symbol

  let symId = typevars.symId

  let hooks = tryLoadAllHooks(symId)
  var needsSomething = false
  for op in low(AttachedOp)..high(AttachedOp):
    let h = hooks.a[op]
    if h[0] != NoSymId and h[1]:
      needsSomething = true
      break
  if not needsSomething: return

  var inferred = initTable[SymId, Cursor]()
  var typeArgs = createTokenBuf()

  inc typevars # skips symbol

  var typevarsSeq: seq[Cursor] = @[]

  while typevars.kind != ParRi:
    typevarsSeq.add typevars
    takeTree(typeArgs, typevars)

  for op in low(AttachedOp)..high(AttachedOp):
    let h = hooks.a[op]
    let hook = h[0]
    if hook != NoSymId and h[1]:
      let res = tryLoadSym(hook)
      if res.status == LacksNothing:
        let info = res.decl.info
        let procDecl = asRoutine(res.decl)
        var typevarsStart = procDecl.typevars
        inc typevarsStart # skips typevars tag

        var counter = 0
        while typevarsStart.kind != ParRi:
          let name = asTypevar(typevarsStart).name.symId
          inferred[name] = typevarsSeq[counter]
          skip typevarsStart # skip the typevar tree
          inc counter
        discard requestRoutineInstance(c, hook, typeArgs, inferred, info)
      else:
        quit "BUG: Could not load hook: " & pool.syms[hook]

proc instantiateMethodForType(c: var SemContext; methodSym, typeInstSym: SymId) =
  # check if instance actually matches method
  let res = tryLoadSym(methodSym)
  assert res.status == LacksNothing
  let procDecl = asRoutine(res.decl)
  var firstParam = procDecl.params
  inc firstParam
  firstParam = skipModifier(asLocal(firstParam).typ)
  if firstParam.typeKind in {RefT, PtrT}:
    # instance is the object type, not a ref/ptr type
    inc firstParam
  var typBuf = createTokenBuf(2)
  typBuf.add symToken(typeInstSym, NoLineInfo)
  var paramMatch = createMatch(addr c)
  typematch paramMatch, firstParam, Item(n: emptyNode(c), typ: beginRead(typBuf))
  if classifyMatch(paramMatch) in {EqualMatch, GenericMatch}:
    # type matched, check that the method can be fully instantiated
    var inferred = ensureMove paramMatch.inferred
    var typevars = procDecl.typevars
    inc typevars
    var typeArgsBuf = createTokenBuf(32)
    while typevars.kind != ParRi:
      let name = takeLocal(typevars, SkipFinalParRi).name.symId
      if name notin inferred:
        c.buildErr res.decl.info, "cannot instantiate method " & pool.syms[methodSym] &
          ", cannot infer generic parameter " & pool.syms[name]
        return
      typeArgsBuf.addSubtree inferred[name]
    discard requestRoutineInstance(c, methodSym, typeArgsBuf, inferred, res.decl.info)
  else:
    # method did not match, fine, consider it unavailable for this instance
    discard

proc requestMethods(c: var SemContext; s: SymId; decl: Cursor) =
  let decl = asTypeDecl(decl)
  var typevars = decl.typevars
  assert classifyType(c, typevars) == InvokeT
  inc typevars
  assert typevars.kind == Symbol

  let base = typevars.symId

  var instanceMethods = c.methods.getOrDefault(s, @[])
  for m in c.methods.getOrDefault(base, @[]):
    if m notin instanceMethods:
      instantiateMethodForType(c, m, s)
      instanceMethods.add m
      c.methods.mgetOrPut(s, @[]).add m

proc addSelfModuleSym(c: var SemContext; path: string) =
  let name = moduleNameFromPath(path)
  let nameId = pool.strings.getOrIncl(name)
  c.selfModuleSym = identToSym(c, nameId, ModuleY)
  let s = Sym(kind: ModuleY, name: c.selfModuleSym, pos: ImportedPos)
  if name != "":
    c.currentScope.addOverloadable(nameId, s)
  var moduleDecl = createTokenBuf(2)
  moduleDecl.addParLe(ModuleY, NoLineInfo)
  moduleDecl.addParRi()
  publish c.selfModuleSym, moduleDecl

proc semcheckCore(c: var SemContext; n0: Cursor) =
  c.pending.add parLeToken(StmtsS, NoLineInfo)
  c.currentScope = Scope(tab: initTable[StrId, seq[Sym]](), up: nil, kind: ToplevelScope)

  assert n0.stmtKind == StmtsS
  let path = getFile(n0.info) # gets current module path, maybe there is a better way
  addSelfModuleSym(c, path)

  if {SkipSystem, IsSystem} * c.moduleFlags == {}:
    let systemFile = ImportedFilename(path: stdlibFile("std/system"), name: "system", isSystem: true)
    importSingleFile(c, systemFile, "", ImportFilter(kind: ImportAll), n0.info)

  #echo "PHASE 1"
  var n1 = phaseX(c, n0, SemcheckTopLevelSyms)
  #echo "PHASE 2: ", toString(n1)
  var n2 = phaseX(c, beginRead(n1), SemcheckSignatures)

  #echo "PHASE 3: ", toString(n2)
  var n = beginRead(n2)
  c.phase = SemcheckBodies
  takeToken c, n
  while n.kind != ParRi:
    semStmt c, n, false

  c.pending.addParRi()
  var cur = beginRead(c.pending)
  inc cur
  c.phase = SemcheckBodies
  while cur.kind != ParRi:
    semStmt c, cur, false
  skipParRi(cur)
  endRead(c.pending)

  instantiateGenerics c
  for val in c.typeInstDecls:
    let s = fetchSym(c, val)
    let res = declToCursor(c, s)
    if res.status == LacksNothing:
      requestHookInstance(c, res.decl)
      requestMethods(c, val, res.decl)
      c.dest.copyTree res.decl
  instantiateGenericHooks c
  takeParRi c, n

  if reportErrors(c) == 0:
    var afterSem = move c.dest
    when true: #defined(enableContracts):
      var moreErrors = analyzeContracts(afterSem)
      if reporters.reportErrors(moreErrors) > 0:
        quit 1
    var finalBuf = beginRead afterSem
    c.dest = injectDerefs(finalBuf)
  else:
    quit 1

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
    canSelfExec: canSelfExec,
    pending: createTokenBuf())

  for magic in ["typeof", "compiles", "defined", "declared"]:
    c.unoverloadableMagics.incl(pool.strings.getOrIncl(magic))

  while true:
    semcheckCore c, n0
    if c.pendingTypePlugins.len == 0: break
    handleTypePlugins c

  if reportErrors(c) == 0:
    writeOutput c, outfile
  else:
    quit 1

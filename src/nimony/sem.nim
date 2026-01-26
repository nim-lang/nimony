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
  semuntyped, contracts, vtables_frontend, module_plugins, deferstmts, pragmacanon, exprexec

import ".." / gear2 / modnames
import ".." / models / [tags, nifindex_tags]

proc semStmt(c: var SemContext; dest: var TokenBuf; n: var Cursor; isNewScope: bool)
proc semStmtBranch(c: var SemContext; dest: var TokenBuf; it: var Item; isNewScope: bool)
proc semConv(c: var SemContext; dest: var TokenBuf; it: var Item)
proc loadSymWithPhase*(c: var SemContext; symId: SymId; targetPhase: SemPhase): LoadResult

proc typeMismatch(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; got, expected: TypeCursor) =
  c.buildErr dest, info, "type mismatch: got: " & typeToString(got) & " but wanted: " & typeToString(expected)

proc typecheck(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; got, expected: TypeCursor) =
  if sameTrees(expected, got):
    discard "fine"
  elif isVoidType(expected) and isVoidType(got):
    discard "fine"
  elif isVoidType(expected) and typeKind(got) == UntypedT:
    # untyped is compatible with void in template contexts
    discard "fine"
  else:
    c.typeMismatch dest, info, got, expected

proc combineType(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; d: var Cursor; src: Cursor) =
  if typeKind(d) == AutoT:
    d = src
  elif sameTrees(d, src):
    discard "fine"
  else:
    c.typeMismatch dest, info, src, d

proc implicitlyDiscardable(n: Cursor, dest: var TokenBuf, noreturnOnly = false): bool =
  template checkBranch(branch) =
    if not implicitlyDiscardable(branch, dest, noreturnOnly):
      return false

  var it = n
  #const
  #  skipForDiscardable = {nkStmtList, nkStmtListExpr,
  #    nkOfBranch, nkElse, nkFinally, nkExceptBranch,
  #    nkElifBranch, nkElifExpr, nkElseExpr, nkBlockStmt, nkBlockExpr,
  #    nkHiddenStdConv, nkHiddenSubConv, nkHiddenDeref}
  while it.kind == ParLe and (stmtKind(it) in {StmtsS, BlockS} or exprKind(it) == ExprX):
    if exprKind(it) == ExprX and dest.len != 0:
      let pos = cursorToPosition(dest, it)
      dest[pos] = parLeToken(StmtsS, dest[pos].info)
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
  var dummy = default(TokenBuf)
  result = implicitlyDiscardable(n, dummy, noreturnOnly = true)

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

proc semExpr(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag] = {})

proc semCall(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag]; source: TransformedCallSource = RegularCall)

proc commonType(c: var SemContext; dest: var TokenBuf; it: var Item; argBegin: int; expected: TypeCursor) =
  if typeKind(expected) == AutoT:
    return

  var arg = Item(n: cursorAt(dest, argBegin), typ: it.typ)
  var done = false
  if typeKind(arg.typ) == VoidT and isNoReturn(arg.n):
    # noreturn allowed in expression context
    # maybe use sem flags to restrict this to statement branches
    done = true
  elif typeKind(arg.typ) == AutoT and not isEmptyContainer(arg.n):
    # auto is valid for empty container, will be handled below
    it.typ = expected
    done = true
  endRead(dest)
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
      shrink dest, argBegin
      dest.add parLeToken(HcallX, info)
      dest.add symToken(convMatch.fn.sym, info)
      if convMatch.genericConverter:
        buildTypeArgs(convMatch)
        if convMatch.err:
          # adding type args errored
          buildErr c, dest, info, getErrorMsg(convMatch)
        else:
          let inst = c.requestRoutineInstance(convMatch.fn.sym, convMatch.typeArgs, convMatch.inferred, arg.n.info)
          dest[dest.len-1].setSymId inst.targetSym
      # ignore refineArgType case, probably environment is generic
      dest.add convMatch.args
      dest.addParRi()
      it.typ = expected
    else:
      shrink dest, argBegin
      #buildErr c, dest, info, getErrorMsg(m)
      c.typeMismatch dest, info, it.typ, expected
  else:
    shrink dest, argBegin
    if m.refineArgType and cursorAt(m.args, 0).exprKind in CallKinds:
      # empty seq call, semcheck
      var call = Item(n: beginRead(m.args), typ: c.types.autoType)
      semCall c, dest, call, {}
    else:
      dest.add m.args
    it.typ = expected

proc producesVoid(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; d: var Cursor) =
  if typeKind(d) in {AutoT, VoidT}:
    combineType c, dest, info, d, c.types.voidType
  else:
    c.typeMismatch dest, info, c.types.voidType, d

proc producesNoReturn(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; d: var Cursor) =
  if typeKind(d) in {AutoT, VoidT}:
    combineType c, dest, info, d, c.types.voidType
  else:
    # allowed in expression context
    discard

# ------------------ include/import handling ------------------------

include semimport

# -------------------- declare `result` -------------------------

proc classifyType(c: var SemContext; n: Cursor): TypeKind =
  result = typeKind(n)

proc declareResult(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo): SymId =
  if c.routine.kind in {ProcY, FuncY, ConverterY, MethodY, MacroY} and
      classifyType(c, c.routine.returnType) != VoidT:
    let name = pool.strings.getOrIncl("result")
    result = identToSym(c, name, ResultY)
    let s = Sym(kind: ResultY, name: result,
                pos: dest.len)
    discard c.currentScope.addNonOverloadable(name, s)
    c.routine.resId = result

    let declStart = dest.len
    buildTree dest, ResultS, info:
      dest.add symdefToken(result, info) # name
      dest.addDotToken() # export marker
      if NoinitP in c.routine.pragmas:
        dest.add parLeToken(PragmasU, info)
        dest.add parLeToken(NoinitP, info)
        dest.addParRi()
        dest.addParRi()
      else:
        dest.addDotToken() # pragmas
      dest.copyTree(c.routine.returnType) # type
      dest.addDotToken() # value
    publish c, dest, result, declStart
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

proc semLocalTypeImpl(c: var SemContext; dest: var TokenBuf; n: var Cursor; context: TypeDeclContext)

proc semLocalType(c: var SemContext; dest: var TokenBuf; n: var Cursor; context = InLocalDecl): TypeCursor =
  let insertPos = dest.len
  semLocalTypeImpl c, dest, n, context
  assert dest.len > insertPos
  result = typeToCursor(c, dest, insertPos)

proc semTypeSection(c: var SemContext; dest: var TokenBuf; n: var Cursor)

proc instantiateType(c: var SemContext; typ: Cursor; bindings: Table[SymId, Cursor]): Cursor =
  var destB = createTokenBuf(30)
  var sc = SubsContext(params: addr bindings)
  subs(c, destB, sc, typ)
  var sub = beginRead(destB)
  var instDest = createTokenBuf(30)
  result = semLocalType(c, instDest, sub)

type
  PassKind = enum checkSignatures, checkBody, checkGenericInst, checkConceptProc

proc semProc(c: var SemContext; dest: var TokenBuf; it: var Item; kind: SymKind; pass: PassKind)
proc instantiateGenericProc(c: var SemContext; dest: var TokenBuf; req: InstRequest) =
  var destB = createTokenBuf(40)
  withFromInfo req:
    subsGenericProc c, destB, req
    var it = Item(n: beginRead(destB), typ: c.types.autoType)
    #echo "now in generic proc: ", toString(it.n)
    semProc c, dest, it, it.n.symKind, checkGenericInst

proc instantiateGenerics(c: var SemContext; dest: var TokenBuf) =
  while c.procRequests.len > 0:
    # This way with `move` ensures it is safe even though
    # the semchecking of generics can add to `c.procRequests`.
    # This is subtle!
    let procReqs = move(c.procRequests)
    for p in procReqs: instantiateGenericProc c, dest, p

proc instantiateGenericHooks(c: var SemContext; dest: var TokenBuf) =
  ## hooks are instantiated after all generics have been instantiated
  while c.procRequests.len > 0:
    let procReqs = move(c.procRequests)
    for p in procReqs: instantiateGenericProc c, dest, p

# -------------------- sem checking -----------------------------

proc instantiateExprIntoBuf(c: var SemContext; buf: var TokenBuf; it: var Item; bindings: Table[SymId, Cursor]) =
  var dest = createTokenBuf(30)
  var sc = SubsContext(params: addr bindings)
  subs(c, dest, sc, it.n)
  var sub = beginRead(dest)
  let start = buf.len
  it.n = sub
  semExpr(c, buf, it)
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

proc semBoolExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  let start = dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, dest, it
  let t = skipModifier(it.typ)
  if classifyType(c, t) != BoolT:
    combineErr c, dest, start, n.info, "expected `bool` but got: " & typeToString(t)
  n = it.n

proc semConstBoolExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor; allowUnresolved = false) =
  let start = dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, dest, it
  n = it.n
  let t = skipModifier(it.typ)
  if classifyType(c, t) != BoolT:
    dest.shrink start
    buildErr c, dest, it.n.info, "expected `bool` but got: " & typeToString(t)
  var e = cursorAt(dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstBoolValue(value):
    if allowUnresolved:
      discard
    elif value.kind == ParLe and value.tagId == ErrT:
      dest.shrink start
      dest.add valueBuf
    else:
      dest.shrink start
      buildErr c, dest, it.n.info, "expected constant bool value but got: " & asNimCode(value)
  else:
    dest.shrink start
    dest.add valueBuf

proc semConstStrExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  let start = dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, dest, it
  n = it.n
  let t = skipModifier(it.typ)
  if not isStringType(t):
    dest.shrink start
    buildErr c, dest, it.n.info, "expected `string` but got: " & typeToString(t)
  var e = cursorAt(dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstStringValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      dest.add valueBuf
    else:
      buildErr c, dest, it.n.info, "expected constant string value but got: " & asNimCode(value)
  else:
    dest.shrink start
    dest.add valueBuf

proc semConstStrExprIgnoreTopLevel(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  case c.phase
  of SemcheckTopLevelSyms:
    # XXX `const`s etc are not evaluated yet
    dest.takeTree n
  of SemcheckSignaturesInProgress, SemcheckSignatures,
     SemcheckBodiesInProgress, SemcheckBodies:
    semConstStrExpr(c, dest, n)

proc semConstIntExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  let start = dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, dest, it
  n = it.n
  let t = skipModifier(it.typ)
  if classifyType(c, t) != IntT:
    dest.shrink start
    buildErr c, dest, it.n.info, "expected `int` but got: " & typeToString(t)
  var e = cursorAt(dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstIntValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      dest.add valueBuf
    else:
      dest.shrink start
      buildErr c, dest, it.n.info, "expected constant integer value but got: " & asNimCode(value)
  else:
    dest.shrink start
    dest.add valueBuf

proc semConstExpr(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let start = dest.len
  var phase = SemcheckBodies
  swap c.phase, phase
  semExpr c, dest, it
  swap c.phase, phase
  # XXX future note: consider when the expression depends on a generic param
  var e = cursorAt(dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(dest)
  dest.shrink start
  var value = beginRead(valueBuf)
  annotateConstantType dest, it.typ, value

proc semStmtsExpr(c: var SemContext; dest: var TokenBuf; it: var Item; isNewScope: bool) =
  let before = dest.len
  takeToken dest, it.n
  while it.n.kind != ParRi:
    if not isLastSon(it.n):
      semStmt c, dest, it.n, false
    else:
      semExpr c, dest, it
  takeParRi dest, it.n
  let kind =
    if classifyType(c, it.typ) in {VoidT, AutoT}:
      (if isNewScope: ScopeTagId else: StmtsTagId)
    else: ExprTagId
  dest[before] = parLeToken(TagId(kind), dest[before].info)

proc semStmt(c: var SemContext; dest: var TokenBuf; n: var Cursor; isNewScope: bool) =
  let info = n.info
  var it = Item(n: n, typ: c.types.autoType)
  let exPos = dest.len
  semExpr c, dest, it
  if it.typ.kind != Symbol and
      classifyType(c, it.typ) in {NoType, VoidT, AutoT, UntypedT}:
    discard "ok"
  else:
    # analyze the expression that was just produced:
    let ex = cursorAt(dest, exPos)
    let discardable = implicitlyDiscardable(ex, dest)
    endRead(dest)
    if not discardable:
      buildErr c, dest, info, "expression of type `" & typeToString(it.typ) & "` must be discarded"
  n = it.n

proc semStmtCallback(c: var SemContext; dest: var TokenBuf; n: Cursor) =
  var n = n
  let oldPhase = c.phase
  c.phase = SemcheckBodies
  semStmt c, dest, n, false
  c.phase = oldPhase


proc semGetSize(c: var SemContext; n: Cursor; strict=false): xint =
  getSize(n, c.g.config.bits div 8, strict)

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
    skipToReturnType n
    result = ProcInstance(targetSym: targetSym, procType: res.decl,
      returnType: n)
  assert result.returnType.kind != UnknownToken

type
  DotExprState = enum
    MatchedDotField ## matched a dot field, i.e. result is a dot expression
    MatchedDotSym ## matched a qualified identifier
    FailedDot
    InvalidDot

proc tryBuiltinDot(c: var SemContext; dest: var TokenBuf; it: var Item; lhs: Item; fieldName: StrId; info: PackedLineInfo; flags: set[SemFlag]): DotExprState

proc tryBuiltinSubscript(c: var SemContext; dest: var TokenBuf; it: var Item; lhs: Item): bool
proc semBuiltinSubscript(c: var SemContext; dest: var TokenBuf; it: var Item; lhs: Item)

proc semBaseobj(c: var SemContext; dest: var TokenBuf; it: var Item) =
  inc it.n # baseobj
  let beforeExpr = dest.len
  let destType = semLocalType(c, dest, it.n)
  if it.n.kind == IntLit:
    # we will recompute it via `typematch` below:
    inc it.n
  else:
    bug("expected integer literal in `baseobj` construct")
  var m = createMatch(addr c)

  var arg = Item(n: it.n, typ: c.types.autoType)
  var argBuf = createTokenBuf(16)
  semExpr c, argBuf, arg
  it.n = arg.n
  arg.n = cursorAt(argBuf, 0)

  typematch m, destType, arg
  if not m.err and m.args[0].tagEnum == BaseobjTagId:
    dest.shrink beforeExpr
    dest.add m.args
  else:
    c.typeMismatch dest, it.n.info, it.typ, destType
  skipParRi it.n
  commonType c, dest, it, beforeExpr, destType

proc addMaybeBaseobjConv(c: var SemContext; dest: var TokenBuf; m: var Match; beforeExpr: int) =
  if m.args[0].tagEnum == BaseobjTagId:
    dest.shrink beforeExpr
    dest.add m.args
    # remove the ')' as the caller will add one for us!
    dest.shrink dest.len - 1
  else:
    dest.add m.args

proc isStringLiteral(n: Cursor): bool =
  # supposing it's a string type
  result = n.kind == StringLit or n.exprKind == SufX

proc semConvArg(c: var SemContext; dest: var TokenBuf; destType: Cursor; arg: Item; info: PackedLineInfo; beforeExpr: int) =
  const
    IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, EnumT, HoleyEnumT}

  var srcType = skipModifier(arg.typ)

  # Check if arg contains a symchoice that needs resolution for enum types
  if arg.n.exprKind in {OchoiceX, CchoiceX}:
    # Try to resolve the symchoice based on the destination type
    var destSym = destType
    if destSym.kind == Symbol:
      let destSymId = destSym.symId
      let impl = typeImpl(destSymId)
      if impl.typeKind in {EnumT, HoleyEnumT}:
        # Try to match the enum choice
        let matchedSym = tryMatchEnumChoice(arg.n, destSymId)
        if matchedSym != SymId(0):
          # Successfully resolved the overload choice
          dest.add symToken(matchedSym, info)
          return
    # If we couldn't resolve it, fall through to normal error handling

  # distinct type conversion?
  var isDistinct = false
  # also skips to type body for symbols:
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)

  if destBase.typeKind == CstringT and isStringType(srcBase):
    if isStringLiteral(arg.n):
      discard "ok"
      dest.addSubtree arg.n
    else:
      c.buildErr dest, info, "Only string literals can be converted to cstring. Use `toCString` for safe conversion."
  elif (destBase.typeKind in IntegralTypes and srcBase.typeKind in IntegralTypes) or
     (destBase.isSomeStringType and srcBase.isSomeStringType) or
     (destBase.containsGenericParams or srcBase.containsGenericParams):
    discard "ok"
    # XXX Add hderef here somehow
    dest.addSubtree arg.n
  elif isDistinct:
    var matchArg = Item(n: arg.n, typ: srcBase)
    var m = createMatch(addr c)
    typematch m, destBase, matchArg
    if m.err:
      c.typeMismatch dest, info, arg.typ, destType
    else:
      # distinct type conversions can also involve conversions
      # between different integer sizes or object types and then
      # `m.args` contains these so use them here:
      dest.add m.args
  else:
    # maybe object types with an inheritance relation?
    var matchArg = arg
    var m = createMatch(addr c)
    typematch m, destType, matchArg
    if not m.err:
      addMaybeBaseobjConv(c, dest, m, beforeExpr)
    else:
      # also try the other direction:
      var m = createMatch(addr c)
      m.flipped = true
      matchArg.typ = destType
      typematch m, srcType, matchArg
      if not m.err:
        addMaybeBaseobjConv(c, dest, m, beforeExpr)
      else:
        c.typeMismatch dest, info, arg.typ, destType

proc isCastableType(t: TypeCursor): bool =
  const IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, PointerT, CstringT, RefT, PtrT, NiltT, EnumT, HoleyEnumT}
  result = t.typeKind in IntegralTypes or isEnumType(t)

proc semCast(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  takeToken dest, it.n
  let destType = semLocalType(c, dest, it.n)
  var x = Item(n: it.n, typ: c.types.autoType)
  # XXX Add hderef here somehow
  semExpr c, dest, x
  it.n = x.n
  takeParRi dest, it.n

  var srcType = skipModifier(x.typ)

  # distinct type conversion?
  var isDistinct = false
  # also skips to type body for symbols:
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)
  if sameTrees(destBase, srcBase):
    commonType c, dest, it, beforeExpr, destType
  elif destBase.isCastableType and srcBase.isCastableType:
    commonType c, dest, it, beforeExpr, destType
  elif containsGenericParams(srcType) or containsGenericParams(destType):
    commonType c, dest, it, beforeExpr, destType
  else:
    dest.shrink beforeExpr
    c.buildErr dest, info, "cannot `cast` between types " & typeToString(srcType) & " and " & typeToString(destType)

proc semLocalTypeExpr(c: var SemContext; dest: var TokenBuf; it: var Item)

proc semReturnType(c: var SemContext; dest: var TokenBuf; n: var Cursor): TypeCursor =
  result = semLocalType(c, dest, n, InReturnTypeDecl)

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

proc semQualifiedIdent(c: var SemContext; dest: var TokenBuf; module: SymId; ident: StrId; info: PackedLineInfo): Sym =
  # mirrors semIdent
  let insertPos = dest.len
  let count =
    if module == c.selfModuleSym:
      buildSymChoiceForSelfModule(c, dest, ident, info)
    else:
      buildSymChoiceForForeignModule(c, dest, module, ident, info)
  if count == 1:
    let sym = dest[insertPos+1].symId
    dest.shrink insertPos
    dest.add symToken(sym, info)
    result = fetchSym(c, sym)
  else:
    result = Sym(kind: if count == 0: NoSym else: CchoiceY)

proc semExprSym(c: var SemContext; dest: var TokenBuf; it: var Item; s: Sym; start: int; flags: set[SemFlag])

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

proc tryBuiltinDot(c: var SemContext; dest: var TokenBuf; it: var Item; lhs: Item; fieldName: StrId;
                   info: PackedLineInfo; flags: set[SemFlag]): DotExprState =
  let exprStart = dest.len
  let expected = it.typ
  dest.addParLe(DotX, info)
  dest.addSubtree lhs.n
  result = FailedDot
  if fieldName == StrId(0):
    # fatal error
    c.buildErr dest, info, "identifier after `.` expected"
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
              dest[exprStart] = parLeToken(DdotX, info)
            dest.add symToken(field.sym, info)
            dest.add intToken(pool.integers.getOrIncl(field.level), info)
            it.typ = field.typ # will be fit later with commonType
            it.kind = FldY
          else:
            dest.add identToken(fieldName, info)
        else:
          # typevars reach here, maybe return untyped state
          # though probably better to fail if this is a call, i.e. `x.int`
          dest.add identToken(fieldName, info)
      else:
        dest.add identToken(fieldName, info)
    elif lhs.kind == ModuleY:
      # this is a qualified identifier, i.e. module.name
      # consider matched even if undeclared
      result = MatchedDotSym
      dest.shrink exprStart
      let module = findModuleSymbol(lhs.n)
      let s = semQualifiedIdent(c, dest, module, fieldName, info)
      semExprSym c, dest, it, s, exprStart, flags
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
            dest[exprStart] = parLeToken(TupatX, info)
            dest.addIntLit(i, info)
            it.typ = field.typ # will be fit later with commonType
            result = MatchedDotField
            break
        skip tup
        inc i
      if result != MatchedDotField:
        dest.add identToken(fieldName, info)
        dest.add intToken(pool.integers.getOrIncl(0), info)
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
          dest.shrink exprStart
          dest.add symToken(field, info)
          let s = Sym(kind: EfldY, name: field, pos: ImportedPos) # placeholder pos
          semExprSym c, dest, it, s, exprStart, flags
          return
        else:
          dest.add identToken(fieldName, info)
      else:
        dest.add identToken(fieldName, info)
    else:
      dest.add identToken(fieldName, info)
  dest.addParRi()
  if result == MatchedDotField:
    commonType c, dest, it, exprStart, expected

proc semDot(c: var SemContext; dest: var TokenBuf, it: var Item; flags: set[SemFlag]) =
  let exprStart = dest.len
  let info = it.n.info
  let expected = it.typ
  # read through the dot expression first:
  inc it.n # skip tag
  var lhsBuf = createTokenBuf(4)
  var lhs = Item(n: it.n, typ: c.types.autoType)
  semExpr c, lhsBuf, lhs, {AllowModuleSym}
  it.n = lhs.n
  lhs.n = cursorAt(lhsBuf, 0)
  let fieldNameCursor = it.n
  let fieldName = takeIdent(it.n)
  # skip optional inheritance depth:
  if it.n.kind == IntLit:
    inc it.n
  skipParRi it.n
  # now interpret the dot expression:
  let state = tryBuiltinDot(c, dest, it, lhs, fieldName, info, flags)
  if state == FailedDot:
    # attempt a dot call, i.e. build b(a) from a.b
    dest.shrink exprStart
    var callBuf = createTokenBuf(16)
    callBuf.addParLe(CallX, info)
    callBuf.addSubtree fieldNameCursor
    callBuf.addSubtree lhs.n # add lhs as first argument
    callBuf.addParRi()
    var call = Item(n: cursorAt(callBuf, 0), typ: expected)
    semCall c, dest, call, flags, DotCall
    it.typ = call.typ

proc semWhile(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  semBoolExpr c, dest, it.n
  inc c.routine.inLoop
  withNewScope c:
    semStmt c, dest, it.n, true
  dec c.routine.inLoop
  takeParRi dest, it.n
  producesVoid c, dest, info, it.typ

proc semBlock(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n

  inc c.routine.inBlock
  withNewScope c:
    if it.n.kind == DotToken:
      takeToken dest, it.n
    else:
      let declStart = dest.len
      let delayed = handleSymDef(c, dest, it.n, BlockY)
      c.addSym dest, delayed
      publish c, dest, delayed.s.name, declStart

    semStmtBranch c, dest, it, true
  dec c.routine.inBlock

  takeParRi dest, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, dest, info, it.typ

proc semBreak(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  if c.routine.inLoop+c.routine.inBlock == 0:
    buildErr c, dest, info, "`break` only possible within a `while` or `block` statement"
    skip it.n
  else:
    if it.n.kind == DotToken:
      wantDot c, dest, it.n
    else:
      let labelInfo = it.n.info
      var a = Item(n: it.n, typ: c.types.autoType)
      semExpr(c, dest, a)
      if a.kind != BlockY:
        buildErr c, dest, labelInfo, "`break` needs a block label"
      it.n = a.n
  takeParRi dest, it.n
  producesNoReturn c, dest, info, it.typ

proc semContinue(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  if c.routine.inLoop == 0:
    buildErr c, dest, info, "`continue` only possible within a loop"
  else:
    wantDot c, dest, it.n
  takeParRi dest, it.n
  producesNoReturn c, dest, info, it.typ

proc wantExportMarker(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  if n.kind == DotToken:
    dest.add n
    inc n
  elif n.kind == Ident and pool.strings[n.litId] == "x":
    if c.currentScope.kind != ToplevelScope:
      buildErr c, dest, n.info, "only toplevel declarations can be exported"
    else:
      dest.add n
    inc n
  elif n.kind == ParLe:
    # export marker could have been turned into a NIF tag
    takeTree dest, n
  else:
    buildErr c, dest, n.info, "expected '.' or 'x' for an export marker"

proc insertType(c: var SemContext; dest: var TokenBuf; typ: TypeCursor; patchPosition: int) =
  let t = skipModifier(typ)
  dest.insert t, patchPosition

proc patchType(c: var SemContext; dest: var TokenBuf; typ: TypeCursor; patchPosition: int) =
  let t = skipModifier(typ)
  dest.replace t, patchPosition

proc semProposition(c: var SemContext; dest: var TokenBuf; n: var Cursor; kind: PragmaKind) =
  let prevPhase = c.phase
  if prevPhase != SemcheckBodies:
    takeTree dest, n
  else:
    c.phase = SemcheckBodies
    withNewScope c:
      if kind == EnsuresP:
        dest.add parLeToken(ExprX, n.info)
        discard declareResult(c, dest, n.info)
      #let start = dest.len
      semBoolExpr c, dest, n
      if kind == EnsuresP:
        dest.addParRi()
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

proc semPragma(c: var SemContext; dest: var TokenBuf; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  let hasParRi = n.kind == ParLe # if false, has no arguments
  if n.substructureKind == KvU:
    inc n
  let pk = pragmaKind(n)
  case pk
  of NoPragma:
    if kind.isRoutine and (let cc = callConvKind(n); cc != NoCallConv):
      dest.addParLe(cc, n.info)
      inc n
      dest.addParRi()
    else:
      let name = getIdent(n)
      if name != StrId(0) and name in c.userPragmas and not hasParRi:
        # custom pragma, cannot have arguments
        inc n
        var read = beginRead(c.userPragmas[name])
        while read.kind != ParRi:
          semPragma c, dest, read, crucial, kind
        endRead(c.userPragmas[name])
      else:
        buildErr c, dest, n.info, "expected pragma"
        inc n
        if hasParRi:
          while n.kind != ParRi: skip n # skip optional pragma arguments
  of MagicP:
    dest.add parLeToken(MagicP, n.info)
    inc n
    if hasParRi and n.kind in {StringLit, Ident}:
      let (magicWord, bits) = magicToTag(pool.strings[n.litId])
      if magicWord == "":
        buildErr c, dest, n.info, "unknown `magic`"
      else:
        crucial.magic = magicWord
        crucial.bits = bits
      takeToken dest, n
    elif n.exprKind == ErrX:
      dest.addSubtree n
    else:
      buildErr c, dest, n.info, "`magic` pragma takes a string literal"
    dest.addParRi()
  of ErrorP, ReportP, DeprecatedP:
    crucial.flags.incl pk
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      semConstStrExprIgnoreTopLevel c, dest, n
    dest.addParRi()
  of ImportcP, ImportcppP, ExportcP, HeaderP, DynlibP, PluginP:
    crucial.flags.incl pk
    let info = n.info
    dest.add parLeToken(pk, info)
    inc n
    let strPos = dest.len
    if hasParRi and n.kind != ParRi:
      semConstStrExprIgnoreTopLevel c, dest, n
    elif crucial.sym != SymId(0):
      var name = pool.syms[crucial.sym]
      extractBasename name
      dest.add strToken(pool.strings.getOrIncl(name), info)
    else:
      c.buildErr dest, info, "invalid import/export symbol"
      dest.addParRi()
      return
    if pk in {ImportcP, ImportcppP, ExportcP} and dest[strPos].kind == StringLit:
      crucial.externName = pool.strings[dest[strPos].litId]
    # Header pragma extra
    if pk == HeaderP:
      let idx = dest.len - 1
      let tok = dest[idx]
      if tok.kind == StringLit:
        var name = replaceSubs(pool.strings[tok.litId], info.getFile(), c.g.config)
        name = name.toRelativePath(c.g.config.nifcachePath)
        dest[idx] = strToken(pool.strings.getOrIncl(name), tok.info)
      crucial.headerFileTok = dest[idx]
    # Finalize expression
    dest.addParRi()
  of AlignP, BitsP:
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      semConstIntExpr(c, dest, n)
    else:
      buildErr c, dest, n.info, "expected int literal"
    dest.addParRi()
  of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoreturnP, BorrowP,
     NoSideEffectP, NodestroyP, BycopyP, ByrefP, InlineP, NoinlineP, NoinitP,
     InjectP, GensymP, UntypedP, SideEffectP, BaseP, ClosureP, PassiveP, IncompleteStructP:
    crucial.flags.incl pk
    dest.add parLeToken(pk, n.info)
    dest.addParRi()
    inc n
  of ViewP, InheritableP, PureP, FinalP, PackedP, UnionP:
    var hasErr = false
    if kind != TypeY:
      buildErr c, dest, n.info, $pk & " pragma is only allowed on types"
      hasErr = true
    elif pk in {ViewP, InheritableP, FinalP, PackedP, UnionP}:
      var n2 = n
      skipToEnd n2
      if n2.typeKind in {RefT, PtrT}:
        inc n2
      if n2.typeKind != ObjectT:
        buildErr c, dest, n.info, $pk & " pragma is only allowed on object types", n
        hasErr = true
    if not hasErr:
      dest.add parLeToken(pk, n.info)
      dest.addParRi()
    inc n
  of CursorP:
    if kind in {VarY, LetY, CursorY}:
      dest.add parLeToken(pk, n.info)
      inc n
    else:
      buildErr c, dest, n.info, "pragma only allowed on local variables"
      inc n
    dest.addParRi()
  of VarargsP:
    crucial.hasVarargs = n.info
    dest.add parLeToken(pk, n.info)
    dest.addParRi()
    inc n
  of RequiresP, EnsuresP:
    crucial.flags.incl pk
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      semProposition c, dest, n, pk
    else:
      buildErr c, dest, n.info, "`requires`/`ensures` pragma takes a bool expression"
    dest.addParRi()
  of TagsP:
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      takeTree dest, n
    else:
      buildErr c, dest, n.info, "expected tags/raises list"
    dest.addParRi()
  of RaisesP:
    crucial.flags.incl pk
    let oldLen = dest.len
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind != ParRi:
      var nn = n
      takeTree dest, n
      dest.addParRi()
      if nn.exprKind == BracketX and nn.firstSon.kind == ParRi:
        # `raises: []` means "does not raise":
        crucial.flags.excl pk
        dest.shrink oldLen
    else:
      dest.addParRi()
  of EmitP, BuildP, StringP, AssumeP, AssertP, PragmaP, PushP, PopP, PassLP, PassCP:
    buildErr c, dest, n.info, "pragma not supported"
    inc n
    if hasParRi:
      while n.kind != ParRi: skip n # skip optional pragma arguments
    dest.addParRi()
  of KeepOverflowFlagP:
    dest.add parLeToken(pk, n.info)
    inc n
    dest.addParRi()
  of SemanticsP:
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.kind in {StringLit, Ident}:
      takeToken dest, n
    else:
      buildErr c, dest, n.info, "`semantics` pragma takes a string literal"
    dest.addParRi()
  if hasParRi:
    if n.kind != ParRi:
      if n.exprKind != ErrX:
        buildErr c, dest, n.info, "too many arguments for pragma"
      while n.kind != ParRi: skip n
    skipParRi n

proc semPragmas(c: var SemContext; dest: var TokenBuf; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  if n.kind == DotToken or n.substructureKind == PragmasU:
    let hasPushedPragma = c.pragmaStack.len != 0
    let emptyPragma = n.kind == DotToken

    if emptyPragma and not hasPushedPragma:
      # there is no pragma
      takeToken dest, n
    else:
      var pragmaAdded = false
      var checkedPragmas = default(CheckedPragmas)
      if n.substructureKind == PragmasU:
        takeToken dest, n
        pragmaAdded = true
        while n.kind != ParRi:
          if n.exprKind == ErrX:
            takeTree dest, n
          else:
            if checkedPragmas.isChecked(n, kind):
              skip n
            else:
              semPragma c, dest, n, crucial, kind

      for ns in c.pragmaStack:
        var n2 = ns
        while n2.kind != ParRi:
          if checkedPragmas.isChecked(n2, kind):
            skip n2
          else:
            if not pragmaAdded:
              dest.addParLe PragmasU, n.info
              pragmaAdded = true
            semPragma c, dest, n2, crucial, kind

      if emptyPragma:
        if pragmaAdded:
          inc n
          dest.addParRi
        else:
          takeToken dest, n
      else:
        takeParRi dest, n
  else:
    buildErr c, dest, n.info, "expected '.' or 'pragmas'"

proc semIdentImpl(c: var SemContext; dest: var TokenBuf; n: var Cursor; ident: StrId; flags: set[SemFlag]): Sym =
  let mode =
    if AllowOverloads in flags: FindOverloads
    else: InnerMost
  let insertPos = dest.len
  let info = n.info
  let count = buildSymChoice(c, dest, ident, info, mode)
  if count == 1:
    let sym = dest[insertPos+1].symId
    dest.shrink insertPos
    dest.add symToken(sym, info)
    result = fetchSym(c, sym)
  else:
    result = Sym(kind: if count == 0: NoSym else: CchoiceY)

proc semIdent(c: var SemContext; dest: var TokenBuf; n: var Cursor; flags: set[SemFlag]): Sym =
  result = semIdentImpl(c, dest, n, n.litId, flags)
  inc n

proc semQuoted(c: var SemContext; dest: var TokenBuf; n: var Cursor; flags: set[SemFlag]): Sym =
  let nameId = takeUnquoted(n)
  result = semIdentImpl(c, dest, n, nameId, flags)

proc maybeInlineMagic(c: var SemContext; dest: var TokenBuf; res: LoadResult) =
  if res.status == LacksNothing:
    var n = res.decl
    inc n # skip the symbol kind
    if n.kind == SymbolDef:
      inc n # skip the SymbolDef
      if n.kind == ParLe:
        # ^ export marker position has a `(`? If so, it is a magic!
        let info = dest[dest.len-1].info
        var tag = n.tagId
        if cast[TagEnum](tag) == IsMainModuleTagId:
          if IsMain in c.moduleFlags:
            tag = TagId(TrueTagId)
          else:
            tag = TagId(FalseTagId)
        dest[dest.len-1] = parLeToken(tag, info)
        inc n
        while true:
          dest.add withLineInfo(n.load, info)
          if n.kind == ParRi: break
          inc n

proc exprToType(c: var SemContext; dest: var TokenBuf; exprType: Cursor; start: int; context: TypeDeclContext; info: PackedLineInfo) =
  case exprType.typeKind
  of TypedescT:
    dest.shrink start
    var base = exprType
    inc base
    dest.addSubtree base
  of ErrT, AutoT:
    # propagate error
    discard
  else:
    # otherwise, is a static value
    if context != AllowValues:
      dest.shrink start
      c.buildErr dest, info, "not a type"

proc semTypeExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor; context: TypeDeclContext; info: PackedLineInfo) =
  # expression needs to be fully evaluated, switch to body phase
  var phase = SemcheckBodies
  swap c.phase, phase
  let start = dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, dest, it
  n = it.n
  exprToType c, dest, it.typ, start, context, info
  swap c.phase, phase

proc semTypeSym(c: var SemContext; dest: var TokenBuf; s: Sym; info: PackedLineInfo; start: int; context: TypeDeclContext) =
  if s.kind in {TypeY, TypevarY}:
    let res = tryLoadSym(s.name)
    let beforeMagic = dest.len
    maybeInlineMagic c, dest, res
    let afterMagic = dest.len
    if s.kind == TypevarY:
      # likely was not magic
      # maybe substitution performed here?
      inc c.usedTypevars
    elif beforeMagic != afterMagic:
      c.expanded.addSymUse s.name, info
      # was magic symbol, may be typeclass, otherwise nothing to do
      if context != InInvokeHead:
        let magic = cursorAt(dest, start).typeKind
        endRead(dest)
        # magic types that are just symbols and not in the syntax:
        if magic in {ArrayT, SetT, RangetypeT, EnumT, HoleyEnumT}:
          var typeclassBuf = createTokenBuf(4)
          typeclassBuf.addParLe(TypeKindT, info)
          typeclassBuf.addParLe(magic, info)
          typeclassBuf.addParRi()
          typeclassBuf.addParRi()
          replace(dest, cursorAt(typeclassBuf, 0), start)
    elif res.status == LacksNothing:
      let typ = asTypeDecl(res.decl)
      if isGeneric(typ) or isNominal(typ.body.typeKind):
        # types that should stay as symbols, see sigmatch.matchSymbol
        # but see if it triggers a module plugin:
        let p = extractPragma(typ.pragmas, PluginP)
        if p != default(Cursor) and p.kind == StringLit:
          if p.litId notin c.pluginBlacklist:
            c.pendingTypePlugins[s.name] = (p.litId, p.info)
      else:
        # remove symbol, inline type:
        dest.shrink dest.len-1
        var t = typ.body
        semLocalTypeImpl c, dest, t, context
  else:
    # non type symbol, treat as expression
    # mirror semTypeExpr but just call semExprSym
    var phase = SemcheckBodies
    swap c.phase, phase
    var dummyBuf = createTokenBuf(1)
    dummyBuf.add dotToken(info)
    var it = Item(n: cursorAt(dummyBuf, 0), typ: c.types.autoType)
    semExprSym c, dest, it, s, start, {}
    exprToType c, dest, it.typ, start, context, info
    swap c.phase, phase

proc semParams(c: var SemContext; dest: var TokenBuf; n: var Cursor)
proc semLocal(c: var SemContext; dest: var TokenBuf; n: var Cursor; kind: SymKind)

type WhenMode = enum
  NormalWhen
  ObjectWhen

proc semWhenImpl(c: var SemContext; dest: var TokenBuf; it: var Item; mode: WhenMode)

type CaseMode = enum
  NormalCase
  ObjectCase

proc semCaseImpl(c: var SemContext; dest: var TokenBuf; it: var Item; mode: CaseMode)

proc semExprMissingPhases(c: var SemContext; dest: var TokenBuf; it: var Item; firstPhase: SemPhase) =
  # Only consider "real" phases, not InProgress markers
  const realPhases = [SemcheckTopLevelSyms, SemcheckSignatures, SemcheckBodies]
  if c.phase <= firstPhase:
    var lastBuf = default(TokenBuf)
    var usingBuf = false
    for phase in realPhases:
      if phase >= c.phase:
        break
      var buf = createTokenBuf()
      var phase = phase
      swap c.phase, phase
      var it2 = Item(typ: it.typ)
      if usingBuf:
        it2.n = beginRead(lastBuf)
      else:
        it2.n = it.n
      semExpr c, buf, it2
      if not usingBuf:
        it.n = it2.n
      swap c.phase, phase
      lastBuf = buf
      usingBuf = true
    let lastN = it.n
    if usingBuf:
      it.n = beginRead(lastBuf)
    semExpr c, dest, it
    if usingBuf:
      it.n = lastN
  else:
    semExpr c, dest, it

proc addVarargsParameter(c: var SemContext; dest: var TokenBuf; paramsAt: int; info: PackedLineInfo) =
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
  if dest[paramsAt].kind == DotToken:
    dest[paramsAt] = parLeToken(ParamsU, info)
    varargsParam.add parRiToken(info)
    dest.insert fromBuffer(varargsParam), paramsAt+1
  else:
    var n = cursorAt(dest, paramsAt)
    if n.substructureKind == ParamsU:
      inc n
      while n.kind != ParRi:
        if n.symKind == ParamY:
          inc n
          let lit = takeIdent(n)
          if lit != StrId(0) and pool.strings[lit] == vanon:
            # already added:
            endRead(dest)
            return
          skipToEnd n
        else:
          break
      let insertPos = cursorToPosition(dest, n)
      endRead(dest)
      dest.insert fromBuffer(varargsParam), insertPos

include semtypes

proc semTypeof(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  inc it.n # typeof

  let beforeMode = dest.len
  var modeArg = Item(n: it.n, typ: c.types.autoType)
  skip modeArg.n # second parameter is TypeOfMode
  semConstExpr c, dest, modeArg
  assert dest.len > beforeMode
  let modeTok = dest[beforeMode]
  if modeTok.kind == ParLe and modeTok.tagId == ErrT:
    dest.insert it.n, beforeMode
    skip it.n
    skip it.n
    takeParRi dest, it.n
    return
  assert modeTok.kind == Symbol
  var semFlags: set[SemFlag] = {}
  var modeSym = pool.syms[modeTok.symId]
  modeSym.extractBasename
  case modeSym
  of "typeOfProc":
    discard
  of "typeOfIter":
    semFlags = {PreferIterators}
  else:
    assert false

  semExpr c, dest, it, semFlags
  var t = it.typ
  if t.typeKind == TypedescT: inc t
  dest.shrink beforeExpr
  dest.addParLe(TypedescT, t.info)
  dest.addSubtree t
  dest.addParRi()
  it.typ = typeToCursor(c, dest, beforeExpr)
  #echo "CAME HERE! ", typeToString(t), " ", c.phase
  #writeStackTrace()
  skip it.n # skip mode
  skipParRi it.n

proc addXint(c: var SemContext; dest: var TokenBuf; x: xint; info: PackedLineInfo) =
  var err = false
  let val = asSigned(x, err)
  if not err:
    dest.add intToken(pool.integers.getOrIncl(val), info)
  else:
    let val = asUnsigned(x, err)
    if not err:
      dest.add uintToken(pool.uintegers.getOrIncl(val), info)
    else:
      c.buildErr dest, info, "enum value not a constant expression"

proc evalConstExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor; expected: TypeCursor): TokenBuf =
  let beforeExpr = dest.len
  var x = Item(n: n, typ: expected)
  semExpr c, dest, x
  n = x.n
  var e = cursorAt(dest, beforeExpr)
  result = evalExpr(c, e)
  endRead(dest)

proc evalConstIntExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor; expected: TypeCursor): xint =
  let info = n.info
  var valueBuf = evalConstExpr(c, dest, n, expected)
  let value = beginRead(valueBuf)
  result = getConstOrdinalValue(value)
  if result.isNaN:
    if value.kind == ParLe and value.tagId == ErrT:
      dest.add valueBuf
    else:
      buildErr c, dest, info, "expected constant integer value but got: " & asNimCode(value)

proc semCaseOfValueImpl(c: var SemContext; dest: var TokenBuf; it: var Item; selectorType: TypeCursor;
                    seen: var seq[(xint, xint)])

proc evalConstCaseBranch(c: var SemContext; dest: var TokenBuf; it: var Item; expected: TypeCursor; seen: var seq[(xint, xint)]; info: PackedLineInfo) =
  let info = it.n.info
  var orig = it.n

  var ignored = createTokenBuf()
  var valueBuf = evalConstExpr(c, ignored, it.n, c.types.autoType)

  var value = beginRead(valueBuf)
  case value.exprKind
  of SetConstrX:
    inc value
    skip value
    var dummy = Item(n: value, typ: expected)
    semCaseOfValueImpl(c, dest, dummy, expected, seen)
  else:
    let a = evalConstIntExpr(c, dest, orig, expected)
    if seen.containsOrIncl(a):
      buildErr c, dest, info, "value already handled"

proc evalConstStrExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor; expected: TypeCursor): StrId =
  let info = n.info
  var valueBuf = evalConstExpr(c, dest, n, expected)
  let value = beginRead(valueBuf)
  result = getConstStringValue(value)
  if result == StrId(0):
    if value.kind == ParLe and value.tagId == ErrT:
      dest.add valueBuf
    else:
      buildErr c, dest, info, "expected constant string value but got: " & asNimCode(value)

include semdecls

proc semExprSym(c: var SemContext; dest: var TokenBuf; it: var Item; s: Sym; start: int; flags: set[SemFlag]) =
  it.kind = s.kind
  let expected = it.typ
  if s.kind == NoSym:
    if AllowUndeclared notin flags:
      var orig = createTokenBuf(1)
      orig.add dest[dest.len-1]
      dest.shrink dest.len-1
      let ident = cursorAt(orig, 0)
      if s.name != SymId(0):
        c.buildErr dest, ident.info, "undeclared identifier: " & pool.syms[s.name], ident
      else:
        let s = getIdent(ident)
        if s != StrId(0):
          c.buildErr dest, ident.info, "undeclared identifier: " & pool.strings[s], ident
        else:
          c.buildErr dest, ident.info, "undeclared identifier", ident
    it.typ = c.types.autoType
  elif s.kind == CchoiceY:
    if KeepMagics notin flags and c.routine.kind != TemplateY:
      # Try to disambiguate based on expected type (e.g., enum fields in case branches)
      if typeKind(expected) != AutoT:
        let choice = cursorAt(dest, start)
        let matchedSym = tryMatchEnumChoice(choice, expected.symId)
        endRead(dest)
        if matchedSym != SymId(0):
          let info = dest[start].info
          dest.shrink start
          dest.add symToken(matchedSym, info)
          return
      c.buildErr dest, dest[start].info, "ambiguous identifier"
    it.typ = c.types.autoType
  elif s.kind == BlockY:
    it.typ = c.types.autoType
  elif s.kind in {TypeY, TypevarY}:
    let typeStart = dest.len
    let info = dest[dest.len-1].info
    dest.buildTree TypedescT, info:
      let symStart = dest.len
      dest.add symToken(s.name, info)
      semTypeSym c, dest, s, info, symStart, InLocalDecl
    it.typ = typeToCursor(c, dest, typeStart)
    dest.shrink typeStart
    commonType c, dest, it, start, expected
  else:
    let res = declToCursor(c, dest, s)
    if KeepMagics notin flags:
      let beforeMagic = dest.len
      let info = dest[beforeMagic-1].info
      maybeInlineMagic c, dest, res
      if beforeMagic != dest.len:
        c.expanded.addSymUse s.name, info
    if res.status == LacksNothing:
      var n = res.decl
      if s.kind.isLocal or s.kind == EfldY:
        skipToLocalType n
      elif s.kind.isRoutine:
        #skipToParams n
        discard "proc begin is also its type"
      elif s.kind == ModuleY:
        if AllowModuleSym notin flags:
          c.buildErr dest, dest[start].info, "module symbol '" & pool.syms[s.name] & "' not allowed in this context"
      else:
        assert false, "not implemented"
      it.typ = n
      commonType c, dest, it, start, expected
    else:
      c.buildErr dest, dest[start].info, "could not load symbol: " & pool.syms[s.name] & "; errorCode: " & $res.status
      it.typ = c.types.autoType

proc semLocalTypeExpr(c: var SemContext; dest: var TokenBuf, it: var Item) =
  let info = it.n.info
  var val = semLocalType(c, dest, it.n)
  if val.typeKind == TypedescT:
    inc val
  let start = dest.len
  dest.buildTree TypedescT, info:
    dest.addSubtree val
  it.typ = typeToCursor(c, dest, start)
  dest.shrink start

proc semSubscriptAsgn(c: var SemContext; dest: var TokenBuf; it: var Item; info: PackedLineInfo) =
  # check if LHS is builtin subscript:
  var subscript = Item(n: it.n, typ: c.types.autoType)
  inc subscript.n # tag
  var subscriptLhsBuf = createTokenBuf(4)
  var subscriptLhs = Item(n: subscript.n, typ: c.types.autoType)
  semExpr c, subscriptLhsBuf, subscriptLhs, {KeepMagics}
  let afterSubscriptLhs = subscriptLhs.n
  subscript.n = afterSubscriptLhs
  subscriptLhs.n = cursorAt(subscriptLhsBuf, 0)
  var subscriptBuf = createTokenBuf(8)
  let builtin = tryBuiltinSubscript(c, subscriptBuf, subscript, subscriptLhs)
  if builtin:
    # build regular assignment:
    dest.addParLe(AsgnS, info)
    dest.add subscriptBuf
    removeModifier(subscript.typ) # remove `var` for rhs
    semExpr c, dest, subscript # use the type and position from the subscript
    it.n = subscript.n
    takeParRi dest, it.n
    producesVoid c, dest, info, it.typ
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
    semCall c, dest, call, {}, SubscriptAsgnCall
    it.typ = call.typ

proc semDotAsgn(c: var SemContext; dest: var TokenBuf; it: var Item; info: PackedLineInfo) =
  # check if LHS is builtin subscript:
  let dotInfo = it.n.info
  var dot = Item(n: it.n, typ: c.types.autoType)
  inc dot.n # tag
  var dotLhsBuf = createTokenBuf(4)
  var dotLhs = Item(n: dot.n, typ: c.types.autoType)
  semExpr c, dotLhsBuf, dotLhs, {KeepMagics}
  dot.n = dotLhs.n
  dotLhs.n = cursorAt(dotLhsBuf, 0)
  let fieldName = takeIdent(dot.n)
  # skip optional inheritance depth:
  if dot.n.kind == IntLit:
    inc dot.n
  skipParRi dot.n
  var dotBuf = createTokenBuf(8)
  let builtin = tryBuiltinDot(c, dotBuf, dot, dotLhs, fieldName, dotInfo, {}) != FailedDot
  if builtin:
    # build regular assignment:
    dest.addParLe(AsgnS, info)
    dest.add dotBuf
    removeModifier(dot.typ) # remove `var` for rhs
    semExpr c, dest, dot # use the type and position from the dot expression
    it.n = dot.n
    takeParRi dest, it.n
    producesVoid c, dest, info, it.typ
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
    semCall c, dest, call, {}, DotAsgnCall
    # XXX original compiler also checks if the call fails and tries a dotcall for the LHS
    it.typ = call.typ

proc semAsgn(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  inc it.n
  case it.n.exprKind
  of AtX:
    semSubscriptAsgn c, dest, it, info
  of DotX, DdotX:
    semDotAsgn c, dest, it, info
  else:
    dest.addParLe(AsgnS, info)
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, dest, a # infers type of `left-hand-side`
    removeModifier(a.typ) # remove `var` for rhs
    semExpr c, dest, a # ensures type compatibility with `left-hand-side`
    it.n = a.n
    takeParRi dest, it.n
    producesVoid c, dest, info, it.typ

proc semEmit(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  dest.add parLeToken(EmitS, info)
  inc it.n
  if it.n.exprKind == BracketX:
    inc it.n
    while it.n.kind != ParRi:
      var a = Item(n: it.n, typ: c.types.autoType)
      semExpr c, dest, a
      it.n = a.n
    skipParRi it.n
  else:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, dest, a
    it.n = a.n
  takeParRi dest, it.n
  producesVoid c, dest, info, it.typ

proc semDiscard(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  if it.n.kind == DotToken:
    takeToken dest, it.n
  else:
    let exInfo = it.n.info
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, dest, a
    it.n = a.n
    if classifyType(c, a.typ) == VoidT:
      buildErr c, dest, exInfo, "expression of type `" & typeToString(a.typ) & "` must not be discarded"
  takeParRi dest, it.n
  producesVoid c, dest, info, it.typ

proc semStmtBranch(c: var SemContext; dest: var TokenBuf; it: var Item; isNewScope: bool) =
  # handle statements that could be expressions
  case classifyType(c, it.typ)
  of AutoT:
    semExpr c, dest, it
  of VoidT:
    # performs discard check:
    semStmt c, dest, it.n, isNewScope
  else:
    var ex = Item(n: it.n, typ: it.typ)
    let start = dest.len
    semExpr c, dest, ex
    # this is handled by commonType, since it has to be done deeply:
    #if classifyType(c, ex.typ) == VoidT:
    #  # allow statement in expression context if it is noreturn
    #  let ignore = isNoReturn(cursorAt(dest, start))
    #  endRead(dest)
    #  if not ignore:
    #    typeMismatch(c, it.n.info, ex.typ, it.typ)
    commonType(c, dest, ex, start, it.typ)
    it.n = ex.n

proc semIf(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  if it.n.substructureKind == ElifU:
    while it.n.substructureKind == ElifU:
      takeToken dest, it.n
      semBoolExpr c, dest, it.n
      withNewScope c:
        semStmtBranch c, dest, it, true
      takeParRi dest, it.n
  else:
    buildErr c, dest, it.n.info, "illformed AST: `elif` inside `if` expected"
  if it.n.substructureKind == ElseU:
    takeToken dest, it.n
    withNewScope c:
      semStmtBranch c, dest, it, true
    takeParRi dest, it.n
  takeParRi dest, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, dest, info, it.typ

proc semExceptionType(c: var SemContext; dest: var TokenBuf; it: var Item): TypeCursor =
  let start = dest.len
  result = semLocalType(c, dest, it.n)
  if result.kind != Symbol or pool.syms[result.symId] != ErrorCodeName:
    dest.shrink start
    buildErr c, dest, it.n.info, "type in `except` must be `system.ErrorCode`"

proc semTry(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  withNewScope c:
    semStmtBranch c, dest, it, true
  while it.n.substructureKind == ExceptU:
    openScope(c)
    takeToken dest, it.n
    if it.n.kind == DotToken:
      takeToken dest, it.n
    elif it.n.exprKind in CallKinds and it.n.firstSon.kind == Ident and pool.strings[it.n.firstSon.litId] == "as":
      # `Type as e`:
      inc it.n
      inc it.n
      let start = dest.len
      let etyp = semExceptionType(c, dest, it)
      dest.shrink start
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
      semLocal c, dest, dd, LetY
    elif it.n.stmtKind == LetS:
      # resem the local declaration:
      semLocal(c, dest, it.n, LetY)
    else:
      discard semExceptionType(c, dest, it)
    semStmtBranch c, dest, it, true
    takeParRi dest, it.n
    closeScope(c)
  if it.n.substructureKind == FinU:
    takeToken dest, it.n
    withNewScope c:
      semStmt c, dest, it.n, true
    takeParRi dest, it.n
  takeParRi dest, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, dest, info, it.typ

proc semWhenImpl(c: var SemContext; dest: var TokenBuf; it: var Item; mode: WhenMode) =
  let start = dest.len
  let info = it.n.info
  takeToken dest, it.n
  var leaveUnresolved = false
  if it.n.substructureKind == ElifU:
    while it.n.substructureKind == ElifU:
      takeToken dest, it.n
      let condStart = dest.len
      var phase = SemcheckBodies
      swap c.phase, phase
      semConstBoolExpr c, dest, it.n, allowUnresolved = c.routine.inGeneric > 0
      swap c.phase, phase
      let condValue = cursorAt(dest, condStart).exprKind
      endRead(dest)
      if not leaveUnresolved:
        if condValue == TrueX:
          dest.shrink start
          case mode
          of NormalWhen:
            semExprMissingPhases c, dest, it, SemcheckSignatures
          of ObjectWhen:
            semObjectComponent c, dest, it.n
          skipParRi it.n # finish elif
          skipToEnd it.n
          return
        elif condValue != FalseX:
          # erroring/unresolved condition, leave entire statement as unresolved
          leaveUnresolved = true
      if leaveUnresolved:
        # might have been set above
        var ctx = createUntypedContext(addr c, UntypedGeneric)
        semTemplBody ctx, dest, it.n
      else:
        takeTree dest, it.n
      takeParRi dest, it.n
  else:
    buildErr c, dest, it.n.info, "illformed AST: `elif` inside `if` expected"
  if it.n.substructureKind == ElseU:
    takeToken dest, it.n
    if not leaveUnresolved:
      dest.shrink start
      case mode
      of NormalWhen:
        semExprMissingPhases c, dest, it, SemcheckSignatures
      of ObjectWhen:
        semObjectComponent c, dest, it.n
      skipParRi it.n # finish else
      skipToEnd it.n
      return
    else:
      var ctx = createUntypedContext(addr c, UntypedGeneric)
      semTemplBody ctx, dest, it.n
    takeParRi dest, it.n
  takeParRi dest, it.n
  if not leaveUnresolved:
    # none of the branches evaluated, output nothing
    dest.shrink start
    producesVoid c, dest, info, it.typ
  else:
    it.typ = c.types.untypedType

proc semWhen(c: var SemContext; dest: var TokenBuf; it: var Item) =
  case c.phase
  of SemcheckTopLevelSyms:
    # XXX `const`s etc are not evaluated yet, so we cannot compile the `when` conditions
    # so symbols inside of `when` are not defined until `SemcheckSignatures`
    # effectively this means types defined in `when` cannot be used before they are declared
    # but this was already not possible in original Nim
    dest.takeTree it.n
    return
  of SemcheckSignaturesInProgress, SemcheckSignatures,
     SemcheckBodiesInProgress, SemcheckBodies:
    discard

  inc c.inWhen
  semWhenImpl(c, dest, it, NormalWhen)
  dec c.inWhen

proc semCaseOfValueImpl(c: var SemContext; dest: var TokenBuf; it: var Item; selectorType: TypeCursor;
                    seen: var seq[(xint, xint)]) =
  while it.n.kind != ParRi:
    let info = it.n.info
    if isRangeExpr(it.n):
      inc it.n # call tag
      skip it.n # `..`
      dest.buildTree RangeU, it.n.info:
        let a = evalConstIntExpr(c, dest, it.n, selectorType)
        let b = evalConstIntExpr(c, dest, it.n, selectorType)
        if seen.doesOverlapOrIncl(a, b):
          buildErr c, dest, info, "overlapping values"
      inc it.n # right paren of call
    elif it.n.substructureKind == RangeU:
      takeToken dest, it.n
      let a = evalConstIntExpr(c, dest, it.n, selectorType)
      let b = evalConstIntExpr(c, dest, it.n, selectorType)
      if seen.doesOverlapOrIncl(a, b):
        buildErr c, dest, info, "overlapping values"
      takeParRi dest, it.n
    else:
      if it.n.exprKind == CurlyX:
        var beforeSetType = dest.len
        dest.buildTree SetT, info:
          dest.addSubtree selectorType

        var curlyExpr = Item(n: it.n, typ: typeToCursor(c, dest, beforeSetType))

        shrink(dest, beforeSetType)
        inc it.n

        var beforeExpr = dest.len
        semExpr c, dest, curlyExpr
        shrink(dest, beforeExpr)

        semCaseOfValueImpl(c, dest, it, selectorType, seen)
        skipParRi(it.n)
      else:
        evalConstCaseBranch(c, dest, it, selectorType, seen, info)

proc semCaseOfValue(c: var SemContext; dest: var TokenBuf; it: var Item; selectorType: TypeCursor;
                    seen: var seq[(xint, xint)]) =
  if it.n.substructureKind == RangesU:
    takeToken dest, it.n
    semCaseOfValueImpl(c, dest, it, selectorType, seen)
    takeParRi dest, it.n
  else:
    buildErr c, dest, it.n.info, "`ranges` within `of` expected"
    skip it.n

proc semCaseOfValueString(c: var SemContext; dest: var TokenBuf; it: var Item; selectorType: TypeCursor;
                          seen: var HashSet[StrId]) =
  if it.n.substructureKind == RangesU:
    takeToken dest, it.n
    while it.n.kind != ParRi:
      let info = it.n.info
      let s = evalConstStrExpr(c, dest, it.n, selectorType) # will error if range is given
      if s != StrId(0): # otherwise error
        # use literal id as value:
        if seen.containsOrIncl(s):
          buildErr c, dest, info, "value already handled"
    takeParRi dest, it.n
  else:
    buildErr c, dest, it.n.info, "`ranges` within `of` expected"
    skip it.n

proc checkExhaustiveness(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; selectorType: TypeCursor; seen: seq[(xint, xint)]) =
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
      buildErr c, dest, info, "not all cases are covered; missing: {" & missing & "}"
  else:
    buildErr c, dest, info, "not all cases are covered"

proc semObjectCaseBranch(c: var SemContext; dest: var TokenBuf; it: var Item) =
  if it.n.stmtKind == StmtsS:
    takeToken dest, it.n
    while it.n.kind != ParRi:
      semObjectComponent c, dest, it.n
    takeParRi dest, it.n
  else:
    dest.addParLe(StmtsS, it.n.info)
    semObjectComponent c, dest, it.n
    dest.addParRi()

proc semCaseImpl(c: var SemContext; dest: var TokenBuf; it: var Item; mode: CaseMode) =
  let info = it.n.info
  takeToken dest, it.n
  var selectorType = default(Cursor)
  case mode
  of NormalCase:
    var selector = Item(n: it.n, typ: c.types.autoType)
    semExpr c, dest, selector
    it.n = selector.n
    selectorType = skipModifier(selector.typ)
  of ObjectCase:
    let selectorStart = dest.len
    semLocal(c, dest, it.n, FldY)
    let field = cursorAt(dest, selectorStart)
    let fieldType = asLocal(field).typ
    let fieldTypePos = cursorToPosition(dest, fieldType)
    endRead(dest)
    selectorType = typeToCursor(c, dest, fieldTypePos)
    if not isOrdinalType(selectorType):
      buildErr c, dest, info, "selector must be of an ordinal type"

  let isString = isSomeStringType(selectorType)
  var seen: seq[(xint, xint)] = @[]
  var seenStr = initHashSet[StrId]()
  if it.n.substructureKind == OfU:
    while it.n.substructureKind == OfU:
      takeToken dest, it.n
      if isString:
        semCaseOfValueString c, dest, it, selectorType, seenStr
      else:
        semCaseOfValue c, dest, it, selectorType, seen
      case mode
      of NormalCase:
        withNewScope c:
          semStmtBranch c, dest, it, true
      of ObjectCase:
        semObjectCaseBranch(c, dest, it)
      takeParRi dest, it.n
  else:
    buildErr c, dest, it.n.info, "illformed AST: `of` inside `case` expected"
  if it.n.substructureKind == ElseU:
    takeToken dest, it.n
    case mode
    of NormalCase:
      withNewScope c:
        semStmtBranch c, dest, it, true
    of ObjectCase:
      semObjectCaseBranch(c, dest, it)
    takeParRi dest, it.n
  elif not isString:
    checkExhaustiveness c, dest, it.n.info, selectorType, seen

  takeParRi dest, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, dest, info, it.typ

proc semCase(c: var SemContext; dest: var TokenBuf; it: var Item) =
  semCaseImpl(c, dest, it, NormalCase)

proc semForLoopVar(c: var SemContext; dest: var TokenBuf; it: var Item; loopvarType: TypeCursor; loopvarTypeMod = NoType) =
  if stmtKind(it.n) == LetS:
    let declStart = dest.len
    takeToken dest, it.n
    let delayed = handleSymDef(c, dest, it.n, LetY)
    c.addSym dest, delayed
    wantDot c, dest, it.n # export marker must be empty
    wantDot c, dest, it.n # pragmas
    if loopvarTypeMod != NoType and loopvarType.typeKind notin TypeModifiers:
      dest.buildTree loopvarTypeMod, it.n.info:
        copyTree dest, loopvarType
    else:
      copyTree dest, loopvarType
    skip it.n # skip over the type which might have been set already as we tend to re-sem stuff
    wantDot c, dest, it.n # value
    takeParRi dest, it.n
    publish c, dest, delayed.s.name, declStart
  else:
    buildErr c, dest, it.n.info, "illformed AST: `let` inside `unpackflat` expected"
    skip it.n

proc isIterator(c: var SemContext; dest: var TokenBuf; s: SymId): bool =
  let sym = fetchSym(c, s)
  let res = declToCursor(c, dest, sym)
  result = res.status == LacksNothing and res.decl.symKind == IteratorY

proc semForLoopTupleVar(c: var SemContext; dest: var TokenBuf; it: var Item; tup: TypeCursor; loopvarTypeMod = NoType) =
  var tup = tup
  var loopvarTypeMod = loopvarTypeMod
  if tup.typeKind in TypeModifiers:
    loopvarTypeMod = tup.typeKind
    inc tup
  inc tup
  while it.n.kind != ParRi and tup.kind != ParRi:
    let field = getTupleFieldType(tup)
    if it.n.substructureKind == UnpacktupU:
      takeToken dest, it.n
      if field.skipModifier.typeKind == TupleT:
        semForLoopTupleVar c, dest, it, field, loopvarTypeMod
      else:
        buildErr c, dest, it.n.info, "tuple types expected, but got: " & $field
      takeParRi dest, it.n
    else:
      semForLoopVar c, dest, it, field, loopvarTypeMod
    skip tup
  if it.n.kind == ParRi:
    if tup.kind == ParRi:
      discard "all fine"
    else:
      buildErr c, dest, it.n.info, "too few for loop variables"
  else:
    buildErr c, dest, it.n.info, "too many for loop variables"
    skipToEnd it.n

include semfields

proc isIteratorCall(c: var SemContext; dest: var TokenBuf; beforeCall: int): bool {.inline.} =
  result = dest.len > beforeCall+1
  if result:
    let callKind =
      if dest[beforeCall].kind == ParLe and rawTagIsNimonyExpr(tagEnum(dest[beforeCall])):
        cast[NimonyExpr](tagEnum(dest[beforeCall]))
      else:
        NoExpr
    result = callKind in CallKinds and
      dest[beforeCall+1].kind == Symbol and
      c.isIterator(dest, dest[beforeCall+1].symId)

proc isIdentCall(c: var SemContext; dest: var TokenBuf; beforeCall: int): bool {.inline.} =
  result = dest.len > beforeCall+1
  if result:
    let callKind =
      if dest[beforeCall].kind == ParLe and rawTagIsNimonyExpr(tagEnum(dest[beforeCall])):
        cast[NimonyExpr](tagEnum(dest[beforeCall]))
      else:
        NoExpr
    result = callKind in CallKinds and
      dest[beforeCall+1].kind == Ident

proc semFor(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  let orig = it.n
  takeToken dest, it.n
  var iterCall = Item(n: it.n, typ: c.types.autoType)
  let callInfo = iterCall.n.info
  let beforeCall = dest.len
  semExpr c, dest, iterCall, {PreferIterators, KeepMagics}
  it.n = iterCall.n
  var isMacroLike = false
  if isIteratorCall(c, dest, beforeCall):
    discard "fine"
  elif dest[beforeCall].kind == ParLe and
      (dest[beforeCall].tagId == TagId(FieldsTagId) or
       dest[beforeCall].tagId == TagId(FieldPairsTagId) or
       dest[beforeCall].tagId == TagId(InternalFieldPairsTagId)):
    var callBuf = createTokenBuf(dest.len - beforeCall)
    for tok in beforeCall ..< dest.len: callBuf.add dest[tok]
    dest.shrink beforeCall-1
    var call = beginRead(callBuf)
    semForFields c, dest, it, call, orig
    return
  elif iterCall.typ.typeKind == UntypedT or
      # for iterators from concepts in generic context:
      isIdentCall(c, dest, beforeCall):
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
      dest.shrink beforeCall
      buildErr c, dest, callInfo, "iterator expected"
    if name != "":
      # try implicit iterator call
      var callBuf = createTokenBuf(32)
      callBuf.addParLe(CallX, callInfo)
      discard buildSymChoice(c, callBuf, pool.strings.getOrIncl(name), info, FindAll)
      for tok in beforeCall ..< dest.len: callBuf.add dest[tok]
      callBuf.addParRi()
      let argType = iterCall.typ
      iterCall = Item(n: beginRead(callBuf), typ: c.types.autoType)
      shrink dest, beforeCall
      semCall c, dest, iterCall, {PreferIterators}
      if isIteratorCall(c, dest, beforeCall):
        discard "fine"
      elif iterCall.typ.typeKind == UntypedT or
          # for iterators from concepts in generic context:
          isIdentCall(c, dest, beforeCall):
        isMacroLike = true
      else:
        if dest[beforeCall].kind == ParLe and dest[beforeCall].tagId == ErrT:
          # original nim gives `items` overload errors so preserve them
          discard
        else:
          dest.shrink beforeCall
          buildErr c, dest, callInfo, "no implicit `" & name & "` iterator found for type " & typeToString(argType)
  withNewScope c:
    case substructureKind(it.n)
    of UnpackflatU:
      takeToken dest, it.n
      var n2 = it.n
      skip n2
      let hasMultiVars = n2.kind != ParRi
      if hasMultiVars:
        if iterCall.typ.skipModifier.typeKind == TupleT:
          semForLoopTupleVar c, dest, it, iterCall.typ
        else:
          # error handling
          while it.n.kind != ParRi:
            semForLoopVar c, dest, it, c.types.autoType
      else:
        semForLoopVar c, dest, it, iterCall.typ

      takeParRi dest, it.n
    of UnpacktupU:
      takeToken dest, it.n
      if iterCall.typ.skipModifier.typeKind == TupleT:
        semForLoopTupleVar c, dest, it, iterCall.typ
      else:
        buildErr c, dest, it.n.info, "tuple types expected, but got: " & $iterCall.typ
      takeParRi dest, it.n
    else:
      buildErr c, dest, it.n.info, "illformed AST: `unpackflat` or `unpacktup` inside `for` expected"
      skip it.n

    if isMacroLike and false:
      takeTree dest, it.n # don't touch the body
    else:
      inc c.routine.inLoop
      semStmt c, dest, it.n, true
      dec c.routine.inLoop

  takeParRi dest, it.n
  producesNoReturn c, dest, info, it.typ

proc semReturn(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  if c.routine.kind == NoSym:
    buildErr c, dest, info, "`return` only allowed within a routine"
  if it.n.kind == DotToken:
    if c.routine.returnType.typeKind != VoidT:
      dest.addSymUse c.routine.resId, info
      inc it.n # skips the dot
    else:
      takeToken dest, it.n
  else:
    var a = Item(n: it.n, typ: c.routine.returnType)
    # `return` within a template refers to the caller, so
    # we allow any type here:
    if c.routine.kind == TemplateY:
      a.typ = c.types.autoType
    semExpr c, dest, a
    it.n = a.n
  takeParRi dest, it.n
  producesNoReturn c, dest, info, it.typ

proc semRaise(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  if c.routine.kind == NoSym:
    buildErr c, dest, info, "`raise` only allowed within a routine"
  elif not c.routine.pragmas.contains(RaisesP) and not c.g.config.compat:
    buildErr c, dest, info, "`raise` only allowed within a routine with `raises` pragma"
  if it.n.kind == DotToken:
    takeToken dest, it.n
  else:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, dest, a
    if a.typ.kind == Symbol and pool.syms[a.typ.symId] == ErrorCodeName:
      discard "ok"
    else:
      buildErr c, dest, info, "only type `system.ErrorCode` is allowed to be raised"
    it.n = a.n
  takeParRi dest, it.n
  producesNoReturn c, dest, info, it.typ

proc semYield(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  if c.routine.kind != IteratorY and not c.routine.pragmas.contains(PassiveP):
    buildErr c, dest, info, "`yield` only allowed within an `iterator`"
  if it.n.kind == DotToken:
    takeToken dest, it.n
  else:
    let expectedType =
      if c.routine.pragmas.contains(PassiveP): c.types.autoType
      else: c.routine.returnType
    var a = Item(n: it.n, typ: expectedType)
    semExpr c, dest, a
    it.n = a.n
  takeParRi dest, it.n
  producesVoid c, dest, info, it.typ

proc semTypedBinaryArithmetic(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  let typeStart = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  let typ = typeToCursor(c, dest, typeStart)
  semExpr c, dest, it
  semExpr c, dest, it
  takeParRi dest, it.n
  commonType c, dest, it, beforeExpr, typ

proc semShift(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  let typeStart = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  let typ = typeToCursor(c, dest, typeStart)
  semExpr c, dest, it
  var shift = Item(n: it.n, typ: c.types.autoType)
  let shiftInfo = shift.n.info
  semExpr c, dest, shift
  it.n = shift.n
  if shift.typ.typeKind notin {IntT, UIntT}:
    c.buildErr dest, shiftInfo, "expected integer for shift operand"
  takeParRi dest, it.n
  commonType c, dest, it, beforeExpr, typ

proc semCmp(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  let typeStart = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  let typ = typeToCursor(c, dest, typeStart)
  var operand = Item(n: it.n, typ: typ)
  semExpr c, dest, operand
  semExpr c, dest, operand
  it.n = operand.n
  takeParRi dest, it.n
  commonType c, dest, it, beforeExpr, c.types.boolType

proc literal(c: var SemContext; dest: var TokenBuf; it: var Item; literalType: TypeCursor) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  let expected = it.typ
  it.typ = literalType
  commonType c, dest, it, beforeExpr, expected

proc literalB(c: var SemContext; dest: var TokenBuf; it: var Item; literalType: TypeCursor) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  var literalType = literalType
  if it.n.kind != ParRi:
    let typeStart = dest.len
    semLocalTypeImpl c, dest, it.n, InLocalDecl
    literalType = typeToCursor(c, dest, typeStart)
    if it.n.kind != ParRi and it.n.exprKind == NilX:
      skip it.n
  takeParRi dest, it.n
  let expected = it.typ
  it.typ = literalType
  commonType c, dest, it, beforeExpr, expected

proc semNil(c: var SemContext; dest: var TokenBuf; it: var Item) =
  literalB c, dest, it, c.types.nilType

proc semTypedUnaryArithmetic(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  let typeStart = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  let typ = typeToCursor(c, dest, typeStart)
  semExpr c, dest, it
  takeParRi dest, it.n
  commonType c, dest, it, beforeExpr, typ

proc semDelay(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  let typeStart = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  let typ = typeToCursor(c, dest, typeStart)
  semStmt c, dest, it.n, false
  takeParRi dest, it.n
  commonType c, dest, it, beforeExpr, typ

type ArrayConstrContext = object
  firstKeyType: TypeCursor
  currentIndex: xint
  firstIdx: xint
  hasFirstIdx: bool

proc semArrayConstrElem(c: var SemContext, dest: var TokenBuf, elem: var Item, it: Item, arrCtx: var ArrayConstrContext) =
  if elem.n.substructureKind == KvU:
    var indexType = c.types.autoType
    if it.typ.typeKind == ArrayT:
      var it2 = it.typ
      inc it2 # skip ArrayT
      skip it2 # skip element type
      indexType = it2
    elif arrCtx.firstKeyType.typeKind != AutoT:
      indexType = arrCtx.firstKeyType
    inc elem.n # skip KvU
    var key = Item(n: elem.n, typ: indexType)
    var keyBuf = createTokenBuf(16)
    semExpr c, keyBuf, key
    if arrCtx.firstKeyType.typeKind == AutoT:
      arrCtx.firstKeyType = key.typ
    let val = evalOrdinal(c, cursorAt(keyBuf, 0))
    if not isNaN(val):
      if not arrCtx.hasFirstIdx:
        arrCtx.firstIdx = val
        arrCtx.hasFirstIdx = true
      elif val - arrCtx.currentIndex != createXint(1'i64):
        c.buildErr(dest, cursorAt(keyBuf, 0).info, "invalid order in array constructor")
      arrCtx.currentIndex = val
    elem.n = key.n
    semExpr c, dest, elem
    skipParRi elem.n
  else:
    if arrCtx.hasFirstIdx:
      inc arrCtx.currentIndex
    else:
      arrCtx.hasFirstIdx = true
    semExpr c, dest, elem    

proc semBracket(c: var SemContext; dest: var TokenBuf, it: var Item; flags: set[SemFlag]) =
  let exprStart = dest.len
  let info = it.n.info

  var orig = it.n
  inc it.n
  dest.addParLe(AconstrX, info)
  if it.n.kind == ParRi:
    # empty array
    case it.typ.typeKind
    of AutoT:
      if AllowEmpty in flags:
        # keep it.typ as auto
        dest.addSubtree it.typ
      else:
        buildErr c, dest, info, "empty array needs a specified type"
      takeParRi dest, it.n
    of ArrayT:
      dest.addSubtree it.typ
      takeParRi dest, it.n
    else:
      # unknown expected type, give empty literal auto type, then match it
      dest.addSubtree c.types.autoType
      takeParRi dest, it.n
      let expected = it.typ
      it.typ = c.types.autoType
      commonType c, dest, it, exprStart, expected
    return

  let typeInsertPos = dest.len
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
    discard

  var ctx = ArrayConstrContext(
    firstKeyType: c.types.autoType,
    currentIndex: createXint(0'i64),
    firstIdx: createXint(0'i64),
    hasFirstIdx: false
  )

  semArrayConstrElem(c, dest, elem, it, ctx)
  if freshElemType:
    # do not save modifier in array type unless it was annotated as such
    # also do not expect it from subsequent elements
    removeModifier(elem.typ)
  var count = 1
  while elem.n.kind != ParRi:
    semArrayConstrElem(c, dest, elem, it, ctx)
    inc count
  it.n = elem.n
  takeParRi dest, it.n
  let typeStart = dest.len
  dest.buildTree ArrayT, info:
    dest.addSubtree elem.typ
    dest.addParLe(RangetypeT, info)
    var idxType = c.types.intType
    if ctx.firstKeyType.typeKind != AutoT:
      idxType = ctx.firstKeyType
    dest.addSubtree idxType
    var serr = false
    dest.addIntLit(asSigned(ctx.firstIdx, serr), info)
    dest.addIntLit(asSigned(ctx.firstIdx + createXint(count.int64 - 1), serr), info)
    dest.addParRi()
  let expected = it.typ
  it.typ = typeToCursor(c, dest, typeStart)

  case expected.typeKind
  of AutoT, ArrayT:
    discard
  else:
    var convMatch = default(Match)
    let convArg = CallArg(n: orig, typ: it.typ)
    if tryConverterMatch(c, convMatch, expected, convArg):
      discard "matching converter found (e.g. `toOpenArray`)"
    else:
      buildErr c, dest, info, "invalid expected type for array constructor: " & typeToString(expected)

  dest.shrink typeStart
  dest.insert it.typ, typeInsertPos
  commonType c, dest, it, exprStart, expected

proc semCurly(c: var SemContext; dest: var TokenBuf, it: var Item; flags: set[SemFlag]) =
  let exprStart = dest.len
  let info = it.n.info
  inc it.n
  dest.addParLe(SetConstrX, info)
  if it.n.kind == ParRi:
    # empty set
    case it.typ.typeKind
    of AutoT:
      if AllowEmpty in flags:
        # keep it.typ as auto
        dest.addSubtree it.typ
      else:
        buildErr c, dest, info, "empty set needs a specified type"
      takeParRi dest, it.n
    of SetT:
      dest.addSubtree it.typ
      takeParRi dest, it.n
    else:
      # unknown expected type, give empty literal auto type, then match it
      dest.addSubtree c.types.autoType
      takeParRi dest, it.n
      let expected = it.typ
      it.typ = c.types.autoType
      commonType c, dest, it, exprStart, expected
    return

  let typeInsertPos = dest.len
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
    buildErr c, dest, info, "invalid expected type for set constructor: " & typeToString(it.typ)
  var elemStart = dest.len
  var elemInfo = elem.n.info
  while elem.n.kind != ParRi:
    if isRangeExpr(elem.n):
      inc elem.n # call tag
      skip elem.n # `..`
      dest.buildTree RangeU, elem.n.info:
        elemStart = dest.len
        elemInfo = elem.n.info
        semExpr c, dest, elem
        if freshElemType:
          # do not save modifier in set type unless it was annotated as such
          # also do not expect it from subsequent elements
          removeModifier(elem.typ)
          freshElemType = false
        semExpr c, dest, elem
      inc elem.n # right paren of call
    elif elem.n.substructureKind == RangeU:
      takeToken dest, elem.n
      semExpr c, dest, elem
      if freshElemType:
        # do not save modifier in set type unless it was annotated as such
        # also do not expect it from subsequent elements
        removeModifier(elem.typ)
        freshElemType = false
      semExpr c, dest, elem
      takeParRi dest, elem.n
    else:
      semExpr c, dest, elem
      if freshElemType:
        # do not save modifier in set type unless it was annotated as such
        # also do not expect it from subsequent elements
        removeModifier(elem.typ)
        freshElemType = false
  if containsGenericParams(elem.typ):
    discard
  elif not isOrdinalType(elem.typ, allowEnumWithHoles = true):
    c.buildErr dest, elemInfo, "set element type must be ordinal"
  #elif elem.typ.typeKind == IntT and dest[elemStart].kind == IntLit:
  #  set to range of 0..<DefaultSetElements
  else:
    let length = lengthOrd(c, elem.typ)
    if length.isNaN or length > MaxSetElements:
      c.buildErr dest, elemInfo, "type " & typeToString(elem.typ) & " is too large to be a set element type"
  it.n = elem.n
  takeParRi dest, it.n
  let typeStart = dest.len
  dest.buildTree SetT, info:
    dest.addSubtree elem.typ
  let expected = it.typ
  it.typ = typeToCursor(c, dest, typeStart)
  dest.shrink typeStart
  dest.insert it.typ, typeInsertPos
  commonType c, dest, it, exprStart, expected

proc semArrayConstr(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let start = dest.len
  let expected = it.typ
  let info = it.n.info
  takeToken dest, it.n
  it.typ = semLocalType(c, dest, it.n)
  # XXX type length not enforced
  var elem = Item(n: it.n, typ: c.types.autoType)
  let t = skipModifier(it.typ)
  if t.typeKind == ArrayT:
    elem.typ = t
    inc elem.typ
  else:
    c.buildErr dest, info, "expected array type for array constructor, got: " & typeToString(t)
  while elem.n.kind != ParRi:
    semExpr c, dest, elem
  it.n = elem.n
  takeParRi dest, it.n
  commonType c, dest, it, start, expected

proc semSetConstr(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let start = dest.len
  let expected = it.typ
  let info = it.n.info
  takeToken dest, it.n
  it.typ = semLocalType(c, dest, it.n)
  var elem = Item(n: it.n, typ: c.types.autoType)
  let t = skipModifier(it.typ)
  if t.typeKind == SetT:
    elem.typ = t
    inc elem.typ
  else:
    c.buildErr dest, info, "expected set type for set constructor, got: " & typeToString(t)
  while elem.n.kind != ParRi:
    if elem.n.substructureKind == RangeU:
      takeToken dest, elem.n
      semExpr c, dest, elem
      semExpr c, dest, elem
      takeParRi dest, elem.n
    else:
      semExpr c, dest, elem
  it.n = elem.n
  takeParRi dest, it.n
  commonType c, dest, it, start, expected

proc semSuf(c: var SemContext; dest: var TokenBuf, it: var Item) =
  let exprStart = dest.len
  takeToken dest, it.n
  var num = Item(n: it.n, typ: c.types.autoType)
  semExpr c, dest, num
  it.n = num.n
  if it.n.kind != StringLit:
    c.buildErr dest, it.n.info, "string literal expected for suf"
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
    c.buildErr dest, it.n.info, "unknown suffix: " & pool.strings[it.n.litId]
  takeToken dest, it.n # suffix
  takeParRi dest, it.n # right paren
  commonType c, dest, it, exprStart, expected

proc semTup(c: var SemContext; dest: var TokenBuf, it: var Item) =
  let exprStart = dest.len
  let origExpected = it.typ
  dest.add parLeToken(TupConstrX, it.n.info)
  inc it.n
  if it.n.kind == ParRi:
    it.typ = c.types.emptyTupleType
    dest.addSubtree it.typ
    takeParRi dest, it.n
    commonType c, dest, it, exprStart, origExpected
    return
  let typePos = dest.len
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
        c.buildErr dest, it.n.info, "expected field name for named tuple constructor"
      else:
        typ.add it.n
        takeToken dest, it.n
        let nameCursor = it.n
        let name = takeIdent(it.n)
        let nameStart = dest.len
        if name == StrId(0):
          c.buildErr dest, nameCursor.info, "invalid tuple field name", nameCursor
        else:
          dest.add identToken(name, nameCursor.info)
        for tok in nameStart ..< dest.len:
          typ.add dest[tok]
    var elem = Item(n: it.n, typ: c.types.autoType)
    var freshElemType = true
    if doExpected:
      elem.typ = getTupleFieldType(expected)
      freshElemType = false
      skip expected
      if expected.kind == ParRi:
        # happens if expected tuple type has less fields than constructor
        doExpected = false
    semExpr c, dest, elem
    it.n = elem.n
    if freshElemType:
      # do not save modifier in tuple type unless it was annotated as such
      removeModifier(elem.typ)
    typ.addSubtree elem.typ # type
    if named:
      # should be KvX
      takeParRi dest, it.n
      typ.addParRi()
  takeParRi dest, it.n
  typ.addParRi()
  let typeStart = dest.len
  var t = typ.cursorAt(0)
  semTupleType c, dest, t
  it.typ = typeToCursor(c, dest, typeStart)
  dest.shrink typeStart
  dest.insert it.typ, typePos
  commonType c, dest, it, exprStart, origExpected

proc semTupleConstr(c: var SemContext; dest: var TokenBuf, it: var Item) =
  let start = dest.len
  let expected = it.typ
  let info = it.n.info
  takeToken dest, it.n
  it.typ = semLocalType(c, dest, it.n)
  var t = skipModifier(it.typ)
  if t.typeKind != TupleT:
    dest.shrink start
    c.buildErr dest, info, "expected tuple type for tuple constructor, got: " & typeToString(t)
    return
  inc t
  while it.n.kind != ParRi:
    let named = it.n.substructureKind == KvU
    if named:
      takeToken dest, it.n
      takeTree dest, it.n
    var elem = Item(n: it.n, typ: c.types.autoType)
    if t.kind != ParRi:
      elem.typ = getTupleFieldType(t)
      skip t
    else:
      c.buildErr dest, info, "tuple type " & typeToString(it.typ) & " too short for tuple constructor"
    semExpr c, dest, elem
    it.n = elem.n
    if named:
      takeParRi dest, it.n
  if t.kind != ParRi:
    c.buildErr dest, info, "tuple type " & typeToString(it.typ) & " too long for tuple constructor"
  takeParRi dest, it.n
  commonType c, dest, it, start, expected

proc callDefault(c: var SemContext; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo) =
  var callBuf = createTokenBuf(16)
  callBuf.addParLe(CallX, info)
  discard buildSymChoice(c, callBuf, pool.strings.getOrIncl("default"), info, FindAll)
  callBuf.addSubtree typ
  callBuf.addParRi()
  var it = Item(n: cursorAt(callBuf, 0), typ: c.types.autoType)
  semCall c, dest, it, {}

proc buildObjConstrField(c: var SemContext; dest: var TokenBuf; field: Local;
                         setFields: Table[SymId, Cursor]; info: PackedLineInfo;
                         bindings: Table[SymId, Cursor]; depth: int) =
  let fieldSym = field.name.symId
  if fieldSym in setFields:
    dest.addSubtree setFields[fieldSym]
  else:
    dest.addParLe(KvU, info)
    dest.add symToken(fieldSym, info)
    var typ = field.typ
    if bindings.len != 0:
      # fields in generic type AST contain generic params of the type
      # for invoked object types, bindings are built from the given arguments
      # and the field type is instantiated based on them here
      typ = instantiateType(c, typ, bindings)
    callDefault c, dest, typ, info
    if depth != 0:
      dest.addIntLit(depth, info)
    dest.addParRi()

proc fieldsPresentInInitExpr(c: var SemContext; n: Cursor; setFields: Table[SymId, Cursor]): (bool, SymId) =
  var n = n
  inc n
  result = (false, SymId(0))
  if n.substructureKind == NilU:
    return
  while n.kind != ParRi:
    let local = takeLocal(n, SkipFinalParRi)
    if local.name.symId in setFields:
      result = (true, local.name.symId)
      break

proc asNimSym(symId: SymId): string =
  result = pool.syms[symId]
  extractBasename(result)

template conflictingBranchesError(c: var SemContext; dest: var TokenBuf, info: PackedLineInfo, prevFields: SymId, currentFields: SymId) =
  c.buildErr dest, info, "The fields '" & asNimSym(prevFields) &
              "' and '" & asNimSym(currentFields) & "' cannot be initialized together, " &
              "because they are from conflicting branches in the case object."

template badDiscriminatorError(c: var SemContext; dest: var TokenBuf, info: PackedLineInfo, field: SymId, discriminator: SymId) =
  c.buildErr dest, info,
    ("cannot prove that it's safe to initialize '$1' with " &
    "the runtime value for the discriminator '$2'.") %
    [asNimSym(field), asNimSym(discriminator)]

template wrongBranchError(c: var SemContext; dest: var TokenBuf, info: PackedLineInfo, field: SymId,
      discriminator: SymId, discriminatorVal: Cursor) =
  c.buildErr dest, info,
      ("a case selecting discriminator '$1' with value '$2' " &
      "appears in the object construction, but the field(s) '$3' " &
      "are in conflict with this value.") %
      [asNimSym(discriminator), asNimCode(discriminatorVal), asNimSym(field)]

proc caseBranchMatchesExpr(c: var SemContext; dest: var TokenBuf; branch, matched: Cursor; selectorType: Cursor): bool =
  result = false
  var branch = branch
  inc branch
  while branch.kind != ParRi:
    case branch.substructureKind
    of RangeU:
      inc branch
      let a = evalConstIntExpr(c, dest, branch, selectorType)
      let b = evalConstIntExpr(c, dest, branch, selectorType)

      var matched = matched
      let value = evalConstIntExpr(c, dest, matched, selectorType)
      if value >= a and value <= b:
        return true
      skipParRi(branch)
    else:
      return sameTrees(branch, matched)

type
  BranchState = enum
    NoSelector
    Unknown
    ThisBranch

proc getValueInKv(n: Cursor): Cursor =
  result = n
  inc result
  skip result

proc fieldsPresentInBranch(c: var SemContext; dest: var TokenBuf; n: var Cursor; selector: Local;
                setFields: Table[SymId, Cursor]; info: PackedLineInfo;
                bindings: Table[SymId, Cursor]; depth: int) =
  var lastFieldSymId = SymId(0)
  var isBranchSelected = false
  let selectorSymId = selector.name.symId

  var bestBranch = default(Cursor)
  block matched:
    while n.kind != ParRi:
      case n.substructureKind
      of OfU:
        inc n
        var state: BranchState
        if selectorSymId in setFields:
          let discriminatorVal = getValueInKv(setFields[selectorSymId])
          if caseBranchMatchesExpr(c, dest, n, discriminatorVal, selector.typ):
            state = ThisBranch
            isBranchSelected = true
          else:
            state = Unknown
        else:
          state = NoSelector
        skip n

        if bestBranch == default(Cursor):
          bestBranch = n
        let (hasField, presentFieldSymId) = fieldsPresentInInitExpr(c, n, setFields)
        if hasField:
          if lastFieldSymId != SymId(0):
            conflictingBranchesError c, dest, info, lastFieldSymId, presentFieldSymId
          elif state == Unknown:
            wrongBranchError(c, dest, info, presentFieldSymId, selectorSymId, getValueInKv(setFields[selectorSymId]))
          inc n # stmt
          while n.kind != ParRi:
            let field = takeLocal(n, SkipFinalParRi)
            buildObjConstrField(c, dest, field, setFields, info, bindings, depth)
          skipParRi n
          lastFieldSymId = presentFieldSymId
        else:
          skip n

        skipParRi n
      of ElseU:
        inc n
        let (hasField, presentFieldSymId) = fieldsPresentInInitExpr(c, n, setFields)
        if hasField:
          if lastFieldSymId != SymId(0):
            conflictingBranchesError c, dest, info, lastFieldSymId, presentFieldSymId
          elif isBranchSelected and selectorSymId in setFields:
            wrongBranchError(c, dest, info, presentFieldSymId, selectorSymId, getValueInKv(setFields[selectorSymId]))
          inc n # stmt
          while n.kind != ParRi:
            let field = takeLocal(n, SkipFinalParRi)
            buildObjConstrField(c, dest, field, setFields, info, bindings, depth)
          skipParRi n # stmt
          lastFieldSymId = presentFieldSymId
        else:
          skip n
        skipParRi n
      else:
        error "illformed AST inside case object: ", n

  if selectorSymId notin setFields:
    if lastFieldSymId != SymId(0):
      badDiscriminatorError(c, dest, info, lastFieldSymId, selectorSymId)
    elif bestBranch != default(Cursor):
      inc bestBranch # stmt
      while bestBranch.kind != ParRi:
        if bestBranch.substructureKind == NilU:
          skip bestBranch
          break
        let field = takeLocal(bestBranch, SkipFinalParRi)
        buildObjConstrField(c, dest, field, setFields, info, bindings, depth)
      skipParRi bestBranch

proc buildObjConstrFields(c: var SemContext; dest: var TokenBuf; n: var Cursor;
                          setFields: Table[SymId, Cursor]; info: PackedLineInfo;
                          bindings: Table[SymId, Cursor]; depth = 0) =
  var iter = initObjFieldIter()
  while nextField(iter, n, keepCase = true):
    if n.substructureKind == CaseU:
      var body = n
      inc body
      # selector
      let field = takeLocal(body, SkipFinalParRi)
      buildObjConstrField(c, dest, field, setFields, info, bindings, depth)

      fieldsPresentInBranch(c, dest, body, field, setFields, info, bindings, depth)
      skip n
    else:
      let field = takeLocal(n, SkipFinalParRi)
      buildObjConstrField(c, dest, field, setFields, info, bindings, depth)

proc buildDefaultObjConstr(c: var SemContext; dest: var TokenBuf; typ: Cursor;
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
      c.buildErr dest, info, "cannot build object constructor for type: " & typeToString(objImpl)
      return
  else:
    c.buildErr dest, info, "cannot build object constructor for type: " & typeToString(objImpl)
    return
  dest.addParLe(constrKind, info)
  dest.addSubtree typ
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
        buildObjConstrFields(c, dest, currentField, setFields, info, bindings, depth)
      parentType = parent.parentType
      inc depth
    # bring back original bindings:
    bindings = origBindings
  var currentField = obj.firstField
  if currentField.kind != DotToken:
    buildObjConstrFields(c, dest, currentField, setFields, info, bindings)
  dest.addParRi()

proc semObjConstr(c: var SemContext; dest: var TokenBuf, it: var Item) =
  let exprStart = dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, dest, it.n)
  dest.shrink exprStart
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
      c.buildErr dest, info, "expected type for object constructor"
      skipToEnd it.n
      return
    objType = decl.objBody
    if objType.typeKind != ObjectT:
      c.buildErr dest, info, "expected object type for object constructor"
      skipToEnd it.n
      return
  else:
    c.buildErr dest, info, "expected type symbol for object constructor"
    skipToEnd it.n
    return
  # build bindings for invoked object type to get proper types for fields:
  let bindings = bindInvokeArgs(decl, invokeArgs)
  var fieldBuf = createTokenBuf(16)
  var setFieldPositions = initTable[SymId, int]()
  while it.n.kind != ParRi:
    if it.n.substructureKind != KvU:
      c.buildErr dest, it.n.info, "expected key/value pair in object constructor"
      skip it.n
    else:
      let fieldStart = fieldBuf.len
      fieldBuf.add it.n
      inc it.n
      let fieldInfo = it.n.info
      let fieldNameCursor = it.n
      let fieldName = takeIdent(it.n)
      if fieldName == StrId(0):
        c.buildErr dest, fieldInfo, "identifier expected for object field"
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
            c.buildErr dest, fieldInfo, "field already set: " & pool.strings[fieldName]
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
            semExpr c, fieldBuf, val
            it.n = val.n
        else:
          c.buildErr dest, fieldInfo, "undeclared field: '" & pool.strings[fieldName] & "' for type " & typeToString(it.typ)
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
  buildDefaultObjConstr(c, dest, it.typ, setFields, info, bindings)
  commonType c, dest, it, exprStart, expected

proc semObjDefault(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let exprStart = dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, dest, it.n)
  dest.shrink exprStart
  skipParRi it.n
  buildDefaultObjConstr(c, dest, it.typ, initTable[SymId, Cursor](), info)
  commonType c, dest, it, exprStart, expected

proc semNewref(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let exprStart = dest.len
  let expected = it.typ
  let info = it.n.info
  dest.takeToken it.n
  let beforeTypeArg = dest.len
  it.typ = semLocalType(c, dest, it.n)
  dest.shrink beforeTypeArg
  if it.typ.typeKind == TypedescT:
    inc it.typ
  dest.addSubtree it.typ
  assert it.typ.typeKind == RefT
  let typeForDefault = it.typ.firstSon
  callDefault c, dest, typeForDefault, info
  skip it.n # type
  if it.n.kind != ParRi:
    skip it.n # existing `default(T)` call
  takeParRi dest, it.n
  commonType c, dest, it, exprStart, expected

proc buildDefaultTuple(c: var SemContext; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo) =
  dest.addParLe(TupConstrX, info)
  dest.addSubtree typ
  var currentField = typ
  inc currentField # skip tuple tag
  while currentField.kind != ParRi:
    let field = getTupleFieldType(currentField)
    callDefault c, dest, field, info
    skip currentField
  dest.addParRi()

proc semTupleDefault(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let exprStart = dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, dest, it.n)
  dest.shrink exprStart
  skipParRi it.n
  buildDefaultTuple(c, dest, it.typ, info)
  commonType c, dest, it, exprStart, expected

proc semDefaultDistinct(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let exprStart = dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, dest, it.n)
  dest.shrink exprStart
  skipParRi it.n
  var isDistinct = false
  let srcBase = skipDistinct(it.typ, isDistinct)

  dest.add parLeToken(DconvX, info)
  dest.add it.typ
  callDefault c, dest, srcBase, info
  dest.addParRi()
  commonType c, dest, it, exprStart, expected

proc semTupAt(c: var SemContext; dest: var TokenBuf; it: var Item) =
  # has already been semchecked but we do it again:
  let exprStart = dest.len
  let expected = it.typ
  takeToken dest, it.n
  var tup = Item(n: it.n, typ: c.types.autoType)
  let tupInfo = tup.n.info
  semExpr c, dest, tup
  let tupleType = skipModifier(tup.typ)
  if tupleType.typeKind != TupleT:
    if tupleType.kind == Symbol and getTypeSection(tupleType.symId).kind == TypevarY:
      # for `T: tuple`
      var index = Item(n: tup.n, typ: c.types.autoType)
      semExpr c, dest, index
      it.n = index.n
      takeParRi dest, it.n
      it.typ = c.types.untypedType
    else:
      c.buildErr dest, tupInfo, "expected tuple but got: " & typeToString(tupleType)
    return
  var idx = tup.n
  let idxStart = dest.len
  let idxInfo = idx.info
  semConstIntExpr c, dest, idx
  var idxValue = evalOrdinal(c, cursorAt(dest, idxStart))
  endRead(dest)
  it.n = idx
  let zero = createXint(0'i64)
  if idxValue.isNaN or idxValue < zero:
    shrink dest, idxStart
    c.buildErr dest, idxInfo, "must be a constant expression >= 0"
    takeParRi dest, it.n
  else:
    it.typ = tupleType
    inc it.typ
    # navigate to the proper type within the tuple type:
    let one = createXint(1'i64)
    while true:
      if it.typ.kind == ParRi:
        shrink dest, idxStart
        c.buildErr dest, idxInfo, "tuple index too large"
        break
      if idxValue > zero:
        skip it.typ
        idxValue = idxValue - one
      else:
        break
    if it.typ.kind != ParRi:
      it.typ = getTupleFieldType(it.typ)
    takeParRi dest, it.n
    commonType c, dest, it, exprStart, expected

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

proc semDefined(c: var SemContext; dest: var TokenBuf; it: var Item) =
  inc it.n
  let info = it.n.info
  let orig = it.n
  let name = getDottedIdent(it.n)
  skipParRi it.n
  if name == "":
    c.buildErr dest, info, "invalid expression for defined: " & asNimCode(orig), orig
  else:
    let isDefined = c.g.config.isDefined(name)
    let beforeExpr = dest.len
    dest.addParLe(if isDefined: TrueX else: FalseX, info)
    dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, dest, it, beforeExpr, expected

proc semDeclared(c: var SemContext; dest: var TokenBuf; it: var Item) =
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
    c.buildErr dest, info, "invalid expression for declared: " & asNimCode(orig), orig
  else:
    let isDeclared = isDeclared(c, nameId)
    let beforeExpr = dest.len
    dest.addParLe(if isDeclared: TrueX else: FalseX, info)
    dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, dest, it, beforeExpr, expected

proc hasError(dest: TokenBuf): bool =
  let errTag = pool.tags.getOrIncl("err")
  var i = 0
  result = false
  while i < dest.len:
    if dest[i].kind == ParLe and dest[i].tagId == errTag:
      result = true
      break
    else:
      inc i

proc semCompiles(c: var SemContext; dest: var TokenBuf; it: var Item) =
  inc it.n
  let info = it.n.info

  let beforeExpr = dest.len

  let oldScope = c.currentScope
  let oldProcRequestsLen = c.procRequests.len
  let oldTypeInstDeclsLen = c.typeInstDecls.len
  let oldInstantiatedFrom = c.instantiatedFrom.len
  let oldInWhen = c.inWhen
  let oldTemplateInstCounter = c.templateInstCounter
  let oldExpanded = c.expanded.len
  let oldIncludeStackLen = c.includeStack.len
  let oldDebugAllowErrors = c.debugAllowErrors
  c.debugAllowErrors = true

  c.openScope()
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr(c, dest, arg)

  c.currentScope = oldScope

  let hasError = hasError(dest)
  shrink dest, beforeExpr
  c.currentScope = oldScope
  c.procRequests.setLen(oldProcRequestsLen)
  c.typeInstDecls.setLen(oldTypeInstDeclsLen)
  c.instantiatedFrom.setLen(oldInstantiatedFrom)
  c.includeStack.setLen(oldIncludeStackLen)

  c.inWhen = oldInWhen
  c.templateInstCounter = oldTemplateInstCounter
  c.expanded.shrink(oldExpanded)
  c.debugAllowErrors = oldDebugAllowErrors


  skip it.n
  skipParRi(it.n)

  let expected = it.typ
  dest.addParLe(if hasError: FalseX else: TrueX, info)
  dest.addParRi()
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected


proc semAstToStr(c: var SemContext; dest: var TokenBuf; it: var Item) =
  inc it.n
  let info = it.n.info
  let astStr = asNimCode(it.n)
  skip it.n
  skipParRi it.n

  let beforeExpr = dest.len
  dest.addStrLit(astStr, info)

  let expected = it.typ
  it.typ = c.types.stringType
  commonType c, dest, it, beforeExpr, expected

proc semIsMainModule(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  inc it.n
  skipParRi it.n
  let isMainModule = IsMain in c.moduleFlags
  let beforeExpr = dest.len
  dest.addParLe(if isMainModule: TrueX else: FalseX, info)
  dest.addParRi()
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc tryExplicitRoutineInst(c: var SemContext; dest: var TokenBuf; syms: Cursor; it: var Item): bool =
  result = false
  let info = syms.info
  let exprStart = dest.len
  # build symchoice first so we can directly add the matching syms:
  dest.add parLeToken(AtX, info)
  dest.add parLeToken(CchoiceX, info)
  var argBuf = createTokenBuf(16)
  swap dest, argBuf
  var argRead = it.n
  while argRead.kind != ParRi:
    semLocalTypeImpl c, dest, argRead, AllowValues
  takeParRi dest, argRead
  swap dest, argBuf
  # XXX investigate this further, seems odd and prevents us from eliminating the swaps:
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
        dest.shrink exprStart
        c.buildErr dest, syms.info, "invalid tag in symchoice: " & pool.tags[syms.tagId]
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
        dest.add symToken(sym, syms.info)
        inc matches
        lastMatch = m
        # mark if routine is suitable for instantiation:
        instLastMatch = routine.kind notin {TemplateY, MacroY} and routine.exported.kind != ParLe
      inc syms
    else:
      dest.shrink exprStart
      c.buildErr dest, syms.info, "invalid token in symchoice: " & $syms.kind
      return
    if nested == 0: break
  dest.addParRi() # close symchoice
  if matches == 0:
    dest.shrink exprStart
    result = false
  elif matches == 1 and c.routine.inGeneric == 0 and instLastMatch:
    # can instantiate single match
    dest.shrink exprStart
    let inst = c.requestRoutineInstance(lastMatch.fn.sym, lastMatch.typeArgs, lastMatch.inferred, info)
    dest.add symToken(inst.targetSym, info)
    it.typ = asRoutine(inst.procType).params
    it.kind = lastMatch.fn.kind
    it.n = argRead
    result = true
  else:
    # multiple matches, leave as subscript of symchoice
    dest.add argBuf
    it.n = argRead
    result = true

proc isSinglePar(n: Cursor): bool =
  var n = n
  inc n
  result = n.kind == ParRi

proc tryBuiltinSubscript(c: var SemContext; dest: var TokenBuf; it: var Item; lhs: Item): bool =
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
    semLocalTypeExpr c, dest, typeItem
    it.typ = typeItem.typ
    return true
  var maybeRoutine = lhs.n
  if maybeRoutine.exprKind in {OchoiceX, CchoiceX}:
    inc maybeRoutine
  if maybeRoutine.kind == Symbol:
    let res = tryLoadSym(maybeRoutine.symId)
    if res.status == LacksNothing and isRoutine(res.decl.symKind):
      # check for explicit generic routine instantiation
      result = tryExplicitRoutineInst(c, dest, lhs.n, it)
      if result: return

proc semBuiltinSubscript(c: var SemContext; dest: var TokenBuf; it: var Item; lhs: Item) =
  # it.n is after lhs, at args
  if tryBuiltinSubscript(c, dest, it, lhs):
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
  semCall c, dest, call, {}, SubscriptCall
  it.typ = call.typ

proc semSubscript(c: var SemContext; dest: var TokenBuf; it: var Item) =
  var n = it.n
  inc n # tag
  var lhsBuf = createTokenBuf(4)
  var lhs = Item(n: n, typ: c.types.autoType)
  semExpr c, lhsBuf, lhs, {KeepMagics}
  it.n = lhs.n
  lhs.n = cursorAt(lhsBuf, 0)
  semBuiltinSubscript(c, dest, it, lhs)

proc semTypedAt(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let expected = it.typ
  takeToken dest, it.n
  let lhsInfo = it.n.info
  var lhs = Item(n: it.n, typ: c.types.autoType)
  semExpr c, dest, lhs
  it.n = lhs.n
  var index = Item(n: it.n, typ: c.types.autoType)
  semExpr c, dest, index
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
      dest.addSubtree t
      var isZero: bool
      case first.kind
      of IntLit:
        isZero = pool.integers[first.intId] == 0
      of UIntLit:
        isZero = pool.uintegers[first.uintId] == 0
      else:
        isZero = true
      if not isZero:
        dest.addSubtree first
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
    c.buildErr dest, lhsInfo, "invalid lhs type for typed index: " & typeToString(typ)
  takeParRi dest, it.n
  commonType c, dest, it, beforeExpr, expected

proc semConv(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  takeToken dest, it.n
  var destType = semLocalType(c, dest, it.n)
  var arg = Item(n: it.n, typ: c.types.autoType)
  var argBuf = createTokenBuf(16)
  semExpr c, argBuf, arg
  it.n = arg.n
  arg.n = cursorAt(argBuf, 0)
  semConvArg(c, dest, destType, arg, info, beforeExpr)
  takeParRi dest, it.n
  let expected = it.typ
  it.typ = destType
  commonType c, dest, it, beforeExpr, expected

proc semDconv(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  takeToken dest, it.n
  var destType = semLocalType(c, dest, it.n)
  var x = Item(n: it.n, typ: c.types.autoType)
  let beforeArg = dest.len
  semExpr c, dest, x
  it.n = x.n

  var isDistinct = false
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(x.typ, isDistinct)
  if not isDistinct:
    shrink dest, beforeExpr
    c.buildErr dest, info, "`dconv` operation only valid for type conversions involving `distinct` types"
  else:
    var arg = Item(n: cursorAt(dest, beforeArg), typ: srcBase)
    var m = createMatch(addr c)
    typematch m, destBase, arg
    endRead dest
    if m.err:
      when defined(debug):
        shrink dest, beforeExpr
        dest.addErrorMsg m
      else:
        c.typeMismatch dest,info, x.typ, destType
    else:
      # distinct type conversions can also involve conversions
      # between different integer sizes or object types and then
      # `m.args` contains these so use them here:
      shrink dest, beforeArg
      dest.add m.args
  it.n = x.n
  takeParRi dest, it.n
  let expected = it.typ
  it.typ = destType
  commonType c, dest, it, beforeExpr, expected

proc semEnumToStr(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  takeToken dest, it.n
  var x = Item(n: it.n, typ: c.types.autoType)

  var exprTokenBuf = createTokenBuf()
  semExpr c, exprTokenBuf, x
  it.n = x.n
  if containsGenericParams(x.typ):
    discard
  else:
    let typeSymId = x.typ.skipModifier.symId
    let typeName = pool.syms[typeSymId]
    let dollorName = "dollar`." & typeName
    let dollorSymId = pool.syms.getOrIncl(dollorName)
    shrink dest, beforeExpr
    dest.add parLeToken(CallX, info)
    dest.add symToken(dollorSymId, info)
  dest.add exprTokenBuf
  takeParRi dest, it.n
  let expected = it.typ
  it.typ = c.types.stringType
  commonType c, dest, it, beforeExpr, expected

proc buildLowValue(c: var SemContext; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo) =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    assert s.status == LacksNothing
    if s.decl.symKind != TypeY:
      c.buildErr dest, typ.info, "cannot get low value of non-type"
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT:
      # first field
      var field = asEnumDecl(decl.body).firstField
      let first = asLocal(field)
      dest.add symToken(first.name.symId, info)
    else:
      c.buildErr dest, info, "invalid type for low: " & typeToString(typ)
  of ParLe:
    case typ.typeKind
    of IntT:
      var bitsCursor = typ
      inc bitsCursor # skip int tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: low(int8).int64
        of 16: low(int16).int64
        of 32: low(int32).int64
        else: low(int64)
      dest.add intToken(pool.integers.getOrIncl(value), info)
      if rawBits != -1:
        dest.add strToken(pool.strings.getOrIncl("i" & $rawBits), info)
        dest.addParRi()
    of UIntT:
      var bitsCursor = typ
      inc bitsCursor # skip uint tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value = 0'u64
      dest.add uintToken(pool.uintegers.getOrIncl(value), info)
      if rawBits != -1:
        dest.add strToken(pool.strings.getOrIncl("u" & $rawBits), info)
        dest.addParRi()
    of CharT:
      dest.add charToken('\0', info)
    of RangetypeT:
      var first = typ
      inc first
      let base = first
      skip first
      dest.addParLe(ConvX, info)
      dest.addSubtree base
      dest.addSubtree first
      dest.addParRi()
    of ArrayT:
      var index = typ
      inc index # tag
      skip index # element
      buildLowValue(c, dest, index, info)
    of BoolT:
      dest.addParLe(FalseX, info)
      dest.addParRi()
    of FloatT:
      dest.addParLe(NegInfX, info)
      dest.addParRi()
    else:
      c.buildErr dest, info, "invalid type for low: " & typeToString(typ)
  else:
    c.buildErr dest, info, "invalid type for low: " & typeToString(typ)

proc buildHighValue(c: var SemContext; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo) =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    assert s.status == LacksNothing
    if s.decl.symKind != TypeY:
      c.buildErr dest, typ.info, "cannot get high value of non-type"
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
      dest.add symToken(last.name.symId, info)
    else:
      c.buildErr dest, info, "invalid type for high: " & typeToString(typ)
  of ParLe:
    case typ.typeKind
    of IntT:
      var bitsCursor = typ
      inc bitsCursor # skip int tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: high(int8).int64
        of 16: high(int16).int64
        of 32: high(int32).int64
        else: high(int64)
      dest.add intToken(pool.integers.getOrIncl(value), info)
      if rawBits != -1:
        dest.add strToken(pool.strings.getOrIncl("i" & $rawBits), info)
        dest.addParRi()
    of UIntT:
      var bitsCursor = typ
      inc bitsCursor # skip uint tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: high(uint8).uint64
        of 16: high(uint16).uint64
        of 32: high(uint32).uint64
        else: high(uint64)
      dest.add uintToken(pool.uintegers.getOrIncl(value), info)
      if rawBits != -1:
        dest.add strToken(pool.strings.getOrIncl("u" & $rawBits), info)
        dest.addParRi()
    of CharT:
      dest.add charToken(high(char), info)
    of RangetypeT:
      var last = typ
      inc last
      let base = last
      skip last
      skip last
      dest.addParLe(ConvX, info)
      dest.addSubtree base
      dest.addSubtree last
      dest.addParRi()
    of ArrayT:
      var index = typ
      inc index # tag
      skip index # element
      buildHighValue(c, dest, index, info)
    of BoolT:
      dest.addParLe(TrueX, info)
      dest.addParRi()
    of FloatT:
      dest.addParLe(InfX, info)
      dest.addParRi()
    else:
      c.buildErr dest, info, "invalid type for high: " & typeToString(typ)
  else:
    c.buildErr dest, info, "invalid type for high: " & typeToString(typ)

proc semLow(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  takeToken dest, it.n
  let typ = semLocalType(c, dest, it.n)
  takeParRi dest, it.n
  if containsGenericParams(typ):
    discard
  else:
    dest.shrink beforeExpr
    buildLowValue(c, dest, typ, info)
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
  commonType c, dest, it, beforeExpr, expected

proc semHigh(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  takeToken dest, it.n
  let typ = semLocalType(c, dest, it.n)
  takeParRi dest, it.n
  if containsGenericParams(typ):
    discard
  else:
    dest.shrink beforeExpr
    buildHighValue(c, dest, typ, info)
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
  commonType c, dest, it, beforeExpr, expected

proc semVoidHook(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let expected = it.typ
  takeToken dest, it.n
  it.typ = c.types.autoType
  semExpr c, dest, it
  if it.n.kind != ParRi:
    # hook has 2nd argument:
    it.typ = c.types.autoType
    semExpr c, dest, it
  takeParRi dest, it.n
  it.typ = c.types.voidType
  commonType c, dest, it, beforeExpr, expected

proc semDupHook(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let expected = it.typ
  takeToken dest, it.n
  it.typ = c.types.autoType
  semExpr c, dest, it
  takeParRi dest, it.n
  it.typ = skipModifier(it.typ)
  commonType c, dest, it, beforeExpr, expected

proc semDeref(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  let expected = it.typ
  takeToken dest, it.n
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, dest, arg
  it.n = arg.n
  takeParRi dest, it.n
  let t = skipModifier(arg.typ)
  case t.typeKind
  of RefT, PtrT:
    it.typ = t
    inc it.typ # get to base type
  else:
    c.buildErr dest, info, "invalid type for deref: " & typeToString(t)
  commonType c, dest, it, beforeExpr, expected

proc semFailed(c: var SemContext; dest: var TokenBuf; it: var Item) =
  # It is not yet clear how this should work.
  let beforeExpr = dest.len
  let expected = it.typ
  takeToken dest, it.n
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, dest, arg
  it.n = arg.n
  takeParRi dest, it.n
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc semAddr(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  let info = it.n.info
  let expected = it.typ
  let beforeArg = dest.len
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, dest, arg
  it.n = arg.n
  takeParRi dest, it.n
  let a = cursorAt(dest, beforeArg)
  if isAddressable(a) or arg.typ.typeKind in {MutT, LentT}:
    endRead dest
  else:
    let asStr = asNimCode(a)
    endRead dest
    dest.shrink beforeArg
    c.buildErr dest, info, "invalid expression for `addr` operation: " & asStr
    dest.addParRi()

  it.typ = ptrTypeOf(c, dest, skipModifier(arg.typ))
  commonType c, dest, it, beforeExpr, expected

proc semSizeof(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let expected = it.typ
  dest.takeToken(it.n)
  # handle types
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  dest.takeParRi(it.n)
  it.typ = c.types.intType
  commonType c, dest, it, beforeExpr, expected

proc whichPass(c: SemContext): PassKind =
  result = if c.phase == SemcheckSignatures: checkSignatures else: checkBody

template toplevelGuard(c: var SemContext; body: untyped) =
  if c.phase == SemcheckBodies:
    body
  else:
    dest.takeTree it.n

template procGuard(c: var SemContext; body: untyped) =
  if c.phase in {SemcheckSignatures, SemcheckBodies}:
    body
  else:
    dest.takeTree it.n

template constGuard(c: var SemContext; body: untyped) =
  if c.phase in {SemcheckSignatures, SemcheckBodies}:
    body
  else:
    dest.takeTree it.n

template pragmaGuard(c: var SemContext; body: untyped) =
  if c.phase in {SemcheckSignatures, SemcheckBodies}:
    body
  else:
    dest.takeTree it.n

proc semAssumeAssert(c: var SemContext; dest: var TokenBuf; it: var Item; kind: StmtKind) =
  let info = it.n.info
  inc it.n
  dest.addParLe(kind, info)
  semBoolExpr c, dest, it.n
  takeParRi dest, it.n

proc semPragmaLine(c: var SemContext; dest: var TokenBuf; it: var Item; isPragmaBlock: bool) =
  case it.n.pragmaKind
  of BuildP:
    let info = it.n.info
    inc it.n
    var args = newSeq[string]()
    while it.n.kind != ParRi:
      if it.n.kind != StringLit:
        buildErr c, dest, it.n.info, "expected `string` but got: " & asNimCode(it.n)
        skip it.n
      else:
        args.add pool.strings[it.n.litId]
        inc it.n

    skipParRi it.n

    if args.len != 2 and args.len != 3:
      buildErr c, dest, info, "build expected 2 or 3 parameters"

    # XXX: Relative paths in makefile are relative to current working directory, not the location of the makefile.
    let curWorkDir = os.getCurrentDir()
    let currentDir = absoluteParentDir(info.getFile)

    # Extract build pragma arguments
    let compileType = args[0]
    var name = replaceSubs(args[1], currentDir, c.g.config).toAbsolutePath(currentDir)
    let customArgs = if args.len == 3: replaceSubs(args[2], currentDir, c.g.config) else: ""

    if not semos.fileExists(name):
      buildErr c, dest, info, "cannot find: " & name
    name = name.toRelativePath(curWorkDir)

    c.toBuild.buildTree TupX, info:
      c.toBuild.addStrLit compileType, info
      c.toBuild.addStrLit name, info
      c.toBuild.addStrLit customArgs, info
  of EmitP:
    semEmit c, dest, it
  of AssumeP:
    semAssumeAssert c, dest, it, AssumeS
  of AssertP:
    semAssumeAssert c, dest, it, AssertS
  of KeepOverflowFlagP:
    if not isPragmaBlock:
      buildErr c, dest, it.n.info, "`keepOverflowFlag` pragma must be used in a pragma block"
    else:
      dest.add parLeToken(KeepOverflowFlagP, it.n.info)
      dest.addParRi()
    skip it.n
  of PluginP:
    dest.add parLeToken(PragmasS, it.n.info)
    dest.add parLeToken(PluginP, it.n.info)
    inc it.n
    if it.n.kind == StringLit:
      if c.routine.inGeneric == 0 and it.n.litId notin c.pluginBlacklist:
        c.pendingModulePlugins.add (it.n.litId, it.n.info)
      dest.add it.n
      inc it.n
    else:
      buildErr c, dest, it.n.info, "expected `string` but got: " & asNimCode(it.n)
      if it.n.kind != ParRi: skip it.n
    takeParRi dest, it.n
    dest.addParRi()
  of PragmaP:
    dest.add parLeToken(PragmasS, it.n.info)
    dest.add parLeToken(PragmaP, it.n.info)
    inc it.n
    let name = takeIdent(it.n)
    if name == StrId(0):
      buildErr c, dest, it.n.info, "expected identifier for pragma"
      takeParRi dest, it.n
      while it.n.kind != ParRi:
        takeTree dest, it.n
    else:
      var buf = createTokenBuf(16)
      dest.add identToken(name, it.n.info)
      takeParRi dest, it.n
      # take remaining pragmas:
      while it.n.kind != ParRi:
        buf.addSubtree it.n
        takeTree dest, it.n
      buf.addParRi() # extra ParRi to make reading easier
      c.userPragmas[name] = buf
    dest.addParRi()
  of PushP:
    var n = it.n
    inc n
    if n.kind == ParRi:
      discard "empty push"
    else:
      c.pragmaStack.add n
    # semcheck push/pop pragmas in both SemcheckSignatures and SemcheckBodies phases
    # so that pushed pragmas works for both procs and variables
    if c.phase == SemcheckBodies:
      skipUntilEnd it.n
    else:
      dest.addParLe PragmasS, it.n.info
      dest.takeToken it.n
      while it.n.kind != ParRi:
        dest.takeTree it.n
      dest.addParRi
  of PopP:
    if c.pragmaStack.len > 0:
      discard c.pragmaStack.pop
    else:
      buildErr c, dest, it.n.info, "{.pop.} without a corresponding {.push.}"
    if c.phase == SemcheckBodies:
      inc it.n
    else:
      dest.addParLe PragmasS, it.n.info
      dest.takeToken it.n
      dest.addParRi
  of PassLP:
    inc it.n
    let start = dest.len
    let s = evalConstStrExpr(c, dest, it.n, c.types.stringType)
    if s != StrId(0):
      dest.shrink start
      c.passL.add pool.strings[s]
    skipParRi it.n
  of PassCP:
    inc it.n
    let start = dest.len
    let s = evalConstStrExpr(c, dest, it.n, c.types.stringType)
    if s != StrId(0):
      dest.shrink start
      c.passC.add pool.strings[s]
    skipParRi it.n
  else:
    buildErr c, dest, it.n.info, "unsupported pragma", it.n
    skip it.n
    while it.n.kind != ParRi: skip it.n

proc semPragmasLine(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  inc it.n
  while it.n.kind != ParRi:
    if it.n.kind == ParLe:
      if it.n.stmtKind in CallKindsS or
          it.n.substructureKind == KvU:
        inc it.n
    semPragmaLine c, dest, it, false
  skipParRi it.n
  producesVoid c, dest, info, it.typ # in case it was not already produced

proc semInclExcl(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  takeToken dest, it.n
  let typeStart = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  let typ = typeToCursor(c, dest, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, dest, op
  if op.typ.typeKind == SetT:
    inc op.typ
  else:
    c.buildErr dest, info, "expected set type"
  semExpr c, dest, op
  it.n = op.n
  takeParRi dest, it.n
  producesVoid c, dest, info, it.typ

proc semInSet(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  let beforeExpr = dest.len
  takeToken dest, it.n
  let typeStart = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  let typ = typeToCursor(c, dest, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, dest, op
  if op.typ.typeKind == SetT:
    inc op.typ
  else:
    c.buildErr dest, info, "expected set type"
  semExpr c, dest, op
  it.n = op.n
  takeParRi dest, it.n
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc semCardSet(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  takeToken dest, it.n
  let typeStart = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  let typ = typeToCursor(c, dest, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, dest, op
  it.n = op.n
  takeParRi dest, it.n
  let expected = it.typ
  it.typ = c.types.intType
  commonType c, dest, it, beforeExpr, expected

proc semPragmaExpr(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  dest.takeToken it.n
  assert it.n.stmtKind == PragmasS
  dest.takeToken it.n
  while it.n.kind != ParRi:
    semPragmaLine c, dest, it, true
  takeParRi dest, it.n
  semStmt(c, dest, it.n, false)
  takeParRi dest, it.n
  producesVoid c, dest, info, it.typ

proc semInstanceof(c: var SemContext; dest: var TokenBuf; it: var Item) =
  type
    State = enum
      NoSubtype
      LacksRtti
      MaybeSubtype
      AlwaysSubtype

  let info = it.n.info
  let beforeExpr = dest.len
  let expected = it.typ
  dest.takeToken(it.n)
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, dest, arg
  it.n = arg.n
  # handle types
  let beforeType = dest.len
  semLocalTypeImpl c, dest, it.n, InLocalDecl
  var ok = MaybeSubtype
  if c.routine.inGeneric == 0:
    let t = cursorAt(dest, beforeType)
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
    dest.endRead()
  dest.takeParRi(it.n)
  case ok
  of MaybeSubtype, AlwaysSubtype:
    discard
  of NoSubtype, LacksRtti:
    let tstr = asNimCode(cursorAt(dest, beforeType))
    dest.endRead()
    dest.shrink beforeExpr
    if ok == NoSubtype:
      c.buildErr dest, info, "type of " & asNimCode(arg.n) & " is never a subtype of " & tstr
    else:
      c.buildErr dest, info, "base type of " & asNimCode(arg.n) & " is " & tstr & " which lacks RTTI and cannot be used in an `of` check"
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc semProccall(c: var SemContext; dest: var TokenBuf; it: var Item) =
  dest.takeToken(it.n)
  semExpr c, dest, it
  dest.takeParRi(it.n)

proc semInternalTypeName(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  takeToken dest, it.n
  let typ = semLocalType(c, dest, it.n)
  if containsGenericParams(typ):
    discard
  else:
    let typeName = pool.syms[typ.symId]
    dest.shrink beforeExpr
    dest.addStrLit typeName, info
  takeParRi dest, it.n
  let expected = it.typ
  it.typ = c.types.stringType
  commonType c, dest, it, beforeExpr, expected

proc semIs(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  let orig = it.n
  inc it.n
  var lhs = Item(n: it.n, typ: c.types.autoType)
  semExpr c, dest, lhs
  it.n = lhs.n
  if lhs.typ.typeKind == TypedescT:
    inc lhs.typ

  let rhs: TypeCursor
  if it.n.exprKind == TupConstrX:
    inc it.n
    rhs = semLocalType(c, dest, it.n)
    while it.n.kind != ParRi:
      skip it.n
    skipParRi it.n
  else:
    rhs = semLocalType(c, dest, it.n)
  skipParRi it.n
  dest.shrink beforeExpr # delete LHS and RHS
  if containsGenericParams(lhs.typ) or containsGenericParams(rhs):
    dest.add orig
    dest.addSubtree lhs.typ
    dest.addSubtree rhs
    dest.addParRi()
  else:
    var m = createMatch(addr c)
    typematch m, rhs, lhs
    if classifyMatch(m) >= SubtypeMatch:
      dest.addParPair(TrueX, info)
    else:
      dest.addParPair(FalseX, info)
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc semTableConstructor(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag]) =
  # we simply transform ``{key: value, key2, key3: value}`` to
  # ``[(key, value), (key2, value2), (key3, value2)]``
  let info = it.n.info
  let orig = it.n
  inc it.n
  var arrayBuf = createTokenBuf(16)
  var singleKeys = newSeq[Cursor]()
  arrayBuf.buildTree BracketX, info:
    while it.n.kind != ParRi:
      if it.n.substructureKind == KvU:
        let kvInfo = it.n.info
        inc it.n
        if singleKeys.len != 0:
          var cur = it.n
          skip cur
          assert cur.kind != ParRi
          for key in singleKeys:
            arrayBuf.buildTree TupX, key.info:
              arrayBuf.copyTree key
              arrayBuf.copyTree cur

          setLen(singleKeys, 0)

        arrayBuf.buildTree TupX, kvInfo:
          arrayBuf.takeTree it.n
          assert it.n.kind != ParRi
          arrayBuf.takeTree it.n
        inc it.n
      else:
        singleKeys.add it.n
        skip it.n

  if singleKeys.len != 0:
    c.buildErr dest, info, "illformed AST: " & asNimCode(orig)

  var item = Item(n: beginRead(arrayBuf), typ: it.typ)
  semBracket c, dest, item, flags
  it.typ = item.typ
  inc it.n

proc semDefer(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  if c.currentScope.kind == ToplevelScope:
    buildErr c, dest, info, "defer statement not supported at top level"
    skip it.n
    it.typ = c.types.voidType
    return

  takeToken dest, it.n
  openScope c
  semStmt c, dest, it.n, false
  closeScope c
  takeParRi dest, it.n
  c.routine.hasDefer = true

proc semExpr(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag] = {}) =
  case it.n.kind
  of IntLit:
    literal c, dest, it, c.types.intType
  of UIntLit:
    literal c, dest, it, c.types.uintType
  of FloatLit:
    literal c, dest, it, c.types.floatType
  of StringLit:
    literal c, dest, it, c.types.stringType
  of CharLit:
    literal c, dest, it, c.types.charType
  of Ident:
    let start = dest.len
    let s = semIdentImpl(c, dest, it.n, it.n.litId, flags)
    semExprSym c, dest, it, s, start, flags
    inc it.n
  of Symbol:
    let start = dest.len
    let s = fetchSym(c, it.n.symId)
    takeToken dest, it.n
    semExprSym c, dest, it, s, start, flags
  of ParLe:
    case exprKind(it.n)
    of QuotedX:
      let start = dest.len
      let s = semQuoted(c, dest, it.n, flags)
      semExprSym c, dest, it, s, start, flags
    of NoExpr:
      case stmtKind(it.n)
      of NoStmt:
        case typeKind(it.n)
        of NoType:
          buildErr c, dest, it.n.info, "expression expected; tag: " & pool.tags[it.n.tag]
          skip it.n
        of ErrT:
          dest.takeTree it.n
        of ObjectT, EnumT, HoleyEnumT, DistinctT, ConceptT:
          buildErr c, dest, it.n.info, "expression expected"
          skip it.n
        of IntT, FloatT, CharT, BoolT, UIntT, VoidT, NiltT, AutoT, SymKindT,
            PtrT, RefT, MutT, OutT, LentT, SinkT, UarrayT, SetT, StaticT, TypedescT,
            TupleT, ArrayT, RangetypeT, VarargsT, UntypedT, TypedT,
            CstringT, PointerT, TypeKindT, OrdinalT, RoutineTypes, ItertypeT:
          # every valid local type expression
          semLocalTypeExpr c, dest, it
        of OrT, AndT, NotT, InvokeT:
          # should be handled in respective expression kinds
          discard
      of ImportasS, StaticstmtS, BindS, MixinS, AsmS:
        buildErr c, dest, it.n.info, "unsupported statement: " & $stmtKind(it.n)
        skip it.n
      of DeferS:
        toplevelGuard c:
          semDefer c, dest, it
      of ProcS:
        procGuard c:
          semProc c, dest, it, ProcY, whichPass(c)
      of FuncS:
        procGuard c:
          semProc c, dest, it, FuncY, whichPass(c)
      of IteratorS:
        procGuard c:
          semProc c, dest, it, IteratorY, whichPass(c)
      of ConverterS:
        procGuard c:
          semProc c, dest, it, ConverterY, whichPass(c)
      of MethodS:
        procGuard c:
          semProc c, dest, it, MethodY, whichPass(c)
      of TemplateS:
        procGuard c:
          semProc c, dest, it, TemplateY, whichPass(c)
      of MacroS:
        procGuard c:
          semProc c, dest, it, MacroY, whichPass(c)
      of WhileS:
        toplevelGuard c:
          semWhile c, dest, it
      of VarS:
        toplevelGuard c:
          semLocal c, dest, it, VarY
      of GvarS:
        toplevelGuard c:
          semLocal c, dest, it, GvarY
      of TvarS:
        toplevelGuard c:
          semLocal c, dest, it, TvarY
      of LetS:
        toplevelGuard c:
          semLocal c, dest, it, LetY
      of GletS:
        toplevelGuard c:
          semLocal c, dest, it, GletY
      of TletS:
        toplevelGuard c:
          semLocal c, dest, it, TletY
      of CursorS:
        toplevelGuard c:
          semLocal c, dest, it, CursorY
      of ResultS:
        toplevelGuard c:
          semLocal c, dest, it, ResultY
      of ConstS:
        constGuard c:
          semLocal c, dest, it, ConstY
      of UnpackdeclS:
        semUnpackDecl c, dest, it
      of StmtsS: semStmtsExpr c, dest, it, false
      of ScopeS: semStmtsExpr c, dest, it, true
      of BreakS:
        toplevelGuard c:
          semBreak c, dest, it
      of ContinueS:
        toplevelGuard c:
          semContinue c, dest, it
      of CallKindsS:
        toplevelGuard c:
          semCall c, dest, it, flags
      of IncludeS: semInclude c, dest, it
      of ImportS: semImport c, dest, it
      of ImportExceptS: semImportExcept c, dest, it
      of FromimportS: semFromImport c, dest, it
      of ExportS: semExport c, dest, it
      of ExportExceptS: semExportExcept c, dest, it
      of AsgnS:
        toplevelGuard c:
          semAsgn c, dest, it
      of DiscardS:
        toplevelGuard c:
          semDiscard c, dest, it
      of IfS:
        toplevelGuard c:
          semIf c, dest, it
      of WhenS:
        semWhen c, dest, it
      of RetS:
        toplevelGuard c:
          semReturn c, dest, it
      of YldS:
        toplevelGuard c:
          semYield c, dest, it
      of TypeS:
        let info = it.n.info
        semTypeSection c, dest, it.n
        producesVoid c, dest, info, it.typ
      of BlockS:
        toplevelGuard c:
          semBlock c, dest, it
      of CaseS:
        toplevelGuard c:
          semCase c, dest, it
      of ForS:
        toplevelGuard c:
          semFor c, dest, it
      of TryS:
        toplevelGuard c:
          semTry c, dest, it
      of RaiseS:
        toplevelGuard c:
          semRaise c, dest, it
      of CommentS:
        # XXX ignored for now
        let info = it.n.info
        skip it.n
        producesVoid c, dest, info, it.typ
      of EmitS:
        pragmaGuard c:
          semEmit c, dest, it
      of PragmasS:
        pragmaGuard c:
          semPragmasLine c, dest, it
      of InclS, ExclS:
        toplevelGuard c:
          semInclExcl c, dest, it
      of AssumeS, AssertS:
        pragmaGuard c:
          semAssumeAssert c, dest, it, it.n.stmtKind
      of UsingS:
        semUsing c, dest, it.n
    of FalseX, TrueX, OvfX:
      literalB c, dest, it, c.types.boolType
    of InfX, NegInfX, NanX:
      literalB c, dest, it, c.types.floatType
    of AndX, OrX, XorX:
      let start = dest.len
      takeToken dest, it.n
      semBoolExpr c, dest, it.n
      semBoolExpr c, dest, it.n
      takeParRi dest, it.n
      let expected = it.typ
      it.typ = c.types.boolType
      commonType c, dest, it, start, expected
    of NotX:
      let start = dest.len
      takeToken dest, it.n
      semBoolExpr c, dest, it.n
      takeParRi dest, it.n
      let expected = it.typ
      it.typ = c.types.boolType
      commonType c, dest, it, start, expected
    of EmoveX:
      takeToken dest, it.n
      semExpr c, dest, it
      takeParRi dest, it.n
    of FailedX:
      semFailed c, dest, it
    of ParX:
      inc it.n
      semExpr c, dest, it
      skipParRi it.n
    of CallX, CmdX, CallStrLitX, InfixX, PrefixX, HcallX:
      toplevelGuard c:
        semCall c, dest, it, flags
    of ProccallX:
      toplevelGuard c:
        semProccall c, dest, it
    of DotX, DdotX:
      toplevelGuard c:
        semDot c, dest, it, flags
    of TupatX:
      toplevelGuard c:
        semTupAt c, dest, it
    of DconvX:
      toplevelGuard c:
        semDconv c, dest, it
    of EqX, NeqX, LeX, LtX, EqSetX, LeSetX, LtSetX:
      semCmp c, dest, it
    of AddX, SubX, MulX, DivX, ModX, BitandX, BitorX, BitxorX, PlusSetX, MinusSetX, MulSetX, XorSetX:
      semTypedBinaryArithmetic c, dest, it
    of AshrX, ShrX, ShlX:
      semShift c, dest, it
    of BitnotX, NegX:
      semTypedUnaryArithmetic c, dest, it
    of DelayX:
      semDelay c, dest, it
    of InSetX:
      semInSet c, dest, it
    of CardX:
      semCardSet c, dest, it
    of BracketX:
      semBracket c, dest, it, flags
    of CurlyX:
      semCurly c, dest, it, flags
    of TupX:
      semTup c, dest, it
    of AconstrX:
      semArrayConstr c, dest, it
    of SetConstrX:
      semSetConstr c, dest, it
    of TupConstrX:
      semTupleConstr c, dest, it
    of SufX:
      semSuf c, dest, it
    of OconstrX, NewobjX:
      semObjConstr c, dest, it
    of NewrefX:
      semNewref c, dest, it
    of DefinedX:
      semDefined c, dest, it
    of DeclaredX:
      semDeclared c, dest, it
    of AstToStrX:
      semAstToStr c, dest, it
    of IsMainModuleX:
      semIsMainModule c, dest, it
    of AtX:
      semSubscript c, dest, it
    of ArrAtX, PatX:
      semTypedAt c, dest, it
    of UnpackX:
      takeToken dest, it.n
      takeParRi dest, it.n
    of FieldsX, FieldpairsX, InternalFieldPairsX:
      takeTree dest, it.n
    of OchoiceX, CchoiceX:
      takeTree dest, it.n
    of HaddrX, HderefX:
      takeToken dest, it.n
      # this is exactly what we need here as these operators have the same
      # type as the operand:
      semExpr c, dest, it
      takeParRi dest, it.n
    of CastX:
      semCast c, dest, it
    of NilX:
      semNil c, dest, it
    of ConvX, HconvX:
      semConv c, dest, it
    of EnumToStrX:
      semEnumToStr c, dest, it
    of DefaultObjX:
      semObjDefault c, dest, it
    of DefaultTupX:
      semTupleDefault c, dest, it
    of DefaultDistinctX:
      semDefaultDistinct c, dest, it
    of LowX:
      semLow c, dest, it
    of HighX:
      semHigh c, dest, it
    of ExprX:
      semStmtsExpr c, dest, it, false
    of DerefX:
      semDeref c, dest, it
    of AddrX:
      semAddr c, dest, it
    of SizeofX:
      semSizeof c, dest, it
    of TypeofX:
      semTypeof c, dest, it
    of DestroyX, CopyX, WasMovedX, SinkhX, TraceX:
      semVoidHook c, dest, it
    of DupX:
      semDupHook c, dest, it
    of ErrX:
      takeTree dest, it.n
    of PragmaxX:
      semPragmaExpr c, dest, it
    of InstanceofX:
      semInstanceof c, dest, it
    of BaseobjX:
      semBaseobj c, dest, it
    of InternalTypeNameX:
      semInternalTypeName c, dest, it
    of IsX:
      semIs c, dest, it
    of TabconstrX:
      semTableConstructor c, dest, it, flags
    of DoX:
      procGuard c:
        semDo c, dest, it, whichPass(c)
    of CompilesX:
      semCompiles c, dest, it
    of CurlyatX, AlignofX, OffsetofX:
      # XXX To implement
      buildErr c, dest, it.n.info, "to implement: " & $exprKind(it.n)
      takeToken dest, it.n
      takeParRi dest, it.n
    of EnvpX:
      bug "frontend should not encounter `envp`"

  of ParRi, EofToken, SymbolDef, UnknownToken, DotToken:
    buildErr c, dest, it.n.info, "expression expected"
    if it.n.kind in {DotToken, UnknownToken}:
      inc it.n


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

proc writeNewDepsFile(c: var SemContext; outfile: string) =
  # Update .s.deps.nif file that doesn't contain modules imported under `when false:`
  # so that Hexer and following phases doesn't read such modules.
  var deps = createTokenBuf(16)
  deps.buildTree StmtsS, NoLineInfo:
    if c.importedModules.len != 0:
      var usedPlugins = false
      deps.buildTree ImportS, NoLineInfo:
        for _, i in c.importedModules:
          if i.fromPlugin.len == 0:
            deps.addStrLit i.path.toAbsolutePath
          else:
            usedPlugins = true
      if usedPlugins:
        for _, i in c.importedModules:
          if i.fromPlugin.len > 0:
            deps.buildTree ImportS, NoLineInfo:
              deps.buildTree PragmaxX, NoLineInfo:
                deps.addStrLit i.path.toAbsolutePath
                deps.buildTree PragmasS, NoLineInfo:
                  deps.buildTree KvU, NoLineInfo:
                    deps.addIdent "plugin"
                    deps.addStrLit i.fromPlugin
    if c.toBuild.len != 0:
      deps.buildTree TagId(BuildIdx), NoLineInfo:
        deps.add c.toBuild
    if c.passL.len != 0:
      deps.buildTree TagId(PassLP), NoLineInfo:
        for i in c.passL:
          deps.addStrLit i
    if c.passC.len != 0:
      deps.buildTree TagId(PassCP), NoLineInfo:
        for i in c.passC:
          deps.addStrLit i
  let depsFile = changeModuleExt(outfile, ".s.deps.nif")
  writeFile deps, depsFile

proc writeOutput(c: var SemContext; dest: TokenBuf; outfile: string) =
  #var b = nifbuilder.open(outfile)
  #b.addHeader "nimony", "nim-sem"
  #b.addRaw toString(dest)
  #b.close()
  writeFile dest, outfile
  let root = dest[0].info
  createIndex outfile, root, true,
    IndexSections(hooks: move c.hookIndexLog,
      converters: move c.converterIndexMap,
      classes: move c.classIndexMap,
      exportBuf: buildIndexExports(c))
  writeNewDepsFile c, outfile

proc phaseX(c: var SemContext; dest: var TokenBuf; n: Cursor; x: SemPhase): TokenBuf =
  assert n.stmtKind == StmtsS
  c.phase = x
  var n = n
  takeToken dest, n
  while n.kind != ParRi:
    semStmt c, dest, n, false
  takeParRi dest, n
  result = move dest
  # clear pragmaStack in case {.pop.} was not called
  c.pragmaStack.setLen(0)

proc getModuleLineInfo(buf: var TokenBuf): PackedLineInfo =
  ## Get the line info from the module's StmtsS tag.
  var n = beginRead(buf)
  assert n.stmtKind == StmtsS
  result = n.info
  endRead(buf)

type
  EnsurePhaseResult* = enum
    PhaseOk,        ## Symbol is now at the required phase
    PhaseCycle,     ## Cyclic dependency detected
    PhaseNotFound   ## Symbol not in prog.mem

proc ensurePhase*(c: var SemContext; symId: SymId; targetPhase: SemPhase): EnsurePhaseResult =
  ## Check if a symbol has been processed to at least targetPhase.
  ## Used for cycle detection during phase 2/3.
  if not prog.mem.hasKey(symId):
    return PhaseNotFound  # Symbol not in mem (external or not yet registered)

  let currentPhase = prog.mem[symId].phase
  if currentPhase >= targetPhase:
    return PhaseOk  # Already at or past target phase

  # Cycle detection: check for InProgress markers
  if currentPhase in {SemcheckSignaturesInProgress, SemcheckBodiesInProgress}:
    return PhaseCycle

  # Symbol not yet at target phase - this is a forward reference
  # The caller should handle this appropriately
  result = PhaseOk

proc loadSymWithPhase*(c: var SemContext; symId: SymId; targetPhase: SemPhase): LoadResult =
  ## Load a symbol, checking for cycles.
  ## For current module symbols in progress, returns cycle error.
  let phaseRes = ensurePhase(c, symId, targetPhase)
  if phaseRes == PhaseCycle:
    return LoadResult(status: LacksOffset)  # Cycle detected
  result = tryLoadSym(symId)

proc semToplevelStmts(c: var SemContext; dest: var TokenBuf; buf: var TokenBuf) =
  ## Iterate over toplevel statements in buf and semcheck each one.
  var n = beginRead(buf)
  assert n.stmtKind == StmtsS
  inc n # skip StmtsS tag
  while n.kind != ParRi:
    semStmt c, dest, n, false
  endRead(buf)

proc phase1(c: var SemContext; dest: var TokenBuf; n: Cursor): (TokenBuf, PackedLineInfo) =
  ## Phase 1: Register toplevel symbols.
  var buf = phaseX(c, dest, n, SemcheckTopLevelSyms)
  let lineInfo = getModuleLineInfo(buf)
  result = (move buf, lineInfo)

proc phase2(c: var SemContext; buf: var TokenBuf; moduleLineInfo: PackedLineInfo): TokenBuf =
  ## Phase 2: Check signatures.
  c.phase = SemcheckSignatures
  result = createTokenBuf()
  result.addParLe(StmtsS, moduleLineInfo)
  semToplevelStmts(c, result, buf)
  result.addParRi()
  c.pragmaStack.setLen(0)

proc phase3(c: var SemContext; buf: var TokenBuf; moduleLineInfo: PackedLineInfo): TokenBuf =
  ## Phase 3: Check bodies.
  c.phase = SemcheckBodies
  result = createTokenBuf()
  result.addParLe(StmtsS, moduleLineInfo)
  semToplevelStmts(c, result, buf)

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

proc instantiateMethodForType(c: var SemContext; dest: var TokenBuf; methodSym, typeInstSym: SymId) =
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
        c.buildErr dest, res.decl.info, "cannot instantiate method " & pool.syms[methodSym] &
          ", cannot infer generic parameter " & pool.syms[name]
        return
      typeArgsBuf.addSubtree inferred[name]
    discard requestRoutineInstance(c, methodSym, typeArgsBuf, inferred, res.decl.info)
  else:
    # method did not match, fine, consider it unavailable for this instance
    discard

proc requestMethods(c: var SemContext; dest: var TokenBuf; s: SymId; decl: Cursor) =
  let decl = asTypeDecl(decl)
  var typevars = decl.typevars
  assert classifyType(c, typevars) == InvokeT
  inc typevars
  assert typevars.kind == Symbol

  let base = typevars.symId

  var instanceMethods = c.methods.getOrDefault(s, @[])
  for m in c.methods.getOrDefault(base, @[]):
    if m notin instanceMethods:
      instantiateMethodForType(c, dest, m, s)
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

proc fromGeneric(dest: var TokenBuf; i: int): SymId =
  var n = cursorAt(dest, i) # at name
  skip n # skip name
  skip n # skip exported
  skip n # pattern
  if n.typeKind == InvokeT:
    result = n.firstSon.symId
  else:
    result = NoSymId
  endRead(dest)

proc findOrigin(dest: var TokenBuf; origin: SymId): int =
  var i = 0
  while i < dest.len:
    if dest[i].kind == SymbolDef and dest[i].symId == origin:
      return i-1 # before name
    inc i
  return -1

proc reorderInnerGenericInstances(c: SemContext; dest: var TokenBuf) =
  #[ Consider:

  proc outer =
    var x = 0
    proc inner[T] = echo x
    inner[int]()

  We instantiate `inner` like any other generic but move it below its generic declaration.
  This ensures proper scoping for lambdalifting to pick up later.
  ]#
  var i = 0
  while i < dest.len:
    if dest[i].stmtKind in {ProcS, FuncS, ConverterS, MethodS, MacroS, IteratorS}:
      inc i
      if dest[i].kind == SymbolDef:
        let origin = fromGeneric(dest, i)
        if origin != NoSymId and origin in c.genericInnerProcs:
          # move to right below the position of the origin
          let originPos = findOrigin(dest, origin)
          assert originPos > 0

          let procDecl = cursorAt(dest, i-1)
          var n = procDecl
          skip n
          let procLen = cursorToPosition(dest,n) - (i-1)
          endRead(dest)

          # Extract the procedure declaration
          var procBuf = createTokenBuf(procLen)
          for j in (i-1)..<(i-1+procLen):
            procBuf.add dest[j]
            dest[j] = dotToken(NoLineInfo) # invalidate

          dest.insert procBuf, originPos
    else:
      inc i

proc semcheckCore(c: var SemContext; dest: var TokenBuf; n0: Cursor) =
  c.currentScope = Scope(tab: initTable[StrId, seq[Sym]](), up: nil, kind: ToplevelScope)

  assert n0.stmtKind == StmtsS
  let path = getFile(n0.info) # gets current module path, maybe there is a better way
  addSelfModuleSym(c, path)

  if {SkipSystem, IsSystem} * c.moduleFlags == {}:
    let systemFile = ImportedFilename(path: stdlibFile("std/system"), name: "system", isSystem: true)
    importSingleFile(c, dest, systemFile, "", ImportFilter(kind: ImportAll), n0.info)

  #echo "PHASE 1"
  var (buf1, moduleLineInfo) = phase1(c, dest, n0)
  #echo "PHASE 2"
  var buf2 = phase2(c, buf1, moduleLineInfo)
  #echo "PHASE 3"
  dest = phase3(c, buf2, moduleLineInfo)

  if c.expanded.len > 0:
    dest.addParLe CommentS, c.expanded[0].info
    dest.add c.expanded
    dest.addParRi()

  instantiateGenerics c, dest
  for val in c.typeInstDecls:
    let s = fetchSym(c, val)
    let res = declToCursor(c, dest, s)
    if res.status == LacksNothing:
      requestHookInstance(c, res.decl)
      requestMethods(c, dest, val, res.decl)
      dest.copyTree res.decl
  instantiateGenericHooks c, dest
  dest.addParRi()

  if reportErrors(dest) == 0:
    var afterSem = move dest
    when true: #defined(enableContracts):
      var moreErrors = analyzeContracts(afterSem)
      if reporters.reportErrors(moreErrors) > 0:
        quit 1
    if c.genericInnerProcs.len > 0:
      reorderInnerGenericInstances(c, afterSem)
    var finalBuf = beginRead afterSem
    dest = injectDerefs(finalBuf)
  else:
    quit 1

proc semcheck*(infiles, outfiles: seq[string]; config: sink NifConfig; moduleFlags: set[ModuleFlag];
               commandLineArgs: sink string; canSelfExec: bool) =
  ## Semantic check one or more modules.
  ## For single modules (len=1), this is the normal case.
  ## For multiple modules, they form a cycle group and are processed together.
  assert infiles.len == outfiles.len
  assert infiles.len > 0

  # For now, only support single module. Cyclic modules need more work.
  if infiles.len > 1:
    quit "cyclic module groups not yet implemented"

  let infile = infiles[0]
  let outfile = outfiles[0]

  var owningBuf = createTokenBuf(300)
  var n0 = setupProgram(infile, outfile, owningBuf)
  var c = SemContext(
    types: createBuiltinTypes(),
    thisModuleSuffix: prog.main.name,
    moduleFlags: moduleFlags,
    g: ProgramContext(config: config),
    phase: SemcheckTopLevelSyms,
    routine: SemRoutine(kind: NoSym),
    commandLineArgs: commandLineArgs,
    canSelfExec: canSelfExec,
    pending: createTokenBuf(),
    executeCall: exprexec.executeCall,
    semStmtCallback: semStmtCallback,
    semGetSize: semGetSize)

  var dest = createTokenBuf()

  for magic in ["typeof", "compiles", "defined", "declared"]:
    c.unoverloadableMagics.incl(pool.strings.getOrIncl(magic))

  while true:
    semcheckCore c, dest, n0
    if c.pendingTypePlugins.len == 0 and c.pendingModulePlugins.len == 0: break
    handleTypePlugins c, dest

  if reportErrors(dest) == 0:
    writeOutput c, dest, outfile
  else:
    quit 1

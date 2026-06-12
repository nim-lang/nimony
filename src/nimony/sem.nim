#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Semantic checking:
## Most important task is to turn identifiers into symbols and to perform
## type checking.

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
import std / [tables, sets, syncio, formatfloat, assertions, strutils, hashes]
from std/os import changeFileExt, getCurrentDir, isAbsolute, absolutePath, normalizedPath
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [symparser, nifindexes, docpaths]
import nimony_model, symtabs, builtintypes, decls, asthelpers,
  programs, sigconcepts, sigmatch, magics, reporters, nifconfig,
  intervals, xints, typeprops,
  semdata, sembasics, semchecks, semconst, semmagics, semimport, templates, sempragmas, semos, expreval, semborrow, enumtostr, derefs, sizeof, renderer,
  semuntyped, vtables_frontend, module_plugins, deferstmts, pragmacanon, exprexec, langmodes,
  features, identstyle, macro_plugin

import contracts_njvl

import ".." / gear2 / modnames
import ".." / models / [tags, nifindex_tags]
when not defined(nimony):
  # The validator currently uses Nim idioms (slice operators, openArray
  # tricks, etc.) that nimony does not implement yet, so the import is
  # gated to host-Nim builds. Once tags_grammar / phase_validator compile
  # under nimony, drop this guard.
  import ".." / validator / phase_validator

proc semStmt*(c: var SemContext; dest: var TokenBuf; n: var Cursor; isNewScope: bool)
proc semStmtBranch(c: var SemContext; dest: var TokenBuf; it: var Item; isNewScope: bool)
proc semConv(c: var SemContext; dest: var TokenBuf; it: var Item)
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
    # Unwrap ExprX by advancing to the last son (the value); do not mutate the tree to StmtsS.
    # `if (let e = f(); e != 0)` should keep ExprX so xelim sees stmt/expr distinction.
    if exprKind(it) == ExprX:
      inc it
      while not isLastSon(it):
        skip it
      # it now points to the last son (the expression); continue unwrapping if needed
    else:
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
    it.into IfS:
      while it.hasMore:
        case it.substructureKind
        of ElifU:
          it.into ElifU:
            skip it, SkipCond # condition
            checkBranch(it)
            skip it
            while it.hasMore: skip it
        of ElseU:
          it.into ElseU:
            checkBranch(it)
            skip it
            while it.hasMore: skip it
        of NoSub, NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU, TypevarU, EfldU, FldU,
           WhenU, TypevarsU, CaseU, OfU, StmtsU, ParamsU, PragmasU, EitherU, JoinU,
           UnpackflatU, UnpacktupU, ExceptU, FinU, UncheckedU, GfldU:
          error "illformed AST: `elif` or `else` inside `if` expected, got ", it
          skip it  # avoid infinite loop on illformed
    # all branches are discardable
    result = true
  of CaseS:
    it.into CaseS:
      skip it, SkipValue # selector
      while it.hasMore:
        case it.substructureKind
        of OfU:
          it.into OfU:
            skip it, SkipValue # ranges
            checkBranch(it)
            skip it
            while it.hasMore: skip it
        of ElifU:
          it.into ElifU:
            skip it, SkipCond # condition
            checkBranch(it)
            skip it
            while it.hasMore: skip it
        of ElseU:
          it.into ElseU:
            checkBranch(it)
            skip it
            while it.hasMore: skip it
        of NoSub, NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU, TypevarU, EfldU, FldU,
           WhenU, TypevarsU, CaseU, StmtsU, ParamsU, PragmasU, EitherU, JoinU,
           UnpackflatU, UnpacktupU, ExceptU, FinU, UncheckedU, GfldU:
          error "illformed AST: `of`, `elif` or `else` inside `case` expected, got ", it
          skip it
    # all branches are discardable
    result = true
  of TryS:
    it.into TryS:
      checkBranch(it)
      while it.hasMore and it.substructureKind == ExceptU:
        it.into ExceptU:
          skip it # `Exception as e` part
          checkBranch(it)
          while it.hasMore: skip it
      # ignore finally part and anything else
      while it.hasMore: skip it
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
          skip decl, SkipName # name
          skip decl, SkipExport # exported
          skip decl # pattern
          skip decl, SkipGenParams # typevars
          skip decl # params
          skip decl # retType
          # decl should now be pragmas (or DotToken for no pragmas):
          let accepted =
            if noreturnOnly: {NoreturnP}
            else: {DiscardableP, NoreturnP}
          if decl.kind == ParLe:
            decl.into:  # (pragmas …)
              while decl.hasMore:
                if pragmaKind(decl) in accepted:
                  while decl.hasMore: skip decl  # mop-up before early-exit
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

proc requestRoutineInstance*(c: var SemContext; origin: SymId;
                            typeArgs: TokenBuf;
                            inferred: Table[SymId, Cursor];
                            info: PackedLineInfo): ProcInstance

proc tryConverterMatch(c: var SemContext; convMatch: var Match; f: TypeCursor, arg: CallArg): bool

type
  TransformedCallSource = enum
    RegularCall, MethodCall,
    DotCall, SubscriptCall, CurlyatCall,
    DotAsgnCall, SubscriptAsgnCall, CurlyatAsgnCall

proc semExpr*(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag] = {})

proc semCall(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag]; source: TransformedCallSource = RegularCall)

proc commonType*(c: var SemContext; dest: var TokenBuf; it: var Item; argBegin: int; expected: TypeCursor) =
  if typeKind(expected) == AutoT:
    return

  var arg = Item(n: cursorAt(dest, argBegin), typ: it.typ)
  var done = false
  if arg.n.exprKind == ErrX:
    # already produced an error, continue with the destination type:
    it.typ = expected
    done = true
  elif typeKind(arg.typ) == VoidT and isNoReturn(arg.n):
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
    var convArg = CallArg(n: arg.n, typ: arg.typ)
    if tryConverterMatch(c, convMatch, expected, convArg):
      # `arg.n` and `convArg.n` are cursors into `dest` (via `cursorAt`
      # earlier and the `=copy` into `convArg`). Each holds an rc ref on
      # the buffer's CursorOwner. Releasing them before we mutate `dest`
      # lets `prepareMutation` take the no-copy fast path; otherwise the
      # `shrink` + `dest.add` below would COW the entire buffer.
      endRead arg.n
      endRead convArg.n
      expectUnique dest
      shrink dest, argBegin
      dest.add parLeToken(HcallX, info)
      dest.add symToken(convMatch.fn.sym, info)
      if convMatch.genericConverter:
        buildTypeArgs(convMatch)
        if convMatch.err:
          # adding type args errored
          buildErr c, dest, info, getErrorMsg(convMatch)
        else:
          let inst = c.requestRoutineInstance(convMatch.fn.sym, convMatch.typeArgs, convMatch.inferred, info)
          dest[dest.len-1].setSymId inst.targetSym
      # ignore refineArgType case, probably environment is generic
      dest.add convMatch.args
      dest.addParRi()
      it.typ = expected
    else:
      endRead arg.n
      endRead convArg.n
      expectUnique dest
      shrink dest, argBegin
      #buildErr c, dest, info, getErrorMsg(m)
      c.typeMismatch dest, info, it.typ, expected
  else:
    endRead arg.n
    expectUnique dest
    shrink dest, argBegin
    if m.refineArgType and cursorAt(m.args, 0).exprKind in CallKinds:
      # empty seq call, semcheck
      var call = Item(n: beginRead(m.args), typ: c.types.autoType)
      semCall c, dest, call, {}
    else:
      dest.add m.args
    it.typ = expected

# -------------------- declare `result` -------------------------

proc declareResult*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo): SymId =
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
  var isField = false
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
      if isField:
        dest.add n
      else:
        let s = n.symId
        let newDef =
          if sc.instSuffix != "":
            # Don't apply instantiation suffix to field names
            newInstSymId(c, s, sc.instSuffix)
          else:
            newSymId(c, s)
        sc.newVars[s] = newDef
        dest.add symdefToken(newDef, n.info)
    of ParLe:
      isField = n.substructureKind in {FldU, GfldU}
      dest.add n
      inc nested
    of ParRi:
      dest.add n
      dec nested
    if nested == 0: break
    inc n

proc produceInvoke(c: var SemContext; dest: var TokenBuf; req: InstRequest;
                   typeVars: Cursor; info: PackedLineInfo) =
  dest.buildTree InvokeT, info:
    dest.add symToken(req.origin, info)
    var typeVars = typeVars
    if typeVars.substructureKind == TypevarsU:
      typeVars.into TypevarsU:
        while typeVars.hasMore:
          if typeVars.symKind == TypevarY:
            var tv = typeVars
            inc tv
            dest.copyTree req.inferred.getOrQuit(tv.symId)
          skip typeVars

proc subsGenericType(c: var SemContext; dest: var TokenBuf; req: InstRequest) =
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

proc semLocalTypeImpl*(c: var SemContext; dest: var TokenBuf; n: var Cursor;
                      context: TypeDeclContext; exported: bool = false; ownerSym: SymId = SymId(0))

proc semLocalType(c: var SemContext; dest: var TokenBuf; n: var Cursor; context = InLocalDecl): TypeCursor =
  let insertPos = dest.len
  semLocalTypeImpl c, dest, n, context
  assert dest.len > insertPos
  result = typeToCursor(c, dest, insertPos)

proc semTypeSection(c: var SemContext; dest: var TokenBuf; n: var Cursor; outerRefOwner: SymId = SymId(0))

proc instantiateType*(c: var SemContext; typ: Cursor; bindings: Table[SymId, Cursor]): Cursor =
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

proc instantiateGenerics*(c: var SemContext; dest: var TokenBuf) =
  while c.procRequests.len > 0:
    # This way with `move` ensures it is safe even though
    # the semchecking of generics can add to `c.procRequests`.
    # This is subtle!
    let procReqs = move(c.procRequests)
    for p in procReqs: instantiateGenericProc c, dest, p

proc instantiateGenericHooks*(c: var SemContext; dest: var TokenBuf) =
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

proc fetchSym*(c: var SemContext; s: SymId): Sym =
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

proc semStmtsExpr(c: var SemContext; dest: var TokenBuf; it: var Item; isNewScope: bool) =
  let before = dest.len
  dest.add it.n
  it.n.into:
    while it.n.hasMore:
      if not isLastSon(it.n):
        semStmt c, dest, it.n, false
      else:
        semExpr c, dest, it
  dest.addParRi()
  let kind =
    if classifyType(c, it.typ) in {VoidT, AutoT}:
      (if isNewScope: ScopeTagId else: StmtsTagId)
    else: ExprTagId
  setTag(dest[before], TagId(kind))

proc semStmt*(c: var SemContext; dest: var TokenBuf; n: var Cursor; isNewScope: bool) =
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

proc semStmtCallback*(c: var SemContext; dest: var TokenBuf; n: Cursor) =
  var n = n
  let oldPhase = c.phase
  c.phase = SemcheckBodies
  semStmt c, dest, n, false
  c.phase = oldPhase

proc forceInstantiateCallback*(c: var SemContext; dest: var TokenBuf) =
  ## Force instantiation of pending generic procs. Used by exprexec to ensure
  ## generic bodies are available before compile-time evaluation.
  instantiateGenerics(c, dest)

proc semGetSize*(c: var SemContext; n: Cursor; strict=false): xint =
  getSize(n, c.g.config.bits div 8, strict)

proc sameIdent(sym: SymId; str: StrId): bool =
  # XXX speed this up by using the `fieldCache` idea
  var name = pool.syms[sym]
  extractBasename(name)
  result = pool.strings.getOrIncl(name) == str

proc sameIdent(a, b: SymId): bool =
  # not used yet
  # XXX speed this up by using the `fieldCache` idea
  var x = pool.syms[a]
  extractBasename(x)
  var y = pool.syms[b]
  extractBasename(y)
  result = x == y

proc requestRoutineInstance*(c: var SemContext; origin: SymId;
                            typeArgs: TokenBuf;
                            inferred: Table[SymId, Cursor];
                            info: PackedLineInfo): ProcInstance =
  let key = typeToCanon(typeArgs, 0)
  var targetSym = c.instantiatedProcs.getOrDefault((origin, key))
  if targetSym == SymId(0):
    # Use the `.I<hash>.<mod>` instantiation naming convention (same as
    # type instantiations via `newInstSymId`). Mixing in plain `newSymId`
    # produced names like `@.0.<userMod>` that are indistinguishable from
    # regular module-local procs, so `isInstantiation` can't detect them
    # and downstream consumers (e.g. `exprexec.collectUsedSymsFromExpr`)
    # can't tell the body-less stub apart from a real local proc.
    #
    # Hash the canonical instantiation `(invok origin typeArgs)`, not the bare
    # `typeArgs`: two distinct generic routines that share a base name and are
    # instantiated with identical args — e.g. `Table.[]=` and `Tracker.[]=` over
    # `[SymId, HashSet[int]]` — would otherwise both mint `[]=.0.I<hash>.<mod>`
    # and collide in codegen. `newInstSymId` keeps only the base name
    # (`removeModule(origin)`), so the routine identity must enter through the
    # suffix. This mirrors the type-instance path, whose suffix is hashed over
    # the `(head args)` invocation (and so already distinguishes heads), and the
    # `(invok …)` shape is the same one written into the signature's pattern
    # below, from which the origin remains recoverable for DCE.
    var instKey = createTokenBuf(typeArgs.len + 3)
    instKey.buildTree InvokeT, info:
      instKey.add symToken(origin, info)
      instKey.add typeArgs
    let instSuffix = instToSuffix(instKey, 0)
    let targetSym = newInstSymId(c, origin, instSuffix)
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
    var newInferred = initTable[SymId, Cursor]()
    var typevar = decl.typevars
    var typeArg = cursorAt(signature, typeArgsStart)
    typevar.into:  # (typevars …)
      while typevar.hasMore:
        assert typeArg.hasMore
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
    IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, EnumT, HoleyEnumT, AnumT}

  var srcType = skipModifier(arg.typ)

  # Check if arg contains a symchoice that needs resolution for enum types
  if arg.n.exprKind in {OchoiceX, CchoiceX}:
    # Try to resolve the symchoice based on the destination type
    var destSym = destType
    if destSym.kind == Symbol:
      let destSymId = destSym.symId
      let impl = typeImpl(destSymId)
      if impl.typeKind in {EnumT, HoleyEnumT, AnumT}:
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
      # retag the wrapping `(conv ...)` as `(dconv ...)` so later phases
      # (e.g. derefs.nim) recognize it as a distinct conversion, which is
      # lvalue-preserving and therefore passable to `var T` parameters.
      if dest[beforeExpr].exprKind == ConvX:
        dest[beforeExpr] = parLeToken(DconvX, dest[beforeExpr].info)
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
  const IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, PointerT, CstringT,
                         RefT, PtrT, NiltT, EnumT, HoleyEnumT, AnumT}
  result = t.typeKind in IntegralTypes or isEnumType(t)

proc semCast(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  takeToken dest, it.n
  let destType = semLocalType(c, dest, it.n)
  var x = Item(n: it.n, typ: c.types.autoType)
  # XXX Add hderef here somehow
  let beforeArg = dest.len
  semExpr c, dest, x
  it.n = x.n
  takeParRi dest, it.n

  var srcType = skipModifier(x.typ)

  # distinct type conversion?
  var isDistinct = false
  # also skips to type body for symbols:
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)
  if dest[beforeArg].exprKind == ErrX:
    # already produced an error, continue with the destination type:
    it.typ = destType
  elif sameTrees(destBase, srcBase):
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

include semcompat
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
    var arg = invokeArgs
    typevar.into TypevarsU:
      while arg.hasMore:
        let tv = asLocal(typevar)
        assert tv.kind == TypevarY
        result[tv.name.symId] = arg
        skip typevar
        skip arg
      while typevar.hasMore: skip typevar  # mop-up if arg ran out first

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
    while arg.hasMore:
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
  skip n, SkipType # skip basetype
  var iter = initObjFieldIter()
  while nextField(iter, n):
    let isGuarded = n.substructureKind == GfldU
    inc n # skip FldU/GfldU
    if n.kind == SymbolDef and sameIdent(n.symId, name):
      let symId = n.symId
      inc n # skip name
      let exported = n.kind != DotToken
      skip n, SkipExport # export marker
      skip n, SkipPragmas # pragmas
      var typ = n
      if bindings.len != 0:
        # fields in generic type AST contain generic params of the type
        # for invoked object types, bindings are built from the given arguments
        # and the field type is instantiated based on them here
        typ = instantiateType(c, typ, bindings)
      return ObjField(sym: symId, level: level, typ: typ, exported: exported, guarded: isGuarded, rootOwner: SymId(0))
    skip n, SkipName # skip name
    skip n, SkipExport # skip export marker
    skip n, SkipPragmas # skip pragmas
    skip n, SkipType # type
    skip n, SkipValue # value
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

proc findObjFieldConsiderVis(c: var SemContext; decl: TypeDecl; name: StrId;
                              bindings: Table[SymId, Cursor];
                              bypassVis = false): ObjField =
  let impl = decl.objBody
  result = findObjFieldAux(c, impl, name, bindings)
  if c.routine.inInst == 0 and not bypassVis:
    # only check visibility during first semcheck, unless the input NIF
    # already carried an access-token proving the access was hygiene-checked
    # at a site that had visibility (e.g. a template body semchecked in the
    # field's owner module).
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
  elif bypassVis and result.level == 0:
    result.rootOwner = genericRootSym(decl)

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
  var f = decl.body
  f.into:
    skip f, SkipType
    if decl.kind == AnumT:
      skip f, AnyType
    while f.hasMore:
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
          let field = findObjFieldConsiderVis(c, decl, fieldName, bindings,
                                              bypassVis = BypassFieldVis in flags)
          if field.level >= 0 and field.guarded and c.inUncheckedAccess == 0 and
              BypassGuardedCheck notin flags:
            dest.shrink exprStart
            c.buildErr dest, info,
              "field '" & pool.strings[fieldName] & "' can only be accessed in a pattern matching `case` branch"
            result = InvalidDot
          elif field.level >= 0:
            result = MatchedDotField
            if doDeref:
              dest[exprStart] = parLeToken(DdotX, info)
            dest.add symToken(field.sym, info)
            dest.add intToken(pool.integers.getOrIncl(field.level), info)
            if not field.exported:
              # Emit the access-token string lit so later re-checks (template
              # expansion, generic instantiation, exprexec serialization) see
              # that this access was hygiene-checked with visibility and must
              # be accepted even if the consumer module cannot see the field.
              dest.addStrLit("x", info)
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
      var i = 0
      tup.into:  # (tuple …)
        while tup.hasMore:
          var field = asTupleField(tup)
          if field.kind == KvU:
            let name = takeIdent(field.name)
            if name == fieldName:
              dest[exprStart] = parLeToken(TupatX, info)
              dest.addIntLit(i, info)
              it.typ = field.typ # will be fit later with commonType
              result = MatchedDotField
              while tup.hasMore: skip tup  # mop-up before exit
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
      if tval.typeKind in {EnumT, OnumT, AnumT}:
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
  # optional access-token StringLit — `"x"` — certifies this dot expression
  # was already type-checked with visibility in the field's owner module.
  var innerFlags = flags
  if fieldNameCursor.kind == Symbol:
    innerFlags.incl BypassGuardedCheck
  if it.n.kind == StringLit:
    innerFlags.incl BypassFieldVis
    inc it.n
  skipParRi it.n
  # now interpret the dot expression:
  let state = tryBuiltinDot(c, dest, it, lhs, fieldName, info, innerFlags)
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

proc handleExportMarker(c: var SemContext; dest: var TokenBuf; n: var Cursor): bool =
  result = false
  if n.kind == DotToken:
    dest.add n
    inc n
  elif n.kind == Ident and pool.strings[n.litId] == "x":
    if c.currentScope.kind != ToplevelScope:
      buildErr c, dest, n.info, "only toplevel declarations can be exported"
    else:
      result = true
      dest.add n
    inc n
  elif n.kind == ParLe:
    # export marker could have been turned into a NIF tag
    takeTree dest, n
    result = true
  else:
    buildErr c, dest, n.info, "expected '.' or 'x' for an export marker"

proc wantExportMarker(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  discard handleExportMarker(c, dest, n)

proc insertType(c: var SemContext; dest: var TokenBuf; typ: TypeCursor; patchPosition: int) =
  let t = skipModifier(typ)
  dest.insert t, patchPosition

proc patchType(c: var SemContext; dest: var TokenBuf; typ: TypeCursor; patchPosition: int) =
  let t = skipModifier(typ)
  dest.replace t, patchPosition

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
        if cast[TagEnum](tag) == IsmainmoduleTagId:
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
  of NoType, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT, TemplateT,
     ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, ArrayT, VarargsT,
     StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
     DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, SymkindT, TypekindT, UntypedT, TypedT,
     CstringT, PointerT, OrdinalT:
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
        if magic in {ArrayT, SetT, RangetypeT, EnumT, HoleyEnumT, AnumT}:
          var typeclassBuf = createTokenBuf(4)
          typeclassBuf.addParLe(TypekindT, info)
          typeclassBuf.addParLe(magic, info)
          typeclassBuf.addParRi()
          typeclassBuf.addParRi()
          replace(dest, cursorAt(typeclassBuf, 0), start)
        elif magic in {CstringT, PointerT} and LenientNilsFeature notin c.features:
          # add default notnil for pointer-like magic types:
          dest.shrink dest.len - 1 # remove ParRi
          dest.addParPair NotnilU, info
          dest.addParRi()
    elif res.status == LacksNothing:
      let typ = asTypeDecl(res.decl)
      if isGeneric(typ) or isNominal(typ.body.typeKind):
        # types that should stay as symbols, see sigmatch.matchSymbol
        # but see if it triggers a module plugin:
        let p = extractPragma(typ.pragmas, PluginP)
        if p != default(Cursor):
          var path = StrId(0)
          var pathInfo = p.info
          if p.kind == StringLit:
            path = p.litId
            pathInfo = p.info
          if path != StrId(0) and path notin c.pluginBlacklist:
            c.pendingTypePlugins[s.name] = PluginObj(path: path, info: pathInfo)
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

type
  WhenMode = enum
    NormalWhen
    ObjectWhen

  SemObjectState = object
    isExported: bool
    isAnum: bool
    guarded: bool # true when inside a case with DotToken selector (sum type)
    ownerSym: SymId

proc semWhenImpl(c: var SemContext; dest: var TokenBuf; it: var Item; mode: WhenMode;
                 state: var SemObjectState)

type CaseMode = enum
  NormalCase
  ObjectCase

proc semCaseImpl(c: var SemContext; dest: var TokenBuf; it: var Item; mode: CaseMode;
                 state: var SemObjectState)

proc semExprMissingPhases(c: var SemContext; dest: var TokenBuf; it: var Item; firstPhase: SemPhase) =
  # Only consider "real" phases, not InProgress markers
  const realPhases = [SemcheckTopLevelSyms, SemcheckSignatures, SemcheckBodies]
  if c.phase <= firstPhase:
    var lastBuf = default(TokenBuf)
    var usingBuf = false
    for ph in realPhases:
      if ph >= c.phase:
        break
      var buf = createTokenBuf()
      var phase = ph
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
    varargsParam.addParRi(info)
    dest.insert fromBuffer(varargsParam), paramsAt+1
  else:
    var n = cursorAt(dest, paramsAt)
    if n.substructureKind == ParamsU:
      inc n
      while n.hasMore:
        if n.symKind == ParamY:
          inc n
          let lit = takeIdent(n)
          if lit != StrId(0) and pool.strings[lit] == vanon:
            # already added:
            endRead(dest)
            return
          while n.hasMore: skip n
          consumeParRi n
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
  if modeTok.kind == ParLe and modeTok.tagId == nifstreams.ErrT:
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

proc semCaseOfValueImpl(c: var SemContext; dest: var TokenBuf; it: var Item; selectorType: TypeCursor;
                    seen: var seq[(xint, xint)])

proc evalConstCaseBranch(c: var SemContext; dest: var TokenBuf; it: var Item; expected: TypeCursor; seen: var seq[(xint, xint)]; info: PackedLineInfo) =
  let info = it.n.info
  var orig = it.n

  var ignored = createTokenBuf()
  var valueBuf = evalConstExpr(c, ignored, it.n, c.types.autoType)

  var value = beginRead(valueBuf)
  case value.exprKind
  of SetconstrX:
    # `evalExpr` has already lowered each enum-field reference to its raw
    # ordinal, so the set's elements are plain int literals. Re-semchecking
    # them through `semCaseOfValueImpl` would try to convert `int -> E` and
    # fail with a spurious type mismatch whose info points back into the
    # imported enum's declaration. Take the ordinals directly, emit them as
    # case labels into `dest`, and track them in `seen`.
    inc value # skip (setconstr
    skip value # skip element type
    while value.hasMore:
      if value.substructureKind == RangeU:
        let rInfo = value.info
        var r = value
        inc r
        let a = getConstOrdinalValue(r); skip r
        let b = getConstOrdinalValue(r); skip r
        if a.isNaN or b.isNaN:
          buildErr c, dest, rInfo, "expected constant ordinal value"
        else:
          if seen.doesOverlapOrIncl(a, b):
            buildErr c, dest, rInfo, "overlapping values"
          dest.takeTree value
      else:
        let x = getConstOrdinalValue(value)
        let vInfo = value.info
        if x.isNaN:
          buildErr c, dest, vInfo, "expected constant ordinal value"
          skip value
        else:
          if seen.containsOrIncl(x):
            buildErr c, dest, vInfo, "value already handled"
          dest.takeTree value
  of NoExpr, ErrX, SufX, AtX, DerefX, DotX, PatX, ParX, AddrX, NilX, InfX, NeginfX, NanX,
     FalseX, TrueX, AndX, OrX, XorX, NotX, NegX, SizeofX, AlignofX, OffsetofX, KvX, OconstrX,
     AconstrX, BracketX, CurlyX, CurlyatX, OvfX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX,
     BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CastX, ConvX, CallX, CmdX,
     CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX, DdotX, HaddrX, NewrefX, NewobjX, TupX,
     TupconstrX, TabconstrX, AshrX, BaseobjX, HconvX, DconvX, CallstrlitX, InfixX,
     PrefixX, HcallX, CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX, ProccallX, HighX,
     LowX, TypeofX, UnpackX, FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, DefaultobjX,
     DefaulttupX, DefaultdistinctX, DelayX, Delay0X, SuspendX, ExprX, DoX, ArratX, TupatX,
     PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
     DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, InternalTypeNameX, InternalFieldPairsX,
     FailedX, IsX, EnvpX:
    let a = evalConstIntExpr(c, dest, orig, expected)
    if seen.containsOrIncl(a):
      buildErr c, dest, info, "value already handled"

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
        var thisProc = asRoutine(n)
        # Closure iterators surface as a first-class `itertype` value. Same
        # shape as proctype (nilTag, params, retType, pragmas) — sigmatch /
        # nifcgen route both through the same paths.
        let isClosureIter =
          s.kind == IteratorY and hasPragma(thisProc.pragmas, ClosureP)
        var procTypeBuf = createTokenBuf()
        procTypeBuf.addParLe (if isClosureIter: ItertypeT else: ProctypeT)
        procTypeBuf.addDotToken() # nilability tag (none — `nil`/`notnil` set later)
        procTypeBuf.addSubtree thisProc.params
        procTypeBuf.addSubtree thisProc.retType
        procTypeBuf.addSubtree thisProc.pragmas
        procTypeBuf.addParRi() # end of (proc|iter)type
        n = beginRead(procTypeBuf)
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
    while it.n.hasMore:
      # arguments of the subscript
      callBuf.takeTree it.n
    skipParRi it.n # end subscript expression
    callBuf.takeTree it.n # assignment value
    callBuf.addParRi()
    skipParRi it.n # end assignment
    var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
    semCall c, dest, call, {}, SubscriptAsgnCall
    it.typ = call.typ

proc semCurlyatAsgn(c: var SemContext; dest: var TokenBuf; it: var Item; info: PackedLineInfo) =
  # `{}=` has no builtin meaning; always rewrite to a call to `{}=`:
  inc it.n # tag of curlyat
  var lhsBuf = createTokenBuf(4)
  var lhs = Item(n: it.n, typ: c.types.autoType)
  semExpr c, lhsBuf, lhs, {KeepMagics}
  it.n = lhs.n
  lhs.n = cursorAt(lhsBuf, 0)
  var callBuf = createTokenBuf(16)
  callBuf.addParLe(CallX, info)
  callBuf.add identToken(pool.strings.getOrIncl("{}="), info)
  callBuf.addSubtree lhs.n
  while it.n.hasMore:
    # arguments of the curly subscript:
    callBuf.takeTree it.n
  skipParRi it.n # end curlyat expression
  callBuf.takeTree it.n # assignment value
  callBuf.addParRi()
  skipParRi it.n # end assignment
  var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
  semCall c, dest, call, {}, CurlyatAsgnCall
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
  var dotFlags: set[SemFlag] = {}
  if dot.n.kind == StringLit:
    dotFlags.incl BypassFieldVis
    inc dot.n
  skipParRi dot.n
  var dotBuf = createTokenBuf(8)
  let builtin = tryBuiltinDot(c, dotBuf, dot, dotLhs, fieldName, dotInfo,
                               dotFlags) != FailedDot
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
  of CurlyatX:
    semCurlyatAsgn c, dest, it, info
  of DotX, DdotX:
    semDotAsgn c, dest, it, info
  of NoExpr, ErrX, SufX, DerefX, PatX, ParX, AddrX, NilX, InfX, NeginfX, NanX,
     FalseX, TrueX, AndX, OrX, XorX, NotX, NegX, SizeofX, AlignofX, OffsetofX, KvX, OconstrX,
     AconstrX, BracketX, CurlyX, OvfX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX,
     BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CastX, ConvX, CallX, CmdX,
     CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX, HaddrX, NewrefX, NewobjX, TupX,
     TupconstrX, SetconstrX, TabconstrX, AshrX, BaseobjX, HconvX, DconvX, CallstrlitX, InfixX,
     PrefixX, HcallX, CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX, ProccallX, HighX,
     LowX, TypeofX, UnpackX, FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, DefaultobjX,
     DefaulttupX, DefaultdistinctX, DelayX, Delay0X, SuspendX, ExprX, DoX, ArratX, TupatX,
     PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
     DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, InternalTypeNameX, InternalFieldPairsX,
     FailedX, IsX, EnvpX:
    dest.addParLe(AsgnS, info)
    var a = Item(n: it.n, typ: c.types.autoType)
    let beforeLhs = dest.len
    semExpr c, dest, a # infers type of `left-hand-side`
    if dest[beforeLhs].exprKind == ErrX:
      a.typ = c.types.autoType
    else:
      removeModifier(a.typ) # remove `var` for rhs
    semExpr c, dest, a # ensures type compatibility with `left-hand-side`
    it.n = a.n
    takeParRi dest, it.n
    producesVoid c, dest, info, it.typ

proc semEmit*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  dest.add parLeToken(EmitS, info)
  inc it.n
  if it.n.exprKind == BracketX:
    it.n.into:
      while it.n.hasMore:
        var a = Item(n: it.n, typ: c.types.autoType)
        semExpr c, dest, a
        it.n = a.n
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
    let start = dest.len
    semExpr c, dest, it
    # A branch that doesn't yield a value (`return`/`raise`/`break`/...)
    # shouldn't pin the surrounding expression's type to void — leave it
    # as `auto` so a sibling branch can still determine the result type.
    if classifyType(c, it.typ) == VoidT:
      let ex = cursorAt(dest, start)
      let nr = isNoReturn(ex)
      endRead(dest)
      if nr:
        it.typ = c.types.autoType
  of VoidT:
    # performs discard check:
    semStmt c, dest, it.n, isNewScope
  else:
    var ex = Item(n: it.n, typ: it.typ)
    let start = dest.len
    semExpr c, dest, ex
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
  result = semLocalType(c, dest, it.n)
  # Allow any exception type - validation for "x != default(T)" semantics can come later

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
    inc c.routine.inExcept
    semStmtBranch c, dest, it, true
    dec c.routine.inExcept
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

proc semWhenImpl(c: var SemContext; dest: var TokenBuf; it: var Item; mode: WhenMode;
                 state: var SemObjectState) =
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
            semObjectComponent c, dest, it.n, state
          skipParRi it.n # finish elif
          while it.n.hasMore: skip it.n
          consumeParRi it.n
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
        semObjectComponent c, dest, it.n, state
      skipParRi it.n # finish else
      while it.n.hasMore: skip it.n
      consumeParRi it.n
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
  # dummy value, not used for when statements, only for object types:
  var state = SemObjectState(isExported: false, isAnum: false)
  semWhenImpl(c, dest, it, NormalWhen, state)
  dec c.inWhen

proc semCaseOfValueImpl(c: var SemContext; dest: var TokenBuf; it: var Item; selectorType: TypeCursor;
                    seen: var seq[(xint, xint)]) =
  while it.n.hasMore:
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
    while it.n.hasMore:
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
    if impl.kind == TypeY and impl.body.typeKind in {EnumT, HoleyEnumT, AnumT}:
      typ = impl.body
      break

  if typ.typeKind != HoleyEnumT:
    # quick check based on the `total` count:
    if total == lengthOrd(c, selectorType):
      return

  if typ.typeKind in {EnumT, HoleyEnumT, AnumT}:
    # check if all values are handled:
    let edecl = asEnumDecl(typ)
    var field = edecl.body
    var missing = ""
    field.into:
      skip field, SkipType
      if edecl.kind == AnumT:
        skip field, AnyType
      while field.hasMore:
        let f = takeLocal(field, SkipFinalParRi)
        let v: xint
        var vnode = f.val.firstSon # skip tuple tag
        case vnode.kind
        of IntLit:
          v = createXint pool.integers[vnode.intId]
        of UIntLit:
          v = createXint pool.uintegers[vnode.uintId]
        else:
          var dummyDest = createTokenBuf(4)
          v = semEnumOrdinalValue(c, dummyDest, vnode)
        if not seen.contains(v):
          if missing.len > 0: missing.add ", "
          var isGlobal = false
          missing.add extractBasename(pool.syms[f.name.symId], isGlobal)
    if missing.len > 0:
      buildErr c, dest, info, "not all cases are covered; missing: {" & missing & "}"
  else:
    buildErr c, dest, info, "not all cases are covered"

type
  SumTypeInfo = object
    valid: bool
    discrimSym: SymId
    discrimType: Cursor
    isRef: bool
    objTypeSym: SymId
    invokeArgs: Cursor # non-default if the selector is a generic instantiation

proc findSumTypeInfo(selectorType: TypeCursor): SumTypeInfo =
  result = SumTypeInfo(valid: false)
  var t = skipModifier(selectorType)
  var isRef = false
  if t.typeKind in {RefT, PtrT}:
    isRef = true
    inc t
  let invokeArgs = skipInvoke(t)
  if t.kind != Symbol: return
  let typeSym = t.symId
  let res = tryLoadSym(typeSym)
  if res.status != LacksNothing: return
  let decl = asTypeDecl(res.decl)
  if decl.kind != TypeY: return
  var body = decl.body
  if body.typeKind in {RefT, PtrT}:
    isRef = true
    inc body
    discard skipInvoke(body)
    # Follow symbol to inner object type for ref/ptr objects:
    if body.kind == Symbol:
      let innerRes = tryLoadSym(body.symId)
      if innerRes.status != LacksNothing: return
      let innerDecl = asTypeDecl(innerRes.decl)
      if innerDecl.kind != TypeY: return
      body = innerDecl.body
  if body.typeKind != ObjectT: return
  let obj = asObjectDecl(body)
  var field = obj.body
  inc field  # past (object
  skip field, AnyType  # parent type / inheritance slot
  while field.hasMore and field.substructureKind == FldU:
    skip field
  if not field.hasMore or field.substructureKind != CaseU: return
  inc field  # past (case
  if not field.hasMore or field.substructureKind != FldU: return
  let fld = asLocal(field)
  var discrimType = fld.typ
  if discrimType.kind == Symbol:
    let typeRes = tryLoadSym(discrimType.symId)
    if typeRes.status == LacksNothing:
      let td = asTypeDecl(typeRes.decl)
      if td.body.typeKind == AnumT:
        result = SumTypeInfo(valid: true, discrimSym: fld.name.symId,
                             discrimType: fld.typ, isRef: isRef,
                             objTypeSym: typeSym, invokeArgs: invokeArgs)

type
  SumTypeBranchField = object
    sym: SymId
    typ: TypeCursor

proc findBranchFields(objTypeSym: SymId; efldSym: SymId): seq[SumTypeBranchField] =
  ## Find the fields belonging to the branch identified by `efldSym`.
  ## Matches by name so it works across generic instantiations where
  ## the efld syms differ from the original definition.
  result = @[]
  let res = tryLoadSym(objTypeSym)
  if res.status != LacksNothing: return
  var decl = asTypeDecl(res.decl)
  if decl.kind != TypeY: return
  var body = decl.body
  if body.typeKind in {RefT, PtrT}:
    inc body
    discard skipInvoke(body)
    # Follow symbol to inner object type:
    if body.kind == Symbol:
      decl = asTypeDecl(tryLoadSym(body.symId).decl)
      if decl.kind != TypeY: return
      body = decl.body
  if body.typeKind != ObjectT: return
  let obj = asObjectDecl(body)
  var n = obj.body
  inc n  # past (object
  skip n  # parent type / inheritance slot
  while n.substructureKind in {FldU, GfldU}:
    skip n
  if n.substructureKind != CaseU: return
  inc n
  skip n # skip discriminator fld
  let efldName = symToIdent(efldSym)
  while n.hasMore:
    if n.substructureKind == OfU:
      inc n
      var found = false
      if n.substructureKind == RangesU:
        var scan = n
        inc scan
        while scan.hasMore:
          if scan.kind == Symbol and sameIdent(scan.symId, efldName):
            found = true
            break
          skip scan
      skip n # skip ranges
      if found and n.substructureKind == StmtsU:
        inc n
        while n.hasMore:
          if n.substructureKind in {FldU, GfldU}:
            let f = asLocal(n)
            result.add SumTypeBranchField(sym: f.name.symId, typ: f.typ)
          skip n
        return
      else:
        skip n # skip stmts
      inc n # skip of parri
    else:
      skip n

proc getEfldOrdinal(sym: SymId): xint =
  let res = tryLoadSym(sym)
  if res.status == LacksNothing and res.decl.substructureKind == EfldU:
    var n = res.decl
    skipToLocalType n
    skip n # skip type → now at value
    if n.kind == ParLe: # TupX
      inc n
      if n.kind == IntLit:
        return createXint pool.integers[n.intId]
  return createNaN()

type
  SumTypeBinding = object
    ident: StrId
    fieldSym: SymId
    fieldType: TypeCursor
    info: PackedLineInfo

proc findOneofEfld(c: var SemContext; name: StrId): SymId =
  result = SymId(0)
  let ignoreStyle = IgnoreStyleFeature in c.features
  var scope = c.currentScope
  while scope != nil:
    for k in stylesOfScope(scope, name, ignoreStyle):
      for sym in scope.tab.getOrDefault(k):
        if sym.kind == EfldY and isAnumEfld(sym.name):
          return sym.name
    scope = scope.up
  for realName in stylesOfImport(c.importTab, name, ignoreStyle):
    for moduleSym in c.importTab.getOrQuit(realName):
      let iface = addr c.importedModules.getOrQuit(moduleSym).iface
      for foreignName in stylesOfIface(iface[], realName, ignoreStyle):
        let candidates = iface[].getOrDefault(foreignName)
        for defId in candidates:
          if isAnumEfld(defId):
            return defId

proc semSumTypeCaseOfValue(c: var SemContext; dest: var TokenBuf; it: var Item;
                            selectorType: TypeCursor; objTypeSym: SymId;
                            seen: var seq[(xint, xint)];
                            bindings: var seq[SumTypeBinding]) =
  if it.n.substructureKind != RangesU:
    buildErr c, dest, it.n.info, "`ranges` within `of` expected"
    skip it.n
    return
  takeToken dest, it.n
  bindings.setLen 0
  while it.n.hasMore:
    let info = it.n.info
    if it.n.exprKind in CallKinds:
      inc it.n
      if it.n.kind == Ident:
        let branchName = it.n.litId
        let efldSym = findOneofEfld(c, branchName)
        if efldSym == SymId(0):
          buildErr c, dest, info, "undeclared sum type branch: " & pool.strings[branchName]
          while it.n.hasMore: skip it.n
          consumeParRi it.n
          continue
        dest.add symToken(efldSym, info)
        inc it.n
        let ordVal = getEfldOrdinal(efldSym)
        if not ordVal.isNaN:
          if seen.containsOrIncl(ordVal):
            buildErr c, dest, info, "value already handled"
        let branchFields = findBranchFields(objTypeSym, efldSym)
        var fieldIdx = 0
        while it.n.hasMore:
          if it.n.kind == Ident and fieldIdx < branchFields.len:
            bindings.add SumTypeBinding(
              ident: it.n.litId,
              fieldSym: branchFields[fieldIdx].sym,
              fieldType: branchFields[fieldIdx].typ,
              info: it.n.info)
          elif it.n.kind == Ident:
            buildErr c, dest, it.n.info, "too many bindings for sum type branch"
          inc it.n
          inc fieldIdx
        inc it.n # skip call ParRi
      elif it.n.exprKind == CurlyX:
        inc it.n # skip curly tag
        var firstEfld = SymId(0)
        var firstBranchFields: seq[SumTypeBranchField] = @[]
        while it.n.hasMore:
          if it.n.kind == Ident:
            let branchName = it.n.litId
            let efldSym = findOneofEfld(c, branchName)
            if efldSym == SymId(0):
              buildErr c, dest, it.n.info, "undeclared sum type branch: " & pool.strings[branchName]
            else:
              dest.add symToken(efldSym, it.n.info)
              let ordVal = getEfldOrdinal(efldSym)
              if not ordVal.isNaN:
                if seen.containsOrIncl(ordVal):
                  buildErr c, dest, it.n.info, "value already handled"
              if firstEfld == SymId(0):
                firstEfld = efldSym
                firstBranchFields = findBranchFields(objTypeSym, efldSym)
              else:
                let otherFields = findBranchFields(objTypeSym, efldSym)
                if otherFields.len != firstBranchFields.len or
                    (otherFields.len > 0 and otherFields[0].sym != firstBranchFields[0].sym):
                  buildErr c, dest, it.n.info,
                    "branches in set pattern must come from the same `of` declaration"
          inc it.n
        inc it.n # skip curly ParRi
        if firstEfld != SymId(0):
          var fieldIdx = 0
          while it.n.hasMore:
            if it.n.kind == Ident and fieldIdx < firstBranchFields.len:
              bindings.add SumTypeBinding(
                ident: it.n.litId,
                fieldSym: firstBranchFields[fieldIdx].sym,
                fieldType: firstBranchFields[fieldIdx].typ,
                info: it.n.info)
            elif it.n.kind == Ident:
              buildErr c, dest, it.n.info, "too many bindings for sum type branch"
            inc it.n
            inc fieldIdx
        else:
          while it.n.hasMore:
            inc it.n
        inc it.n # skip call ParRi
      else:
        buildErr c, dest, info, "identifier expected for sum type branch name"
        while it.n.hasMore: skip it.n
        consumeParRi it.n
    else:
      evalConstCaseBranch(c, dest, it, selectorType, seen, info)
  takeParRi dest, it.n

proc semObjectCaseBranch(c: var SemContext; dest: var TokenBuf; it: var Item;
                         state: var SemObjectState) =
  if it.n.stmtKind == StmtsS:
    takeToken dest, it.n
    while it.n.hasMore:
      semObjectComponent c, dest, it.n, state
    takeParRi dest, it.n
  else:
    dest.addParLe(StmtsS, it.n.info)
    semObjectComponent c, dest, it.n, state
    dest.addParRi()

proc buildEfld(buf: var TokenBuf; sym: SymId; parentType: SymId;
               ordinal: int; name: StrId; info: PackedLineInfo;
               exported: bool) =
  buf.addParLe(EfldY, info)
  buf.add symdefToken(sym, info)
  if exported:
    buf.add identToken(pool.strings.getOrIncl("x"), info)
  else:
    buf.addDotToken()
  buf.addDotToken()
  buf.add symToken(parentType, info)
  buf.addParLe(TupX, info)
  buf.add intToken(pool.integers.getOrIncl(int64(ordinal)), info)
  buf.addStrLit pool.strings[name], info
  buf.addParRi()
  buf.addParRi()

proc synthSumTypeDiscriminator(c: var SemContext; dest: var TokenBuf;
                                it: var Item; info: PackedLineInfo;
                                state: var SemObjectState): TypeCursor =
  skip it.n # skip the empty (fld . . . . .)

  type BranchInfo = object
    name: StrId
    info: PackedLineInfo
  var branches: seq[BranchInfo] = @[]
  var seen = initHashSet[StrId]()
  var scan = it.n
  while scan.substructureKind == OfU:
    scan.into:                                  # (of ...)
      if scan.substructureKind == RangesU:
        scan.into:                              # (ranges ...)
          while scan.hasMore:
            if scan.kind == Ident:
              let name = scan.litId
              if seen.containsOrIncl(name):
                buildErr c, dest, scan.info, "duplicate sum type branch name: " & pool.strings[name]
              else:
                branches.add BranchInfo(name: name, info: scan.info)
            skip scan
      skip scan                                 # action subtree
  if scan.substructureKind == ElseU:
    buildErr c, dest, scan.info, "sum type case objects cannot have an else branch"

  var typeNameStr = "`sumtype"
  c.makeGlobalSym(typeNameStr)
  let oneofTypeSym = pool.syms.getOrIncl(typeNameStr)

  var efldSyms: seq[(SymId, StrId)] = @[]
  var typeBuf = createTokenBuf(30)
  typeBuf.addParLe(TypeY, info)
  typeBuf.add symdefToken(oneofTypeSym, info)
  typeBuf.addDotToken()
  typeBuf.addDotToken()
  typeBuf.addDotToken()
  typeBuf.addParLe(AnumT, info)
  typeBuf.addSubtree c.types.uint8Type
  # Store the owning object type sym so that generic type inference
  # can trace efld → anum → object type (works cross-module):
  if state.ownerSym != SymId(0):
    typeBuf.add symToken(state.ownerSym, info)
  else:
    typeBuf.addDotToken()
  for i, b in branches:
    let sym = identToSym(c, pool.strings[b.name], EfldY)
    efldSyms.add (sym, b.name)
    buildEfld(typeBuf, sym, oneofTypeSym, i, b.name, b.info, state.isExported)
  typeBuf.addParRi()
  typeBuf.addParRi()

  let typeStart = c.pendingSumtypes.len
  c.pendingSumtypes.add typeBuf
  programs.publish oneofTypeSym, c.pendingSumtypes, typeStart, c.phase

  if c.inTypeInst == 0:
    # Only inject constructors for the original generic definition,
    # not for each instantiation (which would cause ambiguous names):
    var rootScope = c.currentScope
    while rootScope.up != nil: rootScope = rootScope.up
    for i, (sym, name) in efldSyms:
      var efldBuf = createTokenBuf(10)
      buildEfld(efldBuf, sym, oneofTypeSym, i, name, branches[i].info, state.isExported)
      programs.publish sym, efldBuf, c.phase
      let s = Sym(kind: EfldY, name: sym, pos: ImportedPos)
      rootScope.addOverloadable(name, s)

  var fldNameStr = "`kind"
  c.makeFieldSym(fldNameStr)
  let fldSym = pool.syms.getOrIncl(fldNameStr)
  dest.addParLe(FldY, info)
  dest.add symdefToken(fldSym, info)
  if state.isExported:
    dest.add identToken(pool.strings.getOrIncl("x"), info)
  else:
    dest.addDotToken()
  dest.addDotToken()
  let typePos = dest.len
  dest.add symToken(oneofTypeSym, info)
  dest.addDotToken()
  dest.addParRi()

  result = typeToCursor(c, dest, typePos)

proc semCaseImpl(c: var SemContext; dest: var TokenBuf; it: var Item; mode: CaseMode;
                 state: var SemObjectState) =
  let info = it.n.info
  takeToken dest, it.n
  var selectorType = default(Cursor)
  var isSumType = false
  var stInfo = SumTypeInfo(valid: false)
  var savedSelector = createTokenBuf(4)
  case mode
  of NormalCase:
    let selectorStart = dest.len
    var selector = Item(n: it.n, typ: c.types.autoType)
    semExpr c, dest, selector
    it.n = selector.n
    selectorType = skipModifier(selector.typ)
    stInfo = findSumTypeInfo(selectorType)
    if stInfo.valid:
      isSumType = true
      for i in selectorStart ..< dest.len:
        savedSelector.add dest[i]
      dest.shrink selectorStart
      var needsExprClose = false
      if savedSelector.len != 1 or savedSelector[0].kind != Symbol:
        dest.addParLe(ExprX, info)
        needsExprClose = true
        var tmpName = "`case"
        c.makeLocalSym(tmpName)
        let tmpSym = pool.syms.getOrIncl(tmpName)
        let tmpDeclStart = dest.len
        dest.addParLe(VarS, info)
        dest.add symdefToken(tmpSym, info)
        dest.addDotToken()
        dest.addDotToken()
        dest.addSubtree selectorType
        for i in 0 ..< savedSelector.len:
          dest.add savedSelector[i]
        dest.addParRi()
        publish c, dest, tmpSym, tmpDeclStart
        let s = Sym(kind: VarY, name: tmpSym, pos: tmpDeclStart)
        discard addNonOverloadable(c.currentScope, pool.strings.getOrIncl(tmpName), s)
        savedSelector = createTokenBuf(1)
        savedSelector.add symToken(tmpSym, info)
      if stInfo.isRef:
        dest.addParLe(DdotX, info)
      else:
        dest.addParLe(DotX, info)
      for i in 0 ..< savedSelector.len:
        dest.add savedSelector[i]
      dest.add symToken(stInfo.discrimSym, info)
      dest.addIntLit(0, info)
      dest.addParRi()
      if needsExprClose:
        dest.addParRi()
      selectorType = stInfo.discrimType
  of ObjectCase:
    var probe = it.n
    inc probe
    if probe.kind == DotToken:
      selectorType = synthSumTypeDiscriminator(c, dest, it, info,
        state)
    else:
      let selectorStart = dest.len
      semLocal(c, dest, it.n, FldY)
      let field = cursorAt(dest, selectorStart)
      let fieldType = asLocal(field).typ
      let fieldTypePos = cursorToPosition(dest, fieldType)
      endRead(dest)
      selectorType = typeToCursor(c, dest, fieldTypePos)
      if not isOrdinalType(selectorType):
        buildErr c, dest, info, "selector must be of an ordinal type"

  let isString = if isSumType: false else: isSomeStringType(selectorType)
  var seen: seq[(xint, xint)] = @[]
  var seenStr = initHashSet[StrId]()
  var bindings: seq[SumTypeBinding] = @[]
  if it.n.substructureKind == OfU:
    while it.n.substructureKind == OfU:
      takeToken dest, it.n
      if isSumType:
        semSumTypeCaseOfValue c, dest, it, selectorType, stInfo.objTypeSym, seen, bindings
      elif isString:
        semCaseOfValueString c, dest, it, selectorType, seenStr
      else:
        semCaseOfValue c, dest, it, selectorType, seen
      case mode
      of NormalCase:
        if isSumType and bindings.len > 0:
          # For generic instantiations, build bindings to instantiate field types:
          var typeBindings = initTable[SymId, Cursor]()
          if stInfo.invokeArgs != default(Cursor):
            let objDecl = getTypeSection(stInfo.objTypeSym)
            typeBindings = bindInvokeArgs(objDecl, stInfo.invokeArgs)
          withNewScope c:
            takeToken dest, it.n
            for b in bindings:
              var bindName = pool.strings[b.ident]
              c.makeLocalSym(bindName)
              let bindSym = pool.syms.getOrIncl(bindName)
              let declStart = dest.len
              dest.addParLe(PatternvarS, b.info)
              dest.add symdefToken(bindSym, b.info)
              dest.addDotToken()
              dest.addDotToken()
              dest.addParLe(MutT, b.info)
              if typeBindings.len > 0:
                let concreteType = instantiateType(c, b.fieldType, typeBindings)
                dest.addSubtree concreteType
              else:
                dest.addSubtree b.fieldType
              dest.addParRi()
              if stInfo.isRef:
                dest.addParLe(DdotX, b.info)
              else:
                dest.addParLe(DotX, b.info)
              for i in 0 ..< savedSelector.len:
                dest.add savedSelector[i]
              dest.add symToken(b.fieldSym, b.info)
              dest.addIntLit(0, b.info)
              dest.addParRi()
              dest.addParRi()
              publish c, dest, bindSym, declStart
              let s = Sym(kind: PatternvarY, name: bindSym, pos: declStart)
              if addNonOverloadable(c.currentScope, b.ident, s) == Conflict:
                buildErr c, dest, b.info, "attempt to redeclare: " & pool.strings[b.ident]
            while it.n.hasMore:
              semStmt c, dest, it.n, false
            takeParRi dest, it.n
        else:
          withNewScope c:
            semStmtBranch c, dest, it, true
      of ObjectCase:
        semObjectCaseBranch(c, dest, it, state)
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
      semObjectCaseBranch(c, dest, it, state)
    takeParRi dest, it.n
  elif not isString:
    checkExhaustiveness c, dest, it.n.info, selectorType, seen

  takeParRi dest, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, dest, info, it.typ

proc semCase(c: var SemContext; dest: var TokenBuf; it: var Item) =
  # dummy value, not used for case statements, only for object types:
  var state = SemObjectState(isExported: false, isAnum: false)
  semCaseImpl(c, dest, it, NormalCase, state)

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
  if res.status != LacksNothing: return false
  if res.decl.symKind == IteratorY:
    return true
  # First-class iter value: a local/param of `itertype`.
  if res.decl.symKind in {LetY, VarY, CursorY, ParamY, ConstY, GletY, TletY, GvarY, TvarY}:
    let local = asLocal(res.decl)
    result = local.typ.typeKind == ItertypeT
  else:
    result = false

proc semForLoopTupleVar(c: var SemContext; dest: var TokenBuf; it: var Item; tup: TypeCursor; loopvarTypeMod = NoType) =
  var tup = tup
  var loopvarTypeMod = loopvarTypeMod
  if tup.typeKind in TypeModifiers:
    loopvarTypeMod = tup.typeKind
    inc tup
  inc tup
  while it.n.hasMore and tup.hasMore:
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
    while it.n.hasMore: skip it.n
    consumeParRi it.n

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
  # A call whose fn is unresolved at sem time and needs re-resolution at
  # instantiation: either a bare Ident, or — when the original sem matched
  # a concept-bound op — the preserved symbol-choice (`addFn` writes the
  # OchoiceX of def-site overloads in that case so the inst site can pick a
  # concrete match). Both forms must be treated as macro-like / deferred,
  # otherwise `semFor`'s implicit-iterator path will report "no implicit
  # iterator found" for any concept-bound iterator.
  result = dest.len > beforeCall+1
  if result:
    let callKind =
      if dest[beforeCall].kind == ParLe and rawTagIsNimonyExpr(tagEnum(dest[beforeCall])):
        cast[NimonyExpr](tagEnum(dest[beforeCall]))
      else:
        NoExpr
    if callKind notin CallKinds:
      return false
    let fnTok = dest[beforeCall+1]
    if fnTok.kind == Ident:
      result = true
    elif fnTok.kind == ParLe:
      let fnExpr = exprKind(fnTok)
      result = fnExpr in {OchoiceX, CchoiceX}
    else:
      result = false

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
  if dest[beforeCall].exprKind == ErrX:
    discard "already produced an error"
  elif isIteratorCall(c, dest, beforeCall):
    discard "fine"
  elif dest[beforeCall].kind == ParLe and
      (dest[beforeCall].tagId == TagId(FieldsTagId) or
       dest[beforeCall].tagId == TagId(FieldpairsTagId) or
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
      varsCursor.into:
        while varsCursor.hasMore:
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
        if dest[beforeCall].kind == ParLe and dest[beforeCall].tagId == nifstreams.ErrT:
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
      let hasMultiVars = n2.hasMore
      if hasMultiVars:
        if iterCall.typ.skipModifier.typeKind == TupleT:
          semForLoopTupleVar c, dest, it, iterCall.typ
        else:
          # error handling
          while it.n.hasMore:
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
    # Templates have no `result` symbol — the `return` is text-substituted
    # into the caller, so the meaning depends on the caller's signature.
    # Preserve the dot and let template expansion resolve it.
    if c.routine.kind != TemplateY and c.routine.returnType.typeKind != VoidT:
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
  elif not c.routine.pragmas.contains(RaisesP) and CanRaiseFeature notin c.features:
    buildErr c, dest, info, "`raise` only allowed within a routine with `raises` pragma"
  if it.n.kind == DotToken:
    if c.routine.inExcept == 0:
      buildErr c, dest, info,
        "bare `raise` is only allowed inside an `except` block"
    takeToken dest, it.n
  else:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, dest, a
    # Type check: raised type must be a subtype of the .raises pragma type
    if not cursorIsNil(c.routine.raisesType) and typeKind(c.routine.raisesType) != AutoT:
      # Allow exact match or subtype (inheritance).
      # If both are `ref T` heap-exception types, peel the ref so the
      # inheritance check below can compare the underlying object types.
      # The local names below are chosen to avoid collisions with the
      # `expectedType` parameter on `SemExpressionExecutor` (semdata.nim) —
      # `makeLocalSym` only guarantees per-module uniqueness, so naming
      # this `expectedType` would share its `pool.syms` ID with that
      # parameter and the borrow check would resolve to the wrong decl.
      var raisedRefT = skipModifier(a.typ)
      var raisesRefT = skipModifier(c.routine.raisesType)
      if raisedRefT.typeKind == RefT and raisesRefT.typeKind == RefT:
        inc raisedRefT
        inc raisesRefT
      var compatible = sameTrees(raisedRefT, raisesRefT)
      # Check if raisedRefT is a subtype of raisesRefT (inheritance)
      if not compatible and raisedRefT.kind == Symbol and raisesRefT.kind == Symbol:
        # Use sigmatch's inheritance checking instead of manual chain walking
        var m = createMatch(addr c)
        matchObjectInheritance(m, raisesRefT, raisedRefT, raisesRefT.symId, raisedRefT.symId, NoType)
        compatible = not m.err
      if not compatible:
        c.typeMismatch dest, info, a.typ, c.routine.raisesType
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
  let beforeShift = dest.len
  semExpr c, dest, shift
  it.n = shift.n
  if dest[beforeShift].exprKind != ErrX and shift.typ.typeKind notin {IntT, UIntT}:
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
  if it.n.hasMore:
    let typeStart = dest.len
    semLocalTypeImpl c, dest, it.n, InLocalDecl
    literalType = typeToCursor(c, dest, typeStart)
    if it.n.hasMore and it.n.exprKind == NilX:
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
  # delay() no-arg -> (delay0)
  # delay(call fn args) -> (delay fn args)  [flatten by stripping the call wrapper]
  # The type is always `Continuation`; typenav returns it for both DelayX and Delay0X.
  let beforeExpr = dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n  # skip (delay tag (always DelayX from addFn)
  if it.n.kind == ParRi:
    # delay() no-arg form: produces (delay0)
    dest.addParLe(Delay0X, info)
    dest.addParRi()
    inc it.n
  elif it.n.exprKind in CallKinds:
    # delay(call) form: produce (delay fn args)
    dest.addParLe(DelayX, info)
    it.n.into:                         # descend past inner call's tag
      while it.n.hasMore:
        takeTree dest, it.n            # copy fn and args verbatim (already semchecked)
    dest.addParRi()
    skipParRi it.n                     # skip outer delay's )
  else:
    buildErr c, dest, it.n.info, "`delay` takes a call expression or no argument"
    skip it.n
    skipParRi it.n
  it.typ = c.types.continuationType
  commonType c, dest, it, beforeExpr, expected

proc semSuspend(c: var SemContext; dest: var TokenBuf; it: var Item) =
  # suspend() -> (suspend)
  # Creates a suspension point and returns Continuation(nil, nil)
  let beforeExpr = dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n  # skip (suspend tag (always SuspendX from addFn)
  if it.n.kind == ParRi:
    dest.addParLe(SuspendX, info)
    dest.addParRi()
    inc it.n
  else:
    buildErr c, dest, it.n.info, "`suspend` takes no argument"
    skip it.n
    skipParRi it.n
  it.typ = c.types.continuationType
  commonType c, dest, it, beforeExpr, expected

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
    of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
       TemplateT, ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, VarargsT,
       StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
       DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, SymkindT, TypekindT, TypedescT,
       UntypedT, TypedT, CstringT, PointerT, OrdinalT:
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
  of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
     TemplateT, ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, VarargsT,
     StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
     DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, SymkindT, TypekindT, TypedescT,
     UntypedT, TypedT, CstringT, PointerT, OrdinalT:
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
  while elem.n.hasMore:
    semArrayConstrElem(c, dest, elem, it, ctx)
    inc count
  it.n = elem.n
  takeParRi dest, it.n
  let typeStart = dest.len
  dest.buildTree ArrayT, info:
    dest.addSubtree elem.typ
    dest.buildTree RangetypeT, info:
      var idxType = c.types.intType
      if ctx.firstKeyType.typeKind != AutoT:
        idxType = ctx.firstKeyType
      dest.addSubtree idxType
      var serr = false
      dest.addIntLit(asSigned(ctx.firstIdx, serr), info)
      dest.addIntLit(asSigned(ctx.firstIdx + createXint(count.int64 - 1), serr), info)
  let expected = it.typ
  it.typ = typeToCursor(c, dest, typeStart)

  case expected.typeKind
  of AutoT, ArrayT:
    discard
  of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
     TemplateT, ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, VarargsT,
     StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
     DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, SymkindT, TypekindT, TypedescT,
     UntypedT, TypedT, CstringT, PointerT, OrdinalT:
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
  dest.addParLe(SetconstrX, info)
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
    of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
       TemplateT, ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, ArrayT, VarargsT,
       StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
       DistinctT, ItertypeT, RangetypeT, UarrayT, SymkindT, TypekindT, TypedescT,
       UntypedT, TypedT, CstringT, PointerT, OrdinalT:
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
  of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
     TemplateT, ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, ArrayT, VarargsT,
     StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
     DistinctT, ItertypeT, RangetypeT, UarrayT, SymkindT, TypekindT, TypedescT,
     UntypedT, TypedT, CstringT, PointerT, OrdinalT:
    buildErr c, dest, info, "invalid expected type for set constructor: " & typeToString(it.typ)
  var elemStart = dest.len
  var elemInfo = elem.n.info
  while elem.n.hasMore:
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
  elif t.typeKind == UarrayT:
    # `(aconstr (uarray T) e1 …)` is the internal IR for a static array
    # literal of unspecified length (used by exprexec's ptr-to-nif rule
    # wrapped in `addr` to form the seq's `data` pointer). Element type
    # is the uarray's inner T; hexer's nifcgen hoists the literal to an
    # anonymous module-level static array and rewrites the surrounding
    # `addr` to point at it.
    elem.typ = t
    inc elem.typ # past uarray tag → element type
  else:
    c.buildErr dest, info, "expected array type for array constructor, got: " & typeToString(t)
  while elem.n.hasMore:
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
  while elem.n.hasMore:
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
  dest.add parLeToken(TupconstrX, it.n.info)
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
  while it.n.hasMore:
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
  while it.n.hasMore:
    let named = it.n.substructureKind == KvU
    if named:
      takeToken dest, it.n
      takeTree dest, it.n
    var elem = Item(n: it.n, typ: c.types.autoType)
    if t.hasMore:
      elem.typ = getTupleFieldType(t)
      skip t
    else:
      c.buildErr dest, info, "tuple type " & typeToString(it.typ) & " too short for tuple constructor"
    semExpr c, dest, elem
    it.n = elem.n
    if named:
      takeParRi dest, it.n
  if t.hasMore:
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
  if setFields.hasKey(fieldSym):
    dest.addSubtree setFields.getOrQuit(fieldSym)
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
  while n.hasMore:
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
    onRaiseQuit(("cannot prove that it's safe to initialize '$1' with " &
    "the runtime value for the discriminator '$2'.") %
    [asNimSym(field), asNimSym(discriminator)])

template wrongBranchError(c: var SemContext; dest: var TokenBuf, info: PackedLineInfo, field: SymId,
      discriminator: SymId, discriminatorVal: Cursor) =
  c.buildErr dest, info,
      onRaiseQuit(("a case selecting discriminator '$1' with value '$2' " &
      "appears in the object construction, but the field(s) '$3' " &
      "are in conflict with this value.") %
      [asNimSym(discriminator), asNimCode(discriminatorVal), asNimSym(field)])

proc caseBranchMatchesExpr(c: var SemContext; dest: var TokenBuf; branch, matched: Cursor; selectorType: Cursor): bool =
  result = false
  var branch = branch
  inc branch
  while branch.hasMore:
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
    of NoSub, NilU, NotnilU, KvU, VvU, RangesU, ParamU, TypevarU, EfldU, FldU,
       WhenU, ElifU, ElseU, TypevarsU, CaseU, OfU, StmtsU, ParamsU, PragmasU,
       EitherU, JoinU, UnpackflatU, UnpacktupU, ExceptU, FinU, UncheckedU, GfldU:
      if sameTrees(branch, matched):
        return true
      skip branch

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
    while n.hasMore:
      case n.substructureKind
      of OfU:
        inc n
        var state: BranchState
        if setFields.hasKey(selectorSymId):
          let discriminatorVal = getValueInKv(setFields.getOrQuit(selectorSymId))
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
            wrongBranchError(c, dest, info, presentFieldSymId, selectorSymId, getValueInKv(setFields.getOrQuit(selectorSymId)))
          inc n # stmt
          while n.hasMore:
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
          elif isBranchSelected and setFields.hasKey(selectorSymId):
            wrongBranchError(c, dest, info, presentFieldSymId, selectorSymId, getValueInKv(setFields.getOrQuit(selectorSymId)))
          inc n # stmt
          while n.hasMore:
            let field = takeLocal(n, SkipFinalParRi)
            buildObjConstrField(c, dest, field, setFields, info, bindings, depth)
          skipParRi n # stmt
          lastFieldSymId = presentFieldSymId
        else:
          skip n
        skipParRi n
      of NoSub, NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU, TypevarU, EfldU, FldU,
         WhenU, ElifU, TypevarsU, CaseU, StmtsU, ParamsU, PragmasU,
         EitherU, JoinU, UnpackflatU, UnpacktupU, ExceptU, FinU, UncheckedU, GfldU:
        error "illformed AST inside case object: ", n

  if selectorSymId notin setFields:
    if lastFieldSymId != SymId(0):
      badDiscriminatorError(c, dest, info, lastFieldSymId, selectorSymId)
    elif bestBranch != default(Cursor):
      inc bestBranch # stmt
      while bestBranch.hasMore:
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
      var currentField = parent.body
      inc currentField   # past (object
      if parent.kind == ObjectT:
        skip currentField  # parent type / inheritance slot
      if currentField.kind != DotToken:
        buildObjConstrFields(c, dest, currentField, setFields, info, bindings, depth)
      parentType = parent.parentType
      inc depth
    # bring back original bindings:
    bindings = origBindings
  var currentField = obj.body
  inc currentField   # past (object
  if obj.kind == ObjectT:
    skip currentField  # parent type / inheritance slot
  if currentField.kind != DotToken:
    buildObjConstrFields(c, dest, currentField, setFields, info, bindings)
  dest.addParRi()

proc getAnumOwnerType(efldSym: SymId): SymId =
  ## Given an efld symbol, trace efld → anum type → owning object type.
  ## The owner is stored in the anum body after the base type.
  let efldRes = tryLoadSym(efldSym)
  if efldRes.status != LacksNothing or efldRes.decl.substructureKind != EfldU:
    return SymId(0)
  var n = efldRes.decl
  skipToLocalType n
  if n.kind != Symbol:
    return SymId(0)
  let anumSym = n.symId
  let anumRes = tryLoadSym(anumSym)
  if anumRes.status != LacksNothing:
    return SymId(0)
  let anumDecl = asTypeDecl(anumRes.decl)
  if anumDecl.body.typeKind != AnumT:
    return SymId(0)
  # anum body layout: (anum basetype ownerSym efld1 ...)
  var body = anumDecl.body
  inc body # skip AnumT tag
  skip body # skip base type
  if body.kind == Symbol:
    return body.symId
  return SymId(0)

proc inferTypevarFromTypes(formal, actual: Cursor; inferred: var Table[SymId, Cursor]) =
  ## Walk formal and actual types in parallel, extracting typevar bindings.
  var f = formal
  var a = actual
  if f.kind == Symbol:
    let res = tryLoadSym(f.symId)
    if res.status == LacksNothing and res.decl.tagEnum == TypevarTagId:
      if f.symId notin inferred:
        inferred[f.symId] = a
      return
  if f.kind == ParLe and a.kind == ParLe and f.tagId == a.tagId:
    inc f; inc a
    while f.hasMore and a.hasMore:
      inferTypevarFromTypes(f, a, inferred)
      skip f; skip a

proc inferFieldTypes(c: var SemContext; args: Cursor;
                      fieldTypesByName: Table[StrId, TypeCursor];
                      inferred: var Table[SymId, Cursor]) =
  ## Scan KV argument pairs, semcheck values with AutoT, and infer typevars.
  var scan = args
  while scan.hasMore:
    if scan.substructureKind == KvU:
      inc scan
      let fieldName = takeIdent(scan)
      if fieldName != StrId(0) and fieldTypesByName.hasKey(fieldName):
        var valBuf = createTokenBuf(16)
        var val = Item(n: scan, typ: c.types.autoType)
        semExpr c, valBuf, val
        inferTypevarFromTypes(fieldTypesByName.getOrQuit(fieldName), val.typ, inferred)
        scan = val.n
      else:
        skip scan
      if scan.kind == ParRi:
        inc scan
      else:
        skip scan
    else:
      skip scan

proc buildInferredInvoke(c: var SemContext; objTypeSym: SymId;
                          decl: TypeDecl; inferred: Table[SymId, Cursor];
                          info: PackedLineInfo): TypeCursor =
  ## Build an InvokeT from inferred type params and instantiate it.
  ## Returns default if not all params were inferred.
  var typeBuf = createTokenBuf(16)
  typeBuf.addParLe(InvokeT, info)
  typeBuf.add symToken(objTypeSym, info)
  var tv = decl.typevars
  inc tv # skip TypevarsU tag
  while tv.hasMore:
    let tvar = asLocal(tv)
    let tvSym = tvar.name.symId
    if inferred.hasKey(tvSym):
      typeBuf.addSubtree inferred.getOrQuit(tvSym)
    else:
      return default(TypeCursor)
    skip tv
  typeBuf.addParRi()
  var instDest = createTokenBuf(16)
  var instRead = cursorAt(typeBuf, 0)
  result = semLocalType(c, instDest, instRead)

proc fieldTypesByNameFromObj(decl: TypeDecl): Table[StrId, TypeCursor] =
  ## Collect field name → type mappings from an object type declaration.
  result = initTable[StrId, TypeCursor]()
  let body = decl.objBody
  if body.typeKind != ObjectT: return
  let obj = asObjectDecl(body)
  var n = obj.body
  inc n  # past (object
  skip n  # parent type / inheritance slot
  var iter = initObjFieldIter()
  while nextField(iter, n):
    let field = takeLocal(n, SkipFinalParRi)
    let name = symToIdent(field.name.symId)
    result[name] = field.typ

proc inferSumTypeFromFields(c: var SemContext; dest: var TokenBuf;
                             efldSym: SymId; args: Cursor;
                             info: PackedLineInfo): TypeCursor =
  ## Infer generic type for a sum type constructor like `Some(val: 4)`.
  let objTypeSym = getAnumOwnerType(efldSym)
  if objTypeSym == SymId(0): return default(TypeCursor)

  let decl = getTypeSection(objTypeSym)
  if not decl.isGeneric:
    var typeBuf = createTokenBuf(1)
    typeBuf.add symToken(objTypeSym, info)
    var instDest = createTokenBuf(16)
    var instRead = cursorAt(typeBuf, 0)
    return semLocalType(c, instDest, instRead)

  let branchFields = findBranchFields(objTypeSym, efldSym)
  var fieldTypesByName = initTable[StrId, TypeCursor]()
  for bf in branchFields:
    fieldTypesByName[symToIdent(bf.sym)] = bf.typ

  var inferred = initTable[SymId, Cursor]()
  inferFieldTypes(c, args, fieldTypesByName, inferred)
  result = buildInferredInvoke(c, objTypeSym, decl, inferred, info)

proc inferObjTypeFromFields(c: var SemContext; objTypeSym: SymId;
                             decl: TypeDecl; args: Cursor;
                             info: PackedLineInfo): TypeCursor =
  ## Infer generic type for an object constructor like `Foo(x: 4)`.
  let fieldTypesByName = fieldTypesByNameFromObj(decl)
  var inferred = initTable[SymId, Cursor]()
  inferFieldTypes(c, args, fieldTypesByName, inferred)
  result = buildInferredInvoke(c, objTypeSym, decl, inferred, info)

proc semSumTypeObjConstr(c: var SemContext; dest: var TokenBuf; it: var Item;
                          efldSym: SymId; expected: TypeCursor; info: PackedLineInfo) =
  let branchInfo = it.n.info
  inc it.n
  var objBuf = createTokenBuf(32)
  objBuf.add parLeToken(OconstrX, info)
  objBuf.addSubtree expected
  let kindName = pool.strings.getOrIncl("`kind")
  objBuf.addParLe(KvU, branchInfo)
  objBuf.add identToken(kindName, branchInfo)
  objBuf.add symToken(efldSym, branchInfo)
  objBuf.addParRi()
  while it.n.hasMore:
    objBuf.addSubtree it.n
    skip it.n
  objBuf.addParRi()
  inc it.n
  var objConstr = Item(n: cursorAt(objBuf, 0), typ: expected)
  semObjConstr c, dest, objConstr
  it.typ = objConstr.typ

proc semObjConstr(c: var SemContext; dest: var TokenBuf, it: var Item) =
  let exprStart = dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  if it.n.kind == Ident:
    block sumTypeCheck:
      let efldSym = findOneofEfld(c, it.n.litId)
      if efldSym != SymId(0):
        var inferredExpected = expected
        if expected.typeKind == AutoT:
          # Try to infer generic type parameters from field values:
          let savedN = it.n
          inc it.n # skip constructor name ident
          inferredExpected = inferSumTypeFromFields(c, dest, efldSym, it.n, info)
          it.n = savedN # restore cursor for semSumTypeObjConstr
          if inferredExpected == default(TypeCursor):
            c.buildErr dest, info, "cannot infer generic type for sum type constructor"
            while it.n.hasMore: skip it.n
            consumeParRi it.n
            return
        semSumTypeObjConstr(c, dest, it, efldSym, inferredExpected, info)
        return
  it.typ = semLocalType(c, dest, it.n)
  dest.shrink exprStart
  var decl = default(TypeDecl)
  var objType = it.typ
  var isGenericObj = containsGenericParams(objType)
  if objType.typeKind in {RefT, PtrT}:
    inc objType
  var invokeArgs = skipInvoke(objType)
  var typeSym = SymId(0)
  if objType.kind == Symbol:
    typeSym = objType.symId
    decl = getTypeSection(typeSym)
    if decl.kind != TypeY:
      # includes typevar case
      c.buildErr dest, info, "expected type for object constructor"
      while it.n.hasMore: skip it.n
      consumeParRi it.n
      return
    objType = decl.objBody
    if objType.typeKind != ObjectT:
      c.buildErr dest, info, "expected object type for object constructor"
      while it.n.hasMore: skip it.n
      consumeParRi it.n
      return
  else:
    c.buildErr dest, info, "expected type symbol for object constructor"
    while it.n.hasMore: skip it.n
    consumeParRi it.n
    return
  if decl.isGeneric and invokeArgs == default(Cursor):
    # Generic object without explicit type params — infer from field values:
    let inferred = inferObjTypeFromFields(c, typeSym, decl, it.n, info)
    if inferred == default(TypeCursor):
      c.buildErr dest, info, "cannot infer generic type for object constructor"
      while it.n.hasMore: skip it.n
      consumeParRi it.n
      return
    it.typ = inferred
    objType = it.typ
    if objType.typeKind in {RefT, PtrT}:
      inc objType
    invokeArgs = skipInvoke(objType)
    if objType.kind == Symbol:
      decl = getTypeSection(objType.symId)
      objType = decl.objBody
    isGenericObj = false
  # build bindings for invoked object type to get proper types for fields:
  let bindings = bindInvokeArgs(decl, invokeArgs)
  var fieldBuf = createTokenBuf(16)
  var setFieldPositions = initTable[SymId, int]()
  while it.n.hasMore:
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
          if res.status == LacksNothing and res.decl.substructureKind in {FldU, GfldU}:
            # trust that it belongs to this object for now
            # level is either given or 0
            hasFieldSym = true
            field = ObjField(sym: sym, typ: asLocal(res.decl).typ, level: 0)
          else:
            # field syms are nested inside the owning type so `tryLoadSym`
            # often cannot resolve them. The Symbol form means a prior
            # semcheck pass already validated visibility, so look up by name
            # but skip the visibility check.
            field = findObjFieldConsiderVis(c, decl, fieldName, bindings, bypassVis = true)
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
        if it.n.hasMore:
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
  if it.n.hasMore:
    skip it.n # existing `default(T)` call
  takeParRi dest, it.n
  commonType c, dest, it, exprStart, expected

proc buildDefaultTuple(c: var SemContext; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo) =
  dest.addParLe(TupconstrX, info)
  dest.addSubtree typ
  var currentField = typ
  inc currentField # skip tuple tag
  while currentField.hasMore:
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
  semConstIntExpr c, dest, idx, c.phase
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
    if it.typ.hasMore:
      it.typ = getTupleFieldType(it.typ)
    takeParRi dest, it.n
    commonType c, dest, it, exprStart, expected

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
  while argRead.hasMore:
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
    while it.n.hasMore:
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
  while it.n.hasMore:
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

proc semCurlyat(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  inc it.n # tag
  var lhsBuf = createTokenBuf(4)
  var lhs = Item(n: it.n, typ: c.types.autoType)
  semExpr c, lhsBuf, lhs, {KeepMagics}
  it.n = lhs.n
  lhs.n = cursorAt(lhsBuf, 0)

  # `{}` has no builtin meaning; always rewrite to a call to `{}`:
  var callBuf = createTokenBuf(16)
  callBuf.addParLe(CallX, info)
  callBuf.add identToken(pool.strings.getOrIncl("{}"), info)
  callBuf.addSubtree lhs.n
  while it.n.hasMore:
    callBuf.takeTree it.n
  callBuf.addParRi()
  skipParRi it.n
  var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
  semCall c, dest, call, {}, CurlyatCall
  it.typ = call.typ

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
    # add array index information to the `ArratX` magic for easy
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
    while it.n.hasMore:
      skip it.n
  of UarrayT:
    it.typ = typ
    inc it.typ
  of CstringT:
    it.typ = c.types.charType
  of SetT:
    it.typ = c.types.uint8Type
  of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
     TemplateT, ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, VarargsT,
     StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
     DistinctT, ItertypeT, RangetypeT, AutoT, SymkindT, TypekindT, TypedescT, UntypedT, TypedT,
     PointerT, OrdinalT:
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

proc semProccall(c: var SemContext; dest: var TokenBuf; it: var Item) =
  # Input: (proccall (call fn args...) )
  # Build (proccall fn args...) and call semCall directly so the flat format
  # is emitted without any intermediate nested representation.
  let info = it.n.info
  inc it.n  # skip (proccall
  assert it.n.exprKind in CallKinds - {ProccallX}
  inc it.n  # skip inner (call
  var callBuf = createTokenBuf(16)
  callBuf.addParLe(ProccallX, info)
  while it.n.hasMore:
    takeTree callBuf, it.n
  callBuf.addParRi()
  skipParRi it.n  # skip inner call's )
  skipParRi it.n  # skip outer proccall's )
  var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
  semCall c, dest, call, {}
  it.typ = call.typ

proc semTableConstructor(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag]) =
  # we simply transform ``{key: value, key2, key3: value}`` to
  # ``[(key, value), (key2, value2), (key3, value2)]``
  let info = it.n.info
  let orig = it.n
  inc it.n
  var arrayBuf = createTokenBuf(16)
  var singleKeys = newSeq[Cursor]()
  arrayBuf.buildTree BracketX, info:
    while it.n.hasMore:
      if it.n.substructureKind == KvU:
        let kvInfo = it.n.info
        inc it.n
        if singleKeys.len != 0:
          var cur = it.n
          skip cur
          assert cur.hasMore
          for key in singleKeys:
            arrayBuf.buildTree TupX, key.info:
              arrayBuf.copyTree key
              arrayBuf.copyTree cur

          setLen(singleKeys, 0)

        arrayBuf.buildTree TupX, kvInfo:
          arrayBuf.takeTree it.n
          assert it.n.hasMore
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

proc expandSymChoice(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  takeToken dest, n
  assert n.kind == Symbol
  var name = pool.syms[n.symId]
  extractBasename(name)
  var marker = initHashSet[SymId]()
  while n.hasMore:
    assert n.kind == Symbol
    marker.incl n.symId
    takeToken dest, n
  addSymChoiceSyms(c, dest, pool.strings.getOrIncl(name), marker, info)
  takeParRi dest, n

proc semSymChoice(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag] = {}) =
  if it.n.exprKind == OchoiceX:
    # could restrict to callees
    expandSymChoice c, dest, it.n
  else:
    takeTree dest, it.n

proc semExpr*(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag] = {}) =
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
        of ObjectT, EnumT, HoleyEnumT, AnumT, DistinctT, ConceptT:
          buildErr c, dest, it.n.info, "expression expected"
          skip it.n
        of IntT, FloatT, CharT, BoolT, UIntT, VoidT, NiltT, AutoT, SymkindT,
            PtrT, RefT, MutT, OutT, LentT, SinkT, UarrayT, SetT, StaticT, TypedescT,
            TupleT, ArrayT, RangetypeT, VarargsT, UntypedT, TypedT,
            CstringT, PointerT, TypekindT, OrdinalT, RoutineTypes:
          # every valid local type expression
          semLocalTypeExpr c, dest, it
        of OrT, AndT, NotT, InvokeT:
          # should be handled in respective expression kinds
          discard
      of PragmaxS:
        semPragmaExpr c, dest, it
      of MixinS, BindS:
        # `mixin` / `bind` affect symbol resolution in untyped template/generic
        # bodies (handled in `semuntyped`). In a fully-typed context they are
        # effectively no-ops; keep the tree so later passes see the statement.
        takeTree dest, it.n
      of ImportasS, StaticstmtS, AsmS:
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
      of CoroforS:
        buildErr c, dest, it.n.info, "`corofor` is a hexer-internal shape and must not appear in source"
        skip it.n
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
      of PatternvarS:
        toplevelGuard c:
          semLocal c, dest, it, PatternvarY
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
      of ImportexceptS: semImportExcept c, dest, it
      of FromimportS: semFromImport c, dest, it
      of ExportS: semExport c, dest, it
      of ExportexceptS: semExportExcept c, dest, it
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
        if c.phase == SemcheckTopLevelSyms:
          # Extract feature pragmas even in phase1 so that e.g. lenientnils
          # is known before type declarations are processed:
          var probe = it.n
          inc probe # skip (pragmas
          while probe.hasMore:
            if probe.substructureKind == KvU:
              inc probe # skip (kv
              if probe.pragmaKind == FeatureP:
                inc probe # skip (feature
                if probe.kind == StringLit:
                  let features = parseFeatures(pool.strings[probe.litId])
                  c.features.incl features
                break
              else:
                break
            elif probe.pragmaKind == FeatureP:
              inc probe # skip (feature
              if probe.kind == StringLit:
                let features = parseFeatures(pool.strings[probe.litId])
                c.features.incl features
              break
            else:
              skip probe
          dest.takeTree it.n
        else:
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
    of InfX, NeginfX, NanX:
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
    of CallX, CmdX, CallstrlitX, InfixX, PrefixX, HcallX:
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
    of EqX, NeqX, LeX, LtX, EqsetX, LesetX, LtsetX:
      semCmp c, dest, it
    of AddX, SubX, MulX, DivX, ModX, BitandX, BitorX, BitxorX, PlussetX, MinussetX, MulsetX, XorsetX:
      semTypedBinaryArithmetic c, dest, it
    of AshrX, ShrX, ShlX:
      semShift c, dest, it
    of BitnotX, NegX:
      semTypedUnaryArithmetic c, dest, it
    of DelayX, Delay0X:
      semDelay c, dest, it
    of SuspendX:
      semSuspend c, dest, it
    of InsetX:
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
    of SetconstrX:
      semSetConstr c, dest, it
    of TupconstrX:
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
    of BindSymX:
      semBindSym c, dest, it
    of BindSymNameX:
      semBindSymName c, dest, it
    of IsmainmoduleX:
      semIsMainModule c, dest, it
    of AtX:
      semSubscript c, dest, it
    of ArratX, PatX:
      semTypedAt c, dest, it
    of UnpackX:
      takeToken dest, it.n
      takeParRi dest, it.n
    of FieldsX, FieldpairsX, InternalFieldPairsX:
      takeTree dest, it.n
    of OchoiceX, CchoiceX:
      if ResemChoiceFeature in c.features:
        semSymChoice c, dest, it
      else:
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
    of EnumtostrX:
      semEnumToStr c, dest, it
    of DefaultobjX:
      semObjDefault c, dest, it
    of DefaulttupX:
      semTupleDefault c, dest, it
    of DefaultdistinctX:
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
    of DestroyX, CopyX, WasmovedX, SinkhX, TraceX:
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
    of CurlyatX:
      semCurlyat c, dest, it
    of AlignofX, OffsetofX:
      # XXX To implement
      buildErr c, dest, it.n.info, "to implement: " & $exprKind(it.n)
      takeToken dest, it.n
      takeParRi dest, it.n
    of EnvpX:
      bug "frontend should not encounter `envp`"
    of KvX:
      takeToken dest, it.n
      var keyIt = Item(n: it.n, typ: c.types.autoType)
      semExpr c, dest, keyIt
      it.n = keyIt.n
      var valIt = Item(n: it.n, typ: c.types.autoType)
      semExpr c, dest, valIt
      it.n = valIt.n
      if it.n.hasMore:
        takeTree dest, it.n
      takeParRi dest, it.n
      it.typ = valIt.typ

  of ParRi, EofToken, SymbolDef, UnknownToken, DotToken:
    buildErr c, dest, it.n.info, "expression expected"
    if it.n.kind in {DotToken, UnknownToken}:
      inc it.n


type
  EnsurePhaseResult* = enum
    PhaseOk,        ## Symbol is now at the required phase
    PhaseCycle,     ## Cyclic dependency detected
    PhaseNotFound   ## Symbol not in prog.mem

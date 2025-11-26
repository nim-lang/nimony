#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Lambda lifting uses multiple passes:

- Determine which local variables cross proc boundaries. Map these to an environment.
  An environment is a scope that is allocated on the heap.
- Each usage of such a local becomes `env.local` but the `env` is not always the same:
  The outer env is itself a local variable, the inner env is a proc parameter.
- Procs that use a local variable that crosses a proc boundary are marked as "closure"
  ("uses environment").
- **Usages** of closure procs are turned from `fn` to `(fn, env)` and these tuple calls are
  turned from `(fn, env)(args)` to `fn(args, env)`. The tuple creation/unpacking can
  be optimized further.
- If all usages of closure procs do not escape, the environment can be allocated on
  the stack. As an approximation, closure procs do not escape if they are only used
  as the `fn` value in a function call `fn(args)`.
- A single indirection might not be enough. Consider:

```nim
  proc outerA =
    var a, b: int
    proc outerB =
      proc innerA = use(a)
      proc innerB = use(b); innerA()
      innerB()
```

Here `outerB` is also a closure.

]##

import std / [assertions, sets, tables]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints,
  builtintypes, langmodes, renderer, reporters]
import hexer_context

type
  EnvMode = enum
    EnvIsLocal, EnvIsParam
  CurrentEnv = object
    s: SymId
    typ: SymId
    mode: EnvMode
    needsHeap: bool

  EnvField = object
    objType: SymId
    field: SymId
    typ: Cursor

  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    closureProcs, createsEnv, escapes: HashSet[SymId]
    localToEnv: Table[SymId, EnvField]
    env: CurrentEnv

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.kind != ParRi:
      tr(c, dest, n)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  #c.typeCache.openScope(ProcScope)
  let decl = n
  copyInto dest, n:
    let symId = n.symId
    c.procStack.add(symId)
    var isConcrete = true # assume it is concrete
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.openProcScope(symId, decl, n)
        c.typeCache.registerParams(symId, decl, n)
      elif i == TypevarsPos:
        isConcrete = n.substructureKind != TypevarsU
      elif i == ProcPragmasPos:
        if hasPragma(n, ClosureP):
          c.closureProcs.incl symId
          c.escapes.incl symId
      takeTree dest, n
    if isConcrete:
      tr(c, dest, n)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()

proc envTypeForProc(c: var Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(s & ".env." & c.thisModuleSuffix)

proc localToField(c: var Context; n: Cursor; local, typ: SymId): SymId =
  if c.localToEnv.hasKey(local):
    result = c.localToEnv[local].field
  else:
    var name = pool.syms[local]
    extractBasename name
    name.add "`f."
    name.add $c.counter
    inc c.counter
    name.add "."
    name.add c.thisModuleSuffix
    result = pool.syms.getOrIncl(name)
    c.localToEnv[local] = EnvField(objType: typ, field: result, typ: c.typeCache.getType(n))

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  dest.add n
  inc n
  if n.kind == Symbol:
    # if a closure proc is called, we don't want to see it as "escaping".
    dest.add n
    inc n
  while n.kind != ParRi:
    tr(c, dest, n)
  dest.takeParRi(n)

proc trNil(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  if n.kind != ParRi and procHasPragma(n, ClosureP):
    # nil closure must be a tuple:
    dest.copyIntoKind TupconstrX, info:
      dest.takeTree n # type
      if n.kind != ParRi: skip n # might have another nil value
      dest.addParPair NilX, info
      dest.addParPair NilX, info
    skipParRi n
  else:
    dest.addParLe NilX, n.info
    while n.kind != ParRi: takeTree dest, n
    dest.takeParRi n

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, UnknownToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of Symbol:
    let loc = c.typeCache.getLocalInfo(n.symId)
    if loc.kind in {ParamY, LetY, VarY, ResultY}:
      let cross = loc.crossedProc.int
      if cross > 0:
        for i in c.procStack.len - cross ..< c.procStack.len:
          c.closureProcs.incl(c.procStack[i])
        let destEnv = c.procStack[0] #c.procStack.len - cross]
        c.createsEnv.incl destEnv
        let envType = c.envTypeForProc(destEnv)
        let fld = c.localToField(n, n.symId, envType)
        #[

        Problem:

          proc outerA =
            var a: int
            proc outerB =
              var b: int
              proc inner =
                use a # uses env from outerA
                use b # uses env from outerB

        But there is only one environment parameter that `inner` can use
        for the accesses to `a` and `b`. Luckily, we analyse the entire
        `outerA` in one go, so we can use `outerA`'s environment for `outerB`
        too.
        ]#
        dest.copyIntoKind EnvpX, n.info:
          dest.addSymUse envType, n.info
          dest.addSymUse fld, n.info
        inc n
      else:
        takeTree dest, n
    elif loc.kind in {ProcY, FuncY, IteratorY, ConverterY, MethodY}:
      # usage of a closure proc not within a call? --> The closure does escape:
      if c.procStack.len > 0:
        #c.escapes.incl n.symId
        c.escapes.incl c.procStack[0]
      takeTree dest, n
    else:
      takeTree dest, n
  of ParLe:
    case n.stmtKind
    of LocalDecls:
      trLocal c, dest, n
    of ProcS, FuncS, MacroS, MethodS, ConverterS:
      trProc c, dest, n
    of IteratorS, TemplateS, TypeS, EmitS, BreakS, ContinueS,
      ForS, IncludeS, ImportS, FromimportS, ImportExceptS,
      ExportS, CommentS,
      PragmasS:
      takeTree dest, n
    of ScopeS:
      c.typeCache.openScope()
      trSons(c, dest, n)
      c.typeCache.closeScope()
    else:
      case n.exprKind
      of CallKinds:
        trCall c, dest, n
      of TypeofX:
        takeTree dest, n
      of NilX:
        trNil c, dest, n
      else:
        trSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc isClosure(typ: Cursor): bool {.inline.} = procHasPragma(typ, ClosureP)

when false:
  proc paramsWithClosurePragma(typ: Cursor): bool =
    var typ = typ
    skip typ
    skip typ # return type
    result = hasPragma(typ, ClosureP)

const
  RootObjName = "RootObj.0." & SystemModuleSuffix
  EnvParamName = "`ep.0"
  EnvLocalName = "`el.0"

proc addRootRef(dest: var TokenBuf; info: PackedLineInfo) =
  dest.copyIntoKind RefT, info:
    dest.addSymUse pool.syms.getOrIncl(RootObjName), info

type
  UntypedEnvMode = enum
    WantValue, WantAddr

proc untypedEnv(dest: var TokenBuf; info: PackedLineInfo; env: CurrentEnv; mode=WantValue) =
  assert env.s != SymId(0)
  case env.mode
  of EnvIsLocal:
    dest.copyIntoKind CastX, info:
      if env.needsHeap:
        dest.addRootRef info
      else:
        dest.copyIntoKind PointerT, info: discard
      if mode == WantAddr:
        dest.copyIntoKind AddrX, info:
          dest.addSymUse env.s, info
      else:
        dest.addSymUse env.s, info
  of EnvIsParam:
    # the parameter already has the erased type:
    if mode == WantAddr:
      dest.copyIntoKind AddrX, info:
        dest.addSymUse env.s, info
    else:
      dest.addSymUse env.s, info

proc typedEnv(dest: var TokenBuf; info: PackedLineInfo; env: CurrentEnv) =
  assert env.s != SymId(0)
  case env.mode
  of EnvIsLocal:
    # the local already has the full type:
    dest.addSymUse env.s, info
  of EnvIsParam:
    # the parameter has the erased type:
    dest.copyIntoKind CastX, info:
      dest.copyIntoKind (if env.needsHeap: RefT else: PtrT), info:
        dest.addSymUse env.typ, info
      dest.addSymUse env.s, info

proc tre(c: var Context; dest: var TokenBuf; n: var Cursor)

proc treSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.kind != ParRi:
      tre(c, dest, n)

proc treLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let s = n.firstSon.symId
  let fld = c.localToEnv.getOrDefault(s)
  let kind = n.symKind
  if fld.field != SymId(0):
    # the local is already a field of an environment object
    let info = n.info
    inc n # into the decl
    let name = n.symId
    for i in 1..3: skip n
    # register the local anyway to keep the type navigator happy:
    c.typeCache.registerLocal(name, kind, n)
    skip n # type
    if n.kind != DotToken:
      # generate an assignment:
      dest.copyIntoKind AsgnS, info:
        dest.copyIntoKind DotX, info:
          if c.env.needsHeap:
            dest.copyIntoKind DerefX, info:
              dest.typedEnv info, c.env
          else:
            dest.typedEnv info, c.env
          dest.addSymUse fld.field, info
        tre c, dest, n # value
    skipParRi n
  else:
    copyInto dest, n:
      let name = n.symId
      takeTree dest, n # name
      takeTree dest, n # export marker
      takeTree dest, n # pragmas
      c.typeCache.registerLocal(name, kind, n)
      tre c, dest, n # type (might grow an environment parameter)
      tre c, dest, n # value

proc addEnvParam(dest: var TokenBuf; info: PackedLineInfo; envTyp: SymId) =
  dest.copyIntoKind ParamU, info:
    dest.addSymDef pool.syms.getOrIncl(EnvParamName), info
    dest.addDotToken() # no export marker
    dest.addDotToken() # no pragmas
    if envTyp == SymId(0):
      dest.copyIntoKind RefT, info:
        dest.addSymUse pool.syms.getOrIncl(RootObjName), info
    else:
      # to keep NIFC's type system happy we need a ptr type here
      # and then a cast in the body!
      dest.copyIntoKind PointerT, info: discard
    dest.addDotToken() # no default value

proc treParams(c: var Context; dest, init: var TokenBuf; n: var Cursor; doAddEnvParam: bool; envTyp: SymId) =
  copyInto dest, n:
    while n.kind != ParRi:
      assert n.substructureKind == ParamU
      copyInto dest, n:
        let name = n.symId
        takeTree dest, n # name
        takeTree dest, n # export marker
        takeTree dest, n # pragmas
        c.typeCache.registerLocal(name, ParamY, n)
        tre c, dest, n # type (might grow an environment parameter)
        tre c, dest, n # value

        # parameter might have been captured:
        let fld = c.localToEnv.getOrDefault(name)
        if fld.field != SymId(0):
          # XXX Check here for memory safety violations: Cannot capture a `var T` parameter
          init.copyIntoKind AsgnS, n.info:
            init.copyIntoKind DotX, n.info:
              if envTyp != SymId(0):
                init.copyIntoKind DerefX, n.info:
                  init.typedEnv n.info, c.env
              else:
                init.typedEnv n.info, c.env
              init.addSymUse fld.field, n.info
            init.addSymUse name, n.info

    if doAddEnvParam:
      addEnvParam dest, n.info, envTyp

proc treProcBody(c: var Context; dest, init: var TokenBuf; n: var Cursor; sym: SymId; needsHeap: bool) =
  if n.stmtKind == StmtsS:
    copyInto dest, n:
      let oldEnv = c.env
      if c.createsEnv.contains(sym):
        let envTyp = c.envTypeForProc(sym)
        c.env = CurrentEnv(s: pool.syms.getOrIncl(EnvLocalName), mode: EnvIsLocal, typ: envTyp, needsHeap: needsHeap)
        dest.copyIntoKind VarS, NoLineInfo:
          dest.addSymDef c.env.s, NoLineInfo
          dest.addDotToken() # no export marker
          dest.addDotToken() # no pragmas
          if needsHeap:
            dest.copyIntoKind RefT, NoLineInfo:
              dest.addSymUse c.env.typ, NoLineInfo
            dest.copyIntoKind NewobjX, NoLineInfo:
              dest.copyIntoKind RefT, NoLineInfo:
                dest.addSymUse c.env.typ, NoLineInfo
          else:
            dest.addSymUse c.env.typ, NoLineInfo
            dest.addDotToken() # no default value
        if needsHeap:
          # Note: If the environment is on the stack, a single `wasMoved`
          # hook will be generated for it so we don't need to do anything here.
          # Otherwise, we need to init the environment via the `=wasMoved` hooks:
          for _, field in c.localToEnv:
            if field.objType == c.env.typ:
              dest.copyIntoKind WasmovedX, NoLineInfo:
                dest.copyIntoKind HaddrX, NoLineInfo:
                  dest.copyIntoKind DotX, NoLineInfo:
                    if needsHeap:
                      dest.copyIntoKind DerefX, NoLineInfo:
                        dest.addSymUse c.env.s, NoLineInfo
                    else:
                      dest.addSymUse c.env.s, NoLineInfo
                    dest.addSymUse field.field, NoLineInfo

      elif c.closureProcs.contains(sym):
        c.env = CurrentEnv(s: pool.syms.getOrIncl(EnvParamName), mode: EnvIsParam, typ: c.envTypeForProc(sym), needsHeap: needsHeap)
      else:
        c.env = CurrentEnv(s: SymId(0), mode: EnvIsParam, typ: SymId(0), needsHeap: needsHeap)
      dest.add init
      while n.kind != ParRi:
        tre(c, dest, n)
      var needsHeapB = c.env.needsHeap
      c.env = oldEnv
      c.env.needsHeap = c.env.needsHeap or needsHeapB
  else:
    tre(c, dest, n)

proc treProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var init = createTokenBuf(10)
  let decl = n
  copyInto dest, n:
    var isConcrete = true # assume it is concrete
    let sym = n.symId
    c.procStack.add(sym)
    let closureOwner = c.procStack[0]
    let needsHeap = c.escapes.contains(closureOwner)
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.openProcScope(sym, decl, n)
        let envType = if needsHeap: SymId(0) else: c.envTypeForProc(closureOwner)
        treParams c, dest, init, n, c.closureProcs.contains(sym), envType
      else:
        if i == TypevarsPos:
          isConcrete = n.substructureKind != TypevarsU
        if i == ReturnTypePos and isConcrete:
          tre c, dest, n
        else:
          takeTree dest, n

    if isConcrete:
      treProcBody(c, dest, init, n, sym, needsHeap)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()

proc treParamsWithEnv(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.kind != ParRi:
      tre(c, dest, n)
    addEnvParam dest, NoLineInfo, SymId(0)

proc isStaticCall(c: var Context;s: SymId): bool =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    let fn = asRoutine(res.decl)
    result = isRoutine(fn.kind)
  else:
    let local = c.typeCache.getLocalInfo(s)
    result = isRoutine(local.kind)

proc genCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.add n # the call node itself
  inc n
  #var fn = n
  let typ = c.typeCache.getType(n, {SkipAliases})
  let wantsEnv = isClosure(typ) or (n.kind == Symbol and c.closureProcs.contains(n.symId))
  var isStatic = false
  var tmp = SymId(0)
  if wantsEnv:
    isStatic = n.kind == Symbol and isStaticCall(c, n.symId)
    if isStatic:
      # do not produce a tuple:
      dest.add n
      inc n
    elif n.kind == Symbol:
      tmp = n.symId
      copyIntoKind dest, TupatX, info:
        #tre c, dest, n
        dest.add n
        dest.addIntLit 0, info
      inc n
    else:
      dest.addParLe(ExprX, info)
      copyIntoKind dest, StmtsS, info:
        tmp = pool.syms.getOrIncl("`llTemp." & $c.counter)
        inc c.counter
        copyIntoKind dest, VarS, info:
          dest.addSymDef tmp, info
          dest.addDotToken() # no export marker
          dest.addDotToken() # no pragmas
          var t = typ
          tre c, dest, t
          tre c, dest, n # value
      dest.addSymUse tmp, info
      dest.addParRi() # ExprX
  while n.kind != ParRi:
    tre(c, dest, n)
  if wantsEnv:
    if isStatic:
      if c.env.s != SymId(0):
        let mode = if c.env.needsHeap: WantValue else: WantAddr
        # use the current environment as the last parameter:
        untypedEnv dest, info, c.env, mode
      else:
        # can happen for toplevel closures that have been declared .closure for interop
        # We have no environment here, so pass `nil` instead:
        dest.copyIntoKind NilX, info: discard
    else:
      # unpack the tuple:
      assert tmp != SymId(0)
      copyIntoKind dest, TupatX, info:
        dest.addSymUse tmp, info
        dest.addIntLit 1, info
  dest.addParRi()
  skipParRi n

proc treProcType(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if isClosure(n):
    # type is really a tuple:
    let info = n.info
    copyIntoKind dest, TupleT, info:
      copyIntoKind dest, ProctypeT, info:
        for i in 1..ParamsPos: dest.addDotToken()
        let usesWrapper = n.typeKind in RoutineTypes
        if usesWrapper:
          inc n
          for i in 1..4: skip n
        if n.substructureKind == ParamsU:
          treParamsWithEnv(c, dest, n)
        else:
          assert n.kind == DotToken
          inc n
          dest.addParLe ParamsU, info
          addEnvParam dest, info, SymId(0)
          dest.addParRi()
        tre c, dest, n # return type
        # pragmas:
        tre c, dest, n
        if usesWrapper:
          # effects and body, deliberately made flexible here for future changes
          # as it's messy to work with.
          if n.kind != ParRi:
            skip n
            if n.kind != ParRi: skip n
          skipParRi n
      copyIntoKind dest, RefT, info:
        dest.addSymUse pool.syms.getOrIncl(RootObjName), info
  else:
    treSons(c, dest, n)

proc toProcType(c: var Context; dest: var TokenBuf; n: Cursor) =
  var n = n
  let info = n.info
  copyIntoKind dest, ProctypeT, info:
    inc n
    for i in 1..ParamsPos:
      dest.addDotToken()
      skip n
    copyIntoKind dest, ParamsU, n.info:
      if n.kind == DotToken:
        inc n
      else:
        inc n
        while n.kind != ParRi:
          tre c, dest, n # params
        skipParRi n
      addEnvParam dest, info, SymId(0)
    tre c, dest, n # return type
    # pragmas:
    tre c, dest, n
    while n.kind != ParRi: skip n
    skipParRi n

proc tre(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    # is this the usage of a proc symbol that is a closure? If so,
    # turn it into a `(fn, env)` tuple and generate the environment.
    let origTyp = c.typeCache.getType(n, {SkipAliases})
    let info = n.info
    if origTyp.typeKind in RoutineTypes and isClosure(origTyp) and c.typeCache.fetchSymKind(n.symId) in RoutineKinds:
      dest.copyIntoKind TupconstrX, info:
        dest.copyIntoKind TupleT, info:
          c.toProcType(dest, origTyp)
          dest.addRootRef info
        dest.addSymUse n.symId, info
        dest.untypedEnv info, c.env
      inc n
    else:
      let repWith = c.localToEnv.getOrDefault(n.symId)
      if repWith.field != SymId(0):
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.typedEnv info, c.env
          dest.addSymUse repWith.field, info
        inc n
      else:
        takeTree dest, n
  of DotToken, UnknownToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of ParLe:
    case n.stmtKind
    of LocalDecls:
      treLocal c, dest, n
    of ProcS, FuncS, MacroS, MethodS, ConverterS:
      treProc c, dest, n
    of IteratorS, TemplateS, EmitS, BreakS, ContinueS,
      ForS, IncludeS, ImportS, FromimportS, ImportExceptS,
      ExportS, CommentS,
      PragmasS:
      takeTree dest, n
    of ScopeS:
      c.typeCache.openScope()
      treSons(c, dest, n)
      c.typeCache.closeScope()
    else:
      case n.exprKind
      of CallKinds:
        genCall(c, dest, n)
      of EnvpX:
        let info = n.info
        inc n
        dest.copyIntoKind DotX, info:
          dest.copyIntoKind DerefX, info:
            dest.copyIntoKind CastX, info:
              dest.copyIntoKind (if c.env.needsHeap: RefT else: PtrT), info:
                dest.takeTree n # type
              dest.addSymUse c.env.s, info
          assert n.kind == Symbol
          dest.takeTree n # the symbol
        skipParRi n
      of TypeofX:
        takeTree dest, n
      else:
        if n.typeKind in RoutineTypes:
          treProcType(c, dest, n)
        else:
          treSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc genObjectTypes(c: var Context; dest: var TokenBuf) =
  var objectTypes = initTable[SymId, seq[EnvField]]()
  for local, field in c.localToEnv:
    objectTypes.mgetOrPut(field.objType, @[]).add(field)
  for objType, fields in objectTypes:
    let beforeType = dest.len
    dest.copyIntoKind TypeS, NoLineInfo:
      dest.addSymDef objType, NoLineInfo
      dest.addDotToken() # no export marker
      dest.addDotToken() # no generic params
      dest.addDotToken() # no pragmas
      dest.copyIntoKind ObjectT, NoLineInfo:
        # inherits from RootObj:
        dest.addSymUse pool.syms.getOrIncl(RootObjName), NoLineInfo
        for field in items fields:
          let beforeField = dest.len
          dest.copyIntoKind FldY, NoLineInfo:
            dest.addSymDef field.field, NoLineInfo
            dest.addDotToken() # no export marker
            dest.addDotToken() # no pragmas
            var n = field.typ
            tre(c, dest, n) # type might need an environment parameter
            dest.addDotToken() # no default value
          programs.publish(field.field, dest, beforeField)
    programs.publish(objType, dest, beforeType)

proc elimLambdas*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix)
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  tr c, result, n
  c.typeCache.closeScope()

  # second pass: generate environments
  if c.localToEnv.len > 0:
    # some closure usage has been found, so we need to generate environments
    c.typeCache.openScope()
    let cap = result.len
    var oldResult = move result
    result = createTokenBuf(cap)
    var n = beginRead(oldResult)
    assert n.stmtKind == StmtsS
    result.add n # stmts
    inc n
    genObjectTypes(c, result)
    while n.kind != ParRi:
      tre(c, result, n)
    result.takeParRi n
    endRead(oldResult)
    c.typeCache.closeScope()

  #echo "PRODUCED ", toString(result, false)

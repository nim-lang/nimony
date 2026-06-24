#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Sem-checking of pragmas: routine/type pragma lists (`semPragma`/`semPragmas`),
## pragma statements and blocks (`{.push.}`, `{.emit.}`, `{.assert.}`, …) and
## pragma expressions / propositions.
##
## Formerly textually `include`d into sem.nim; now a separate module. It
## re-enters the sem core only through callbacks on `SemContext`; the shims
## below restore the original names so the bodies read unchanged.

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
  import std / syncio
import std / [tables, sets, hashes, assertions, strutils]
from std/os import changeFileExt, getCurrentDir, isAbsolute, absolutePath, normalizedPath, splitFile, extractFilename, `/`
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser
import nimony_model, builtintypes, decls, asthelpers, programs,
  magics, nifconfig, semdata, sembasics,
  semchecks, semconst, semos, renderer, features, pragmacanon, identstyle,
  symtabs

# --- thin shims forwarding into the sem core via SemContext callbacks ---
# (const-eval entry points — semBoolExpr, semConstIntExpr, … — come directly
# from the `semconst` module imported above, so they need no callbacks.)

proc semStmt(c: var SemContext; dest: var TokenBuf; n: var Cursor; isNewScope: bool) =
  c.semStmtCB(c, dest, n, isNewScope)

proc declareResult(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo): SymId =
  c.declareResultCB(c, dest, info)

proc semEmit(c: var SemContext; dest: var TokenBuf; it: var Item) =
  c.semEmitCB(c, dest, it)

proc semLocalTypeImpl(c: var SemContext; dest: var TokenBuf; n: var Cursor; context: TypeDeclContext) =
  c.semLocalTypeImplCB(c, dest, n, context, false, SymId(0))

proc semLocalType(c: var SemContext; dest: var TokenBuf; n: var Cursor; context = InLocalDecl): TypeCursor =
  let insertPos = dest.len
  semLocalTypeImpl c, dest, n, context
  result = typeToCursor(c, dest, insertPos)

# --- handlers (moved verbatim from sem.nim) ---

proc symbolIsCustomPragmaTemplate(s: SymId): bool =
  let loaded = tryLoadSym(s)
  result = loaded.status == LacksNothing and
           loaded.decl.symKind == TemplateY and
           hasPragma(asRoutine(loaded.decl).pragmas, PragmaP)

proc customPragmaSym*(c: SemContext; name: StrId): SymId =
  ## The symbol of the `template name {.pragma.}` custom pragma in scope, or
  ## `NoSymId`. Used to preserve a custom pragma annotation as `(pragma <sym>)`
  ## on a decl (so plugins can introspect it) instead of dropping it.
  let ignoreStyle = IgnoreStyleFeature in c.features
  var scope = c.currentScope
  while scope != nil:
    for k in stylesOfScope(scope, name, ignoreStyle):
      for sym in scope.tab.getOrDefault(k):
        if sym.kind == TemplateY and symbolIsCustomPragmaTemplate(sym.name):
          return sym.name
    scope = scope.up

  for realName in stylesOfImport(c.importTab, name, ignoreStyle):
    for moduleId in c.importTab.getOrDefault(realName):
      if c.importedModules.hasKey(moduleId):
        let imported = c.importedModules.getOrQuit(moduleId)
        for foreignName in stylesOfIface(imported.iface, realName, ignoreStyle):
          for symId in imported.iface.getOrDefault(foreignName):
            if symbolIsCustomPragmaTemplate(symId):
              return symId
  result = NoSymId

proc isCustomPragmaTemplate*(c: SemContext; name: StrId): bool =
  name in c.customPragmaTemplates or customPragmaSym(c, name) != NoSymId

proc isPreservedCustomPragma(n: Cursor): bool =
  ## True when `n` is a previously-preserved custom-pragma attachment
  ## `(pragma <sym>)` (the `pragma` tag with a symbol child), as opposed to the
  ## bare `(pragma)` marker on a custom-pragma template declaration.
  if n.kind == ParLe and n.pragmaKind == PragmaP:
    var probe = n
    inc probe
    result = probe.kind == Symbol
  else:
    result = false

proc semProposition*(c: var SemContext; dest: var TokenBuf; n: var Cursor; kind: PragmaKind) =
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

proc resolveHeaderPath*(raw: string; currentFile: string; config: NifConfig): string =
  ## Resolves header pragma paths. Only converts to absolute when ${path} or
  ## ${nifcache} is used; other headers (e.g. "bar.h", "<stdio.h>") stay as-is.
  if raw.len == 0 or raw[0] in {'<', '#'}: return raw
  if find(raw, "${path}") < 0 and find(raw, "${nifcache}") < 0: return raw
  let resolvedFile = onRaiseQuit:
    if currentFile.isAbsolute: absolutePath(currentFile)
    elif config.baseDir.len > 0 and '/' notin currentFile and '\\' notin currentFile: absolutePath(normalizedPath(joinPath(config.baseDir, currentFile)))
    else: absolutePath(currentFile)
  result = replaceSubs(raw, resolvedFile, config)
  result = onRaiseQuit toAbsolutePath(result, absoluteParentDir(resolvedFile))

proc semPragma*(c: var SemContext; dest: var TokenBuf; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  var hasParRi = n.kind == ParLe # if false, has no arguments
  if n.substructureKind == KvU:
    inc n
  var pk = pragmaKind(n)
  if pk == NoPragma and n.kind == Ident and IgnoreStyleFeature in c.features:
    # Under `.feature: "ignoreStyle".` accept builtin pragma names spelled in
    # any case / underscore variant (Nim's `cmpIgnoreStyle` rule). The
    # downstream case branches still emit the canonical form into `dest`, so
    # the lowering pipeline never sees the user-written spelling.
    pk = pragmaKindByStyle(n.litId)
  case pk
  of NoPragma:
    # `cc` bound inside the `and` is only conditionally initialized, which the
    # init analysis cannot see through; compute it up front so it is always set.
    let cc = if kind.isRoutine: callConvKind(n) else: NoCallConv
    if cc != NoCallConv:
      dest.addParLe(cc, n.info)
      inc n
      dest.addParRi()
    elif n.kind == ParLe and kind == TypeY and (let hk = hookKind(n.tagId); hk != NoHook):
      dest.takeTree n
      hasParRi = false
    else:
      let name = getIdent(n)
      if name != StrId(0) and c.userPragmas.hasKey(name) and not hasParRi:
        # custom pragma, cannot have arguments
        inc n
        let pragBuf = addr c.userPragmas.getOrQuit(name)
        var read = beginRead(pragBuf[])
        while read.hasMore:
          semPragma c, dest, read, crucial, kind
        endRead(pragBuf[])
      elif name != StrId(0) and (let psym = c.customPragmaSym(name); psym != NoSymId):
        # Pragma that resolves to a `template X {.pragma.}` declaration. Unlike
        # Nim (which drops `sfCustomPragma`), preserve it as `(pragma <sym>)`
        # so it survives into the serialized decl and plugins can introspect it
        # (e.g. `.linear`). Arguments are not yet supported and are dropped.
        let info = n.info
        inc n
        if hasParRi:
          while n.hasMore: skip n
        dest.add parLeToken(PragmaP, info)
        dest.add symToken(psym, info)
        dest.addParRi()
      else:
        buildErr c, dest, n.info, "expected pragma"
        inc n
        if hasParRi:
          while n.hasMore: skip n # skip optional pragma arguments
  of MagicP:
    dest.add parLeToken(MagicP, n.info)
    inc n
    if hasParRi and n.kind in {StringLit, Ident}:
      let (magicWord, bits) = magicToTag(pool.strings[n.litId], c.g.config.bits)
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
    if hasParRi and n.hasMore:
      semConstStrExprIgnoreTopLevel c, dest, n
    dest.addParRi()
  of ImportcP, ImportcppP, ExportcP, HeaderP, DynlibP:
    crucial.flags.incl pk
    let info = n.info
    dest.add parLeToken(pk, info)
    inc n
    let strPos = dest.len
    if hasParRi and n.hasMore:
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
        let raw = pool.strings[tok.litId]
        let name = resolveHeaderPath(raw, info.getFile(), c.g.config)
        if name != raw:
          dest[idx] = strToken(pool.strings.getOrIncl(name), tok.info)
      crucial.headerFileTok = dest[idx]
    # Finalize expression
    dest.addParRi()
  of PluginP:
    # `.plugin: "path"` — single-string form. (The historical
    # `("path", "<version>")` tuple form, which selected between the Nim 2
    # and Nimony compilers, was removed when the Nim 2 plugin path went away.)
    crucial.flags.incl pk
    let pragInfo = n.info
    inc n
    var path = StrId(0)
    var pathInfo = n.info
    var errMsg = ""
    var alreadyErr = false
    if hasParRi:
      if n.kind == StringLit:
        path = n.litId
        pathInfo = n.info
        inc n
      elif n.exprKind == ErrX:
        # Re-sem path: a previous sem pass already produced an err. Pass
        # through without re-reporting.
        alreadyErr = true
        var passBuf = createTokenBuf(8)
        passBuf.takeTree n
        dest.add parLeToken(pk, pragInfo)
        dest.add passBuf
        dest.addParRi()
      else:
        errMsg = "plugin path must be a string literal"
        if n.hasMore: skip n
    if not alreadyErr:
      dest.add parLeToken(pk, pragInfo)
      if errMsg.len > 0:
        buildErr c, dest, pathInfo, errMsg
      elif path != StrId(0):
        dest.add strToken(path, pathInfo)
      dest.addParRi()
  of AlignP, BitsP, SizeP:
    dest.add parLeToken(pk, n.info)
    inc n
    let valueStart = dest.len
    if hasParRi and n.hasMore:
      semConstIntExpr(c, dest, n, SemcheckBodies)
    else:
      buildErr c, dest, n.info, "expected int literal"
    if pk == SizeP and dest[valueStart].kind == IntLit:
      crucial.size = int(pool.integers[dest[valueStart].intId])
    dest.addParRi()
  of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoreturnP, BorrowP,
     NoSideEffectP, NodestroyP, BycopyP, ByrefP, InlineP, NoinlineP, NoinitP,
     InjectP, GensymP, DirtyP, UntypedP, SideEffectP, BaseP, ClosureP, PassiveP, IncompleteStructP:
    crucial.flags.incl pk
    dest.add parLeToken(pk, n.info)
    dest.addParRi()
    inc n
  of ViewP, InheritableP, PureP, FinalP, PackedP, UnionP, AcyclicP:
    var hasErr = false
    if kind != TypeY:
      buildErr c, dest, n.info, $pk & " pragma is only allowed on types"
      hasErr = true
    elif pk in {ViewP, InheritableP, FinalP, PackedP, UnionP, AcyclicP}:
      var n2 = n
      while n2.hasMore: skip n2
      consumeParRi n2
      if n2.typeKind in {RefT, PtrT}:
        inc n2
      # Later passes replace the inline body of `ref object` / `ptr object`
      # with a symbol that stands for the synthesized inner object type; accept
      # that form as valid — the first pass has already validated the shape.
      if n2.kind != Symbol and n2.typeKind != ObjectT:
        buildErr c, dest, n.info, $pk & " pragma is only allowed on object types", n
        hasErr = true
    if not hasErr:
      dest.add parLeToken(pk, n.info)
      dest.addParRi()
    inc n
  of CursorP:
    if kind in {VarY, LetY, CursorY, FldY, GfldY}:
      # On object fields, `.cursor` marks the field as a non-owning alias:
      # the lifter's `unravelObjField` already special-cases such fields
      # (no recursive destroy/dup), and `trObjConstr`/`trNewobjFields` in
      # the duplifier emit `WantNonOwner` reads for them so no `=dup` is
      # spliced around the value at construction time.
      dest.add parLeToken(pk, n.info)
      inc n
    else:
      buildErr c, dest, n.info, "pragma only allowed on local variables or object fields"
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
    if hasParRi and n.hasMore:
      semProposition c, dest, n, pk
    else:
      buildErr c, dest, n.info, "`requires`/`ensures` pragma takes a bool expression"
    dest.addParRi()
  of TagsP:
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.hasMore:
      takeTree dest, n
    else:
      buildErr c, dest, n.info, "expected tags/raises list"
    dest.addParRi()
  of CastP:
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.hasMore:
      takeTree dest, n
    else:
      buildErr c, dest, n.info, "expected `cast` pragma expression"
    dest.addParRi()
  of ProfilerP, StacktraceP, GcsafeP, UsedP:
    # accepted for Nim source compatibility; semantically ignored by Nimony
    inc n
    if hasParRi:
      while n.hasMore: skip n
  of UncheckedAssignP:
    buildErr c, dest, n.info, "`uncheckedAssign` is only valid inside `{.cast(uncheckedAssign).}:` pragma blocks"
    inc n
    if hasParRi:
      while n.hasMore: skip n
  of UncheckedAccessP:
    buildErr c, dest, n.info, "`uncheckedAccess` is only valid inside `{.cast(uncheckedAccess).}:` pragma blocks"
    inc n
    if hasParRi:
      while n.hasMore: skip n
  of RaisesP:
    crucial.flags.incl pk
    let oldLen = dest.len
    dest.add parLeToken(pk, n.info)
    inc n
    if hasParRi and n.hasMore:
      var nn = n
      let typeStart = dest.len
      # Sem-check the type properly
      crucial.raisesType = semLocalType(c, dest, n)
      # TODO: validate that type supports "x != default(T)" interpretation
      dest.addParRi()
      if nn.exprKind == BracketX and nn.firstSon.kind == ParRi:
        # `raises: []` means "does not raise":
        crucial.flags.excl pk
        dest.shrink oldLen
        crucial.raisesType = default(TypeCursor)
    else:
      # No type specified - default to system.ErrorCode
      let typeStart = dest.len
      dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), n.info
      crucial.raisesType = c.typeToCursor(dest, typeStart)
      dest.addParRi()
  of CallConvP:
    inc n
    if hasParRi and n.kind == Ident:
      let cc = callConvKind(n)
      if cc != NoCallConv:
        dest.addParLe(cc, n.info)
        inc n
        dest.addParRi()
      else:
        buildErr c, dest, n.info, "unknown calling convention"
        inc n
    else:
      buildErr c, dest, n.info, "`callConv` pragma takes a calling convention identifier"
  of EmitP, BuildP, BundleP, CompileP, StringP, AssumeP, AssertP, PragmaP, PushP, PopP, PassLP, PassCP:
    if pk == PragmaP and kind == TemplateY and crucial.sym != SymId(0):
      # `template X(args) {.pragma.}` declares `X` as a custom pragma. The
      # body is not expanded at attachment sites — the annotation is
      # recorded as a known custom-pragma name that will be silently
      # accepted (and dropped) wherever it is later attached. Mirrors Nim's
      # `sfCustomPragma`.
      let info = n.info
      inc n
      if hasParRi and n.hasMore:
        buildErr c, dest, info, "`pragma` takes no arguments"
        while n.hasMore: skip n
      else:
        var basename = pool.syms[crucial.sym]
        extractBasename basename
        c.customPragmaTemplates.incl pool.strings.getOrIncl(basename)
        dest.add parLeToken(PragmaP, info)
        dest.addParRi()
    elif pk == PragmaP and isPreservedCustomPragma(n):
      # An already-preserved custom-pragma attachment `(pragma <sym>)`, seen
      # again when a decl's pragmas are re-sem'd across phases / instantiation.
      # Re-emit it so it stays introspectable and idempotent. Consume only the
      # opening tag and the children here, leaving the pragma's own `)` for the
      # shared `if hasParRi: ... skipParRi n` epilogue below; taking the whole
      # tree would let that epilogue skip the *next* `)` (the enclosing
      # `pragmas` closer) and swallow the routine body.
      dest.add parLeToken(PragmaP, n.info)
      inc n
      while n.kind != ParRi:
        takeTree dest, n
      dest.addParRi()
    else:
      buildErr c, dest, n.info, "pragma not supported"
      inc n
      if hasParRi:
        while n.hasMore: skip n # skip optional pragma arguments
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
  of FeatureP:
    buildErr c, dest, n.info, "`feature` pragma is only allowed as top level pragma"
  of MethodsP:
    dest.add parLeToken(pk, n.info)
    inc n
    while n.hasMore:
      dest.takeTree n
    dest.addParRi()
  if hasParRi:
    if n.hasMore:
      if n.exprKind != ErrX:
        buildErr c, dest, n.info, "too many arguments for pragma"
      while n.hasMore: skip n
    skipParRi n

proc semPragmas*(c: var SemContext; dest: var TokenBuf; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  var pragmaOpen = false
  let info = n.info
  if n.kind == DotToken or n.substructureKind == PragmasU:
    if AutoClosuresFeature in c.features and kind in {ProcY, MethodY, FuncY, ConverterY}:
      var isAutoClosure = false
      var it = c.routine.parent
      while it != nil:
        if it.kind in {ProcY, MethodY, FuncY, ConverterY}:
          isAutoClosure = true
          break
        it = it.parent
      if isAutoClosure:
        crucial.flags.incl ClosureP
        dest.add parLeToken(PragmasU, info)
        dest.add parLeToken(ClosureP, info)
        dest.addParRi()
        pragmaOpen = true

    var checkedPragmas = default(CheckedPragmas)
    if n.kind == DotToken:
      inc n
    elif n.substructureKind == PragmasU:
      if not pragmaOpen:
        dest.add parLeToken(PragmasU, info)
        pragmaOpen = true
      n.into PragmasU:
        while n.hasMore:
          if n.exprKind == ErrX:
            takeTree dest, n
          elif n.substructureKind in {NotnilU, NilU, UncheckedU}:
            takeTree dest, n # nil annotations, pass through
          else:
            if checkedPragmas.isChecked(n, kind):
              skip n
            else:
              semPragma c, dest, n, crucial, kind
    for i in 0 ..< c.pragmaStack.len:
      var n2 = beginRead(c.pragmaStack[i])
      while n2.hasMore:
        if checkedPragmas.isChecked(n2, kind):
          skip n2
        else:
          if not pragmaOpen:
            dest.addParLe PragmasU, info
            pragmaOpen = true
          semPragma c, dest, n2, crucial, kind
    # `{.feature: "untyped".}` applies only within the current module, but the
    # relaxed semcheck it enables is needed at every instantiation site. Stamp
    # `UntypedP` onto generics/templates here so the flag travels with the
    # decl and `untypedIsActive` picks it up across module boundaries.
    if UntypedFeature in c.features and kind.isRoutine and c.routine.inGeneric > 0 and
        UntypedP notin crucial.flags:
      if not pragmaOpen:
        dest.addParLe PragmasU, info
        pragmaOpen = true
      crucial.flags.incl UntypedP
      dest.addParLe UntypedP, info
      dest.addParRi()
    if pragmaOpen:
      dest.addParRi()
    else:
      dest.addDotToken()
  else:
    buildErr c, dest, n.info, "expected '.' or 'pragmas'"

proc semAssumeAssert*(c: var SemContext; dest: var TokenBuf; it: var Item; kind: StmtKind) =
  let info = it.n.info
  inc it.n
  dest.addParLe(kind, info)
  semBoolExpr c, dest, it.n
  takeParRi dest, it.n

proc semCastInnerPragma*(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  ## Process a single pragma item inside a `(cast (pragmas ...))` list.
  ## Only `noSideEffect` and `uncheckedAssign` are accepted; the result is
  ## emitted in canonical tag form so later passes can dispatch on the kind.
  let info = n.info
  let pk = n.pragmaKind
  case pk
  of NoSideEffectP, UncheckedAssignP, UncheckedAccessP:
    dest.add parLeToken(pk, info)
    dest.addParRi()
    if n.kind == ParLe: skip n
    else: inc n
  else:
    buildErr c, dest, info, "invalid `cast` pragma argument; expected `noSideEffect`, `uncheckedAssign` or `uncheckedAccess`"
    if n.kind == ParLe: skip n
    else: inc n

proc readPragmaStrings(c: var SemContext; dest: var TokenBuf; it: var Item): seq[string] =
  ## Collect the string-literal arguments of a statement pragma like `build`/
  ## `compile`, advancing `it.n` past the whole `(tag …)` node.
  result = newSeq[string]()
  inc it.n
  while it.n.hasMore:
    if it.n.kind != StringLit:
      buildErr c, dest, it.n.info, "expected `string` but got: " & asNimCode(it.n)
      skip it.n
    else:
      result.add pool.strings[it.n.litId]
      inc it.n
  skipParRi it.n

proc addBuildTarget(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo;
                    lang, rawName, rawArgs: string) =
  ## Resolve a `compile` source path (relative to the pragma's own file)
  ## and record a `(tup lang name args)` entry in `c.toBuild`. `deps.nim` reuses
  ## these via `(build …)` to compile and link the foreign object.
  # XXX: Relative paths in makefile are relative to current working directory, not the location of the makefile.
  let curWorkDir = onRaiseQuit os.getCurrentDir()
  let currentDir = absoluteParentDir(info.getFile)
  var name = replaceSubs(rawName, currentDir, c.g.config).toAbsolutePath(currentDir)
  let customArgs = replaceSubs(rawArgs, currentDir, c.g.config)
  if not semos.fileExists(name):
    buildErr c, dest, info, "cannot find: " & name
  name = name.toRelativePath(curWorkDir)
  c.toBuild.buildTree TupX, info:
    c.toBuild.addStrLit lang, info
    c.toBuild.addStrLit name, info
    c.toBuild.addStrLit customArgs, info

proc addBackendTool(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo;
                    builder, rawTool, rawArgs, rawLinkFlags: string) =
  ## Record a `{.build(builder, tool[, args[, linkflags]]).}` custom-backend entry:
  ## the module carrying this pragma has its Leng IR (`.c.nif`) piped through
  ## `tool` — a standalone program compiled on demand by the generic `builder`
  ## command (e.g. `"nimony c"`, `"nim c"`, `"nimony c --path:…"`; the first
  ## token is the compiler, the rest is passed through verbatim, so neither the
  ## builder nor its subcommand is hardcoded). The optional `linkflags` are
  ## per-file link flags scoped to THIS module's output: `deps.nim` attaches them
  ## as a `(flags …)` child on the module's object in the link manifest, so they
  ## are passed to the linker together with that file (cf. `passL`, which is
  ## global). The whole link step is *not* overridden here — that is what the
  ## separate `.bundle` pragma does. It is stored as
  ## `(tup builder toolSource args linkflags)` in `c.toBuild`, sharing the
  ## `(build …)` channel with `.compile`; `deps.nim`/`processBuild` tells the two
  ## apart by the first field carrying a space-separated builder command vs a
  ## single C/ObjC/Cpp language token. The tool is a *backend*, not a *plugin*:
  ## built like a plugin (compiled on demand) but scheduled like a tool (an
  ## external process that is a node in the build DAG).
  let curWorkDir = onRaiseQuit os.getCurrentDir()
  let currentDir = absoluteParentDir(info.getFile)
  var tool = replaceSubs(rawTool, currentDir, c.g.config).toAbsolutePath(currentDir)
  let customArgs = replaceSubs(rawArgs, currentDir, c.g.config)
  if not semos.fileExists(tool):
    buildErr c, dest, info, "build: cannot find tool source: " & tool
  tool = tool.toRelativePath(curWorkDir)
  # Link flags are passed through verbatim (with `${path}` substitution), NOT a
  # file path — they end up on the linker command line next to this module's `.o`.
  let linkFlags = replaceSubs(rawLinkFlags, currentDir, c.g.config)
  c.toBuild.buildTree TupX, info:
    c.toBuild.addStrLit builder, info
    c.toBuild.addStrLit tool, info
    c.toBuild.addStrLit customArgs, info
    c.toBuild.addStrLit linkFlags, info

proc addBundle(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo;
               builder, rawTool, rawArgs: string) =
  ## Record a `{.bundle(builder, tool[, args]).}` custom-linker entry: `tool` is a
  ## standalone link driver compiled on demand by the generic `builder` command
  ## (same resolution as `.build`'s tool). When *any* module supplies a bundle,
  ## it overrides the final link step — `deps.nim` runs that tool, handing it the
  ## project link manifest (every object/artifact, the app-type, link flags) plus
  ## `args`, and the tool links/bundles the program as it sees fit. Stored as
  ## `(tup builder toolSource args)` in its own `c.toBundle` `(bundle …)` channel.
  let curWorkDir = onRaiseQuit os.getCurrentDir()
  let currentDir = absoluteParentDir(info.getFile)
  var tool = replaceSubs(rawTool, currentDir, c.g.config).toAbsolutePath(currentDir)
  let customArgs = replaceSubs(rawArgs, currentDir, c.g.config)
  if not semos.fileExists(tool):
    buildErr c, dest, info, "bundle: cannot find linker tool source: " & tool
  tool = tool.toRelativePath(curWorkDir)
  c.toBundle.buildTree TupX, info:
    c.toBundle.addStrLit builder, info
    c.toBundle.addStrLit tool, info
    c.toBundle.addStrLit customArgs, info

proc semPragmaLine*(c: var SemContext; dest: var TokenBuf; it: var Item; isPragmaBlock: bool) =
  case it.n.pragmaKind
  of BuildP:
    # Repurposed: `{.build(builder, tool[, args]).}` routes this module's Leng IR
    # through a custom backend `tool` (built by `builder`). NOT the old foreign-
    # source `.build` — that single use (mimalloc) moved to the standard
    # `.compile`.
    let info = it.n.info
    let args = readPragmaStrings(c, dest, it)
    if args.len < 2 or args.len > 4:
      buildErr c, dest, info, "build expected 2 to 4 parameters: (builder, tool[, args[, linkflags]])"
    elif args[0].len == 0:
      buildErr c, dest, info,
        "build: the builder command (e.g. \"nimony c\" or \"nim c\") must not be empty"
    else:
      addBackendTool c, dest, info, args[0], args[1],
        (if args.len >= 3: args[2] else: ""),
        (if args.len >= 4: args[3] else: "")
  of BundleP:
    # `{.bundle(builder, tool[, args]).}` overrides the final link step with a
    # custom link driver `tool` (built by `builder`). Distinct from `.build`,
    # which routes a module's Leng IR through a backend tool.
    let info = it.n.info
    let args = readPragmaStrings(c, dest, it)
    if args.len < 2 or args.len > 3:
      buildErr c, dest, info, "bundle expected 2 to 3 parameters: (builder, tool[, args])"
    elif args[0].len == 0:
      buildErr c, dest, info,
        "bundle: the builder command (e.g. \"nimony c\" or \"nim c\") must not be empty"
    else:
      addBundle c, dest, info, args[0], args[1],
        (if args.len >= 3: args[2] else: "")
  of CompileP:
    # Nim-compatible `{.compile("file"[, "flags"]).}`. Unlike `build` there is no
    # explicit language argument: it is inferred from the file extension and
    # forced via `-x` (which precedes the input in the `cc` command), so e.g. an
    # Objective-C `.m` file is compiled correctly regardless of the C compiler's
    # own extension heuristics.
    let info = it.n.info
    let args = readPragmaStrings(c, dest, it)
    if args.len != 1 and args.len != 2:
      buildErr c, dest, info, "compile expected 1 or 2 parameters"
    else:
      let userArgs = if args.len == 2: args[1] else: ""
      let ext = args[0].splitFile.ext.toLowerAscii
      # Plain `var`s (not a tuple-`let` bound to a `case`-expression): the
      # self-hosted compiler's initialization analysis is conservative about the
      # temporaries such expressions lower to.
      var lang = "C"
      var xflag = ""
      case ext
      of ".m": lang = "ObjC"; xflag = "-x objective-c"
      of ".mm": lang = "ObjCpp"; xflag = "-x objective-c++"
      of ".cpp", ".cc", ".cxx", ".c++": lang = "Cpp"; xflag = "-x c++"
      else: discard
      var customArgs = userArgs
      if xflag.len > 0:
        customArgs = if userArgs.len > 0: xflag & " " & userArgs else: xflag
      addBuildTarget c, dest, info, lang, args[0], customArgs
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
  of CastP:
    if not isPragmaBlock:
      buildErr c, dest, it.n.info, "`cast` pragma must be used in a pragma block"
      skip it.n
    else:
      let info = it.n.info
      let hasPragmasSub = it.n.firstSon.substructureKind == PragmasU
      dest.add parLeToken(CastP, info)
      dest.add parLeToken(PragmasS, info)
      inc it.n # past CastP head
      if hasPragmasSub:
        inc it.n # past inner PragmasS head
      elif it.n.kind == DotToken:
        # need because parser produces `.` with unknown-type cast expr but it
        # is not part of the cast pragma
        inc it.n
      while it.n.hasMore:
        semCastInnerPragma c, dest, it.n
      skipParRi it.n # close (pragmas) of canonical form, or (cast) of the raw form
      if hasPragmasSub:
        skipParRi it.n # close (cast)
      dest.addParRi() # close (pragmas)
      dest.addParRi() # close (cast)
  of PluginP:
    # `.plugin: "path"` — single-string form. (The historical
    # `("path", "<version>")` tuple form was removed when the Nim 2 plugin
    # compile path went away.)
    let pragInfo = it.n.info
    inc it.n
    var path = StrId(0)
    var pathInfo = it.n.info
    var errMsg = ""
    if it.n.kind == StringLit:
      path = it.n.litId
      pathInfo = it.n.info
      inc it.n
    else:
      errMsg = "plugin path must be a string literal"
      if it.n.hasMore: skip it.n
    if path != StrId(0) and errMsg.len == 0:
      if c.routine.inGeneric == 0 and path notin c.pluginBlacklist:
        c.pendingModulePlugins.add PluginObj(path: path, info: pathInfo)
    skipParRi it.n                      # close the original (plugin ...)
    dest.add parLeToken(PragmasS, pragInfo)
    dest.add parLeToken(PluginP, pragInfo)
    if errMsg.len > 0:
      buildErr c, dest, pathInfo, errMsg
    elif path != StrId(0):
      dest.add strToken(path, pathInfo)
    dest.addParRi()                     # close (plugin
    dest.addParRi()                     # close (pragmas
  of PragmaP:
    dest.add parLeToken(PragmasS, it.n.info)
    dest.add parLeToken(PragmaP, it.n.info)
    inc it.n
    let name = takeIdent(it.n)
    if name == StrId(0):
      buildErr c, dest, it.n.info, "expected identifier for pragma"
      takeParRi dest, it.n
      while it.n.hasMore:
        takeTree dest, it.n
    else:
      var buf = createTokenBuf(16)
      dest.add identToken(name, it.n.info)
      takeParRi dest, it.n
      # take remaining pragmas:
      while it.n.hasMore:
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
      var buf = createTokenBuf(16)
      while n.hasMore:
        buf.addSubtree n
        skip n
      buf.addParRi() # sentinel to stop iteration
      c.pragmaStack.add buf
    # semcheck push/pop pragmas in both SemcheckSignatures and SemcheckBodies phases
    # so that pushed pragmas works for both procs and variables
    if c.phase == SemcheckBodies:
      skipUntilEnd it.n
    else:
      dest.addParLe PragmasS, it.n.info
      dest.takeToken it.n
      while it.n.hasMore:
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
  of FeatureP:
    inc it.n
    let info = it.n.info
    let start = dest.len
    let s = evalConstStrExpr(c, dest, it.n, c.types.stringType)
    if s != StrId(0):
      dest.shrink start
      let features = parseFeatures(pool.strings[s])
      if features == {}:
        skipUntilEnd it.n
        buildErr c, dest, info, "unknown `feature`"
      else:
        c.features.incl features
        skipParRi it.n
    else:
      skipUntilEnd it.n
      buildErr c, dest, info, "`feature` pragma takes a string literal"
  else:
    buildErr c, dest, it.n.info, "unsupported pragma", it.n
    skip it.n
    while it.n.hasMore: skip it.n

proc semPragmasLine*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  it.n.into:
    while it.n.hasMore:
      if it.n.kind == ParLe:
        if it.n.stmtKind in CallKindsS or
            it.n.substructureKind == KvU:
          inc it.n
      semPragmaLine c, dest, it, false
  producesVoid c, dest, info, it.typ # in case it was not already produced

proc hasCastUncheckedAccess(n: Cursor): bool =
  ## Scan the pragma list to see if it contains `{.cast(uncheckedAccess).}`.
  result = false
  var scan = n
  var nested = 0
  while true:
    case scan.kind
    of ParLe:
      if scan.pragmaKind == UncheckedAccessP:
        return true
      inc nested
      inc scan
    of ParRi:
      dec nested
      if nested == 0: return false
      inc scan
    else:
      inc scan

proc semPragmaExpr*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  dest.takeToken it.n
  assert it.n.stmtKind == PragmasS
  let hasUncheckedAccess = hasCastUncheckedAccess(it.n.firstSon)
  dest.takeToken it.n
  while it.n.hasMore:
    semPragmaLine c, dest, it, true
  takeParRi dest, it.n
  if hasUncheckedAccess:
    inc c.inUncheckedAccess
  semStmt(c, dest, it.n, false)
  if hasUncheckedAccess:
    dec c.inUncheckedAccess
  takeParRi dest, it.n
  producesVoid c, dest, info, it.typ

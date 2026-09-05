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
import ".." / lib / [symparser, intrinsics]
import nimony_model, builtintypes, decls, asthelpers, programs,
  magics, nifconfig, semdata, sembasics,
  semchecks, semconst, semos, renderer, features, pragmacanon, identstyle,
  symtabs

# --- thin shims forwarding into the sem core via SemContext callbacks ---
# (const-eval entry points — semBoolExpr, semConstIntExpr, … — come directly
# from the `semconst` module imported above, so they need no callbacks.)

proc semStmt(c: var SemContext; dest: var TokenBuf; n: var Cursor; isNewScope: bool) =
  c.semStmtCB(c, dest, n, isNewScope)

proc declareResult(c: var SemContext; dest: var TokenBuf; info: NifLineInfo): SymId =
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

proc resolveCustomPragma(c: SemContext; n: Cursor): SymId =
  ## The `{.pragma.}` template an annotation refers to.
  ##
  ## A template body is semchecked in the scope the template was *declared*
  ## in, so an annotation written there arrives already bound and must be
  ## taken as it is: looking its name up again where the template expands is
  ## the hygiene bug -- `std/json` wraps `nifcore.into`, so `into`'s body ends
  ## up expanded in a module that never imported the annotation's own module,
  ## and the name resolves to nothing there.
  if n.kind == Symbol and symbolIsCustomPragmaTemplate(n.symId):
    result = n.symId
  else:
    let name = getIdent(n)
    result = if name != StrId(0): c.customPragmaSym(name) else: NoSymId

proc isPreservedCustomPragma(n: Cursor): bool =
  ## True when `n` is a previously-preserved custom-pragma attachment
  ## `(pragma <sym>)` (the `pragma` tag with a symbol child), as opposed to the
  ## bare `(pragma)` marker on a custom-pragma template declaration.
  if n.isTagLit and n.pragmaKind == PragmaP:
    var probe = n
    probe = sub(probe) # bound the peek; `probe` is a copy
    result = probe.hasMore and probe.isSymbol
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
        dest.addParLe(ExprX, n.info)
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
  var hasParRi = n.isTagLit # if false, has no arguments
  var start = default(Cursor)
  if n.substructureKind == KvU:
    start = n; n = sub(n)
  template toPragmaArgs() =
    # step past the pragma name: enter a tag-form pragma's scope, or skip
    # the name ident inside an already entered `(kv ...)` wrapper
    if n.isTagLit:
      start = n; n = sub(n)
    else:
      inc n
  var pk = pragmaKind(n)
  if pk == NoPragma and n.isIdent and IgnoreStyleFeature in c.features:
    # Under `.feature: "ignoreStyle".` accept builtin pragma names spelled in
    # any case / underscore variant (Nim's `cmpIgnoreStyle` rule). The
    # downstream case branches still emit the canonical form into `dest`, so
    # the lowering pipeline never sees the user-written spelling.
    pk = pragmaKindByStyle(n.strId)
  case pk
  of NoPragma:
    if kind.isRoutine and (let cc = callConvKind(n); cc != NoCallConv):
      dest.addParLe(cc, n.info)
      toPragmaArgs()
      dest.addParRi()
    elif n.isTagLit and kind == TypeY and (let hk = hookKind(n.cursorTagId); hk != NoHook):
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
      elif (let psym = c.resolveCustomPragma(n); psym != NoSymId):
        # Pragma that resolves to a `template X {.pragma.}` declaration. Unlike
        # Nim (which drops `sfCustomPragma`), preserve it as
        # `(pragma <sym> <args>)` so it survives into the serialized decl and
        # can be introspected -- by a plugin (`.linear`), or by the validator,
        # which reads `{.ensuresNif: addedExpr(dest).}` off the declaration.
        #
        # The arguments are preserved exactly as written, not semchecked:
        # `{.pragma.}` declares the template's parameters `untyped`, and the
        # arguments are routinely not expressions at all -- `addedExpr(dest)`
        # names a predicate of the validator's own vocabulary, and semchecking
        # it would only report that no such proc exists.
        let info = n.info
        toPragmaArgs()
        dest.addParLe(PragmaP, info)
        dest.addSymUse(psym, info)
        if hasParRi:
          while n.hasMore: takeTree dest, n
        dest.addParRi()
      else:
        buildErr c, dest, n.info, "expected pragma"
        toPragmaArgs()
        if hasParRi:
          while n.hasMore: skip n # skip optional pragma arguments
  of MagicP:
    dest.addParLe(MagicP, n.info)
    toPragmaArgs()
    if hasParRi and n.hasMore and n.kind in {StrLit, Ident}:
      let (magicWord, bits) = magicToTag(pool.strings[n.strId], c.g.config.bits)
      if magicWord == "":
        buildErr c, dest, n.info, "unknown `magic`"
      else:
        crucial.magic = magicWord
        crucial.bits = bits
      takeTree dest, n
    elif n.hasMore and n.exprKind == ErrX:
      dest.addSubtree n
    else:
      buildErr c, dest, n.info, "`magic` pragma takes a string literal"
    dest.addParRi()
  of AssemblerP:
    # `{.assembler.}` — every construct in the body maps one-to-one to assembler.
    # The CHECKING is delegated to the back end (arkham), which is where the
    # machine model lives; NIF carries precise line info, so its diagnostics are
    # as good as a front-end pass would give. Sem only records the flag.
    crucial.flags.incl pk
    if not kind.isRoutine:
      buildErr c, dest, n.info, "`assembler` pragma is only allowed on routines"
      toPragmaArgs()
      if hasParRi:
        while n.hasMore: skip n
    else:
      dest.addParLe(pk, n.info)
      dest.addParRi()
      toPragmaArgs()
      if hasParRi:
        while n.hasMore: skip n
  of NakedP:
    # `{.naked.}` — no prologue, no epilogue. Like `assembler` (which it may only
    # accompany) the machine-level checking belongs to the back end; sem records
    # the flag and forwards the tag.
    crucial.flags.incl pk
    if not kind.isRoutine:
      buildErr c, dest, n.info, "`naked` pragma is only allowed on routines"
      toPragmaArgs()
      if hasParRi:
        while n.hasMore: skip n
    else:
      dest.addParLe(pk, n.info)
      dest.addParRi()
      toPragmaArgs()
      if hasParRi:
        while n.hasMore: skip n
  of InterruptP:
    # `{.interrupt: "SysTick".}` — this routine handles the named vector. WHICH
    # names a part has is a target question, arkham's exactly as for `register`
    # below; what sem owns is the shape, checked in `interruptSignatureError`
    # once the params and the return type are known.
    crucial.flags.incl pk
    let pinfo = n.info
    if not kind.isRoutine:
      buildErr c, dest, pinfo, "`interrupt` pragma is only allowed on routines"
      toPragmaArgs()
      if hasParRi:
        while n.hasMore: skip n
    else:
      dest.addParLe(pk, pinfo)
      toPragmaArgs()
      if hasParRi and n.hasMore:
        semConstStrExprIgnoreTopLevel c, dest, n
      else:
        buildErr c, dest, pinfo, "`interrupt` pragma takes a vector name"
      dest.addParRi()
  of RegisterP:
    # `{.register: "rdi".}` on a parameter, result or local. Which register names
    # exist, and whether the annotation is consistent with the proc's ABI, is a
    # target question — arkham's, not sem's.
    crucial.flags.incl pk
    let pinfo = n.info                 # the pragma NAME's info: `toPragmaArgs` moves
    dest.addParLe(pk, pinfo)           # `n` past it, and a bare `{.register.}` has
    toPragmaArgs()                     # nothing after it to report against
    if hasParRi and n.hasMore:
      semConstStrExprIgnoreTopLevel c, dest, n
    else:
      buildErr c, dest, pinfo, "`register` pragma takes a register name"
    dest.addParRi()
  of StackP:
    crucial.flags.incl pk
    dest.addParLe(pk, n.info)
    dest.addParRi()
    toPragmaArgs()
  of InstructionP, IntrinsicP:
    # `{.instruction: "bsf".}` / `{.intrinsic: "Ctz".}` — the argument is an opcode
    # NAME from `lib/intrinsics`, spelled as a string literal like every other
    # pragma that names a thing (`importc`, `register`). A string rather than an
    # ident because the name is DATA: it is resolved by a table lookup here, never
    # by scope or overload resolution, so an ident would only look like a symbol
    # without being one — and would additionally have to dodge Nim's keywords,
    # which is why the machine's `of`/`and`/`shl` needed cover names.
    #
    # Resolving it here, at the declaration, is what turns a typo into an error
    # with a source location instead of an "unsupported intrinsic" assert three
    # passes later. The signature check against the row happens in `semProcImpl`,
    # where the params and the return type are already in `dest`.
    crucial.flags.incl pk
    let cls = if pk == InstructionP: icPinned else: icPortable
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    if not kind.isRoutine:
      buildErr c, dest, n.info, $pk & " pragma is only allowed on routines"
      if hasParRi and n.hasMore: skip n
    elif hasParRi and n.hasMore and n.kind == StrLit:
      let opName = pool.strings[n.strId]
      let op = intrinsicOpByName(opName, cls)
      if op == NoIntrinsicOp:
        buildErr c, dest, n.info, "unknown " & $pk & ": " & opName
      else:
        crucial.intrinsic = op
      takeTree dest, n
    elif n.hasMore and n.exprKind == ErrX:
      dest.addSubtree n
    else:
      buildErr c, dest, n.info,
        "`" & $pk & "` pragma takes an opcode name as a string literal"
    dest.addParRi()
  of ErrorP, ReportP, DeprecatedP:
    crucial.flags.incl pk
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    if hasParRi and n.hasMore:
      semConstStrExprIgnoreTopLevel c, dest, n
    dest.addParRi()
  of ImportcP, ImportcppP, ExportcP, HeaderP, DynlibP:
    crucial.flags.incl pk
    let info = n.info
    dest.addParLe(pk, info)
    toPragmaArgs()
    let strPos = dest.len
    if hasParRi and n.hasMore:
      semConstStrExprIgnoreTopLevel c, dest, n
    elif crucial.sym != SymId(0):
      var name = pool.syms[crucial.sym]
      extractBasename name
      dest.addStrLit(name, info)
    else:
      c.buildErr dest, info, "invalid import/export symbol"
      dest.addParRi()
      return
    if pk in {ImportcP, ImportcppP, ExportcP} and dest[strPos].kind == StrLit:
      crucial.externName = pool.strings[readonlyCursorAt(dest, strPos).strId]
    # Header pragma extra
    if pk == HeaderP:
      # not `dest.len - 1`: the string may carry a line-info suffix token
      let idx = lastValueStart(dest)
      if dest[idx].isStringLit:
        # Read through a Cursor, never `dest[idx].strId`: a string of at most
        # `StrInlineMaxLen` bytes (`"x.h"`!) is stored INSIDE the token, and the
        # token-level `strId` would decode those packed bytes as a pool id.
        let raw = pool.strings[readonlyCursorAt(dest, idx).strId]
        let name = resolveHeaderPath(raw, info.getFile(), c.g.config)
        if name != raw:
          dest[idx] = strLitToken(pool.strings.getOrIncl(name))
      crucial.headerFileTok = dest[idx]
    # Finalize expression
    dest.addParRi()
  of PluginP:
    # `.plugin: "path"` — single-string form. (The historical
    # `("path", "<version>")` tuple form, which selected between the Nim 2
    # and Nimony compilers, was removed when the Nim 2 plugin path went away.)
    crucial.flags.incl pk
    let pragInfo = n.info
    toPragmaArgs()
    var path = StrId(0)
    var pathInfo = n.info
    var errMsg = ""
    var alreadyErr = false
    if hasParRi:
      if n.hasMore and n.isStringLit:
        path = n.strId
        pathInfo = n.info
        inc n
      elif n.hasMore and n.exprKind == ErrX:
        # Re-sem path: a previous sem pass already produced an err. Pass
        # through without re-reporting.
        alreadyErr = true
        var passBuf = createTokenBuf(8)
        passBuf.takeTree n
        dest.addParLe(pk, pragInfo)
        dest.add passBuf
        dest.addParRi()
      else:
        errMsg = "plugin path must be a string literal"
        if n.hasMore: skip n
    if not alreadyErr:
      dest.addParLe(pk, pragInfo)
      if errMsg.len > 0:
        buildErr c, dest, pathInfo, errMsg
      elif path != StrId(0):
        dest.addStrLit(path, pathInfo)
      dest.addParRi()
  of AlignP, BitsP, SizeP:
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    let valueStart = dest.len
    if hasParRi and n.hasMore:
      semConstIntExpr(c, dest, n, SemcheckBodies)
    else:
      buildErr c, dest, n.info, "expected int literal"
    if pk == SizeP and dest[valueStart].kind == IntLit:
      crucial.size = int(readonlyCursorAt(dest, valueStart).intVal)
    dest.addParRi()
  of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoreturnP, BorrowP,
     NoSideEffectP, NodestroyP, BycopyP, ByrefP, InlineP, NoinlineP,
     AlwaysInlineP, NoinitP,
     InjectP, GensymP, DirtyP, UntypedP, SideEffectP, BaseP, ClosureP, PassiveP, IncompleteStructP:
    crucial.flags.incl pk
    dest.addParLe(pk, n.info)
    dest.addParRi()
    toPragmaArgs()
  of EstablishesBorrowP:
    # Declares that the result borrows from the first parameter. The borrow
    # checker needs this at the *call site*: a view constructor builds its
    # result from a raw pointer, so nothing in the callee's body tells the
    # caller that the returned value still aliases the argument.
    if not kind.isRoutine:
      buildErr c, dest, n.info, $pk & " pragma is only allowed on routines"
    else:
      crucial.flags.incl pk
      dest.addParLe(pk, n.info)
      dest.addParRi()
    toPragmaArgs()
  of ViewP, InheritableP, PureP, FinalP, PackedP, UnionP, AcyclicP:
    var hasErr = false
    if kind != TypeY:
      buildErr c, dest, n.info, $pk & " pragma is only allowed on types"
      hasErr = true
    elif pk in {ViewP, InheritableP, FinalP, PackedP, UnionP, AcyclicP}:
      # peek across the pragmas' close at the type decl's body slot:
      var n2 = n
      while n2.hasMore: skip n2
      n2 = peekPastEnd(n2)
      if n2.typeKind in {RefT, PtrT}:
        inc n2
      # Later passes replace the inline body of `ref object` / `ptr object`
      # with a symbol that stands for the synthesized inner object type; accept
      # that form as valid — the first pass has already validated the shape.
      if n2.kind != Symbol and n2.typeKind != ObjectT:
        buildErr c, dest, n.info, $pk & " pragma is only allowed on object types", n
        hasErr = true
    if not hasErr:
      dest.addParLe(pk, n.info)
      dest.addParRi()
    toPragmaArgs()
  of CursorP:
    if kind in {VarY, LetY, CursorY, FldY, GfldY}:
      # On object fields, `.cursor` marks the field as a non-owning alias:
      # the lifter's `unravelObjField` already special-cases such fields
      # (no recursive destroy/dup), and `trObjConstr`/`trNewobjFields` in
      # the duplifier emit `WantNonOwner` reads for them so no `=dup` is
      # spliced around the value at construction time.
      dest.addParLe(pk, n.info)
      toPragmaArgs()
    else:
      buildErr c, dest, n.info, "pragma only allowed on local variables or object fields"
      toPragmaArgs()
    dest.addParRi()
  of VarargsP:
    crucial.hasVarargs = n.info
    dest.addParLe(pk, n.info)
    dest.addParRi()
    toPragmaArgs()
  of RequiresP, EnsuresP:
    crucial.flags.incl pk
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    if hasParRi and n.hasMore:
      semProposition c, dest, n, pk
    else:
      buildErr c, dest, n.info, "`requires`/`ensures` pragma takes a bool expression"
    dest.addParRi()
  of TagsP:
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    if hasParRi and n.hasMore:
      takeTree dest, n
    else:
      buildErr c, dest, n.info, "expected tags/raises list"
    dest.addParRi()
  of CastP:
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    if hasParRi and n.hasMore:
      takeTree dest, n
    else:
      buildErr c, dest, n.info, "expected `cast` pragma expression"
    dest.addParRi()
  of ProfilerP, StacktraceP, GcsafeP, UsedP:
    # accepted for Nim source compatibility; semantically ignored by Nimony
    toPragmaArgs()
    if hasParRi:
      while n.hasMore: skip n
  of UncheckedAssignP:
    buildErr c, dest, n.info, "`uncheckedAssign` is only valid inside `{.cast(uncheckedAssign).}:` pragma blocks"
    toPragmaArgs()
    if hasParRi:
      while n.hasMore: skip n
  of UncheckedAccessP:
    buildErr c, dest, n.info, "`uncheckedAccess` is only valid inside `{.cast(uncheckedAccess).}:` pragma blocks"
    toPragmaArgs()
    if hasParRi:
      while n.hasMore: skip n
  of RaisesP:
    crucial.flags.incl pk
    let oldLen = dest.len
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    if hasParRi and n.hasMore:
      var nn = n
      let typeStart = dest.len
      # Sem-check the type properly
      crucial.raisesType = semLocalType(c, dest, n)
      # TODO: validate that type supports "x != default(T)" interpretation
      dest.addParRi()
      var emptyRaises = false
      if nn.exprKind == BracketX:
        nn = sub(nn) # bound the peek; `nn` is a copy
        emptyRaises = not nn.hasMore
      if emptyRaises:
        # `raises: []` means "does not raise":
        crucial.flags.excl pk
        dest.shrink oldLen
        crucial.raisesType = default(TypeCursor)
    else:
      # No type specified - default to system.ErrorCode
      let typeStart = dest.len
      dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), n.endInfo
      crucial.raisesType = c.typeToCursor(dest, typeStart)
      dest.addParRi()
  of CallConvP:
    toPragmaArgs()
    if hasParRi and n.hasMore and n.isIdent:
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
    if pk == PragmaP and kind == TemplateY and crucial.sym != SymId(0) and
        not isPreservedCustomPragma(n):
      # `template X(args) {.pragma.}` declares `X` as a custom pragma. The
      # `isPreservedCustomPragma` guard keeps a custom pragma *attached to* a
      # template out of this branch: re-sem sees the attachment as `(pragma
      # <sym>)`, which is shaped like a declaration marker with an argument,
      # and this branch would reject it as "`pragma` takes no arguments". Only
      # the bare `(pragma)` marker declares one.
      # body is not expanded at attachment sites — the annotation is
      # recorded as a known custom-pragma name that will be silently
      # accepted (and dropped) wherever it is later attached. Mirrors Nim's
      # `sfCustomPragma`.
      let info = n.info
      toPragmaArgs()
      if hasParRi and n.hasMore:
        buildErr c, dest, info, "`pragma` takes no arguments"
        while n.hasMore: skip n
      else:
        var basename = pool.syms[crucial.sym]
        extractBasename basename
        c.customPragmaTemplates.incl pool.strings.getOrIncl(basename)
        dest.addParLe(PragmaP, info)
        dest.addParRi()
    elif pk == PragmaP and isPreservedCustomPragma(n):
      # An already-preserved custom-pragma attachment `(pragma <sym>)`, seen
      # again when a decl's pragmas are re-sem'd across phases / instantiation.
      # Re-emit it so it stays introspectable and idempotent. Consume only the
      # opening tag and the children here, leaving the pragma's own `)` for the
      # shared `if hasParRi:` epilogue below; taking the whole tree would let
      # that epilogue skip the *next* `)` (the enclosing `pragmas` closer) and
      # swallow the routine body.
      dest.addParLe(PragmaP, n.info)
      toPragmaArgs()
      while n.hasMore:
        takeTree dest, n
      dest.addParRi()
    else:
      buildErr c, dest, n.info, "pragma not supported"
      toPragmaArgs()
      if hasParRi:
        while n.hasMore: skip n # skip optional pragma arguments
      dest.addParRi()
  of KeepOverflowFlagP:
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    dest.addParRi()
  of SemanticsP:
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    if hasParRi and n.hasMore and n.kind in {StrLit, Ident}:
      takeTree dest, n
    else:
      buildErr c, dest, n.info, "`semantics` pragma takes a string literal"
    dest.addParRi()
  of FeatureP:
    buildErr c, dest, n.info, "`feature` pragma is only allowed as top level pragma"
  of MethodsP:
    dest.addParLe(pk, n.info)
    toPragmaArgs()
    while n.hasMore:
      dest.takeTree n
    dest.addParRi()
  if hasParRi:
    if n.hasMore:
      if n.exprKind != ErrX:
        buildErr c, dest, n.info, "too many arguments for pragma"
      while n.hasMore: skip n
    n = start; skip n

proc semPragmas*(c: var SemContext; dest: var TokenBuf; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  var pragmaOpen = false
  let info = n.info
  if n.isDotToken or n.substructureKind == PragmasU:
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
        dest.addParLe(PragmasU, info)
        dest.addParLe(ClosureP, info)
        dest.addParRi()
        pragmaOpen = true

    var checkedPragmas = default(CheckedPragmas)
    if n.isDotToken:
      inc n
    elif n.substructureKind == PragmasU:
      if not pragmaOpen:
        dest.addParLe(PragmasU, info)
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

# ── intrinsic declarations ──────────────────────────────────────────────────
# The declaration is the typing contract: what a proc signature already
# expresses stays in the signature, and the one thing it cannot express — the
# operand model — is unified against the row HERE, once. After this check the
# symbol is an ordinary typed proc, so no later pass (sigmatch, getType, hexer,
# arkham) needs a rule per opcode.

proc intBitsOf(c: var SemContext; typ: Cursor): int =
  ## Bit width of `(i N)` / `(u N)` / `(c N)`, or -1 for anything else.
  ## `int`/`uint` already carry the config's width here, so no resolution step.
  result = -1
  if typ.kind == TagLit and typ.typeKind in {IntT, UIntT, CharT}:
    var bits = typ
    inc bits
    if bits.kind == IntLit: result = typebits(bits.load)

proc floatBitsOf(typ: Cursor): int =
  ## Bit width of `(f N)`, or -1 for anything else.
  result = -1
  if typ.kind == TagLit and typ.typeKind == FloatT:
    var bits = typ
    inc bits
    if bits.kind == IntLit: result = typebits(bits.load)

proc matchAtomicCell(c: var SemContext; typ: Cursor;
                     w: var int; widths: set[uint8]): bool =
  ## The type an atomic operates ON: `ptValW`, and the pointee of `ptPtrW`.
  ##
  ## Three shapes, and the third is the interesting one:
  ##
  ## * an INTEGER — binds `W` to its width, exactly like `ptAnyIntW`;
  ## * a POINTER (or `bool`, an 8-bit cell) — a machine word, so it binds 64.
  ##   These are real atomic cells that no integer pattern would admit: a
  ##   lock-free list head, an `AtomicFlag`;
  ## * a TYPE VARIABLE — binds nothing, and that is the point. The atomics are
  ##   generic over the cell type (one `atomicLoadN[T]`, not one row per width),
  ##   so the width is a property of the INSTANTIATION and is not knowable here.
  ##   For a generic declaration this check is therefore a SHAPE check — arity,
  ##   pointer-ness, which operand is `var`, where a memory order goes — and the
  ##   width is read off the pointee at the call site by whichever back end lowers
  ##   it (arkham's `atomicBits`, and the C compiler for `cBuiltinFor`). A
  ##   CONCRETE declaration still gets the full check, because `W` does bind then.
  result = false
  if typ.kind == Symbol:
    result = true                  # a type variable: nothing to bind, nothing to check
  elif typ.kind == TagLit:
    var bits = 0
    case typ.typeKind
    of PtrT, PointerT, ProctypeT: bits = 64
    of BoolT: bits = 8
    of IntT, UIntT, CharT: bits = intBitsOf(c, typ)
    else: bits = 0
    if bits > 0 and uint8(bits) in widths:
      if w == 0: w = bits          # bind W
      result = w == bits           # ... or match what it is already bound to

proc matchPat(c: var SemContext; pat: PatKind; typ: Cursor;
              w: var int; widths: set[uint8]): bool =
  ## Unify one type pattern against one declared type. `w` carries the row's
  ## single width variable `W` across the whole signature: unbound (0) on the
  ## first `…W` pattern, then required to match.
  case pat
  of ptNone:
    result = false
  of ptVoid:
    result = typ.kind == DotToken or (typ.kind == TagLit and typ.typeKind == VoidT)
  of ptBool:
    result = typ.kind == TagLit and typ.typeKind == BoolT
  of ptInt32:
    result = typ.kind == TagLit and typ.typeKind == IntT and intBitsOf(c, typ) == 32
  of ptAnyInt:
    result = intBitsOf(c, typ) > 0
  of ptIntW, ptUIntW, ptAnyIntW:
    if typ.kind != TagLit:
      result = false
    else:
      let k = typ.typeKind
      let kindOk =
        if pat == ptIntW: k == IntT
        elif pat == ptUIntW: k == UIntT
        else: k in {IntT, UIntT, CharT}
      let bits = intBitsOf(c, typ)
      if not kindOk or bits <= 0 or uint8(bits) notin widths:
        result = false
      else:
        if w == 0: w = bits          # bind W
        result = w == bits           # ... or match what it is already bound to
  of ptValW:
    result = matchAtomicCell(c, typ, w, widths)
  of ptPtrW:
    if typ.kind == TagLit and typ.typeKind == PtrT:
      var pointee = typ
      inc pointee
      result = matchAtomicCell(c, pointee, w, widths)
    else:
      result = false
  of ptRawPtr:
    result = typ.kind == TagLit and typ.typeKind == PointerT
  of ptWeak:
    result = typ.kind == TagLit and typ.typeKind == BoolT
  of ptMemOrder:
    # v1 accepts ANY type here because v1 reads none: both back ends emit every
    # atomic sequence at sequential-consistency strength, so the argument is
    # evaluated for its side effects and discarded. The two declaration sites
    # already spell it differently — a plain `cint` in `std/atomics`, a distinct
    # `AtomMemModel` in `system/atomintrin` — and neither is wrong while nothing
    # consults it. A v2 that honours the order tightens this to "the memory-order
    # enumeration" and gains a real check; today such a check would only forbid
    # spellings that behave identically.
    result = true
  of ptVec128:
    # The opaque 128-bit SIMD value, spelled `(f 128)`: a bag of bits whose lane
    # meaning lives in the opcode. Never binds `W` — the lane width is the
    # trailing `ptLaneBits` literal, not a property of the value's type.
    result = floatBitsOf(typ) == 128
  of ptFloatW:
    let bits = floatBitsOf(typ)
    if bits <= 0 or uint8(bits) notin widths:
      result = false
    else:
      if w == 0: w = bits
      result = w == bits
  of ptAnyPtr:
    # `aptr` is a Leng-level spelling; the frontend's pointer kinds are these two.
    result = typ.kind == TagLit and typ.typeKind in {PtrT, PointerT}
  of ptLaneBits:
    # An int the back end reads as a LITERAL (32/64) at the call site; the
    # declared parameter type is any integer.
    result = intBitsOf(c, typ) > 0
  of ptImmLit:
    # Same shape as `ptLaneBits` and checked the same way: the DECLARATION only
    # has to say "an integer". That the argument must be a literal is a fact
    # about the instruction's encoding, so it is checked where the encoding is —
    # at the call site, by the back end.
    result = intBitsOf(c, typ) > 0

proc intrinsicSignatureError*(c: var SemContext; dest: var TokenBuf;
                              paramsAt: int; op: IntrinsicOp): string =
  ## The mismatch message, or "" when the declaration matches the row. Reads
  ## `dest` through cursors ONLY — the caller emits any error afterwards, once
  ## every cursor is gone (appending to `dest` may reallocate it), and into the
  ## routine's `effects` slot, the one slot that already accepts an `(err …)`.
  let row = IntrinsicRows[op]
  let opName = IntrinsicNames[op]
  result = ""
  var w = 0
  var i = 0
  var n = cursorAt(dest, paramsAt)
  if n.substructureKind == ParamsU:
    var p = sub(n)
    while p.hasMore:
      if i >= row.arity:
        result = "`" & opName & "` takes " & $row.arity & " operand(s)"
        break
      let param = asLocal(p)
      # §4.1: the roles dictate the spelling, with no author choice. An `inout`
      # operand is read AND written, which only `var` expresses — and the `var` is
      # what makes the call site emit `(haddr d)`, the tag that tells the back end
      # to bind d's location instead of materialising a pointer to it.
      var ptyp = param.typ
      let wantsVar = row.roles[i] == roInout
      let isVar = ptyp.kind == TagLit and ptyp.typeKind == MutT
      if wantsVar and not isVar:
        result = "operand " & $(i + 1) & " of `" & opName & "` is read and " &
                 "written, so it must be declared `var`"
        break
      if isVar and not wantsVar:
        result = "operand " & $(i + 1) & " of `" & opName & "` is only read, " &
                 "so it must not be declared `var`"
        break
      if isVar: inc ptyp             # `(mut T)` → match the row's pattern against T
      if not matchPat(c, row.params[i], ptyp, w, row.widths):
        result = "operand " & $(i + 1) & " of `" & opName & "` has a type the " &
                 "instruction cannot take"
        break
      inc i
      skip p
  if result.len == 0:
    if i != row.arity:
      result = "`" & opName & "` takes " & $row.arity & " operand(s), " &
               "but " & $i & " were declared"
    else:
      var ret = n
      skip ret                       # past the (params …) → the return type
      if not matchPat(c, row.ret, ret, w, row.widths):
        result = "the result type of `" & opName & "` does not match the " &
                 "instruction's destination"
  endRead n

proc interruptSignatureError*(dest: var TokenBuf; paramsAt: int): string =
  ## The mismatch message for `{.interrupt: "…".}`, or "" when the declaration
  ## is the shape a handler must have. Read through cursors only and emitted by
  ## the caller into the `effects` slot, for the same reason
  ## `intrinsicSignatureError` is.
  ##
  ## The rule is not a target's: hardware enters a handler with no arguments and
  ## with nowhere to put a result, on every part that has a interrupt table. A
  ## parameter would be read from whatever the interrupted code left in r0, and a
  ## result would be written into a register the hardware restores on the way out
  ## — neither is a diagnosable failure at run time, so both are refused here.
  result = ""
  var n = cursorAt(dest, paramsAt)
  if n.substructureKind == ParamsU:
    var p = sub(n)
    if p.hasMore:
      result = "an `interrupt` handler is entered by hardware, which passes no " &
               "arguments, so it must take no parameters"
    endRead n
    if result.len > 0: return
    n = cursorAt(dest, paramsAt)
  var ret = n
  skip ret                           # past the (params …) → the return type
  if not ret.isDotToken:
    result = "an `interrupt` handler returns to the interrupted code, not to a " &
             "caller, so it must not return a value"
  endRead n

proc semAssumeAssert*(c: var SemContext; dest: var TokenBuf; it: var Item; kind: StmtKind) =
  let info = it.n.info
  dest.addParLe(kind, info)
  it.n.into:
    semBoolExpr c, dest, it.n
  dest.addParRi()

proc semCastInnerPragma*(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  ## Process a single pragma item inside a `(cast (pragmas ...))` list.
  ## Only `noSideEffect` and `uncheckedAssign` are accepted; the result is
  ## emitted in canonical tag form so later passes can dispatch on the kind.
  let info = n.info
  let pk = n.pragmaKind
  case pk
  of NoSideEffectP, UncheckedAssignP, UncheckedAccessP:
    dest.addParLe(pk, info)
    dest.addParRi()
    if n.isTagLit: skip n
    else: inc n
  else:
    buildErr c, dest, info, "invalid `cast` pragma argument; expected `noSideEffect`, `uncheckedAssign` or `uncheckedAccess`"
    if n.isTagLit: skip n
    else: inc n

proc readPragmaStrings(c: var SemContext; dest: var TokenBuf; it: var Item): seq[string] =
  ## Collect the string-literal arguments of a statement pragma like `build`/
  ## `compile`; `it.n` is already positioned at the first argument and is left
  ## at the end of the pragma's scope (the caller closes it).
  result = newSeq[string]()
  while it.n.hasMore:
    if it.n.kind != StrLit:
      buildErr c, dest, it.n.info, "expected `string` but got: " & asNimCode(it.n)
      skip it.n
    else:
      result.add pool.strings[it.n.strId]
      inc it.n

proc addBuildTarget(c: var SemContext; dest: var TokenBuf; info: NifLineInfo;
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

proc addBackendTool(c: var SemContext; dest: var TokenBuf; info: NifLineInfo;
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

proc addBundle(c: var SemContext; dest: var TokenBuf; info: NifLineInfo;
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
  # A statement pragma arrives either wrapped — `(call build "a")` /
  # `(kv assume expr)` with the pragma name as first child — or as a bare
  # ident / tag-form node. For wrapped forms the wrapper's scope is entered
  # here; the branches step past the name via `toPragmaArgs` and close the
  # scope via `closePragmaLine`.
  var start = default(Cursor)
  var hasScope = false
  if not isPragmaBlock and it.n.isTagLit and
      (it.n.stmtKind in CallKindsS or it.n.substructureKind == KvU):
    start = it.n; it.n = sub(it.n)
    hasScope = true
  template toPragmaArgs() =
    if it.n.isTagLit:
      start = it.n; it.n = sub(it.n)
      hasScope = true
    else:
      inc it.n
  template closePragmaLine() =
    if hasScope:
      it.n = start; skip it.n
    else: skipParRi it.n # degenerate bare-ident form; historical behavior
  case it.n.pragmaKind
  of BuildP:
    # Repurposed: `{.build(builder, tool[, args]).}` routes this module's Leng IR
    # through a custom backend `tool` (built by `builder`). NOT the old foreign-
    # source `.build` — that single use (mimalloc) moved to the standard
    # `.compile`.
    let info = it.n.info
    toPragmaArgs()
    let args = readPragmaStrings(c, dest, it)
    closePragmaLine()
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
    toPragmaArgs()
    let args = readPragmaStrings(c, dest, it)
    closePragmaLine()
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
    toPragmaArgs()
    let args = readPragmaStrings(c, dest, it)
    closePragmaLine()
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
    toPragmaArgs()
    semEmit c, dest, it
    closePragmaLine()
  of AssumeP, AssertP:
    let kind = if it.n.pragmaKind == AssumeP: AssumeS else: AssertS
    let info = it.n.info
    toPragmaArgs()
    dest.addParLe(kind, info)
    semBoolExpr c, dest, it.n
    dest.addParRi()
    closePragmaLine()
  of ErrorP:
    # Statement-level `{.error: "msg".}` — distinct from `{.error.}` on a routine
    # decl (handled in `semPragma`). Classic Nim calls `localError` here; dead
    # branches (`when false: ... else: {.error.}`) never reach this.
    let info = it.n.info
    toPragmaArgs()
    let start = dest.len
    let s = evalConstStrExpr(c, dest, it.n, c.types.stringType)
    closePragmaLine()
    if s != StrId(0):
      dest.shrink start
      buildErr c, dest, info, pool.strings[s]
  of KeepOverflowFlagP:
    if not isPragmaBlock:
      buildErr c, dest, it.n.info, "`keepOverflowFlag` pragma must be used in a pragma block"
    else:
      dest.addParLe(KeepOverflowFlagP, it.n.info)
      dest.addParRi()
    skip it.n
  of CastP:
    if not isPragmaBlock:
      buildErr c, dest, it.n.info, "`cast` pragma must be used in a pragma block"
      skip it.n
    else:
      let info = it.n.info
      dest.addParLe(CastP, info)
      dest.addParLe(PragmasS, info)
      it.n.into: # (cast
        if it.n.hasMore and it.n.substructureKind == PragmasU:
          it.n.into: # inner (pragmas of the canonical form
            while it.n.hasMore:
              semCastInnerPragma c, dest, it.n
        else:
          if it.n.hasMore and it.n.isDotToken:
            # need because parser produces `.` with unknown-type cast expr but it
            # is not part of the cast pragma
            inc it.n
          while it.n.hasMore:
            semCastInnerPragma c, dest, it.n
      dest.addParRi() # close (pragmas)
      dest.addParRi() # close (cast)
  of PluginP:
    # `.plugin: "path"` — single-string form. (The historical
    # `("path", "<version>")` tuple form was removed when the Nim 2 plugin
    # compile path went away.)
    let pragInfo = it.n.info
    toPragmaArgs()
    var path = StrId(0)
    var pathInfo = it.n.endInfo
    var errMsg = ""
    if it.n.hasMore and it.n.isStringLit:
      path = it.n.strId
      pathInfo = it.n.info
      inc it.n
    else:
      errMsg = "plugin path must be a string literal"
      if it.n.hasMore: skip it.n
    if path != StrId(0) and errMsg.len == 0:
      if c.routine.inGeneric == 0 and path notin c.pluginBlacklist:
        c.pendingModulePlugins.add PluginObj(path: path, info: pathInfo)
    closePragmaLine()                   # close the original (plugin ...)
    dest.addParLe(PragmasS, pragInfo)
    dest.addParLe(PluginP, pragInfo)
    if errMsg.len > 0:
      buildErr c, dest, pathInfo, errMsg
    elif path != StrId(0):
      dest.addStrLit(path, pathInfo)
    dest.addParRi()                     # close (plugin
    dest.addParRi()                     # close (pragmas
  of PragmaP:
    dest.addParLe(PragmasS, it.n.info)
    dest.addParLe(PragmaP, it.n.info)
    toPragmaArgs()
    let name = takeIdent(it.n)
    if name == StrId(0):
      buildErr c, dest, it.n.endInfo, "expected identifier for pragma"
      dest.addParRi()
      closePragmaLine()
      while it.n.hasMore:
        takeTree dest, it.n
    else:
      var buf = createTokenBuf(16)
      dest.addIdent(name, it.n.endInfo)
      dest.addParRi()
      closePragmaLine()
      # take remaining pragmas:
      while it.n.hasMore:
        buf.addSubtree it.n
        takeTree dest, it.n
      # no closing sentinel: consumers iterate with `hasMore`
      c.userPragmas[name] = buf
    dest.addParRi()
  of PushP:
    var n = it.n
    if n.isTagLit:
      n = sub(n) # bound the peek; `n` is a copy
    else:
      inc n
    if not n.hasMore:
      discard "empty push"
    else:
      var buf = createTokenBuf(16)
      while n.hasMore:
        buf.addSubtree n
        skip n
      # no closing sentinel: consumers iterate with `hasMore`
      c.pragmaStack.add buf
    # semcheck push/pop pragmas in both SemcheckSignatures and SemcheckBodies phases
    # so that pushed pragmas works for both procs and variables
    if c.phase == SemcheckBodies:
      while it.n.hasMore: skip it.n
    else:
      dest.addParLe PragmasS, it.n.info
      dest.takeTree it.n
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
      dest.takeTree it.n
      dest.addParRi
  of PassLP:
    toPragmaArgs()
    let start = dest.len
    let s = evalConstStrExpr(c, dest, it.n, c.types.stringType)
    if s != StrId(0):
      dest.shrink start
      c.passL.add pool.strings[s]
    closePragmaLine()
  of PassCP:
    toPragmaArgs()
    let start = dest.len
    let s = evalConstStrExpr(c, dest, it.n, c.types.stringType)
    if s != StrId(0):
      dest.shrink start
      c.passC.add pool.strings[s]
    closePragmaLine()
  of FeatureP:
    toPragmaArgs()
    let info = it.n.info
    let start = dest.len
    let s = evalConstStrExpr(c, dest, it.n, c.types.stringType)
    if s != StrId(0):
      dest.shrink start
      let features = parseFeatures(pool.strings[s])
      if features == {}:
        while it.n.hasMore: skip it.n
        buildErr c, dest, info, "unknown `feature`"
      else:
        c.features.incl features
        closePragmaLine()
    else:
      while it.n.hasMore: skip it.n
      buildErr c, dest, info, "`feature` pragma takes a string literal"
  else:
    if (let psym = c.resolveCustomPragma(it.n); psym != NoSymId):
      # A custom pragma as a *statement*. It marks the region it stands in
      # rather than a declaration, which is what a wrapper template needs: the
      # marker is written in the template's body, so every expansion carries it
      # without the reader having to work out which template it came from.
      #
      # Preserved as a `(pragmas (pragma <sym> <args>))` statement. Nothing
      # downstream has to learn anything: hexer's passes already take such a
      # statement through untouched and lengcgen already skips it.
      let info = it.n.info
      toPragmaArgs()
      dest.addParLe(PragmasS, info)
      dest.addParLe(PragmaP, info)
      dest.addSymUse(psym, info)
      while it.n.hasMore: takeTree dest, it.n
      dest.addParRi()
      dest.addParRi()
      closePragmaLine()
      producesVoid c, dest, info, it.typ
    else:
      buildErr c, dest, it.n.info, "unsupported pragma", it.n
      skip it.n
      while it.n.hasMore: skip it.n

proc semPragmasLine*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  it.n.into:
    while it.n.hasMore:
      # kv/call wrappers around a pragma line are entered by `semPragmaLine`
      semPragmaLine c, dest, it, false
  producesVoid c, dest, info, it.typ # in case it was not already produced

proc hasCastUncheckedAccess(n: Cursor): bool =
  ## Scan the first pragma of the `(pragmas ...)` list at `n` to see if it
  ## contains `{.cast(uncheckedAccess).}`.
  result = false
  var scan = n
  scan = sub(scan) # into (pragmas; bound the walk, `scan` is a copy
  if scan.hasMore and scan.isTagLit:
    if scan.pragmaKind == UncheckedAccessP:
      result = true
    else:
      scan.linearScan:
        if scan.pragmaKind == UncheckedAccessP:
          result = true
          break

proc semPragmaExpr*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  var hasUncheckedAccess = false
  copyInto dest, it.n: # (pragmax
    assert it.n.stmtKind == PragmasS
    hasUncheckedAccess = hasCastUncheckedAccess(it.n)
    copyInto dest, it.n: # (pragmas
      while it.n.hasMore:
        semPragmaLine c, dest, it, true
    if hasUncheckedAccess:
      inc c.inUncheckedAccess
    semStmt(c, dest, it.n, false)
    if hasUncheckedAccess:
      dec c.inUncheckedAccess
  producesVoid c, dest, info, it.typ

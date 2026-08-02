#       Nimony Compiler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## This module implements the template expansion mechanism.
##
## Formerly textually `include`d into sem.nim; now a separate module. Template
## expansion is purely a token-substitution pass — it does not re-enter the sem
## dispatcher — so this module needs no callbacks into the core.

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
import std / [tables, sets, assertions]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser
import ".." / models / tags
import nimony_model, symtabs, decls, programs,
  semdata, sembasics, semos, semuntyped

type
  ExpansionContext = object
    newVars: Table[SymId, SymId]
    formalParams, typevars: Table[SymId, Cursor]
    firstVarargMatch: Cursor
    inferred: ptr Table[SymId, Cursor]

proc expandTemplateImpl(c: var SemContext; dest: var TokenBuf;
                        e: var ExpansionContext; body: Cursor) =
  ## Expands a single tree/token of the template body into `dest`.
  var body = body
  case body.kind
  of UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, DotToken, Ident:
    dest.addSubtree body
  of Symbol:
    let s = body.symId
    let arg = e.formalParams.getOrDefault(s)
    if arg != default(Cursor):
      dest.addSubtree arg
    else:
      let nv = e.newVars.getOrDefault(s)
      if nv != SymId(0):
        dest.addSymUse(nv, body.info)
      else:
        let tv = e.inferred[].getOrDefault(s)
        if tv != default(Cursor):
          dest.addSubtree tv
        else:
          dest.addSubtree body # keep Symbol as it was
  of SymbolDef:
    let s = body.symId
    let newDef = newSymId(c, s)
    e.newVars[s] = newDef
    dest.addSymDef(newDef, body.info)
  of StrLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.addSubtree body
  of TagLit:
    let forStmt = asForStmt(body)
    if forStmt.kind == ForS and forStmt.iter.exprKind == UnpackX:
      # the loop body is expanded once per matched vararg; the `(for …)`
      # tree itself produces no output
      assert forStmt.vars.substructureKind == UnpackflatU
      var arg = e.firstVarargMatch
      var fv = forStmt.vars
      inc fv
      inc fv
      let vid = fv.symId
      if arg.hasMore and not arg.isDotToken:
        while arg.hasMore:
          e.formalParams[vid] = arg
          expandTemplateImpl c, dest, e, forStmt.body
          skip arg
    elif body.exprKind == UnpackX:
      var un = body
      un = sub(un) # bounded: `kind` is ParRi for a bare `unpack()`
      var arg = e.firstVarargMatch
      if not un.hasMore:
        # `unpack()` variant:
        while arg.hasMore:
          dest.takeTree arg
      else:
        # `unpack(fn)` variant:
        while arg.hasMore:
          dest.addParLe CallX, arg.info
          dest.copyTree un # fn
          dest.takeTree arg
          dest.addParRi()
    else:
      dest.addParLe(body.cursorTagId, body.info)
      body.into:
        while body.hasMore:
          expandTemplateImpl c, dest, e, body
          skip body
        dest.addParRi(body.endInfo)
  else:
    discard "ParRi/close (classic) or stray suffix (nifcore)"

type
  PluginOutcome* = enum
    NoPluginRan       ## the template carries no `.plugin` pragma
    PluginExpanded    ## the plugin produced a replacement tree
    PluginDeferred    ## the plugin answered `(deferexpansion)`: it cannot decide
                      ## while the arguments still contain type variables
    PluginFailed      ## the exchange broke down; `errMsg` says how. Reported by
                      ## the caller against the call site — an `(err …)` written
                      ## into the plugin's own output buffer is re-checked as an
                      ## expression and its message lost

const MaxPluginRounds = 32
  ## Backstop only: `(needtypes …)` already has to name a symbol it was not given,
  ## so a well-formed exchange terminates after at most one round per level of
  ## type nesting. This catches a plugin that finds new symbols to ask about
  ## forever (a cyclic decl walked without a `seen` set, say).

proc appendDecl(s: SymId; dest: var TokenBuf): bool =
  ## Appends `s`'s declaration — a type *or* a type variable — and says whether
  ## there was one. Type variables are shipped too, so that a plugin can tell
  ## "this is a `T` still to be substituted" from "this is a type" positively,
  ## rather than guessing from what is absent.
  result = false
  let res = tryLoadSym(s)
  if res.status != LacksNothing: return
  if res.decl.symKind notin {TypeY, TypevarY, StaticTypevarY}: return
  dest.addSubtree res.decl
  result = true

proc buildTypeDefsInput(decls: var TokenBuf; info: NifLineInfo): TokenBuf =
  ## Wrap the declarations gathered so far as `(stmts <decl>*)`. A template
  ## plugin always gets this second input, empty on the first round, so
  ## `loadTypeDefinitions()` is safe to call unconditionally.
  result = createTokenBuf(decls.len + 4)
  result.addParLe StmtsS, info
  if decls.len > 0:
    var d = beginRead(decls)
    while d.hasMore:
      result.addSubtree d
      skip d
    endRead d
  result.addParRi()

proc readRequestedSyms(dest: var TokenBuf; outputStart: int; syms: var seq[SymId]) =
  ## Reads the symbols out of a `(needtypes …)` output.
  var o = readonlyCursorAt(dest, outputStart)
  o.into:
    while o.hasMore:
      if o.isSymbol: syms.add o.symId
      skip o
  endRead o

proc expandPlugin(c: var SemContext; dest: var TokenBuf; temp: Routine, args: Cursor;
                  errMsg: var string): PluginOutcome =
  result = NoPluginRan
  var p = temp.pragmas
  if not p.isTagLit:
    return
  var path = StrId(0)
  var pathInfo = p.endInfo # a degenerate/empty pragma list still needs an info
  p.into:  # (pragmas …)
    while p.hasMore:
      if p.pragmaKind == PluginP:
        p.into PluginP:
          # `.plugin: "path"` — single-string form only.
          if p.isStringLit:
            path = p.strId
            pathInfo = p.info
          while p.hasMore: skip p
      else:
        skip p
  if path == StrId(0):
    return

  # Declarations the plugin has asked for so far. Nothing is shipped up front:
  # a plugin that does not inspect types never pays for the lookup, and one that
  # does gets exactly the symbols it names rather than a transitive closure.
  var decls = createTokenBuf(0)
  var provided = initHashSet[SymId]()
  var rounds = 0

  while true:
    inc rounds
    var b = createTokenBuf(30)
    b.addParLe StmtsS, args.endInfo # zero-arg calls: args sits at a scope's end
    # Pass the invoked template's name as the first child of the input
    # so a single shared plugin can dispatch on which template was
    # called. The plugin reads it with `pluginName` and skips to
    # the real call-site arguments with `callArgs`.
    b.addIdent(symToIdent(temp.name.symId), args.endInfo)
    var a = args
    while a.hasMore:
      b.takeTree a
    b.addParRi()

    var types = buildTypeDefsInput(decls, args.endInfo)
    let outputStart = dest.len
    runPlugin(c, dest, pathInfo, pool.strings[path], b, types)

    var marker = NoSub
    if dest.len > outputStart:
      var o = readonlyCursorAt(dest, outputStart)
      marker = o.substructureKind
      endRead o

    case marker
    of DeferexpansionU:
      # The marker is a signal, not a tree: drop it and let the caller keep the
      # call itself, to be re-driven once instantiation has substituted.
      dest.shrink outputStart
      return PluginDeferred
    of NeedtypesU:
      var wanted: seq[SymId] = @[]
      readRequestedSyms(dest, outputStart, wanted)
      dest.shrink outputStart
      var progress = false
      for s in wanted:
        if not provided.containsOrIncl(s):
          progress = true
          discard appendDecl(s, decls)
      if not progress:
        errMsg = "plugin '" & pool.strings[path] &
          "' asked again for declarations it was already given"
        return PluginFailed
      if rounds >= MaxPluginRounds:
        errMsg = "plugin '" & pool.strings[path] &
          "' kept asking for declarations (" & $rounds & " rounds)"
        return PluginFailed
    else:
      return PluginExpanded

proc isPluginTemplate*(fnId: SymId): bool =
  ## True for a template carrying `.plugin`. A deferred plugin call survives in
  ## a generic body as `(at <thisSym> <args>…)`, which reaches `semInvoke`
  ## looking like a generic type instantiation; this is how it is told apart.
  result = false
  let res = tryLoadSym(fnId)
  if res.status == LacksNothing and res.decl.symKind == TemplateY:
    let templ = asRoutine(res.decl, SkipInclBody)
    result = hasPragma(templ.pragmas, PluginP)

proc runPluginForSym*(c: var SemContext; dest: var TokenBuf; fnId: SymId;
                      args: Cursor; errMsg: var string): PluginOutcome =
  ## Re-drive a plugin template from a symbol plus already-checked arguments —
  ## the instantiation-time counterpart of the call-site `expandPlugin`.
  result = NoPluginRan
  let res = tryLoadSym(fnId)
  if res.status == LacksNothing and res.decl.symKind == TemplateY:
    var templ = asRoutine(res.decl, SkipInclBody)
    result = expandPlugin(c, dest, templ, args, errMsg)

proc addTemplFormalsToScope(c: var SemContext; buf: TokenBuf; at: int) =
  ## Put a promoted template's OWN typevars and params on the parameter scope.
  ##
  ## `semTemplBody` resolves a body `Ident` through `getIdentReplaceParams`,
  ## which asks `buildSymChoice` (`InnerMost`) for a symbol and substitutes it
  ## only when that SymId is a formal of THIS template (`isTemplParam`, i.e.
  ## `ctx.params`). Lazy promotion runs inside the CALLER's live scope, so
  ## without this the innermost `T` is the caller's homonymous typevar — not one
  ## of our formals, so the ident stays an `Ident`, is re-sem'd at the expansion
  ## site against yet another `T`, and yields the bogus `got: T but wanted: T`
  ## (issue #2181). Registering our formals in the inner parameter scope makes
  ## `InnerMost` pick ours and the substitution happen.
  ##
  ## Phase 3 gets all this for free: `semGenericParams` / `semParams` `addSym`
  ## every formal as they check it. Promotion starts from an already-sem'd
  ## published decl and skips both, so it has to re-attach them here.
  var p = readonlyCursorAt(buf, at)
  if p.substructureKind in {ParamsU, TypevarsU}:
    p.into:
      while p.hasMore:
        let param = asLocal(p)
        if param.name.isSymbolDef:
          var nameStr = pool.syms[param.name.symId]
          extractBasename(nameStr)
          if nameStr.len > 0:
            # `param.kind` is the decl's own `ParamY` / `TypevarY`, so the two
            # call sites cannot disagree with the structure they pass.
            addOverloadable(c.currentScope, pool.strings.getOrIncl(nameStr),
                            Sym(kind: param.kind, name: param.name.symId, pos: 0))
        skip p

proc tryPromoteTemplateBody*(c: var SemContext; sym: SymId): bool =
  ## On-demand upgrade of a verbatim-published template body. Phase 2's
  ## `semProcImpl` takes the template body verbatim via `takeTree` and
  ## publishes it with `phase = SemcheckSignatures`. That body is still
  ## in Ident form: `(stmts x)` rather than `(stmts x.0)`, because
  ## phase-2 `checkSignatures` skipped body sem entirely. But
  ## `expandTemplateImpl` substitutes formal parameters by SymId — a
  ## bare `Ident x` never matches the table — so any caller that hits a
  ## phase-2-published template (notably `const` initializers, which
  ## `constGuard` already evaluates in phase 2) sees an unsubstitutable
  ## body and produces `undeclared identifier: x`.
  ##
  ## Closing the gap eagerly in phase 2 is too aggressive (typed
  ## templates' bodies routinely reference symbols declared later in
  ## the file — e.g. lib/std/system.nim's `incl` template). Instead,
  ## upgrade lazily here: when `semTemplateCall` is about to expand a
  ## template whose published phase still says Signatures, run the
  ## existing lazy `semTemplBody` pass — it resolves param idents to
  ## Symbols while leaving everything else as Idents to be re-sem'd at
  ## expansion site. The phase 3 typed-body publish later overwrites
  ## this entry, so non-`const` consumers continue to get the fully
  ## typed body.
  ##
  ## Regression: `tests/nimony/templates/tconst_template.nim`.
  ## See also `fixed_const_template_param_subst.md`.
  if not prog.mem.inSignatureCheck(sym):
    return false

  # Walk the published decl and copy everything except the body into a
  # fresh buffer; then run `semTemplBody` over the body and append the
  # closing paren.
  var oldHead = readonlyCursorAt(prog.mem[sym].buffer, 0)
  if oldHead.symKind != TemplateY: return false

  var newBuf = createTokenBuf(prog.mem[sym].buffer.len + 16)
  newBuf.addParLe(oldHead.cursorTagId, oldHead.info)          # `(template`
  oldHead.into:
    newBuf.takeTree oldHead     # name (SymbolDef)
    newBuf.takeTree oldHead     # exported marker
    newBuf.takeTree oldHead     # pattern
    let typevarsAt = newBuf.len
    newBuf.takeTree oldHead     # typevars
    let paramsAt = newBuf.len
    newBuf.takeTree oldHead     # params
    newBuf.takeTree oldHead     # return type
    newBuf.takeTree oldHead     # pragmas
    newBuf.takeTree oldHead     # effects
    # oldHead is now positioned at the body.

    let oldRoutine = c.routine
    c.routine = createSemRoutine(TemplateY, c.routine)
    # Mirror `semProcImpl`'s template setup so the lazy body sem matches
    # what phase 3 would do.
    inc c.routine.inLoop
    inc c.routine.inGeneric
    inc c.inGenericDefinition
    c.openScope()  # parameter scope
    c.openScope()  # body scope

    var ctx = createUntypedContext(addr c, UntypedTemplate, dirty = false)
    addParams(ctx, newBuf, typevarsAt)
    addParams(ctx, newBuf, paramsAt)

    # `addParams` populates `ctx.params`, but that is only half of it — see
    # `addTemplFormalsToScope`.
    addTemplFormalsToScope(c, newBuf, typevarsAt)
    addTemplFormalsToScope(c, newBuf, paramsAt)

    semTemplBody ctx, newBuf, oldHead
    # `oldHead` is now past the body, at the template's (possibly elided) close.

    c.closeScope()  # body scope
    c.closeScope()  # parameter scope
    dec c.inGenericDefinition
    c.routine = oldRoutine

    # Closing `)` for the template
    newBuf.addParRi(oldHead.endInfo)

  prog.mem[sym].buffer = newBuf
  prog.mem[sym].phase = SemcheckBodies
  result = true

proc loadSymWithPhase*(c: var SemContext; symId: SymId; targetPhase: SemPhase): LoadResult =
  ## Drive `symId` toward `targetPhase` on demand, then load it. This is the
  ## single entry point for cross-phase symbol resolution: today the only
  ## registered driver is template-body promotion (Signatures -> Bodies, see
  ## `tryPromoteTemplateBody`); as more of the phased passes become on-demand,
  ## additional drivers hang off here rather than being invoked ad hoc at each
  ## consumption site (toward nim-lang/nimony#2064's phase de-rigidification).
  if targetPhase >= SemcheckBodies:
    discard tryPromoteTemplateBody(c, symId)
  if ensurePhase(symId, targetPhase) == PhaseCycle:
    return LoadResult(status: LacksOffset)  # cycle detected
  result = tryLoadSym(symId)

proc expandTemplate*(c: var SemContext; dest: var TokenBuf;
                     templateDecl, args, firstVarargMatch: Cursor;
                     inferred: ptr Table[SymId, Cursor];
                     info: NifLineInfo; errMsg: var string): PluginOutcome =
  var templ = asRoutine(templateDecl, SkipInclBody)

  result = expandPlugin(c, dest, templ, args, errMsg)
  if result != NoPluginRan:
    return

  var e = ExpansionContext(
    newVars: initTable[SymId, SymId](),
    formalParams: initTable[SymId, Cursor](),
    firstVarargMatch: firstVarargMatch,
    inferred: inferred)

  var a = args
  var f = templ.params
  if not f.isDotToken:
    assert f.isParamsTag
    f.into ParamsU:
      while f.hasMore and a.hasMore:
        var param = f
        inc param
        assert param.isSymbolDef
        e.formalParams[param.symId] = a
        skip a
        skip f
      while f.hasMore: skip f  # mop-up if a ran out first

  if templ.body.isDotToken:
    c.buildErr dest, info, "cannot expand template from prototype; possibly a recursive template call"
  else:
    expandTemplateImpl c, dest, e, templ.body

  for _, newVar in e.newVars:
    c.freshSyms.incl newVar

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
  of UnknownToken, EofToken, DotToken, Ident:
    dest.add body
  of Symbol:
    let s = body.symId
    let arg = e.formalParams.getOrDefault(s)
    if arg != default(Cursor):
      dest.addSubtree arg
    else:
      let nv = e.newVars.getOrDefault(s)
      if nv != SymId(0):
        dest.add symToken(nv, body.info)
      else:
        let tv = e.inferred[].getOrDefault(s)
        if tv != default(Cursor):
          dest.addSubtree tv
        else:
          dest.add body # keep Symbol as it was
  of SymbolDef:
    let s = body.symId
    let newDef = newSymId(c, s)
    e.newVars[s] = newDef
    dest.add symdefToken(newDef, body.info)
  of StringLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.add body
  of ParLe:
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
      if arg.kind notin {DotToken, ParRi}:
        while arg.hasMore:
          e.formalParams[vid] = arg
          expandTemplateImpl c, dest, e, forStmt.body
          skip arg
    elif body.exprKind == UnpackX:
      var un = body
      discard enterScope(un) # bounded: `kind` is ParRi for a bare `unpack()`
      var arg = e.firstVarargMatch
      if un.kind == ParRi:
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
      dest.add body
      body.into:
        while body.hasMore:
          expandTemplateImpl c, dest, e, body
          skip body
        dest.addParRi(body.endInfo)
  of ParRi:
    discard "cannot happen: subtree ends are consumed by the bounded scope"

proc expandPlugin(c: var SemContext; dest: var TokenBuf; temp: Routine, args: Cursor): bool =
  result = false
  var p = temp.pragmas
  if p.kind != ParLe:
    return
  p.into:  # (pragmas …)
    while p.hasMore:
      if p.pragmaKind == PluginP:
        p.into PluginP:
          # `.plugin: "path"` — single-string form only.
          var path = StrId(0)
          var pathInfo = p.info
          if p.kind == StringLit:
            path = p.litId
            pathInfo = p.info
          if path != StrId(0):
            var b = createTokenBuf(30)
            b.addParLe StmtsS, args.info
            # Pass the invoked template's name as the first child of the input
            # so a single shared plugin can dispatch on which template was
            # called. The plugin reads it with `pluginName` and skips to
            # the real call-site arguments with `callArgs`.
            b.add identToken(symToIdent(temp.name.symId), args.info)
            var a = args
            while a.hasMore:
              b.takeTree a
            b.addParRi()
            runPlugin(c, dest, pathInfo, pool.strings[path], b.toString)
            result = true
          while p.hasMore: skip p
        if result: return
      else:
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
  newBuf.takeToken oldHead    # `(template`
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
  c.openScope()  # parameter scope
  c.openScope()  # body scope

  var ctx = createUntypedContext(addr c, UntypedTemplate, dirty = false)
  addParams(ctx, newBuf, typevarsAt)
  addParams(ctx, newBuf, paramsAt)

  # `addParams` populates `ctx.params` (used by `getIdentReplaceParams`'s
  # `isTemplParam` check) — but `getIdentReplaceParams` first calls
  # `buildSymChoice`, which scans the actual SemContext scope. The
  # original phase-3 path got params into scope via `semParams`, which
  # ran `addSym` for each param. Lazily promoting from the published
  # decl skips that, so re-attach the params to the scope here.
  block addParamsToScope:
    var p = readonlyCursorAt(newBuf, paramsAt)
    if p.substructureKind == ParamsU:
      p.into ParamsU:
        while p.hasMore:
          let param = asLocal(p)
          if param.name.kind == SymbolDef:
            var nameStr = pool.syms[param.name.symId]
            extractBasename(nameStr)
            if nameStr.len > 0:
              let s = Sym(kind: ParamY, name: param.name.symId, pos: 0)
              addOverloadable(c.currentScope,
                              pool.strings.getOrIncl(nameStr), s)
          skip p

  semTemplBody ctx, newBuf, oldHead
  # `oldHead` is now past the body, sitting on the closing `)`.

  c.closeScope()  # body scope
  c.closeScope()  # parameter scope
  c.routine = oldRoutine

  # Closing `)` for the template
  newBuf.takeToken oldHead

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
                     info: PackedLineInfo) =
  var templ = asRoutine(templateDecl, SkipInclBody)

  if expandPlugin(c, dest, templ, args):
    return

  var e = ExpansionContext(
    newVars: initTable[SymId, SymId](),
    formalParams: initTable[SymId, Cursor](),
    firstVarargMatch: firstVarargMatch,
    inferred: inferred)

  var a = args
  var f = templ.params
  if f.kind != DotToken:
    assert f.isParamsTag
    f.into ParamsU:
      while f.hasMore and a.hasMore:
        var param = f
        inc param
        assert param.kind == SymbolDef
        e.formalParams[param.symId] = a
        skip a
        skip f
      while f.hasMore: skip f  # mop-up if a ran out first

  if templ.body.kind == DotToken:
    c.buildErr dest, info, "cannot expand template from prototype; possibly a recursive template call"
  else:
    expandTemplateImpl c, dest, e, templ.body

  for _, newVar in e.newVars:
    c.freshSyms.incl newVar

#
#
#           Nimony Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Facts extracted from a *semchecked* NIF module (`.s.nif`) for the plugin and
## compiler-pass validator.
##
## The untyped front end (nifler's `.p.nif`) had to guess at everything it
## needed: types by spelling, callees by identifier, variables by name. A
## semchecked module resolves all three, so this module is mostly lookups:
##
## * a local's type is a `SymId`, so `NifCursor` and `Cursor` are one type and
##   two `scan`s in sibling scopes are two variables;
## * a callee is a `SymId`, so `nifcore.skip` is never confused with a
##   same-named local proc;
## * a field's type comes from its object declaration via `tryLoadSym`, even
##   when that object lives in another module — the "same module only"
##   restriction of the untyped registry is gone.
##
## What a semchecked module *loses* is the source idiom: `copyIntoKind`,
## `withTree`, `linearScan` and the whole `Replacer` API are templates, already
## expanded when we see the tree. They are recovered from the marker each of
## those templates carries in its own body, which every expansion of it then
## opens with -- see `markerRole`. What is left for the line info to settle is
## only whether a node is the author's code or somebody else's, which the file
## alone answers.

import std / [tables, sets, strutils, assertions]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs]

type
  TrackedKind* = enum
    tkUnknown     ## not a type we track
    tkCursor      ## `Cursor` — traversal state, has a must-skip obligation
    tkTokenBuf    ## `TokenBuf` — output, has a must-fill obligation
    tkOther       ## a known type, but not one we track

  VarInfo* = object
    sym*: SymId
    name*: string           ## readable name, for diagnostics
    tracked*: TrackedKind
    typ*: SymId             ## nominal type symbol, `NoSymId` when structural
    isMut*: bool            ## `var`/`out` parameter, or a mutable local
    isParam*: bool

  ProcFacts* = object
    sym*: SymId
    name*: string
    info*: NifLineInfo
    params*: Cursor
    body*: Cursor
    vars*: Table[SymId, VarInfo]
    cursorParams*: seq[SymId]   ## `var Cursor` parameters (must-skip obligation)
    hasCursor*: bool
    hasBuffer*: bool

  SemModule* = object
    suffix*: string         ## this module's NIF suffix
    file*: string           ## the `.nim` source it was built from
    root*: Cursor           ## the module's `(stmts)`
    procs*: seq[ProcFacts]

# ---------------------------------------------------------------------------
# Expansion provenance
# ---------------------------------------------------------------------------

type
  NodeOrigin* = enum
    noUser        ## written in the module's own source
    noLibrary     ## code some other file contributed, an expansion included

proc fileOf*(info: NifLineInfo): string =
  ## The raw line-info filename, provenance included.
  if info.file.isValid: pool.filenames[info.file] else: ""

proc sameFileName*(a, b: string): bool =
  ## Compare two source paths the way diagnostics do: both sides can arrive
  ## absolute or relative to the compile's cwd, so the tail is what matters.
  if a.len == 0 or b.len == 0: return false
  let x = a.replace('\\', '/')
  let y = b.replace('\\', '/')
  result = x == y or x.endsWith(y) or y.endsWith(x)

proc originOf*(info: NifLineInfo; moduleFile: string): NodeOrigin =
  ## Whether a node is code the module itself contains. Everything the author
  ## wrote — including a block argument nested *inside* a template expansion —
  ## points at the module's own source, while a template's own statements point
  ## at wherever it was declared, so the two stay distinguishable by file
  ## alone. (`realFile` also settles the forged provenance filenames that
  ## `--inlineframes:on` produces, should a module have been built with it.)
  let f = realFile(fileOf(info))
  if f.len == 0 or sameFileName(f, moduleFile): noUser
  else: noLibrary

# ---------------------------------------------------------------------------
# Symbol lookups
# ---------------------------------------------------------------------------

var declFileCache: Table[SymId, string]

proc declFileOf*(s: SymId): string =
  ## The source file a symbol was declared in, or "" when it cannot be
  ## resolved (a symbol whose module is not in the cache, typically).
  if s == NoSymId: return ""
  if declFileCache.hasKey(s): return declFileCache[s]
  result = ""
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    result = realFile(fileOf(res.decl.info))
  declFileCache[s] = result

proc baseName*(s: SymId): string =
  ## `skip.0.nifcore` -> `skip`.
  if s == NoSymId: return ""
  result = pool.syms[s]
  extractBasename result

proc isDeclaredIn*(s: SymId; fileTail: string): bool =
  ## True when `s` was declared in a file whose path ends in `fileTail`
  ## (`"lib/nifcore.nim"`). This is what makes callee recognition exact: a
  ## pass-local proc named `skip` is not `nifcore`'s.
  let f = declFileOf(s).replace('\\', '/')
  f.len > 0 and f.endsWith(fileTail)

# ---------------------------------------------------------------------------
# Type classification
# ---------------------------------------------------------------------------

const
  CursorDecl = ["lib/nifcore.nim"]
  TokenBufDecl = ["lib/nifcore.nim"]

var trackedCache: Table[SymId, TrackedKind]

proc classifyTypeSym*(s: SymId): TrackedKind =
  ## Classify a nominal type by identity: base name plus the file it was
  ## declared in, so an unrelated `Cursor` in a plugin's own code is not
  ## mistaken for the traversal type.
  if s == NoSymId: return tkUnknown
  if trackedCache.hasKey(s): return trackedCache[s]
  let n = baseName(s)
  result = tkOther
  if n == "Cursor":
    for f in CursorDecl:
      if isDeclaredIn(s, f): result = tkCursor
  elif n == "TokenBuf":
    for f in TokenBufDecl:
      if isDeclaredIn(s, f): result = tkTokenBuf
  trackedCache[s] = result

proc typeSymOf*(typ: Cursor): SymId =
  ## The nominal symbol of a type slot, looking through the wrappers that do
  ## not change identity (`mut`, `out`, `lent`, `sink`, `cursor`).
  var t = typ
  var fuel = 8
  while fuel > 0:
    dec fuel
    if t.kind == Symbol:
      return t.symId
    if not t.isTagLit: return NoSymId
    case t.typeKind
    of MutT, OutT, LentT, SinkT:
      t = childCursor(t)
    else:
      return NoSymId
  NoSymId

proc isMutType*(typ: Cursor): bool {.inline.} =
  typ.isTagLit and typ.typeKind in {MutT, OutT}

proc classifyType*(typ: Cursor): TrackedKind {.inline.} =
  classifyTypeSym(typeSymOf(typ))

# ---------------------------------------------------------------------------
# Field resolution — this is what needed a whole registry before
# ---------------------------------------------------------------------------

var fieldCache: Table[SymId, TrackedKind]
var moduleFields: Table[SymId, TrackedKind]
  ## Fields of the types this module declares. A field is not a top-level
  ## declaration, so it has no index entry of its own and `tryLoadSym` cannot
  ## reach it — which is why `c.dest` went unrecognised until these were
  ## collected from the module's own tree.

proc fieldTracked*(fld: SymId): TrackedKind =
  ## The tracked kind of an object field: from this module's own declarations,
  ## or across modules through the declaring module's index.
  if fld == NoSymId: return tkUnknown
  if moduleFields.hasKey(fld): return moduleFields[fld]
  if fieldCache.hasKey(fld): return fieldCache[fld]
  result = tkUnknown
  let res = tryLoadSym(fld)
  if res.status == LacksNothing and res.decl.symKind in {FldY, GfldY}:
    result = classifyType(asLocal(res.decl).typ)
  fieldCache[fld] = result

var moduleBufferObjects: HashSet[SymId]
  ## Types of this module that hold a `TokenBuf`; see `moduleFields`.

proc objectHasBufferField*(typ: SymId): bool =
  ## True when `typ` is an object with a `TokenBuf` field — the pattern that
  ## makes `c: var Context` count as buffer access.
  if typ == NoSymId: return false
  if typ in moduleBufferObjects: return true
  let res = tryLoadSym(typ)
  if res.status != LacksNothing: return false
  let decl = asTypeDecl(res.decl)
  if decl.body.typeKind != ObjectT: return false
  var n = childCursor(decl.body)
  if n.hasMore: skip n   # base type
  while n.hasMore:
    if n.isTagLit and n.symKind in {FldY, GfldY}:
      if classifyType(asLocal(n).typ) == tkTokenBuf: return true
    skip n
  false

# ---------------------------------------------------------------------------
# Roles — declared by the API, not tabulated here
# ---------------------------------------------------------------------------

type
  OpRole* = enum
    roleNone        ## nothing we track
    roleAdvance     ## `{.nifAdvance.}` — moves the cursor, emits nothing
    roleBalanced    ## `{.nifBalanced.}` — moves it and emits what it moved over
    roleWrap        ## `{.nifWrap.}` — opens a tree, runs a body, closes it
    roleReads       ## `{.nifReads.}` — consumes a structural unit
    roleDelegates   ## `{.nifDelegates.}` — hands the cursor to another pass
    roleEmits       ## emits without consuming; inferred from the signature
    roleOpens       ## `{.nifOpens.}` — opens a tree; its 2nd argument is the tag
    roleCloses      ## `{.nifCloses.}` — closes the innermost open tree

proc roleOfPragmaName(n: string): OpRole =
  case n
  of "nifAdvance": roleAdvance
  of "nifBalanced": roleBalanced
  of "nifWrap": roleWrap
  of "nifReads": roleReads
  of "nifDelegates": roleDelegates
  of "nifOpens": roleOpens
  of "nifCloses": roleCloses
  of "nifEmits": roleEmits
  else: roleNone

proc declaredRole(pragmas: Cursor): OpRole =
  ## `(pragmas (pragma <sym>) (inline) …)` — a custom pragma survives sem as
  ## the symbol it resolved to, which is why this is an identity test.
  result = roleNone
  if not pragmas.isTagLit or pragmas.substructureKind != PragmasU: return
  var n = childCursor(pragmas)
  while n.hasMore:
    if n.isTagLit and n.pragmaKind == PragmaP:
      var arg = childCursor(n)
      if arg.hasMore and arg.kind == Symbol:
        let r = roleOfPragmaName(baseName(arg.symId))
        if r != roleNone: return r
    skip n

proc pragmaArg*(pragmas: Cursor; name: string): Cursor =
  ## The first argument of a named custom pragma on a declaration, or a nil
  ## cursor. The arguments are preserved unchecked, so this is the raw tree the
  ## author wrote.
  result = default(Cursor)
  if not pragmas.isTagLit or pragmas.substructureKind != PragmasU: return
  var n = childCursor(pragmas)
  while n.hasMore:
    if n.isTagLit and n.pragmaKind == PragmaP:
      var arg = childCursor(n)
      if arg.hasMore and arg.kind == Symbol and baseName(arg.symId) == name:
        skip arg
        if arg.hasMore: return arg
    skip n

proc emittedKindOf*(s: SymId): string =
  ## The grammar letter a `{.nifEmits: X.}` routine contributes, or "".
  if s == NoSymId: return ""
  let res = tryLoadSym(s)
  if res.status != LacksNothing or not isRoutine(res.decl.symKind): return ""
  let arg = pragmaArg(asRoutine(res.decl).pragmas, "nifEmits")
  if not arg.hasMore: ""          # no such pragma: `pragmaArg` yields a nil cursor
  elif arg.kind == StrLit: arg.strVal
  else: ""

proc emitsBySignature(params: Cursor): bool =
  ## A routine whose first parameter is a `var TokenBuf` and which takes no
  ## `var Cursor` emits and nothing else. That is what the signature already
  ## says, so the forty `add*` overloads do not each need an annotation — and
  ## unlike a name table it cannot mistake `seq.add` for `TokenBuf.add`.
  if not params.isTagLit or params.substructureKind != ParamsU: return false
  var n = childCursor(params)
  var first = true
  var firstIsBuf = false
  while n.hasMore:
    if n.isTagLit and n.symKind == ParamY:
      let typ = asLocal(n).typ
      let tracked = classifyType(typ)
      if tracked == tkCursor and isMutType(typ): return false
      if first: firstIsBuf = tracked == tkTokenBuf and isMutType(typ)
      first = false
    skip n
  firstIsBuf

var roleCache: Table[SymId, OpRole]

proc roleOfSym*(s: SymId): OpRole =
  ## The role a routine declares, read from its declaration — reachable for a
  ## proc through the callee symbol and for a template through the symbol its
  ## expansion's provenance names.
  if s == NoSymId: return roleNone
  if roleCache.hasKey(s): return roleCache[s]
  result = roleNone
  let res = tryLoadSym(s)
  if res.status == LacksNothing and isRoutine(res.decl.symKind):
    let r = asRoutine(res.decl)
    result = declaredRole(r.pragmas)
    if result == roleNone and emitsBySignature(r.params):
      result = roleEmits
  roleCache[s] = result

var noReturnCache: Table[SymId, bool]

proc isNoReturn*(s: SymId): bool =
  ## `{.noreturn.}` on the callee's declaration. A call to one ends the path it
  ## stands on, which is why `error "…"` as the last word of a `case` branch is
  ## not a branch that failed to advance the cursor.
  if s == NoSymId: return false
  if noReturnCache.hasKey(s): return noReturnCache[s]
  result = false
  let res = tryLoadSym(s)
  if res.status == LacksNothing and isRoutine(res.decl.symKind):
    var n = asRoutine(res.decl).pragmas
    if n.isTagLit and n.substructureKind == PragmasU:
      var p = childCursor(n)
      while p.hasMore:
        if p.isTagLit and p.pragmaKind == NoreturnP:
          result = true
          break
        skip p
  noReturnCache[s] = result

proc markerRole*(n: Cursor): OpRole =
  ## The role a template *expansion* declares about itself.
  ##
  ## A wrapper writes `{.nifWrap.}` as the first statement of its body, so
  ## every expansion of it opens with `(pragmas (pragma nifWrap))` — the marker
  ## stands at the head of exactly the region it describes. That is the whole
  ## mechanism: no provenance to decode, no declaration to go and read, and
  ## nothing that depends on the module having been built with
  ## `--inlineframes:on`.
  result = roleNone
  if not n.isTagLit or n.stmtKind != StmtsS: return
  var c = childCursor(n)
  if not c.isTagLit or c.substructureKind != PragmasU: return
  var prag = childCursor(c)
  if not prag.isTagLit or prag.pragmaKind != PragmaP: return
  var arg = childCursor(prag)
  if not arg.hasMore: return
  if arg.kind == Symbol: result = roleOfPragmaName(baseName(arg.symId))
  elif arg.kind == Ident: result = roleOfPragmaName(arg.strVal)

# ---------------------------------------------------------------------------
# Lvalues
# ---------------------------------------------------------------------------

proc unwrapAddr*(n: Cursor): Cursor =
  ## Post-sem, an argument passed to a `var` parameter is wrapped in `haddr`
  ## (or `addr`). The lvalue underneath is what the call operates on.
  result = n
  var fuel = 4
  while fuel > 0 and result.isTagLit and
      result.exprKind in {HaddrX, AddrX, HderefX, DerefX}:
    dec fuel
    result = childCursor(result)

proc rootSymOf*(n: Cursor): SymId =
  ## The variable an lvalue is rooted in: `c`, `c.dest`, `c.dest[i]` all root
  ## in `c`.
  var n = unwrapAddr(n)
  var fuel = 16
  while fuel > 0:
    dec fuel
    if n.kind == Symbol: return n.symId
    if not n.isTagLit: return NoSymId
    case n.exprKind
    of DotX, AtX, DerefX, HderefX, HaddrX, AddrX, TupatX, DdotX:
      n = childCursor(n)
    else:
      return NoSymId
  NoSymId

const FieldAccess* = {DotX, DdotX}
  ## `ddot` is `dot` through a pointer. A `var` parameter *is* a pointer at
  ## this stage, so `c.dest` inside a pass whose context is a `var Context`
  ## comes out as `(ddot …)` — treating only `dot` as field access left every
  ## such buffer unrecognised.

proc dotField*(n: Cursor): SymId =
  ## The field symbol of a `(dot obj fld level suffix)` node.
  if not n.isTagLit or n.exprKind notin FieldAccess: return NoSymId
  var c = childCursor(n)
  if c.hasMore: skip c    # the object
  if c.hasMore and c.kind == Symbol: return c.symId
  NoSymId

proc sameLvalue*(a, b: Cursor): bool =
  ## Identity of two lvalues, once the `haddr` a `var` argument is wrapped in
  ## is off both sides. Symbols make this exact where the untyped engine
  ## compared rendered strings; the rest is `sameTrees`, which also settles the
  ## literal children `(dot obj fld 0 "suffix")` carries — comparing only
  ## symbols and tags made every field access differ from itself.
  let x = unwrapAddr(a)
  let y = unwrapAddr(b)
  if not x.hasMore or not y.hasMore: return false
  sameTrees(x, y)

proc lvalueToStr*(n: Cursor; vars: Table[SymId, VarInfo]): string =
  ## Render an lvalue for a diagnostic, using the readable local names.
  let x = unwrapAddr(n)
  if x.kind == Symbol:
    let s = x.symId
    if vars.hasKey(s): return vars[s].name
    return baseName(s)
  if x.isTagLit and x.exprKind in FieldAccess:
    var c = childCursor(x)
    let recv = lvalueToStr(c, vars)
    let fld = dotField(x)
    if fld != NoSymId: return recv & "." & baseName(fld)
    return recv
  result = "<expr>"

# ---------------------------------------------------------------------------
# Expression classification
# ---------------------------------------------------------------------------

proc classifyExpr*(p: ProcFacts; n: Cursor): TrackedKind =
  ## Classify an expression as cursor, buffer or neither. A variable whose
  ## object type has a `TokenBuf` field counts as buffer access, matching the
  ## untyped engine's `c: var Context` rule.
  let x = unwrapAddr(n)
  if x.kind == Symbol:
    let s = x.symId
    if p.vars.hasKey(s):
      let v = p.vars[s]
      if v.tracked in {tkCursor, tkTokenBuf}: return v.tracked
      if objectHasBufferField(v.typ): return tkTokenBuf
      return v.tracked
    return tkUnknown
  if x.isTagLit and x.exprKind in FieldAccess:
    let fld = dotField(x)
    if fld != NoSymId: return fieldTracked(fld)
  tkUnknown

# ---------------------------------------------------------------------------
# Calls
# ---------------------------------------------------------------------------

proc calleeSym*(n: Cursor): SymId =
  ## The symbol a call resolves to. Post-sem the callee is the first child and
  ## is always a symbol for a resolved call.
  if not n.isTagLit or n.exprKind notin CallKinds: return NoSymId
  var c = childCursor(n)
  if c.kind == Symbol: return c.symId
  NoSymId

iterator callArgs*(n: Cursor): Cursor =
  ## The arguments of a call, callee excluded.
  if n.isTagLit and n.exprKind in CallKinds:
    var c = childCursor(n)
    if c.hasMore: skip c   # callee
    while c.hasMore:
      yield c
      skip c

proc hasTrackedArg*(p: ProcFacts; n: Cursor; k: TrackedKind): bool =
  for a in callArgs(n):
    if classifyExpr(p, a) == k: return true
  false

proc isDirectBuffer*(p: ProcFacts; a: Cursor): bool =
  ## A `TokenBuf` this routine can write to *here*: a buffer variable or
  ## parameter, or a buffer field reached explicitly (`c.dest`). Being handed
  ## the whole context object is not it -- `error c, "…", n` passes something
  ## that merely *contains* a buffer, which is why "has a buffer" and "emits"
  ## are different questions.
  let x = unwrapAddr(a)
  if x.kind == Symbol:
    if p.vars.hasKey(x.symId): return p.vars[x.symId].tracked == tkTokenBuf
    return false
  if x.isTagLit and x.exprKind in FieldAccess:
    let fld = dotField(x)
    if fld != NoSymId: return fieldTracked(fld) == tkTokenBuf
  false

proc movesCursor*(p: ProcFacts; n: Cursor): bool =
  ## Whether a call could *move* a tracked cursor, as opposed to merely reading
  ## one. `skip n` moves it; `n.info` and `getType(c.typeCache, n)` do not, and
  ## counting those as advances makes every read look like dropped input.
  ##
  ## Post-sem the two are told apart by the wrapper, and the wrapper is the
  ## opposite way round for a local and for a `var` parameter: a local handed to
  ## a `var` parameter is `(haddr x)` while handed by value it is bare `x`; a
  ## `var` parameter handed on is bare `n` while *read* it is `(hderef n)`.
  ## Unwrapping both -- which is what `classifyExpr` does, and is right when the
  ## question is merely *which* cursor an argument names -- erases exactly the
  ## distinction this asks about.
  for a in callArgs(n):
    if a.kind == Symbol:
      if p.vars.hasKey(a.symId):
        let v = p.vars[a.symId]
        if v.tracked == tkCursor and v.isParam and v.isMut: return true
    elif a.isTagLit and a.exprKind in {HaddrX, AddrX}:
      if classifyExpr(p, childCursor(a)) == tkCursor: return true
  false

# ---------------------------------------------------------------------------
# Collecting the module's facts
# ---------------------------------------------------------------------------

proc addVar(p: var ProcFacts; name, typ: Cursor; isParam, isMut: bool) =
  if name.kind != SymbolDef: return
  let s = name.symId
  let tracked = classifyType(typ)
  p.vars[s] = VarInfo(sym: s, name: baseName(s), tracked: tracked,
                      typ: typeSymOf(typ), isMut: isMut, isParam: isParam)
  if tracked == tkTokenBuf:
    p.hasBuffer = true
  elif tracked == tkOther and objectHasBufferField(typeSymOf(typ)):
    p.hasBuffer = true

proc collectParams(p: var ProcFacts) =
  if not p.params.isTagLit or p.params.substructureKind != ParamsU: return
  var n = childCursor(p.params)
  while n.hasMore:
    if n.isTagLit and n.symKind == ParamY:
      let local = asLocal(n)
      p.addVar local.name, local.typ, isParam = true, isMut = isMutType(local.typ)
      if classifyType(local.typ) == tkCursor and isMutType(local.typ):
        p.cursorParams.add local.name.symId
        p.hasCursor = true
    skip n

const
  MutableDecls = {VarY, CursorY, ResultY, GvarY, TvarY}
    ## Mutability of a local is its declaration's, not its type's: a `let` of
    ## type `Cursor` cannot be advanced, a `var` of the same type can.

proc collectLocals(p: var ProcFacts) =
  if not p.body.isTagLit: return
  var n = p.body
  n.linearScan:
    let k = n.symKind
    if k in {VarY, LetY, CursorY, ConstY, ResultY, GvarY, TvarY, GletY, TletY}:
      let local = asLocal(n)
      p.addVar local.name, local.typ, isParam = false, isMut = k in MutableDecls

proc collectObjectFields(body: Cursor; hasBuf: var bool) =
  ## Record every field of an object, variant branches included.
  if not body.isTagLit: return
  var n = childCursor(body)
  while n.hasMore:
    if n.isTagLit:
      if n.symKind in {FldY, GfldY}:
        let local = asLocal(n)
        if local.name.kind == SymbolDef:
          let k = classifyType(local.typ)
          moduleFields[local.name.symId] = k
          if k == tkTokenBuf: hasBuf = true
      else:
        collectObjectFields(n, hasBuf)   # `(case …)` in a variant object
    skip n

proc collectTypes(m: var SemModule) =
  var n = m.root
  if not n.isTagLit: return
  var c = childCursor(n)
  while c.hasMore:
    if c.isTagLit and c.symKind == TypeY:
      let decl = asTypeDecl(c)
      if decl.body.typeKind == ObjectT and decl.name.kind == SymbolDef:
        var hasBuf = false
        collectObjectFields(decl.body, hasBuf)
        if hasBuf: moduleBufferObjects.incl decl.name.symId
    skip c

proc collectProcsIn(m: var SemModule; n: Cursor) =
  ## Every routine *declared in this module* — an imported generic that got
  ## instantiated here is not the plugin author's code and is not checked.
  ##
  ## Descends through nested statement lists: an `include` splices the included
  ## file's declarations in behind a `(stmts)` of their own, and those are the
  ## file's own procs, not someone else's.
  var c = childCursor(n)
  while c.hasMore:
    if c.isTagLit and c.stmtKind == StmtsS:
      collectProcsIn(m, c)
    elif c.isTagLit and c.symKind in {ProcY, FuncY, IteratorY, ConverterY, MethodY}:
      let r = asRoutine(c, SkipInclBody)
      if r.name.kind == SymbolDef and r.body.isTagLit:
        let s = r.name.symId
        # Owned by this module *and* written in its source. A generic
        # instantiated here (`seq[SymId]`'s `add`, `=destroy`, …) is emitted
        # into this module's NIF under this module's suffix and even inherits
        # the instantiation site's file, so the name is what gives it away:
        # an instance carries its `I…` key.
        if extractModule(pool.syms[s]) == m.suffix and
            not isInstantiation(pool.syms[s]) and
            sameFileName(realFile(fileOf(c.info)), m.file):
          var p = ProcFacts(sym: s, name: baseName(s), info: c.info,
                            params: r.params, body: r.body,
                            vars: initTable[SymId, VarInfo]())
          p.collectParams()
          p.collectLocals()
          m.procs.add p
    skip c

proc collectProcs*(m: var SemModule) =
  if m.root.isTagLit: collectProcsIn(m, m.root)

proc openSemModule*(nifFile, sourceFile: string; owningBuf: var TokenBuf): SemModule =
  ## Load a `.s.nif` and its world: `setupProgram` registers the module and
  ## points `suffixToNif` at the same cache directory, which is what lets
  ## `tryLoadSym` reach declarations in the modules this one imports.
  result = SemModule(file: sourceFile)
  result.root = setupProgram(nifFile, nifFile, owningBuf, hasIndex = true)
  result.suffix = prog.main.name
  if result.file.len == 0:
    result.file = realFile(fileOf(result.root.info))
  result.collectTypes()
  result.collectProcs()

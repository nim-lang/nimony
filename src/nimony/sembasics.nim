#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Lots of basic helpers for semantic checking.

import std / [tables, sets, hashes, syncio, formatfloat, assertions]
include ".." / lib / nifprelude
include ".." / lib / compat2
import nimony_model, symtabs, builtintypes, decls, asthelpers,
  programs, sigmatch, magics, reporters, nifconfig,
  intervals, xints, features,
  semdata, semos, expreval
import ".." / lib / [symparser, nifindexes]

import ".." / gear2 / modnames

# -------------- symbol lookups -------------------------------------

type
  ChoiceOption* = enum
    FindAll, FindOverloads, InnerMost

template buildTree*(dest: var TokenBuf; kind: StmtKind|ExprKind|TypeKind|SymKind|NimonyOther;
                    info: PackedLineInfo; body: untyped) =
  addParLe(dest, kind, info)
  body
  dest.addParRi()

proc considerImportedSymbols(c: var SemContext; dest: var TokenBuf; name: StrId; info: PackedLineInfo;
                             option = FindAll): int =
  result = 0
  let ignoreStyle = IgnoreStyleFeature in c.features
  for realName in stylesOfImport(c.importTab, name, ignoreStyle):
    for moduleId in c.importTab.getOrDefault(realName):
      let iface = addr c.importedModules.getOrQuit(moduleId).iface
      for foreignName in stylesOfIface(iface[], realName, ignoreStyle):
        let candidates = addr iface[].getOrQuit(foreignName)
        for defId in candidates[]:
          # when resolving a caller `fn`, keep re-exported modules out of the
          # sym choice so `foo[...]` binds the proc, not a same-named module
          # (nim-lang/nimony#2130):
          var callable = true
          if option == FindOverloads:
            let res = tryLoadSym(defId)
            if res.status == LacksNothing:
              callable = isValidFnHead(res.decl.symKind)
          if callable:
            inc result
            dest.addSymUse(defId, info)

proc addSymUse*(dest: var TokenBuf; s: Sym; info: PackedLineInfo) =
  dest.addSymUse(s.name, info)

proc buildSymChoiceForDot(c: var SemContext; dest: var TokenBuf; identifier: StrId; info: PackedLineInfo) =
  # not used yet
  var count = 0
  let oldLen = dest.len
  let ignoreStyle = IgnoreStyleFeature in c.features
  dest.buildTree OchoiceX, info:
    var it = c.currentScope
    while it != nil:
      for k in stylesOfScope(it, identifier, ignoreStyle):
        for sym in it.tab.getOrDefault(k):
          if sym.kind in {ProcY, FuncY, ConverterY, MethodY, TemplateY, MacroY, IteratorY, TypeY}:
            dest.addSymUse sym, info
            inc count
      it = it.up
    inc count, considerImportedSymbols(c, dest, identifier, info)

  # if the sym choice is empty, create an ident node:
  if count == 0:
    dest.shrink oldLen
    dest.addIdent(identifier, info)

proc isNonOverloadable*(t: SymKind): bool {.inline.} =
  t in {LetY, VarY, ParamY, TypevarY, StaticTypevarY, ConstY, TypeY, ResultY, FldY, GfldY, CursorY, PatternvarY, BlockY, GletY, TletY, GvarY, TvarY}

proc buildSymChoiceForSelfModule*(c: var SemContext; dest: var TokenBuf;
                                  identifier: StrId; info: PackedLineInfo): int =
  result = 0
  let oldLen = dest.len
  let ignoreStyle = IgnoreStyleFeature in c.features
  dest.buildTree OchoiceX, info:
    # add symbols from top scope:
    var it = c.currentScope
    while it.up != nil: it = it.up
    for k in stylesOfScope(it, identifier, ignoreStyle):
      for sym in it.tab.getOrDefault(k):
        dest.addSymUse sym, info
        inc result
  # if the sym choice is empty, create an ident node:
  if result == 0:
    dest.shrink oldLen
    dest.addIdent(identifier, info)

iterator topLevelSyms*(c: var SemContext; identifier: StrId): SymId {.sideEffect.} =
  let ignoreStyle = IgnoreStyleFeature in c.features
  var it = c.currentScope
  while it.up != nil: it = it.up
  for k in stylesOfScope(it, identifier, ignoreStyle):
    for sym in it.tab.getOrDefault(k):
      yield sym.name

proc rawBuildSymChoiceForForeignModule(c: var SemContext; dest: var TokenBuf; module: SymId;
                                       identifier: StrId; info: PackedLineInfo;
                                       marker: var HashSet[SymId]): int =
  result = 0
  let m = addr c.importedModules.getOrQuit(module)
  let ignoreStyle = IgnoreStyleFeature in c.features
  for k in stylesOfIface(m[].iface, identifier, ignoreStyle):
    let candidates = m[].iface.getOrDefault(k)
    for defId in candidates:
      if not marker.containsOrIncl(defId):
        dest.addSymUse(defId, info)
      inc result
  for forward, filter in m[].exports:
    if filterAllows(filter, identifier):
      inc result, rawBuildSymChoiceForForeignModule(c, dest, forward, identifier, info, marker)

proc buildSymChoiceForForeignModule*(c: var SemContext; dest: var TokenBuf; module: SymId;
                                     identifier: StrId; info: PackedLineInfo): int =
  let oldLen = dest.len
  dest.buildTree OchoiceX, info:
    var marker = initHashSet[SymId]()
    result = rawBuildSymChoiceForForeignModule(c, dest, module, identifier, info, marker)
  # if the sym choice is empty, create an ident node:
  if result == 0:
    dest.shrink oldLen
    dest.addIdent(identifier, info)

proc rawBuildSymChoice(c: var SemContext; dest: var TokenBuf; identifier: StrId; info: PackedLineInfo;
                       option = FindAll): int =
  result = 0
  let ignoreStyle = IgnoreStyleFeature in c.features
  var it = c.currentScope
  while it != nil:
    var nonOverloadable = 0
    for k in stylesOfScope(it, identifier, ignoreStyle):
      for sym in it.tab.getOrDefault(k):
        # when resolving a caller `fn`, keep the module symbol out of the sym
        # choice so `foo[...]` binds a same-named proc (nim-lang/nimony#2130):
        if option != FindOverloads or isValidFnHead(sym.kind):
          dest.addSymUse sym, info
          inc result
          if sym.kind.isNonOverloadable:
            inc nonOverloadable
    if result == 1 and (option == InnerMost or
        (option == FindOverloads and nonOverloadable == 1)):
      # unambiguous local symbol found
      # in case of FindOverloads, if symbol is overloadable, consider other overloads
      return
    it = it.up
  inc result, considerImportedSymbols(c, dest, identifier, info, option)

proc buildSymChoice*(c: var SemContext; dest: var TokenBuf; identifier: StrId; info: PackedLineInfo;
                    option: ChoiceOption): int =
  let oldLen = dest.len
  dest.buildTree OchoiceX, info:
    result = rawBuildSymChoice(c, dest, identifier, info, option)
  # if the sym choice is empty, create an ident node:
  if result == 0:
    dest.shrink oldLen
    dest.addIdent(identifier, info)

proc addSymChoiceSyms*(c: var SemContext; dest: var TokenBuf; identifier: StrId; marker: var HashSet[SymId]; info: PackedLineInfo) =
  # like rawBuildSymChoice but adds to an existing symchoice, ignoring duplicates
  let ignoreStyle = IgnoreStyleFeature in c.features
  var it = c.currentScope
  while it != nil:
    for k in stylesOfScope(it, identifier, ignoreStyle):
      for sym in it.tab.getOrDefault(k):
        if not marker.containsOrIncl(sym.name):
          dest.addSymUse sym, info
    it = it.up
  # mirror considerImportedSymbols:
  for realName in stylesOfImport(c.importTab, identifier, ignoreStyle):
    for moduleId in c.importTab.getOrDefault(realName):
      let iface = addr c.importedModules.getOrQuit(moduleId).iface
      for foreignName in stylesOfIface(iface[], realName, ignoreStyle):
        let candidates = addr iface[].getOrQuit(foreignName)
        for defId in candidates[]:
          if not marker.containsOrIncl(defId):
            dest.addSymUse(defId, info)

proc isDeclared*(c: var SemContext; name: StrId): bool =
  let ignoreStyle = IgnoreStyleFeature in c.features
  var scope = c.currentScope
  while scope != nil:
    for k in stylesOfScope(scope, name, ignoreStyle):
      if k in scope.tab: return true
    scope = scope.up
  for k in stylesOfImport(c.importTab, name, ignoreStyle):
    if k in c.importTab: return true
  result = false

proc openScope*(c: var SemContext) =
  c.currentScope = Scope(tab: initTable[StrId, seq[Sym]](), up: c.currentScope, kind: NormalScope)

proc closeScope*(c: var SemContext) =
  c.currentScope = c.currentScope.up

template withNewScope*(c: var SemContext; body: untyped) =
  openScope(c)
  try:
    body
  finally:
    closeScope(c)

# -------------------------- error handling -------------------------

proc pushErrorContext*(c: var SemContext; info: PackedLineInfo) = c.instantiatedFrom.add info
proc popErrorContext(c: var SemContext) = discard c.instantiatedFrom.pop

template withErrorContext*(c: var SemContext; info: PackedLineInfo; body: untyped) =
  pushErrorContext(c, info)
  try:
    body
  finally:
    popErrorContext(c)

proc buildErr*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; msg: string; orig: Cursor) =
  when defined(debugBuildErr):
    if not c.debugAllowErrors:
      writeStackTrace()
      for instFrom in items(c.instantiatedFrom):
        echo "instantiated from: ", infoToStr(instFrom)

      echo infoToStr(info) & " Error: " & msg
      if not orig.isDotToken:
        echo "Source: ", toString(orig, false)
      quit 1
  var n = orig
  var hasErr = false
  if n.isTagLit:
    if n.tagId == nifstreams.ErrT:
      hasErr = true
    else:
      n.linearScan:
        if n.tagId == nifstreams.ErrT:
          hasErr = true
          break
  let info = if hasErr: n.info else: info
  dest.buildTree ErrT, info:
    if hasErr:
      inc n
      dest.takeTree n
    else:
      dest.addSubtree orig
    for instFrom in items(c.instantiatedFrom):
      dest.addDotToken(instFrom)
    if hasErr:
      while n.isDotToken: inc n
      dest.takeTree n
    else:
      dest.addStrLit(msg, info)

proc buildErr*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; msg: string) =
  var orig = createTokenBuf(1)
  orig.addDotToken()
  c.buildErr dest, info, msg, cursorAt(orig, 0)

proc combineErr*(c: var SemContext; dest: var TokenBuf; pos: int; info: PackedLineInfo; msg: string; orig: Cursor) =
  ## Builds ErrT node and combine it with the node at `pos` so that no nodes are added outside of
  ## the node at `pos`.
  ## When there is no node at `pos`, New ErrT node is added to `c.dest`.
  ## Assumes the node at `pos` is the last node.
  var needsParRi = false
  if dest.len > pos:
    needsParRi = true
    if dest[pos].stmtKind == StmtsS:
      dest.reopenLastTree pos
    else:
      # nifcore cannot splice an unbalanced open; skip the stmts wrapper for
      # this error-recovery path (the err node is still emitted below).
      needsParRi = false
  buildErr c, dest, info, msg, orig
  if needsParRi:
    dest.addParRi

proc combineErr*(c: var SemContext; dest: var TokenBuf; pos: int; info: PackedLineInfo; msg: string) =
  var orig = createTokenBuf(1)
  orig.addDotToken()
  c.combineErr dest, pos, info, msg, cursorAt(orig, 0)

proc buildLocalErr*(dest: var TokenBuf; info: PackedLineInfo; msg: string; orig: Cursor) =
  when defined(debug):
    if true: # not c.debugAllowErrors: - c not given
      writeStackTrace()
      echo infoToStr(info) & " Error: " & msg
      quit msg
  dest.buildTree ErrT, info:
    dest.addSubtree orig
    dest.addStrLit(msg, info)

proc buildLocalErr*(dest: var TokenBuf; info: PackedLineInfo; msg: string) =
  var orig = createTokenBuf(1)
  orig.addDotToken()
  dest.buildLocalErr info, msg, cursorAt(orig, 0)

# -------------------------- type handling ---------------------------

proc ptrTypeOf*(c: var SemContext; dest: var TokenBuf; typ: TypeCursor): TypeCursor =
  let typeBegin = dest.len
  dest.addParLe PtrT, typ.info
  dest.addSubtree typ.skipModifier
  # Mirror `semLocalTypeImpl`'s PtrT/RefT path: under `{.feature: "lenientnils".}`
  # an implicit `ptr T` is unchecked, otherwise it defaults to notnil.
  if LenientNilsFeature in c.features:
    dest.addParPair UncheckedU, typ.info
  else:
    dest.addParPair NotnilU, typ.info
  dest.addParRi()
  result = typeToCursor(c, dest, typeBegin)
  dest.shrink typeBegin

proc declToCursor*(c: var SemContext; dest: var TokenBuf; s: Sym): LoadResult =
  if knowsSym(s.name) or s.pos == ImportedPos:
    result = tryLoadSym(s.name)
  elif s.pos > 0:
    var buf = createTokenBuf(10)
    # XXX optimize this for non-generic procs. No need to
    # copy their bodies here.
    let decl = cursorAt(dest, s.pos - 1)
    buf.addSubtree decl
    endRead(dest)
    result = LoadResult(status: LacksNothing, decl: cursorAt(buf, 0))
    programs.publish s.name, buf, c.phase
  else:
    result = LoadResult(status: LacksPosition)

# --------------------- symbol name creation -------------------------

proc makeGlobalSym*(c: var SemContext; result: var string) =
  var counter = addr c.globals.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]
  result.add '.'
  result.add c.thisModuleSuffix

proc makeFieldSym*(c: var SemContext; result: var string) =
  # Fields are scoped to their owning object type, so they are NOT numbered with
  # a global running counter (which made the same field diverge — `a.0` vs `a.2`
  # — depending on semcheck order). Within a type a field is `.0`; the number is
  # only bumped when an ancestor type already declares the same field name.
  # `c.fieldCounts` holds the seed counts for the object currently being
  # declared (set up and torn down by `semObjectType`).
  var counter = addr c.fieldCounts.mgetOrPut(result, 0)
  let n = counter[]
  counter[] += 1
  result.add '.'
  result.addInt n

proc makeLocalSym*(c: var SemContext; result: var string) =
  var counter = addr c.locals.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]

proc newSymId*(c: var SemContext; s: SymId): SymId =
  var isGlobal = false
  var name = extractBasename(pool.syms[s], isGlobal)
  if isGlobal:
    c.makeGlobalSym(name)
  else:
    c.makeLocalSym(name)
  result = pool.syms.getOrIncl(name)

proc classifyType*(c: var SemContext; n: Cursor): TypeKind =
  result = typeKind(n)

proc hasErrorSince*(dest: TokenBuf; start: int): bool =
  ## True when `dest[start..]` already contains an `(err ...)` node. Used to
  ## avoid stacking a redundant follow-up error on top of one semExpr already
  ## produced (e.g. `auto`-typed expression from an undeclared identifier).
  let errTag = pool.tags.getOrIncl("err")
  var i = start
  result = false
  while i < dest.len:
    if dest[i].kind == OpenTagKind and dest[i].tagId == errTag:
      result = true
      break
    inc i

proc makeTemplateSym*(c: var SemContext; result: var string) =
  ## Make a fresh symbol for a name introduced by a template body. Templates
  ## need a separate mechanism from `makeLocalSym` so that cross-module
  ## interference can be addressed without promoting the sym to global
  ## layout — i.e. without mangling in `c.thisModuleSuffix`.
  var counter = addr c.locals.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]

type
  SymStatus* = enum
    ErrNoIdent, ErrRedef, OkNew, OkExisting, OkExistingFresh

  DelayedSym* = object
    status*: SymStatus
    lit*: StrId
    s*: Sym
    info*: PackedLineInfo

proc identToSym*(c: var SemContext; str: sink string; kind: SymKind): SymId =
  var name = str
  # Replace dots with spaces to avoid conflicts with NIF symbol format
  when false:
    # XXX activate this later!
    for i in 0..<name.len:
      if name[i] == '.': name[i] = ' '
  if kind in {FldY, GfldY}:
    c.makeFieldSym(name)
  elif c.currentScope.kind == ToplevelScope or
      kind in {TypevarY, StaticTypevarY, ProcY, FuncY, ConverterY, MethodY, TemplateY, MacroY, IteratorY, TypeY, EfldY}:
    c.makeGlobalSym(name)
  else:
    c.makeLocalSym(name)
  result = pool.syms.getOrIncl(name)

proc identToSym*(c: var SemContext; lit: StrId; kind: SymKind): SymId =
  result = identToSym(c, pool.strings[lit], kind)

proc symToIdent*(s: SymId): StrId =
  var name = pool.syms[s]
  extractBasename name
  when false:
    # XXX activate this later!
    for i in 0..<name.len:
      if name[i] == ' ': name[i] = '.'
  result = pool.strings.getOrIncl name

proc declareSym*(c: var SemContext; dest: var TokenBuf; it: var Item; kind: SymKind): SymStatus =
  let info = it.n.info
  if it.n.isSymbolDef:
    if not c.freshSyms.missingOrExcl(it.n.symId):
      result = OkExistingFresh
    else:
      result = OkExisting
    inc it.n
  else:
    let lit = takeIdent(it.n)
    if lit == StrId(0):
      c.buildErr dest, info, "identifier expected"
      result = ErrNoIdent
    else:
      let s = Sym(kind: kind, name: identToSym(c, lit, kind),
                  pos: dest.len)
      if addNonOverloadable(c.currentScope, lit, s) == Conflict:
        c.buildErr dest, info, "attempt to redeclare: " & pool.strings[lit]
        result = ErrRedef
      else:
        dest.addSymDef(s.name, info)
        result = OkNew

proc declareOverloadableSym*(c: var SemContext; dest: var TokenBuf; it: var Item; kind: SymKind): (SymId, SymStatus) =
  let info = it.n.info
  if it.n.isSymbolDef:
    var status = OkExisting
    if not c.freshSyms.missingOrExcl(it.n.symId):
      status = OkExistingFresh
    result = (it.n.symId, status)
    dest.takeToken it.n
  else:
    let lit = takeIdent(it.n)
    if lit == StrId(0):
      c.buildErr dest, info, "identifier expected"
      result = (SymId(0), ErrNoIdent)
    else:
      result = (identToSym(c, lit, kind), OkNew)
      let s = Sym(kind: kind, name: result[0],
                  pos: dest.len)
      addOverloadable(c.currentScope, lit, s)
      dest.addSymDef(s.name, info)

proc success*(s: SymStatus): bool {.inline.} = s in {OkNew, OkExisting, OkExistingFresh}
proc success*(s: DelayedSym): bool {.inline.} = success s.status

proc markSymInProgress*(c: var SemContext; s: SymId)  # forward decl

proc handleSymDef*(c: var SemContext; dest: var TokenBuf; n: var Cursor; kind: SymKind): DelayedSym =
  let info = n.info
  if n.isIdent:
    let lit = n.litId
    if kind in {LetY, VarY, GletY, GvarY, TletY, TvarY} and
        c.currentScope.kind == ToplevelScope and
        c.onDemandResolved.hasKey(lit):
      # #1974: this toplevel let/var was already resolved on demand in the
      # signature phase (for a `when` condition). Reuse that symbol so the
      # body phase neither redeclares it nor shifts its global-counter name.
      # `getOrDefault` (not `[]`) because nimony rejects the raising `Table.[]`
      # in effect-checked code; presence is already guaranteed by `hasKey` above.
      let def = c.onDemandResolved.getOrDefault(lit, SymId(0))
      let s = Sym(kind: kind, name: def, pos: dest.len)
      result = DelayedSym(status: OkExistingFresh, lit: lit, s: s, info: info)
      dest.addSymDef(def, info)
      inc n
    else:
      let def = identToSym(c, lit, kind)
      let s = Sym(kind: kind, name: def,
                  pos: dest.len)
      result = DelayedSym(status: OkNew, lit: lit, s: s, info: info)
      dest.addSymDef(def, info)
      inc n
  elif n.isSymbolDef:
    discard "ok, and no need to re-add it to the symbol table ... or is there?"
    let status =
      if c.phase == SemcheckBodies and kind in {ParamY, TypevarY, StaticTypevarY}: OkNew
      elif not c.freshSyms.missingOrExcl(n.symId): OkExistingFresh
      else: OkExisting

    let s = Sym(kind: kind, name: n.symId, pos: dest.len)
    result = DelayedSym(status: status, lit: symToIdent(s.name), s: s, info: info)
    dest.takeToken n
    # Mark toplevel declarations as InProgress for cycle detection
    if kind in {TypeY, ProcY, FuncY, IteratorY, ConverterY, MethodY, TemplateY, MacroY}:
      markSymInProgress(c, s.name)
  elif n.isDotToken:
    var name = "`anon"
    c.makeLocalSym(name)
    let symId = pool.syms.getOrIncl(name)
    let s = Sym(kind: kind, name: symId, pos: dest.len)
    result = DelayedSym(status: OkExisting, s: s, info: info)
    dest.addSymDef(symId, info)
    inc n
  else:
    let lit = takeIdent(n)
    if lit == StrId(0):
      c.buildErr dest, info, "identifier expected"
      result = DelayedSym(status: ErrNoIdent, info: info)
    else:
      let def = identToSym(c, lit, kind)
      let s = Sym(kind: kind, name: def,
                  pos: dest.len)
      result = DelayedSym(status: OkNew, lit: lit, s: s, info: info)
      dest.addSymDef(def, info)

proc addSym*(c: var SemContext; dest: var TokenBuf; s: DelayedSym) =
  if s.status == OkNew:
    if addNonOverloadable(c.currentScope, s.lit, s.s) == Conflict:
      c.buildErr dest, s.info, "attempt to redeclare: " & pool.strings[s.lit]

proc addSymForwardError*(c: var SemContext; s: DelayedSym): bool =
  if s.status == OkNew:
    result = addNonOverloadable(c.currentScope, s.lit, s.s) == Conflict
  else:
    result = false

proc markSymInProgress*(c: var SemContext; s: SymId) =
  ## Mark a symbol as being processed (for cycle detection in phase 2/3)
  if s != SymId(0) and prog.mem.hasKey(s):
    if c.phase == SemcheckSignatures:
      prog.mem[s].phase = SemcheckSignaturesInProgress
    elif c.phase == SemcheckBodies:
      prog.mem[s].phase = SemcheckBodiesInProgress

proc publish*(c: var SemContext; dest: var TokenBuf; s: SymId; start: int) =
  assert s != SymId(0)
  var buf = createTokenBuf(dest.len - start + 1)
  for i in start..<dest.len:
    buf.addRaw dest[i]
  programs.publish s, buf, c.phase

# -------------------------------------------------------------------------------------------------

proc wantDot*(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  if n.isDotToken:
    dest.takeToken n
  else:
    buildErr c, dest, n.info, "expected '.'"

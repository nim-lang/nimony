#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Lots of basic helpers for semantic checking.

import std / [tables, sets, syncio, formatfloat, assertions]
include nifprelude
import nimony_model, symtabs, builtintypes, decls, symparser, asthelpers,
  programs, sigmatch, magics, reporters, nifconfig, nifindexes,
  intervals, xints,
  semdata, semos, expreval

import ".." / gear2 / modnames

# -------------- symbol lookups -------------------------------------

template buildTree*(dest: var TokenBuf; kind: StmtKind|ExprKind|TypeKind|SymKind|NimonyOther;
                    info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(cast[TagId](kind), info)
  body
  dest.addParRi()

proc considerImportedSymbols(c: var SemContext; dest: var TokenBuf; name: StrId; info: PackedLineInfo): int =
  result = 0
  for moduleId in c.importTab.getOrDefault(name):
    # prevent copies
    let candidates = addr c.importedModules[moduleId].iface[name]
    inc result, candidates[].len
    for defId in candidates[]:
      dest.add symToken(defId, info)

proc addSymUse*(dest: var TokenBuf; s: Sym; info: PackedLineInfo) =
  dest.add symToken(s.name, info)

proc buildSymChoiceForDot(c: var SemContext; dest: var TokenBuf; identifier: StrId; info: PackedLineInfo) {.used.} =
  # not used yet
  var count = 0
  let oldLen = dest.len
  dest.buildTree OchoiceX, info:
    var it = c.currentScope
    while it != nil:
      for sym in it.tab.getOrDefault(identifier):
        if sym.kind in {ProcY, FuncY, ConverterY, MethodY, TemplateY, MacroY, IteratorY, TypeY}:
          dest.addSymUse sym, info
          inc count
      it = it.up
    inc count, considerImportedSymbols(c, dest, identifier, info)

  # if the sym choice is empty, create an ident node:
  if count == 0:
    dest.shrink oldLen
    dest.add identToken(identifier, info)

proc isNonOverloadable(t: SymKind): bool {.inline.} =
  t in {LetY, VarY, ParamY, TypevarY, ConstY, TypeY, ResultY, FldY, CursorY, BlockY, GletY, TletY, GvarY, TvarY}

proc buildSymChoiceForSelfModule*(c: var SemContext; dest: var TokenBuf;
                                  identifier: StrId; info: PackedLineInfo): int =
  result = 0
  let oldLen = dest.len
  dest.buildTree OchoiceX, info:
    # add symbols from top scope:
    var it = c.currentScope
    while it.up != nil: it = it.up
    for sym in it.tab.getOrDefault(identifier):
      dest.addSymUse sym, info
      inc result
  # if the sym choice is empty, create an ident node:
  if result == 0:
    dest.shrink oldLen
    dest.add identToken(identifier, info)

iterator topLevelSyms*(c: var SemContext; identifier: StrId): SymId =
  var it = c.currentScope
  while it.up != nil: it = it.up
  for sym in it.tab.getOrDefault(identifier):
    yield sym.name

proc rawBuildSymChoiceForForeignModule(c: var SemContext; dest: var TokenBuf; module: SymId;
                                       identifier: StrId; info: PackedLineInfo;
                                       marker: var HashSet[SymId]): int =
  result = 0
  let candidates = c.importedModules[module].iface.getOrDefault(identifier)
  for defId in candidates:
    if not marker.containsOrIncl(defId):
      dest.add symToken(defId, info)
    inc result
  for forward, filter in c.importedModules[module].exports:
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
    dest.add identToken(identifier, info)

type
  ChoiceOption* = enum
    FindAll, FindOverloads, InnerMost

proc rawBuildSymChoice(c: var SemContext; dest: var TokenBuf; identifier: StrId; info: PackedLineInfo;
                       option = FindAll): int =
  result = 0
  var it = c.currentScope
  while it != nil:
    var nonOverloadable = 0
    for sym in it.tab.getOrDefault(identifier):
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
  inc result, considerImportedSymbols(c, dest, identifier, info)

proc buildSymChoice*(c: var SemContext; dest: var TokenBuf; identifier: StrId; info: PackedLineInfo;
                    option: ChoiceOption): int =
  let oldLen = dest.len
  dest.buildTree OchoiceX, info:
    result = rawBuildSymChoice(c, dest, identifier, info, option)
  # if the sym choice is empty, create an ident node:
  if result == 0:
    dest.shrink oldLen
    dest.add identToken(identifier, info)

proc isDeclared*(c: var SemContext; name: StrId): bool =
  var scope = c.currentScope
  while scope != nil:
    if name in scope.tab:
      return true
    scope = scope.up
  result = name in c.importTab

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
  when defined(debug):
    if not c.debugAllowErrors:
      writeStackTrace()
      for instFrom in items(c.instantiatedFrom):
        echo "instantiated from: ", infoToStr(instFrom)

      echo infoToStr(info) & " Error: " & msg
      if orig.kind != DotToken:
        echo "Source: ", toString(orig, false)
      quit 1
  var n = orig
  var hasErr = false
  if n.kind == ParLe:
    if n.tagId == ErrT:
      hasErr = true
    else:
      var nested = 0
      while true:
        inc n
        if n.kind == ParRi:
          if nested == 0: break
          dec nested
        elif n.kind == ParLe:
          if n.tagId == ErrT:
            hasErr = true
            break
          else:
            inc nested
  let info = if hasErr: n.info else: info
  dest.buildTree ErrT, info:
    if hasErr:
      inc n
      dest.takeTree n
    else:
      dest.addSubtree orig
    for instFrom in items(c.instantiatedFrom):
      dest.add dotToken(instFrom)
    if hasErr:
      while n.kind == DotToken: inc n
      dest.takeTree n
    else:
      dest.add strToken(pool.strings.getOrIncl(msg), info)

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
      assert dest[dest.len - 1].kind == ParRi
      dest.shrink(dest.len - 1)
    else:
      dest.insert [parLeToken(StmtsS, dest[pos].info)], pos
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
    dest.add strToken(pool.strings.getOrIncl(msg), info)

proc buildLocalErr*(dest: var TokenBuf; info: PackedLineInfo; msg: string) =
  var orig = createTokenBuf(1)
  orig.addDotToken()
  dest.buildLocalErr info, msg, cursorAt(orig, 0)

# -------------------------- type handling ---------------------------

proc ptrTypeOf*(c: var SemContext; dest: var TokenBuf; typ: TypeCursor): TypeCursor =
  let typeBegin = dest.len
  dest.buildTree PtrT, typ.info:
    dest.addSubtree typ.skipModifier
  result = typeToCursor(c, dest, typeBegin)
  dest.shrink typeBegin

proc declToCursor*(c: var SemContext; dest: var TokenBuf; s: Sym): LoadResult =
  if knowsSym(s.name) or s.pos == ImportedPos:
    result = tryLoadSym(s.name)
  elif s.pos > 0:
    var buf = createTokenBuf(10)
    var pos = s.pos - 1
    var nested = 0
    # XXX optimize this for non-generic procs. No need to
    # copy their bodies here.
    while true:
      buf.add dest[pos]
      case dest[pos].kind
      of ParLe: inc nested
      of ParRi:
        dec nested
        if nested == 0: break
      else: discard
      inc pos
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

proc makeLocalSym*(c: var SemContext; result: var string) =
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
  if c.currentScope.kind == ToplevelScope or
      kind in {FldY, EfldY, TypevarY, ProcY, FuncY, ConverterY, MethodY, TemplateY, MacroY, IteratorY, TypeY}:
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
  if it.n.kind == SymbolDef:
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
        dest.add symdefToken(s.name, info)
        result = OkNew

proc declareOverloadableSym*(c: var SemContext; dest: var TokenBuf; it: var Item; kind: SymKind): (SymId, SymStatus) =
  let info = it.n.info
  if it.n.kind == SymbolDef:
    var status = OkExisting
    if not c.freshSyms.missingOrExcl(it.n.symId):
      status = OkExistingFresh
    result = (it.n.symId, status)
    dest.add it.n
    inc it.n
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
      dest.add symdefToken(s.name, info)

proc success*(s: SymStatus): bool {.inline.} = s in {OkNew, OkExisting, OkExistingFresh}
proc success*(s: DelayedSym): bool {.inline.} = success s.status

proc markSymInProgress*(c: var SemContext; s: SymId)  # forward decl

proc handleSymDef*(c: var SemContext; dest: var TokenBuf; n: var Cursor; kind: SymKind): DelayedSym =
  let info = n.info
  if n.kind == Ident:
    let lit = n.litId
    let def = identToSym(c, lit, kind)
    let s = Sym(kind: kind, name: def,
                pos: dest.len)
    result = DelayedSym(status: OkNew, lit: lit, s: s, info: info)
    dest.add symdefToken(def, info)
    inc n
  elif n.kind == SymbolDef:
    discard "ok, and no need to re-add it to the symbol table ... or is there?"
    let status =
      if c.phase == SemcheckBodies and kind in {ParamY, TypevarY}: OkNew
      elif not c.freshSyms.missingOrExcl(n.symId): OkExistingFresh
      else: OkExisting

    let s = Sym(kind: kind, name: n.symId, pos: dest.len)
    result = DelayedSym(status: status, lit: symToIdent(s.name), s: s, info: info)
    dest.add n
    inc n
    # Mark toplevel declarations as InProgress for cycle detection
    if kind in {TypeY, ProcY, FuncY, IteratorY, ConverterY, MethodY, TemplateY, MacroY}:
      markSymInProgress(c, s.name)
  elif n.kind == DotToken:
    var name = "`anon"
    c.makeLocalSym(name)
    let symId = pool.syms.getOrIncl(name)
    let s = Sym(kind: kind, name: symId, pos: dest.len)
    result = DelayedSym(status: OkExisting, s: s, info: info)
    dest.add symdefToken(symId, info)
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
      dest.add symdefToken(def, info)

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
    buf.add dest[i]
  programs.publish s, buf, c.phase

# -------------------------------------------------------------------------------------------------

proc wantDot*(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  if n.kind == DotToken:
    dest.add n
    inc n
  else:
    buildErr c, dest, n.info, "expected '.'"

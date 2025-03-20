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

proc considerImportedSymbols(c: var SemContext; name: StrId; info: PackedLineInfo): int =
  result = 0
  for moduleId in c.importTab.getOrDefault(name):
    # prevent copies
    let candidates = addr c.importedModules[moduleId].iface[name]
    inc result, candidates[].len
    for defId in candidates[]:
      c.dest.add symToken(defId, info)

proc addSymUse*(dest: var TokenBuf; s: Sym; info: PackedLineInfo) =
  dest.add symToken(s.name, info)

proc buildSymChoiceForDot(c: var SemContext; identifier: StrId; info: PackedLineInfo) {.used.} =
  # not used yet
  var count = 0
  let oldLen = c.dest.len
  c.dest.buildTree OchoiceX, info:
    var it = c.currentScope
    while it != nil:
      for sym in it.tab.getOrDefault(identifier):
        if sym.kind in {ProcY, FuncY, ConverterY, MethodY, TemplateY, MacroY, IteratorY, TypeY}:
          c.dest.addSymUse sym, info
          inc count
      it = it.up
    inc count, considerImportedSymbols(c, identifier, info)

  # if the sym choice is empty, create an ident node:
  if count == 0:
    c.dest.shrink oldLen
    c.dest.add identToken(identifier, info)

proc isNonOverloadable(t: SymKind): bool {.inline.} =
  t in {LetY, VarY, ParamY, TypevarY, ConstY, TypeY, ResultY, FldY, CursorY, BlockY, GletY, TletY, GvarY, TvarY}

proc buildSymChoiceForSelfModule(c: var SemContext;
                                 identifier: StrId; info: PackedLineInfo) {.used.} =
  # not used yet
  var count = 0
  let oldLen = c.dest.len
  c.dest.buildTree OchoiceX, info:
    var it = c.currentScope
    while it.up != nil: it = it.up
    var nonOverloadable = 0
    for sym in it.tab.getOrDefault(identifier):
      # for non-overloadable symbols prefer the innermost symbol:
      if sym.kind.isNonOverloadable:
        inc nonOverloadable
        if nonOverloadable == 1:
          c.dest.addSymUse sym, info
          inc count
      else:
        c.dest.addSymUse sym, info
        inc count
      it = it.up
  # if the sym choice is empty, create an ident node:
  if count == 0:
    c.dest.shrink oldLen
    c.dest.add identToken(identifier, info)

proc buildSymChoiceForForeignModule*(c: var SemContext; importFrom: ImportedModule;
                                     identifier: StrId; info: PackedLineInfo): int =
  result = 0
  let oldLen = c.dest.len
  c.dest.buildTree OchoiceX, info:
    let candidates = importFrom.iface.getOrDefault(identifier)
    for defId in candidates:
      c.dest.add symToken(defId, info)
      inc result
  # if the sym choice is empty, create an ident node:
  if result == 0:
    c.dest.shrink oldLen
    c.dest.add identToken(identifier, info)

type
  ChoiceOption* = enum
    FindAll, FindOverloads, InnerMost

proc rawBuildSymChoice(c: var SemContext; identifier: StrId; info: PackedLineInfo;
                       option = FindAll): int =
  result = 0
  var it = c.currentScope
  while it != nil:
    var nonOverloadable = 0
    for sym in it.tab.getOrDefault(identifier):
      c.dest.addSymUse sym, info
      inc result
      if sym.kind.isNonOverloadable:
        inc nonOverloadable
    if result == 1 and (option == InnerMost or
        (option == FindOverloads and nonOverloadable == 1)):
      # unambiguous local symbol found
      # in case of FindOverloads, if symbol is overloadable, consider other overloads
      return
    it = it.up
  inc result, considerImportedSymbols(c, identifier, info)

proc buildSymChoice*(c: var SemContext; identifier: StrId; info: PackedLineInfo;
                    option: ChoiceOption): int =
  let oldLen = c.dest.len
  c.dest.buildTree OchoiceX, info:
    result = rawBuildSymChoice(c, identifier, info, option)
  # if the sym choice is empty, create an ident node:
  if result == 0:
    c.dest.shrink oldLen
    c.dest.add identToken(identifier, info)

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

proc buildErr*(c: var SemContext; info: PackedLineInfo; msg: string; orig: Cursor) =
  when defined(debug):
    if not c.debugAllowErrors:
      writeStackTrace()
      echo infoToStr(info) & " Error: " & msg
      quit msg
  c.dest.buildTree ErrT, info:
    c.dest.addSubtree orig
    for instFrom in items(c.instantiatedFrom):
      c.dest.add dotToken(instFrom)
    c.dest.add strToken(pool.strings.getOrIncl(msg), info)

proc buildErr*(c: var SemContext; info: PackedLineInfo; msg: string) =
  var orig = createTokenBuf(1)
  orig.addDotToken()
  c.buildErr info, msg, cursorAt(orig, 0)

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

proc ptrTypeOf*(c: var SemContext; typ: TypeCursor): TypeCursor =
  let typeBegin = c.dest.len
  c.dest.buildTree PtrT, typ.info:
    c.dest.addSubtree typ.skipModifier
  result = typeToCursor(c, typeBegin)
  c.dest.shrink typeBegin

proc declToCursor*(c: var SemContext; s: Sym): LoadResult =
  if knowsSym(s.name) or s.pos == ImportedPos:
    result = tryLoadSym(s.name)
  elif s.pos > 0:
    var buf = createTokenBuf(10)
    var pos = s.pos - 1
    var nested = 0
    # XXX optimize this for non-generic procs. No need to
    # copy their bodies here.
    while true:
      buf.add c.dest[pos]
      case c.dest[pos].kind
      of ParLe: inc nested
      of ParRi:
        dec nested
        if nested == 0: break
      else: discard
      inc pos
    result = LoadResult(status: LacksNothing, decl: cursorAt(buf, 0))
    publish s.name, buf
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
  if c.currentScope.kind == ToplevelScope or kind in {FldY, EfldY, TypevarY}:
    c.makeGlobalSym(name)
  else:
    c.makeLocalSym(name)
  result = pool.syms.getOrIncl(name)

proc identToSym*(c: var SemContext; lit: StrId; kind: SymKind): SymId =
  result = identToSym(c, pool.strings[lit], kind)

proc symToIdent*(s: SymId): StrId =
  var name = pool.syms[s]
  extractBasename name
  result = pool.strings.getOrIncl name

proc declareSym*(c: var SemContext; it: var Item; kind: SymKind): SymStatus =
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
      c.buildErr info, "identifier expected"
      result = ErrNoIdent
    else:
      let s = Sym(kind: kind, name: identToSym(c, lit, kind),
                  pos: c.dest.len)
      if addNonOverloadable(c.currentScope, lit, s) == Conflict:
        c.buildErr info, "attempt to redeclare: " & pool.strings[lit]
        result = ErrRedef
      else:
        c.dest.add symdefToken(s.name, info)
        result = OkNew

proc declareOverloadableSym*(c: var SemContext; it: var Item; kind: SymKind): (SymId, SymStatus) =
  let info = it.n.info
  if it.n.kind == SymbolDef:
    var status = OkExisting
    if not c.freshSyms.missingOrExcl(it.n.symId):
      status = OkExistingFresh
    result = (it.n.symId, status)
    c.dest.add it.n
    inc it.n
  else:
    let lit = takeIdent(it.n)
    if lit == StrId(0):
      c.buildErr info, "identifier expected"
      result = (SymId(0), ErrNoIdent)
    else:
      result = (identToSym(c, lit, kind), OkNew)
      let s = Sym(kind: kind, name: result[0],
                  pos: c.dest.len)
      addOverloadable(c.currentScope, lit, s)
      c.dest.add symdefToken(s.name, info)

proc success*(s: SymStatus): bool {.inline.} = s in {OkNew, OkExisting, OkExistingFresh}
proc success*(s: DelayedSym): bool {.inline.} = success s.status

proc handleSymDef*(c: var SemContext; n: var Cursor; kind: SymKind): DelayedSym =
  let info = n.info
  if n.kind == Ident:
    let lit = n.litId
    let def = identToSym(c, lit, kind)
    let s = Sym(kind: kind, name: def,
                pos: c.dest.len)
    result = DelayedSym(status: OkNew, lit: lit, s: s, info: info)
    c.dest.add symdefToken(def, info)
    inc n
  elif n.kind == SymbolDef:
    discard "ok, and no need to re-add it to the symbol table ... or is there?"
    let status =
      if c.phase == SemcheckBodies and kind in {ParamY, TypevarY}: OkNew
      elif not c.freshSyms.missingOrExcl(n.symId): OkExistingFresh
      else: OkExisting

    let s = Sym(kind: kind, name: n.symId, pos: c.dest.len)
    result = DelayedSym(status: status, lit: symToIdent(s.name), s: s, info: info)
    c.dest.add n
    inc n
  elif n.kind == DotToken:
    var name = "`anon"
    c.makeLocalSym(name)
    let symId = pool.syms.getOrIncl(name)
    let s = Sym(kind: kind, name: symId, pos: c.dest.len)
    result = DelayedSym(status: OkExisting, s: s, info: info)
    c.dest.add symdefToken(symId, info)
    inc n
  else:
    let lit = takeIdent(n)
    if lit == StrId(0):
      c.buildErr info, "identifier expected"
      result = DelayedSym(status: ErrNoIdent, info: info)
    else:
      let def = identToSym(c, lit, kind)
      let s = Sym(kind: kind, name: def,
                  pos: c.dest.len)
      result = DelayedSym(status: OkNew, lit: lit, s: s, info: info)
      c.dest.add symdefToken(def, info)

proc addSym*(c: var SemContext; s: DelayedSym) =
  if s.status == OkNew:
    if addNonOverloadable(c.currentScope, s.lit, s.s) == Conflict:
      c.buildErr s.info, "attempt to redeclare: " & pool.strings[s.lit]

proc publish*(c: var SemContext; s: SymId; start: int) =
  assert s != SymId(0)
  var buf = createTokenBuf(c.dest.len - start + 1)
  for i in start..<c.dest.len:
    buf.add c.dest[i]
  programs.publish s, buf

proc publishSignature*(c: var SemContext; s: SymId; start: int) =
  var buf = createTokenBuf(c.dest.len - start + 3)
  for i in start..<c.dest.len:
    buf.add c.dest[i]
  buf.addDotToken() # body is empty for a signature
  buf.addParRi()
  programs.publish s, buf

# -------------------------------------------------------------------------------------------------

proc takeTree*(c: var SemContext; n: var Cursor) =
  takeTree c.dest, n

# -------------------------------------------------------------

proc takeParRi*(c: var SemContext; n: var Cursor) =
  if n.kind == ParRi:
    c.dest.add n
    inc n
  else:
    error "expected ')', but got: ", n

proc takeToken*(c: var SemContext; n: var Cursor) {.inline.} =
  c.dest.add n
  inc n

proc wantDot*(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    c.dest.add n
    inc n
  else:
    buildErr c, n.info, "expected '.'"

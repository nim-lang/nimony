#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Lots of basic helpers for semantic checking.

import std / [tables, sets, syncio, formatfloat, assertions]
include nifprelude
import nimony_model, symtabs, builtintypes, decls, symparser,
  programs, sigmatch, magics, reporters, nifconfig, nifindexes,
  intervals, xints,
  semdata, semos, expreval

import ".." / gear2 / modnames

# -------------- symbol lookups -------------------------------------

proc unquote*(c: var Cursor): StrId =
  var r = ""
  while true:
    case c.kind
    of ParLe:
      inc c
    of ParRi:
      inc c
      break
    of EofToken:
      r.add "<unexpected eof>"
      break
    of Ident, StringLit:
      r.add pool.strings[c.litId]
      inc c
    of IntLit:
      r.addInt pool.integers[c.intId]
      inc c
    of CharLit:
      let ch = char(c.uoperand)
      r.add ch
      inc c
    of UIntLit:
      r.add $pool.uintegers[c.uintId]
      inc c
    of FloatLit:
      r.addFloat pool.floats[c.floatId]
      inc c
    of UnknownToken, DotToken, Symbol, SymbolDef:
      r.add "<unexpected token>: " & $c.kind
      inc c
  assert r.len > 0
  result = getOrIncl(pool.strings, r)

proc getIdent*(n: var Cursor): StrId =
  var nested = 0
  while exprKind(n) in {OchoiceX, CchoiceX}:
    inc nested
    inc n
  case n.kind
  of Ident:
    result = n.litId
    inc n
  of Symbol, SymbolDef:
    let sym = pool.syms[n.symId]
    var isGlobal = false
    result = pool.strings.getOrIncl(extractBasename(sym, isGlobal))
    inc n
  of ParLe:
    if exprKind(n) == QuotedX:
      result = unquote(n)
    else:
      result = StrId(0)
  else:
    result = StrId(0)
  while nested > 0:
    if n.kind == ParRi: dec nested
    inc n

template buildTree*(dest: var TokenBuf; kind: StmtKind|ExprKind|TypeKind|SymKind;
                    info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(pool.tags.getOrIncl($kind), info)
  body
  dest.addParRi()

proc considerImportedSymbols(c: var SemContext; name: StrId; info: PackedLineInfo): int =
  result = 0
  let candidates = c.importTab.getOrDefault(name)
  inc result, candidates.len
  for defId in candidates:
    c.dest.add symToken(defId, info)

proc addSymUse*(dest: var TokenBuf; s: Sym; info: PackedLineInfo) =
  dest.add symToken(s.name, info)

proc addSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) =
  dest.add symToken(s, info)

proc buildSymChoiceForDot(c: var SemContext; identifier: StrId; info: PackedLineInfo) =
  var count = 0
  let oldLen = c.dest.len
  c.dest.buildTree OchoiceX, info:
    var it = c.currentScope
    while it != nil:
      for sym in it.tab.getOrDefault(identifier):
        if sym.kind in {ProcY, FuncY, ConverterY, MethodY, TemplateY, MacroY, IterY, TypeY}:
          c.dest.addSymUse sym, info
          inc count
      it = it.up
    inc count, considerImportedSymbols(c, identifier, info)

  # if the sym choice is empty, create an ident node:
  if count == 0:
    c.dest.shrink oldLen
    c.dest.add identToken(identifier, info)

proc isNonOverloadable(t: SymKind): bool {.inline.} =
  t in {LetY, VarY, ParamY, TypevarY, ConstY, TypeY, ResultY, FldY, CursorY, LabelY}

proc buildSymChoiceForSelfModule(c: var SemContext;
                                 identifier: StrId; info: PackedLineInfo) =
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

proc buildSymChoiceForForeignModule(c: var SemContext; importFrom: ImportedModule;
                                    identifier: StrId; info: PackedLineInfo) =
  var count = 0
  let oldLen = c.dest.len
  c.dest.buildTree OchoiceX, info:
    let candidates = importFrom.iface.getOrDefault(identifier)
    for defId in candidates:
      c.dest.add symToken(defId, info)
      inc count
  # if the sym choice is empty, create an ident node:
  if count == 0:
    c.dest.shrink oldLen
    c.dest.add identToken(identifier, info)

type
  ChoiceOption* = enum
    FindAll, InnerMost

proc rawBuildSymChoice(c: var SemContext; identifier: StrId; info: PackedLineInfo;
                       option = FindAll): int =
  result = 0
  var it = c.currentScope
  var nonOverloadable = 0
  while it != nil:
    for sym in it.tab.getOrDefault(identifier):
      # for non-overloadable symbols prefer the innermost symbol:
      if sym.kind.isNonOverloadable:
        if nonOverloadable == 0:
          c.dest.addSymUse sym, info
          inc result
        inc nonOverloadable
        if result == 1 and nonOverloadable == 1 and option == InnerMost:
          return
      else:
        c.dest.addSymUse sym, info
        inc result
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

proc typeToCanon*(buf: TokenBuf; start: int): string =
  result = ""
  for i in start..<buf.len:
    case buf[i].kind
    of ParLe:
      result.add '('
      result.addInt buf[i].tagId.int
    of ParRi: result.add ')'
    of Ident, StringLit:
      result.add ' '
      result.addInt buf[i].litId.int
    of UnknownToken: result.add " unknown"
    of EofToken: result.add " eof"
    of DotToken: result.add '.'
    of Symbol, SymbolDef:
      result.add " s"
      result.addInt buf[i].symId.int
    of CharLit:
      result.add " c"
      result.addInt buf[i].uoperand.int
    of IntLit:
      result.add " i"
      result.addInt buf[i].intId.int
    of UIntLit:
      result.add " u"
      result.addInt buf[i].uintId.int
    of FloatLit:
      result.add " f"
      result.addInt buf[i].floatId.int

proc sameTrees*(a, b: TypeCursor): bool =
  var a = a
  var b = b
  var nested = 0
  let isAtom = a.kind != ParLe
  while true:
    if a.kind != b.kind: return false
    case a.kind
    of ParLe:
      if a.tagId != b.tagId: return false
      inc nested
    of ParRi:
      dec nested
      if nested == 0: return true
    of Symbol, SymbolDef:
      if a.symId != b.symId: return false
    of IntLit:
      if a.intId != b.intId: return false
    of UIntLit:
      if a.uintId != b.uintId: return false
    of FloatLit:
      if a.floatId != b.floatId: return false
    of StringLit, Ident:
      if a.litId != b.litId: return false
    of CharLit, UnknownToken:
      if a.uoperand != b.uoperand: return false
    of DotToken, EofToken: discard "nothing else to compare"
    if isAtom: return true
    inc a
    inc b
  return false

proc typeToCursor*(c: var SemContext; buf: TokenBuf; start: int): TypeCursor =
  let key = typeToCanon(buf, start)
  if c.typeMem.hasKey(key):
    result = cursorAt(c.typeMem[key], 0)
  else:
    var newBuf = createTokenBuf(buf.len - start)
    for i in start..<buf.len:
      newBuf.add buf[i]
    # make resilient against crashes:
    #if newBuf.len == 0: newBuf.add dotToken(NoLineInfo)
    result = cursorAt(newBuf, 0)
    c.typeMem[key] = newBuf

proc typeToCursor*(c: var SemContext; start: int): TypeCursor =
  typeToCursor(c, c.dest, start)

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
    ErrNoIdent, ErrRedef, OkNew, OkExisting

  DelayedSym* = object
    status*: SymStatus
    lit*: StrId
    s*: Sym
    info*: PackedLineInfo

proc identToSym*(c: var SemContext; lit: StrId; kind: SymKind): SymId =
  var name = pool.strings[lit]
  if c.currentScope.kind == ToplevelScope or kind in {FldY, EfldY}:
    c.makeGlobalSym(name)
  else:
    c.makeLocalSym(name)
  result = pool.syms.getOrIncl(name)

proc symToIdent*(s: SymId): StrId =
  var name = pool.syms[s]
  extractBasename name
  result = pool.strings.getOrIncl name

proc declareSym*(c: var SemContext; it: var Item; kind: SymKind): SymStatus =
  let info = it.n.info
  if it.n.kind == Ident:
    let lit = it.n.litId
    let s = Sym(kind: kind, name: identToSym(c, lit, kind),
                pos: c.dest.len)
    if addNonOverloadable(c.currentScope, lit, s) == Conflict:
      c.buildErr info, "attempt to redeclare: " & pool.strings[lit]
      result = ErrRedef
    else:
      c.dest.add symdefToken(s.name, info)
      result = Oknew
    inc it.n
  elif it.n.kind == SymbolDef:
    inc it.n
    result = OkExisting
  else:
    c.buildErr info, "identifier expected"
    result = ErrNoIdent

proc declareOverloadableSym*(c: var SemContext; it: var Item; kind: SymKind): SymId =
  let info = it.n.info
  if it.n.kind == Ident:
    let lit = it.n.litId
    result = identToSym(c, lit, kind)
    let s = Sym(kind: kind, name: result,
                pos: c.dest.len)
    addOverloadable(c.currentScope, lit, s)
    c.dest.add symdefToken(s.name, info)
    inc it.n
  elif it.n.kind == SymbolDef:
    result = it.n.symId
    c.dest.add it.n
    inc it.n
  else:
    let lit = getIdent(it.n)
    if lit == StrId(0):
      c.buildErr info, "identifier expected"
      result = SymId(0)
    else:
      result = identToSym(c, lit, kind)
      let s = Sym(kind: kind, name: result,
                  pos: c.dest.len)
      addOverloadable(c.currentScope, lit, s)
      c.dest.add symdefToken(s.name, info)

proc success*(s: SymStatus): bool {.inline.} = s in {OkNew, OkExisting}
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
    let lit = getIdent(n)
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

proc takeTree*(dest: var TokenBuf; n: var Cursor) =
  if n.kind != ParLe:
    dest.add n
    inc n
  else:
    var nested = 0
    while true:
      dest.add n
      case n.kind
      of ParLe: inc nested
      of ParRi:
        dec nested
        if nested == 0:
          inc n
          break
      of EofToken:
        error "expected ')', but EOF reached"
      else: discard
      inc n

proc takeTree*(c: var SemContext; n: var Cursor) =
  takeTree c.dest, n

proc copyTree*(dest: var TokenBuf; n: Cursor) =
  var n = n
  takeTree dest, n

# -------------------------------------------------------------

proc wantParRi*(c: var SemContext; n: var Cursor) =
  if n.kind == ParRi:
    c.dest.add n
    inc n
  else:
    error "expected ')', but got: ", n

proc skipParRi*(n: var Cursor) =
  if n.kind == ParRi:
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

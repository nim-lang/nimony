#
#
#        Lengc set-of-modules handling — nifcore port
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## NIF set-of-modules handling on the **nifcore** stack — the nifcore port of
## `lengc/nifmodules.nim`. Loads foreign declarations lazily, IC-style: a
## module's `.nif` file is opened once, its embedded index (`(.index (x sym
## offset) …)`) read, and each requested declaration materialized on demand by
## seeking the reader to the indexed offset and parsing exactly one subtree.
##
## Compared to the nifcursors original this is simpler: `nifcoreparse.parse`
## replaces the hand-rolled recursive token copier, and `nifstreams` drops out
## entirely — `nifreader.Reader` already provides `jumpTo`/`offset`, and the
## embedded index is read at the raw-token level with no pool involvement.

import std / [assertions, tables]
import ".." / "lib" / nifcoreparse        # re-exports nifcore + parse
import ".." / "lib" / nifcdecl              # stmtKind/symKind/pragmaKind, decls
import ".." / "lib" / nifreader as rd       # Reader, jumpTo, indexStartsAt
import ".." / "lib" / symparser             # splitSymName, splitModulePath, basename
import ".." / "lib" / foreignmodules         # shared lazy loader (ForeignModule)
import noptions                              # ConfigRef

type
  Definition* = object
    pos*: Cursor            ## points into the owning ForeignModule's decl buffer
    kind*: LengSym
    extern*: StrId          ## importc/exportc name, cached (frequently queried)
    isImport*: bool         ## true for importc/importcpp, false for exportc-only

  NifProgram = object
    mods: Table[string, ForeignModule]   ## module suffix -> lazily-opened module
    scheme: SplittedModulePath

  TypeScope* {.acyclic.} = ref object
    locals*: Table[SymId, Cursor]
    parent*: TypeScope

  MainModule* = object
    src*: TokenBuf
    pool*: Pool
    tags*: TagPool
    types*: seq[Cursor]                  ## points into MainModule.src
    filename*: string
    config*: ConfigRef
    mem*: seq[TokenBuf]                  ## intermediate results (computed types)
    builtinTypes*: Table[string, Cursor]
    current*: TypeScope
    defs: Table[SymId, Definition]
    typeBodyToDecl: Table[int, Cursor]   ## body `toUniqueId` -> its `(type …)` decl
    prog: NifProgram
    requestedForeignSyms*: seq[Cursor]

proc loadForeign(c: var MainModule; s: SplittedSymName): Cursor =
  ## Resolve a foreign symbol's declaration through the shared `ForeignModule`
  ## lazy loader: open (and cache) the owning module, then jump to the symbol's
  ## indexed offset and parse just that one decl. The cursor stays valid because
  ## the `ForeignModule` owns the per-decl buffer.
  if s.module == "":
    raiseAssert "Cannot lookup declaration without module name: " & s.name
  var m: ForeignModule
  if c.prog.mods.hasKey(s.module):
    m = c.prog.mods[s.module]
  else:
    c.prog.scheme.name = s.module
    m = openForeignModule($c.prog.scheme)
    c.prog.mods[s.module] = m
  let key = $s
  if not hasDecl(m, key):
    raiseAssert "Symbol not found in NIF module: " & key
  result = getDecl(m, key, c.tags, c.pool)   # share the main module's pool (SymId-keyed)

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc externName*(s: SymId; n: Cursor): StrId =
  ## Extract the importc/exportc name from a pragma node `n` (or fall back to the
  ## symbol's basename). Pool-free: uses the cursor's own buffer pool.
  let nn = firstChild(n)
  let p = n.pool
  if nn.kind == StrLit:
    result = p.strings.getOrIncl(strVal(nn, p))
  else:
    var base = p.syms[s]
    extractBasename base
    result = p.strings.getOrIncl(base)

proc extractExtern(c: var MainModule; n: var Cursor; pragmasAt: int;
                   isImport: var bool): StrId =
  result = StrId(0)
  isImport = false
  n.into:  # enter the toplevel (type/proc/var/…)
    if n.kind != SymbolDef:
      raiseAssert "Expected SymbolDef after toplevel declaration"
    let symId = n.symId
    inc n
    for i in 1 ..< pragmasAt: skip n
    if n.substructureKind == PragmasU:
      n.into:
        while n.hasMore:
          let pk = n.pragmaKind
          if pk in {ImportcP, ImportcppP, ExportcP}:
            result = externName(symId, n)
            if pk in {ImportcP, ImportcppP}:
              isImport = true
          skip n
    elif n.kind == DotToken:
      discard "ok"
    else:
      raiseAssert "pragmas not at the correct position"
    while n.hasMore:
      skip n

proc registerTypeBody(c: var MainModule; declPos: Cursor) =
  ## Map a `(type …)` decl's body position to the decl, so `tracebackTypeC` can
  ## recover the decl from a body cursor without walking the buffer backwards.
  c.typeBodyToDecl[asTypeDecl(declPos).body.toUniqueId()] = declPos

proc tracebackTypeC*(c: var MainModule; n: Cursor): Cursor =
  ## The nifcore replacement for the nifcursors backward walk: given a type
  ## *body* cursor, return its enclosing `(type …)` declaration. Returns
  ## `default(Cursor)` for an unregistered body (e.g. an anonymous inline type).
  c.typeBodyToDecl.getOrDefault(n.toUniqueId(), default(Cursor))

proc getDeclOrNil*(c: var MainModule; s: SymId): ptr Definition =
  if not c.defs.hasKey(s):
    let splitted = splitSymName(c.pool.syms[s])
    if splitted.module == "": return nil
    let pos = loadForeign(c, splitted)
    if firstChild(pos).kind == SymbolDef:
      let sk = pos.symKind
      var extern = StrId(0)
      var isImport = false
      var n = pos
      case sk
      of TypeY:
        c.types.add pos
        registerTypeBody(c, pos)
        extern = extractExtern(c, n, 1, isImport)
      of ProcY:
        extern = extractExtern(c, n, 3, isImport)
      of VarY, ConstY, GvarY, TvarY:
        extern = extractExtern(c, n, 1, isImport)
      else: discard
      c.defs[s] = Definition(pos: pos, kind: sk, extern: extern,
                             isImport: isImport)
      c.requestedForeignSyms.add pos
    else:
      raiseAssert "Expected SymbolDef after toplevel declaration"
  result = addr c.defs[s]

proc getExtern*(c: var MainModule; s: SymId): StrId =
  let d = c.getDeclOrNil(s)
  result = if d != nil: d.extern else: StrId(0)

# ---- scopes ---------------------------------------------------------------

proc registerLocal*(c: var MainModule; s: SymId; typ: Cursor) =
  c.current.locals[s] = typ

proc openScope*(c: var MainModule) =
  c.current = TypeScope(locals: initTable[SymId, Cursor](), parent: c.current)

proc closeScope*(c: var MainModule) =
  c.current = c.current.parent

# ---- module loading -------------------------------------------------------

proc processToplevelDecl(c: var MainModule; n: var Cursor; kind: LengSym;
                         pragmasAt: int) =
  let decl = n
  let s = firstChild(decl).symId
  var isImport = false
  let extern = extractExtern(c, n, pragmasAt, isImport)
  c.defs[s] = Definition(pos: decl, kind: kind, extern: extern, isImport: isImport)

proc detectToplevelDecls(c: var MainModule) =
  var n = cursorAt(c.src, 0)
  if n.kind != TagLit: return
  # the src buffer starts with a (stmts …) wrapper; walk its children.
  n.into:
    while n.hasMore:
      if n.kind == TagLit:
        case n.stmtKind
        of TypeS:
          c.types.add n
          registerTypeBody(c, n)
          processToplevelDecl(c, n, TypeY, 1)
        of ProcS:
          processToplevelDecl(c, n, ProcY, 3)
        else:
          case n.symKind
          of VarY, ConstY, GvarY, TvarY:
            processToplevelDecl(c, n, n.symKind, 1)
          else:
            skip n
      else:
        inc n

proc densify(dest: var TokenBuf; n: var Cursor; cur: var NifLineInfo) =
  ## Copy the tree at `n` into `dest`, stamping *every* head token with its
  ## effective line info. nifcore stores line info sparsely (only when it
  ## changes); the nifcursors world propagated it to every node, and the
  ## backends rely on `info(n)` being valid at each statement/expression (e.g.
  ## LLVM `!dbg` / C `#line`). Densifying once at load restores that invariant
  ## for all backends with no per-call-site changes. `cur` is the running
  ## sequential (depth-first) line info, matching how nifcursors propagated it —
  ## a node inherits the most recent info *in stream order*, not its parent's.
  let raw = rawLineInfo(n)
  if raw.isValid: cur = raw
  let eff = cur
  case n.kind
  of TagLit:
    let tag = n.cursorTagId
    dest.openTag tag
    if eff.isValid: dest.appendLineInfo eff
    n.into:
      while n.hasMore: densify(dest, n, cur)
    dest.closeTag()
  of DotToken:
    dest.addDotToken();          (if eff.isValid: dest.appendLineInfo eff); inc n
  of Ident:
    dest.addIdent strVal(n);     (if eff.isValid: dest.appendLineInfo eff); inc n
  of Symbol:
    dest.addSymUse symName(n);   (if eff.isValid: dest.appendLineInfo eff); inc n
  of SymbolDef:
    dest.addSymDef symName(n);   (if eff.isValid: dest.appendLineInfo eff); inc n
  of StrLit:
    dest.addStrLit strVal(n);    (if eff.isValid: dest.appendLineInfo eff); inc n
  of CharLit:
    dest.addCharLit charLit(n);  (if eff.isValid: dest.appendLineInfo eff); inc n
  of IntLit:
    dest.addIntLit intVal(n);    (if eff.isValid: dest.appendLineInfo eff); inc n
  of UIntLit:
    dest.addUIntLit uintVal(n);  (if eff.isValid: dest.appendLineInfo eff); inc n
  of FloatLit:
    dest.addFloatLit floatVal(n);(if eff.isValid: dest.appendLineInfo eff); inc n
  of ExtendedSuffix, LineInfoLit, UnknownToken, EofToken, ParLe, ParRi:
    inc n  # absorbed into the head token's own value/info; never freestanding

proc load*(filename: string): MainModule =
  var r = rd.open(filename)
  case rd.processDirectives(r)
  of rd.Success: discard
  of rd.WrongHeader: quit "nif files must start with Version directive"
  of rd.WrongMeta: quit "the format of meta information is wrong!"
  let nodeCount = rd.fileSize(r) div 7
  # Parse with a canonical Leng tag pool so interned TagIds equal the master
  # ordinals that `stmtKind`/`typeKind`/`symKind` decode against (otherwise a
  # fresh pool assigns IDs in encounter order and every kind reads as None).
  var raw = createTokenBuf(nodeCount, nil, createLengTagPool())
  nifcoreparse.parse(r, raw)
  rd.close(r)
  # Densify line info so `info(n)` is valid at every node (see `densify`). The
  # densified buffer MUST share `raw`'s pool/tags: line info carries a `FileId`
  # interned in that pool, so copying it across pools would dangle.
  result = MainModule(src: createTokenBuf(nodeCount, raw.pool, raw.tags),
                      current: TypeScope(locals: initTable[SymId, Cursor]()),
                      filename: filename,
                      prog: NifProgram(scheme: splitModulePath(filename)))
  var rc = beginRead(raw)
  var curInfo = NoNifLineInfo
  densify(result.src, rc, curInfo)
  result.pool = result.src.pool
  result.tags = result.src.tags
  detectToplevelDecls(result)

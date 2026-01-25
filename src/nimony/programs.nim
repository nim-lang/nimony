#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [syncio, os, tables, sequtils, times, sets]
include ".." / lib / nifprelude
import ".." / lib / [nifindexes, symparser]
import ".." / gear2 / modnames
import reporters, builtintypes
import ".." / models / [nifindex_tags]

type
  Iface* = OrderedTable[StrId, seq[SymId]] # eg. "foo" -> @["foo.1.mod", "foo.3.mod"]

  NifModule = ref object
    stream: Stream
    index: NifIndex

  SemPhase* = enum
    SemcheckTopLevelSyms,
    SemcheckSignaturesInProgress,  ## currently processing signature (cycle detection)
    SemcheckSignatures,
    SemcheckBodiesInProgress,      ## currently processing body (cycle detection)
    SemcheckBodies

  ToplevelEntry* = object
    buffer*: TokenBuf  # semchecked result (for lookups)
    phase*: SemPhase

  ToplevelEntries* = object
    ## Stores toplevel entries with both ordered access and SymId-based lookup.
    ## Supports entries without SymIds (e.g., when statements, imports).
    entries: seq[ToplevelEntry]
    bySymId: Table[SymId, int]  # maps SymId to index in entries

  Program* = object
    mods: Table[string, NifModule]
    main*: SplittedModulePath
    mem*: ToplevelEntries

  ImportFilterKind* = enum
    ImportAll, FromImport, ImportExcept

  ImportFilter* = object
    kind*: ImportFilterKind
    list*: HashSet[StrId] # `from import` or `import except` symbol list

var
  prog*: Program

# -------------- ToplevelEntries methods --------------

proc len*(t: ToplevelEntries): int {.inline.} = t.entries.len

proc hasKey*(t: ToplevelEntries; s: SymId): bool {.inline.} =
  t.bySymId.hasKey(s)

proc `[]`*(t: ToplevelEntries; s: SymId): lent ToplevelEntry {.inline.} =
  t.entries[t.bySymId[s]]

proc `[]`*(t: var ToplevelEntries; s: SymId): var ToplevelEntry {.inline.} =
  t.entries[t.bySymId[s]]

proc `[]=`*(t: var ToplevelEntries; s: SymId; entry: sink ToplevelEntry) =
  ## Add or update an entry with a SymId key.
  if t.bySymId.hasKey(s):
    t.entries[t.bySymId[s]] = entry
  else:
    let idx = t.entries.len
    t.entries.add entry
    t.bySymId[s] = idx

proc add*(t: var ToplevelEntries; entry: sink ToplevelEntry) =
  ## Add an entry without a SymId (e.g., when statement, import).
  t.entries.add entry

proc del*(t: var ToplevelEntries; s: SymId) =
  ## Remove an entry by SymId. The entry is cleared but not removed from the seq.
  if t.bySymId.hasKey(s):
    let idx = t.bySymId[s]
    t.entries[idx].buffer = default(TokenBuf)  # clear the buffer
    t.bySymId.del(s)

iterator items*(t: ToplevelEntries): lent ToplevelEntry =
  for e in t.entries:
    yield e

iterator mitems*(t: var ToplevelEntries): var ToplevelEntry =
  for e in t.entries.mitems:
    yield e

iterator pairs*(t: ToplevelEntries): (int, lent ToplevelEntry) =
  for i, e in t.entries.pairs:
    yield (i, e)

# -------------- end ToplevelEntries methods --------------

proc newNifModule(infile: string): NifModule =
  result = NifModule(stream: nifstreams.open(infile))
  discard processDirectives(result.stream.r)

proc loadModuleContent*(infile: string; owningBuf: var TokenBuf; paths: openArray[string]): Cursor =
  ## Load a module's content into owningBuf and return a cursor to it.
  ## Also registers the module in prog.mods.
  let m = newNifModule(infile)
  owningBuf = fromStream(m.stream)
  result = beginRead(owningBuf)
  let suffix = moduleSuffix(infile, paths)
  prog.mods[suffix] = m

proc suffixToNif*(suffix: string): string {.inline.} =
  # always imported from semchecked files
  prog.main.dir / suffix & ".s.nif"

proc customToNif*(suffix: string): string {.inline.} =
  prog.main.dir / suffix & ".nif"

proc needsRecompile*(dep, output: string): bool =
  result = not fileExists(output) or getLastModificationTime(output) < getLastModificationTime(dep)

proc load(suffix: string): NifModule =
  if not prog.mods.hasKey(suffix):
    let infile = suffixToNif suffix
    result = newNifModule(infile)
    let indexName = infile.changeModuleExt".s.idx.nif"
    #if not fileExists(indexName) or getLastModificationTime(indexName) < getLastModificationTime(infile):
    #  createIndex infile
    result.index = readIndex(indexName)
    prog.mods[suffix] = result
  else:
    result = prog.mods[suffix]

proc mergeFilter*(f: var ImportFilter; g: ImportFilter) =
  # applies filter f to filter g, commutative since it computes the intersection
  case g.kind
  of ImportAll: discard
  of ImportExcept:
    case f.kind
    of ImportAll: f = g
    of ImportExcept:
      f.list.incl(g.list)
    of FromImport:
      f.list.excl(g.list)
  of FromImport:
    case f.kind
    of ImportAll: f = g
    of ImportExcept:
      let exc = f.list
      f = g
      f.list.excl(exc)
    of FromImport:
      f.list = intersection(f.list, g.list)

proc filterAllows*(f: ImportFilter; name: StrId): bool =
  case f.kind
  of ImportAll: result = true
  of ImportExcept: result = name notin f.list
  of FromImport: result = name in f.list

proc loadInterface*(suffix: string; iface: var Iface;
                    module: SymId; importTab: var OrderedTable[StrId, seq[SymId]];
                    converters, methods: var Table[SymId, seq[SymId]];
                    exports: var seq[(string, ImportFilter)];
                    filter: ImportFilter) =
  let m = load(suffix)
  let alreadyLoaded = iface.len != 0
  var marker = filter.list
  let negateMarker = filter.kind == FromImport
  for k, _ in m.index.public:
    var base = k
    extractBasename(base)
    let strId = pool.strings.getOrIncl(base)
    let symId = pool.syms.getOrIncl(k)
    if not alreadyLoaded:
      iface.mgetOrPut(strId, @[]).add symId
    let symMarked =
      if negateMarker: marker.missingOrExcl(strId)
      else: marker.containsOrIncl(strId)
    if not symMarked:
      # mark that this module contains the identifier `strId`:
      importTab.mgetOrPut(strId, @[]).addUnique(module)
  for k, v in m.index.converters.items:
    var name = v
    extractBasename(name)
    let nameId = pool.strings.getOrIncl(name)
    # check that the converter is imported, slow but better to be slow here:
    if nameId in importTab and module in importTab[nameId]:
      let key = if k == ".": SymId(0) else: pool.syms.getOrIncl(k)
      let val = pool.syms.getOrIncl(v)
      converters.mgetOrPut(key, @[]).addUnique(val)
  for class in m.index.classes:
    for mt in class.methods:
      methods.mgetOrPut(class.cls, @[]).addUnique(mt.fn)
  for ex in m.index.exports:
    let (path, kind, names) = ex
    let filterKind =
      case kind
      of ExportIdx: ImportAll
      of FromexportIdx: FromImport
      of ExportexceptIdx: ImportExcept
      else: ImportAll
    var exportFilter = ImportFilter(kind: filterKind)
    for s in names:
      exportFilter.list.incl(s)
    mergeFilter(exportFilter, filter)
    exports.add (path, ensureMove exportFilter)

template reportImpl(msg: string; c: Cursor; level: string) =
  when defined(debug):
    writeStackTrace()
  write stdout, level
  if isValid(c.info):
    write stdout, infoToStr(c.info)
    write stdout, " "
  write stdout, msg
  writeLine stdout, toString(c, false)
  quit 1

template reportImpl(msg: string; level: string) =
  when defined(debug):
    writeStackTrace()
  write stdout, level
  writeLine stdout, msg
  quit 1

proc error*(msg: string; c: Cursor) {.noreturn.} =
  reportImpl(msg, c, "[Error] ")

proc error*(msg: string) {.noreturn.} =
  reportImpl(msg, "[Error] ")

proc bug*(msg: string; c: Cursor) {.noreturn.} =
  writeStackTrace()
  reportImpl(msg, c, "[Bug] ")

proc bug*(msg: string) {.noreturn.} =
  writeStackTrace()
  reportImpl(msg, "[Bug] ")

type
  LoadStatus* = enum
    LacksModuleName, LacksOffset, LacksPosition, LacksNothing
  LoadResult* = object
    status*: LoadStatus
    decl*: Cursor

proc tryLoadSym*(s: SymId): LoadResult =
  if prog.mem.hasKey(s):
    result = LoadResult(status: LacksNothing, decl: cursorAt(prog.mem[s].buffer, 0))
  else:
    let nifName = pool.syms[s]
    let modname = extractModule(nifName)
    if modname == "":
      result = LoadResult(status: LacksModuleName)
    else:
      var m = load(modname)
      var indexEntry = m.index.public.getOrDefault(nifName)
      if indexEntry.offset == 0:
        indexEntry = m.index.private.getOrDefault(nifName)
      if indexEntry.offset == 0:
        result = LoadResult(status: LacksOffset)
      else:
        m.stream.r.jumpTo indexEntry.offset
        var buf = createTokenBuf(30)
        nifcursors.parse(m.stream, buf, indexEntry.info)
        let decl = cursorAt(buf, 0)
        prog.mem[s] = ToplevelEntry(buffer: ensureMove(buf), phase: SemcheckBodies)
        result = LoadResult(status: LacksNothing, decl: decl)

proc tryLoadHook*(op: AttachedOp; typ: SymId; wantGeneric: bool): SymId =
  let nifName = pool.syms[typ]
  let modname = extractModule(nifName)
  result = SymId(0)
  if modname != "":
    var m = load(modname)
    if m.index.hooks.hasKey(typ):
      let res = m.index.hooks[typ].a[op]
      if res[1] == wantGeneric: result = res[0]

proc tryLoadAllHooks*(typ: SymId): HooksPerType =
  let nifName = pool.syms[typ]
  let modname = extractModule(nifName)
  result = HooksPerType(a: default(array[AttachedOp, (SymId, bool)]))
  if modname != "":
    var m = load(modname)
    if m.index.hooks.hasKey(typ):
      result = m.index.hooks[typ]

proc loadVTable*(typ: SymId): seq[MethodIndexEntry] =
  let nifName = pool.syms[typ]
  let modname = extractModule(nifName)
  if modname != "":
    var m = load(modname)
    for entry in m.index.classes:
      if entry.cls == typ:
        return entry.methods
  return @[]

proc loadSyms*(suffix: string; identifier: StrId): seq[SymId] =
  # gives top level exported syms of a module
  result = @[]
  var m = load(suffix)
  for k, _ in m.index.public:
    var base = k
    extractBasename(base)
    let strId = pool.strings.getOrIncl(base)
    if strId == identifier:
      let symId = pool.syms.getOrIncl(k)
      result.add symId

proc registerHook*(suffix: string; typ: SymId; op: AttachedOp; hook: SymId; isGeneric: bool) =
  let m: NifModule
  if not prog.mods.hasKey(suffix):
    let infile = suffixToNif suffix
    m = newNifModule(infile)
    prog.mods[suffix] = m
  else:
    m = prog.mods[suffix]
  if not m.index.hooks.hasKey(typ):
    m.index.hooks[typ] = HooksPerType(a: default(array[AttachedOp, (SymId, bool)]))
  m.index.hooks[typ].a[op] = (hook, isGeneric)

proc knowsSym*(s: SymId): bool {.inline.} = prog.mem.hasKey(s)

proc getEntry*(s: SymId): ptr ToplevelEntry {.inline.} =
  ## Returns a pointer to the entry for mutation. Use with care.
  if prog.mem.hasKey(s):
    result = addr prog.mem[s]
  else:
    result = nil

proc publish*(s: SymId; buf: sink TokenBuf; phase = SemcheckBodies) =
  if prog.mem.hasKey(s):
    prog.mem[s].buffer = buf
    prog.mem[s].phase = phase
  else:
    prog.mem[s] = ToplevelEntry(buffer: buf, phase: phase)

proc publish*(s: SymId; dest: TokenBuf; start: int; phase = SemcheckBodies) =
  var buf = createTokenBuf(dest.len - start + 1)
  for i in start..<dest.len:
    buf.add dest[i]
  publish s, buf, phase

proc publishSignature*(dest: TokenBuf; s: SymId; start: int) =
  var buf = createTokenBuf(dest.len - start + 3)
  for i in start..<dest.len:
    buf.add dest[i]
  buf.addDotToken() # body is empty for a signature
  buf.addParRi()
  publish s, buf, SemcheckSignatures

proc publishStringType() =
  # This logic is not strictly necessary for "system.nim" itself, but
  # for modules that emulate system via --isSystem.
  let symId = pool.syms.getOrIncl(StringName)
  let aId = pool.syms.getOrIncl(StringAField)
  let iId = pool.syms.getOrIncl(StringIField)
  let exportMarker = pool.strings.getOrIncl("x")
  var str = createTokenBuf(10)
  str.copyIntoUnchecked "type", NoLineInfo:
    str.add symdefToken(symId, NoLineInfo)
    str.add identToken(exportMarker, NoLineInfo)
    str.addDotToken() # pragmas
    str.addDotToken() # generic parameters
    str.copyIntoUnchecked "object", NoLineInfo:
      str.addDotToken() # inherits from nothing
      str.copyIntoUnchecked "fld", NoLineInfo:
        str.add symdefToken(aId, NoLineInfo)
        str.addDotToken() # export marker
        str.addDotToken() # pragmas
        # type is `ptr UncheckedArray[char]`
        str.copyIntoUnchecked "ptr", NoLineInfo:
          str.copyIntoUnchecked "uarray", NoLineInfo:
            str.copyIntoUnchecked "c", NoLineInfo:
              str.add intToken(pool.integers.getOrIncl(8), NoLineInfo)
        str.addDotToken() # default value

      str.copyIntoUnchecked "fld", NoLineInfo:
        str.add symdefToken(iId, NoLineInfo)
        str.addDotToken() # export marker
        str.addDotToken() # pragmas
        str.copyIntoUnchecked "i", NoLineInfo:
          str.add intToken(pool.integers.getOrIncl(-1), NoLineInfo)
        str.addDotToken() # default value

  publish symId, str, SemcheckBodies

proc setupProgram*(infile, outfile: string; owningBuf: var TokenBuf; hasIndex=false): Cursor =
  prog.main = splitModulePath(infile)
  let outp = splitModulePath(outfile)
  if prog.main.dir.len == 0:
    prog.main.dir = getCurrentDir()
  prog.main.ext = outp.ext

  var m = newNifModule(infile)

  if hasIndex:
    let indexName = infile.changeModuleExt".s.idx.nif"
    #if not fileExists(indexName) or getLastModificationTime(indexName) < getLastModificationTime(infile):
    #  createIndex infile
    m.index = readIndex(indexName)

  #echo "INPUT IS ", toString(m.buf)
  owningBuf = fromStream(m.stream)

  result = beginRead(owningBuf)
  prog.mods[prog.main.name] = m
  publishStringType()

proc setupProgramForTesting*(dir, file, ext: string) =
  prog.main.dir = dir
  prog.main.name = file
  prog.main.ext = ext
  publishStringType()

proc takeParRi*(dest: var TokenBuf; n: var Cursor) =
  if n.kind == ParRi:
    dest.add n
    inc n
  else:
    bug "expected ')', but got: ", n

proc skipParRi*(n: var Cursor) =
  if n.kind == ParRi:
    inc n
  else:
    bug "expected ')', but got: ", n

template isLocalDecl*(s: SymId): bool =
  extractModule(pool.syms[s]) == ""

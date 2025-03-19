#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [syncio, os, tables, sequtils, times, sets]
include nifprelude
import nifindexes, symparser, reporters, builtintypes
import ".." / models / [nifindex_tags]

type
  Iface* = OrderedTable[StrId, seq[SymId]] # eg. "foo" -> @["foo.1.mod", "foo.3.mod"]

  NifModule = ref object
    buf: TokenBuf
    stream: Stream
    index: NifIndex

  Program* = object
    mods: Table[string, NifModule]
    dir, main*, ext: string
    mem: Table[SymId, TokenBuf]

  ImportFilterKind* = enum
    ImportAll, FromImport, ImportExcept

  ImportFilter* = object
    kind*: ImportFilterKind
    list*: HashSet[StrId] # `from import` or `import except` symbol list

var
  prog*: Program

proc newNifModule(infile: string): NifModule =
  result = NifModule(stream: nifstreams.open(infile))
  discard processDirectives(result.stream.r)
  result.buf = fromStream(result.stream)

proc suffixToNif*(suffix: string): string {.inline.} =
  # always imported from semchecked files
  prog.dir / suffix & ".2.nif"

proc needsRecompile*(dep, output: string): bool =
  result = not fileExists(output) or getLastModificationTime(output) < getLastModificationTime(dep)

proc load(suffix: string): NifModule =
  if not prog.mods.hasKey(suffix):
    let infile = suffixToNif suffix
    result = newNifModule(infile)
    let indexName = infile.changeFileExt".idx.nif"
    #if not fileExists(indexName) or getLastModificationTime(indexName) < getLastModificationTime(infile):
    #  createIndex infile
    result.index = readIndex(indexName)
    prog.mods[suffix] = result
  else:
    result = prog.mods[suffix]

proc mergeFilter(f: var ImportFilter; g: ImportFilter) =
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

proc loadInterface*(suffix: string; iface: var Iface;
                    module: SymId; importTab: var OrderedTable[StrId, seq[SymId]];
                    converters: var Table[SymId, seq[SymId]];
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

proc error*(msg: string; c: Cursor) {.noreturn.} =
  when defined(debug):
    writeStackTrace()
  write stdout, "[Error] "
  if isValid(c.info):
    write stdout, infoToStr(c.info)
    write stdout, " "
  write stdout, msg
  writeLine stdout, toString(c, false)
  quit 1

proc error*(msg: string) {.noreturn.} =
  when defined(debug):
    writeStackTrace()
  write stdout, "[Error] "
  write stdout, msg
  quit 1

type
  LoadStatus* = enum
    LacksModuleName, LacksOffset, LacksPosition, LacksNothing
  LoadResult* = object
    status*: LoadStatus
    decl*: Cursor

proc tryLoadSym*(s: SymId): LoadResult =
  if prog.mem.hasKey(s):
    result = LoadResult(status: LacksNothing, decl: cursorAt(prog.mem[s], 0))
  else:
    let nifName = pool.syms[s]
    let modname = extractModule(nifName)
    if modname == "":
      result = LoadResult(status: LacksModuleName)
    else:
      var m = load(modname)
      var entry = m.index.public.getOrDefault(nifName)
      if entry.offset == 0:
        entry = m.index.private.getOrDefault(nifName)
      if entry.offset == 0:
        result = LoadResult(status: LacksOffset)
      else:
        m.stream.r.jumpTo entry.offset
        var buf = createTokenBuf(30)
        nifcursors.parse(m.stream, buf, entry.info)
        let decl = cursorAt(buf, 0)
        prog.mem[s] = ensureMove(buf)
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

proc publish*(s: SymId; buf: sink TokenBuf) =
  prog.mem[s] = buf

proc splitModulePath*(s: string): (string, string, string) =
  var (dir, main, ext) = splitFile(s)
  let dotPos = find(main, '.')
  if dotPos >= 0:
    ext = substr(main, dotPos) & ext
    main.setLen dotPos
  result = (dir, main, ext)

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

  publish symId, str

proc setupProgram*(infile, outfile: string; hasIndex=false): Cursor =
  let (dir, file, _) = splitModulePath(infile)
  let (_, _, ext) = splitModulePath(outfile)
  prog.dir = (if dir.len == 0: getCurrentDir() else: dir)
  prog.ext = ext
  prog.main = file

  var m = newNifModule(infile)

  if hasIndex:
    let indexName = infile.changeFileExt".idx.nif"
    #if not fileExists(indexName) or getLastModificationTime(indexName) < getLastModificationTime(infile):
    #  createIndex infile
    m.index = readIndex(indexName)

  #echo "INPUT IS ", toString(m.buf)
  result = beginRead(m.buf)
  prog.mods[prog.main] = m
  publishStringType()

proc setupProgramForTesting*(dir, file, ext: string) =
  prog.dir = dir
  prog.main = file
  prog.ext = ext
  publishStringType()

proc takeParRi*(dest: var TokenBuf; n: var Cursor) =
  if n.kind == ParRi:
    dest.add n
    inc n
  else:
    error "expected ')', but got: ", n

proc skipParRi*(n: var Cursor) =
  if n.kind == ParRi:
    inc n
  else:
    error "expected ')', but got: ", n

template isLocalProcDecl*(s: SymId): bool =
  extractModule(pool.syms[s]) == ""

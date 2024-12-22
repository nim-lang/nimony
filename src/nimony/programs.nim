#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [syncio, os, tables, times]
include nifprelude
import nifindexes, symparser, reporters

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

var
  prog*: Program

proc newNifModule(infile: string): NifModule =
  result = NifModule(stream: nifstreams.open(infile))
  discard processDirectives(result.stream.r)
  result.buf = fromStream(result.stream)

proc suffixToNif*(suffix: string): string {.inline.} =
  prog.dir / suffix & prog.ext

proc needsRecompile*(nimfile, suffix: string): bool =
  let nifModule = suffixToNif(suffix)
  result =  not fileExists(nifModule) or getLastModificationTime(nifModule) < getLastModificationTime(nimfile)

proc load*(suffix: string): NifModule =
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

proc loadInterface*(suffix: string; iface: var Iface) =
  let m = load(suffix)
  for k, _ in m.index.public:
    var base = k
    extractBasename(base)
    let strId = pool.strings.getOrIncl(base)
    let symId = pool.syms.getOrIncl(k)
    iface.mgetOrPut(strId, @[]).add symId

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

proc knowsSym*(s: SymId): bool {.inline.} = prog.mem.hasKey(s)

proc publish*(s: SymId; buf: sink TokenBuf) =
  prog.mem[s] = buf

proc splitModulePath(s: string): (string, string, string) =
  var (dir, main, ext) = splitFile(s)
  let dotPos = find(main, '.')
  if dotPos >= 0:
    ext = substr(main, dotPos) & ext
    main.setLen dotPos
  result = (dir, main, ext)

proc setupProgram*(infile, outfile: string): Cursor =
  let (dir, file, _) = splitModulePath(infile)
  let (_, _, ext) = splitModulePath(outfile)
  prog.dir = (if dir.len == 0: getCurrentDir() else: dir)
  prog.ext = ext
  prog.main = file

  var m = newNifModule(infile)
  #echo "INPUT IS ", toString(m.buf)
  result = beginRead(m.buf)
  prog.mods[prog.main] = m

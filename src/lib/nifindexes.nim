#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Create an index file for a NIF file.

import std / [tables, assertions, hashes, syncio, strutils]
import bitabs, lineinfos, nifreader, nifpools, nifchecksums, symparser, vfs, stringviews

when defined(nimony):
  import std / sha1
else:
  # See `nifchecksums.nim`: `std/sha1` is the portable choice that ships in
  # `lib/std` on every Nim 2.x (the `$nim/dist/checksums/...` path only exists in
  # a git/devel layout). Same SHA-1 fork → identical digests.
  {.push warning[Deprecated]: off.}
  import std / sha1
  {.pop.}
import ".." / models / [tags, nifindex_tags]

include compat2

from nifcoreparse import parse

proc entryKind(tag: TagId): NifIndexKind =
  if rawTagIsNifIndexKind(cast[TagEnum](tag)):
    result = cast[NifIndexKind](tag)
  else:
    result = NoIndexTag

proc isImportant(s: string): bool =
  var c = 0
  for ch in s:
    if ch == '.': inc c
  result = c >= 2

proc isExported(n: Cursor): bool =
  var n = n
  if n.isSymbolDef:
    inc n
    if not n.isDotToken:
      return true
  return false

proc processDeclForChecksum(dest: var Sha1State; n: var Cursor) =
  ## Hashes the single tree/token at `n` — the exported declarations in it,
  ## to be exact — advancing past it.
  let inlineT = TagId(InlineIdx)
  if n.isTagLit:
    var foundInline = false
    let k = entryKind(n.cursorTagId)
    case k
    of LetIdx, VarIdx, CursorIdx, ConstIdx, TypeIdx, GletIdx, TletIdx, GvarIdx, TvarIdx:
      n.into: # tag
        if isExported(n):
          updateLoop(dest, n, inlineT, foundInline) # SymbolDef
          updateLoop(dest, n, inlineT, foundInline) # Export marker
          updateLoop(dest, n, inlineT, foundInline) # pragmas
          updateLoop(dest, n, inlineT, foundInline) # type
          updateLoop(dest, n, inlineT, foundInline) # value
        while n.hasMore: skip n
    of TemplateIdx, MacroIdx, IteratorIdx:
      # these always have inline semantics
      n.into: # tag
        if isExported(n):
          updateLoop(dest, n, inlineT, foundInline) # SymbolDef
          updateLoop(dest, n, inlineT, foundInline) # Export marker
          updateLoop(dest, n, inlineT, foundInline) # pattern
          updateLoop(dest, n, inlineT, foundInline) # typevars
          updateLoop(dest, n, inlineT, foundInline) # params
          updateLoop(dest, n, inlineT, foundInline) # retType
          updateLoop(dest, n, inlineT, foundInline) # pragmas
          updateLoop(dest, n, inlineT, foundInline) # effects
          updateLoop(dest, n, inlineT, foundInline) # body
        while n.hasMore: skip n
    of ProcIdx, FuncIdx, MethodIdx, ConverterIdx:
      n.into: # tag
        if isExported(n):
          var dummy = false
          updateLoop(dest, n, inlineT, dummy) # SymbolDef
          updateLoop(dest, n, inlineT, dummy) # Export marker
          updateLoop(dest, n, inlineT, dummy) # pattern
          updateLoop(dest, n, inlineT, dummy) # typevars
          updateLoop(dest, n, inlineT, dummy) # params
          updateLoop(dest, n, inlineT, dummy) # retType
          updateLoop(dest, n, inlineT, foundInline) # pragmas
          updateLoop(dest, n, inlineT, dummy) # effects
          if foundInline:
            updateLoop(dest, n, inlineT, dummy) # body
          else:
            skip n
        while n.hasMore: skip n
    of NoIndexTag, InlineIdx, KvIdx, VvIdx, BuildIdx, BundleIdx, IndexIdx,
       ExportIdx, FromexportIdx, ExportexceptIdx:
      n.loopInto:
        processDeclForChecksum(dest, n)
  else:
    inc n

proc processForChecksum(dest: var Sha1State; content: var TokenBuf) =
  var n = beginRead(content)
  processDeclForChecksum(dest, n)

type
  HookIndexEntry* = object
    typ*, hook*: SymId
    isGeneric*: bool

  IndexSections* = object
    converters*: seq[(SymId, SymId)] # string is for compat with `methods`
    exportBuf*: TokenBuf

proc getSymbolSection(tag: TagId; values: seq[(SymId, SymId)]): TokenBuf =
  result = createTokenBuf(30)
  result.addParLe tag

  for value in values:
    let (key, sym) = value
    result.addParLe(TagId(KvIdx), NoLineInfo)
    if key == SymId(0):
      result.addDotToken(NoLineInfo)
    else:
      result.addSymUse(key, NoLineInfo)
    result.addSymUse(sym, NoLineInfo)
    result.addParRi()

  result.addParRi()

proc createIndex*(infile: string; root: PackedLineInfo; buildChecksum: bool; sections: IndexSections) {.canRaise.} =
  # Mirror the doc-mode cache split: `foo.sc.nif` → `foo.sc.idx.nif`, the regular
  # `foo.s.nif` → `foo.s.idx.nif`. Keeps both populations valid in parallel.
  let isDocMode = infile.endsWith(".sc.nif")
  let idxExt = if isDocMode: ".sc.idx.nif" else: ".s.idx.nif"
  let indexName = changeModuleExt(infile, idxExt)
  var content = "(.nif27)\n(index\n"

  if sections.converters.len != 0:
    let converterSectionBuf = getSymbolSection(TagId(ConverterIdx), sections.converters)

    content.add toString(converterSectionBuf)
    content.add "\n"

  if sections.exportBuf.len != 0:
    content.add toString(sections.exportBuf)
    content.add "\n"

  if buildChecksum:
    var r = nifreader.open(infile)
    var buf = createTokenBuf()
    nifcoreparse.parse(r, buf)
    var checksum = newSha1State()
    processForChecksum(checksum, buf)
    let final = SecureHash checksum.finalize()
    content.add "(checksum \"" & $final & "\")"
  content.add "\n)\n"
  # OnlyIfChanged: clients (downstream nimsem/hexer) depend on this file.
  # When a module's interface and inline-proc bodies are unchanged, its
  # checksum is identical, so we keep the old mtime to avoid cascading
  # rebuilds through importers. nifmake's `needsRebuild` uses the freshest
  # of a node's outputs (max), so the always-written `.s.nif` still proves
  # "we ran since the inputs changed" for nimsem's own staleness check.
  let existingContent = try: vfsRead(indexName) except: ""
  if existingContent != content:
    vfsWrite(indexName, content)

proc createIndex*(infile: string; buildChecksum: bool; root: PackedLineInfo) {.canRaise.} =
  createIndex(infile, root, buildChecksum, IndexSections())

proc writeFileAndIndex*(outfile: string; content: TokenBuf) {.canRaise.} =
  vfsWrite(outfile, toString(content, true))
  var c = cursorAt(cast[ptr TokenBuf](unsafeAddr content)[], 0)
  createIndex(outfile, true, c.info)
type
  IndexVisibility* = enum
    Hidden, Exported
  NifIndexEntry* = object
    offset*: int
    info*: PackedLineInfo
    vis*: IndexVisibility

  NifIndex* = object
    converters*: seq[(string, string)] # map of dest types to converter symbols
    exports*: seq[(string, NifIndexKind, seq[StrId])] # module, export kind, filtered names

proc symOrIdentOrDot(n: var Cursor): string =
  if n.isSymbol: result = pool.syms[n.symId]
  elif n.isIdent: result = pool.strings[n.strId]
  elif n.isDotToken: result = "."
  else: result = ""
  skip n

proc readSymbolSectionC(n: var Cursor; tab: var seq[(string, string)]) =
  ## Cursor walk of a `(converter (kv key value) …)` section.
  n.into:
    while n.hasMore:
      if n.isTagLit and n.cursorTagId == TagId(KvIdx):
        n.into:
          let key = symOrIdentOrDot(n)
          let value = symOrIdentOrDot(n)
          tab.add((key, value))
      else:
        skip n

proc readIndex*(indexName: string): NifIndex =
  var r = nifreader.open(indexName)
  var buf = createTokenBuf()
  nifcoreparse.parse(r, buf)   # reads directives + the `(index …)` tree
  r.close()
  result = default(NifIndex)
  var n = beginRead(buf)
  if not (n.isTagLit and n.cursorTagId == TagId(IndexIdx)):
    assert false, "expected 'index' tag"
    return
  n.into:
    while n.hasMore:
      if n.isTagLit:
        let t = n.cursorTagId
        if t == TagId(ConverterIdx):
          readSymbolSectionC(n, result.converters)
        elif t == TagId(ExportIdx) or t == TagId(FromexportIdx) or
             t == TagId(ExportexceptIdx):
          let kind = cast[NifIndexKind](n.cursorTagId)
          n.into:
            assert n.isStringLit
            let path = pool.strings[n.strId]
            skip n
            var names: seq[StrId] = @[]
            while n.hasMore:
              if n.isIdent: names.add n.strId
              skip n
            result.exports.add (path, kind, names)
        else:
          skip n
      else:
        skip n

proc readEmbeddedIndex*(r: var Reader): Table[string, NifIndexEntry] =
  ## Reader-based twin of the classic streaming version (mirrors
  ## `foreignmodules.readEmbeddedIndex`, but keeps visibility). Line info is
  ## not recovered from the raw index tokens — `addEmbeddedIndex` only needs
  ## the visibility and offset, so `info` is left `NoLineInfo`.
  result = initTable[string, NifIndexEntry]()
  let indexPos = indexStartsAt(r)
  if indexPos <= 0: return
  let contentPos = offset(r)
  r.jumpTo(indexPos)
  var previousOffset = 0
  var t = default(ExpandedToken)
  next(r, t)                                   # `(.index`
  if t.tk == ParLe and t.data == ".index":
    next(r, t)
    while t.tk != EofToken and t.tk != ParRi:
      if t.tk == ParLe:
        let vis = if t.data == "x": Exported else: Hidden
        next(r, t)                             # the symbol
        var key = ""
        if t.tk == Symbol: key = decodeStr(r, t)
        next(r, t)                             # the (delta) offset
        if t.tk == IntLit:
          let off = int(decodeInt t) + previousOffset
          if key.len > 0:
            result[key] = NifIndexEntry(offset: off, info: NoLineInfo, vis: vis)
          previousOffset = off
        next(r, t)                             # closing `)`
        if t.tk == ParRi: next(r, t)
      else:
        next(r, t)
  r.jumpTo(contentPos)

when isMainModule:
  import std / [os]
  try:
    createIndex paramStr(1), false, NoLineInfo
  except:
    quit "createIndex failed"

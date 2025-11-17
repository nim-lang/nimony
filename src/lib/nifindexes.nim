#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Create an index file for a NIF file.

import std / [os, tables, assertions, syncio]
import bitabs, lineinfos, nifreader, nifstreams, nifcursors, nifchecksums, symparser

#import std / [sha1]
import "$nim"/dist/checksums/src/checksums/sha1
import ".." / models / [tags, nifindex_tags]

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
  if n.kind == SymbolDef:
    inc n
    if n.kind != DotToken:
      return true
  return false

proc processForChecksum(dest: var Sha1State; content: var TokenBuf) =
  var n = beginRead(content)
  var nested = 0
  let inlineT = TagId(InlineIdx)
  while true:
    case n.kind
    of ParLe:
      var foundInline = false
      let k = entryKind(n.tagId)
      case k
      of LetIdx, VarIdx, CursorIdx, ConstIdx, TypeIdx, GletIdx, TletIdx, GvarIdx, TvarIdx:
        inc n # tag
        if isExported(n):
          updateLoop(dest, n, inlineT, foundInline) # SymbolDef
          updateLoop(dest, n, inlineT, foundInline) # Export marker
          updateLoop(dest, n, inlineT, foundInline) # pragmas
          updateLoop(dest, n, inlineT, foundInline) # type
          updateLoop(dest, n, inlineT, foundInline) # value
        skipToEnd n
      of TemplateIdx, MacroIdx, IteratorIdx:
        # these always have inline semantics
        inc n # tag
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
        skipToEnd n
      of ProcIdx, FuncIdx, MethodIdx, ConverterIdx:
        inc n # tag
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
        skipToEnd n
      of NoIndexTag, InlineIdx, KvIdx, VvIdx, BuildIdx, IndexIdx, PublicIdx, PrivateIdx,
         DestroyIdx, DupIdx, CopyIdx, WasmovedIdx, SinkhIdx, TraceIdx,
         ExportIdx, FromexportIdx, ExportexceptIdx:
        inc n
        inc nested
    of ParRi:
      dec nested
      if nested == 0:
        break
      inc n
    else:
      inc n

type
  AttachedOp* = enum # this one can be used as an array index
    attachedDestroy,
    attachedWasMoved,
    attachedDup,
    attachedCopy,
    attachedSink,
    attachedTrace

  HookIndexEntry* = object
    typ*, hook*: SymId
    isGeneric*: bool

  MethodIndexEntry* = object
    fn*: SymId
    signature*: StrId

  ClassIndexEntry* = object
    cls*: SymId
    methods*: seq[MethodIndexEntry]

  IndexSections* = object
    hooks*: array[AttachedOp, seq[HookIndexEntry]]
    converters*: seq[(SymId, SymId)] # string is for compat with `methods`
    classes*: seq[ClassIndexEntry]
    exportBuf*: TokenBuf

proc hookName*(op: AttachedOp): string =
  case op
  of attachedDestroy: "destroy"
  of attachedWasMoved: "wasmoved"
  of attachedDup: "dup"
  of attachedCopy: "copy"
  of attachedSink: "sinkh"
  of attachedTrace: "trace"

proc hookToTag(op: AttachedOp): TagId =
  case op
  of attachedDestroy: TagId(DestroyIdx)
  of attachedWasMoved: TagId(WasmovedIdx)
  of attachedDup: TagId(DupIdx)
  of attachedCopy: TagId(CopyIdx)
  of attachedSink: TagId(SinkhIdx)
  of attachedTrace: TagId(TraceIdx)

proc getHookSection(tag: TagId; values: openArray[HookIndexEntry]): TokenBuf =
  result = createTokenBuf(30)
  result.addParLe tag

  for v in values:
    let t = if v.isGeneric: TagId(VvIdx) else: TagId(KvIdx)
    result.buildTree t, NoLineInfo:
      result.add symToken(v.typ, NoLineInfo)
      result.add symToken(v.hook, NoLineInfo)

  result.addParRi()

proc getSymbolSection(tag: TagId; values: seq[(SymId, SymId)]): TokenBuf =
  result = createTokenBuf(30)
  result.addParLe tag

  for value in values:
    let (key, sym) = value
    result.buildTree TagId(KvIdx), NoLineInfo:
      if key == SymId(0):
        result.add dotToken(NoLineInfo)
      else:
        result.add symToken(key, NoLineInfo)
      result.add symToken(sym, NoLineInfo)

  result.addParRi()

proc getClassesSection(tag: TagId; values: seq[ClassIndexEntry]): TokenBuf =
  result = createTokenBuf(30)
  result.addParLe tag

  for value in values:
    result.buildTree TagId(KvIdx), NoLineInfo:
      result.add symToken(value.cls, NoLineInfo)
      result.buildTree TagId(StmtsTagId), NoLineInfo:
        for m in value.methods:
          result.buildTree TagId(KvIdx), NoLineInfo:
            result.add symToken(m.fn, NoLineInfo)
            result.add strToken(m.signature, NoLineInfo)
  result.addParRi()

proc createIndex*(infile: string; root: PackedLineInfo; buildChecksum: bool; sections: IndexSections) =
  let indexName = changeModuleExt(infile, ".s.idx.nif")

  var s = nifstreams.open(infile)
  discard processDirectives(s.r)
  var target = -1
  var previousPublicTarget = 0
  var previousPrivateTarget = 0
  var tagId = TagId(0)

  var public = createTokenBuf(30)
  var private = createTokenBuf(30)
  public.addParLe TagId(PublicIdx), root
  private.addParLe TagId(PrivateIdx), root
  var buf = createTokenBuf(100)
  var stack: seq[PackedLineInfo] = @[root]
  while true:
    let offs = offset(s.r)
    let t = next(s)
    if t.kind == EofToken: break
    buf.add t
    if t.kind == ParLe:
      stack.add t.info
      target = offs
      tagId = t.tagId
    elif t.kind == ParRi:
      if stack.len > 1:
        discard stack.pop()
    elif t.kind == SymbolDef:
      #let symInfo = t.info
      let sym = t.symId
      if pool.syms[sym].isImportant:
        let tb = next(s)
        buf.add tb
        # object field symbols are always private so that identifiers outside of dot or
        # object constructors are not bound to them.
        let isPublic = tb.kind != DotToken and tagId != TagId(FldTagId)
        var dest =
          if isPublic:
            addr(public)
          else:
            addr(private)
        let diff = if isPublic: target - previousPublicTarget
                  else: target - previousPrivateTarget
        dest[].buildTree TagId(KvIdx), stack[^2]:
          dest[].add symToken(sym, NoLineInfo)
          dest[].add intToken(pool.integers.getOrIncl(diff), NoLineInfo)
        if isPublic:
          previousPublicTarget = target
        else:
          previousPrivateTarget = target

        if tb.kind == ParLe: stack.add tb.info

  public.addParRi()
  private.addParRi()
  close s

  var content = "(.nif24)\n(index\n"
  content.add toString(public)
  content.add "\n"
  content.add toString(private)
  content.add "\n"

  for op in AttachedOp:
    let tag = hookToTag(op)
    let hookSectionBuf = getHookSection(tag, sections.hooks[op])

    content.add toString(hookSectionBuf)
    content.add "\n"

  if sections.converters.len != 0:
    let converterSectionBuf = getSymbolSection(TagId(ConverterIdx), sections.converters)

    content.add toString(converterSectionBuf)
    content.add "\n"

  if sections.classes.len != 0:
    let classesSectionBuf = getClassesSection(TagId(MethodIdx), sections.classes)

    content.add toString(classesSectionBuf)
    content.add "\n"

  if sections.exportBuf.len != 0:
    content.add toString(sections.exportBuf)
    content.add "\n"

  if buildChecksum:
    var checksum = newSha1State()
    processForChecksum(checksum, buf)
    let final = SecureHash checksum.finalize()
    content.add "(checksum \"" & $final & "\")"
  content.add "\n)\n"
  writeFile(indexName, content)

proc createIndex*(infile: string; buildChecksum: bool; root: PackedLineInfo) =
  createIndex(infile, root, buildChecksum,
    IndexSections())

proc writeFileAndIndex*(outfile: string; content: TokenBuf) =
  writeFile(content, outfile)
  createIndex(outfile, true, content[0].info)

type
  NifIndexEntry* = object
    offset*: int
    info*: PackedLineInfo
  HooksPerType* = object
    a*: array[AttachedOp, (SymId, bool)]

  NifIndex* = object
    public*, private*: Table[string, NifIndexEntry]
    hooks*: Table[SymId, HooksPerType]
    converters*: seq[(string, string)] # map of dest types to converter symbols
    #methods*: seq[(string, string)] # map of dest types to method symbols
    classes*: seq[ClassIndexEntry]
    exports*: seq[(string, NifIndexKind, seq[StrId])] # module, export kind, filtered names

proc readSection(s: var Stream; tab: var Table[string, NifIndexEntry]) =
  var previousOffset = 0
  var t = next(s)
  var nested = 1
  while t.kind != EofToken:
    let info = t.info
    if t.kind == ParLe:
      inc nested
      if t.tagId == TagId(KvIdx):
        t = next(s)
        var key: string
        if t.kind == Symbol:
          key = pool.syms[t.symId]
        elif t.kind == Ident:
          key = pool.strings[t.litId]
        else:
          raiseAssert "invalid (kv) construct: symbol expected"
        t = next(s) # skip Symbol
        if t.kind == IntLit:
          let offset = int pool.integers[t.intId] + previousOffset
          tab[key] = NifIndexEntry(offset: offset, info: info)
          previousOffset = offset
        else:
          assert false, "invalid (kv) construct: IntLit expected"
        t = next(s) # skip offset
        if t.kind == ParRi:
          t = next(s)
          dec nested
        else:
          assert false, "invalid (kv) construct: ')' expected"
      else:
        assert false, "expected (kv) construct"
    elif t.kind == ParRi:
      dec nested
      if nested == 0:
        break
      t = next(s)
    else:
      assert false, "expected (kv) construct"
      #t = next(s)

proc readHookSection(s: var Stream; tab: var Table[SymId, HooksPerType]; op: AttachedOp) =
  var t = next(s)
  var nested = 1
  while t.kind != EofToken:
    if t.kind == ParLe:
      inc nested
      if t.tagId == TagId(KvIdx) or t.tagId == TagId(VvIdx):
        let isGeneric = t.tagId == TagId(VvIdx)
        t = next(s)
        var key = SymId(0)
        if t.kind == Symbol:
          key = t.symId
        else:
          raiseAssert "invalid (kv) construct: symbol expected"
        t = next(s) # skip Symbol
        if t.kind == Symbol:
          if not tab.hasKey(key):
            tab[key] = HooksPerType(a: default(array[AttachedOp, (SymId, bool)]))
          tab[key].a[op] = (t.symId, isGeneric)
        else:
          assert false, "invalid (kv) construct: symbol expected"
        t = next(s) # skip Symbol 2
        if t.kind == ParRi:
          t = next(s)
          dec nested
        else:
          assert false, "invalid (kv) construct: ')' expected"
      else:
        assert false, "expected (kv) or (vv) construct"
    elif t.kind == ParRi:
      dec nested
      if nested == 0:
        break
      t = next(s)
    else:
      assert false, "expected (kv) or (vv) construct"
      #t = next(s)

proc readSymbolSection(s: var Stream; tab: var seq[(string, string)]) =
  var t = next(s)
  var nested = 1
  while t.kind != EofToken:
    if t.kind == ParLe:
      inc nested
      if t.tagId == TagId(KvIdx):
        t = next(s)
        var key: string
        if t.kind == Symbol:
          key = pool.syms[t.symId]
        elif t.kind == Ident:
          key = pool.strings[t.litId]
        elif t.kind == DotToken:
          key = "."
        else:
          raiseAssert "invalid (kv) construct: symbol expected"
        t = next(s) # skip Symbol
        var value: string
        if t.kind == Symbol:
          value = pool.syms[t.symId]
        elif t.kind == Ident:
          value = pool.strings[t.litId]
        else:
          raiseAssert "invalid (kv) construct: symbol expected"
        t = next(s) # skip value symbol
        tab.add((key, value))
        if t.kind == ParRi:
          t = next(s)
          dec nested
        else:
          assert false, "invalid (kv) construct: ')' expected"
      else:
        assert false, "expected (kv) construct"
    elif t.kind == ParRi:
      dec nested
      if nested == 0:
        break
      t = next(s)
    else:
      assert false, "expected (kv) construct"
      #t = next(s)

proc readClassesSection(s: var Stream; tab: var seq[ClassIndexEntry]) =
  var t = next(s)
  while t.kind == ParLe and t.tagId == TagId(KvIdx):
    t = next(s)
    var cls = SymId(0)
    if t.kind == Symbol:
      cls = t.symId
    else:
      raiseAssert "invalid (kv) construct: symbol expected"
    t = next(s) # skip Symbol
    var methods: seq[MethodIndexEntry] = @[]
    if t.kind == ParLe and t.tagId == TagId(StmtsTagId):
      t = next(s)
      while t.kind == ParLe and t.tagId == TagId(KvIdx):
        t = next(s)
        var fn = SymId(0)
        if t.kind == Symbol:
          fn = t.symId
        else:
          raiseAssert "invalid (kv) construct: symbol expected"
        t = next(s) # skip Symbol
        var signature = StrId(0)
        if t.kind == StringLit:
          signature = t.litId
        else:
          raiseAssert "invalid (kv) construct: string expected"
        t = next(s) # skip StringLit
        methods.add(MethodIndexEntry(fn: fn, signature: signature))
        if t.kind == ParRi: # KvIdx
          t = next(s)
        else:
          raiseAssert "invalid (kv) construct: ')' expected"
      if t.kind == ParRi:
        t = next(s)
      else:
        assert false, "invalid (stmts) construct: ')' expected"
    tab.add ClassIndexEntry(cls: cls, methods: methods)
    if t.kind == ParRi:
      t = next(s)
    else:
      assert false, "invalid (kv) construct: ')' expected"
  if t.kind == ParRi: # MethodIdx
    t = next(s)
  else:
    raiseAssert "invalid (method) construct: ')' expected"

proc readIndex*(indexName: string): NifIndex =
  var s = nifstreams.open(indexName)
  let res = processDirectives(s.r)
  assert res == Success

  result = default(NifIndex)
  var t = next(s)
  if t.tag == TagId(IndexIdx):
    t = next(s)
    if t.tag == TagId(PublicIdx):
      readSection s, result.public
    else:
      assert false, "'public' expected"
    t = next(s)
    if t.tag == TagId(PrivateIdx):
      readSection s, result.private
    else:
      assert false, "'private' expected"
    t = next(s)
    for op in AttachedOp:
      if t.kind == ParLe and pool.tags[t.tag] == hookName(op):
        readHookSection(s, result.hooks, op)
        t = next(s)
    if t.tag == TagId(ConverterIdx):
      readSymbolSection(s, result.converters)
      t = next(s)
    if t.tag == TagId(MethodIdx):
      readClassesSection(s, result.classes)
      t = next(s)

    while t.tag == TagId(ExportIdx) or t.tag == TagId(FromexportIdx) or t.tag == TagId(ExportexceptIdx):
      let kind = cast[NifIndexKind](t.tag)
      t = next(s)
      assert t.kind == StringLit
      let path = pool.strings[t.litId]
      t = next(s)
      var names: seq[StrId] = @[]
      while t.kind != ParRi:
        assert t.kind == Ident
        names.add t.litId
        t = next(s)
      result.exports.add (path, kind, names)
      t = next(s)
  else:
    assert false, "expected 'index' tag"

when isMainModule:
  createIndex paramStr(1), false, NoLineInfo

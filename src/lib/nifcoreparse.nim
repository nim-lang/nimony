#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## NIF text parser producing a `nifcore.TokenBuf`.
##
## Reuses the existing `nifreader`, whose `ExpandedToken` is independent of
## the in-memory representation: its `ParLe`/`ParRi` events drive nifcore's
## `openTag`/`closeTag`, and atoms drive the builder. The reader's relative
## line positions are resolved to absolute file/line/col (against a parent
## stack, exactly like `nifstreams.rawNext`) and emitted as a sparse
## `LineInfoLit` suffix only where the position changes. Callers that need
## random-access effective locations can request dense line info while parsing.
##
## Note: NIF `#comment#` decorations are not carried into the token buffer —
## the codegen path has no use for them.

import std / assertions
import nifcore
import nifreader as rd
import nifbuilder
import stringviews
import lineinfos  # for `==`(FileId) used by NifLineInfo's structural `==`

export nifcore

type
  Parent = tuple[file: FileId; line, col: int32]

proc resolveInfo(b: var TokenBuf; tok: rd.ExpandedToken;
                 parents: seq[Parent]): NifLineInfo {.inline.} =
  ## Absolute file/line/col for `tok`, plus any `#…#` comment. Absolute when
  ## the token carries a filename; otherwise relative to the enclosing tag
  ## (parents[^1]). A comment is only carried when the file resolves (line info
  ## is the comment's vehicle); in the IC use it always rides a positioned sym.
  var comment = StrId(0)
  if tok.comment.len != 0:
    comment = b.pool.strings.getOrIncl(rd.decodeComment(tok))
  if tok.filename.len != 0:
    let f = b.pool.filenames.getOrIncl(rd.decodeFilename(tok))
    result = NifLineInfo(file: f, line: tok.pos.line, col: tok.pos.col,
                         comment: comment)
  else:
    let p = parents[^1]
    result = NifLineInfo(file: p.file,
                         line: p.line + tok.pos.line,
                         col:  p.col + tok.pos.col,
                         comment: comment)

proc parse*(r: var rd.Reader; b: var TokenBuf;
            parentSeed: NifLineInfo = NoNifLineInfo;
            denseLineInfo = false) =
  ## Read one complete NIF tree (or until EOF) from `r` into `b`. `parentSeed`
  ## seeds the parent line-info stack so the first token's relative position
  ## resolves against the right origin (index-jumped reads pass the indexed
  ## compound's parent info; whole-file reads pass `NoNifLineInfo`).
  ## With `denseLineInfo`, every positioned value receives its effective
  ## location instead of only changes in the location stream.
  var parents = @[(file: parentSeed.file, line: parentSeed.line,
                   col: parentSeed.col)]
  var last = NoNifLineInfo
  var nested = 0
  var tok = default(rd.ExpandedToken)
  template emit(info: NifLineInfo) =
    if info.isValid and (denseLineInfo or info != last):
      b.appendLineInfo info
      last = info
  while true:
    rd.next(r, tok)
    case tok.tk
    of rd.EofToken, rd.UnknownToken:
      break
    of rd.ParLe:
      let info = resolveInfo(b, tok, parents)
      b.openTag b.tags.tags.getOrInclFromView(tok.data)
      emit info
      parents.add (info.file, info.line, info.col)
      inc nested
    of rd.ParRi:
      b.closeTag()
      if parents.len > 1: discard parents.pop()
      dec nested
      if nested == 0: break
    of rd.DotToken:
      let info = resolveInfo(b, tok, parents); b.addDotToken(); emit info
    of rd.Ident:
      let info = resolveInfo(b, tok, parents)
      b.addIdent rd.decodeStr(r, tok); emit info
    of rd.Symbol:
      let info = resolveInfo(b, tok, parents)
      b.addSymUse rd.decodeStr(r, tok); emit info
    of rd.SymbolDef:
      let info = resolveInfo(b, tok, parents)
      b.addSymDef rd.decodeStr(r, tok); emit info
    of rd.StringLit:
      let info = resolveInfo(b, tok, parents)
      b.addStrLit rd.decodeStr(r, tok); emit info
    of rd.CharLit:
      let info = resolveInfo(b, tok, parents); b.addCharLit rd.decodeChar(tok); emit info
    of rd.IntLit:
      let info = resolveInfo(b, tok, parents); b.addIntLit rd.decodeInt(tok); emit info
    of rd.UIntLit:
      let info = resolveInfo(b, tok, parents); b.addUIntLit rd.decodeUInt(tok); emit info
    of rd.FloatLit:
      let info = resolveInfo(b, tok, parents); b.addFloatLit rd.decodeFloat(tok); emit info
    if nested == 0 and tok.tk notin {rd.ParLe}:
      # A bare top-level atom is a complete "tree" on its own.
      break

proc parseFromBuffer*(input: string; thisModule: sink string;
                      sizeHint = 100; sharedPool: Pool = nil;
                      sharedTags: TagPool = nil;
                      denseLineInfo = false): TokenBuf =
  var r = rd.openFromBuffer(input, thisModule)
  result = createTokenBuf(sizeHint, sharedPool, sharedTags)
  parse(r, result, denseLineInfo = denseLineInfo)

proc parseFromFile*(filename: string; sizeHint = 100;
                    sharedPool: Pool = nil;
                    sharedTags: TagPool = nil;
                    denseLineInfo = false): TokenBuf =
  var r = rd.open(filename)
  discard rd.processDirectives(r)
  result = createTokenBuf(sizeHint, sharedPool, sharedTags)
  parse(r, result, denseLineInfo = denseLineInfo)

# ── toString ─────────────────────────────────────────────────────────────

proc emitRelLineInfo(bld: var Builder; abs, parent: NifLineInfo; pool: Pool;
                     emitComment = true) =
  ## Emit a NIF27 line-info suffix for `abs`, relative to the enclosing tag's
  ## `parent` info when they share a file (absolute, with filename, otherwise).
  ## Mirrors `nifstreams.emitLineInfo`. When `abs` carries a `comment` and
  ## `emitComment` is set, also append it as a `#…#` decoration (the embedded
  ## index passes `emitComment = false` to keep its format comment-free).
  if not abs.file.isValid: return
  var line = abs.line
  var col = abs.col
  var fileStr = ""
  if parent.file.isValid:
    if abs.file != parent.file: fileStr = pool.filenames[abs.file]
    if fileStr.len == 0:                 # same file → relative diff
      line = abs.line - parent.line
      col = abs.col - parent.col
  else:
    fileStr = pool.filenames[abs.file]
  bld.attachLineInfo(col, line, fileStr)
  if emitComment and uint32(abs.comment) != 0'u32:
    bld.attachComment(pool.strings[abs.comment])

proc emitValueWithLineInfo(bld: var Builder; c: var Cursor;
                           cur: var NifLineInfo;
                           parents: var seq[NifLineInfo];
                           tags: TagPool; pool: Pool) =
  ## Emit one value (atom or whole TagLit subtree), advancing `c` past it.
  ## `cur` is the running absolute line info (a head with no `LineInfoLit`
  ## inherits it); `parents` holds enclosing-tag infos for relative encoding.
  let li = rawLineInfo(c)
  let abs = if li.isValid: li else: cur
  if li.isValid:
    cur = li
    cur.comment = StrId(0)   # a comment is a one-shot decoration, never inherited
  case c.kind
  of TagLit:
    bld.addTree(tags.tags[c.cursorTagId])
    emitRelLineInfo(bld, abs, parents[^1], pool)
    parents.add abs
    c.into:
      while c.hasMore:
        emitValueWithLineInfo(bld, c, cur, parents, tags, pool)
    discard parents.pop()
    bld.endTree()
  of DotToken:
    bld.addEmpty(); emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of Ident:
    bld.addIdent(strVal(c, pool))
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
  of StrLit:
    bld.addStrLit(strVal(c, pool))
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
  of Symbol:
    bld.addSymbol(symName(c, pool))
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
  of SymbolDef:
    bld.addSymbolDef(symName(c, pool))
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
  of CharLit:
    bld.addCharLit(charLit(c))
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
  of IntLit:
    bld.addIntLit(intVal(c))
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
  of UIntLit:
    bld.addUIntLit(uintVal(c))
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
  of FloatLit:
    bld.addFloatLit(floatVal(c))
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
  of ExtendedSuffix, LineInfoLit:
    assert false, "suffix token is not a value head"

proc emitValueWithoutLineInfo(bld: var Builder; c: var Cursor;
                              tags: TagPool; pool: Pool) =
  ## Fast diagnostic serializer: no location decoding or parent stack.
  case c.kind
  of TagLit:
    bld.addTree(tags.tags[c.cursorTagId])
    c.into:
      while c.hasMore:
        emitValueWithoutLineInfo(bld, c, tags, pool)
    bld.endTree()
  of DotToken:
    bld.addEmpty(); c.inc
  of Ident:
    bld.addIdent(strVal(c, pool)); c.inc
  of StrLit:
    bld.addStrLit(strVal(c, pool)); c.inc
  of Symbol:
    bld.addSymbol(symName(c, pool)); c.inc
  of SymbolDef:
    bld.addSymbolDef(symName(c, pool)); c.inc
  of CharLit:
    bld.addCharLit(charLit(c)); c.inc
  of IntLit:
    bld.addIntLit(intVal(c)); c.inc
  of UIntLit:
    bld.addUIntLit(uintVal(c)); c.inc
  of FloatLit:
    bld.addFloatLit(floatVal(c)); c.inc
  of ExtendedSuffix, LineInfoLit:
    assert false, "suffix token is not a value head"

proc toString*(b: var TokenBuf; sizeHint = 0;
               includeLineInfo = true): string =
  ## Canonical NIF text for the whole buffer (one or more top-level values).
  ## Set `includeLineInfo` to false for location-free diagnostic rendering.
  var bld = nifbuilder.open(if sizeHint > 0: sizeHint else: b.len * 8)
  var c = b.beginRead()
  if includeLineInfo:
    var cur = NoNifLineInfo
    var parents = @[NoNifLineInfo]
    while c.hasMore:
      emitValueWithLineInfo(bld, c, cur, parents, b.tags, b.pool)
  else:
    while c.hasMore:
      emitValueWithoutLineInfo(bld, c, b.tags, b.pool)
  result = bld.extract()

proc toString*(node: Cursor; sizeHint = 0;
               includeLineInfo = true): string =
  ## Canonical NIF text for the value or subtree at `node`.
  if not node.hasMore:
    return ""
  var bld = nifbuilder.open(
    if sizeHint > 0: sizeHint else: subtreeWidth(node) * 8)
  var c = node
  if includeLineInfo:
    var cur = NoNifLineInfo
    var parents = @[NoNifLineInfo]
    emitValueWithLineInfo(bld, c, cur, parents, node.tags, node.pool)
  else:
    emitValueWithoutLineInfo(bld, c, node.tags, node.pool)
  result = bld.extract()

# ── toModuleString (full file with embedded index) ─────────────────────────

proc emitValueIndexed(bld: var Builder; c: var Cursor; cur: var NifLineInfo;
                      parents: var seq[NifLineInfo]; tags: TagPool; pool: Pool;
                      index: var Builder; dottedSuffix: string;
                      mostRecentOffset, previousOffset: var int;
                      rootInfo: NifLineInfo) =
  ## Like `emitValue`, but also records an index entry for every *global*
  ## SymbolDef. The recorded offset is that of the most recently opened tag (the
  ## declaration's `(`), delta-encoded against the previous entry — mirroring
  ## `nifstreams.toModuleString` so the embedded index is byte-compatible with the
  ## canonical reader (`nifindexes.readEmbeddedIndex`).
  let li = rawLineInfo(c)
  let abs = if li.isValid: li else: cur
  if li.isValid:
    cur = li
    cur.comment = StrId(0)   # a comment is a one-shot decoration, never inherited
  case c.kind
  of TagLit:
    mostRecentOffset = bld.offset            # offset of this `(`
    bld.addTree(tags.tags[c.cursorTagId])
    emitRelLineInfo(bld, abs, parents[^1], pool)
    parents.add abs
    c.into:
      while c.hasMore:
        emitValueIndexed(bld, c, cur, parents, tags, pool, index, dottedSuffix,
                         mostRecentOffset, previousOffset, rootInfo)
    discard parents.pop()
    bld.endTree()
  of SymbolDef:
    let name = symName(c, pool)
    let isGlobal = bld.addSymbolDefRetIsGlobal(name, dottedSuffix)
    emitRelLineInfo(bld, abs, parents[^1], pool)
    c.inc
    if isGlobal:
      # Hidden if the next sibling is the empty export marker (`.`), else exported.
      if c.kind == DotToken: index.addTree "h"
      else: index.addTree "x"
      # Record the indexed compound's PARENT info (parents[^2]); on lookup `parse`
      # seeds its parent stack with this and the first token it reads — the
      # compound's `(` — carries the diff. (See nifstreams.toModuleString.)
      let parentInfo = if parents.len >= 2: parents[^2] else: rootInfo
      emitRelLineInfo(index, parentInfo, rootInfo, pool, emitComment = false)
      index.addSymbol(name, dottedSuffix)
      index.addIntLit(mostRecentOffset - previousOffset)
      previousOffset = mostRecentOffset
      index.endTree()
  of Symbol:
    bld.addSymbol(symName(c, pool), dottedSuffix)
    emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of DotToken:
    bld.addEmpty(); emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of Ident:
    bld.addIdent(strVal(c, pool)); emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of StrLit:
    bld.addStrLit(strVal(c, pool)); emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of CharLit:
    bld.addCharLit(charLit(c)); emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of IntLit:
    bld.addIntLit(intVal(c)); emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of UIntLit:
    bld.addUIntLit(uintVal(c)); emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of FloatLit:
    bld.addFloatLit(floatVal(c)); emitRelLineInfo(bld, abs, parents[^1], pool); c.inc
  of ExtendedSuffix, LineInfoLit:
    assert false, "suffix token is not a value head"

proc toModuleString*(b: var TokenBuf; dottedSuffix = ""; sizeHint = 0): string =
  ## Like `toString` but emits a full module file: the `(.nif27)` header with a
  ## patched `.indexat`, the body, and a trailing `(.index …)` mapping each global
  ## SymbolDef to its declaration's byte offset. `dottedSuffix` (e.g. `.mymod`)
  ## compresses self-module symbol suffixes to a trailing dot (the reader re-
  ## expands via its `thisModule`). Mirrors `nifstreams.toModuleString` so the
  ## output is byte-compatible with the canonical tooling (`reindex`).
  var bld = nifbuilder.open(if sizeHint > 0: sizeHint else: b.len * 20)
  let patchPos = bld.addHeader27()
  var c = b.beginRead()
  var cur = NoNifLineInfo
  var parents = @[NoNifLineInfo]
  var index = nifbuilder.open(b.len * 2)
  index.addTree ".index"
  let rootInfo = rawLineInfo(c)
  if rootInfo.isValid:
    emitRelLineInfo(index, rootInfo, NoNifLineInfo, b.pool)
  var mostRecentOffset = 0
  var previousOffset = 0
  while c.hasMore:
    emitValueIndexed(bld, c, cur, parents, b.tags, b.pool, index, dottedSuffix,
                     mostRecentOffset, previousOffset, rootInfo)
  bld.patchIndexAt(patchPos, bld.offset)
  result = bld.extract()
  index.endTree()
  result.add index.extract()

when isMainModule:
  import std / syncio
  proc sameTokens(a, b: var TokenBuf): bool =
    if a.len != b.len: return false
    for i in 0 ..< a.len:
      if not (a[i] == b[i]): return false
    true
  proc roundTrips(b1: var TokenBuf): bool =
    ## parse → toString → parse must be byte-identical (idempotent).
    let txt = toString(b1)
    var b2 = parseFromBuffer(txt, "t")
    result = sameTokens(b1, b2)
    if not result: echo "round-trip MISMATCH:\n", txt

  block samples:
    for s in [
      "(stmts (call foo 42 \"hi\") (asgn x 3.14) (ret -7))",
      "(proc :myproc.0 . . (params (param x.1 (i +32))) (i +32) (stmts (ret 0)))",
      "(x \"longer than three bytes\" 'a' +18446744073709551615u -9223372036854775808)",
      "(nested (a (b (c (d .)))))"]:
      var b1 = parseFromBuffer(s, "t")
      doAssert roundTrips(b1), s

  block overflow_jump:
    # subtree larger than the 19-bit jump → ExtendedSuffix splice, then text round-trip
    var b1 = createTokenBuf(int(InlineJumpCap) + 16)
    let t = b1.tags.registerTag("big")
    b1.openTag t
    for i in 0 ..< (int(InlineJumpCap) + 5): b1.addIntLit int64(i and 7)
    b1.closeTag()
    doAssert roundTrips(b1)

  block line_info:
    # build a tree carrying line info, then text round-trip (file/line/col survive)
    var b1 = createTokenBuf(16)
    let t = b1.tags.registerTag("stmts")
    let f = b1.pool.filenames.getOrIncl("test.nim")
    b1.openTag t;            b1.appendLineInfo f, 1'i32, 1'i32
    b1.addIdent "a";         b1.appendLineInfo f, 2'i32, 3'i32
    b1.addIntLit 99;         b1.appendLineInfo f, 2'i32, 5'i32   # same line, new col
    b1.addStrLit "z";        b1.appendLineInfo f, 40000'i32, 9'i32  # wide layout
    b1.closeTag()
    doAssert roundTrips(b1)

  echo "nifcoreparse self-tests passed"

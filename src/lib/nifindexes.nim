#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Create an index file for a NIF file.

import std / [os, tables, assertions, syncio, formatfloat]
import bitabs, lineinfos, nifreader, nifstreams, nifcursors

#import std / [sha1]
import "$nim"/dist/checksums/src/checksums/sha1

proc registerTag(tag: string): TagId = pool.tags.getOrIncl(tag)

proc isImportant(s: string): bool =
  var c = 0
  for ch in s:
    if ch == '.': inc c
  result = c >= 2

proc update(dest: var Sha1State; n: PackedToken) =
  case n.kind
  of ParLe:
    update(dest, "(")
    update(dest, pool.tags[n.tagId])
  of ParRi:
    update(dest, ")")
  of SymbolDef:
    update(dest, " :")
    update(dest, pool.syms[n.symId])
  of Symbol:
    update(dest, " ")
    update(dest, pool.syms[n.symId])
  of Ident:
    update(dest, " ")
    update(dest, pool.strings[n.litId])
  of IntLit:
    update(dest, " ")
    update(dest, $pool.integers[n.intId])
  of UIntLit:
    update(dest, " ")
    update(dest, $pool.uintegers[n.uintId])
  of FloatLit:
    update(dest, " ")
    update(dest, $pool.floats[n.floatId])
  of StringLit:
    update(dest, " ")
    update(dest, pool.strings[n.litId])
  of CharLit:
    update(dest, " ")
    update(dest, $n.uoperand)
  of DotToken:
    update(dest, ".")
  of UnknownToken:
    update(dest, "?")
  of EofToken:
    update(dest, "!EOF!")

proc updateLoop(dest: var Sha1State; n: var Cursor; inlineT: TagId; foundInline: var bool) =
  var nested = 0
  while true:
    update dest, n.load
    case n.kind
    of ParLe:
      if n.tagId == inlineT: foundInline = true
      inc nested
    of ParRi:
      dec nested
    else: discard
    if nested <= 0:
      break
    inc n

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
  let letT = registerTag("let")
  let varT = registerTag("var")
  let cursorT = registerTag("cursor")
  let constT = registerTag("const")
  let typeT = registerTag("type")
  let procT = registerTag("proc")
  let templateT = registerTag("template")
  let funcT = registerTag("func")
  let macroT = registerTag("macro")
  let converterT = registerTag("converter")
  let methodT = registerTag("method")
  let iteratorT = registerTag("iterator")
  let inlineT = registerTag("inline")
  while true:
    case n.kind
    of ParLe:
      var foundInline = false
      if n.tagId == letT or n.tagId == varT or n.tagId == cursorT or n.tagId == constT or n.tagId == typeT:
        inc n # tag
        if isExported(n):
          updateLoop(dest, n, inlineT, foundInline) # SymbolDef
          updateLoop(dest, n, inlineT, foundInline) # Export marker
          updateLoop(dest, n, inlineT, foundInline) # pragmas
          updateLoop(dest, n, inlineT, foundInline) # type
          updateLoop(dest, n, inlineT, foundInline) # value
        skipToEnd n
      elif n.tagId == templateT or n.tagId == macroT or n.tagId == iteratorT:
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
      elif n.tagId == procT or n.tagId == funcT or n.tagId == methodT or n.tagId == converterT:
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
      else:
        inc n
        inc nested
    of ParRi:
      dec nested
      if nested == 0:
        break
      inc n
    else:
      inc n

proc createIndex*(infile: string; buildChecksum: bool) =
  let PublicT = registerTag "public"
  let PrivateT = registerTag "private"
  let KvT = registerTag "kv"

  let indexName = changeFileExt(infile, ".idx.nif")

  var s = nifstreams.open(infile)
  discard processDirectives(s.r)
  var target = -1
  var previousPublicTarget = 0
  var previousPrivateTarget = 0

  var public = default(TokenBuf)
  var private = default(TokenBuf)
  public.addParLe PublicT
  private.addParLe PrivateT
  var buf = createTokenBuf(100)

  while true:
    let offs = offset(s.r)
    let t = next(s)
    if t.kind == EofToken: break
    buf.add t
    if t.kind == ParLe:
      target = offs
    elif t.kind == SymbolDef:
      let info = t.info
      let sym = t.symId
      if pool.syms[sym].isImportant:
        let tb = next(s)
        buf.add tb
        let isPublic = tb.kind != DotToken
        var dest =
          if isPublic:
            addr(public)
          else:
            addr(private)
        let diff = if isPublic: target - previousPublicTarget
                  else: target - previousPrivateTarget
        dest[].buildTree KvT, info:
          dest[].add symToken(sym, NoLineInfo)
          dest[].add intToken(pool.integers.getOrIncl(diff), NoLineInfo)
        if isPublic:
          previousPublicTarget = target
        else:
          previousPrivateTarget = target

  public.addParRi()
  private.addParRi()
  close s

  var content = "(.nif24)\n(index\n"
  content.add toString(public)
  content.add "\n"
  content.add toString(private)
  if buildChecksum:
    var checksum = newSha1State()
    processForChecksum(checksum, buf)
    let final = SecureHash checksum.finalize()
    content.add "\n(checksum \"" & $final & "\")"
  content.add "\n)\n"
  if fileExists(indexName) and readFile(indexName) == content:
    discard "no change"
  else:
    writeFile(indexName, content)

type
  NifIndexEntry* = object
    offset*: int
    info*: PackedLineInfo
  NifIndex* = object
    public*, private*: Table[string, NifIndexEntry]

proc readSection(s: var Stream; tab: var Table[string, NifIndexEntry]) =
  let KvT = registerTag "kv"
  var previousOffset = 0
  var t = next(s)
  var nested = 1
  while t.kind != EofToken:
    let info = t.info
    if t.kind == ParLe:
      inc nested
      if t.tagId == KvT:
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
          let offset = pool.integers[t.intId] + previousOffset
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

proc readIndex*(indexName: string): NifIndex =
  var s = nifstreams.open(indexName)
  let res = processDirectives(s.r)
  assert res == Success

  let PublicT = registerTag "public"
  let PrivateT = registerTag "private"
  let IndexT = registerTag "index"

  result = default(NifIndex)
  var t = next(s)
  if t.tag == IndexT:
    t = next(s)
    if t.tag == PublicT:
      readSection s, result.public
    else:
      assert false, "'public' expected"
    t = next(s)
    if t.tag == PrivateT:
      readSection s, result.private
    else:
      assert false, "'private' expected"
  else:
    assert false, "expected 'index' tag"

when isMainModule:
  createIndex paramStr(1), false

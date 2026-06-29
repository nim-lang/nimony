#       Nimony Compiler
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Semantic handling for Nimony plugin output.

import std / [tables, sets, assertions]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / models / [tags, nimony_tags, plugin_tags]
import nimony_model, semdata, sembasics, semos

type
  GeneratedSymbol = object
    name: SymId
    hint: string
    kind: SymKind

proc readGeneratedSymbol(n: var Cursor): tuple[slot: int, hint: string,
                                                kind: SymKind] =
  result = (slot: 0, hint: "", kind: NoSym)
  n.into:
    assert n.kind == IntLit, "plugin symbol slot must be an integer"
    result.slot = int(pool.integers[n.intId])
    inc n

    assert n.kind == StringLit, "plugin symbol hint must be a string"
    result.hint = pool.strings[n.litId]
    inc n

    assert n.kind == StringLit, "plugin symbol kind must be a string"
    let tagId = pool.tags.getOrIncl(pool.strings[n.litId])
    assert uint32(tagId) <= uint32(high(TagEnum)),
      "unknown plugin symbol kind"
    let tag = cast[TagEnum](tagId)
    assert rawTagIsNimonySym(tag), "plugin symbol kind must be a NimonySym"
    result.kind = cast[SymKind](tag)
    inc n

    assert not n.hasMore, "unexpected data in plugin symbol marker"

proc resolveGeneratedSymbols(c: var SemContext; dest: var TokenBuf;
                             n: var Cursor;
                             symbols: var Table[int, GeneratedSymbol]) =
  if n.kind == ParLe and rawTagIsPluginKind(n.tagEnum):
    let info = n.info
    let isDefinition = n.tagEnum == PluginSymDefTagId
    let marker = readGeneratedSymbol(n)

    var generated = symbols.getOrDefault(marker.slot)
    if generated.name == SymId(0):
      generated = GeneratedSymbol(
        name: identToSym(c, marker.hint, marker.kind),
        hint: marker.hint,
        kind: marker.kind)
      symbols[marker.slot] = generated
      c.freshSyms.incl generated.name
    else:
      assert generated.hint == marker.hint and generated.kind == marker.kind,
        "inconsistent plugin symbol marker"

    if isDefinition:
      dest.addSymDef generated.name, info
    else:
      dest.addSymUse generated.name, info
  elif n.kind == ParLe:
    dest.add n
    n.into:
      while n.hasMore:
        resolveGeneratedSymbols(c, dest, n, symbols)
    dest.addParRi()
  else:
    dest.add n
    inc n

proc runPlugin*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo;
                pluginName: string; input: string; additionalInput = "") =
  var output = createTokenBuf(300)
  runPluginOutput(c, output, info, pluginName, input, additionalInput)

  var symbols = initTable[int, GeneratedSymbol]()
  var n = beginRead(output)
  resolveGeneratedSymbols(c, dest, n, symbols)

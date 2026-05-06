#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## nifprims-based variant of nifchecksums. The checksum stream still gets
## one `)` per open tag — virtual closes are synthesised after `into`'s body
## so the hash matches the wire-format text.

import std / [formatfloat]

import bitabs, nifreader, nifprims, lineinfos

when defined(nimony):
  import std / sha1
else:
  import "$nim"/dist/checksums/src/checksums/sha1

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

let SyntheticParRi = parRiToken(NoLineInfo)
  ## Fed to `update` whenever a virtual close fires, so the checksum keeps
  ## one `)` per open tag whether the in-memory representation has a real
  ## ParRi token or not.

proc updateOne(dest: var Sha1State; c: var Cursor; inlineT: TagId; foundInline: var bool) =
  ## Walks one token or subtree at `c`, feeding every token (incl. synthetic
  ## ParRi) to `update` and noting any inline tag.
  if c.kind != ParLe:
    update dest, c.load
    inc c
    return
  if c.tagId == inlineT: foundInline = true
  update dest, c.load
  c.into:
    while c.hasMore:
      updateOne(dest, c, inlineT, foundInline)
  update dest, SyntheticParRi

proc updateLoop*(dest: var Sha1State; n: var Cursor; inlineT: TagId; foundInline: var bool) =
  updateOne(dest, n, inlineT, foundInline)

proc computeChecksum*(n: Cursor): string =
  var checksum = newSha1State()
  var c = n
  var unused: bool = false
  updateOne(checksum, c, TagId(0), unused)
  let final = SecureHash checksum.finalize()
  result = $final

proc computeChecksum*(s: string): string =
  var state = newSha1State()
  state.update(s)
  $SecureHash(state.finalize())

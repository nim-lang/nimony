#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [formatfloat]

import bitabs, nifreader, nifstreams, nifcursors

when defined(nimony):
  import std / sha1
else:
  # `std/sha1` is `{.deprecated.}` in favor of the `checksums` package, but it is
  # the only SHA-1 module guaranteed to ship in `lib/std` on every Nim 2.x and
  # devel install. The previous `$nim/dist/checksums/...` path only exists in a
  # git/devel layout, so a released Nim 2 (as the docs require) failed with
  # "cannot open file". It is the same SHA-1 fork, so digests are identical.
  {.push warning[Deprecated]: off.}
  import std / sha1
  {.pop.}

proc updateAtom(dest: var Sha1State; n: Cursor) =
  ## Hashes one leaf token (build-agnostic: the hashed content is the tag/sym
  ## name, string, or numeric value, so the digest matches classic byte-for-byte).
  if n.isSymbolDef:
    update(dest, " :"); update(dest, pool.syms[n.symId])
  elif n.isSymbol:
    update(dest, " "); update(dest, pool.syms[n.symId])
  elif n.isIdent:
    update(dest, " "); update(dest, pool.strings[n.litId])
  elif n.isIntLit:
    update(dest, " "); update(dest, $pool.integers[n.intId])
  elif n.isUIntLit:
    update(dest, " "); update(dest, $pool.uintegers[n.uintId])
  elif n.isFloatLit:
    update(dest, " "); update(dest, $pool.floats[n.floatId])
  elif n.isStringLit:
    update(dest, " "); update(dest, pool.strings[n.litId])
  elif n.isCharLit:
    update(dest, " "); update(dest, $n.uoperand)
  elif n.isDotToken:
    update(dest, ".")

proc updateLoop*(dest: var Sha1State; n: var Cursor; inlineT: TagId; foundInline: var bool) =
  ## Hashes the single tree/token at `n`, advancing past it. The digest is
  ## the same with and without ParRi elision: the close is hashed as ")"
  ## whether or not its token physically exists.
  if n.isTagLit:
    update(dest, "(")
    update(dest, pool.tags[n.tagId])
    if n.tagId == inlineT: foundInline = true
    n.into:
      while n.hasMore:
        updateLoop(dest, n, inlineT, foundInline)
    update dest, ")"
  else:
    updateAtom(dest, n)
    inc n

const
  NoInlineTag = TagId(high(uint32)) ## never matches a real tag

proc computeChecksum*(n: Cursor): string =
  var checksum = newSha1State()
  var n = n
  var dummy = false
  updateLoop(checksum, n, NoInlineTag, dummy)
  let final = SecureHash checksum.finalize()
  result = $final

proc computeChecksum*(s: string): string =
  var state = newSha1State()
  state.update(s)
  $SecureHash(state.finalize())

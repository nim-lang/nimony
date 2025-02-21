#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [formatfloat]

import bitabs, nifreader, nifstreams, nifcursors

#import std / [sha1]
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

proc updateLoop*(dest: var Sha1State; n: var Cursor; inlineT: TagId; foundInline: var bool) =
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

proc computeChecksum*(n: Cursor): string =
  var checksum = newSha1State()
  var n = n
  var nested = 0
  while true:
    update checksum, n.load
    case n.kind
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else: discard
    if nested <= 0:
      break
    inc n
  let final = SecureHash checksum.finalize()
  result = $final

proc computeChecksum*(s: string): string =
  var state = newSha1State()
  state.update(s)
  $SecureHash(state.finalize())

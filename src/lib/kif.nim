#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## KIF reader/writer. Achieves a compression ratio of 3x for system.nim.

import std / [syncio, hashes, tables, varints, assertions, formatfloat, strutils, os]
import nifreader, symparser

const
  MagicCookie = [0x00'u8, 0xCE, 0xFC, 0xEF]

proc writeVu64*(f: File; x: uint64) =
  var buf = default array[9, byte]
  let size = writeVu64(buf, x)
  let written = f.writeBuffer(addr buf, size)
  if written != size:
    quit "Failed to write varint"

type
  KifTokenKind* = enum
    ParLeKif, ParRiKif,
    DotKif,
    SymbolKif, SymbolDefKif, SymbolDot,
    StringLitKif, CharLitKif, IntLitKif, UIntLitKif, FloatLitKif, IdentKif,
    LineInfoKif, UnknownKif

  Key* = (KifTokenKind, string)

static:
  assert high(KifTokenKind).int < 16

proc writeString*(outp: File; tab: var OrderedTable[Key, uint64]; s: Key) =
  var idx = tab.getOrDefault(s)
  if idx == 0'u64 and not hasKey(tab, s):
    idx = uint64(tab.len)
    tab[s] = idx
  writeVu64(outp, idx)

proc writeSymbol*(outp: File; tab: var OrderedTable[Key, uint64]; s: Key) =
  var b = s[1]
  let m = extractModule(b)
  if m.len > 0:
    var x = removeModule s[1]
    writeString(outp, tab, (s[0], x))
    writeString(outp, tab, (SymbolDot, m))
  else:
    writeString(outp, tab, s)

proc main =
  var r = nifreader.open("nimcache/sysvq0asl.2.nif")
  discard processDirectives(r)
  var tab = initOrderedTable[Key, uint64]()
  var outp = default File
  if not outp.open("sys.kif", fmWrite):
    r.close()

  discard outp.writeBytes(MagicCookie, 0, 4)
  discard outp.writeBytes(MagicCookie, 0, 4) # will be overwritten with the offset of the table
  var pendingParRi = 0
  while true:
    var tok = r.next()

    if tok.filename.len != 0:
      writeString(outp, tab, (LineInfoKif, $tok.pos.col))
      writeString(outp, tab, (LineInfoKif, $tok.pos.line))
      writeString(outp, tab, (LineInfoKif, decodeFilename(tok)))
    elif tok.pos.line != 0:
      writeString(outp, tab, (LineInfoKif, $tok.pos.col))
      writeString(outp, tab, (LineInfoKif, $tok.pos.line))
    elif tok.pos.col != 0:
      writeString(outp, tab, (LineInfoKif, $tok.pos.col))

    if tok.tk != ParRi:
      if pendingParRi > 0:
        let s = repeat(")", pendingParRi)
        writeString(outp, tab, (ParRiKif, s))
        pendingParRi = 0

    case tok.tk
    of EofToken:
      break
    of UnknownToken:
      writeString(outp, tab, (UnknownKif, ""))
    of Symbol:
      writeSymbol(outp, tab, (SymbolKif, decodeStr(tok)))
    of SymbolDef:
      writeSymbol(outp, tab, (SymbolDefKif, decodeStr(tok)))
    of StringLit:
      writeString(outp, tab, (StringLitKif, decodeStr(tok)))
    of CharLit:
      writeString(outp, tab, (CharLitKif, $decodeChar(tok)))
    of IntLit:
      writeString(outp, tab, (IntLitKif, decodeStr(tok)))
    of UIntLit:
      writeString(outp, tab, (UIntLitKif, decodeStr(tok)))
    of FloatLit:
      writeString(outp, tab, (FloatLitKif, decodeStr(tok)))
    of ParLe:
      writeString(outp, tab, (ParLeKif, decodeStr(tok)))
    of ParRi:
      #writeString(outp, tab, (ParRiKif, ""))
      inc pendingParRi
    of DotToken:
      writeString(outp, tab, (DotKif, ""))
    of Ident:
      writeString(outp, tab, (IdentKif, decodeStr(tok)))

  if pendingParRi > 0:
    let s = repeat(")", pendingParRi)
    writeString(outp, tab, (ParRiKif, s))

  # write the table
  var pos = 0'u64
  for k, v in tab.pairs:
    assert pos == v
    inc pos
    var prefix = uint64(k[0]) or uint64(k[1].len shl 4)
    writeVu64(outp, prefix)
    write(outp, k[1])

  close(outp)
  r.close()

main()

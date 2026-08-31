#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / assertions
include ".." / lib / nifprelude
include ".." / lib / compat2

import ".." / models / [tags, finalir_tags]
export finalir_tags

template tagEnum(c: Cursor): TagEnum = cast[TagEnum](cursorTagId(c))

template tagEnum(c: NifToken): TagEnum = cast[TagEnum](tagId(c))

proc finalIrKind*(c: NifToken): FinalIrKind {.inline.} =
  if c.isTagLit and rawTagIsFinalIrKind(tagEnum(c)):
    result = cast[FinalIrKind](tagEnum(c))
  else:
    result = NoVTag

proc finalIrKind*(c: Cursor): FinalIrKind {.inline.} =
  result = finalIrKind(c.load())

proc addParLe*(dest: var TokenBuf; kind: FinalIrKind; info = NoLineInfo) =
  dest.addParLe(cast[TagId](uint32(ord(kind))), info)

template copyIntoKind*(dest: var TokenBuf; kind: FinalIrKind;
                       info: NifLineInfo; body: untyped) =
  dest.addParLe(kind, info)
  body
  dest.addParRi()

#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / assertions
include ".." / lib / nifprelude

import ".." / models / [tags, njvl_tags]
export njvl_tags

template tagEnum(c: Cursor): TagEnum = cast[TagEnum](tag(c))

template tagEnum(c: PackedToken): TagEnum = cast[TagEnum](tag(c))

proc njvlKind*(c: PackedToken): NjvlKind {.inline.} =
  if c.kind == ParLe and rawTagIsNjvlKind(tagEnum(c)):
    result = cast[NjvlKind](tagEnum(c))
  else:
    result = NoVTag

proc njvlKind*(c: Cursor): NjvlKind {.inline.} =
  result = njvlKind(c.load())

proc parLeToken*(kind: NjvlKind; info = NoLineInfo): PackedToken =
  parLeToken(cast[TagId](kind), info)

template copyIntoKind*(dest: var TokenBuf; kind: NjvlKind;
                       info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(kind, info)
  body
  dest.addParRi()

#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

# For the line information we use 32 bits. They are used as follows:
# Bit 0 (AsideBit): If we have inline line information or not.
# If inline line information is not used bit 1 is 0 and
# remaining 30 bits are used as an index into a seq[(FileId, int, int)].
# If bit 1 is 1 then the remaining 30 bits are can store an arbitrary payload.
# This "payload" mechanism is used to attach a token/node to its original index.
# It is used to combine a control flow graph with its original tree-like structure.
#
# We use 10 bits for the "file ID", this means a program can consist of as much
# as 1024 different files. (If it uses more files than that, the overflow bit
# would be set.)
# This means we have 21 bits left to encode the (line, col) pair. We use 7 bits for the column
# so 128 is the limit and 14 bits for the line number.
# The packed representation supports files with up to 16384 lines.
# Keep in mind that whenever any limit is reached the AsideBit is set and the real line
# information is kept in a side channel.

import std / assertions

const
  AsideBit = 1
  FileBits = 10
  LineBits = 14
  ColBits = 7
  FileMax = (1 shl FileBits) - 1
  LineMax = (1 shl LineBits) - 1
  ColMax = (1 shl ColBits) - 1

static:
  assert AsideBit + FileBits + LineBits + ColBits == 32

type
  PackedLineInfo* = distinct uint32

  FileId* = distinct uint32

proc `==`*(a, b: FileId): bool {.borrow.}
proc `==`*(a, b: PackedLineInfo): bool {.borrow.}

type
  LineInfoUnpacked* = object
    file*: FileId
    line*: int32
    col*: int32

  LineInfoManager* = object
    aside: seq[LineInfoUnpacked]

const
  NoLineInfo* = PackedLineInfo(0'u32)
  NoFile* = FileId(0'u32)

proc isValid*(x: PackedLineInfo): bool {.inline.} = uint32(x) != uint32(NoLineInfo)
proc isValid*(x: FileId): bool {.inline.} = x != NoFile

proc pack*(m: var LineInfoManager; file: FileId; line, col: int32): PackedLineInfo =
  if file.uint32 <= FileMax.uint32 and line <= LineMax and col <= ColMax:
    let col = if col < 0'i32: 0'u32 else: col.uint32
    let line = if line < 0'i32: 0'u32 else: line.uint32
    # use inline representation:
    result = PackedLineInfo((file.uint32 shl 1'u32) or (line shl uint32(AsideBit + FileBits)) or
      (col shl uint32(AsideBit + FileBits + LineBits)))
  else:
    result = PackedLineInfo((m.aside.len shl 2) or AsideBit)
    m.aside.add LineInfoUnpacked(file: file, line: line, col: col)

proc isPayload*(i: PackedLineInfo): bool {.inline.} =
  result = (i.uint32 and 3'u32) == 3'u32

proc unpack*(m: LineInfoManager; info: PackedLineInfo): LineInfoUnpacked =
  let i = info.uint32
  if (i and 1'u32) == 0'u32:
    # inline representation:
    result = LineInfoUnpacked(file: FileId((i shr 1'u32) and FileMax.uint32),
      line: int32((i shr uint32(AsideBit + FileBits)) and LineMax.uint32),
      col: int32((i shr uint32(AsideBit + FileBits + LineBits)) and ColMax.uint32))
  elif not isPayload(info):
    result = m.aside[int(i shr 2'u32)]
  else:
    result = LineInfoUnpacked(file: NoFile)

proc getPayload*(i: PackedLineInfo): uint32 {.inline.} =
  assert isPayload(i)
  result = i.uint32 shr 2'u32

proc toPayload*(val: uint32): PackedLineInfo {.inline.} =
  result = PackedLineInfo((val shl 2'u32) or 3'u32)

proc getFileId*(m: LineInfoManager; i: PackedLineInfo): FileId =
  result = unpack(m, i).file

proc memSize*(m: LineInfoManager): int = m.aside.len

when isMainModule:
  var m = LineInfoManager(aside: @[])
  for i in 0'i32..<16388'i32:
    for col in 0'i32..<100'i32:
      let packed = pack(m, FileId(1023), i, col)
      assert(not isPayload(packed))
      let u = unpack(m, packed)
      assert u.file == FileId(1023)
      assert u.line == i
      assert u.col == col
  echo m.aside.len

  let i = toPayload(8000u32)
  assert isPayload(i)
  assert getPayload(i) == 8000u32

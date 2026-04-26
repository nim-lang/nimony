#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

{.feature: "lenientnils".}

## This module provides a stream interface and two implementations thereof:
## the `FileStream <#FileStream>`_ and the `StringStream <#StringStream>`_
## which implement the stream interface for Nim file objects (`File`) and
## strings.
##
## Other modules may provide other implementations for this standard
## stream interface.
##
## .. warning:: Due to the use of `pointer`, the `readData`, `peekData` and
## `writeData` interfaces are not available on the compile-time VM, and must
## be cast from a `ptr string` on the JS backend. However, `readDataStr` is
## available generally in place of `readData`.

import syncio
import assertions
export FileMode

template isNil(s: typed): bool {.untyped.} =
  s == nil

type
  Stream* = ref StreamObj
    ## All procedures of this module use this type.
    ## Procedures don't directly use `StreamObj <#StreamObj>`_.
  StreamObj* = object of RootObj
    ## Stream interface that supports writing or reading.
    ##
    ## **Note:**
    ## * That these fields here shouldn't be used directly.
    ##   They are accessible so that a stream implementation can override them.
    closeImpl*: nil proc (s: Stream) {.nimcall, raises, tags: [WriteIOEffect].}
    atEndImpl*: nil proc (s: Stream): bool {.nimcall, raises, tags: [].}
    setPositionImpl*: nil proc (s: Stream; pos: int) {.nimcall, raises, tags: [].}
    getPositionImpl*: nil proc (s: Stream): int {.nimcall, raises, tags: [].}

    readDataStrImpl*: nil proc (s: Stream; buffer: var string;
                                slice: Slice[int]): int
      {.nimcall, raises, tags: [ReadIOEffect].}

    readLineImpl*: nil proc (s: Stream; line: var string): bool
      {.nimcall, raises, tags: [ReadIOEffect].}

    readDataImpl*: nil proc (s: Stream; buffer: pointer; bufLen: int): int
      {.nimcall, raises, tags: [ReadIOEffect].}
    peekDataImpl*: nil proc (s: Stream; buffer: pointer; bufLen: int): int
      {.nimcall, raises, tags: [ReadIOEffect].}
    writeDataImpl*: nil proc (s: Stream; buffer: pointer; bufLen: int)
      {.nimcall, raises, tags: [WriteIOEffect].}

    flushImpl*: nil proc (s: Stream) {.nimcall, raises, tags: [WriteIOEffect].}

proc flush*(s: Stream) {.raises.} =
  ## Flushes the buffers that the stream `s` might use.
  if not isNil(s.flushImpl): s.flushImpl(s)

proc close*(s: Stream) {.raises.} =
  ## Closes the stream `s`.
  if not isNil(s) and not isNil(s.closeImpl):
    s.closeImpl(s)

proc atEnd*(s: Stream): bool {.raises.} =
  ## Checks if more data can be read from `s`. Returns `true` if all data has
  ## been read.
  result = s.atEndImpl(s)

proc setPosition*(s: Stream; pos: int) {.raises.} =
  ## Sets the position `pos` of the stream `s`.
  s.setPositionImpl(s, pos)

proc getPosition*(s: Stream): int {.raises.} =
  ## Retrieves the current position in the stream `s`.
  result = s.getPositionImpl(s)

proc readData*(s: Stream; buffer: pointer; bufLen: int): int {.raises.} =
  ## Low level proc that reads data into an untyped `buffer` of `bufLen` size.
  result = s.readDataImpl(s, buffer, bufLen)

proc readDataStr*(s: Stream; buffer: var string;
                  slice: Slice[int]): int {.raises.} =
  ## Low level proc that reads data into a string `buffer` at `slice`.
  if not isNil(s.readDataStrImpl):
    result = s.readDataStrImpl(s, buffer, slice)
  else:
    # fallback via readData
    let length = slice.b + 1 - slice.a
    if length <= 0:
      result = 0
    else:
      let p = beginStore(buffer, slice.b + 1, slice.a)
      result = s.readDataImpl(s, p, length)
      endStore(buffer)

proc readAll*(s: Stream): string {.raises.} =
  ## Reads all available data.
  const bufferSize = 1024
  result = ""
  var buffer {.noinit.}: array[bufferSize, char]
  while true:
    let readBytes = readData(s, addr buffer[0], bufferSize)
    if readBytes == 0:
      break
    let prevLen = result.len
    let dest = beginStore(result, prevLen + readBytes, prevLen)
    copyMem(dest, addr buffer[0], readBytes)
    endStore(result)
    if readBytes < bufferSize:
      break

proc peekData*(s: Stream; buffer: pointer; bufLen: int): int {.raises.} =
  ## Low level proc that reads data into an untyped `buffer` of `bufLen` size
  ## without moving stream position.
  result = s.peekDataImpl(s, buffer, bufLen)

proc writeData*(s: Stream; buffer: pointer; bufLen: int) {.raises.} =
  ## Low level proc that writes an untyped `buffer` of `bufLen` size
  ## to the stream `s`.
  s.writeDataImpl(s, buffer, bufLen)

proc write*[T](s: Stream; x: T) {.raises.} =
  ## Generic write procedure. Writes `x` to the stream `s`.
  var y = x
  writeData(s, addr y, sizeof(T))

proc write*(s: Stream; x: string) {.raises.} =
  ## Writes the string `x` to the stream `s`. No length field or
  ## terminating zero is written.
  if x.len > 0:
    var y = x
    writeData(s, toCString(y), x.len)

proc writeLine*(s: Stream; x: string) {.raises.} =
  ## Writes a string followed by a newline to the stream `s`.
  write(s, x)
  write(s, "\n")

proc read*[T](s: Stream; result: var T) {.raises.} =
  ## Generic read procedure. Reads `result` from the stream `s`.
  if readData(s, addr result, sizeof(T)) != sizeof(T):
    raise IOError

proc peek*[T](s: Stream; result: var T) {.raises.} =
  ## Generic peek procedure. Peeks `result` from the stream `s`.
  if peekData(s, addr result, sizeof(T)) != sizeof(T):
    raise IOError

proc readChar*(s: Stream): char {.raises.} =
  ## Reads a char from the stream `s`.
  ## Returns '\0' as an EOF marker.
  result = '\0'
  if readData(s, addr result, sizeof(result)) != 1:
    result = '\0'

proc peekChar*(s: Stream): char {.raises.} =
  ## Peeks a char from the stream `s`.
  ## Returns '\0' as an EOF marker.
  result = '\0'
  if peekData(s, addr result, sizeof(result)) != 1:
    result = '\0'

proc readBool*(s: Stream): bool {.raises.} =
  ## Reads a bool from the stream `s`.
  var t: uint8 = 0'u8
  read(s, t)
  result = t != 0'u8

proc peekBool*(s: Stream): bool {.raises.} =
  ## Peeks a bool from the stream `s`.
  var t: uint8 = 0'u8
  peek(s, t)
  result = t != 0'u8

proc readInt8*(s: Stream): int8 {.raises.} =
  ## Reads an int8 from the stream `s`.
  result = 0'i8
  read(s, result)

proc peekInt8*(s: Stream): int8 {.raises.} =
  ## Peeks an int8 from the stream `s`.
  result = 0'i8
  peek(s, result)

proc readInt16*(s: Stream): int16 {.raises.} =
  ## Reads an int16 from the stream `s`.
  result = 0'i16
  read(s, result)

proc peekInt16*(s: Stream): int16 {.raises.} =
  ## Peeks an int16 from the stream `s`.
  result = 0'i16
  peek(s, result)

proc readInt32*(s: Stream): int32 {.raises.} =
  ## Reads an int32 from the stream `s`.
  result = 0'i32
  read(s, result)

proc peekInt32*(s: Stream): int32 {.raises.} =
  ## Peeks an int32 from the stream `s`.
  result = 0'i32
  peek(s, result)

proc readInt64*(s: Stream): int64 {.raises.} =
  ## Reads an int64 from the stream `s`.
  result = 0'i64
  read(s, result)

proc peekInt64*(s: Stream): int64 {.raises.} =
  ## Peeks an int64 from the stream `s`.
  result = 0'i64
  peek(s, result)

proc readUint8*(s: Stream): uint8 {.raises.} =
  ## Reads a uint8 from the stream `s`.
  result = 0'u8
  read(s, result)

proc peekUint8*(s: Stream): uint8 {.raises.} =
  ## Peeks a uint8 from the stream `s`.
  result = 0'u8
  peek(s, result)

proc readUint16*(s: Stream): uint16 {.raises.} =
  ## Reads a uint16 from the stream `s`.
  result = 0'u16
  read(s, result)

proc peekUint16*(s: Stream): uint16 {.raises.} =
  ## Peeks a uint16 from the stream `s`.
  result = 0'u16
  peek(s, result)

proc readUint32*(s: Stream): uint32 {.raises.} =
  ## Reads a uint32 from the stream `s`.
  result = 0'u32
  read(s, result)

proc peekUint32*(s: Stream): uint32 {.raises.} =
  ## Peeks a uint32 from the stream `s`.
  result = 0'u32
  peek(s, result)

proc readUint64*(s: Stream): uint64 {.raises.} =
  ## Reads a uint64 from the stream `s`.
  result = 0'u64
  read(s, result)

proc peekUint64*(s: Stream): uint64 {.raises.} =
  ## Peeks a uint64 from the stream `s`.
  result = 0'u64
  peek(s, result)

proc readFloat32*(s: Stream): float32 {.raises.} =
  ## Reads a float32 from the stream `s`.
  result = 0.0'f32
  read(s, result)

proc peekFloat32*(s: Stream): float32 {.raises.} =
  ## Peeks a float32 from the stream `s`.
  result = 0.0'f32
  peek(s, result)

proc readFloat64*(s: Stream): float64 {.raises.} =
  ## Reads a float64 from the stream `s`.
  result = 0.0'f64
  read(s, result)

proc peekFloat64*(s: Stream): float64 {.raises.} =
  ## Peeks a float64 from the stream `s`.
  result = 0.0'f64
  peek(s, result)

proc readStrPrivate(s: Stream; length: int; str: var string) {.raises.} =
  if length > str.len: setLen(str, length)
  let L = readData(s, toCString(str), length)
  if L != str.len: setLen(str, L)

proc readStr*(s: Stream; length: int; str: var string) {.raises.} =
  ## Reads a string of length `length` from the stream `s`.
  readStrPrivate(s, length, str)

proc readStr*(s: Stream; length: int): string {.raises.} =
  ## Reads a string of length `length` from the stream `s`.
  result = newString(length)
  readStrPrivate(s, length, result)

proc peekStrPrivate(s: Stream; length: int; str: var string) {.raises.} =
  if length > str.len: setLen(str, length)
  let L = peekData(s, toCString(str), length)
  if L != str.len: setLen(str, L)

proc peekStr*(s: Stream; length: int; str: var string) {.raises.} =
  ## Peeks a string of length `length` from the stream `s`.
  peekStrPrivate(s, length, str)

proc peekStr*(s: Stream; length: int): string {.raises.} =
  ## Peeks a string of length `length` from the stream `s`.
  result = newString(length)
  peekStrPrivate(s, length, result)

proc readLine*(s: Stream; line: var string): bool {.raises.} =
  ## Reads a line of text from the stream `s` into `line`. A line of text may
  ## be delimited by `LF` or `CRLF`. The newline character(s) are not part of
  ## the returned string. Returns `false` if the end of the file has been
  ## reached, `true` otherwise.
  if not isNil(s.readLineImpl):
    result = s.readLineImpl(s, line)
  else:
    # fallback: build a line out of single-char reads
    line.setLen 0
    while true:
      var c: char = '\0'
      let got = s.readDataImpl(s, addr c, 1)
      if got != 1:
        if line.len > 0: break
        else: return false
      if c == '\c':
        var nxt: char = '\0'
        discard s.readDataImpl(s, addr nxt, 1)
        break
      elif c == '\L': break
      elif c == '\0':
        if line.len > 0: break
        else: return false
      line.add c
    result = true

proc peekLine*(s: Stream; line: var string): bool {.raises.} =
  ## Peeks a line of text from the stream `s` into `line`.
  let pos = getPosition(s)
  try:
    result = readLine(s, line)
  finally:
    setPosition(s, pos)

proc readLine*(s: Stream): string {.raises.} =
  ## Reads a line from a stream `s`.
  result = ""
  if s.atEnd:
    raise EndOfStreamError
  while true:
    var c = readChar(s)
    if c == '\c':
      c = readChar(s)
      break
    if c == '\L' or c == '\0':
      break
    else:
      result.add c

proc peekLine*(s: Stream): string {.raises.} =
  ## Peeks a line from a stream `s`.
  let pos = getPosition(s)
  try:
    result = readLine(s)
  finally:
    setPosition(s, pos)

iterator lines*(s: Stream): string {.sideEffect, raises.} =
  ## Iterates over every line in the stream.
  var line: string = ""
  while s.readLine(line):
    yield line

# -------------------- StringStream --------------------

type
  StringStream* = ref StringStreamObj
    ## A stream that encapsulates a string.
  StringStreamObj* = object of StreamObj
    ## A string stream object.
    data*: string ## The underlying string data.
                  ## This is updated when calling `writeLine` etc.
    pos: int

proc ssAtEnd(s: Stream): bool {.nimcall, raises, tags: [].} =
  var ss = StringStream(s)
  result = ss.pos >= ss.data.len

proc ssSetPosition(s: Stream; pos: int) {.nimcall, raises, tags: [].} =
  var ss = StringStream(s)
  if pos < 0:
    ss.pos = 0
  elif pos > ss.data.len:
    ss.pos = ss.data.len
  else:
    ss.pos = pos

proc ssGetPosition(s: Stream): int {.nimcall, raises, tags: [].} =
  var ss = StringStream(s)
  result = ss.pos

proc ssReadDataStr(s: Stream; buffer: var string;
                   slice: Slice[int]): int {.nimcall, raises,
                                             tags: [ReadIOEffect].} =
  var ss = StringStream(s)
  let avail = ss.data.len - ss.pos
  let want = slice.b + 1 - slice.a
  result = if want < avail: want else: avail
  if result > 0:
    let dest = beginStore(buffer, slice.a + result, slice.a)
    copyMem(dest, readRawData(ss.data, ss.pos), result)
    endStore(buffer)
    inc ss.pos, result
  else:
    result = 0

proc ssReadData(s: Stream; buffer: pointer;
                bufLen: int): int {.nimcall, raises,
                                    tags: [ReadIOEffect].} =
  var ss = StringStream(s)
  let avail = ss.data.len - ss.pos
  result = if bufLen < avail: bufLen else: avail
  if result > 0:
    copyMem(buffer, readRawData(ss.data, ss.pos), result)
    inc ss.pos, result
  else:
    result = 0

proc ssPeekData(s: Stream; buffer: pointer;
                bufLen: int): int {.nimcall, raises,
                                    tags: [ReadIOEffect].} =
  var ss = StringStream(s)
  let avail = ss.data.len - ss.pos
  result = if bufLen < avail: bufLen else: avail
  if result > 0:
    copyMem(buffer, readRawData(ss.data, ss.pos), result)
  else:
    result = 0

proc ssWriteData(s: Stream; buffer: pointer;
                 bufLen: int) {.nimcall, raises,
                                tags: [WriteIOEffect].} =
  var ss = StringStream(s)
  if bufLen <= 0:
    return
  let dest = beginStore(ss.data, ss.pos + bufLen, ss.pos)
  copyMem(dest, buffer, bufLen)
  endStore(ss.data)
  inc ss.pos, bufLen

proc ssClose(s: Stream) {.nimcall, raises,
                          tags: [WriteIOEffect].} =
  var ss = StringStream(s)
  ss.data = ""

proc newStringStream*(s: sink string = ""): StringStream =
  ## Creates a new stream from the string `s`.
  result = StringStream(data: s, pos: 0,
    closeImpl: ssClose,
    atEndImpl: ssAtEnd,
    setPositionImpl: ssSetPosition,
    getPositionImpl: ssGetPosition,
    readDataStrImpl: ssReadDataStr,
    readDataImpl: ssReadData,
    peekDataImpl: ssPeekData,
    writeDataImpl: ssWriteData)

# -------------------- FileStream --------------------

type
  FileStream* = ref FileStreamObj
    ## A stream that encapsulates a `File`.
  FileStreamObj* = object of StreamObj
    ## A file stream object.
    f: File

proc fsClose(s: Stream) {.nimcall, raises,
                          tags: [WriteIOEffect].} =
  var fs = FileStream(s)
  if fs.f != nil:
    close(fs.f)
    fs.f = nil

proc fsFlush(s: Stream) {.nimcall, raises,
                          tags: [WriteIOEffect].} =
  flushFile(FileStream(s).f)

proc fsAtEnd(s: Stream): bool {.nimcall, raises,
                                tags: [].} =
  result = endOfFile(FileStream(s).f)

proc fsSetPosition(s: Stream; pos: int) {.nimcall, raises,
                                          tags: [].} =
  setFilePos(FileStream(s).f, pos.int64)

proc fsGetPosition(s: Stream): int {.nimcall, raises,
                                     tags: [].} =
  result = int(getFilePos(FileStream(s).f))

proc fsReadData(s: Stream; buffer: pointer;
                bufLen: int): int {.nimcall, raises,
                                    tags: [ReadIOEffect].} =
  result = readBuffer(FileStream(s).f, buffer, bufLen)

proc fsReadDataStr(s: Stream; buffer: var string;
                   slice: Slice[int]): int {.nimcall, raises,
                                             tags: [ReadIOEffect].} =
  let length = slice.b + 1 - slice.a
  if length <= 0:
    result = 0
  else:
    let p = beginStore(buffer, slice.b + 1, slice.a)
    result = readBuffer(FileStream(s).f, p, length)
    endStore(buffer)

proc fsPeekData(s: Stream; buffer: pointer;
                bufLen: int): int {.nimcall, raises,
                                    tags: [ReadIOEffect].} =
  let pos = fsGetPosition(s)
  result = readBuffer(FileStream(s).f, buffer, bufLen)
  fsSetPosition(s, pos)

proc fsWriteData(s: Stream; buffer: pointer;
                 bufLen: int) {.nimcall, raises,
                                tags: [WriteIOEffect].} =
  if writeBuffer(FileStream(s).f, buffer, bufLen) != bufLen:
    raise IOError

proc fsReadLine(s: Stream; line: var string): bool {.nimcall, raises,
                                                     tags: [ReadIOEffect].} =
  result = readLine(FileStream(s).f, line)

proc newFileStream*(f: File): FileStream =
  ## Creates a new stream from the file `f`.
  result = FileStream(
    f: f,
    closeImpl: fsClose,
    atEndImpl: fsAtEnd,
    setPositionImpl: fsSetPosition,
    getPositionImpl: fsGetPosition,
    readDataStrImpl: fsReadDataStr,
    readDataImpl: fsReadData,
    readLineImpl: fsReadLine,
    peekDataImpl: fsPeekData,
    writeDataImpl: fsWriteData,
    flushImpl: fsFlush)

proc newFileStream*(filename: string; mode: FileMode = fmRead;
                    bufSize: int = -1): FileStream =
  ## Creates a new stream from the file named `filename` with the mode `mode`.
  ## If the file cannot be opened, `nil` is returned.
  var f: File
  if open(f, filename, mode, bufSize):
    result = newFileStream(f)
  else:
    result = nil

proc openFileStream*(filename: string; mode: FileMode = fmRead;
                     bufSize: int = -1): FileStream {.raises.} =
  ## Creates a new stream from the file named `filename` with the mode `mode`.
  ## If the file cannot be opened, an IO error is raised.
  var f: File
  if open(f, filename, mode, bufSize):
    result = newFileStream(f)
  else:
    raise IOError

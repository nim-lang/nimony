#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

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
##
## Basic usage
## ===========
##
## The basic flow of using this module is:
##
## 1. Open input stream
## 2. Read or write stream
## 3. Close stream
##
## StringStream example
## --------------------
##
##   ```Nim
##   import std/streams
##
##   var strm = newStringStream("""The first line
##   the second line
##   the third line""")
##
##   var line = ""
##
##   while strm.readLine(line):
##     echo line
##
##   # Output:
##   # The first line
##   # the second line
##   # the third line
##
##   strm.close()
##   ```
##
## FileStream example
## ------------------
##
## Write file stream example:
##
##   ```Nim
##   import std/streams
##
##   var strm = newFileStream("somefile.txt", fmWrite)
##   var line = ""
##
##   if not isNil(strm):
##     strm.writeLine("The first line")
##     strm.writeLine("the second line")
##     strm.writeLine("the third line")
##     strm.close()
##
##   # Output (somefile.txt):
##   # The first line
##   # the second line
##   # the third line
##   ```
##
## Read file stream example:
##
##   ```Nim
##   import std/streams
##
##   var strm = newFileStream("somefile.txt", fmRead)
##   var line = ""
##
##   if not isNil(strm):
##     while strm.readLine(line):
##       echo line
##     strm.close()
##
##   # Output:
##   # The first line
##   # the second line
##   # the third line
##   ```
##
## See also
## ========
## * `asyncstreams module <asyncstreams.html>`_
## * `io module <syncio.html>`_ for `FileMode enum <syncio.html#FileMode>`_

import std/syncio
export FileMode

# TODO: basic stuffs
#---------------------------------------
# import std/assertions

template isNil(s: typed): bool {.untyped.} =
  s == nil

type
  byte = uint8

proc clamp(x, a, b: int): int =
  ## Limits the value `x` within the interval \[a, b].
  ## This proc is equivalent to but faster than `max(a, min(b, x))`.
  ## 
  ## .. warning:: `a <= b` is assumed and will not be checked (currently).
  ##
  ## **See also:**
  ## `math.clamp` for a version that takes a `Slice[T]` instead.
  # runnableExamples:
  #   assert (1.4).clamp(0.0, 1.0) == 1.0
  #   assert (0.5).clamp(0.0, 1.0) == 0.5
  #   assert 4.clamp(1, 3) == max(1, min(3, 4))
  if x < a: return a
  if x > b: return b
  return x

#---------------------------------------


when defined(windows):
  import "../../vendor/errorcodes/src" / errorcodes_windows
else:
  import "../../vendor/errorcodes/src" / errorcodes_posix

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
    closeImpl*: proc (s: Stream)
      {.nimcall, raises: [IOError, OSError], tags: [WriteIOEffect].}
    atEndImpl*: proc (s: Stream): bool
      {.nimcall, raises: [Defect, IOError, OSError], tags: [].}
    setPositionImpl*: proc (s: Stream, pos: int)
      {.nimcall, raises: [Defect, IOError, OSError], tags: [].}
    getPositionImpl*: proc (s: Stream): int
      {.nimcall, raises: [Defect, IOError, OSError], tags: [].}

    readDataStrImpl*: proc (s: Stream, buffer: var string, slice: Slice[int]): int
      {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].}

    readLineImpl*: proc(s: Stream, line: var string): bool
      {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].}

    readDataImpl*: proc (s: Stream, buffer: pointer, bufLen: int): int
      {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].}
    peekDataImpl*: proc (s: Stream, buffer: pointer, bufLen: int): int
      {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].}
    writeDataImpl*: proc (s: Stream, buffer: pointer, bufLen: int)
      {.nimcall, raises: [Defect, IOError, OSError], tags: [WriteIOEffect].}

    flushImpl*: proc (s: Stream)
      {.nimcall, raises: [Defect, IOError, OSError], tags: [WriteIOEffect].}

proc flush*(s: Stream) {.raises: [Defect, IOError, OSError].} =
  ## Flushes the buffers that the stream `s` might use.
  ##
  ## This procedure causes any unwritten data for that stream to be delivered
  ## to the host environment to be written to the file.
  ##
  ## See also:
  ## * `close proc <#close,Stream>`_
  # when false: # runnableExamples:
  #   from std/os import removeFile

  #   var strm = newFileStream("somefile.txt", fmWrite)

  #   assert "Before write:" & readFile("somefile.txt") == "Before write:"
  #   strm.write("hello")
  #   assert "After  write:" & readFile("somefile.txt") == "After  write:"

  #   strm.flush()
  #   assert "After  flush:" & readFile("somefile.txt") == "After  flush:hello"
  #   strm.write("HELLO")
  #   strm.flush()
  #   assert "After  flush:" & readFile("somefile.txt") == "After  flush:helloHELLO"

    # strm.close()
    # assert "After  close:" & readFile("somefile.txt") == "After  close:helloHELLO"
    # removeFile("somefile.txt")

  if not isNil(s.flushImpl): s.flushImpl(s)

proc close*(s: Stream) {.raises: [IOError, OSError].} =
  ## Closes the stream `s`.
  ##
  ## See also:
  ## * `flush proc <#flush,Stream>`_
  when false: # runnableExamples:
    block:
      let strm = newStringStream("The first line\nthe second line\nthe third line")
      ## do something...
      strm.close()

    block:
      let strm = newFileStream("amissingfile.txt")
      # deferring works even if newFileStream fails
      defer: strm.close()
      if not isNil(strm):
        ## do something...

  if not isNil(s) and not isNil(s.closeImpl):
    s.closeImpl(s)

proc atEnd*(s: Stream): bool {.raises: [Defect, IOError, OSError].} =
  ## Checks if more data can be read from `s`. Returns ``true`` if all data has
  ## been read.
  when false: # runnableExamples:
    var strm = newStringStream("The first line\nthe second line\nthe third line")
    var line = ""
    assert strm.atEnd() == false
    while strm.readLine(line):
      discard
    assert strm.atEnd() == true
    strm.close()

  result = s.atEndImpl(s)

proc setPosition*(s: Stream, pos: int) {.raises: [Defect, IOError, OSError].} =
  ## Sets the position `pos` of the stream `s`.
  when false: # runnableExamples:
    var strm = newStringStream("The first line\nthe second line\nthe third line")
    strm.setPosition(4)
    assert strm.readLine() == "first line"
    strm.setPosition(0)
    assert strm.readLine() == "The first line"
    strm.close()

  s.setPositionImpl(s, pos)

proc getPosition*(s: Stream): int {.raises: [Defect, IOError, OSError].} =
  ## Retrieves the current position in the stream `s`.
  when false: # runnableExamples:
    var strm = newStringStream("The first line\nthe second line\nthe third line")
    assert strm.getPosition() == 0
    discard strm.readLine()
    assert strm.getPosition() == 15
    strm.close()

  result = s.getPositionImpl(s)

proc readData*(s: Stream, buffer: pointer, bufLen: int): int {.raises: [Defect, IOError, OSError].} =
  ## Low level proc that reads data into an untyped `buffer` of `bufLen` size.
  ##
  ## **JS note:** `buffer` is treated as a ``ptr string`` and written to between
  ## ``0..<bufLen``.
  when false: # runnableExamples:
    var strm = newStringStream("abcde")
    var buffer: array[6, char]
    assert strm.readData(addr(buffer), 1024) == 5
    assert buffer == ['a', 'b', 'c', 'd', 'e', '\x00']
    assert strm.atEnd() == true
    strm.close()

  result = s.readDataImpl(s, buffer, bufLen)

proc readDataStr*(s: Stream, buffer: var string, slice: Slice[int]): int {.raises: [Defect, IOError, OSError].} =
  ## Low level proc that reads data into a string ``buffer`` at ``slice``.
  when false: # runnableExamples:
    var strm = newStringStream("abcde")
    var buffer = "12345"
    assert strm.readDataStr(buffer, 0..3) == 4
    assert buffer == "abcd5"
    strm.close()

  if s.readDataStrImpl != nil:
    result = s.readDataStrImpl(s, buffer, slice)
  else:
    # fallback
    result = s.readData(addr prepareMutationAt(buffer, slice.a), slice.b + 1 - slice.a)

# template jsOrVmBlock(caseJsOrVm: untyped, caseElse: untyped): untyped {.untyped.} =
#   when false: # nimvm
#     block:
#       caseJsOrVm
#   else:
#     block:
#       when defined(js) or defined(nimscript):
#         # nimscript has to be here to avoid semantic checking of caseElse
#         caseJsOrVm
#       else:
#         caseElse

when not defined(js):
  proc readAll*(s: Stream): string {.raises.} =
    ## Reads all available data.
    when false: # runnableExamples:
      var strm = newStringStream("The first line\nthe second line\nthe third line")
      assert strm.readAll() == "The first line\nthe second line\nthe third line"
      assert strm.atEnd() == true
      strm.close()

    const bufferSize = 1024
    result = ""
    when true:
    # jsOrVmBlock:
    #   var buffer2 = newString(bufferSize)
    #   while true:
    #     let readBytes = readDataStr(s, buffer2, 0..<bufferSize)
    #     if readBytes == 0:
    #       break
    #     let prevLen = result.len
    #     result.setLen(prevLen + readBytes)
    #     result[prevLen..<prevLen+readBytes] = buffer2[0..<readBytes]
    #     if readBytes < bufferSize:
    #       break
    # do: # not JS or VM
      var buffer {.noinit.}: array[bufferSize, char]
      while true:
        let readBytes = readData(s, addr(buffer[0]), bufferSize)
        if readBytes == 0:
          break
        let prevLen = result.len
        result.setLen(prevLen + readBytes)
        copyMem(addr(prepareMutationAt(result, prevLen)), addr(buffer[0]), readBytes)
        if readBytes < bufferSize:
          break

proc peekData*(s: Stream, buffer: pointer, bufLen: int): int {.raises: [Defect, IOError, OSError].} =
  ## Low level proc that reads data into an untyped `buffer` of `bufLen` size
  ## without moving stream position.
  ##
  ## **JS note:** `buffer` is treated as a ``ptr string`` and written to between
  ## ``0..<bufLen``.
  when false: # runnableExamples:
    var strm = newStringStream("abcde")
    var buffer: array[6, char]
    assert strm.peekData(addr(buffer), 1024) == 5
    assert buffer == ['a', 'b', 'c', 'd', 'e', '\x00']
    assert strm.atEnd() == false
    strm.close()

  result = s.peekDataImpl(s, buffer, bufLen)

proc writeData*(s: Stream, buffer: pointer, bufLen: int) {.raises.} =
  ## Low level proc that writes an untyped `buffer` of `bufLen` size
  ## to the stream `s`.
  ##
  ## **JS note:** `buffer` is treated as a ``ptr string`` and read between
  ## ``0..<bufLen``.
  when false: # runnableExamples:
    ## writeData
    var strm = newStringStream("")
    var buffer = ['a', 'b', 'c', 'd', 'e']
    strm.writeData(addr(buffer), sizeof(buffer))
    assert strm.atEnd() == true
    ## readData
    strm.setPosition(0)
    var buffer2: array[6, char]
    assert strm.readData(addr(buffer2), sizeof(buffer2)) == 5
    assert buffer2 == ['a', 'b', 'c', 'd', 'e', '\x00']
    strm.close()

  s.writeDataImpl(s, buffer, bufLen)

proc write*[T](s: Stream, x: T) {.raises.} =
  ## Generic write procedure. Writes `x` to the stream `s`. Implementation:
  ##
  ## **Note:** Not available for JS backend. Use `write(Stream, string)
  ## <#write,Stream,string>`_ for now.
  ##
  ##   ```Nim
  ##   s.writeData(s, addr(x), sizeof(x))
  ##   ```
  when false: # runnableExamples:
    var strm = newStringStream("")
    strm.write("abcde")
    strm.setPosition(0)
    assert strm.readAll() == "abcde"
    strm.close()

  writeData(s, addr(x), sizeof(x))

proc write*(s: Stream, x: string) {.raises.} =
  ## Writes the string `x` to the stream `s`. No length field or
  ## terminating zero is written.
  when false: # runnableExamples:
    var strm = newStringStream("")
    strm.write("THE FIRST LINE")
    strm.setPosition(0)
    assert strm.readLine() == "THE FIRST LINE"
    strm.close()

  when false: # nimvm
    writeData(s, cstring(x), x.len)
  else:
    if x.len > 0:
      when defined(js):
        var x = x
        writeData(s, addr(x), x.len)
      else:
        var x = x
        writeData(s, toCString(x), x.len)

when false: # TODO:
  proc write*(s: Stream, args: varargs[string, `$`]) =
    ## Writes one or more strings to the the stream. No length fields or
    ## terminating zeros are written.
    when false: # runnableExamples:
      var strm = newStringStream("")
      strm.write(1, 2, 3, 4)
      strm.setPosition(0)
      assert strm.readLine() == "1234"
      strm.close()

    for str in args: write(s, str)

  proc writeLine*(s: Stream, args: varargs[string, `$`]) =
    ## Writes one or more strings to the the stream `s` followed
    ## by a new line. No length field or terminating zero is written.
    when false: # runnableExamples:
      var strm = newStringStream("")
      strm.writeLine(1, 2)
      strm.writeLine(3, 4)
      strm.setPosition(0)
      assert strm.readAll() == "12\n34\n"
      strm.close()

    for str in args: write(s, str)
    write(s, "\n")

proc read*[T](s: Stream, result: var T) {.raises.} =
  ## Generic read procedure. Reads `result` from the stream `s`.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream("012")
    ## readInt
    var i: int8
    strm.read(i)
    assert i == 48
    ## readData
    var buffer: array[2, char]
    strm.read(buffer)
    assert buffer == ['1', '2']
    strm.close()

  if readData(s, addr(result), sizeof(T)) != sizeof(T):
    raise IOError #newEIO("cannot read from stream")

proc peek*[T](s: Stream, result: var T) {.raises.} =
  ## Generic peek procedure. Peeks `result` from the stream `s`.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream("012")
    ## peekInt
    var i: int8
    strm.peek(i)
    assert i == 48
    ## peekData
    var buffer: array[2, char]
    strm.peek(buffer)
    assert buffer == ['0', '1']
    strm.close()

  if peekData(s, addr(result), sizeof(T)) != sizeof(T):
    raise IOError#newEIO("cannot read from stream")

proc readChar*(s: Stream): char {.raises.} =
  ## Reads a char from the stream `s`.
  ##
  ## Raises `IOError` if an error occurred.
  ## Returns '\\0' as an EOF marker.
  when false: # runnableExamples:
    var strm = newStringStream("12\n3")
    assert strm.readChar() == '1'
    assert strm.readChar() == '2'
    assert strm.readChar() == '\n'
    assert strm.readChar() == '3'
    assert strm.readChar() == '\x00'
    strm.close()
  result = '\0'
  when true:
  # jsOrVmBlock:
  #   var str = " "
  #   if readDataStr(s, str, 0..0) != 1: result = '\0'
  #   else: result = str[0]
  # do:
    if readData(s, addr(result), sizeof(result)) != 1: result = '\0'

proc peekChar*(s: Stream): char {.raises.} =
  ## Peeks a char from the stream `s`. Raises `IOError` if an error occurred.
  ## Returns '\\0' as an EOF marker.
  when false: # runnableExamples:
    var strm = newStringStream("12\n3")
    assert strm.peekChar() == '1'
    assert strm.peekChar() == '1'
    discard strm.readAll()
    assert strm.peekChar() == '\x00'
    strm.close()

  result = '\0'
  when defined(js):
    var str = " "
    if peekData(s, addr(str), sizeof(result)) != 1: result = '\0'
    else: result = str[0]
  else:
    if peekData(s, addr(result), sizeof(result)) != 1: result = '\0'

proc readBool*(s: Stream): bool {.raises.} =
  ## Reads a bool from the stream `s`.
  ##
  ## A bool is one byte long and it is `true` for every non-zero
  ## (`0000_0000`) value.
  ## Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(true)
    strm.write(false)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readBool() == true
    assert strm.readBool() == false
    doAssertRaises(IOError): discard strm.readBool()
    strm.close()

  var t: byte = byte(0)
  read(s, t)
  result = t != 0.byte

proc peekBool*(s: Stream): bool {.raises.} =
  ## Peeks a bool from the stream `s`.
  ##
  ## A bool is one byte long and it is `true` for every non-zero
  ## (`0000_0000`) value.
  ## Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(true)
    strm.write(false)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekBool() == true
    ## not false
    assert strm.peekBool() == true
    assert strm.readBool() == true
    assert strm.peekBool() == false
    strm.close()

  var t: byte = byte(0)
  peek(s, t)
  result = t != 0.byte

proc readInt8*(s: Stream): int8 {.raises.} =
  ## Reads an int8 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'i8)
    strm.write(2'i8)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readInt8() == 1'i8
    assert strm.readInt8() == 2'i8
    doAssertRaises(IOError): discard strm.readInt8()
    strm.close()
  result = int8(0)
  read(s, result)

proc peekInt8*(s: Stream): int8 {.raises.} =
  ## Peeks an int8 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'i8)
    strm.write(2'i8)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekInt8() == 1'i8
    ## not 2'i8
    assert strm.peekInt8() == 1'i8
    assert strm.readInt8() == 1'i8
    assert strm.peekInt8() == 2'i8
    strm.close()
  result = int8(0)
  peek(s, result)

proc readInt16*(s: Stream): int16 {.raises.} =
  ## Reads an int16 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'i16)
    strm.write(2'i16)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readInt16() == 1'i16
    assert strm.readInt16() == 2'i16
    doAssertRaises(IOError): discard strm.readInt16()
    strm.close()
  result = int16(0)
  read(s, result)

proc peekInt16*(s: Stream): int16 {.raises.} =
  ## Peeks an int16 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'i16)
    strm.write(2'i16)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekInt16() == 1'i16
    ## not 2'i16
    assert strm.peekInt16() == 1'i16
    assert strm.readInt16() == 1'i16
    assert strm.peekInt16() == 2'i16
    strm.close()
  result = int16(0)
  peek(s, result)

proc readInt32*(s: Stream): int32 {.raises.} =
  ## Reads an int32 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'i32)
    strm.write(2'i32)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readInt32() == 1'i32
    assert strm.readInt32() == 2'i32
    doAssertRaises(IOError): discard strm.readInt32()
    strm.close()
  result = int32(0)
  read(s, result)

proc peekInt32*(s: Stream): int32 {.raises.} =
  ## Peeks an int32 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'i32)
    strm.write(2'i32)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekInt32() == 1'i32
    ## not 2'i32
    assert strm.peekInt32() == 1'i32
    assert strm.readInt32() == 1'i32
    assert strm.peekInt32() == 2'i32
    strm.close()
  result = int32(0)
  peek(s, result)

proc readInt64*(s: Stream): int64 {.raises.} =
  ## Reads an int64 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'i64)
    strm.write(2'i64)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readInt64() == 1'i64
    assert strm.readInt64() == 2'i64
    doAssertRaises(IOError): discard strm.readInt64()
    strm.close()
  result = int64(0)
  read(s, result)

proc peekInt64*(s: Stream): int64 {.raises.} =
  ## Peeks an int64 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'i64)
    strm.write(2'i64)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekInt64() == 1'i64
    ## not 2'i64
    assert strm.peekInt64() == 1'i64
    assert strm.readInt64() == 1'i64
    assert strm.peekInt64() == 2'i64
    strm.close()
  result = int64(0)
  peek(s, result)

proc readUint8*(s: Stream): uint8 {.raises.} =
  ## Reads an uint8 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'u8)
    strm.write(2'u8)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readUint8() == 1'u8
    assert strm.readUint8() == 2'u8
    doAssertRaises(IOError): discard strm.readUint8()
    strm.close()
  result = uint8(0)
  read(s, result)

proc peekUint8*(s: Stream): uint8 {.raises.} =
  ## Peeks an uint8 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'u8)
    strm.write(2'u8)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekUint8() == 1'u8
    ## not 2'u8
    assert strm.peekUint8() == 1'u8
    assert strm.readUint8() == 1'u8
    assert strm.peekUint8() == 2'u8
    strm.close()
  result = uint8(0)
  peek(s, result)

proc readUint16*(s: Stream): uint16 {.raises.} =
  ## Reads an uint16 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'u16)
    strm.write(2'u16)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readUint16() == 1'u16
    assert strm.readUint16() == 2'u16
    doAssertRaises(IOError): discard strm.readUint16()
    strm.close()
  result = uint16(0)
  read(s, result)

proc peekUint16*(s: Stream): uint16 {.raises.} =
  ## Peeks an uint16 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'u16)
    strm.write(2'u16)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekUint16() == 1'u16
    ## not 2'u16
    assert strm.peekUint16() == 1'u16
    assert strm.readUint16() == 1'u16
    assert strm.peekUint16() == 2'u16
    strm.close()
  result = uint16(0)
  peek(s, result)

proc readUint32*(s: Stream): uint32 {.raises.} =
  ## Reads an uint32 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'u32)
    strm.write(2'u32)
    strm.flush()
    strm.setPosition(0)

    ## get data
    assert strm.readUint32() == 1'u32
    assert strm.readUint32() == 2'u32
    doAssertRaises(IOError): discard strm.readUint32()
    strm.close()
  result = uint32(0)
  read(s, result)

proc peekUint32*(s: Stream): uint32 {.raises.} =
  ## Peeks an uint32 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'u32)
    strm.write(2'u32)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekUint32() == 1'u32
    ## not 2'u32
    assert strm.peekUint32() == 1'u32
    assert strm.readUint32() == 1'u32
    assert strm.peekUint32() == 2'u32
    strm.close()
  result = uint32(0)
  peek(s, result)

proc readUint64*(s: Stream): uint64 {.raises.} =
  ## Reads an uint64 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'u64)
    strm.write(2'u64)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readUint64() == 1'u64
    assert strm.readUint64() == 2'u64
    doAssertRaises(IOError): discard strm.readUint64()
    strm.close()
  result = uint64(0)
  read(s, result)

proc peekUint64*(s: Stream): uint64 {.raises.} =
  ## Peeks an uint64 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'u64)
    strm.write(2'u64)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekUint64() == 1'u64
    ## not 2'u64
    assert strm.peekUint64() == 1'u64
    assert strm.readUint64() == 1'u64
    assert strm.peekUint64() == 2'u64
    strm.close()
  result = uint64(0)
  peek(s, result)

proc readFloat32*(s: Stream): float32 {.raises.} =
  ## Reads a float32 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'f32)
    strm.write(2'f32)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readFloat32() == 1'f32
    assert strm.readFloat32() == 2'f32
    doAssertRaises(IOError): discard strm.readFloat32()
    strm.close()
  result = 0.0'f32
  read(s, result)

proc peekFloat32*(s: Stream): float32 {.raises.} =
  ## Peeks a float32 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'f32)
    strm.write(2'f32)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekFloat32() == 1'f32
    ## not 2'f32
    assert strm.peekFloat32() == 1'f32
    assert strm.readFloat32() == 1'f32
    assert strm.peekFloat32() == 2'f32
    strm.close()
  result = 0.0'f32
  peek(s, result)

proc readFloat64*(s: Stream): float64 {.raises.} =
  ## Reads a float64 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `readStr <#readStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'f64)
    strm.write(2'f64)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.readFloat64() == 1'f64
    assert strm.readFloat64() == 2'f64
    doAssertRaises(IOError): discard strm.readFloat64()
    strm.close()
  result = 0.0
  read(s, result)

proc peekFloat64*(s: Stream): float64 {.raises.} =
  ## Peeks a float64 from the stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** Not available for JS backend. Use `peekStr <#peekStr,Stream,int>`_ for now.
  when false: # runnableExamples:
    var strm = newStringStream()
    ## setup for reading data
    strm.write(1'f64)
    strm.write(2'f64)
    strm.flush()
    strm.setPosition(0)
    ## get data
    assert strm.peekFloat64() == 1'f64
    ## not 2'f64
    assert strm.peekFloat64() == 1'f64
    assert strm.readFloat64() == 1'f64
    assert strm.peekFloat64() == 2'f64
    strm.close()
  result = 0.0
  peek(s, result)

proc readStrPrivate(s: Stream, length: int, str: var string) {.raises.} =
  if length > len(str): setLen(str, length)
  var L: int
  when false: # nimvm
    L = readDataStr(s, str, 0..length-1)
  else:
    when defined(js):
      L = readData(s, addr(str), length)
    else:
      L = readData(s, toCString(str), length)
  if L != len(str): setLen(str, L)

proc readStr*(s: Stream, length: int, str: var string) {.raises.} =
  ## Reads a string of length `length` from the stream `s`. Raises `IOError` if
  ## an error occurred.
  readStrPrivate(s, length, str)

proc readStr*(s: Stream, length: int): string {.raises.} =
  ## Reads a string of length `length` from the stream `s`. Raises `IOError` if
  ## an error occurred.
  when false: # runnableExamples:
    var strm = newStringStream("abcde")
    assert strm.readStr(2) == "ab"
    assert strm.readStr(2) == "cd"
    assert strm.readStr(2) == "e"
    assert strm.readStr(2) == ""
    strm.close()
  result = newString(length)
  readStrPrivate(s, length, result)

proc peekStrPrivate(s: Stream, length: int, str: var string) {.raises.} =
  if length > len(str): setLen(str, length)
  when defined(js):
    let L = peekData(s, addr(str), length)
  else:
    let L = peekData(s, toCString(str), length)
  if L != len(str): setLen(str, L)

proc peekStr*(s: Stream, length: int, str: var string) {.raises.} =
  ## Peeks a string of length `length` from the stream `s`. Raises `IOError` if
  ## an error occurred.
  peekStrPrivate(s, length, str)

proc peekStr*(s: Stream, length: int): string {.raises.} =
  ## Peeks a string of length `length` from the stream `s`. Raises `IOError` if
  ## an error occurred.
  when false: # runnableExamples:
    var strm = newStringStream("abcde")
    assert strm.peekStr(2) == "ab"
    ## not "cd
    assert strm.peekStr(2) == "ab"
    assert strm.readStr(2) == "ab"
    assert strm.peekStr(2) == "cd"
    strm.close()
  result = newString(length)
  peekStrPrivate(s, length, result)

proc readLine*(s: Stream, line: var string): bool {.raises.} =
  ## Reads a line of text from the stream `s` into `line`. `line` must not be
  ## ``nil``! May throw an IO exception.
  ##
  ## A line of text may be delimited by ``LF`` or ``CRLF``.
  ## The newline character(s) are not part of the returned string.
  ## Returns ``false`` if the end of the file has been reached, ``true``
  ## otherwise. If ``false`` is returned `line` contains no new data.
  ##
  ## See also:
  ## * `readLine(Stream) proc <#readLine,Stream>`_
  ## * `peekLine(Stream) proc <#peekLine,Stream>`_
  ## * `peekLine(Stream, string) proc <#peekLine,Stream,string>`_
  when false: # runnableExamples:
    var strm = newStringStream("The first line\nthe second line\nthe third line")
    var line = ""
    assert strm.readLine(line) == true
    assert line == "The first line"
    assert strm.readLine(line) == true
    assert line == "the second line"
    assert strm.readLine(line) == true
    assert line == "the third line"
    assert strm.readLine(line) == false
    assert line == ""
    strm.close()

  if s.readLineImpl != nil:
    result = s.readLineImpl(s, line)
  else:
    # fallback
    line.setLen(0)
    while true:
      var c = readChar(s)
      if c == '\c':
        c = readChar(s)
        break
      elif c == '\L': break
      elif c == '\0':
        if line.len > 0: break
        else: return false
      line.add(c)
    result = true

proc peekLine*(s: Stream, line: var string): bool {.raises.} =
  ## Peeks a line of text from the stream `s` into `line`. `line` must not be
  ## ``nil``! May throw an IO exception.
  ##
  ## A line of text may be delimited by ``CR``, ``LF`` or
  ## ``CRLF``. The newline character(s) are not part of the returned string.
  ## Returns ``false`` if the end of the file has been reached, ``true``
  ## otherwise. If ``false`` is returned `line` contains no new data.
  ##
  ## See also:
  ## * `readLine(Stream) proc <#readLine,Stream>`_
  ## * `readLine(Stream, string) proc <#readLine,Stream,string>`_
  ## * `peekLine(Stream) proc <#peekLine,Stream>`_
  when false: # runnableExamples:
    var strm = newStringStream("The first line\nthe second line\nthe third line")
    var line = ""
    assert strm.peekLine(line) == true
    assert line == "The first line"
    assert strm.peekLine(line) == true
    ## not "the second line"
    assert line == "The first line"
    assert strm.readLine(line) == true
    assert line == "The first line"
    assert strm.peekLine(line) == true
    assert line == "the second line"
    strm.close()

  let pos = getPosition(s)
  defer: setPosition(s, pos)
  result = readLine(s, line)

proc readLine*(s: Stream): string {.raises.} =
  ## Reads a line from a stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** This is not very efficient.
  ##
  ## See also:
  ## * `readLine(Stream, string) proc <#readLine,Stream,string>`_
  ## * `peekLine(Stream) proc <#peekLine,Stream>`_
  ## * `peekLine(Stream, string) proc <#peekLine,Stream,string>`_
  when false: # runnableExamples:
    var strm = newStringStream("The first line\nthe second line\nthe third line")
    assert strm.readLine() == "The first line"
    assert strm.readLine() == "the second line"
    assert strm.readLine() == "the third line"
    doAssertRaises(IOError): discard strm.readLine()
    strm.close()

  result = ""
  if s.atEnd:
    raise EndOfStreamError#newEIO("cannot read from stream")
  while true:
    var c = readChar(s)
    if c == '\c':
      c = readChar(s)
      break
    if c == '\L' or c == '\0':
      break
    else:
      result.add(c)

proc peekLine*(s: Stream): string {.raises.} =
  ## Peeks a line from a stream `s`. Raises `IOError` if an error occurred.
  ##
  ## **Note:** This is not very efficient.
  ##
  ## See also:
  ## * `readLine(Stream) proc <#readLine,Stream>`_
  ## * `readLine(Stream, string) proc <#readLine,Stream,string>`_
  ## * `peekLine(Stream, string) proc <#peekLine,Stream,string>`_
  when false: # runnableExamples:
    var strm = newStringStream("The first line\nthe second line\nthe third line")
    assert strm.peekLine() == "The first line"
    ## not "the second line"
    assert strm.peekLine() == "The first line"
    assert strm.readLine() == "The first line"
    assert strm.peekLine() == "the second line"
    strm.close()

  let pos = getPosition(s)
  defer: setPosition(s, pos)
  result = readLine(s)

iterator lines*(s: Stream): string {.raises.} =
  ## Iterates over every line in the stream.
  ## The iteration is based on ``readLine``.
  ##
  ## See also:
  ## * `readLine(Stream) proc <#readLine,Stream>`_
  ## * `readLine(Stream, string) proc <#readLine,Stream,string>`_
  when false: # runnableExamples:
    var strm = newStringStream("The first line\nthe second line\nthe third line")
    var lines: seq[string]
    for line in strm.lines():
      lines.add line
    assert lines == @["The first line", "the second line", "the third line"]
    strm.close()

  var line: string = ""
  while s.readLine(line):
    yield line

type
  StringStream* = ref StringStreamObj
    ## A stream that encapsulates a string.
  StringStreamObj* = object of StreamObj
    ## A string stream object.
    data*: string ## A string data.
                  ## This is updated when called `writeLine` etc.
    pos: int

when false: # (NimMajor, NimMinor) < (1, 3) and defined(js):
  proc ssAtEnd(s: Stream): bool {.compileTime.} =
    var s = StringStream(s)
    return s.pos >= s.data.len

  proc ssSetPosition(s: Stream, pos: int) {.compileTime.} =
    var s = StringStream(s)
    s.pos = clamp(pos, 0, s.data.len)

  proc ssGetPosition(s: Stream): int {.compileTime.} =
    var s = StringStream(s)
    return s.pos

  proc ssReadDataStr(s: Stream, buffer: var string, slice: Slice[int]): int {.compileTime.} =
    var s = StringStream(s)
    result = min(slice.b + 1 - slice.a, s.data.len - s.pos)
    if result > 0:
      buffer[slice.a..<slice.a+result] = s.data[s.pos..<s.pos+result]
      inc(s.pos, result)
    else:
      result = 0

  proc ssClose(s: Stream) {.compileTime.} =
    var s = StringStream(s)
    s.data = ""

  proc newStringStream*(s: string = ""): StringStream {.compileTime.} =
    new(result)
    result.data = s
    result.pos = 0
    result.closeImpl = ssClose
    result.atEndImpl = ssAtEnd
    result.setPositionImpl = ssSetPosition
    result.getPositionImpl = ssGetPosition
    result.readDataStrImpl = ssReadDataStr

  proc readAll*(s: Stream): string {.compileTime.} =
    const bufferSize = 1024
    var bufferr: string
    bufferr.setLen(bufferSize)
    while true:
      let readBytes = readDataStr(s, bufferr, 0..<bufferSize)
      if readBytes == 0:
        break
      let prevLen = result.len
      result.setLen(prevLen + readBytes)
      result[prevLen..<prevLen+readBytes] = bufferr[0..<readBytes]
      if readBytes < bufferSize:
        break

else: # after 1.3 or JS not defined
  proc ssAtEnd(s: Stream): bool {.nimcall, raises: [Defect, IOError, OSError], tags: [].} =
    var s = StringStream(s)
    return s.pos >= s.data.len

  proc ssSetPosition(s: Stream, pos: int) {.nimcall, raises: [Defect, IOError, OSError], tags: [].} =
    var s = StringStream(s)
    s.pos = clamp(pos, 0, s.data.len)

  proc ssGetPosition(s: Stream): int {.nimcall, raises: [Defect, IOError, OSError], tags: [].} =
    var s = StringStream(s)
    return s.pos

  proc ssReadDataStr(s: Stream, buffer: var string, slice: Slice[int]): int {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].} =
    var s = StringStream(s)
    when false: # nimvm
      discard
    else:
      when declared(prepareMutation):
        prepareMutation(buffer) # buffer might potentially be a CoW literal with ARC
    result = min(slice.b + 1 - slice.a, s.data.len - s.pos)
    if result > 0:
      when true:
      # jsOrVmBlock:
      #   buffer[slice.a..<slice.a+result] = s.data[s.pos..<s.pos+result]
      # do:
        copyMem(addr prepareMutationAt(buffer, slice.a), addr prepareMutationAt(s.data, s.pos), result)
      inc(s.pos, result)
    else:
      result = 0

  proc ssReadData(s: Stream, buffer: pointer, bufLen: int): int {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].} =
    var s = StringStream(s)
    result = min(bufLen, s.data.len - s.pos)
    if result > 0:
      when defined(js):
        try:
          cast[ptr string](buffer)[][0..<result] = s.data[s.pos..<s.pos+result]
        except:
          raise newException(Defect, "could not read string stream, " &
            "did you use a non-string buffer pointer?", getCurrentException())
      elif not defined(nimscript):
        copyMem(buffer, addr(prepareMutationAt(s.data, s.pos)), result)
      inc(s.pos, result)
    else:
      result = 0

  proc ssPeekData(s: Stream, buffer: pointer, bufLen: int): int {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].} =
    var s = StringStream(s)
    result = min(bufLen, s.data.len - s.pos)
    if result > 0:
      when defined(js):
        try:
          cast[ptr string](buffer)[][0..<result] = s.data[s.pos..<s.pos+result]
        except:
          raise newException(Defect, "could not peek string stream, " &
            "did you use a non-string buffer pointer?", getCurrentException())
      elif not defined(nimscript):
        copyMem(buffer, addr(prepareMutationAt(s.data, s.pos)), result)
    else:
      result = 0

  proc ssWriteData(s: Stream, buffer: pointer, bufLen: int) {.nimcall, raises: [Defect, IOError, OSError], tags: [WriteIOEffect].} =
    var s = StringStream(s)
    if bufLen <= 0:
      return
    if s.pos + bufLen > s.data.len:
      setLen(s.data, s.pos + bufLen)
    when defined(js):
      try:
        s.data[s.pos..<s.pos+bufLen] = cast[ptr string](buffer)[][0..<bufLen]
      except:
        raise newException(Defect, "could not write to string stream, " &
          "did you use a non-string buffer pointer?", getCurrentException())
    elif not defined(nimscript):
      copyMem(addr(prepareMutationAt(s.data, s.pos)), buffer, bufLen)
    inc(s.pos, bufLen)

  proc ssClose(s: Stream) {.nimcall, raises: [IOError, OSError], tags: [WriteIOEffect].} =
    var s = StringStream(s)
    s.data = ""

  proc newStringStream*(s: sink string = ""): StringStream =
    ## Creates a new stream from the string `s`.
    ##
    ## See also:
    ## * `newFileStream proc <#newFileStream,File>`_ creates a file stream from
    ##   opened File.
    ## * `newFileStream proc <#newFileStream,string,FileMode,int>`_  creates a
    ##   file stream from the file name and the mode.
    ## * `openFileStream proc <#openFileStream,string,FileMode,int>`_ creates a
    ##   file stream from the file name and the mode.
    when false: # runnableExamples:
      var strm = newStringStream("The first line\nthe second line\nthe third line")
      assert strm.readLine() == "The first line"
      assert strm.readLine() == "the second line"
      assert strm.readLine() == "the third line"
      strm.close()

    result = StringStream(data: s)
    when false: # nimvm
      discard
    else:
      when declared(prepareMutation):
        prepareMutation(result.data) # Allows us to mutate using `addr` logic like `copyMem`, otherwise it errors.
    result.pos = 0
    result.closeImpl = ssClose
    result.atEndImpl = ssAtEnd
    result.setPositionImpl = ssSetPosition
    result.getPositionImpl = ssGetPosition
    result.readDataStrImpl = ssReadDataStr
    when false: # nimvm
      discard
    else:
      result.readDataImpl = ssReadData
      result.peekDataImpl = ssPeekData
      result.writeDataImpl = ssWriteData

type
  FileStream* = ref FileStreamObj
    ## A stream that encapsulates a `File`.
    ##
    ## **Note:** Not available for JS backend.
  FileStreamObj* = object of Stream
    ## A file stream object.
    ##
    ## **Note:** Not available for JS backend.
    f: File

proc fsClose(s: Stream) {.nimcall, raises: [IOError, OSError], tags: [WriteIOEffect].} =
  if FileStream(s).f != nil:
    close(FileStream(s).f)
    FileStream(s).f = nil
proc fsFlush(s: Stream) {.nimcall, raises: [Defect, IOError, OSError], tags: [WriteIOEffect].} = flushFile(FileStream(s).f)
proc fsAtEnd(s: Stream): bool {.nimcall, raises: [Defect, IOError, OSError], tags: [].} = return endOfFile(FileStream(s).f)
proc fsSetPosition(s: Stream, pos: int) {.nimcall, raises: [Defect, IOError, OSError], tags: [].} = setFilePos(FileStream(s).f, pos)
proc fsGetPosition(s: Stream): int {.nimcall, raises: [Defect, IOError, OSError], tags: [].} = return int(getFilePos(FileStream(s).f))

proc fsReadData(s: Stream, buffer: pointer, bufLen: int): int {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].} =
  result = readBuffer(FileStream(s).f, buffer, bufLen)

proc fsReadDataStr(s: Stream, buffer: var string, slice: Slice[int]): int {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].} =
  result = readBuffer(FileStream(s).f, addr(prepareMutationAt(buffer, slice.a)), slice.b + 1 - slice.a)

proc fsPeekData(s: Stream, buffer: pointer, bufLen: int): int {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].} =
  let pos = fsGetPosition(s)
  defer: fsSetPosition(s, pos)
  result = readBuffer(FileStream(s).f, buffer, bufLen)

proc fsWriteData(s: Stream, buffer: pointer, bufLen: int) {.nimcall, raises: [Defect, IOError, OSError], tags: [WriteIOEffect].} =
  if writeBuffer(FileStream(s).f, buffer, bufLen) != bufLen:
    raise IOError #newEIO("cannot write to stream")

proc fsReadLine(s: Stream, line: var string): bool {.nimcall, raises: [Defect, IOError, OSError], tags: [ReadIOEffect].} =
  result = readLine(FileStream(s).f, line)

proc newFileStream*(f: File): FileStream =
  ## Creates a new stream from the file `f`.
  ##
  ## **Note:** Not available for JS backend.
  ##
  ## See also:
  ## * `newStringStream proc <#newStringStream,string>`_ creates a new stream
  ##   from string.
  ## * `newFileStream proc <#newFileStream,string,FileMode,int>`_ is the same
  ##   as using `open proc <syncio.html#open,File,string,FileMode,int>`_
  ##   on Examples.
  ## * `openFileStream proc <#openFileStream,string,FileMode,int>`_ creates a
  ##   file stream from the file name and the mode.
  when false: # runnableExamples:
    ## Input (somefile.txt):
    ## The first line
    ## the second line
    ## the third line
    var f: File
    if open(f, "somefile.txt", fmRead, -1):
      var strm = newFileStream(f)
      var line = ""
      while strm.readLine(line):
        echo line
      ## Output:
      ## The first line
      ## the second line
      ## the third line
      strm.close()

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
    flushImpl: fsFlush
  )

proc newFileStream*(filename: string, mode: FileMode = fmRead,
    bufSize: int = -1): FileStream =
  ## Creates a new stream from the file named `filename` with the mode `mode`.
  ##
  ## If the file cannot be opened, `nil` is returned. See the `io module
  ## <syncio.html>`_ for a list of available `FileMode enums <syncio.html#FileMode>`_.
  ##
  ## **Note:**
  ## * **This function returns nil in case of failure.**
  ##   To prevent unexpected behavior and ensure proper error handling,
  ##   use `openFileStream proc <#openFileStream,string,FileMode,int>`_
  ##   instead.
  ## * Not available for JS backend.
  ##
  ## See also:
  ## * `newStringStream proc <#newStringStream,string>`_ creates a new stream
  ##   from string.
  ## * `newFileStream proc <#newFileStream,File>`_ creates a file stream from
  ##   opened File.
  ## * `openFileStream proc <#openFileStream,string,FileMode,int>`_ creates a
  ##   file stream from the file name and the mode.
  # when false: # runnableExamples:
  #   from std/os import removeFile
  #   var strm = newFileStream("somefile.txt", fmWrite)
  #   if not isNil(strm):
  #     strm.writeLine("The first line")
  #     strm.writeLine("the second line")
  #     strm.writeLine("the third line")
  #     strm.close()
  #     ## Output (somefile.txt)
  #     ## The first line
  #     ## the second line
  #     ## the third line
  #     removeFile("somefile.txt")

  var f: File = default(File)
  if open(f, filename, mode, bufSize): result = newFileStream(f)
  else: result = nil

proc openFileStream*(filename: string, mode: FileMode = fmRead,
    bufSize: int = -1): FileStream {.raises.} =
  ## Creates a new stream from the file named `filename` with the mode `mode`.
  ## If the file cannot be opened, an IO exception is raised.
  ##
  ## **Note:** Not available for JS backend.
  ##
  ## See also:
  ## * `newStringStream proc <#newStringStream,string>`_ creates a new stream
  ##   from string.
  ## * `newFileStream proc <#newFileStream,File>`_ creates a file stream from
  ##   opened File.
  ## * `newFileStream proc <#newFileStream,string,FileMode,int>`_  creates a
  ##   file stream from the file name and the mode.
  when false: # runnableExamples:
    try:
      ## Input (somefile.txt):
      ## The first line
      ## the second line
      ## the third line
      var strm = openFileStream("somefile.txt")
      echo strm.readLine()
      ## Output:
      ## The first line
      strm.close()
    except:
      stderr.write getCurrentExceptionMsg()

  var f: File = default(File)
  if open(f, filename, mode, bufSize):
    return newFileStream(f)
  else:
    raise IOError#newEIO("cannot open file stream: " & filename)

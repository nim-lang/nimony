#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [syncio, strutils, os, terminal, assertions, sets]
import ".." / lib / [nifstreams, nifcursors, bitabs, lineinfos]

type
  MsgKind* = enum
    Info = "Info: ",
    Warning = "Warning: ",
    Error = "Error: "
    Trace = "Trace: "
    Debug = "Debug: "

  Reporter* = object
    verbosity*: int
    noColors*: bool
    assertOnError*: bool
    warnings*: int
    errors*: int
    reportedErrSources: HashSet[PackedLineInfo]


proc useColors*(): bool = terminal.isatty(stdout)

proc writeMessage(c: var Reporter; category: string; p, arg: string) =
  var msg = p
  msg.add ' '
  msg.add category
  msg.add arg
  stdout.writeLine msg

proc writeMessage(c: var Reporter; k: MsgKind; p, arg: string) =
  if k == Trace and c.verbosity < 1: return
  elif k == Debug and c.verbosity < 2: return

  if c.noColors:
    writeMessage(c, $k, p, arg)
  else:
    let (color, style) =
      case k
      of Debug: (fgWhite, styleDim)
      of Trace: (fgBlue, styleBright)
      of Info: (fgGreen, styleBright)
      of Warning: (fgYellow, styleBright)
      of Error: (fgRed, styleBright)
    stdout.styledWriteLine(fgCyan, p, " ", resetStyle, color, style, $k, resetStyle, arg)

proc message(c: var Reporter; k: MsgKind; p, arg: string) =
  ## collects messages or prints them out immediately
  # c.messages.add (k, p, arg)
  writeMessage c, k, p, arg

proc warn*(c: var Reporter; p, arg: string) =
  c.message(Warning, p, arg)
  # writeMessage c, Warning, p, arg
  inc c.warnings

proc error*(c: var Reporter; p, arg: string) =
  when defined(debug):
    writeStackTrace()
  if c.assertOnError:
    raise newException(AssertionDefect, p & ": " & arg)
  c.message(Error, p, arg)
  inc c.errors

proc info*(c: var Reporter; p, arg: string) =
  c.message(Info, p, arg)

proc trace*(c: var Reporter; p, arg: string) =
  c.message(Trace, p, arg)

proc debug*(c: var Reporter; p, arg: string) =
  c.message(Debug, p, arg)

proc fatal*(msg: string) =
  when defined(debug):
    writeStackTrace()
  quit "[Error] " & msg

proc shortenDir*(x: string): string =
  var to = getCurrentDir()
  when defined(windows):
    let x = x.replace('\\', '/')
    to = to.replace('\\', '/')
  if not to.endsWith('/'):
    to.add '/'
  if startsWith(x, to):
    result = substr(x, to.len, x.len-1)
  else:
    result = x

proc infoToStr*(info: PackedLineInfo): string =
  let rawInfo = unpack(pool.man, info)
  if not info.isValid or not rawInfo.file.isValid:
    result = "???"
  else:
    result = pool.files[rawInfo.file].shortenDir()
    result.add "(" & $rawInfo.line & ", " & $(rawInfo.col+1) & ")"

proc reportErrors*(dest: var TokenBuf): int =
  let errTag = pool.tags.getOrIncl("err")
  var i = 0
  var r = Reporter(verbosity: 2, noColors: not useColors())
  result = 0
  while i < dest.len:
    if dest[i].kind == ParLe and dest[i].tagId == errTag:
      inc result
      let info = dest[i].info
      let doReport = not r.reportedErrSources.containsOrIncl(info)
      inc i
      # original expression, optional:
      if dest[i].kind == DotToken:
        inc i
      else:
        let x = cursorAt(dest, i)
        inc i, span(x)
        endRead(dest)
      # instantiation contexts:
      while dest[i].kind == DotToken:
        if doReport:
          r.trace infoToStr(dest[i].info), "instantiation from here"
        inc i
      # error message:
      assert dest[i].kind == StringLit
      if doReport:
        r.error infoToStr(info), pool.strings[dest[i].litId]
      inc i
    else:
      inc i

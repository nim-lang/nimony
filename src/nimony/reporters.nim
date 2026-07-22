#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [syncio, strutils, os, assertions, sets, terminal]
import ".." / lib / [nifcursors, nifstreams, bitabs, lineinfos]

include ".." / lib / compat2

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
    var color: ForegroundColor
    var style: Style
    case k
    of Debug:
      color = fgWhite; style = styleDim
    of Trace:
      color = fgBlue; style = styleBright
    of Info:
      color = fgGreen; style = styleBright
    of Warning:
      color = fgYellow; style = styleBright
    of Error:
      color = fgRed; style = styleBright
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
  when defined(debug) and not defined(nimony):
    writeStackTrace()
  c.message(Error, p, arg)
  inc c.errors

proc info*(c: var Reporter; p, arg: string) =
  c.message(Info, p, arg)

proc trace*(c: var Reporter; p, arg: string) =
  c.message(Trace, p, arg)

proc debug*(c: var Reporter; p, arg: string) =
  c.message(Debug, p, arg)

proc fatal*(msg: string) =
  when defined(debug) and not defined(nimony):
    writeStackTrace()
  quit "[Error] " & msg

proc shortenDir*(x: string): string =
  # `getCurrentDir` is `.raises`, but the only way it actually fails is a
  # transient I/O error that would affect any diagnostic equally. Swallow it
  # here so `shortenDir` (and by extension `infoToStr`) stays non-raising.
  var to = ""
  try:
    to = getCurrentDir()
  except:
    return x
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

proc reportErrorsRec(r: var Reporter; n: var Cursor; errTag: TagId; count: var int) =
  ## Recursive cursor walk (build-agnostic): an `(err …)` node is
  ## `(err <origExpr|.> <instantiation-dots…> <stringMsg>)`; the err's own head
  ## carries the source line info.
  if n.isTagLit:
    if n.tagId == errTag:
      inc count
      let info = n.info
      let doReport = not r.reportedErrSources.containsOrIncl(info)
      n.peekInto:
        # original expression, optional:
        if n.isDotToken: inc n
        else: skip n
        # instantiation contexts:
        while n.isDotToken:
          if doReport:
            r.trace infoToStr(n.info), "instantiation from here"
          inc n
        # error message:
        if n.isStringLit:
          if doReport:
            r.error infoToStr(info), pool.strings[n.litId]
          inc n
    else:
      n.into:
        while n.hasMore:
          reportErrorsRec(r, n, errTag, count)
  else:
    skip n

proc reportErrors*(dest: var TokenBuf): int =
  let errTag = pool.tags.getOrIncl("err")
  var r = Reporter(verbosity: 2, noColors: not useColors())
  result = 0
  var n = beginRead(dest)
  while n.hasMore:
    reportErrorsRec(r, n, errTag, result)

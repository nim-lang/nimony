#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [syncio, strutils, os, assertions, sets, terminal]
import ".." / lib / [nifpools, bitabs]

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
    reportedErrSources: HashSet[NifLineInfo]


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

proc infoToStr*(info: NifLineInfo): string =
  if not info.isValid:
    result = "???"
  else:
    # `realFile`: expanded code carries a forged filename recording where it came
    # from (see `comesfrom`'s `CrucialPrefix`). A user-facing message wants the
    # actual source path, not the provenance chain.
    result = realFile(pool.filenames[info.file]).shortenDir()
    result.add "(" & $info.line & ", " & $(info.col+1) & ")"

proc reportErrorsRec(r: var Reporter; n: var Cursor; errTag: TagId; count: var int) =
  ## Recursive cursor walk (build-agnostic): an `(err …)` node is
  ## `(err <origExpr|.> <instantiation-dots…> <stringMsg>)`; the err's own head
  ## carries the source line info.
  while n.hasMore:
    if n.isTagLit:
      if n.cursorTagId == errTag:
        inc count
        let info = n.info
        let doReport = not r.reportedErrSources.containsOrIncl(info)
        n.peekInto:
          # original expression, optional; remember it — it may contain nested
          # `(err …)` nodes of its own (classic's linear token scan reported
          # those too), reported after this outer one to keep the classic order:
          var payload = default(Cursor)
          if n.isDotToken:
            inc n
          else:
            payload = n
            skip n
          # instantiation contexts:
          while n.isDotToken:
            if doReport:
              r.trace infoToStr(n.info), "instantiation from here"
            inc n
          # error message: an EMPTY one is a deliberately silent `(err …)` —
          # the node still counts (so the module fails) but the diagnostic was
          # already reported at the real cause, e.g. a call whose argument is
          # itself erroneous. Printing again would just stack noise on top.
          if n.isStringLit:
            if doReport and pool.strings[n.strId].len > 0:
              r.error infoToStr(info), pool.strings[n.strId]
            inc n
          if not cursorIsNil(payload):
            reportErrorsRec(r, payload, errTag, count)
          # an `(err …)` produced by wrapping a whole decl (e.g. attachConverter)
          # carries the decl's children after the message; walk them too:
          reportErrorsRec(r, n, errTag, count)
      else:
        inc n
    else:
      skip n

proc reportErrors*(dest: var TokenBuf): int =
  let errTag = globalTags.registerTag("err")
  var r = Reporter(verbosity: 2, noColors: not useColors())
  result = 0
  var n = beginRead(dest)
  reportErrorsRec(r, n, errTag, result)
  endRead(n)

#
#
#            Nim's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a simple logger. It is a port of Nim's
## `std/logging` with one important difference: **every log entry is a line
## of NIF**, in NIF's line-based syntax. A log file is therefore a NIF module
## that NIF tools can read, with the line-based extension enabled (for
## example `nifreader.open(filename, lineBased = true)`).
##
## An entry is the level as the tag, followed by the fields of the logger's
## format string, followed by the message as a string literal:
##
## ```nif
## info "server started"
## warn 2026-09-29T09:14:03Z "myapp" "disk almost full"
## ```
##
## is the same as the core NIF `(info "server started")` and
## `(warn "2026-09-29T09:14:03Z" "myapp" "disk almost full")`.
##
## Format strings
## ==============
##
## Unlike Nim's logger, the format string is not a free-form prefix but a
## whitespace separated list of fields; each one becomes a child of the entry.
## A word that starts with `$` is one of the variables below, any other word
## is emitted as a string literal.
##
## ================  =======================================================
## Variable          Output
## ================  =======================================================
## `$date`           Current date in UTC (`2026-09-29`)
## `$time`           Current time in UTC (`"09:14:03"`)
## `$datetime`       `$date` and `$time` in ISO 8601 (`2026-09-29T09:14:03Z`)
## `$app`            `os.getAppFilename()`
## `$appname`        Base name of `$app`
## `$appdir`         Directory name of `$app`
## `$levelid`        First letter of the log level as an identifier (`I`)
## `$levelname`      The log level as an identifier (`INFO`)
## ================  =======================================================
##
## Nim's `std/times` is local time; Nimony's is UTC, hence the `Z`.
##
## Usage
## =====
##
## ```nim
## import std/logging
##
## var logger = newConsoleLogger(fmtStr = verboseFmtStr)
## addHandler(logger)
## info("server started on port ", 8080)
## ```
##
## Just like in Nim, the handlers are thread-local: every thread must call
## `addHandler` for the loggers it uses.

when defined(nimony):
  {.feature: "lenientnils".}

import std / [syncio, strutils, times, os, dirs, paths]

type
  Level* = enum ## Enumeration of logging levels.
    lvlAll,     ## All levels active
    lvlDebug,   ## Debug level and above are active
    lvlInfo,    ## Info level and above are active
    lvlNotice,  ## Notice level and above are active
    lvlWarn,    ## Warn level and above are active
    lvlError,   ## Error level and above are active
    lvlFatal,   ## Fatal level and above are active
    lvlNone     ## No levels active; nothing is logged

const
  LevelNames*: array[Level, string] = [
    "DEBUG", "DEBUG", "INFO", "NOTICE", "WARN", "ERROR", "FATAL", "NONE"
  ] ## Array of strings representing each logging level.

  LevelTags*: array[Level, string] = [
    "debug", "debug", "info", "notice", "warn", "error", "fatal", "none"
  ] ## The NIF tag of an entry of each logging level.

  defaultFmtStr* = "" ## The default format string: no fields besides
                      ## the level and the message.
  verboseFmtStr* = "$datetime $appname" ## \
    ## A more verbose format string: the time and the application's name.

  defaultFlushThreshold = lvlAll

type
  Logger* = ref object of RootObj
    ## The abstract base type of all loggers.
    ##
    ## Custom loggers should inherit from this type and override the `log`
    ## method.
    levelThreshold*: Level ## Only messages that are at or above this
                           ## threshold will be logged
    fmtStr*: string ## The fields of each entry; see `Format strings`_

  ConsoleLogger* = ref object of Logger
    ## A logger that writes log entries to the console.
    useStderr*: bool ## If true, writes to stderr; otherwise, writes to stdout
    flushThreshold*: Level ## Only messages that are at or above this
                           ## threshold will be flushed immediately

  FileLogger* = ref object of Logger
    ## A logger that writes log entries to a file.
    file*: File ## The wrapped file
    flushThreshold*: Level ## Only messages that are at or above this
                           ## threshold will be flushed immediately

  RollingFileLogger* = ref object of FileLogger
    ## A logger that writes log entries to a file, starting a new one when
    ## the current file reaches `maxLines` entries. The old files are
    ## renamed to `basename.1`, `basename.2` and so on.
    maxLines: int # maximum number of lines
    curLine: int
    baseName: string # initial filename
    baseMode: FileMode # initial file mode
    logFiles: int # how many log files already created, e.g. basename.1, basename.2...
    bufSize: int # size of output buffer (-1: use system defaults, 0: unbuffered, >0: fixed buffer size)

var
  logFilter {.threadvar.}: Level      ## global log filter
  handlers {.threadvar.}: seq[Logger] ## handlers with their own log levels

# ── NIF output ───────────────────────────────────────────────────────────

proc addNifStrLit(result: var string; s: string) =
  ## `s` as a NIF string literal. Bytes that would end the literal or the
  ## line are escaped, so that every entry stays on a line of its own.
  const HexChars = "0123456789ABCDEF"
  result.add '"'
  for c in s:
    case c
    of '"': result.add "\\^"
    of '\\': result.add "\\|"
    of '\n': result.add "\\n"
    of '\t': result.add "\\t"
    of '\r': result.add "\\r"
    of '\0'..'\x08', '\x0B', '\x0C', '\x0E'..'\x1F', '\x7F':
      result.add '\\'
      result.add HexChars[int(c) shr 4]
      result.add HexChars[int(c) and 0xF]
    else: result.add c
  result.add '"'

proc isAutoString(s: string): bool =
  ## Whether `s` can be written bare and a line-based NIF reader reads it
  ## back as the string `s`: a word starting like an identifier (or with
  ## `/`) that contains a `/` or `-` and nothing that ends or escapes it.
  const Special = {'(', ')', '[', ']', '{', '}', '~', '#', '\'', '"', ':',
                   '@', '\\', ' ', '\x7F'}
  if s.len == 0 or s[0] notin {'a'..'z', 'A'..'Z', '_', '/', '\x80'..'\xFF'}:
    return false
  result = false
  for c in s:
    if c < '!' or c in Special: return false
    if c == '/' or c == '-': result = true

proc addNifStr(result: var string; s: string) =
  if isAutoString(s): result.add s
  else: addNifStrLit(result, s)

proc pad(result: var string; x: int; digits: int) =
  let s = $x
  for i in s.len ..< digits: result.add '0'
  result.add s

proc addDate(result: var string; dt: DateTime) =
  pad(result, dt.year, 4)
  result.add '-'
  pad(result, int(dt.month), 2)
  result.add '-'
  pad(result, int(dt.monthday), 2)

proc addTime(result: var string; dt: DateTime) =
  pad(result, int(dt.hour), 2)
  result.add ':'
  pad(result, int(dt.minute), 2)
  result.add ':'
  pad(result, int(dt.second), 2)

proc addField(result: var string; v: string; level: Level) =
  case v
  of "date":
    # a number followed by `-`: an auto string
    addDate(result, now())
  of "time":
    # a bare time is not an auto string
    var t = ""
    addTime(t, now())
    addNifStrLit(result, t)
  of "datetime":
    let dt = now()
    addDate(result, dt)
    result.add 'T'
    addTime(result, dt)
    result.add 'Z'
  of "app": addNifStr(result, getAppFilename())
  of "appdir": addNifStr(result, getAppFilename().splitFile.dir)
  of "appname": addNifStr(result, getAppFilename().splitFile.name)
  of "levelid": result.add LevelNames[level][0]
  of "levelname": result.add LevelNames[level]
  else: addNifStrLit(result, "$" & v)

template concatArgs(args: untyped): string =
  var msg = ""
  for a in args: msg.add a
  msg

proc formatEntry(frmt: string; level: Level; msg: string): string =
  result = newStringOfCap(frmt.len + msg.len + 20)
  result.add LevelTags[level]
  var i = 0
  while i < frmt.len:
    if frmt[i] in Whitespace:
      inc i
    else:
      result.add ' '
      var w = ""
      if frmt[i] == '$':
        inc i
        while i < frmt.len and frmt[i] in IdentChars:
          w.add toLowerAscii(frmt[i])
          inc i
        addField(result, w, level)
      else:
        while i < frmt.len and frmt[i] notin Whitespace:
          w.add frmt[i]
          inc i
        addNifStr(result, w)
  result.add ' '
  addNifStrLit(result, msg)

proc substituteLog*(frmt: string; level: Level;
                    args: varargs[string, `$`]): string =
  ## Formats a log entry: the level as the tag, the fields of the format
  ## string `frmt`, and the concatenated `args` as a string literal. The
  ## result is one line of line-based NIF, without the trailing newline.
  ##
  ## See `Format strings`_ for the fields that can be used.
  runnableExamples:
    doAssert substituteLog(defaultFmtStr, lvlInfo, "a message") == "info \"a message\""
    doAssert substituteLog("$levelid", lvlError, "an ", "error") == "error E \"an error\""
    doAssert substituteLog("", lvlWarn, "say \"hi\"\n") == "warn \"say \\^hi\\^\\n\""
  result = formatEntry(frmt, level, concatArgs(args))

# ── Loggers ──────────────────────────────────────────────────────────────

method log*(logger: Logger; level: Level; args: varargs[string, `$`]) {.base.} =
  ## Override this method in custom loggers. The default implementation does
  ## nothing.
  discard

method log*(logger: ConsoleLogger; level: Level; args: varargs[string, `$`]) =
  ## Logs to the console, if `level` passes both the logger's threshold and
  ## the global log filter.
  if level >= logFilter and level >= logger.levelThreshold:
    let ln = formatEntry(logger.fmtStr, level, concatArgs(args))
    let handle = if logger.useStderr: stderr else: stdout
    writeLine(handle, ln)
    if level >= logger.flushThreshold: flushFile(handle)

proc newConsoleLogger*(levelThreshold = lvlAll; fmtStr = defaultFmtStr;
                       useStderr = false;
                       flushThreshold = defaultFlushThreshold): ConsoleLogger =
  ## Creates a new ConsoleLogger. Entries at or above `flushThreshold` are
  ## flushed immediately.
  result = ConsoleLogger(levelThreshold: levelThreshold, fmtStr: fmtStr,
                         useStderr: useStderr, flushThreshold: flushThreshold)

method log*(logger: FileLogger; level: Level; args: varargs[string, `$`]) =
  ## Logs to the logger's file, if `level` passes both the logger's threshold
  ## and the global log filter.
  if level >= logFilter and level >= logger.levelThreshold:
    writeLine(logger.file, formatEntry(logger.fmtStr, level, concatArgs(args)))
    if level >= logger.flushThreshold: flushFile(logger.file)

proc defaultFilename*(): string =
  ## Returns the filename used by default: the application's name with the
  ## `.log` extension.
  let (path, name, _) = splitFile(getAppFilename())
  result = changeFileExt(path / name, "log")

proc newFileLogger*(file: File; levelThreshold = lvlAll;
                    fmtStr = defaultFmtStr;
                    flushThreshold = defaultFlushThreshold): FileLogger =
  ## Creates a new FileLogger that logs to an already open `file`.
  result = FileLogger(levelThreshold: levelThreshold, fmtStr: fmtStr,
                      file: file, flushThreshold: flushThreshold)

proc newFileLogger*(filename = defaultFilename(); mode: FileMode = fmAppend;
                    levelThreshold = lvlAll; fmtStr = defaultFmtStr;
                    bufSize: int = -1;
                    flushThreshold = defaultFlushThreshold): FileLogger =
  ## Creates a new FileLogger that logs to the file `filename`. By default
  ## the file is appended to, so the log file stays one line-based NIF
  ## module across runs.
  let file = open(filename, mode, bufSize = bufSize)
  result = newFileLogger(file, levelThreshold, fmtStr, flushThreshold)

proc countLogLines(logger: RollingFileLogger): int =
  result = 0
  for line in lines(logger.baseName):
    inc result

proc countFiles(filename: string): int =
  # Example: file.log.1
  result = 0
  var (dir, name, ext) = splitFile(filename)
  if dir == "":
    dir = "."
  let prefix = name & ext & ExtSep
  try:
    for kind, path in walkDir(path(dir), relative = true):
      if kind == pcFile:
        let fn = $path
        if fn.startsWith(prefix):
          let num = parseInt(fn.substr(prefix.len))
          if num > result:
            result = num
  except:
    discard

proc newRollingFileLogger*(filename = defaultFilename();
                           mode: FileMode = fmReadWrite;
                           levelThreshold = lvlAll;
                           fmtStr = defaultFmtStr;
                           maxLines: Positive = 1000;
                           bufSize: int = -1;
                           flushThreshold = defaultFlushThreshold): RollingFileLogger =
  ## Creates a new RollingFileLogger. Once the current file reaches
  ## `maxLines` entries, it is renamed to `filename.1` (an existing
  ## `filename.1` becomes `filename.2`, and so on) and a new file is started.
  result = RollingFileLogger(levelThreshold: levelThreshold, fmtStr: fmtStr,
    flushThreshold: flushThreshold, maxLines: maxLines, bufSize: bufSize,
    file: open(filename, mode, bufSize = bufSize), curLine: 0,
    baseName: filename, baseMode: mode, logFiles: countFiles(filename))
  if mode == fmAppend:
    # We need to get a line count because we will be appending to the file.
    result.curLine = countLogLines(result)

proc rotate(logger: RollingFileLogger) =
  let (dir, name, ext) = splitFile(logger.baseName)
  for i in countdown(logger.logFiles, 0):
    let srcSuff = if i != 0: ExtSep & $i else: ""
    try:
      discard tryMoveFSObject(dir / (name & ext & srcSuff),
                              dir / (name & ext & ExtSep & $(i+1)), false)
    except:
      discard

method log*(logger: RollingFileLogger; level: Level; args: varargs[string, `$`]) =
  ## Logs to the logger's file, starting a new file if the current one is
  ## full, if `level` passes both the logger's threshold and the global log
  ## filter.
  if level >= logFilter and level >= logger.levelThreshold:
    if logger.curLine >= logger.maxLines:
      logger.file.close()
      rotate(logger)
      inc logger.logFiles
      logger.curLine = 0
      logger.file = open(logger.baseName, logger.baseMode,
                         bufSize = logger.bufSize)
    writeLine(logger.file, formatEntry(logger.fmtStr, level, concatArgs(args)))
    if level >= logger.flushThreshold: flushFile(logger.file)
    inc logger.curLine

# ── The logging API ──────────────────────────────────────────────────────

proc logLoop(level: Level; msg: string) =
  for logger in items(handlers):
    if level >= logger.levelThreshold:
      log(logger, level, msg)

proc getLogFilter*(): Level =
  ## Gets the global log filter.
  result = logFilter

proc setLogFilter*(lvl: Level) =
  ## Sets the global log filter: messages below `lvl` are not logged by any
  ## handler.
  logFilter = lvl

template log*(level: Level; args: varargs[string, `$`]) =
  ## Logs a message at `level` to all registered handlers.
  let lvl = level
  if lvl >= getLogFilter():
    var msg = ""
    for a in unpack(): msg.add a
    logLoop(lvl, msg)

template debug*(args: varargs[string, `$`]) =
  ## Logs a debug message to all registered handlers.
  if lvlDebug >= getLogFilter():
    var msg = ""
    for a in unpack(): msg.add a
    logLoop(lvlDebug, msg)

template info*(args: varargs[string, `$`]) =
  ## Logs an info message to all registered handlers.
  if lvlInfo >= getLogFilter():
    var msg = ""
    for a in unpack(): msg.add a
    logLoop(lvlInfo, msg)

template notice*(args: varargs[string, `$`]) =
  ## Logs a notice to all registered handlers.
  if lvlNotice >= getLogFilter():
    var msg = ""
    for a in unpack(): msg.add a
    logLoop(lvlNotice, msg)

template warn*(args: varargs[string, `$`]) =
  ## Logs a warning to all registered handlers.
  if lvlWarn >= getLogFilter():
    var msg = ""
    for a in unpack(): msg.add a
    logLoop(lvlWarn, msg)

template error*(args: varargs[string, `$`]) =
  ## Logs an error to all registered handlers.
  if lvlError >= getLogFilter():
    var msg = ""
    for a in unpack(): msg.add a
    logLoop(lvlError, msg)

template fatal*(args: varargs[string, `$`]) =
  ## Logs a fatal error to all registered handlers. This does not quit the
  ## program.
  if lvlFatal >= getLogFilter():
    var msg = ""
    for a in unpack(): msg.add a
    logLoop(lvlFatal, msg)

proc addHandler*(handler: Logger) =
  ## Adds a handler to the list of handlers of the current thread.
  handlers.add(handler)

proc removeHandler*(handler: Logger) =
  ## Removes a handler from the list of handlers of the current thread.
  for i in 0 ..< handlers.len:
    if handlers[i] == handler:
      handlers.delete(i)
      return

proc getHandlers*(): seq[Logger] =
  ## Returns the list of handlers of the current thread.
  result = handlers

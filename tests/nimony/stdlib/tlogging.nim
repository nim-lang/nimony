# Every entry `std/logging` writes is a line of line-based NIF: read a log
# back with `nifreader` and check that each entry is the node it should be.

import std / [syncio, assertions, logging, dirs, paths]
import ".." / ".." / ".." / src / lib / nifreader

assert substituteLog(defaultFmtStr, lvlInfo, "a message") == "info \"a message\""
assert substituteLog("$levelid $levelname", lvlError, "an ", "error", 3) ==
  "error E ERROR \"an error3\""
assert substituteLog("", lvlWarn, "say \"hi\"\n\\") == "warn \"say \\^hi\\^\\n\\|\""
# words of the format string are strings; bare where they read back as one
assert substituteLog("a/b.txt key (x) x-y", lvlDebug, "") ==
  "debug a/b.txt \"key\" \"(x)\" x-y \"\""

let logFile = "tlogging_test.log"
discard tryRemoveFile(path(logFile))

block:
  let L = newFileLogger(logFile, fmWrite, fmtStr = verboseFmtStr & " $date $time $app")
  addHandler(L)
  info("hello ", 42)
  warn("tricky: (a) \"b\" \\c #d# @e ~f\nsecond line")
  debug("")
  setLogFilter(lvlWarn)
  info("filtered out")
  error("still logged")
  setLogFilter(lvlAll)
  removeHandler(L)
  info("no handler")
  L.file.close()

type Entry = object
  tag: string
  fields: seq[string]  # `kind:text` of each child but the message

proc readLog(filename: string): seq[Entry] =
  var r = nifreader.open(filename, lineBased = true)
  result = @[]
  var tok = default(ExpandedToken)
  var e = Entry(tag: "", fields: @[])
  var nested = 0
  while true:
    r.next(tok)
    case tok.tk
    of EofToken: break
    of ParLe:
      assert nested == 0
      e = Entry(tag: decodeStr(r, tok), fields: @[])
      inc nested
    of ParRi:
      dec nested
      result.add e
    of StrLit:
      e.fields.add "s:" & decodeStr(r, tok)
    of Ident:
      e.fields.add "i:" & decodeStr(r, tok)
    else:
      assert false, "unexpected token " & $tok.tk
  assert nested == 0
  r.close()

let entries = readLog(logFile)
assert entries.len == 4
assert entries[0].tag == "info"
assert entries[1].tag == "warn"
assert entries[2].tag == "debug"
assert entries[3].tag == "error"
for e in entries:
  # $datetime $appname $date $time $app, then the message
  assert e.fields.len == 6
  assert e.fields[0].len == len("s:2026-09-29T09:14:03Z")
  assert e.fields[0][len("s:2026-09-29")] == 'T'
  assert e.fields[1] == "s:tlogging"
  assert e.fields[2].len == len("s:2026-09-29")
  assert e.fields[3].len == len("s:09:14:03")
  assert e.fields[4][0 .. 1] == "s:"
assert entries[0].fields[5] == "s:hello 42"
assert entries[1].fields[5] == "s:tricky: (a) \"b\" \\c #d# @e ~f\nsecond line"
assert entries[2].fields[5] == "s:"
assert entries[3].fields[5] == "s:still logged"

discard tryRemoveFile(path(logFile))

# A rolling logger starts a new file every `maxLines` entries; every file is
# a line-based NIF module of its own.
block:
  let base = "tlogging_rolling.log"
  for f in [base, base & ".1", base & ".2"]: discard tryRemoveFile(path(f))
  let R = newRollingFileLogger(base, maxLines = 2, fmtStr = "$levelid")
  addHandler(R)
  for i in 1 .. 5: info("entry ", i)
  removeHandler(R)
  R.file.close()
  let newest = readLog(base)
  assert newest.len == 1 and newest[0].fields == @["i:I", "s:entry 5"]
  let middle = readLog(base & ".1")
  assert middle.len == 2 and middle[1].fields == @["i:I", "s:entry 4"]
  let oldest = readLog(base & ".2")
  assert oldest.len == 2 and oldest[0].fields == @["i:I", "s:entry 1"]
  for f in [base, base & ".1", base & ".2"]: discard tryRemoveFile(path(f))

# The console logger writes the same format.
let C = newConsoleLogger(fmtStr = "$levelname")
addHandler(C)
notice("to stdout")
fatal("the end (", 1, ")")

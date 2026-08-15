import std/[assertions, parseopt]

# The parsing rules are exercised through `initOptParser(seq[string])`, which
# takes an explicit argument list. Poking the process argv the entry point
# received is not portable: on Windows there is none — `std/cmdline` asks
# `GetCommandLineW` and splits it itself. Only the first block, which needs no
# arguments, goes through the real process vector.

block:
  assert paramCount() == 0

  var p = initOptParser()
  next(p)

  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["a"])
  next(p)
  assert p.kind == cmdArgument
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["xyz"])
  next(p)
  assert p.kind == cmdArgument
  assert p.key == "xyz"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["abc", "def"])
  next(p)
  assert p.kind == cmdArgument
  assert p.key == "abc"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdArgument
  assert p.key == "def"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["-a"])
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["-abc:12"])
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "b"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "c"
  assert p.val ==  "12"
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["-a", "-b"])
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "b"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["-ab:", "12"])
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "b"
  assert p.val ==  "12"
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["-a=12", "-b=", "c"])
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  "12"
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "b"
  assert p.val ==  "c"
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["--a"])
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["--abc"])
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "abc"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["--abc:12"])
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "abc"
  assert p.val ==  "12"
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["--abc=123", "--def=", "xyz"])
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "abc"
  assert p.val ==  "123"
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "def"
  assert p.val ==  "xyz"
  next(p)
  assert p.kind == cmdEnd

block:
  var p = initOptParser(@["--abc", "-aaa", "arg0", "--foo:bar", "arg1", "-a:1",
                          "--xyz=", "qwe"])
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "abc"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdArgument
  assert p.key == "arg0"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "foo"
  assert p.val ==  "bar"
  next(p)
  assert p.kind == cmdArgument
  assert p.key == "arg1"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  "1"
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "xyz"
  assert p.val ==  "qwe"
  next(p)
  assert p.kind == cmdEnd

block getopt_over_a_parser:
  # Nim's `getopt(p: var OptParser)` overload: build the parser yourself, then
  # iterate it. Without it, `for kind, key, val in getopt(p)` fails overload
  # resolution and the follow-on error is a misleading "not all cases are
  # covered" on the `case kind` inside the loop.
  var p = initOptParser(@["--abc:12", "arg0", "-a"])
  var kinds: seq[CmdLineKind] = @[]
  var keys: seq[string] = @[]
  var vals: seq[string] = @[]
  for kind, key, val in getopt(p):
    kinds.add kind
    keys.add key
    vals.add val
  assert kinds == @[cmdLongOption, cmdArgument, cmdShortOption]
  assert keys == @["abc", "arg0", "a"]
  assert vals == @["12", "", ""]

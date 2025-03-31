import std/[assertions, parseopt]

var
  nifcArgc {.importc: "cmdCount".}: int32
  nifcArgv {.importc: "cmdLine".}: ptr UncheckedArray[cstring]

block:
  nifcArgc = 1

  assert paramCount() == 0

  var p = initOptParser()
  next(p)

  assert p.kind == cmdEnd

block:
  nifcArgc = 2
  const cargv = [cstring"exe", cstring"a"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 1

  var p = initOptParser()
  next(p)
  assert p.kind == cmdArgument
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  nifcArgc = 2
  const cargv = [cstring"exe", cstring"xyz"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 1

  var p = initOptParser()
  next(p)
  assert p.kind == cmdArgument
  assert p.key == "xyz"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  nifcArgc = 3
  const cargv = [cstring"exe", cstring"abc", cstring"def"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 2

  var p = initOptParser()
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
  nifcArgc = 2
  const cargv = [cstring"exe", cstring"-a"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 1

  var p = initOptParser()
  next(p)
  assert p.kind == cmdShortOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  nifcArgc = 2
  const cargv = [cstring"exe", cstring"-abc:12"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 1

  var p = initOptParser()
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
  nifcArgc = 3
  const cargv = [cstring"exe", cstring"-a", cstring"-b"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 2

  var p = initOptParser()
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
  nifcArgc = 3
  const cargv = [cstring"exe", cstring"-ab:", cstring"12"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 2

  var p = initOptParser()
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
  nifcArgc = 4
  const cargv = [cstring"exe", cstring"-a=12", cstring"-b=", "c"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 3

  var p = initOptParser()
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
  nifcArgc = 2
  const cargv = [cstring"exe", cstring"--a"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 1

  var p = initOptParser()
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "a"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  nifcArgc = 2
  const cargv = [cstring"exe", cstring"--abc"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 1

  var p = initOptParser()
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "abc"
  assert p.val ==  ""
  next(p)
  assert p.kind == cmdEnd

block:
  nifcArgc = 2
  const cargv = [cstring"exe", cstring"--abc:12"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 1

  var p = initOptParser()
  next(p)
  assert p.kind == cmdLongOption
  assert p.key == "abc"
  assert p.val ==  "12"
  next(p)
  assert p.kind == cmdEnd

block:
  nifcArgc = 4
  const cargv = [cstring"exe", cstring"--abc=123", cstring"--def=", cstring"xyz"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 3

  var p = initOptParser()
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
  nifcArgc = 9
  const cargv = [cstring"exe", cstring"--abc", cstring"-aaa", cstring"arg0", cstring"--foo:bar", cstring"arg1", cstring"-a:1", cstring"--xyz=", cstring"qwe"]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  assert paramCount() == 8

  var p = initOptParser()
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

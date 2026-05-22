import std / syncio

proc echoSum*(args: varargs[int]): int =
  result = 0
  for a in args: result += a

proc sumWith*(base: int; args: varargs[int]): int =
  result = base
  for a in args: result += a

proc say*(args: varargs[string, `$`]) =
  for s in args: echo s

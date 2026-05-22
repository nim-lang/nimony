proc echoSum*(args: varargs[int]): int =
  result = 0
  for a in args: result += a

proc sumWith*(base: int; args: varargs[int]): int =
  result = base
  for a in args: result += a

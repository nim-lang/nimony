# Helper routines for reading the arguments of a compile-time call. Used by
# the `exprexec` module: a const-eval program for a call is compiled once per
# callee signature and reads the argument values of each individual call from
# the file `paramStr(1)`, writing the result to `paramStr(2)` via `writenif`.
#
# The input is not NIF but a flat stream that mirrors the argument types, so
# the reader needs no tree structure: ordinals are decimal integers, floats are
# their IEEE bit pattern as an unsigned integer and strings are `len:bytes`.
# Every atom is followed by a single space.

# `inp` belongs to the compile-time evaluator, which runs single threaded.
{.feature: "assumeSync".}

import std/[syncio, cmdline]
import std/writenif

var inp: string
var pos: int

proc setupCall*() =
  ## Reads the argument file and opens the result file.
  try:
    inp = readFile(paramStr(1))
  except:
    inp = ""
  pos = 0
  setup(paramStr(2))

proc readNifUInt*(): uint =
  result = 0'u
  while pos < inp.len and inp[pos] in {'0'..'9'}:
    result = result * 10'u + uint(ord(inp[pos]) - ord('0'))
    inc pos
  inc pos # the separator

proc readNifInt*(): int =
  var neg = false
  if pos < inp.len and inp[pos] == '-':
    neg = true
    inc pos
  let u = readNifUInt()
  # `0 - u` keeps `low(int)` representable
  result = if neg: cast[int](0'u - u) else: cast[int](u)

proc readNifFloat*(): float =
  result = cast[float](readNifUInt())

proc readNifBool*(): bool =
  result = readNifUInt() != 0'u

proc readNifChar*(): char =
  result = char(readNifUInt())

proc readNifStr*(): string =
  let n = int(readNifUInt())
  result = newString(n)
  for i in 0 ..< n:
    result[i] = inp[pos+i]
  inc pos, n + 1 # bytes and the separator

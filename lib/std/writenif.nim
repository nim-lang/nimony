# Helper routines for generating NIF code on stdout. Used by the `exprexec` module.

import std/[syncio, math, formatfloat]

# all atoms must start with a space, keeps the logic simple

proc writeNifFloat*(f: float) =
  case classify(f)
  of fcInf: write(stdout, "(inf)")
  of fcNan: write(stdout, "(nan)")
  of fcNegInf: write(stdout, "(neginf)")
  of fcNegZero: write(stdout, " -0.0")
  of fcNormal, fcSubnormal, fcZero:
    if f >= 0.0:
      write(stdout, " +")
    else:
      write(stdout, " ")
    var buf = newStringOfCap(32)
    buf.addFloat f
    for i in 0 ..< buf.len:
      if buf[i] == 'e': buf[i] = 'E'
    write(stdout, buf)

proc writeNifInt*(i: int) =
  if i >= 0:
    write(stdout, " +")
  else:
    write(stdout, " ")
  write(stdout, i)

proc writeNifUInt*(u: uint) =
  write(stdout, " +")
  write(stdout, u)
  write(stdout, "u")

proc writeNifBool*(b: bool) =
  if b:
    write(stdout, "(true)")
  else:
    write(stdout, "(false)")

const
  ControlChars* = {'(', ')', '[', ']', '{', '}', '~', '#', '\'', '"', '\\', ':'}

proc escape(c: char) =
  const HexChars = "0123456789ABCDEF"
  var n = int(c)
  write(stdout, "\\")
  write(stdout, HexChars[n shr 4 and 0xF])
  write(stdout, HexChars[n and 0xF])

template needsEscape(c: char): bool = c < ' ' or c in ControlChars

proc writeNifChar*(c: char) =
  write(stdout, " '")
  if c.needsEscape:
    escape c
  else:
    write(stdout, c)
  write(stdout, "'")

proc writeNifRaw*(s: string) =
  write(stdout, s)

proc writeNifStr*(s: string) =
  write(stdout, " \"")
  for c in s.items:
    if c.needsEscape:
      escape c
    else:
      write(stdout, c)
  write(stdout, "\"")

proc writeNifSymbol*(s: string) =
  if s.len > 0:
    let c = s[0]
    if c in {'.', '0'..'9', '+', '-', '~'} or c.needsEscape:
      escape c
    else:
      write(stdout, c)
    for i in 1..<s.len:
      let c = s[i]
      # Symbols imported from C can have a space like "struct foo".
      if c == ' ' or c.needsEscape:
        escape c
      else:
        write(stdout, c)

proc writeNifParLe*(tag: string) =
  write(stdout, "(")
  write(stdout, tag)

proc writeNifParRi*() =
  write(stdout, ")")

# Helper routines for generating NIF code. Used by the `exprexec` module.

import std/[syncio, math, formatfloat]

var outp: File

proc setup*(filename: string) =
  outp = open(filename, fmWrite)

proc teardown*() =
  close(outp)

# all atoms must start with a space, keeps the logic simple

proc writeNifFloat*(f: float) =
  case classify(f)
  of fcInf: write(outp, "(inf)")
  of fcNan: write(outp, "(nan)")
  of fcNegInf: write(outp, "(neginf)")
  of fcNegZero: write(outp, " -0.0")
  of fcNormal, fcSubnormal, fcZero:
    if f >= 0.0:
      write(outp, " +")
    else:
      write(outp, " ")
    var buf = newStringOfCap(32)
    buf.addFloat f
    for i in 0 ..< buf.len:
      if buf[i] == 'e': buf[i] = 'E'
    write(outp, buf)

proc writeNifInt*(i: int) =
  if i >= 0:
    write(outp, " +")
  else:
    write(outp, " ")
  write(outp, i)

proc writeNifUInt*(u: uint) =
  write(outp, " +")
  write(outp, u)
  write(outp, "u")

proc writeNifBool*(b: bool) =
  if b:
    write(outp, "(true)")
  else:
    write(outp, "(false)")

const
  ControlChars* = {'(', ')', '[', ']', '{', '}', '~', '#', '\'', '"', '\\', ':'}

proc escape(c: char) =
  const HexChars = "0123456789ABCDEF"
  var n = int(c)
  write(outp, "\\")
  write(outp, HexChars[n shr 4 and 0xF])
  write(outp, HexChars[n and 0xF])

template needsEscape(c: char): bool = c < ' ' or c in ControlChars

proc writeNifChar*(c: char) =
  write(outp, " '")
  if c.needsEscape:
    escape c
  else:
    write(outp, c)
  write(outp, "'")

proc writeNifRaw*(s: string) =
  write(outp, s)

proc writeNifStr*(s: string) =
  write(outp, " \"")
  for c in s.items:
    if c.needsEscape:
      escape c
    else:
      write(outp, c)
  write(outp, "\"")

proc writeNifSymbol*(s: string) =
  if s.len > 0:
    let c = s[0]
    if c in {'.', '0'..'9', '+', '-', '~'} or c.needsEscape:
      escape c
    else:
      write(outp, c)
    for i in 1..<s.len:
      let c = s[i]
      # Symbols imported from C can have a space like "struct foo".
      if c == ' ' or c.needsEscape:
        escape c
      else:
        write(outp, c)

proc writeNifParLe*(tag: string) =
  write(outp, "(")
  write(outp, tag)

proc writeNifParRi*() =
  write(outp, ")")

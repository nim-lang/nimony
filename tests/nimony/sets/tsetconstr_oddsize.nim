import std/syncio

# A set constructor with RUNTIME elements for a set whose size is not a machine
# word: 33 elements need 5 bytes, which is a byte array, but the constructor was
# built as if it were an integer (a C `NU40`, which does not exist).

type Reg = enum
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15,
  R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30,
  SP, NoReg

proc pick(on: bool; r: Reg): set[Reg] =
  result = if on: {r} else: {}

proc range3(a: Reg): set[Reg] =
  result = {a .. Reg(ord(a) + 2), NoReg}

let s = pick(true, R29)
echo R29 in s, " ", R28 in s, " ", card(s)
echo card(pick(false, R3))
let t = range3(R30)
echo R30 in t, " ", SP in t, " ", NoReg in t, " ", R0 in t, " ", card(t)

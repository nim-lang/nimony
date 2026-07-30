## Object construction for a case object with a *range* branch (`of a..b:`).
## The branch bounds are constant-folded to decide which branch a construction
## selects; that evaluation used to leave its scratch output inside the
## `(oconstr …)` being built, which made every later pass -- they expect nothing
## but `kv` children there -- crash.

import std/assertions

type
  Kind = enum
    kBin, kZext, kSext, kTrunc, kRet

  Instr = object
    label: string
    case kind: Kind
    of kBin:
      binOp: string
    of kZext..kTrunc:
      castOp: string
    of kRet:
      retVal: int

proc mkBin(op: string): Instr =
  Instr(label: "", kind: kBin, binOp: op)

proc mkSext(op: string): Instr =
  Instr(label: "l", kind: kSext, castOp: op)

let a = mkBin("add")
assert a.kind == kBin
assert a.binOp == "add"
assert a.label == ""

# a value from the middle of the range picks the range branch:
let b = mkSext("sext")
assert b.kind == kSext
assert b.castOp == "sext"
assert b.label == "l"

# and so do both of its ends:
let c = Instr(label: "c", kind: kZext, castOp: "zext")
assert c.castOp == "zext"
let d = Instr(label: "d", kind: kTrunc, castOp: "trunc")
assert d.castOp == "trunc"

let e = Instr(label: "e", kind: kRet, retVal: 42)
assert e.retVal == 42

# the discriminator may also be left out entirely: the first branch wins
let f = Instr(label: "f")
assert f.kind == kBin
assert f.binOp == ""

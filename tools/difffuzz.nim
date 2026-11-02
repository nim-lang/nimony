## Differential fuzzer: `nimony c` against `nimony n`.
##
## Generates random, UB-free Nimony programs from a seed, compiles each with
## the C backend (gcc: the oracle) and the native backend (arkham + nifasm),
## runs both and compares their output. Every value is printed as its BIT
## PATTERN, so a wrong sign extension, a lost `-0.0` or a different NaN shows
## up as a mismatch, not as a rounding question.
##
## What a program may contain is chosen so that both backends must agree:
##
## * integer ARITHMETIC only on unsigned types (it wraps by definition);
##   signed values come from casts, widening conversions and literals, and
##   are only compared, widened, cast back or converted to float;
## * divisors are forced nonzero, shift counts and array indices are masked;
## * no float -> int conversion (out of range is undefined in C);
## * floats are free: NaN, infinities and subnormals are exactly what we want.
##
## A program is many independent `t<i>` procs. Each proc is generated from its
## own seed and prints one line, so a mismatch names the proc. The reducer
## then drops every other proc and deletes statements while the mismatch
## persists (all locals are declared at the top, so any statement can go).
##
##   nim c -d:release -o:bin/difffuzz tools/difffuzz.nim
##   bin/difffuzz --count:20                  # 20 random programs
##   bin/difffuzz --seed:1234 --count:1       # reproduce one
##   bin/difffuzz --flags:-d:release          # other optimization level
##
## Findings (original + reduced program, both outputs) go to `--out:DIR`
## (default `difffuzz.out`).

import std / [os, osproc, strutils, random, times, strformat, parseopt]

type
  Ty = enum tU64, tU32, tU8, tI32, tI64, tF64, tF32, tBool, tObj  ## tObj: the object type `V`

  Node = ref object
    line: string                 ## the statement, or an `if`/`for` header
    kids: seq[Node]              ## body of `if`/`for`
    elseKids: seq[Node]          ## `else` body of an `if`
    hasElse: bool

  Helper = object
    name, decl: string
    params: seq[Ty]
    ret: Ty

  Gen = object
    r: Rand
    vars: seq[(string, Ty)]      ## scalar locals and parameters in scope
    loopVars: seq[string]        ## `for` counters in scope (int)
    helpers: seq[Helper]
    depth: int
    haveObjs: bool               ## `v`, `w`, `arr` are declared (not in helpers,
                                 ## not in the locals' initializers)

  Program = object
    helpers: seq[Helper]
    procs: seq[seq[Node]]        ## statement trees, one per `t<i>`
    procHeads: seq[seq[string]]  ## local declarations, one block per `t<i>`
    procPrinted: seq[seq[string]] ## what the final `echo` of each `t<i>` prints
    calls: seq[string]           ## `t<i>(…)` call per proc

  Outcome = enum
    okSame, mismatch, nativeCompileFails, nativeRunFails, invalid

  Result = object
    outcome: Outcome
    detail, outC, outN: string
    badProc: int

const
  VType = tObj
  ParamDecl = "p0: uint64; p1: float64; p2: uint32; p3: float32; p4: int32"
  Params = [("p0", tU64), ("p1", tF64), ("p2", tU32), ("p3", tF32), ("p4", tI32)]

proc tyName(t: Ty): string =
  case t
  of tU64: "uint64"
  of tU32: "uint32"
  of tU8: "uint8"
  of tI32: "int32"
  of tI64: "int64"
  of tF64: "float64"
  of tF32: "float32"
  of tBool: "bool"
  of tObj: "V"

proc suffix(t: Ty): string =
  case t
  of tU64: "'u64"
  of tU32: "'u32"
  of tU8: "'u8"
  of tI32: "'i32"
  of tI64: "'i64"
  of tF32: "'f32"
  else: ""

proc bitsOf(t: Ty; e: string): string =
  ## The printed form of a value: its bit pattern, never a formatted float.
  case t
  of tU64, tU32, tU8: e
  of tI32: "cast[uint32](" & e & ")"
  of tI64: "cast[uint64](" & e & ")"
  # A NaN prints canonically: IEEE 754 leaves its sign and payload to the
  # implementation (x86 `sqrtsd` makes a negative default NaN where gcc's
  # constant folder makes a positive one), so they are not a finding.
  of tF64: "(if " & e & " != " & e & ": 0x7ff8000000000000'u64 else: cast[uint64](" & e & "))"
  of tF32: "(if " & e & " != " & e & ": 0x7fc00000'u32 else: cast[uint32](" & e & "))"
  of tBool: "ord(" & e & ")"
  of tObj: e

# ---- literals ---------------------------------------------------------------

proc pick[T](g: var Gen; xs: openArray[T]): T = xs[g.r.rand(xs.len - 1)]
proc chance(g: var Gen; p: float): bool = g.r.rand(1.0) < p

proc lit(g: var Gen; t: Ty): string =
  case t
  of tU64:
    let v = g.pick([0'u64, 1, 2, 3, 7, 255, 0x7fffffff, 0x80000000'u64,
                    0xffffffff'u64, 0x100000000'u64, 0x7fffffffffffffff'u64,
                    0x8000000000000000'u64, 0xffffffffffffffff'u64,
                    g.r.next(), g.r.next() shr 33])
    "0x" & toHex(v) & "'u64"
  of tU32:
    let v = g.pick([0'u32, 1, 2, 5, 255, 0x7fff, 0x8000, 0x7fffffff,
                    0x80000000'u32, 0xffffffff'u32, uint32(g.r.next() and 0xffffffff'u64)])
    "0x" & toHex(v) & "'u32"
  of tU8:
    $g.pick([0, 1, 2, 127, 128, 200, 255, g.r.rand(255)]) & "'u8"
  of tI32:
    "(" & $g.pick([0'i64, 1, -1, 2, -2, 0x7fffffff, -0x80000000'i64, 1000, -1000,
                   int64(g.r.rand(0x7fffffff)) - 0x40000000]) & "'i32)"
  of tI64:
    "(" & $g.pick([0'i64, 1, -1, high(int64), low(int64) + 1, 0x80000000'i64,
                   -0x80000000'i64, int64(g.r.next() shr 2) - (1'i64 shl 60)]) & "'i64)"
  of tF64:
    if g.chance(0.15):
      "cast[float64](0x" & toHex(g.pick([0x7ff0000000000000'u64, 0xfff0000000000000'u64,
        0x7ff8000000000000'u64, 0x7ff0000000000001'u64, 0x0000000000000001'u64,
        0x8000000000000000'u64, 0x000fffffffffffff'u64, g.r.next()])) & "'u64)"
    else:
      g.pick(["0.0", "1.0", "-1.0", "0.5", "-1.5", "2.5", "3.141592653589793",
              "1e300", "-1e300", "1e-310", "1e10", "0.1", "-7.25", "65536.0",
              "9007199254740993.0", "-2147483648.0", "4294967296.0"])
  of tF32:
    if g.chance(0.15):
      "cast[float32](0x" & toHex(g.pick([0x7f800000'u32, 0xff800000'u32,
        0x7fc00000'u32, 0x00000001'u32, 0x80000000'u32,
        uint32(g.r.next() and 0xffffffff'u64)])) & "'u32)"
    else:
      g.pick(["0.0", "1.0", "-1.0", "0.5", "2.5", "3.4e38", "1e-40", "-0.25", "16777217.0"]) & "'f32"
  of tBool:
    g.pick(["true", "false"])
  of tObj:
    "V(x: 1.0, y: -2.0, n: 3'u32)"

# ---- expressions ------------------------------------------------------------

proc expr(g: var Gen; t: Ty; d: int): string

proc leaf(g: var Gen; t: Ty): string =
  var cands: seq[string] = @[]
  for (n, vt) in g.vars:
    if vt == t: cands.add n
  if g.haveObjs:
    case t
    of tF64: cands.add ["v.x", "v.y", "w.x"]
    of tU32: cands.add ["v.n", "w.n"]
    of tU64: cands.add "arr[" & "(" & g.pick(["p0", "a0", "0x5'u64"]) & ") and 7'u64]"
    else: discard
  if t == tU64:
    for lv in g.loopVars: cands.add "uint64(" & lv & ")"
  if cands.len == 0 or g.chance(0.25): g.lit(t)
  else: g.pick(cands)

proc callHelper(g: var Gen; t: Ty; d: int): string =
  ## A call to a generated helper returning `t`, or "" when there is none.
  var hs: seq[int] = @[]
  for i, h in g.helpers:
    if h.ret == t: hs.add i
  if hs.len == 0: return ""
  let h = g.helpers[g.pick(hs)]
  var args: seq[string] = @[]
  for p in h.params:
    args.add(if p != VType: g.expr(p, d - 1)
             elif g.haveObjs: g.pick(["v", "w"])
             else: "V(x: 1.0, y: -2.0, n: 3'u32)")
  h.name & "(" & args.join(", ") & ")"

proc cond(g: var Gen; d: int): string =
  g.expr(tBool, d)

proc nanFreeBits(g: var Gen; t: Ty; x: string): string =
  ## The bit pattern of a float as an INTEGER value, with NaN canonicalized: a
  ## NaN's sign is unspecified (gcc may fold `-(a*b)` to `(-a)*b`), and once it
  ## is an integer no printing can hide it any more.
  if t == tF64: "(if " & x & " != " & x & ": 0x7ff8000000000000'u64 else: cast[uint64](" & x & "))"
  else: "(if " & x & " != " & x & ": 0x7fc00000'u32 else: cast[uint32](" & x & "))"

proc expr(g: var Gen; t: Ty; d: int): string =
  if d <= 0 or g.chance(0.2): return g.leaf(t)
  template e(tt: Ty): string = g.expr(tt, d - 1)
  if g.chance(0.08):
    return "(if " & g.cond(d - 1) & ": " & e(t) & " else: " & e(t) & ")"
  if g.chance(0.06):
    let c = g.callHelper(t, d)
    if c.len > 0: return c
  case t
  of tU64, tU32, tU8:
    let s = suffix(t)
    let mask = (case t
      of tU64: "63"
      of tU32: "31"
      else: "7") & s
    case g.r.rand(13)
    of 0..5:
      "(" & e(t) & " " & g.pick(["+", "-", "*", "xor", "and", "or"]) & " " & e(t) & ")"
    of 6: "(" & e(t) & " " & g.pick(["shl", "shr"]) & " (" & e(t) & " and " & mask & "))"
    of 7: "(" & e(t) & " " & g.pick(["div", "mod"]) & " (" & e(t) & " or 1" & s & "))"
    of 8:
      case t
      of tU64: g.pick(["uint64(" & e(tU32) & ")", "uint64(" & e(tU8) & ")",
                       g.nanFreeBits(tF64, e(tF64)), "cast[uint64](" & e(tI64) & ")"])
      of tU32: g.pick(["uint32(" & e(tU64) & " and 0xffffffff'u64)", "uint32(" & e(tU8) & ")",
                       "cast[uint32](" & e(tI32) & ")", g.nanFreeBits(tF32, e(tF32))])
      else: g.pick(["uint8(" & e(tU64) & " and 255'u64)", "uint8(" & e(tU32) & " and 255'u32)"])
    of 9: "(not " & e(t) & ")"
    of 10: tyName(t) & "(ord(" & g.cond(d - 1) & "))"
    else: g.leaf(t)
  of tI32:
    case g.r.rand(3)
    of 0, 1: "cast[int32](" & e(tU32) & ")"
    else: g.leaf(t)
  of tI64:
    case g.r.rand(4)
    of 0, 1: "int64(" & e(tI32) & ")"
    of 2: "cast[int64](" & e(tU64) & ")"
    else: g.leaf(t)
  of tF64:
    case g.r.rand(15)
    of 0..4: "(" & e(t) & " " & g.pick(["+", "-", "*", "/"]) & " " & e(t) & ")"
    of 5: "(-(" & e(t) & "))"
    of 6: g.pick(["float64(" & e(tU32) & ")", "float64(" & e(tI32) & ")",
                  "float64(" & e(tI64) & ")", "float64(" & e(tU64) & ")",
                  "float64(" & e(tF32) & ")"])
    of 7: "cast[float64](" & e(tU64) & ")"
    of 8: g.pick(["sqrt", "floor", "round", "trunc", "ceil", "abs"]) & "(" & e(t) & ")"
    of 9: g.pick(["min", "max"]) & "(" & e(t) & ", " & e(t) & ")"
    of 10: (if g.chance(0.3): "pow(" & e(t) & ", " & e(t) & ")" else: g.leaf(t))
    else: g.leaf(t)
  of tF32:
    case g.r.rand(9)
    of 0..3: "(" & e(t) & " " & g.pick(["+", "-", "*", "/"]) & " " & e(t) & ")"
    of 4: g.pick(["float32(" & e(tF64) & ")", "float32(" & e(tU32) & ")",
                  "float32(" & e(tI32) & ")"])
    of 5: "cast[float32](" & e(tU32) & ")"
    of 6: "sqrt(" & e(t) & ")"
    else: g.leaf(t)
  of tObj:
    g.leaf(t)
  of tBool:
    case g.r.rand(7)
    of 0..3:
      let ct = g.pick([tU64, tU32, tU8, tI32, tI64, tF64, tF32])
      "(" & e(ct) & " " & g.pick(["<", "<=", "==", "!=", ">", ">="]) & " " & e(ct) & ")"
    of 4: "(" & e(tBool) & " " & g.pick(["and", "or"]) & " " & e(tBool) & ")"
    of 5: "(not " & e(tBool) & ")"
    of 6:
      let ft = g.pick([tF64, tF32])
      let x = e(ft)
      "(" & x & " != " & x & ")"
    else: g.leaf(t)

# ---- statements ---------------------------------------------------------------

proc stmts(g: var Gen; n: int): seq[Node]

proc stmt(g: var Gen): Node =
  let d = 3
  let k = g.r.rand(99)
  if k < 10 and g.depth < 2:
    inc g.depth
    result = Node(line: "if " & g.cond(2) & ":", kids: g.stmts(1 + g.r.rand(3)))
    if g.chance(0.5):
      result.hasElse = true
      result.elseKids = g.stmts(1 + g.r.rand(2))
    dec g.depth
  elif k < 16 and g.depth < 2:
    inc g.depth
    let lv = "k" & $g.loopVars.len
    g.loopVars.add lv
    result = Node(line: "for " & lv & " in 0 ..< " & $(1 + g.r.rand(4)) & ":",
                  kids: g.stmts(1 + g.r.rand(3)))
    discard g.loopVars.pop()
    dec g.depth
  elif k < 24:
    result = Node(line: g.pick(["v", "w"]) & "." & g.pick(["x", "y"]) & " = " & g.expr(tF64, d))
  elif k < 28:
    result = Node(line: g.pick(["v", "w"]) & ".n = " & g.expr(tU32, d))
  elif k < 31:
    result = Node(line: g.pick(["v", "w"]) & " = V(x: " & g.expr(tF64, 2) & ", y: " &
                        g.expr(tF64, 2) & ", n: " & g.expr(tU32, 2) & ")")
  elif k < 33:
    result = Node(line: g.pick(["v = w", "w = v"]))
  elif k < 36:
    let c = g.callHelper(VType, d)
    result = Node(line: (if c.len > 0: g.pick(["v", "w"]) & " = " & c else: "v = w"))
  elif k < 41:
    result = Node(line: "arr[(" & g.expr(tU64, 2) & ") and 7'u64] = " & g.expr(tU64, d))
  else:
    let (n, t) = g.pick(g.vars)
    if n.startsWith("p"):                   # parameters are immutable
      result = Node(line: "a0 = " & g.expr(tU64, d))
    else:
      result = Node(line: n & " = " & g.expr(t, d))

proc stmts(g: var Gen; n: int): seq[Node] =
  for i in 0 ..< n: result.add g.stmt()

# ---- helpers and procs ----------------------------------------------------------

proc genHelper(g: var Gen; i: int): Helper =
  let shapes = [(@[tF64, tF64], tF64), (@[tU64, tU32], tU64), (@[VType, tF64], VType),
                (@[tF32, tI32], tF32), (@[tF64, VType, tU32], VType), (@[tI32, tU32], tI32)]
  let (ps, ret) = g.pick(shapes)
  result = Helper(name: "h" & $i, params: ps, ret: ret)
  g.vars.setLen 0
  var decl: seq[string] = @[]
  var objParam = ""
  for j, p in ps:
    if p == VType:
      decl.add "q" & $j & ": V"
      objParam = "q" & $j
    else:
      decl.add "q" & $j & ": " & tyName(p)
      g.vars.add ("q" & $j, p)
  let retName = if ret == VType: "V" else: tyName(ret)
  var body: string
  # helpers see no `v`/`w`/`arr`: rename those leaves to the object param (or a literal)
  if objParam.len > 0:
    g.vars.add (objParam & ".x", tF64)
    g.vars.add (objParam & ".n", tU32)
  if ret == VType:
    body = "V(x: " & g.expr(tF64, 2) & ", y: " & g.expr(tF64, 2) & ", n: " & g.expr(tU32, 2) & ")"
  else:
    body = g.expr(ret, 3)
  result.decl = "proc " & result.name & "(" & decl.join("; ") & "): " & retName & " =\n  " & body

proc genProc(g: var Gen; i: int): (seq[string], seq[Node], seq[string], string) =
  g.haveObjs = false
  g.vars.setLen 0
  for p in Params: g.vars.add p
  var head: seq[string] = @[]
  let locals = [tU64, tU64, tU32, tU8, tI32, tI64, tF64, tF64, tF32, tBool]
  var printed: seq[string] = @[]
  for j, t in locals:
    let n = "a" & $j
    let init = g.expr(t, 2)
    head.add "  var " & n & ": " & tyName(t) & " = " & init
    g.vars.add (n, t)
    printed.add bitsOf(t, n)
  head.add "  var v = V(x: " & g.expr(tF64, 1) & ", y: " & g.expr(tF64, 1) & ", n: " & g.expr(tU32, 1) & ")"
  head.add "  var w = V(x: " & g.expr(tF64, 1) & ", y: p1, n: p2)"
  var elems: seq[string] = @[]
  for k in 0 ..< 8: elems.add "p0 + " & $k & "'u64"
  head.add "  var arr: array[8, uint64] = [" & elems.join(", ") & "]"
  g.haveObjs = true
  let body = g.stmts(6 + g.r.rand(14))
  for f in ["v.x", "v.y", "w.x", "w.y"]: printed.add bitsOf(tF64, f)
  printed.add "v.n"
  printed.add "w.n"
  for j in 0 ..< 8: printed.add "arr[" & $j & "]"
  var call = "t" & $i & "(" & g.lit(tU64) & ", " & g.lit(tF64) & ", " & g.lit(tU32) &
             ", " & g.lit(tF32) & ", " & g.lit(tI32) & ")"
  (head, body, printed, call)

proc generate(seed, nprocs: int): Program =
  var g = Gen(r: initRand(seed))
  for i in 0 ..< 4: g.helpers.add g.genHelper(i)
  result.helpers = g.helpers
  for i in 0 ..< nprocs:
    g.r = initRand(seed * 1000 + i + 1)      # each proc reproducible on its own
    let (head, body, printed, call) = g.genProc(i)
    result.procHeads.add head
    result.procs.add body
    result.procPrinted.add printed
    result.calls.add call

# ---- rendering ------------------------------------------------------------------

proc render(n: Node; indent: int; res: var string) =
  let pad = repeat(' ', indent)
  res.add pad & n.line & "\n"
  if n.line.endsWith(":"):
    if n.kids.len == 0: res.add pad & "  discard\n"
    for k in n.kids: render(k, indent + 2, res)
    if n.hasElse:
      res.add pad & "else:\n"
      if n.elseKids.len == 0: res.add pad & "  discard\n"
      for k in n.elseKids: render(k, indent + 2, res)

proc render(p: Program; only: int = -1): string =
  result = "# generated by tools/difffuzz.nim\nimport std/[syncio, math]\n\n" &
           "type V = object\n  x, y: float64\n  n: uint32\n\n"
  for h in p.helpers: result.add h.decl & "\n\n"
  for i in 0 ..< p.procs.len:
    if only >= 0 and i != only: continue
    result.add "proc t" & $i & "(" & ParamDecl & ") =\n"
    for l in p.procHeads[i]: result.add l & "\n"
    for n in p.procs[i]: render(n, 2, result)
    result.add "  echo \"t" & $i & " \", " & p.procPrinted[i].join(", \" \", ") & "\n\n"
  result.add "proc main() =\n"
  for i in 0 ..< p.calls.len:
    if only >= 0 and i != only: continue
    result.add "  " & p.calls[i] & "\n"
  result.add "\nmain()\n"

# ---- running ----------------------------------------------------------------------

var nimonyExe = "bin/nimony"
var flags = "-d:danger"
var workDir = getTempDir() / "difffuzz"
var verbose = false

proc buildRun(src, backend, tag: string): (bool, bool, string) =
  ## (compiled, ran ok, output-or-error)
  let dir = workDir / tag / backend
  removeDir(dir)
  createDir(dir)
  let file = workDir / tag / "fz.nim"
  let cmd = quoteShell(nimonyExe) & " " & backend & " " & flags &
            " --nimcache:" & quoteShell(dir) & " " & quoteShell(file)
  let (outp, code) = execCmdEx(cmd)
  if code != 0: return (false, false, outp)
  var exe = ""
  for f in walkDirRec(dir):
    if f.extractFilename == "fz" and fpUserExec in getFilePermissions(f): exe = f
  if exe.len == 0: return (false, false, "no executable produced\n" & outp)
  let (r, rc) = execCmdEx(quoteShell(exe))
  (true, rc == 0, (if rc == 0: r else: r & "\nexit code " & $rc))

proc firstDiffProc(a, b: string): int =
  let la = a.splitLines
  let lb = b.splitLines
  for i in 0 ..< max(la.len, lb.len):
    let x = if i < la.len: la[i] else: ""
    let y = if i < lb.len: lb[i] else: ""
    if x != y:
      let s = if x.startsWith("t"): x else: y
      if s.startsWith("t"):
        let sp = s.find(' ')
        if sp > 1:
          try: return parseInt(s[1 ..< sp]) except ValueError: discard
      return -1
  -1

proc check(src, tag: string): Result =
  createDir(workDir / tag)
  writeFile(workDir / tag / "fz.nim", src)
  let (cc, cok, cout) = buildRun(src, "c", tag)
  if not cc or not cok:
    return Result(outcome: invalid, detail: "C backend: " & cout)
  let (nc, nok, nout) = buildRun(src, "n", tag)
  if not nc:
    # arkham/nifasm name the proc they failed in: `… in proc t7.0.fz…`.
    var bad = -1
    let k = nout.find("in proc t")
    if k >= 0:
      var j = k + "in proc t".len
      var num = ""
      while j < nout.len and nout[j] in {'0'..'9'}:
        num.add nout[j]; inc j
      if num.len > 0: bad = parseInt(num)
    return Result(outcome: nativeCompileFails, outC: cout, outN: nout, detail: nout, badProc: bad)
  if not nok:
    return Result(outcome: nativeRunFails, outC: cout, outN: nout, badProc: firstDiffProc(cout, nout))
  if cout != nout:
    return Result(outcome: mismatch, outC: cout, outN: nout, badProc: firstDiffProc(cout, nout))
  Result(outcome: okSame)

proc errorKey(s: string): string =
  ## The line that identifies a native compile failure, so reduction keeps
  ## chasing the SAME failure instead of drifting to another one.
  for l in s.splitLines:
    if "Error" in l or "Assertion" in l or "[Error]" in l: return l.strip
  s.strip

# ---- reduction -----------------------------------------------------------------------

proc sameFailure(a, b: Result): bool =
  if a.outcome != b.outcome: return false
  if a.outcome == nativeCompileFails: return errorKey(a.detail) == errorKey(b.detail)
  true

var reduceBudget = 150

proc reduce(p: var Program; target: Result; only: int; tag: string): string =
  ## Delete statements (and unwrap `if`/`for`) while the failure persists.
  var tests = 0
  proc still(p: Program): bool =
    inc tests
    if tests > reduceBudget: return false
    let r = check(render(p, only), tag)
    sameFailure(r, target)

  proc tryList(p: var Program; list: var seq[Node]): bool =
    result = false
    var i = 0
    while i < list.len:
      let saved = list[i]
      list.delete(i)
      if still(p):
        result = true
        continue
      list.insert(saved, i)
      if saved.kids.len > 0:
        # unwrap: replace the compound statement by its body
        list.delete(i)
        for j, k in saved.kids: list.insert(k, i + j)
        if still(p):
          result = true
          continue
        for j in 0 ..< saved.kids.len: list.delete(i)
        list.insert(saved, i)
        if tryList(p, saved.kids): result = true
        if saved.hasElse and tryList(p, saved.elseKids): result = true
      inc i

  proc tryHeads(p: var Program; pi: int): bool =
    ## Replace each local's initializer by a trivial value of its type.
    result = false
    for li in 0 ..< p.procHeads[pi].len:
      let line = p.procHeads[pi][li]
      let colon = line.find(": ")
      let eq = line.find(" = ")
      if not line.startsWith("  var ") or eq < 0: continue
      var simple = ""
      if line.startsWith("  var v = ") or line.startsWith("  var w = "):
        simple = line[0 ..< eq] & " = V(x: 0.0, y: 0.0, n: 0'u32)"
      elif colon > 0 and colon < eq:
        let tn = line[colon + 2 ..< eq]
        let zero = case tn
          of "uint64": "0'u64"
          of "uint32": "0'u32"
          of "uint8": "0'u8"
          of "int32": "0'i32"
          of "int64": "0'i64"
          of "float64": "0.0"
          of "float32": "0.0'f32"
          of "bool": "false"
          else: ""
        if zero.len > 0: simple = line[0 ..< eq] & " = " & zero
      if simple.len == 0 or simple == line: continue
      p.procHeads[pi][li] = simple
      if still(p): result = true
      else: p.procHeads[pi][li] = line

  # Print only the values that differ: everything else printed is a call (and
  # a live range) the miscompile may not need.
  if only >= 0 and target.outcome == mismatch:
    let la = target.outC.splitLines
    let lb = target.outN.splitLines
    for k in 0 ..< min(la.len, lb.len):
      if la[k] != lb[k] and la[k].startsWith("t" & $only & " "):
        let xa = la[k].split(' ')
        let xb = lb[k].split(' ')
        var keep: seq[string] = @[]
        for c in 1 ..< min(xa.len, xb.len):
          if xa[c] != xb[c] and c - 1 < p.procPrinted[only].len:
            keep.add p.procPrinted[only][c - 1]
        if keep.len > 0:
          let saved = p.procPrinted[only]
          p.procPrinted[only] = keep
          let r = check(render(p, only), tag)
          inc tests
          if r.outcome != mismatch: p.procPrinted[only] = saved
        break

  var changed = true
  while changed and tests <= reduceBudget:
    changed = false
    for pi in 0 ..< p.procs.len:
      if only >= 0 and pi != only: continue
      if tryList(p, p.procs[pi]): changed = true
      if tryHeads(p, pi): changed = true
  # Last, a plain line-level pass over the rendered text: it can delete what the
  # structured passes cannot (unused declarations, whole helpers), which is
  # often what pins down a register-allocation dependent failure.
  var lines = render(p, only).splitLines
  var i = 0
  while i < lines.len and tests <= reduceBudget:
    let l = lines[i]
    let keep = l.len == 0 or l.startsWith("import") or l.startsWith("type") or
               l.startsWith("  x, y") or l.startsWith("  n: uint32") or
               l.startsWith("proc t") or l.startsWith("proc main") or
               l.startsWith("main()") or l.startsWith("  echo") or
               (l.startsWith("  t") and "(" in l)
    if not keep:
      var cand = lines
      cand.delete(i)
      inc tests
      if sameFailure(check(cand.join("\n"), tag), target):
        lines = cand
        continue
    inc i
  lines.join("\n")

# ---- driver ------------------------------------------------------------------------

proc main() =
  var seed = int(epochTime() * 1000) mod 1_000_000
  var count = 10
  var nprocs = 12
  var outDir = "difffuzz.out"
  var doReduce = true
  for kind, key, val in getopt():
    case key
    of "seed": seed = parseInt(val)
    of "count": count = parseInt(val)
    of "procs": nprocs = parseInt(val)
    of "flags": flags = val
    of "nimony": nimonyExe = val
    of "out": outDir = val
    of "work": workDir = val
    of "budget": reduceBudget = parseInt(val)
    of "noreduce": doReduce = false
    of "verbose": verbose = true
    else: quit "unknown option: " & key
  nimonyExe = absolutePath(nimonyExe)
  createDir(outDir)
  var found = 0
  var invalidCount = 0
  for k in 0 ..< count:
    let s = seed + k
    var p = generate(s, nprocs)
    let tag = "s" & $s
    let r = check(render(p), tag)
    case r.outcome
    of okSame:
      echo &"seed {s}: same"
    of invalid:
      inc invalidCount
      echo &"seed {s}: INVALID (generator bug; C side failed)"
      writeFile(outDir / &"invalid_{s}.nim", render(p))
      writeFile(outDir / &"invalid_{s}.txt", r.detail)
    else:
      inc found
      echo &"seed {s}: {r.outcome} (proc t{r.badProc})"
      let base = outDir / &"{r.outcome}_{s}"
      writeFile(base & ".nim", render(p))
      writeFile(base & ".c.txt", r.outC)
      writeFile(base & ".n.txt", (if r.outcome == nativeCompileFails: r.detail else: r.outN))
      if doReduce:
        var only = r.badProc
        if only >= 0 and not sameFailure(check(render(p, only), tag), r): only = -1
        let reduced = reduce(p, r, only, tag)
        let rr = check(reduced, tag)
        writeFile(base & ".reduced.nim", reduced)
        writeFile(base & ".reduced.c.txt", rr.outC)
        writeFile(base & ".reduced.n.txt", (if rr.outcome == nativeCompileFails: rr.detail else: rr.outN))
        echo &"  reduced to {reduced.countLines} lines: {base}.reduced.nim"
  echo &"{count} programs, {found} findings, {invalidCount} invalid (generator) — {outDir}"
  if found > 0: quit 1

main()

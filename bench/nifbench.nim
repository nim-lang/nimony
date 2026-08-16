#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## nifbench — the compiler's NIF work, extracted into one small program.
##
## Every phase of the toolchain (nifler, nimsem, hexer, shoggoth, lengc,
## arkham) is the same handful of operations over a `nifcore.TokenBuf`: parse
## text into tokens, walk them with a cursor, splice subtrees into a new
## buffer, intern names, write the result back out. `nimsem` compiling itself
## is the honest benchmark of that, but it takes minutes to build and its
## profile mixes in file IO, process spawning and the semantic checker's own
## logic. `nifbench` is the same operations with none of that: one file, a
## couple of seconds to build, deterministic workload, per-phase timings.
##
## Two jobs:
##
## 1. **Iterate on codegen.** It compiles under host Nim, `nimony c` and
##    `nimony n`, so the arkham-vs-gcc gap can be measured per phase in
##    seconds instead of re-booting the compiler. The instruction mix is the
##    compiler's — pointer chasing over a flat `uint32` array, hash lookups,
##    small allocations — not a numeric kernel's.
##
## 2. **Benchmark nifcore itself.** NIF is not only the compiler's IR: the
##    stdlib's `json` (and `html`, `nifply`) map onto the *same* `TokenBuf`,
##    so `parse`/`walk`/`clone` throughput is a stdlib number, not just a
##    compiler one. The `json` bench measures that path directly.
##
## The workload is generated in-process from a fixed seed, so a run is
## reproducible on any machine without carrying a corpus around. Pass a real
## `.nif` file to benchmark that instead.
##
## Usage::
##
##   nifbench [options] [file.nif]
##
##   --only:SUBSTR   run only benchmarks whose name contains SUBSTR
##   --reps:N        repetitions per benchmark, best is reported (default 5)
##   --scale:N       workload size multiplier (default 1 ≈ 300k tokens)
##   --list          print the benchmark names and exit
##   --csv           emit `name,ns,units,unit-name` instead of a table

import std / [assertions, syncio, monotimes, parseopt, strutils, tables]
import ".." / src / lib / [nifcore, nifcoreparse, bif]

# The `bif` bench needs a scratch file, and that is the one place where the two
# stdlibs disagree: Nimony puts `removeFile` in `std/dirs` over a distinct
# `Path`, host Nim puts it in `std/os` over a `string`.
when defined(nimony):
  import std / [dirs, paths]
  proc deleteFile(p: string) =
    try: removeFile(path(p))
    except: discard
else:
  import std / os
  proc deleteFile(p: string) =
    try: removeFile(p)
    except: discard

proc toInt(s: string; fallback: int): int =
  ## `parseInt` raises, and in Nimony that is viral: leaving it uncaught would
  ## push `.raises` onto `main` and everything it touches. Caught at the leaf.
  try: result = parseInt(s)
  except: result = fallback

const BifScratch = "nifbench.scratch.bif"
  ## Relative to the working directory rather than the temp dir: `getTempDir`
  ## differs between the two stdlibs too, and a benchmark that writes into the
  ## directory you ran it from is easier to reason about than one that does not.

const
  Version = "0.1.0"
  BaseProcs = 900
    ## Toplevel decls at `--scale:1`. Chosen so the generated module lands near
    ## 300k tokens / 2 MB of text — the size of a big real module, and big
    ## enough that every phase runs well past timer noise at one iteration.

# ── deterministic pseudo-randomness ─────────────────────────────────────────
# `Math.random` would make runs incomparable, and a table of canned values
# would make the workload periodic in a way the branch predictor learns. A
# 64-bit LCG (the constants are Knuth's MMIX) is neither, and it is three
# instructions, so the generator never dominates what it is generating.

type Gen = object
  s: uint64

proc nextInt(g: var Gen): int =
  g.s = g.s * 6364136223846793005'u64 + 1442695040888963407'u64
  result = int((g.s shr 33'u64) and 0x7FFF_FFFF'u64)

proc below(g: var Gen; n: int): int = g.nextInt() mod n

# ── the tag vocabulary ──────────────────────────────────────────────────────
# Real tag spellings from the compiler's own trees, so string lengths and the
# inline/pooled split in the tag pool match what nimsem actually interns.

type
  Vocab = object
    stmts, prc, params, param, pragmas, inline, varDecl, asgn, call, add, sub,
      cmp, ifS, elifU, elseU, whileS, ret, brk, i, u, f, ptrT, obj, fld, dot,
      at, tupleT, typeDecl, aconstr, oconstr, kv, nullT, trueT,
      falseT: TagId

proc initVocab(tp: TagPool): Vocab =
  result = Vocab(
    stmts: tp.registerTag("stmts"), prc: tp.registerTag("proc"),
    params: tp.registerTag("params"), param: tp.registerTag("param"),
    pragmas: tp.registerTag("pragmas"), inline: tp.registerTag("inline"),
    varDecl: tp.registerTag("var"), asgn: tp.registerTag("asgn"),
    call: tp.registerTag("call"), add: tp.registerTag("add"),
    sub: tp.registerTag("sub"), cmp: tp.registerTag("lt"),
    ifS: tp.registerTag("if"), elifU: tp.registerTag("elif"),
    elseU: tp.registerTag("else"), whileS: tp.registerTag("while"),
    ret: tp.registerTag("ret"), brk: tp.registerTag("break"),
    i: tp.registerTag("i"), u: tp.registerTag("u"), f: tp.registerTag("f"),
    ptrT: tp.registerTag("ptr"), obj: tp.registerTag("object"),
    fld: tp.registerTag("fld"), dot: tp.registerTag("dot"),
    at: tp.registerTag("at"), tupleT: tp.registerTag("tuple"),
    typeDecl: tp.registerTag("type"),
    # the JSON mapping (see lib/std/json.nim) shares this pool
    aconstr: tp.registerTag("aconstr"), oconstr: tp.registerTag("oconstr"),
    kv: tp.registerTag("kv"), nullT: tp.registerTag("null"),
    trueT: tp.registerTag("true"), falseT: tp.registerTag("false"))

# ── workload generation ─────────────────────────────────────────────────────
# Shaped like hexer's output: a `(stmts)` of type and proc declarations, each
# proc a params list, a return type, a pragma list and a body of nested
# control flow. What matters for the benchmark is the SHAPE — the branching
# factor, the nesting depth, and the ratio of symbols to literals to tags —
# not that the tree would survive a type checker.

proc genType(b: var TokenBuf; v: Vocab; g: var Gen) =
  case g.below(4)
  of 0:
    b.buildTree v.i:
      b.addIntLit(64)
  of 1:
    b.buildTree v.u:
      b.addIntLit(8)
  of 2:
    b.buildTree v.f:
      b.addIntLit(64)
  else:
    b.buildTree v.ptrT:
      b.buildTree v.i:
        b.addIntLit(32)

proc genExpr(b: var TokenBuf; v: Vocab; g: var Gen; depth: int) =
  if depth <= 0:
    case g.below(4)
    of 0: b.addIntLit(int64(g.below(4096)))
    of 1, 2: b.addSymUse("x" & $g.below(12) & ".0.bench")
    else: b.addStrLit("s" & $g.below(200))
  else:
    case g.below(5)
    of 0:
      b.buildTree v.call:
        b.addSymUse("f" & $g.below(64) & ".0.bench")
        for i in 0 ..< 1 + g.below(3): genExpr(b, v, g, depth - 1)
    of 1:
      b.buildTree v.add:
        genType(b, v, g)
        genExpr(b, v, g, depth - 1)
        genExpr(b, v, g, depth - 1)
    of 2:
      b.buildTree v.sub:
        genType(b, v, g)
        genExpr(b, v, g, depth - 1)
        genExpr(b, v, g, depth - 1)
    of 3:
      b.buildTree v.dot:
        b.addSymUse("o" & $g.below(16) & ".0.bench")
        b.addSymUse("fld" & $g.below(24) & ".0.bench")
        b.addIntLit(0)
    else:
      b.buildTree v.at:
        b.addSymUse("a" & $g.below(16) & ".0.bench")
        genExpr(b, v, g, depth - 1)

proc genStmts(b: var TokenBuf; v: Vocab; g: var Gen; depth: int) =
  b.buildTree v.stmts:
    for i in 0 ..< 2 + g.below(4):
      case (if depth <= 0: g.below(2) else: g.below(5))
      of 0:
        b.buildTree v.asgn:
          b.addSymUse("x" & $g.below(12) & ".0.bench")
          genExpr(b, v, g, 2)
      of 1:
        b.buildTree v.varDecl:
          b.addSymDef("t" & $g.below(40) & ".0.bench")
          b.addDotToken()
          genType(b, v, g)
          genExpr(b, v, g, 1)
      of 2:
        b.buildTree v.ifS:
          b.buildTree v.elifU:
            b.buildTree v.cmp:
              genExpr(b, v, g, 1)
              genExpr(b, v, g, 1)
            genStmts(b, v, g, depth - 1)
          if g.below(2) == 0:
            b.buildTree v.elseU:
              genStmts(b, v, g, depth - 1)
      of 3:
        b.buildTree v.whileS:
          b.buildTree v.cmp:
            genExpr(b, v, g, 1)
            genExpr(b, v, g, 1)
          genStmts(b, v, g, depth - 1)
      else:
        b.buildTree v.ret:
          genExpr(b, v, g, 2)

proc genProc(b: var TokenBuf; v: Vocab; g: var Gen; id: int) =
  b.buildTree v.prc:
    b.addSymDef("proc" & $id & ".0.bench")
    b.buildTree v.params:
      for p in 0 ..< g.below(5):
        b.buildTree v.param:
          b.addSymDef("x" & $p & ".0.bench")
          b.addDotToken()
          genType(b, v, g)
    genType(b, v, g)
    b.buildTree v.pragmas:
      if g.below(3) == 0:
        b.buildTree v.inline:
          discard
    genStmts(b, v, g, 3)

proc genObject(b: var TokenBuf; v: Vocab; g: var Gen; id: int) =
  b.buildTree v.typeDecl:
    b.addSymDef("T" & $id & ".0.bench")
    b.buildTree v.obj:
      b.addDotToken()
      for f in 0 ..< 2 + g.below(6):
        b.buildTree v.fld:
          b.addSymDef("fld" & $f & ".0.bench")
          b.addDotToken()
          genType(b, v, g)

proc genModule(v: Vocab; tp: TagPool; pool: Pool; decls: int): TokenBuf =
  var g = Gen(s: 0x9E3779B97F4A7C15'u64)
  result = createTokenBuf(1 shl 18, pool, tp)
  result.buildTree v.stmts:
    for i in 0 ..< decls:
      if i mod 7 == 0: genObject(result, v, g, i)
      else: genProc(result, v, g, i)

# ── traversal ───────────────────────────────────────────────────────────────

type Stats = object
  tags, syms, ints, strs: int
  acc: uint64
    ## Deliberately UNSIGNED: this is a checksum, and a real module carries
    ## int literals near `high(int64)` (hashes, sentinels), so a signed
    ## accumulator overflows on the first one. Unsigned wraps by definition in
    ## both compilers, which is also what makes the value comparable across
    ## backends — every build must print the same number or one of them is
    ## miscompiling.

proc walk(c: var Cursor; st: var Stats)

proc walkOne(c: var Cursor; st: var Stats) =
  ## Consume EXACTLY one value. Split out from `walk` because `skim` needs to
  ## descend into a single sibling without running off the end of the scope —
  ## a bare `while c.hasMore` loop there would swallow every remaining
  ## declaration and quietly turn `skim` back into `walk`.
  case c.kind
  of TagLit:
    inc st.tags
    st.acc = st.acc + uint64(uint32(c.cursorTagId))
    c.into:
      walk(c, st)
  of Symbol, SymbolDef:
    inc st.syms
    st.acc = st.acc + uint64(uint32(c.symId))
    c.skip()
  of IntLit:
    inc st.ints
    st.acc = st.acc + cast[uint64](c.intVal)
    c.skip()
  of StrLit:
    inc st.strs
    st.acc = st.acc + uint64(c.strVal.len)
    c.skip()
  else:
    c.skip()

proc walk(c: var Cursor; st: var Stats) =
  ## The shape every analysis pass has: dispatch on kind, descend into trees,
  ## touch the payload of each atom. `acc` exists so nothing here is dead code
  ## — without a consumed result an optimizing backend is free to delete the
  ## whole walk, and one of them would.
  while c.hasMore:
    walkOne(c, st)

proc skim(c: var Cursor; st: var Stats; every: int) =
  ## The *other* traversal shape, and the more common one: walk the toplevel
  ## declarations, `skip` past almost all of them, descend into the few that
  ## matter. This is what an index-driven read does, and it exercises the jump
  ## field rather than the token-by-token path — a backend that is fast at
  ## `walk` can still be slow here.
  var n = 0
  c.into:
    while c.hasMore:
      if n mod every == 0:
        walkOne(c, st)
      else:
        inc st.tags
        c.skip()
      inc n

proc collectSyms(c: var Cursor; dest: var Table[SymId, int]) =
  var n = 0
  while c.hasMore:
    case c.kind
    of TagLit:
      c.into:
        collectSyms(c, dest)
    of Symbol, SymbolDef:
      dest[c.symId] = n
      inc n
      c.skip()
    else:
      c.skip()

# ── harness ─────────────────────────────────────────────────────────────────

type Result = object
  name, unit: string
  ns: int64
  units: int

var
  results: seq[Result] = @[]
  only = ""
  reps = 5
  sink: uint64 = 0
    ## Every benchmark folds something into this and `main` prints it. A
    ## benchmark whose result is never observed is a benchmark the optimizer
    ## may delete outright.

proc wanted(name: string): bool =
  # `only in name` would be the idiomatic spelling, but Nimony has no
  # substring `contains` for strings and resolves `in` to the openArray
  # overloads instead. `find` is unambiguous in both compilers.
  only.len == 0 or find(name, only) >= 0

proc report(name, unit: string; ns: int64; units: int) =
  results.add Result(name: name, unit: unit, ns: ns, units: units)

template measure(name, unit: string; units: int; body: untyped) =
  ## Best-of-`reps`. Best rather than mean: the distribution is one-sided —
  ## scheduler preemption and page faults only ever make a run slower — so the
  ## minimum is the cleanest estimate of the code's own cost.
  if wanted(name):
    var best = high(int64)
    for r in 0 ..< reps:
      let t0 = getMonoTime().ticks
      body
      let dt = getMonoTime().ticks - t0
      if dt < best: best = dt
    report(name, unit, best, units)

# ── the JSON mapping ────────────────────────────────────────────────────────
# `lib/std/json.nim` encodes a document as a nifcore tree: `(aconstr …)` for
# arrays, `(oconstr (kv "key" value) …)` for objects, `(null)`/`(true)`/
# `(false)` for the singletons, and bare atoms for numbers and strings. The
# scanner below builds exactly that. It is here rather than imported because
# `std/json` pulls in the *nimony* stdlib's `streams`/`parsejson`, which host
# Nim cannot resolve — and nifbench has to build with both compilers. What is
# being measured is the NIF side anyway: interning keys, opening and sealing
# trees, growing the token array.

proc genJson(g: var Gen; depth: int; dest: var string) =
  # Three of the five branches are containers. With an average of 3.5 children
  # that is an expansion factor of ~2.1 per level, so the document actually
  # grows with `depth` instead of collapsing into a list of scalars.
  # Cases 0..2 are containers and 3..5 scalars, so the depth-0 pick
  # (`3 + below(3)`) can never re-enter a container and recurse forever.
  case (if depth <= 0: 3 + g.below(3) else: g.below(6))
  of 0, 2:
    dest.add '['
    for i in 0 ..< 1 + g.below(6):
      if i > 0: dest.add ','
      genJson(g, depth - 1, dest)
    dest.add ']'
  of 1:
    dest.add '{'
    for i in 0 ..< 1 + g.below(6):
      if i > 0: dest.add ','
      dest.add '"'
      dest.add "key" & $g.below(80)
      dest.add "\":"
      genJson(g, depth - 1, dest)
    dest.add '}'
  of 3: dest.add $g.below(100000)
  of 4:
    dest.add '"'
    dest.add "value" & $g.below(500)
    dest.add '"'
  else:
    dest.add (case g.below(3)
              of 0: "null"
              of 1: "true"
              else: "false")

proc skipWs(s: string; i: var int) =
  while i < s.len and (s[i] == ' ' or s[i] == '\n' or s[i] == '\t' or
                       s[i] == '\r'): inc i

proc scanJson(s: string; i: var int; b: var TokenBuf; v: Vocab) =
  skipWs(s, i)
  if i >= s.len: return
  case s[i]
  of '[':
    inc i
    b.openTag v.aconstr
    skipWs(s, i)
    while i < s.len and s[i] != ']':
      scanJson(s, i, b, v)
      skipWs(s, i)
      if i < s.len and s[i] == ',': inc i
      skipWs(s, i)
    inc i
    b.closeTag()
  of '{':
    inc i
    b.openTag v.oconstr
    skipWs(s, i)
    while i < s.len and s[i] != '}':
      let start = i + 1
      inc i                                   # the opening quote
      while i < s.len and s[i] != '"': inc i
      let key = s.substr(start, i - 1)
      inc i                                   # the closing quote
      skipWs(s, i)
      inc i                                   # the colon
      b.openTag v.kv
      b.addStrLit(key)
      scanJson(s, i, b, v)
      b.closeTag()
      skipWs(s, i)
      if i < s.len and s[i] == ',': inc i
      skipWs(s, i)
    inc i
    b.closeTag()
  of '"':
    let start = i + 1
    inc i
    while i < s.len and s[i] != '"': inc i
    b.addStrLit(s.substr(start, i - 1))
    inc i
  of 'n':
    inc i, 4
    b.buildTree v.nullT:
      discard
  of 't':
    inc i, 4
    b.buildTree v.trueT:
      discard
  of 'f':
    inc i, 5
    b.buildTree v.falseT:
      discard
  else:
    var n: int64 = 0
    while i < s.len and s[i] >= '0' and s[i] <= '9':
      n = n * 10 + int64(ord(s[i]) - ord('0'))
      inc i
    b.addIntLit(n)

# ── output ──────────────────────────────────────────────────────────────────

proc hundredths(v: int64): string =
  ## `v` is a count of hundredths; render it as `d.dd`. Integer-only, because
  ## `std/formatfloat` bottoms out in `snprintf` and a libc-free `nimony n`
  ## image has no libc to link it from — and because a benchmark whose numbers
  ## are rounded by three different runtimes is a benchmark you cannot diff.
  let whole = v div 100
  let frac = v mod 100
  result = $whole & "." & (if frac < 10: "0" else: "") & $frac

proc fmtRate(units: int; ns: int64; unit: string): string =
  if ns <= 0 or units <= 0: return ""
  let u = int64(units)
  if unit == "bytes":
    # MB/s = units * 1e9 / ns / 2^20, and 1e9/2^20 = 953.674…, so two decimals
    # is `units * 95367 div ns` — 0.0005 % low, and it cannot overflow: a 4 MB
    # corpus gives 4e11, eight orders below int64's range.
    result = hundredths((u * 95367) div ns) & " MB/s"
  else:
    # M<unit>/s = units / ns * 1000, two decimals.
    result = hundredths((u * 100_000) div ns) & " M" & unit & "/s"

proc printTable(csv: bool) =
  if csv:
    echo "name,ns,units,unit"
    for r in results:
      echo r.name, ",", r.ns, ",", r.units, ",", r.unit
  else:
    echo "benchmark            time        throughput"
    echo "-------------------- ----------- --------------"
    for r in results:
      let ms = hundredths(r.ns div 10_000) & " ms"
      echo r.name & repeat(' ', max(1, 21 - r.name.len)) &
           repeat(' ', max(0, 11 - ms.len)) & ms & "  " &
           fmtRate(r.units, r.ns, r.unit)

# ── main ────────────────────────────────────────────────────────────────────

const BenchNames = ["build", "emit", "parse", "walk", "skim", "clone",
                    "reintern", "symtab", "bif", "json"]

proc main =
  var scale = 1
  var csv = false
  var inputFile = ""
  for kind, key, val in getopt():
    case kind
    of cmdArgument: inputFile = key
    of cmdLongOption, cmdShortOption:
      case key
      of "only": only = val
      of "reps": reps = max(1, toInt(val, 5))
      of "scale": scale = max(1, toInt(val, 1))
      of "csv": csv = true
      of "list":
        for n in BenchNames: echo n
        return
      of "version", "v":
        echo "nifbench ", Version
        return
      of "help", "h":
        echo "nifbench [--only:S] [--reps:N] [--scale:N] [--csv] [--list] [file.nif]"
        return
      else: quit "unknown option: " & key
    of cmdEnd: discard

  let tp = newTagPool()
  let pool = newPool()
  let v = initVocab(tp)

  # The corpus: either a real file or the generated module. Both end up as a
  # TokenBuf plus its canonical text, which is what every bench below consumes.
  var buf = default(TokenBuf)
  var text = ""
  if inputFile.len > 0:
    buf = parseFromFile(inputFile, sizeHint = 1 shl 18,
                        sharedPool = pool, sharedTags = tp)
    text = buf.toString()
  else:
    buf = genModule(v, tp, pool, BaseProcs * scale)
    text = buf.toString()

  let tokens = buf.len
  var corpusStats = default(Stats)
  block:
    var c = buf.beginRead()
    walk(c, corpusStats)
  let symCount = corpusStats.syms
  echo "corpus: ", (if inputFile.len > 0: inputFile else: "generated"),
       "  ", tokens, " tokens (", corpusStats.tags, " trees, ", symCount,
       " syms, ", corpusStats.ints, " ints, ", corpusStats.strs,
       " strs), ", text.len, " bytes of NIF text, reps=", reps

  if inputFile.len == 0:
    # `build` measures the GENERATOR, so it only has meaning when the corpus is
    # the generated one. With a real file as input the two are unrelated and
    # its throughput would be reported against the wrong token count.
    measure "build", "tokens", tokens:
      var b = genModule(v, tp, pool, BaseProcs * scale)
      sink = sink + uint64(b.len)

  measure "emit", "bytes", text.len:
    let s = buf.toString()
    sink = sink + uint64(s.len)

  measure "parse", "bytes", text.len:
    var b = parseFromBuffer(text, "bench", sizeHint = 1 shl 18)
    sink = sink + uint64(b.len)

  measure "walk", "tokens", tokens:
    var st = default(Stats)
    var c = buf.beginRead()
    walk(c, st)
    sink = sink + st.acc + uint64(st.tags + st.syms + st.ints + st.strs)

  measure "skim", "tokens", tokens:
    var st = default(Stats)
    var c = buf.beginRead()
    skim(c, st, 8)
    sink = sink + st.acc + uint64(st.tags)

  measure "clone", "tokens", tokens:
    # Same pool, one `addSubtree` per DECLARATION rather than one for the whole
    # module: that is what hexer does, and it keeps the bench from degenerating
    # into a single 1.4 MB memcpy that measures nothing but `rep movsq`.
    var dest = createTokenBuf(1 shl 18, pool, tp)
    var c = buf.beginRead()
    c.into:
      while c.hasMore:
        dest.addSubtree(c)
        c.skip()
    sink = sink + uint64(dest.len)

  measure "reintern", "tokens", tokens:
    # Fresh pool: every string, symbol and tag has to be looked up and
    # re-interned on the way across. This is the cost of pulling a
    # declaration out of a dependency module.
    var dest = createTokenBuf(1 shl 18, newPool(), newTagPool())
    var c = buf.beginRead()
    c.into:
      while c.hasMore:
        dest.addSubtree(c)
        c.skip()
    sink = sink + uint64(dest.len)

  measure "symtab", "syms", symCount:
    var t = initTable[SymId, int]()
    var c = buf.beginRead()
    collectSyms(c, t)
    sink = sink + uint64(t.len)

  if wanted("bif"):
    # The binary cache round trip. `store` and `load` are file-based by
    # design — that IS the compiler's cache path — so this one benchmark
    # touches the filesystem. The file is written once outside the timed
    # region for the load half, and the page cache makes the read a memcpy.
    measure "bif", "tokens", tokens:
      let s = buf.storeToString()
      sink = sink + uint64(s.len)
    buf.store(BifScratch)
    measure "bif-load", "tokens", tokens:
      var m = bif.load(BifScratch)
      sink = sink + uint64(m.buf.len)
    deleteFile(BifScratch)

  if wanted("json"):
    var g = Gen(s: 0x243F6A8885A308D3'u64)
    var doc = ""
    for i in 0 ..< 300 * scale:
      if i > 0: doc.add ','
      genJson(g, 5, doc)
    doc = "[" & doc & "]"
    measure "json", "bytes", doc.len:
      var b = createTokenBuf(1 shl 16, newPool(), tp)
      var i = 0
      scanJson(doc, i, b, v)
      var st = default(Stats)
      var c = b.beginRead()
      walk(c, st)
      sink = sink + st.acc + uint64(b.len)

  printTable(csv)
  echo "checksum: ", sink

main()

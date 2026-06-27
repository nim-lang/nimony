## For-loop plugin backing `std/parfor`'s `||` iterator.
##
## Rewrites `for i in a || b: BODY` into a worker-pool fan-out plus a structured
## join. The emitted shape (nifler "nim-parsed" dialect, re-sem'd in place):
##
##   (stmts
##     (call ensureParPool)
##     (var pforA … <a>) (var pforB … <b>)
##     (var pforStep … <step>) (var pforChunkSize … <chunkSize>) (var pforWorkload … <workload>)
##     (var pforJ … (call default ParJoin))
##     (var pforIters … (call parIterCount pforA pforB pforStep))
##     (var pforGrain … (call parGrain pforIters pforChunkSize))
##     (var pforTotal … (call parChunkCount pforIters pforGrain))
##     (call parBegin pforJ pforTotal)
##     (proc pforChunk … {.passive, closure.} (params pforLo pforHi)
##       (stmts
##         (var pforIt … pforLo)
##         (while (infix < pforIt pforHi)
##           (stmts
##             (let pforI … (infix + pforA (infix * pforIt pforStep)))
##             <BODY[i→pforI]> (asgn pforIt (infix + pforIt 1))))
##         (call parChunkDone (call addr pforJ))))
##     (var pforK … 0)
##     (while (infix < pforK pforTotal)
##       (stmts
##         (let pforLo2 … (call parChunkLo pforGrain pforK))
##         (let pforHi2 … (call parChunkHi pforIters pforGrain pforK))
##         (call parSubmit (call delay (call pforChunk pforLo2 pforHi2)) pforK)
##         (asgn pforK (infix + pforK 1))))
##     (call parWait pforJ pforWorkload))
##
## `pforLo`/`pforHi` chunk the *iteration index* space `[0, pforIters)`; the
## runner maps each index back to `pforA + j*pforStep`. The body is spliced
## typed; the loop variable (a resolved `Symbol`) is substituted by the fresh
## ident `pforI`. Outer reads (`input`) and the disjoint output (`x[i]`) are
## captured by the closure.

import plugins

# ── input navigation ───────────────────────────────────────────────────────

proc loopVarSym(n: NifCursor): SymId =
  ## The resolved loop-variable symbol, from `(loopvars (unpackflat (let (symdef i) …)))`.
  var vars = forLoopVars(n)
  if vars.kind == ParLe and vars.otherKind == LoopvarsU:
    vars = firstChild(vars)          # (unpackflat …) or (unpacktup …)
  if vars.kind == ParLe and vars.tagText == "unpackflat":
    vars = firstChild(vars)          # (let …)
    if vars.kind == ParLe:
      vars = firstChild(vars)        # (symdef i)
    if vars.kind == SymbolDef:
      return vars.symId
  result = default(SymId)

proc collectArgs(n: NifCursor): seq[NifCursor] =
  ## The `||` call arguments (a, b, step, chunkSize, workload) as cursors.
  var c = pluginCallArgs(n)
  result = @[]
  while c.kind != ParRi:
    result.add c
    skip c

# ── static race-rule checker ─────────────────────────────────────────────────
# Runs over the *typed* body before lowering and rejects anything not provably
# race-free under the chunked execution model. The safety argument: chunk A
# writes a disjoint index range from chunk B iff every output write targets
# `output[i ± const]` with one fixed offset *and* a single outer iteration `i`
# touches exactly one index (so adjacent chunks never overlap). That second
# condition fails the moment the write sits inside a nested loop — then one `i`
# fans out across a whole inner range and neighbouring chunks collide.

const
  IndexKinds = {ArratX, AtX, PatX, CurlyatX, TupatX}
  PeelKinds  = {ArratX, AtX, PatX, CurlyatX, TupatX,
                DotX, DdotX, DerefX, HderefX, AddrX, HaddrX}

type
  Ctx = object
    loopSym: SymId
    locals: seq[SymId]          ## symbols declared inside the body
    outBases: seq[SymId]        ## discovered output collections
    outOffs: seq[int]           ## the offset each output is written at
    err: string                 ## first error ("" = still ok)

proc has(s: seq[SymId]; x: SymId): bool =
  for e in s:
    if e == x: return true
  result = false

proc displayName(s: SymId): string =
  ## The readable prefix of a mangled symbol (`x.0.mymod` → `x`).
  result = ""
  for ch in symText(s):
    if ch == '.': break
    result.add ch

proc child(n: NifCursor; idx: int): NifCursor =
  result = firstChild(n)
  var k = idx
  while true:
    if k <= 0: break
    skip result
    dec k

proc lvalueBase(n: NifCursor): SymId =
  ## Innermost base symbol of an lvalue, peeling index/field/deref layers.
  ## Recursive (descent terminates at a leaf) rather than a `while` with a
  ## `firstChild` reassignment, which the termination checker doesn't recognise
  ## as progress.
  if n.kind == ParLe and n.exprKind in PeelKinds:
    result = lvalueBase(firstChild(n))
  elif n.kind == Symbol:
    result = n.symId
  else:
    result = default(SymId)

proc analyzeIndex(idx: NifCursor; loopSym: SymId): tuple[ok: bool, off: int] =
  ## Accepts `i`, `i + c`, `c + i`, `i - c` (with `c` an integer literal).
  if idx.kind == Symbol and idx.symId == loopSym:
    return (true, 0)
  if idx.kind == ParLe and (idx.exprKind == AddX or idx.exprKind == SubX):
    var op1 = firstChild(idx)   # type child
    skip op1                    # → first operand
    var op2 = op1
    skip op2                    # → second operand
    let isAdd = idx.exprKind == AddX
    if op1.kind == Symbol and op1.symId == loopSym and op2.kind == IntLit:
      return (true, if isAdd: int(op2.intValue) else: -int(op2.intValue))
    if isAdd and op2.kind == Symbol and op2.symId == loopSym and op1.kind == IntLit:
      return (true, int(op1.intValue))
  result = (false, 0)

proc recordOutput(c: var Ctx; base: SymId; off: int) =
  var i = 0
  while true:
    if i >= c.outBases.len: break
    if c.outBases[i] == base:
      if c.outOffs[i] != off:
        c.err = "`||` writes its output `" & displayName(base) &
          "` at two different offsets; every write must share one `i ± const` form (else adjacent chunks overlap)"
      return
    inc i
  c.outBases.add base
  c.outOffs.add off

proc collectLocals(c: var Ctx; n: var NifCursor) =
  if n.kind == SymbolDef:
    c.locals.add n.symId
    skip n
  elif n.kind == ParLe:
    n.into:
      while n.kind != ParRi:
        collectLocals(c, n)
  else:
    skip n

proc handleAsgn(c: var Ctx; lhs: NifCursor; depth: int) =
  let base = lvalueBase(lhs)
  if lhs.kind == ParLe and lhs.exprKind in IndexKinds:
    if base != default(SymId) and has(c.locals, base):
      discard "private per-iteration buffer — safe"
    elif base == c.loopSym:
      c.err = "`||` body assigns through the loop variable"
    elif base == default(SymId):
      c.err = "`||` body: unsupported assignment target"
    else:
      let a = analyzeIndex(child(lhs, 1), c.loopSym)
      if not a.ok:
        c.err = "`||` output `" & displayName(base) &
          "` must be indexed by `i` or `i ± const`; a different index means chunks overlap and race"
      elif depth > 0:
        c.err = "`||` writes `" & displayName(base) &
          "[i]` inside a nested loop — a single `i` then spans a range of indices and adjacent chunks race; hoist the write out of the inner loop"
      else:
        recordOutput(c, base, a.off)
  elif lhs.kind == Symbol:
    if has(c.locals, lhs.symId):
      discard "local mutation — safe"
    elif lhs.symId == c.loopSym:
      c.err = "`||` body assigns to the loop variable `i`"
    else:
      c.err = "`||` body assigns to the outer variable `" & displayName(lhs.symId) &
        "`; only `output[i]` and locals may be written (shared mutation races)"
  else:
    if base != default(SymId) and has(c.locals, base):
      discard "field/deref of a local — safe"
    else:
      c.err = "`||` body: assignment target must be `output[i]` or a local"

proc checkWrites(c: var Ctx; n: var NifCursor; depth: int) =
  if c.err.len > 0:
    skip n; return
  if n.kind != ParLe:
    skip n; return
  let sk = n.stmtKind
  if sk == AsgnS:
    handleAsgn(c, child(n, 0), depth)
    n.into:
      while n.kind != ParRi:
        checkWrites(c, n, depth)
  elif sk == ForS or sk == WhileS:
    n.into:
      while n.kind != ParRi:
        checkWrites(c, n, depth + 1)
  else:
    n.into:
      while n.kind != ParRi:
        checkWrites(c, n, depth)

proc readErr(s: SymId): string =
  "`||` reads its output `" & displayName(s) &
    "` inside the body; another chunk may be writing it concurrently " &
    "(reductions like `x[i] += …` aren't supported here)"

proc checkReads(c: var Ctx; n: var NifCursor)

proc checkWriteLhs(c: var Ctx; n: var NifCursor) =
  ## Walk an asgn LHS: the output write-base symbol is allowed, but every index
  ## / bound / field sub-expression is a read.
  if n.kind == ParLe and n.exprKind in IndexKinds:
    n.into:
      var first = true
      while n.kind != ParRi:
        if first:
          if n.kind == Symbol and has(c.outBases, n.symId):
            skip n
          else:
            checkWriteLhs(c, n)
          first = false
        else:
          checkReads(c, n)
  else:
    checkReads(c, n)

proc checkReads(c: var Ctx; n: var NifCursor) =
  if c.err.len > 0:
    skip n; return
  case n.kind
  of Symbol:
    if has(c.outBases, n.symId):
      c.err = readErr(n.symId)
    skip n
  of SymbolDef:
    skip n
  of ParLe:
    if n.stmtKind == AsgnS:
      n.into:
        var first = true
        while n.kind != ParRi:
          if first:
            checkWriteLhs(c, n)
            first = false
          else:
            checkReads(c, n)
    else:
      n.into:
        while n.kind != ParRi:
          checkReads(c, n)
  else:
    skip n

proc raceCheck(loopSym: SymId; body: NifCursor): string =
  ## Returns "" when the body is provably data-race free, else the error.
  var c = Ctx(loopSym: loopSym, err: "")
  var b1 = body
  collectLocals(c, b1)
  var b2 = body
  checkWrites(c, b2, 0)
  if c.err.len == 0:
    var b3 = body
    checkReads(c, b3)
  result = c.err

# ── body rewriting ──────────────────────────────────────────────────────────

proc emitBody(o: var NifBuilder; n: var NifCursor; loopSym: SymId) =
  ## Copy the typed body, replacing each use of `loopSym` with `(ident pforI)`.
  if n.kind == ParLe:
    o.copyInto(n):
      while n.kind != ParRi:
        emitBody(o, n, loopSym)
  elif n.kind == Symbol and n.symId == loopSym:
    o.addIdent("pforI")
    skip n
  else:
    o.addSubtree(n)
    skip n

# ── builder helpers (nifler "nim-parsed" dialect) ────────────────────────────

proc beginCall(o: var NifBuilder; fn: string) =
  ## Opens `(call <fn>` — caller emits the args and a matching `addParRi`.
  o.addParLe("call")
  o.addIdent(fn)

proc beginVar(o: var NifBuilder; tag, name: string) =
  ## Opens `(<tag> <name> . . .` (a `var`/`let` decl) — caller emits the
  ## initializer expression and a matching `addParRi`.
  o.addParLe(tag)
  o.addIdent(name)
  o.addEmptyNode3()   # exported, pragmas, type

proc emitInfix(o: var NifBuilder; op, lhs, rhs: string) =
  o.addParLe("infix")
  o.addIdent(op)
  o.addIdent(lhs)
  o.addIdent(rhs)
  o.addParRi()

proc emitIncr(o: var NifBuilder; name: string) =
  ## `(asgn name (infix + name 1))`
  o.addParLe("asgn")
  o.addIdent(name)
  o.addParLe("infix")
  o.addIdent("+")
  o.addIdent(name)
  o.addIntLit(1)
  o.addParRi()
  o.addParRi()

# ── transform ────────────────────────────────────────────────────────────────

proc transform(n: NifCursor): NifBuilder =
  let info = n.info
  result = createTree()

  let loopSym = loopVarSym(n)
  if loopSym == default(SymId):
    return errorTree("`||` parallel-for: could not resolve the loop variable", info)

  # Overload resolution fills the `step`/`chunkSize`/`workload` defaults and
  # normalises any named args into declaration order, so a resolved `||` always
  # arrives with all five positional args (a, b, step, chunkSize, workload) —
  # the plugin needs no default-handling logic of its own.
  let args = collectArgs(n)
  if args.len < 5:
    return errorTree("`||` parallel-for expects `a || b` (optionally with step / chunkSize / workload)", info)

  # Static race-rule check: reject any body that isn't provably data-race free.
  let raceErr = raceCheck(loopSym, forLoopBody(n))
  if raceErr.len > 0:
    return errorTree(raceErr, info)

  # Wrap in a `block` so the helper locals (pforA, pforJ, pforChunk, …) get a
  # fresh scope -- several `||` loops can then coexist in one routine.
  result.addParLe("block")
  result.addDotToken()
  result.withTree StmtsS, info:
    # ensureParPool()
    result.beginCall("ensureParPool"); result.addParRi()

    # var pforA = <a>; pforB = <b>; pforStep = <step>; pforChunkSize = <chunkSize>; pforWorkload = <workload>
    result.beginVar("var", "pforA"); result.addSubtree(args[0]); result.addParRi()
    result.beginVar("var", "pforB"); result.addSubtree(args[1]); result.addParRi()
    result.beginVar("var", "pforStep"); result.addSubtree(args[2]); result.addParRi()
    result.beginVar("var", "pforChunkSize"); result.addSubtree(args[3]); result.addParRi()
    result.beginVar("var", "pforWorkload"); result.addSubtree(args[4]); result.addParRi()

    # var pforJ = default(ParJoin)
    result.beginVar("var", "pforJ")
    result.beginCall("default"); result.addIdent("ParJoin"); result.addParRi()
    result.addParRi()

    # var pforIters = parIterCount(pforA, pforB, pforStep)
    result.beginVar("var", "pforIters")
    result.beginCall("parIterCount")
    result.addIdent("pforA"); result.addIdent("pforB"); result.addIdent("pforStep")
    result.addParRi()
    result.addParRi()

    # var pforGrain = parGrain(pforIters, pforChunkSize)
    result.beginVar("var", "pforGrain")
    result.beginCall("parGrain")
    result.addIdent("pforIters"); result.addIdent("pforChunkSize")
    result.addParRi()
    result.addParRi()

    # var pforTotal = parChunkCount(pforIters, pforGrain)
    result.beginVar("var", "pforTotal")
    result.beginCall("parChunkCount")
    result.addIdent("pforIters"); result.addIdent("pforGrain")
    result.addParRi()
    result.addParRi()

    # parBegin(pforJ, pforTotal)
    result.beginCall("parBegin")
    result.addIdent("pforJ"); result.addIdent("pforTotal")
    result.addParRi()

    # proc pforChunk(pforLo, pforHi: int) {.passive, closure.} = ...
    result.addParLe("proc")
    result.addIdent("pforChunk")
    result.addEmptyNode3()              # exported, pattern, generics
    result.addParLe("params")
    block:
      result.addParLe("param"); result.addIdent("pforLo")
      result.addEmptyNode2(); result.addIdent("int"); result.addDotToken()
      result.addParRi()
      result.addParLe("param"); result.addIdent("pforHi")
      result.addEmptyNode2(); result.addIdent("int"); result.addDotToken()
      result.addParRi()
    result.addParRi()                   # /params
    result.addDotToken()                # return type (void)
    result.addParLe("pragmas")
    result.addIdent("passive"); result.addIdent("closure")
    result.addParRi()
    result.addDotToken()                # effects
    result.withTree StmtsS, info:
      # pforLo/pforHi are iteration indices; map each back to `pforA + j*pforStep`.
      # var pforIt = pforLo
      result.beginVar("var", "pforIt"); result.addIdent("pforLo"); result.addParRi()
      # while pforIt < pforHi:
      #   let pforI = pforA + pforIt * pforStep; <body>; pforIt = pforIt + 1
      result.addParLe("while")
      result.emitInfix("<", "pforIt", "pforHi")
      result.withTree StmtsS, info:
        # let pforI = pforA + pforIt * pforStep
        result.beginVar("let", "pforI")
        result.addParLe("infix"); result.addIdent("+"); result.addIdent("pforA")
        result.addParLe("infix"); result.addIdent("*")
        result.addIdent("pforIt"); result.addIdent("pforStep")
        result.addParRi()               # /inner infix
        result.addParRi()               # /outer infix
        result.addParRi()               # /let
        var body = forLoopBody(n)
        emitBody(result, body, loopSym)
        result.emitIncr("pforIt")
      result.addParRi()                 # /while
      # parChunkDone(addr pforJ)
      result.beginCall("parChunkDone")
      result.beginCall("addr"); result.addIdent("pforJ"); result.addParRi()
      result.addParRi()
    result.addParRi()                   # /proc

    # var pforK = 0
    result.beginVar("var", "pforK"); result.addIntLit(0); result.addParRi()

    # while pforK < pforTotal:
    #   let pforLo2 = parChunkLo(pforGrain, pforK)
    #   let pforHi2 = parChunkHi(pforIters, pforGrain, pforK)
    #   parSubmit(delay(pforChunk(pforLo2, pforHi2)), pforK)   -- pforK spreads
    #     chunks across stripes; submit-all-then-`parWait` is the structured join.
    #   pforK = pforK + 1
    result.addParLe("while")
    result.emitInfix("<", "pforK", "pforTotal")
    result.withTree StmtsS, info:
      result.beginVar("let", "pforLo2")
      result.beginCall("parChunkLo")
      result.addIdent("pforGrain"); result.addIdent("pforK")
      result.addParRi(); result.addParRi()
      result.beginVar("let", "pforHi2")
      result.beginCall("parChunkHi")
      result.addIdent("pforIters"); result.addIdent("pforGrain"); result.addIdent("pforK")
      result.addParRi(); result.addParRi()
      result.beginCall("parSubmit")
      result.beginCall("delay")
      result.beginCall("pforChunk")
      result.addIdent("pforLo2"); result.addIdent("pforHi2")
      result.addParRi()                 # /pforChunk
      result.addParRi()                 # /delay
      result.addIdent("pforK")
      result.addParRi()                 # /parSubmit
      result.emitIncr("pforK")
    result.addParRi()                   # /while

    # parWait(pforJ, pforWorkload)
    result.beginCall("parWait")
    result.addIdent("pforJ"); result.addIdent("pforWorkload")
    result.addParRi()
  result.addParRi()                     # /block

let input = loadPluginInput()
saveTree transform(input)

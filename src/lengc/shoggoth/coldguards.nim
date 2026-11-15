#
#
#           Lengc Cold Guard Sinking (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Moves the failure arm of every guard to the end of the proc body:
##
## ```
## (ite C (stmts (call panic msg)) .)       (ite C (stmts (jmp `cold.1)) .)
## …                                    →   …
##                                          (ret .)            # if it fell through
##                                          (lab :`cold.1)
##                                          (scope (stmts (call panic msg)))
## ```
##
## A guard is what hexer emits for a bounds check or an undischarged `.requires`
## (`emitRequiresGuard`): an `ite` without an else-arm whose arm is one call to a
## proc that never returns. In place, the arm puts its argument setup and call in
## the middle of the hot path, and the backend has to branch around it on every
## check that passes. At the end of the body the check becomes a single
## conditional branch to the cold block, the hot path falls through, and the cold
## code sits outside every loop, which is also what a loop-depth-weighted register
## allocator wants to see. No backend needs to know about guards for this; each
## only has to lower `(ite C (jmp L) .)` as one conditional branch.
##
## Identical arms share one cold block. A bounds check's message names the
## `requires` it comes from, not the indexing site, so a body with many `s[i]`
## needs only one block per distinct conjunct.
##
## ## Legality
##
## * The arm may read only names that are in scope at the end of the body:
##   globals and literals. It must not mention a local or a parameter, and it
##   must declare nothing. `panic(<string literal>)` qualifies;
##   `raiseIndexError3(i, 0, b)` does not and stays where it is.
## * The cold blocks must not be reachable by falling off the end of the body. If
##   the body can fall through, a void proc gets `(ret .)` before the first
##   block; a proc with a result that could fall through is left alone (Leng
##   bodies that compute a value end in `ret` on every path, so this is not
##   expected to happen).
##
## ## Ordering
##
## This runs last, after the tail-call encoding. CSE and the redundant-guard
## elimination USE a guard as a fact ("everything it dominates may assume
## `not C`"), which a guard turned into a `jmp` no longer provides; and the
## tail-call pass reads the body's last statement, which this pass changes.

import std / [tables, sets, assertions]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/twoWayParts/hasArm
import ".." / ".." / "models" / tags          # tag ids for synthesis
import ".." / nifmodules                      # MainModule
import cse                                    # SummaryResolver, FunctionSummaryTable

type
  Context = object
    resolver: SummaryResolver
    locals: HashSet[SymId]         ## every name the body or the params declare
    arms: seq[Cursor]              ## the cold arms, in first-use order
    labels: seq[string]            ## their labels, parallel to `arms`
    byText: Table[string, int]     ## arm text → index into `arms`

proc collectDefs(n: Cursor; acc: var HashSet[SymId]) =
  case n.kind
  of SymbolDef: acc.incl n.symId
  of TagLit:
    var c = n
    c.into:
      while c.hasMore:
        collectDefs(c, acc)
        skip c
  else: discard

proc isMovable(c: Context; n: Cursor): bool =
  ## No name in `n` is a local or param, and `n` declares nothing.
  case n.kind
  of SymbolDef: result = false
  of Symbol: result = n.symId notin c.locals
  of TagLit:
    result = true
    var it = n
    it.into:
      while it.hasMore:
        if result and not c.isMovable(it): result = false
        skip it
  else: result = true

proc callIsNoReturn(c: var Context; call: Cursor): bool =
  if call.kind != TagLit or call.stmtKind != CallS: return false
  var callee = call
  inc callee
  if callee.kind != Symbol: return false
  var s = default(FunctionSummary)
  result = resolveCallee(c.resolver, callee.symId, s) and s.noReturn

proc soleStmt(n: Cursor): Cursor =
  ## Through `(stmts X)`/`(scope X)` wrappers with exactly one real child.
  result = n
  while result.kind == TagLit and result.stmtKind in {StmtsS, ScopeS}:
    var it = result
    var only = default(Cursor)
    var count = 0
    it.into:
      while it.hasMore:
        if it.kind != DotToken:
          only = it
          inc count
        skip it
    if count != 1: return default(Cursor)
    result = only

proc coldArm(c: var Context; n: Cursor; arm: var Cursor): bool =
  ## `n` is a guard; `arm` is set to its failure arm.
  var cond, thenArm, elseArm = default(Cursor)
  if n.kind != TagLit or n.stmtKind != IteS: return false
  if not twoWayParts(n, cond, thenArm, elseArm) or hasArm(elseArm): return false
  let call = soleStmt(thenArm)
  if cursorIsNil(call) or not c.callIsNoReturn(call): return false
  if not c.isMovable(thenArm): return false
  arm = thenArm
  result = true

proc labelFor(c: var Context; arm: Cursor): string =
  let key = toString(arm, includeLineInfo = false)
  var idx = c.byText.getOrDefault(key, -1)
  if idx < 0:
    idx = c.arms.len
    c.byText[key] = idx
    c.arms.add arm
    c.labels.add "`cold." & $(idx + 1)
  result = c.labels[idx]

proc openLike(dest: var TokenBuf; n: Cursor) =
  dest.openTag n.cursorTagId
  let li = rawLineInfo(n)
  if li.isValid: dest.appendLineInfo li

proc rewrite(c: var Context; dest: var TokenBuf; n: Cursor) =
  ## Copy `n`, replacing each guard's arm by a `jmp` to its cold block.
  var arm = default(Cursor)
  if c.coldArm(n, arm):
    let lab = c.labelFor(arm)
    dest.openLike n                             # (ite
    var it = n
    it.into:
      c.rewrite(dest, it)                       #   C
      skip it
      dest.openLike arm                         #   (stmts (jmp L))
      dest.openTag TagId(ord(JmpTagId))
      dest.addSymUse lab
      dest.closeTag()
      dest.closeTag()
      skip it
      while it.hasMore:                         #   .
        dest.addSubtree it
        skip it
    dest.closeTag()
  elif n.kind == TagLit:
    dest.openLike n
    var it = n
    it.into:
      while it.hasMore:
        c.rewrite(dest, it)
        skip it
    dest.closeTag()
  else:
    dest.addSubtree n

proc terminates(c: var Context; n: Cursor): bool =
  ## Control never continues after `n`.
  if n.kind != TagLit: return false
  case n.stmtKind
  of RetS, JmpS: result = true
  of CallS: result = c.callIsNoReturn(n)
  of StmtsS, ScopeS:
    var last = default(Cursor)
    var it = n
    it.into:
      while it.hasMore:
        if it.kind != DotToken: last = it
        skip it
    result = not cursorIsNil(last) and c.terminates(last)
  of IteS:
    var cond, thenArm, elseArm = default(Cursor)
    result = twoWayParts(n, cond, thenArm, elseArm) and hasArm(elseArm) and
             c.terminates(thenArm) and c.terminates(elseArm)
  else: result = false

proc runColdGuards*(buf: var TokenBuf; summaries: ptr FunctionSummaryTable;
                    m: ptr MainModule; params: Cursor; isVoid: bool): int =
  ## Sink the guards of the body in `buf` (a `(stmts …)`). Returns the number of
  ## cold blocks appended; 0 leaves `buf` untouched.
  result = 0
  var body = buf.beginRead()
  if body.kind != TagLit or body.stmtKind != StmtsS:
    endRead body
    return
  var c = Context(resolver: initSummaryResolver(summaries, m))
  collectDefs(body, c.locals)
  if not cursorIsNil(params) and params.kind == TagLit:
    collectDefs(params, c.locals)
  var dest = createTokenBuf(buf.len + 16, buf.pool, buf.tags)
  dest.openLike body
  var it = body
  it.into:
    while it.hasMore:
      c.rewrite(dest, it)
      skip it
  if c.arms.len > 0:
    if not c.terminates(body):
      if not isVoid:
        endRead body
        return                                  # see "Legality"
      dest.openTag TagId(ord(RetTagId))
      dest.addDotToken()
      dest.closeTag()
    for i in 0 ..< c.arms.len:
      dest.openTag TagId(ord(LabTagId))
      dest.addSymDef c.labels[i]
      dest.closeTag()
      # Each block is its own scope, so a backend can give the temporaries of
      # all of them (a `panic`'s string argument) the same stack bytes.
      dest.openTag TagId(ord(ScopeTagId))
      dest.addSubtree c.arms[i]
      dest.closeTag()
  dest.closeTag()
  endRead body
  if c.arms.len > 0:
    result = c.arms.len
    buf = ensureMove(dest)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  import std / syncio
  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  # The module and body buffers share one pool in the real pipeline, so the
  # `SymId` keys of the summary table line up; mirror that with `sharedPool`.
  var modBuf = parse("(stmts (proc :panic.0.M . . (pragmas (attr \"noreturn\")) .) " &
                     "(proc :other.0.M . . . .))")
  var summaries = collectFunctionSummaries(modBuf)

  proc run(input: string; isVoid = true; params = ""): (string, int) =
    var buf = parseFromBuffer(input, "M", 100, sharedPool = modBuf.pool,
                              sharedTags = createLengTagPool())
    var pbuf = parseFromBuffer(if params.len > 0: params else: "(params)", "M", 100,
                               sharedPool = modBuf.pool, sharedTags = createLengTagPool())
    let n = runColdGuards(buf, addr summaries, nil, pbuf.beginRead(), isVoid)
    (toString(buf), n)

  template chk(input, expected: string; isVoid = true) =
    let (got, _) = run(input, isVoid)
    let want = canon(expected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template unchanged(input: string; isVoid = true; params = "") =
    let (got, n) = run(input, isVoid, params)
    doAssert n == 0 and got == canon(input), "expected unchanged:\n  " & got

  block sinks_and_adds_ret:
    chk("(stmts (ite (not (lt i.0.M n.0.M)) (stmts (call panic.0.M m.0.M)) .) " &
        "(asgn x.0.M 1))",
        "(stmts (ite (not (lt i.0.M n.0.M)) (stmts (jmp `cold.1)) .) " &
        "(asgn x.0.M 1) (ret .) (lab :`cold.1) (scope (stmts (call panic.0.M m.0.M))))")

  block identical_arms_share_a_block:
    chk("(stmts (ite a.0.M (stmts (call panic.0.M m.0.M)) .) " &
        "(ite b.0.M (stmts (call panic.0.M m.0.M)) .) " &
        "(ite c.0.M (stmts (call panic.0.M k.0.M)) .) (ret .))",
        "(stmts (ite a.0.M (stmts (jmp `cold.1)) .) " &
        "(ite b.0.M (stmts (jmp `cold.1)) .) " &
        "(ite c.0.M (stmts (jmp `cold.2)) .) (ret .) " &
        "(lab :`cold.1) (scope (stmts (call panic.0.M m.0.M))) " &
        "(lab :`cold.2) (scope (stmts (call panic.0.M k.0.M))))")

  block nested_in_loop:
    chk("(stmts (loop (stmts (ite (not (lt i.0.M n.0.M)) (stmts (call panic.0.M m.0.M)) .) " &
        "(continue (lt i.0.M n.0.M)))) (ret x.0.M))",
        "(stmts (loop (stmts (ite (not (lt i.0.M n.0.M)) (stmts (jmp `cold.1)) .) " &
        "(continue (lt i.0.M n.0.M)))) (ret x.0.M) " &
        "(lab :`cold.1) (scope (stmts (call panic.0.M m.0.M))))", isVoid = false)

  block arm_reads_a_local:
    unchanged("(stmts (var :i.0 . (i 64) 0) " &
              "(ite (not (lt i.0 n.0.M)) (stmts (call panic.0.M i.0)) .) (ret .))")

  block arm_reads_a_param:
    unchanged("(stmts (ite (not (lt p.0 n.0.M)) (stmts (call panic.0.M p.0)) .) (ret .))",
              params = "(params (param :p.0 . (i 64)))")

  block returning_call_is_not_a_guard:
    unchanged("(stmts (ite a.0.M (stmts (call other.0.M m.0.M)) .) (ret .))")

  block else_arm_is_not_a_guard:
    unchanged("(stmts (ite a.0.M (stmts (call panic.0.M m.0.M)) (stmts (asgn x.0.M 1))) (ret .))")

  block result_proc_that_falls_through:
    unchanged("(stmts (ite a.0.M (stmts (call panic.0.M m.0.M)) .) (asgn x.0.M 1))",
              isVoid = false)

  block ends_in_total_ite:
    chk("(stmts (ite a.0.M (stmts (call panic.0.M m.0.M)) .) " &
        "(ite b.0.M (stmts (ret 1)) (stmts (ret 2))))",
        "(stmts (ite a.0.M (stmts (jmp `cold.1)) .) " &
        "(ite b.0.M (stmts (ret 1)) (stmts (ret 2))) " &
        "(lab :`cold.1) (scope (stmts (call panic.0.M m.0.M))))", isVoid = false)

  echo "coldguards: all self-tests passed"

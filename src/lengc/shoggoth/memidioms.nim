#
#
#           NIFC Memory-Idiom Recognition (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Replaces element-wise FILL loops with one `Memset` intrinsic and puts a
## `Memcpy` in front of element-wise COPY loops (nimony#2350). Both are
## `(instr …)` applications of declarations this pass adds to the module
## (`addMemIntrinsicDecl`): an intrinsic has no body, so unlike a call to
## `c_memset` there is no definition DCE (which ran before us) could have
## dropped.
##
## Generics and bounds safety make Nim produce these loops everywhere a C
## programmer would have written `memset`: `default(array[N, T])` is a
## `for i in low..high: result[i] = default(T)` loop, a `seq`'s `setLen` default-
## initializes the new tail one element at a time, and an all-zero object
## constructor stored per element is the same thing again with a bigger stride.
## The C compiler finds some of them on its own; arkham finds none, and on the
## native backend each stays a loop of one store per element.
##
## The shape matched is the one hexer's `for` lowering produces, after the rest
## of the pipeline has cleaned it up:
##
##   (while (true)
##     (if (elif (lt IV HI)               ; or `le`
##            STORE                       ; (asgn (at B IV) V) / (asgn (pat P IV) V)
##            (asgn IV (add T IV 1)))
##         (else (jmp L))))
##   (lab L)
##
## seen through the `(scope …)`/`(stmts …)` wrappers and the labels nobody jumps
## to that inlining leaves behind. It becomes
##
##   (if (elif (lt IV HI)
##     (stmts (instr memset (addr (at B IV)) BYTE (mul (i -1) COUNT (sizeof E)))
##            (asgn IV HI))))             ; the value the loop left behind
##
## so nothing after the loop can tell the difference, the induction variable
## included.
##
## **What makes it sound.** The loop evaluates `B`/`P` and `HI` once per
## iteration and `memset` evaluates them once, so the rewrite is valid only
## when no element store can change them. A value rooted at a local whose
## address is never taken cannot be reached by a store through a pointer. A
## LOAD in them (a field behind a `deref`, a global) is protected by type-based
## aliasing, the same rule the C backend's output is already compiled under
## (`gcc -O3` without `-fno-strict-aliasing`): a store of the element's scalar
## leaves cannot modify a location of a different scalar type. A one-byte store
## and an unresolved type alias everything; all pointers alias each other. An
## `le` bound must be a constant below the induction variable's maximum, or
## the original loop would never have terminated.
##
## The value stored must be the same in every byte of the element: any
## constant for a one-byte element, all-zero bits for a wider one — an all-zero
## object constructor included, which is what makes the `setLen` tails match.
##
## **Where it runs.** After CSE and before the vectorizer, which would
## otherwise turn the same loops into vector stores. The backends lower the
## rows with the inline loop or builtin they already used for a call to C's
## `memset` — except AVR, which has none, so the driver turns the pass off
## there.
##
## **Copies.** `D[IV] = S[IV]` needs the same invariance argument for both
## bases, and one more thing: `memcpy` is only a forward element loop when the
## two ranges do not overlap. Two different local arrays whose addresses are
## never taken cannot (nor can such an array and anything behind a pointer); the
## loop is then replaced like a fill. Otherwise the copy goes IN FRONT of the
## untouched loop, guarded by a runtime test that the byte ranges are disjoint:
##
##   (if (elif (and (lt IV HI) DISJOINT)
##     (stmts (instr memcpy (addr D[IV]) (addr S[IV]) N) (asgn IV HI))))
##   (while (true) …)                     ; finds IV == HI and exits at once
##
## so an overlapping copy still runs the loop it always ran, and no label in
## that loop is ever duplicated.

import std / [tables, sets, assertions]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/typeKind/substructureKind
import ".." / nifmodules                      # MainModule
import ".." / typenav                         # getType / registerLocal / scopes
import patchsets
import vmrewriter                             # subtreeEqual
import ".." / ".." / "lib" / intrinsics       # MemsetOp / MemcpyOp

type
  LeafKind = enum
    ## The scalar classes type-based aliasing distinguishes. Signedness does not
    ## matter (C lets `int` and `unsigned` alias), width does.
    lkByte, lkI16, lkI32, lkI64, lkF32, lkF64, lkPtr, lkUnknown

  Leaves = set[LeafKind]

  Facts = object
    ## Proc-wide facts the matcher consults, gathered in one walk first.
    locals: HashSet[SymId]           ## params and every `var` the body declares
    addrTaken: HashSet[SymId]        ## locals whose location escapes (`addr`/`haddr`)
    jumpedTo: HashSet[SymId]         ## labels some `jmp` names
    writes: HashSet[SymId]           ## locals some whole-symbol assignment writes
    inits: Table[SymId, int]         ## a `var`'s initializer, by position

  MemSite = object
    pos: int                         ## the loop / the store
    start: int                       ## what replaces it, or goes before it
    insert: bool                     ## a guarded fast path IN FRONT of the loop

  MemOpts* = object
    ## What the target allows. The driver fills it from the command line.
    intrinsics*: bool                ## the back end lowers the `Memset`/`Memcpy` rows
    mergeWordBytes*: int             ## > 0: merge adjacent constant field stores
                                     ## into stores up to this many bytes (a
                                     ## little-endian target that does not
                                     ## reorder by type — never the C backend)

  MemScan = object
    ## The match side. It also BUILDS each replacement into `scratch` as soon as
    ## a site matches: the element type is a `typenav` answer, and typenav's
    ## scopes only describe the site while the walk is standing on it.
    buf: ptr TokenBuf
    m: ptr MainModule
    facts: Facts
    scratch: TokenBuf
    sites: seq[MemSite]
    opts: MemOpts
    suffix: string                   ## the module's, for the intrinsic names
    used: set[IntrinsicOp]           ## which mem intrinsics were emitted

proc posOf(s: MemScan; n: Cursor): int {.inline.} =
  cursorToPosition(s.buf[], n)

# ---- proc-wide facts -------------------------------------------------------

proc rootLocal(n: Cursor): SymId =
  ## The symbol whose storage the location `n` lies in, when it is a variable
  ## itself rather than memory behind a pointer.
  result = SymId(0)
  var c = n
  var done = false
  while not done:
    done = true
    if c.kind == Symbol:
      result = c.symId
    elif c.kind == TagLit and c.exprKind in {DotC, AtC, ParC}:
      c = c.childCursor
      done = false

proc gather(f: var Facts; buf: TokenBuf; n: Cursor) =
  if n.kind != TagLit: return
  case n.stmtKind
  of JmpS:
    let l = n.childCursor
    if l.kind == Symbol: f.jumpedTo.incl l.symId
  of AsgnS, KeepovfS:
    let lhs = n.childCursor
    if lhs.kind == Symbol: f.writes.incl lhs.symId
  of VarS:
    var c = n
    let decl = takeVarDecl(c)
    if decl.name.kind == SymbolDef:
      f.locals.incl decl.name.symId
      if decl.value.kind != DotToken:
        f.inits[decl.name.symId] = cursorToPosition(buf, decl.value)
  else:
    if n.exprKind in {AddrC, HaddrC}:
      let r = rootLocal(n.childCursor)
      if r != SymId(0): f.addrTaken.incl r
  var it = n
  it.into:
    while it.hasMore:
      gather(f, buf, it)
      skip it

proc gatherFacts(buf: var TokenBuf; params: Cursor): Facts =
  result = Facts()
  if params.kind == TagLit:
    var p = params
    p.loopInto:
      if p.substructureKind == ParamU:
        let d = takeParamDecl(p)
        if d.name.kind == SymbolDef: result.locals.incl d.name.symId
      else:
        skip p
  var n = beginRead(buf)
  gather(result, buf, n)
  endRead n

# ---- types -----------------------------------------------------------------

proc leavesOf(m: ptr MainModule; t: Cursor; depth = 0): Leaves =
  ## The scalar classes a value of type `t` consists of.
  if depth > 16: return {lkUnknown}
  let b = navigateToObjectBody(m[], t)
  if b.kind != TagLit: return {lkUnknown}
  case b.typeKind
  of IT, UT, CT:
    let bits = b.childCursor
    if bits.kind != IntLit: return {lkUnknown}
    case intVal(bits)
    of 8: {lkByte}
    of 16: {lkI16}
    of 32: {lkI32}
    of 64: {lkI64}
    else: {lkI32, lkI64}                      # `(i -1)`: the target's word
  of BoolT: {lkByte}
  of FT:
    let bits = b.childCursor
    if bits.kind == IntLit and intVal(bits) == 32: {lkF32}
    elif bits.kind == IntLit and intVal(bits) == 64: {lkF64}
    else: {lkUnknown}
  of PtrT, AptrT, ProctypeT: {lkPtr}
  of EnumT, ArrayT, FlexarrayT:               # the base / element type comes first
    leavesOf(m, b.childCursor, depth + 1)
  of ObjectT, UnionT:
    var res: Leaves = {}
    var c = b.childCursor
    if b.typeKind == ObjectT and c.hasMore:
      if c.kind != DotToken: res.incl leavesOf(m, c, depth + 1)   # the base object
      skip c
    while c.hasMore:
      if c.kind == TagLit and c.substructureKind == FldU:
        var f = c.childCursor
        skip f                                # name
        if f.hasMore: skip f                  # pragmas
        if f.hasMore: res.incl leavesOf(m, f, depth + 1)
        else: res.incl lkUnknown
      elif c.kind != DotToken:
        res.incl lkUnknown                    # a case-object branch: not modelled
      skip c
    res
  else: {lkUnknown}

proc mayClobber(stored, loaded: Leaves): bool {.inline.} =
  ## Can a store of `stored` change a location of type `loaded`?
  lkUnknown in stored + loaded or lkByte in stored + loaded or
    stored * loaded != {}

proc elemByteSize(m: ptr MainModule; t: Cursor): int =
  ## 1 for a one-byte scalar, 0 for anything else (we only need to know).
  let b = navigateToObjectBody(m[], t)
  result = 0
  if b.kind == TagLit:
    case b.typeKind
    of IT, UT, CT:
      let bits = b.childCursor
      if bits.kind == IntLit and intVal(bits) == 8: result = 1
    of BoolT: result = 1
    else: discard

proc maxOfType(m: ptr MainModule; t: Cursor; v: var int64; depth = 0): bool =
  ## The largest value the integer type `t` can hold, when it is one we can
  ## name. An enum holds what its base type holds: that is where a loop over
  ## it would wrap.
  let b = navigateToObjectBody(m[], t)
  result = false
  if b.kind == TagLit and b.typeKind == EnumT and depth < 4:
    result = maxOfType(m, b.childCursor, v, depth + 1)
  elif b.kind == TagLit and b.typeKind in {IT, UT, CT}:
    let bits = b.childCursor
    if bits.kind == IntLit:
      let n = intVal(bits)
      if b.typeKind == IT and n in 8 .. 64:
        v = if n == 64: high(int64) else: (1'i64 shl (n - 1)) - 1
        result = true
      elif n in 8 .. 63:
        v = (1'i64 shl n) - 1
        result = true
      elif n == 64:
        v = high(int64)                       # all a signed comparison can use
        result = true

# ---- values ----------------------------------------------------------------

proc truncateTo(m: ptr MainModule; t: Cursor; bits: var uint64; depth = 0): bool =
  ## What a `conv`/`cast` to the type `t` leaves of `bits`; false unless `t`
  ## is an integer type or an enum. The target's word, `(i -1)`, keeps all 64
  ## bits: the store that finally consumes the value masks it to its own width.
  let b = navigateToObjectBody(m[], t)
  if b.kind == TagLit and b.typeKind == EnumT and depth < 4:
    result = truncateTo(m, b.childCursor, bits, depth + 1)
  else:
    result = b.kind == TagLit and b.typeKind in {IT, UT, CT} and
      b.childCursor.kind == IntLit
    let n = if result: intVal(b.childCursor) else: 0
    if n in 1 .. 63:
      let mask = (1'u64 shl n) - 1'u64
      bits = bits and mask
      if b.typeKind == IT and (bits shr (n - 1)) != 0'u64: bits = bits or not mask

proc safeLocal(s: MemScan; sym: SymId): bool {.inline.} =
  sym in s.facts.locals and sym notin s.facts.addrTaken

proc constBits(s: MemScan; n: Cursor; bits: var uint64; depth = 0): bool =
  ## The bit pattern of a constant: a literal, `true`, `false` or `nil`, seen
  ## through `suf`/`par`, through a `conv`/`cast` to an integer type and
  ## through a local only ever written by its initializer (copyprop leaves the
  ## `for` loop's bound temp alone). A
  ## float only as `0.0` (not `-0.0`): the width of any other is the
  ## context's. An object constructor only when it is made of nothing but
  ## zeros: an `oconstr` is TOTAL (it names every field) and lengc renders it
  ## as a C designated initializer, so the bytes it leaves unnamed — padding,
  ## the other members of a union — are not promised anything a zero would
  ## break.
  result = false
  if depth <= 32:
    case n.kind
    of IntLit:
      bits = cast[uint64](intVal(n))
      result = true
    of UIntLit:
      bits = uintVal(n)
      result = true
    of CharLit:
      bits = uint64(ord(charLit(n)))
      result = true
    of FloatLit:
      bits = cast[uint64](floatVal(n))
      result = bits == 0'u64
    of Symbol:
      let sym = n.symId
      if s.safeLocal(sym) and sym notin s.facts.writes and s.facts.inits.hasKey(sym):
        var init = cursorAt(s.buf[], s.facts.inits.getOrDefault(sym))
        result = constBits(s, init, bits, depth + 1)
        endRead init
    of TagLit:
      case n.exprKind
      of FalseC, NilC:
        bits = 0
        result = true
      of TrueC:
        bits = 1
        result = true
      of SufC, ParC:
        result = constBits(s, n.childCursor, bits, depth + 1)
      of ConvC, CastC:
        var c = n.childCursor
        let target = c
        skip c
        result = constBits(s, c, bits, depth + 1) and
          (bits == 0'u64 or truncateTo(s.m, target, bits))
      of OconstrC:
        var c = n.childCursor
        skip c                                # the type
        result = true
        while result and c.hasMore:
          var v = c                           # an inherited part: `(oconstr Base …)`
          if c.kind == TagLit and c.substructureKind == KvU:
            v = c.childCursor
            skip v                            # the field
          result = v.hasMore and constBits(s, v, bits, depth + 1) and bits == 0'u64
          skip c
        bits = 0
      else: discard
    else: discard

proc byteValue(s: MemScan; v: Cursor; elemType: Cursor; b: var int64): bool =
  ## The byte `memset` must write to reproduce storing `v` in every element:
  ## any constant for a one-byte element, all-zero bits for a wider one.
  var bits = 0'u64
  result = constBits(s, v, bits) and
    (bits == 0'u64 or elemByteSize(s.m, elemType) == 1)
  b = int64(bits and 0xFF'u64)

# ---- loop invariance -------------------------------------------------------

proc invariantValue(s: MemScan; n: Cursor; iv: SymId; stored: Leaves): bool
proc invariantLoc(s: MemScan; n: Cursor; iv: SymId; stored: Leaves): bool =
  ## The ADDRESS of location `n` is the same in every iteration.
  case n.kind
  of Symbol: n.symId != iv
  of TagLit:
    case n.exprKind
    of DotC, ParC: invariantLoc(s, n.childCursor, iv, stored)
    of AtC:
      var c = n.childCursor
      let base = c
      skip c
      invariantLoc(s, base, iv, stored) and invariantValue(s, c, iv, stored)
    of DerefC: invariantValue(s, n.childCursor, iv, stored)
    of PatC:
      var c = n.childCursor
      let base = c
      skip c
      invariantValue(s, base, iv, stored) and invariantValue(s, c, iv, stored)
    else: false
  else: false

proc invariantValue(s: MemScan; n: Cursor; iv: SymId; stored: Leaves): bool =
  ## The VALUE of `n` is the same in every iteration: nothing in it is the
  ## induction variable, and no element store can change what it reads.
  case n.kind
  of IntLit, UIntLit, CharLit, FloatLit: true
  of Symbol:
    let sym = n.symId
    if sym == iv: false
    elif s.safeLocal(sym): true
    else:
      # A global, a proc, or a local whose address escaped: a store reaches
      # it only if the types allow it.
      not mayClobber(stored, leavesOf(s.m, getType(s.m[], n)))
  of TagLit:
    case n.exprKind
    of SufC, ParC: invariantValue(s, n.childCursor, iv, stored)
    of ConvC, CastC:
      var c = n.childCursor
      skip c                                  # the type
      invariantValue(s, c, iv, stored)
    of AddC, SubC, MulC, DivC, ModC, ShlC, ShrC, BitandC, BitorC, BitxorC,
       NegC, BitnotC:
      var c = n.childCursor
      skip c                                  # the type
      var res = true
      while res and c.hasMore:
        res = invariantValue(s, c, iv, stored)
        skip c
      res
    of AddrC, HaddrC: invariantLoc(s, n.childCursor, iv, stored)
    of DotC, AtC, DerefC, PatC:
      # A LOAD. Its address must not move, and the stores must not be able to
      # write what it reads — unless it reads a local's own storage, which no
      # store through a pointer reaches.
      let r = rootLocal(n)
      invariantLoc(s, n, iv, stored) and
        ((r != SymId(0) and s.safeLocal(r) and n.exprKind in {DotC, AtC}) or
         not mayClobber(stored, leavesOf(s.m, getType(s.m[], n))))
    else: false
  else: false

# ---- the loop grammar ------------------------------------------------------

type
  LoopMatch = object
    ## A matched loop, as cursors into the body: valid while the scan stands
    ## on it, which is all the build below needs.
    cond, hi, incTyp, one, lhs, value, elemType: Cursor
    iv, exitLab: SymId
    isLe: bool

  StmtRole = enum
    ## What the next statement of a sibling run must be.
    srIf                             ## `(if (elif COND BODY) (else EXIT))`
    srStore                          ## `(asgn (at B IV) V)` / `(asgn (pat P IV) V)`
    srIncrement                      ## `(asgn IV (add T IV 1))`
    srExit                           ## `(jmp L)`, `L` the label after the loop

proc isOne(n: Cursor): bool =
  case n.kind
  of IntLit: intVal(n) == 1
  of UIntLit: uintVal(n) == 1'u64
  of TagLit:
    if n.exprKind in {SufC, ParC}: isOne(n.childCursor)
    elif n.exprKind == ConvC:
      var c = n.childCursor
      skip c
      isOne(c)
    else: false
  else: false

proc matchIncrement(n: Cursor; iv: SymId; typ, one: var Cursor): bool =
  ## `(asgn IV (add T IV 1))`.
  result = false
  if n.kind == TagLit and n.stmtKind == AsgnS:
    var c = n.childCursor
    if c.kind == Symbol and c.symId == iv:
      skip c
      if c.kind == TagLit and c.exprKind == AddC:
        var a = c.childCursor
        typ = a
        skip a
        if a.kind == Symbol and a.symId == iv:
          skip a
          if isOne(a):
            one = a
            skip a
            result = not a.hasMore

proc isElemAt(n: Cursor; iv: SymId): bool =
  ## `(at B IV)` / `(pat P IV)`: element `IV` of an array or a pointer.
  result = false
  if n.kind == TagLit and n.exprKind in {AtC, PatC}:
    var idx = n.childCursor
    skip idx
    if idx.kind == Symbol and idx.symId == iv:
      skip idx
      result = not idx.hasMore

proc matchStore(n: Cursor; iv: SymId; lhs, value: var Cursor): bool =
  ## `(asgn (at B IV) V)` / `(asgn (pat P IV) V)`.
  result = false
  if n.kind == TagLit and n.stmtKind == AsgnS:
    var c = n.childCursor
    if isElemAt(c, iv):
      lhs = c
      skip c
      if c.hasMore:
        value = c
        skip c
        result = not c.hasMore

proc matchCond(s: MemScan; n: Cursor; lm: var LoopMatch): bool =
  ## `(lt IV HI)` / `(le IV HI)`, `IV` a local whose address is never taken.
  result = false
  if n.kind == TagLit and n.exprKind in {LtC, LeC}:
    var c = n.childCursor
    if c.kind == Symbol and s.safeLocal(c.symId):
      lm.cond = n
      lm.isLe = n.exprKind == LeC
      lm.iv = c.symId
      skip c
      lm.hi = c
      skip c
      result = not c.hasMore

proc isDeadLabel(s: MemScan; n: Cursor): bool =
  ## A `(lab L)` nobody jumps to: inlining leaves them behind.
  result = false
  if n.kind == TagLit and n.stmtKind == LabS:
    let l = n.childCursor
    result = l.kind == SymbolDef and l.symId notin s.facts.jumpedTo

proc matchStmts(s: var MemScan; first: Cursor; roles: openArray[StmtRole];
                lm: var LoopMatch): bool

proc matchIf(s: var MemScan; n: Cursor; lm: var LoopMatch): bool =
  ## `(if (elif COND STORE INCREMENT) (else EXIT))`.
  result = false
  if n.kind == TagLit and n.stmtKind == IfS:
    let elifNode = n.childCursor
    var arm = elifNode
    if arm.substructureKind == ElifU:
      skip arm
      if arm.hasMore and arm.substructureKind == ElseU:
        let elseNode = arm
        skip arm
        var e = elifNode.childCursor
        if not arm.hasMore and matchCond(s, e, lm):
          skip e
          result = matchStmts(s, e, [srStore, srIncrement], lm) and
            matchStmts(s, elseNode.childCursor, [srExit], lm)

proc matchRole(s: var MemScan; n: Cursor; role: StmtRole; lm: var LoopMatch): bool =
  case role
  of srIf: matchIf(s, n, lm)
  of srStore: matchStore(n, lm.iv, lm.lhs, lm.value)
  of srIncrement: matchIncrement(n, lm.iv, lm.incTyp, lm.one)
  of srExit:
    n.kind == TagLit and n.stmtKind == JmpS and
      n.childCursor.kind == Symbol and n.childCursor.symId == lm.exitLab

proc matchRun(s: var MemScan; first: Cursor; roles: openArray[StmtRole];
              next: var int; lm: var LoopMatch): bool =
  ## Match the statements of the sibling run starting at `first` against
  ## `roles[next..]`, seen through `(stmts …)`/`(scope …)` and past `.` holes
  ## and labels nobody jumps to. A label somebody DOES jump to is control flow
  ## we do not model: no match.
  result = true
  var it = first
  while result and it.hasMore:
    if it.kind == TagLit and it.stmtKind in {StmtsS, ScopeS}:
      result = matchRun(s, it.childCursor, roles, next, lm)
    elif it.kind != DotToken and not isDeadLabel(s, it):
      result = next < roles.len and matchRole(s, it, roles[next], lm)
      inc next
    skip it

proc matchStmts(s: var MemScan; first: Cursor; roles: openArray[StmtRole];
                lm: var LoopMatch): bool =
  ## The sibling run starting at `first` is exactly `roles`, in order.
  var next = 0
  result = matchRun(s, first, roles, next, lm) and next == roles.len

proc matchLoop(s: var MemScan; w: Cursor; lm: var LoopMatch): bool =
  ## The loop grammar of the module comment, up to the store: everything a
  ## fill and a copy have in common.
  var c = w.childCursor
  result = c.kind == TagLit and c.exprKind == TrueC
  if result:
    skip c
    result = matchStmts(s, c, [srIf], lm)
  if result:
    lm.elemType = getNominalType(s.m[], lm.lhs)

proc baseInvariant(s: MemScan; elem: Cursor; iv: SymId; stored: Leaves): bool =
  ## The array (`at`) or pointer (`pat`) an element access indexes is the same
  ## in every iteration.
  let base = elem.childCursor
  if elem.exprKind == AtC: invariantLoc(s, base, iv, stored)
  else: invariantValue(s, base, iv, stored)

proc boundOk(s: MemScan; lm: LoopMatch; stored: Leaves): bool =
  ## `HI` does not change, and the loop terminates: `IV <= HI` with `HI` the
  ## type's maximum never becomes false, and a `memset` must not make it.
  result = invariantValue(s, lm.hi, lm.iv, stored)
  if result and lm.isLe:
    var hv = 0'u64
    var maxv = 0'i64
    result = constBits(s, lm.hi, hv) and maxOfType(s.m, lm.incTyp, maxv) and
      cast[int64](hv) < maxv

proc localArray(s: MemScan; elem: Cursor): SymId =
  ## The local an `(at …)` element lives in, when its address is never taken.
  result = SymId(0)
  if elem.exprKind == AtC:
    let r = rootLocal(elem.childCursor)
    if r != SymId(0) and s.safeLocal(r): result = r

proc staticallyDisjoint(s: MemScan; dst, src: Cursor): bool =
  ## Two element ranges that cannot overlap whatever the pointers hold: arrays
  ## stored in two different locals, or one such array and anything reached
  ## through a pointer — no pointer leads into a local whose address is never
  ## taken.
  let a = localArray(s, dst)
  let b = localArray(s, src)
  result = (a != SymId(0) or b != SymId(0)) and a != b

# ---- build -----------------------------------------------------------------

proc emitIntType(b: var TokenBuf; tag: LengType) =
  ## `(i -1)` / `(u -1)`: the target's word.
  b.openTag TagId(tag)
  b.addIntLit -1
  b.closeTag()

proc emitCount(b: var TokenBuf; lm: LoopMatch) =
  ## The iterations left: `HI - IV`, plus one for `le`, in the loop's own type.
  if lm.isLe:
    b.openTag TagId(AddC)
    b.addSubtree lm.incTyp
  b.openTag TagId(SubC)
  b.addSubtree lm.incTyp
  b.addSubtree lm.hi
  b.addSymUse lm.iv
  b.closeTag()
  if lm.isLe:
    b.addSubtree lm.one
    b.closeTag()

proc emitBytes(b: var TokenBuf; lm: LoopMatch) =
  ## `(mul (i -1) COUNT (sizeof E))`.
  b.openTag TagId(MulC)
  emitIntType(b, IT)
  b.openTag TagId(ConvC)
  emitIntType(b, IT)
  emitCount(b, lm)
  b.closeTag()
  b.openTag TagId(ConvC)
  emitIntType(b, IT)
  b.openTag TagId(SizeofC)
  b.addSubtree lm.elemType
  b.closeTag()
  b.closeTag()
  b.closeTag()

proc emitAddr(b: var TokenBuf; elem: Cursor) =
  b.openTag TagId(AddrC)
  b.addSubtree elem
  b.closeTag()

proc emitFinalIv(b: var TokenBuf; lm: LoopMatch) =
  ## `(asgn IV <the value the loop leaves in it>)`.
  b.openTag TagId(AsgnS)
  b.addSymUse lm.iv
  if lm.isLe:
    b.openTag TagId(AddC)
    b.addSubtree lm.incTyp
    b.addSubtree lm.hi
    b.addSubtree lm.one
    b.closeTag()
  else:
    b.addSubtree lm.hi
  b.closeTag()

proc memIntrinsicName(op: IntrinsicOp; suffix: string): string =
  ## The module-local declaration an `(instr …)` this pass emits names; see
  ## `addMemIntrinsicDecl`.
  (if op == MemcpyOp: "memcpy." else: "memset.") & suffix

proc emitMemStmt(b: var TokenBuf; lm: LoopMatch; name: string; isCopy: bool;
                 byteVal: int64) =
  ## `(instr memset (addr DST) BYTE N)` / `(instr memcpy (addr DST) (addr SRC) N)`.
  b.openTag TagId(InstrS)
  b.addSymUse name
  emitAddr(b, lm.lhs)
  if isCopy: emitAddr(b, lm.value)
  else: b.addIntLit byteVal
  emitBytes(b, lm)
  b.closeTag()

proc emitAddrWord(b: var TokenBuf; elem: Cursor) =
  ## The address of `elem` as an unsigned word, for comparing.
  b.openTag TagId(CastC)
  emitIntType(b, UT)
  emitAddr(b, elem)
  b.closeTag()

proc emitEndsBefore(b: var TokenBuf; lm: LoopMatch; first, second: Cursor) =
  ## `&first + BYTES <= &second`: the range starting at `first` ends before
  ## `second` begins.
  b.openTag TagId(LeC)
  b.openTag TagId(AddC)
  emitIntType(b, UT)
  emitAddrWord(b, first)
  b.openTag TagId(ConvC)
  emitIntType(b, UT)
  emitBytes(b, lm)
  b.closeTag()
  b.closeTag()
  emitAddrWord(b, second)
  b.closeTag()

proc emitReplacement(b: var TokenBuf; lm: LoopMatch; name: string; isCopy: bool;
                     byteVal: int64; guardDisjoint: bool) =
  ## The `(if …)` that does the loop's work in one call to the intrinsic
  ## `name`:
  ##
  ##   (if (elif COND (stmts MEMSTMT (asgn IV FINAL))))
  ##
  ## With `guardDisjoint` the condition also demands that the two ranges do
  ## not overlap, and the original loop stays BEHIND it: on the fast path it
  ## finds `IV` already at its final value and exits at once, otherwise it
  ## runs as it always did. Leaving it in place rather than copying it into
  ## an `else` keeps every label in it defined exactly once.
  b.openTag TagId(IfS)
  b.openTag TagId(ElifU)
  if guardDisjoint:
    b.openTag TagId(AndC)
    b.addSubtree lm.cond
    b.openTag TagId(OrC)
    emitEndsBefore(b, lm, lm.lhs, lm.value)
    emitEndsBefore(b, lm, lm.value, lm.lhs)
    b.closeTag()
    b.closeTag()
  else:
    b.addSubtree lm.cond
  b.openTag TagId(StmtsS)
  emitMemStmt(b, lm, name, isCopy, byteVal)
  emitFinalIv(b, lm)
  b.closeTag()
  b.closeTag()
  b.closeTag()

proc tryLoop(s: var MemScan; w: Cursor; exitLab: SymId) =
  ## Match the loop `w` (its exit label `exitLab` follows it) and, when it is a
  ## fill or a copy, build its replacement. A copy is `D[IV] = S[IV]` (the
  ## assignment says the element types agree); unless its two ranges are
  ## disjoint by construction, it becomes a guarded fast path IN FRONT of the
  ## loop.
  var lm = LoopMatch(exitLab: exitLab)
  if matchLoop(s, w, lm):
    let stored = leavesOf(s.m, lm.elemType)
    let isCopy = isElemAt(lm.value, lm.iv)
    var byteVal = 0'i64
    let valueOk =
      if isCopy: baseInvariant(s, lm.value, lm.iv, stored)
      else: byteValue(s, lm.value, lm.elemType, byteVal)
    if valueOk and baseInvariant(s, lm.lhs, lm.iv, stored) and boundOk(s, lm, stored):
      let guarded = isCopy and not staticallyDisjoint(s, lm.lhs, lm.value)
      let op = if isCopy: MemcpyOp else: MemsetOp
      s.used.incl op
      let start = s.scratch.len
      emitReplacement(s.scratch, lm, memIntrinsicName(op, s.suffix), isCopy,
                      byteVal, guarded)
      s.sites.add MemSite(pos: s.posOf(w), start: start, insert: guarded)

# ---- adjacent field stores ------------------------------------------------
#
# `o.a = 1; o.b = 2` with `a`, `b` adjacent one-byte fields becomes ONE 16-bit
# store of `0x0201` — fewer stores, and on a modern core the store buffer and
# the store ports are what such init sequences are bound by. Object
# initializations are full of these: constant fields, mostly zeros.
#
# Only for a back end that neither reorders memory accesses by type nor runs
# on a big-endian machine — arkham, never C: writing two `uint8` fields
# through a `uint16*` is undefined there, and `gcc -O3` with strict aliasing
# may move a later byte load in front of the merged store. The driver passes
# `--merge-stores:<word bits>` for the native back end only.
#
# A merged store covers [off, off+W) with W in 2/4/8, `off` a multiple of W
# (naturally aligned: fine on every core, the ones that trap on a misaligned
# store included) and W at most the target word. Every byte in it belongs to
# a field the run stores or to padding — never to a field the run leaves
# alone. The layout is C's, which is arkham's; anything whose layout is not
# certain (packed, aligned, imported, bit fields, unions, inheritance) is left
# as it is.

type
  FieldSlot = object
    sym: SymId
    off, size: int

  StoreEntry = object
    pos: int                         ## the `(asgn …)`
    lhs: Cursor                      ## its `(dot B f)`
    fld: SymId
    off, size: int
    value: uint64                    ## the constant, masked to `size` bytes
    leaves: Leaves

proc hasLayoutPragma(pragmas: Cursor): bool =
  ## Pragmas that move fields away from C's natural layout, or that hand the
  ## layout to a C header we cannot see.
  result = false
  if pragmas.kind == TagLit:
    var p = pragmas.childCursor
    while not result and p.hasMore:
      result = p.kind == TagLit and p.pragmaKind in {PackedP, AlignP, ImportcP,
                                                      ImportcppP, NodeclP, BitsP}
      skip p

proc resolveTypeDecl(s: MemScan; t: Cursor; body: var Cursor): bool =
  ## Follow named types to their body; false for a declaration whose layout
  ## pragmas make the C rules inapplicable.
  var c = t
  var guard = 0
  while c.kind == Symbol and guard < 20:
    inc guard
    let d = s.m[].getDeclOrNil(c.symId)
    if d == nil or d.pos.stmtKind != TypeS: return false
    let td = asTypeDecl(d.pos)
    if hasLayoutPragma(td.pragmas): return false
    c = td.body
  body = c
  result = c.kind == TagLit

proc sizeAlignOf(s: MemScan; t: Cursor; size, align: var int; depth = 0): bool

proc layoutObject(s: MemScan; body: Cursor; fields: var seq[FieldSlot];
                  size, align: var int; depth: int): bool =
  ## C layout of a base-less object of plain fields.
  var c = body.childCursor
  if not c.hasMore or c.kind != DotToken: return false     # inheritance: no
  skip c
  var off = 0
  var maxAl = 1
  while c.hasMore:
    if not (c.kind == TagLit and c.substructureKind == FldU): return false
    var f = c.childCursor
    if f.kind != SymbolDef: return false
    let sym = f.symId
    skip f
    if f.kind != DotToken: return false                    # field pragmas: no
    skip f
    var fs = 0
    var fa = 0
    if not sizeAlignOf(s, f, fs, fa, depth + 1): return false
    off = (off + fa - 1) div fa * fa
    fields.add FieldSlot(sym: sym, off: off, size: fs)
    off += fs
    maxAl = max(maxAl, fa)
    skip c
  size = (off + maxAl - 1) div maxAl * maxAl
  align = maxAl
  result = true

proc sizeAlignOf(s: MemScan; t: Cursor; size, align: var int; depth = 0): bool =
  if depth > 16: return false
  var b = default(Cursor)
  if not resolveTypeDecl(s, t, b): return false
  result = true
  case b.typeKind
  of IT, UT, FT, CT:
    let bits = b.childCursor
    if bits.kind != IntLit: return false
    let n = int(intVal(bits))
    size = if n > 0: n div 8 else: s.opts.mergeWordBytes
    align = size
  of BoolT:
    size = 1
    align = 1
  of PtrT, AptrT, ProctypeT:
    size = s.opts.mergeWordBytes
    align = size
  of EnumT:
    result = sizeAlignOf(s, b.childCursor, size, align, depth + 1)
  of ArrayT:
    var c = b.childCursor
    var es = 0
    if not sizeAlignOf(s, c, es, align, depth + 1): return false
    skip c
    if c.kind != IntLit: return false
    size = es * int(intVal(c))
  of ObjectT:
    var fields: seq[FieldSlot] = @[]
    result = layoutObject(s, b, fields, size, align, depth)
  else:
    result = false

proc storeConst(s: MemScan; v: Cursor; size: int; bits: var uint64): bool =
  ## The bit pattern a constant store writes, masked to `size` bytes.
  result = constBits(s, v, bits)
  if result and size < 8:
    bits = bits and ((1'u64 shl (size * 8)) - 1'u64)

proc matchFieldStore(s: MemScan; n: Cursor; base: var Cursor; fld: var SymId;
                     value: var Cursor): bool =
  ## `(asgn (dot B f [0]) V)`.
  result = false
  if n.kind == TagLit and n.stmtKind == AsgnS:
    var c = n.childCursor
    if c.kind == TagLit and c.exprKind == DotC:
      var d = c.childCursor
      base = d
      skip d
      if d.kind == Symbol:
        fld = d.symId
        skip d
        if d.hasMore and d.kind == IntLit and intVal(d) == 0:
          skip d                           # an inheritance depth must be 0
        skip c
        if not d.hasMore and c.hasMore:
          value = c
          skip c
          result = not c.hasMore

proc baseMayTakeAddr(s: MemScan; base: Cursor): bool =
  ## Taking `&B.f` must not demote a local that could live in registers: the
  ## object has to be memory already — behind a pointer, a global, or a local
  ## whose address is taken anyway.
  let r = rootLocal(base)
  result = r == SymId(0) or r notin s.facts.locals or r in s.facts.addrTaken

proc emitMerged(b: var TokenBuf; e: StoreEntry; width: int; value: uint64) =
  ## `(asgn (deref (cast (ptr (u W)) (addr (dot B f)))) VALUE)`.
  b.openTag TagId(AsgnS)
  b.openTag TagId(DerefC)
  b.openTag TagId(CastC)
  b.openTag TagId(PtrT)
  b.openTag TagId(UT)
  b.addIntLit width * 8
  b.closeTag()
  b.closeTag()
  emitAddr(b, e.lhs)
  b.closeTag()
  b.closeTag()
  b.addUIntLit value
  b.closeTag()

proc mergeRun(s: var MemScan; base: Cursor; run: seq[StoreEntry];
              layout: seq[FieldSlot]; structSize: int) =
  ## Find the merged stores a run allows and record them.
  if run.len < 2: return
  var stored: Leaves = {}
  for e in run: stored.incl e.leaves
  if not baseMayTakeAddr(s, base) or not invariantLoc(s, base, SymId(0), stored):
    return
  # byte ownership: -2 padding, -1 a field the run does not store, else run index
  var owner = newSeq[int](structSize)
  for i in 0 ..< structSize: owner[i] = -2
  for f in layout:
    for i in f.off ..< f.off + f.size: owner[i] = -1
  for k, e in run:
    for i in e.off ..< e.off + e.size: owner[i] = k
  var merged = newSeq[bool](run.len)
  var order: seq[int] = @[]
  for k in 0 ..< run.len: order.add k
  for k in order:
    if merged[k]: continue
    let e = run[k]
    for width in [8, 4, 2]:
      if width > s.opts.mergeWordBytes or e.off mod width != 0 or
         e.off + width > structSize:
        continue
      var ok = true
      var members: seq[int] = @[]
      for i in e.off ..< e.off + width:
        let o = owner[i]
        if o == -1 or (o >= 0 and merged[o]):
          ok = false
          break
        if o >= 0 and o notin members: members.add o
      if not ok or members.len < 2: continue
      for m in members:                    # every member entirely inside
        if run[m].off + run[m].size > e.off + width:
          ok = false
      if not ok: continue
      var value = 0'u64
      for m in members:
        value = value or (run[m].value shl ((run[m].off - e.off) * 8))
      # The merged store takes the place of the FIRST member in program
      # order; the members are disjoint, so the order among them is free. Its
      # ADDRESS is that of the member at the start of the window, `e` — not
      # of whichever member came first: in `o.d = 4; o.c = 3` those differ.
      var first = members[0]
      for m in members:
        if run[m].pos < run[first].pos: first = m
      let start = s.scratch.len
      emitMerged(s.scratch, e, width, value)
      s.sites.add MemSite(pos: run[first].pos, start: start, insert: false)
      for m in members:
        merged[m] = true
        if m != first:
          s.sites.add MemSite(pos: run[m].pos, start: s.scratch.len, insert: false)
          s.scratch.addDotToken()             # the store goes; a hole is no statement
      break

proc storesField(run: seq[StoreEntry]; fld: SymId): bool =
  result = false
  for e in run: result = result or e.fld == fld

proc scanStoreRuns(s: var MemScan; list: Cursor) =
  ## Runs of adjacent constant stores to fields of one object, in one list.
  ## Anything else in between — a non-constant store included — ends a run:
  ## the stores cannot be moved across it.
  var run: seq[StoreEntry] = @[]
  var runBase = default(Cursor)
  var layout: seq[FieldSlot] = @[]
  var structSize = 0                       # 0: no layout for `runBase`
  var it = list.childCursor
  while it.hasMore:
    if it.kind == DotToken:                # a hole: not a statement
      skip it
      continue
    var base = default(Cursor)
    var value = default(Cursor)
    var fld = SymId(0)
    var added = false
    if matchFieldStore(s, it, base, fld, value):
      if run.len == 0 or not subtreeEqual(base, runBase) or storesField(run, fld):
        mergeRun(s, runBase, run, layout, structSize)
        run.setLen 0
        layout.setLen 0
        structSize = 0
        var body = default(Cursor)
        var align = 0
        runBase = base
        if not (resolveTypeDecl(s, getNominalType(s.m[], base), body) and
                body.typeKind == ObjectT and
                layoutObject(s, body, layout, structSize, align, 0)):
          structSize = 0
      if structSize > 0:
        for f in layout:
          var bits = 0'u64
          if f.sym == fld and storeConst(s, value, f.size, bits):
            let lhs = it.childCursor
            run.add StoreEntry(pos: s.posOf(it), lhs: lhs, fld: fld, off: f.off,
                               size: f.size, value: bits,
                               leaves: leavesOf(s.m, getNominalType(s.m[], lhs)))
            added = true
    if not added:
      mergeRun(s, runBase, run, layout, structSize)
      run.setLen 0
    skip it
  mergeRun(s, runBase, run, layout, structSize)

# ---- the walk --------------------------------------------------------------

proc exitLabelAfter(list: Cursor; w: Cursor; lab: var SymId): bool =
  ## Is the next real statement after `w` in its list a `(lab L)`?
  var it = w
  skip it
  while it.hasMore and it.kind == DotToken: skip it
  result = it.hasMore and it.kind == TagLit and it.stmtKind == LabS and
    it.childCursor.kind == SymbolDef
  if result: lab = it.childCursor.symId

proc scan(s: var MemScan; n: Cursor)

proc scanList(s: var MemScan; list: Cursor) =
  if s.opts.mergeWordBytes > 0: scanStoreRuns(s, list)
  var it = list.childCursor
  while it.hasMore:
    if it.kind == TagLit and it.stmtKind == WhileS:
      var lab = SymId(0)
      if s.opts.intrinsics and exitLabelAfter(list, it, lab): tryLoop(s, it, lab)
    scan(s, it)
    skip it

proc scan(s: var MemScan; n: Cursor) =
  ## Every statement list under `n`, keeping typenav's scopes in step with the
  ## walk so `getType` resolves the locals declared so far.
  if n.kind != TagLit: return
  case n.stmtKind
  of ScopeS:
    s.m[].openScope()
    scanList(s, n)
    s.m[].closeScope()
  of StmtsS:
    scanList(s, n)
  of VarS:
    var c = n.childCursor
    let name = c
    skip c                                    # name
    if c.hasMore: skip c                      # pragmas
    if c.hasMore and name.kind == SymbolDef:
      s.m[].registerLocal(name.symId, c)
  else:
    var it = n
    it.into:
      while it.hasMore:
        scan(s, it)
        skip it

proc addMemIntrinsicDecl*(dest: var TokenBuf; suffix: string; op: IntrinsicOp) =
  ## The `{.intrinsic.}` declaration the emitted `(instr …)` applications of
  ## `op` name, appended once per module, like the vectorizer's:
  ## declaration-only (body `.`), so there is nothing for DCE to keep alive and
  ## nothing a back end emits for it. The pragma string is what selects the
  ## row.
  let name = memIntrinsicName(op, suffix)
  dest.openTag TagId(ProcS)
  dest.addSymDef name
  dest.openTag TagId(ParamsT)
  for i in 0 ..< 3:
    dest.openTag TagId(ParamU)
    dest.addSymDef "a" & $i & "." & name
    dest.addDotToken()
    if i == 0 or (i == 1 and op == MemcpyOp):
      dest.openTag TagId(PtrT)                # dest / src
      emitIntType(dest, UT)
      dest.closeTag()
    else:
      emitIntType(dest, IT)                   # the byte / the size
    dest.closeTag()                           # param
  dest.closeTag()                             # params
  dest.addDotToken()                          # void result
  dest.openTag TagId(PragmasU)
  dest.openTag TagId(IntrinsicP)
  dest.addStrLit IntrinsicNames[op]
  dest.closeTag()
  dest.closeTag()                             # pragmas
  dest.addDotToken()                          # no body
  dest.closeTag()                             # proc

proc runMemIdioms*(buf: var TokenBuf; m: ptr MainModule; params: Cursor;
                   opts: MemOpts; suffix: string;
                   used: var set[IntrinsicOp]): int =
  ## Replace every fill loop in the body with a `Memset` and put a `Memcpy` in
  ## front of every copy loop; returns how many. `used` collects the rows the
  ## module then needs declarations for (`addMemIntrinsicDecl`).
  ## `m` must have the proc's scope open with its params registered.
  result = 0
  if m == nil: return
  var s = MemScan(buf: addr buf, m: m, facts: gatherFacts(buf, params),
                  scratch: createTokenBuf(64, buf.pool, buf.tags), opts: opts,
                  suffix: suffix, used: {})
  var root = beginRead(buf)
  m[].openScope()
  while root.hasMore:
    scan(s, root)
    skip root
  m[].closeScope()
  endRead root
  if s.sites.len > 0:
    var patch = initPatchset(addr buf)
    for site in s.sites:
      if site.insert: patch.addInsert(site.pos, cursorAt(s.scratch, site.start))
      else: patch.addSubst(site.pos, cursorAt(s.scratch, site.start))
    var nb = patch.apply()
    buf = ensureMove(nb)
    result = s.sites.len
  used = used + s.used

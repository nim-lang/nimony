#
#
#        Lengc access paths for CSE invalidation
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Access paths in the sense of `doc/cse.md`: a root symbol plus one selector
## per step of an lvalue, so that "can this write change what that read reads?"
## is a **prefix comparison on the paths**, not a question about their types.
##
## This is the vocabulary Nimony's borrow checker already uses
## (`nimony/contracts_fir.nim`: `BorrowInfo.path` + `pathsOverlap`), moved to
## Leng. The three rules it inherits:
##
## * **Indices collapse.** `a[i]` and `a[j]` both give `a :: Elem`. Distinguishing
##   them is arithmetic, which this analysis stays out of; the induction-variable
##   pass is where index reasoning belongs.
## * **A path ends at a pointer.** A `deref` is recorded as a step, not followed:
##   nothing here guesses what a pointer points to. A path that contains one is
##   *indirect*, and indirect paths are only ever compared coarsely.
## * **What cannot be described is not guessed at.** A call result, a forging
##   cast, an unrecognized shape: `known = false`, which every comparison reads
##   as "may overlap".
##
## `aliasing.accessRoots` performs this same walk and keeps only the root;
## everything below is that walk with the selectors retained. The one deliberate
## difference is index operands: `accessRoots` skips them (an index selects
## *within* the base, so it roots no separate object), while an invalidator must
## treat them as reads of their own — `a[y.i]` changes when `y.i` is written.
## `collectReadPaths` therefore descends into them, `pathOfLvalue` does not.

import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # exprKind, tag enums

const MaxPathSteps* = 8
  ## Paths this long do not occur in lowered Leng (`x.a.b[i].c` is 4). Beyond
  ## the bound the tail is dropped and `truncated` is set, which only ever makes
  ## a comparison answer "may overlap".

type
  StepKind* = enum
    FieldStep,   ## `(dot base fld)` — the selected field symbol
    ElemStep,    ## `(at base i)` / `(pat base i)` — index collapsed away
    DerefStep    ## `(deref base)` / the pointer step of `(pat …)`

  PathStep* = object
    kind*: StepKind
    field*: SymId   ## `FieldStep` only

  AccessPath* = object
    ## A value type on purpose: paths are built per invalidation event and per
    ## cache entry, and a heap allocation there would show up in shoggoth's own
    ## runtime, which is compile time for everyone else.
    root*: SymId          ## `SymId(0)` when there is none
    known*: bool          ## false ⇒ every comparison answers "may overlap"
    truncated*: bool      ## deeper than `MaxPathSteps`
    len*: int
    steps*: array[MaxPathSteps, PathStep]

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc unknownPath*(): AccessPath {.inline.} =
  AccessPath(root: SymId(0), known: false, truncated: false, len: 0)

proc add(p: var AccessPath; s: PathStep) {.inline.} =
  if p.len < MaxPathSteps:
    p.steps[p.len] = s
    inc p.len
  else:
    p.truncated = true

proc isIndirect*(p: AccessPath): bool {.inline.} =
  ## Does the path pass through a pointer? An unknown path counts as indirect:
  ## the caller must treat it as reaching anywhere.
  if not p.known: return true
  for i in 0 ..< p.len:
    if p.steps[i].kind == DerefStep: return true
  result = false

proc isDirect*(p: AccessPath): bool {.inline.} =
  ## Rooted in a named symbol and reached without following a pointer, so the
  ## storage it names is exactly the root's.
  p.known and p.root != SymId(0) and not isIndirect(p)

proc samePrefix*(a, b: AccessPath): bool =
  ## Do the two paths select overlapping storage, assuming a common root? True
  ## iff one selector list is a prefix of the other. `a.b` vs `a.b.c` overlap;
  ## `a.b` vs `a.c` do not. `ElemStep` matches `ElemStep` — indices are collapsed,
  ## so two elements of one array always overlap.
  let n = min(a.len, b.len)
  for i in 0 ..< n:
    if a.steps[i].kind != b.steps[i].kind: return false
    if a.steps[i].kind == FieldStep and a.steps[i].field != b.steps[i].field:
      return false
  result = true

proc directOverlap*(a, b: AccessPath): bool {.inline.} =
  ## For two DIRECT paths: distinct roots are distinct storage, so the whole
  ## question is the root plus the prefix test.
  a.root == b.root and samePrefix(a, b)

# ---- extraction -----------------------------------------------------------

proc build(n: Cursor; p: var AccessPath) =
  ## Walk outside-in, append the step on the way back out, so `steps` ends up in
  ## root-first order (`x.a[i]` ⇒ `[Field a, Elem]`).
  case n.kind
  of Symbol, SymbolDef:
    p.root = symId(n)
    p.known = true
  of TagLit:
    case n.exprKind
    of DotC:
      var fld = firstChild(n)
      skip fld                       # past the object; the field symbol follows
      build(firstChild(n), p)
      if p.known:
        if fld.kind == Symbol:
          p.add PathStep(kind: FieldStep, field: symId(fld))
        else:
          # A field slot that is not a plain symbol: describe no further rather
          # than describe wrongly.
          p.truncated = true
    of AtC:
      build(firstChild(n), p)
      if p.known: p.add PathStep(kind: ElemStep, field: SymId(0))
    of DerefC:
      build(firstChild(n), p)
      if p.known: p.add PathStep(kind: DerefStep, field: SymId(0))
    of PatC:
      # `p[i]` through a pointer: the pointer step and then the element.
      build(firstChild(n), p)
      if p.known:
        p.add PathStep(kind: DerefStep, field: SymId(0))
        p.add PathStep(kind: ElemStep, field: SymId(0))
    of AddrC, HaddrC:
      # The VALUE is an address; as a location it is the addressed lvalue, which
      # is what a store target or a conservative reader wants.
      build(firstChild(n), p)
    of ConvC, CastC:
      var inner = firstChild(n)
      skip inner                     # the type operand
      build(inner, p)
    of BaseobjC:
      var inner = firstChild(n)
      skip inner                     # type
      skip inner                     # inheritance-depth intlit
      build(inner, p)
    of ParC:
      build(firstChild(n), p)
    else:
      p = unknownPath()
  else:
    p = unknownPath()

proc pathOfLvalue*(n: Cursor): AccessPath =
  ## The path an lvalue names — a store target, or the principal path of a read.
  ## Index operands are NOT descended into: they are values selecting within the
  ## base, not part of the location.
  result = AccessPath(root: SymId(0), known: false, truncated: false, len: 0)
  build(n, result)

proc collectReadPaths*(n: Cursor; acc: var seq[AccessPath]) =
  ## Every path the expression READS: its own, plus those of index operands and
  ## of any nested operand. An entry is invalidated when a write may overlap any
  ## one of them.
  case n.kind
  of Symbol, SymbolDef:
    acc.add pathOfLvalue(n)
  of TagLit:
    case n.exprKind
    of DotC, AtC, DerefC, PatC, AddrC, HaddrC, ConvC, CastC, BaseobjC, ParC:
      acc.add pathOfLvalue(n)
      # The selectors themselves can read memory (`a[y.i]`), so walk the operands
      # that are not part of the location.
      case n.exprKind
      of AtC, PatC:
        var idx = firstChild(n)
        skip idx                     # the base
        collectReadPaths(idx, acc)
      else: discard
    of CallC, InstrC:
      # Neither a call's value nor an intrinsic's is described by any path
      # (`isPureExpr` keeps both out of the load cache; guard facts can still hold
      # one). Conservative for a PURE intrinsic too, and free: a pure row's value
      # is not a memory path either, so there was never a path to lose.
      acc.add unknownPath()
    else:
      # Constructors, arithmetic, comparisons: the union of the operands' reads.
      var r = n
      r.loopInto:
        collectReadPaths(r, acc)
        skip r
  else:
    discard                          # literals read no memory

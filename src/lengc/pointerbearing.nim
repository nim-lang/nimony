#
#
#        Leng "can this value hold a pointer?"
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The type- and shape-based pointer test shared by every pass that decides
## whether copying a value can create aliasing: shoggoth's `aliasing` (which
## unions two partition classes) and hexer's `funcsummary` (which decides
## whether a call's actual argument can escape through the callee).
##
## Both answers come from `typenav`: Leng is nominal — `(param :x.0 . Point.0.M)`,
## never an inline object — so a pass that cannot follow a `Symbol` in a type
## slot has to call *every* aggregate pointer-bearing. `MainModule` resolves
## those, including across modules, on demand.
##
## `resolve` is for the passes whose `MainModule` cannot see the whole program.
## Shoggoth's can: it reads finished `.c.nif`s and every symbol resolves through
## the module index, so it passes `nil`. Hexer's is built over the module it is
## still *producing* (`nifmodules.fromBuffer`), so every **imported** symbol
## lands in `resolve` — not as an exception, as the common case — and is
## answered from that symbol's Nimony declaration.
##
## What is deliberately absent is a "could not work it out, so assume it points
## somewhere" branch. Every declaration exists by the time these passes run; a
## lookup that comes up empty is a bug in the resolution path, and it reports
## itself instead of degrading quietly into lost optimization.

import std / syncio                  # `quit`
import ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / "lib" / nifcdecl        # exprKind/typeKind, takeFieldDecl
import nifmodules                     # MainModule
import typenav                        # getType / navigateToObjectBody

type
  ForeignSymResolver* = proc (s: SymId): bool {.nimcall.}
    ## Answers "can a value of this symbol's type hold a pointer?" for a symbol
    ## the local `MainModule` does not declare, by loading that symbol's own
    ## declaration. `s` may name a type or a value.

  ForeignFieldResolver* = proc (owner: SymId; field: string): bool {.nimcall.}
    ## The same question for `owner.field`. A field needs its own entry point
    ## because a Leng field symbol is a *local* name: it carries no module, so
    ## it cannot be looked up on its own and has to be found on the type that
    ## declares it.

const MaxDepth = 100
  ## A type only nests into itself through a `ptr`, and that answers without
  ## recursing, so a by-value chain is finite. This bound only stops a malformed
  ## cycle from spinning, and reports if it ever trips.

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc isUnknownType*(t: Cursor): bool {.inline.} =
  ## `typenav.getType`'s `(err)` sentinel: a tag that is not a type tag at all.
  ## It means "could not recompute the type of this *expression*" — a question
  ## about the expression, not a missing declaration.
  t.kind == TagLit and t.typeKind == NoType

proc symName(m: ptr MainModule; s: SymId): string =
  if m != nil: m[].pool.syms[s] else: "<sym>"

proc tagName(m: ptr MainModule; c: Cursor): string =
  if m != nil and c.kind == TagLit: m[].tags.tags[c.cursorTagId] else: "?"

proc resolveSym(m: ptr MainModule; s: SymId; resolve: ForeignSymResolver;
                      resolveFld: ForeignFieldResolver): bool =
  if resolve == nil:
    quit "pointerbearing: no declaration for " & symName(m, s) &
         " and no resolver to ask"
  result = resolve(s)

proc resolveField(m: ptr MainModule; owner: SymId; fld: Cursor;
                  resolve: ForeignSymResolver;
                      resolveFld: ForeignFieldResolver): bool =
  if resolveFld == nil or fld.kind != Symbol:
    quit "pointerbearing: no declaration for a field of " & symName(m, owner) &
         " and no resolver to ask"
  result = resolveFld(owner, symName(m, fld.symId))

proc selectionBase(n: Cursor; found: var bool): Cursor =
  ## The operand an access path selects *within*: `(at a i)` → `a`,
  ## `(dot o f)` → `o`. One step, not all the way down to the root symbol —
  ## every step in a path has its own type and its own way of being resolved,
  ## and skipping past them throws that away.
  found = n.kind == TagLit
  result = n
  if not found: return
  case n.exprKind
  of DotC, AtC, PatC, DerefC, ParC:
    result = firstChild(n)
  of ConvC, CastC, BaseobjC:
    result = firstChild(n)
    skip result                    # the type operand
  else:
    found = false

proc pointerBearing*(m: ptr MainModule; typ: Cursor;
                     resolve: ForeignSymResolver = nil;
                     resolveFld: ForeignFieldResolver = nil; depth = 0): bool =
  ## True if a value of `typ` may hold a pointer, so copying it can create
  ## aliasing. `m == nil` is a caller explicitly opting out of type information
  ## (`computeAliasing`'s coarse mode); every named type then goes to `resolve`.
  if depth > MaxDepth:
    quit "pointerbearing: type nesting too deep"
  if cursorIsNil(typ):
    quit "pointerbearing: empty type slot"
  if typ.kind == Symbol:
    # Nominal. Resolved here rather than under `NoType` below, because
    # `typeKind` decodes a tag and a symbol carries none.
    var nb = typ
    if m != nil: nb = navigateToObjectBody(m[], typ)
    return (if nb.kind == Symbol: resolveSym(m, nb.symId, resolve, resolveFld)
            else: pointerBearing(m, nb, resolve, resolveFld, depth+1))
  if typ.kind != TagLit:
    quit "pointerbearing: not a type: " & $typ.kind
  case typ.typeKind
  of IT, UT, FT, CT, BoolT, EnumT, VoidT:
    result = false
  of PtrT, AptrT, FlexarrayT:
    result = true
  of ProctypeT, ParamsT:
    # A proc value is a code pointer. `getType` of a proc symbol hands back its
    # `(params …)`, which is that same answer wearing the signature's clothes.
    result = true
  of ArrayT, VarargsT:
    # Leng's `varargs` keeps only the element type, so for this question both
    # are "an element type, repeated".
    result = pointerBearing(m, firstChild(typ), resolve, resolveFld, depth+1)
  of ObjectT, UnionT:
    # `sub`, not `into:` — the walk stops at the first pointer it finds, and
    # `into:` asserts that its body consumed every child, so returning early
    # from inside one trips that check on any object whose pointer field is not
    # the last. (Latent until `getType` started resolving `deref` through a
    # named pointer type, after which object bodies actually reach here.)
    result = false
    var body = sub(typ)
    if typ.typeKind == ObjectT and body.kind == Symbol:     # base type
      if pointerBearing(m, body, resolve, resolveFld, depth+1): return true
      inc body
    while body.hasMore:
      if body.substructureKind == FldU:
        let fld = takeFieldDecl(body)
        if pointerBearing(m, fld.typ, resolve, resolveFld, depth+1): return true
      else:
        skip body
  of NoType:
    # A tag that is not a Leng type tag — `(err)`, or an expression that landed
    # in a type slot. Callers settle the expression *before* asking about its
    # type (see `sourceMayBePointer`), so arriving here means one did not.
    quit "pointerbearing: not a type tag: (" & tagName(m, typ) & ")"

proc baseMayBePointer(m: ptr MainModule; n: Cursor;
                      resolve: ForeignSymResolver;
                      resolveFld: ForeignFieldResolver): bool

proc sourceMayBePointer*(m: ptr MainModule; n: Cursor;
                         resolve: ForeignSymResolver = nil;
                     resolveFld: ForeignFieldResolver = nil): bool =
  ## True unless `n` is provably a pointer-free value. `(cast[uint](p))` and
  ## `(add (cast[uint](p)) n)` still carry `p`'s identity, even though their
  ## static type is an integer — pointer arithmetic is not forging.
  ##
  ## Where the *type* is the answer, `getType` may still come up empty: not
  ## because a declaration is missing, but because the value it names is
  ## declared in another module. Those go to `resolve` by symbol, exactly as a
  ## nominal type does.
  var n = n
  while n.kind == TagLit and n.exprKind == ParC:
    n = firstChild(n)
  case n.kind
  of Symbol, SymbolDef:
    if m == nil: return true           # coarse mode: a symbol might be a pointer
    let t = getType(m[], n)
    result = if isUnknownType(t): resolveSym(m, n.symId, resolve, resolveFld)
             else: pointerBearing(m, t, resolve, resolveFld)
  of IntLit, UIntLit, FloatLit, CharLit, StrLit:
    result = false
  of TagLit:
    case n.exprKind
    of AddrC, HaddrC, DerefC, PatC, NilC, CallC:
      result = true
    of TrueC, FalseC, InfC, NeginfC, NanC, SizeofC, AlignofC, OffsetofC:
      result = false
    of ConvC, CastC:
      var inner = n
      inc inner
      skip inner                       # type; bits may still be a pointer
      result = sourceMayBePointer(m, inner, resolve, resolveFld)
    of DotC:
      if m == nil: return sourceMayBePointer(m, firstChild(n), resolve, resolveFld)
      let t = getType(m[], n)
      if not isUnknownType(t): return pointerBearing(m, t, resolve, resolveFld)
      # The object's type is declared in another module, so the navigator could
      # not reach the field. Its *type* usually still resolves nominally, as a
      # symbol, and the field is found on that.
      let obj = firstChild(n)
      var fld = obj
      skip fld                         # past the object
      let ot = getType(m[], obj)
      if ot.kind == Symbol:
        result = resolveField(m, ot.symId, fld, resolve, resolveFld)
      elif not isUnknownType(ot):
        result = pointerBearing(m, ot, resolve, resolveFld)
      else:
        result = baseMayBePointer(m, n, resolve, resolveFld)
    of AtC:
      if m == nil: return sourceMayBePointer(m, firstChild(n), resolve, resolveFld)
      let t = getType(m[], n)
      result = if isUnknownType(t): baseMayBePointer(m, n, resolve, resolveFld)
               else: pointerBearing(m, t, resolve, resolveFld)
    of AddC, SubC, BitandC, BitorC, BitxorC:
      result = false
      var r = n
      var idx = 0
      r.loopInto:
        # idx 0 is the result type; only operands can carry pointer identity.
        if idx > 0 and not result and sourceMayBePointer(m, r, resolve, resolveFld):
          result = true
        inc idx
        skip r
    else:
      if m == nil: return true
      let t = getType(m[], n)
      result = if isUnknownType(t): baseMayBePointer(m, n, resolve, resolveFld)
               else: pointerBearing(m, t, resolve, resolveFld)
  else:
    result = false

proc baseMayBePointer(m: ptr MainModule; n: Cursor;
                      resolve: ForeignSymResolver;
                      resolveFld: ForeignFieldResolver): bool =
  ## The answer for an access path whose own type the navigator could not
  ## recompute, taken from the value it selects within.
  ##
  ## This is a *bound*, not a guess: `a[i]` is part of `a`, so if `a` holds no
  ## pointer then neither does any element of it. It is needed because a Leng
  ## path can step through a field or an index that lowering introduced, which
  ## the declaring module's type does not name at that offset — but the value
  ## being selected within is still fully described.
  ##
  ## One step, then straight back into `sourceMayBePointer`: the base is where
  ## the type information actually is. `(at (dot (deref p) a) i)` on an imported
  ## object resolves as "the field `a` of `p`'s pointee type", which the field
  ## resolver answers exactly — while peeling all the way to `p` in one go would
  ## instead ask about a *local*, which no declaration describes because a local
  ## does not have one to load.
  var found = false
  let base = selectionBase(n, found)
  if not found:
    quit "pointerbearing: cannot type (" & tagName(m, n) & ") and it selects " &
         "within nothing"
  result = sourceMayBePointer(m, base, resolve, resolveFld)

proc transfersPointer*(m: ptr MainModule; n: Cursor;
                       resolve: ForeignSymResolver = nil;
                     resolveFld: ForeignFieldResolver = nil): bool =
  ## True unless copying `n` provably moves no pointer. The type is the primary
  ## answer; when the navigator cannot recompute one, the expression's own shape
  ## still decides many cases (a literal, a `sizeof`, an arithmetic result over
  ## pointer-free operands), so fall through to the structural test instead of
  ## giving up. Requires a type context: callers short-circuit on `m == nil`.
  let t = getType(m[], n)
  if isUnknownType(t): sourceMayBePointer(m, n, resolve, resolveFld)
  else: pointerBearing(m, t, resolve, resolveFld)

proc isForgingCast*(m: ptr MainModule; c: Cursor;
                    resolve: ForeignSymResolver = nil;
                     resolveFld: ForeignFieldResolver = nil): bool =
  ## `cast[ptr T](non-pointer)`: the pointee is outside every tracked graph.
  if c.kind != TagLit or c.exprKind != CastC: return false
  var t = firstChild(c)
  var inner = t
  skip inner
  result = pointerBearing(m, t, resolve, resolveFld) and
           not sourceMayBePointer(m, inner, resolve, resolveFld)

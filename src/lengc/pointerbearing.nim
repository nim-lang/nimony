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
## `unresolved` covers the one case `MainModule` cannot: a type whose defining
## `.c.nif` is not readable at the time the caller runs. Passing `nil` there
## answers "may hold a pointer", which is the conservative direction —
## over-merging costs optimization, under-merging is a missed invalidation.

import ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / "lib" / nifcdecl        # exprKind/typeKind, takeFieldDecl
import nifmodules                     # MainModule
import typenav                        # getType / navigateToObjectBody

type
  UnresolvedTypeHook* = proc (s: SymId): bool {.nimcall.}
    ## Last resort for a type symbol `MainModule` cannot load: answers whether
    ## a value of that type may hold a pointer. `nil` ⇒ assume it may.

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc isUnknownType*(t: Cursor): bool {.inline.} =
  ## `typenav.getType`'s `(err)` sentinel: a tag that is not a type tag at all.
  ## It means "could not recompute", not "no pointer" — but it is also not a
  ## reason to give up, since the *expression* may still be provably pointer
  ## free (see `transfersPointer`).
  t.kind == TagLit and t.typeKind == NoType

proc pointerBearing*(m: ptr MainModule; typ: Cursor;
                     unresolved: UnresolvedTypeHook = nil; depth = 0): bool =
  ## True if a value of `typ` may hold a pointer, so copying it can create
  ## aliasing. Pointer-free scalars return false; an unresolved/unknown type
  ## returns true (conservative — never miss an alias). `m == nil` skips
  ## named-type resolution (Symbol → `unresolved`, else true).
  if depth > 12 or cursorIsNil(typ): return true
  if typ.kind notin {TagLit, Symbol}:
    # Not a type at all: an omitted (`.`) type slot, or a `getType` that ran out
    # of information and handed back the expression. `typeKind` decodes only a
    # tag, so falling through would read a verdict out of a token that never
    # encoded one — and a *pointer-free* verdict is the unsound direction.
    return true
  case typ.typeKind
  of IT, UT, FT, CT, BoolT, EnumT, VoidT:
    result = false
  of PtrT, AptrT, ProctypeT, FlexarrayT:
    result = true
  of ArrayT:
    result = pointerBearing(m, firstChild(typ), unresolved, depth+1)  # element
  of ObjectT, UnionT:
    result = false
    var body = typ
    body.into:
      if typ.typeKind == ObjectT and body.kind == Symbol:   # base type
        if pointerBearing(m, body, unresolved, depth+1): return true
        inc body
      while body.hasMore:
        if body.substructureKind == FldU:
          let fld = takeFieldDecl(body)
          if pointerBearing(m, fld.typ, unresolved, depth+1): return true
        else:
          skip body
  of NoType:
    if typ.kind == Symbol:
      var nb = typ
      if m != nil: nb = navigateToObjectBody(m[], typ)
      if nb.kind == Symbol:
        # Still nominal: the declaration is not in this module and could not be
        # loaded from the one that owns it.
        result = if unresolved != nil: unresolved(nb.symId) else: true
      else:
        result = pointerBearing(m, nb, unresolved, depth+1)
    else:
      result = true
  else:
    result = true   # err / params / anything unexpected -> conservative

proc sourceMayBePointer*(m: ptr MainModule; n: Cursor;
                         unresolved: UnresolvedTypeHook = nil): bool =
  ## True unless `n` is provably a pointer-free value. `(cast[uint](p))` and
  ## `(add (cast[uint](p)) n)` still carry `p`'s identity, even though their
  ## static type is an integer — pointer arithmetic is not forging.
  var n = n
  while n.kind == TagLit and n.exprKind == ParC:
    n = firstChild(n)
  case n.kind
  of Symbol, SymbolDef:
    if m != nil: result = pointerBearing(m, getType(m[], n), unresolved)
    else: result = true                 # no types: a symbol might be a pointer
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
      skip inner                        # type; bits may still be a pointer
      result = sourceMayBePointer(m, inner, unresolved)
    of DotC, AtC:
      if m != nil: result = pointerBearing(m, getType(m[], n), unresolved)
      else: result = sourceMayBePointer(m, firstChild(n), unresolved)
    of AddC, SubC, BitandC, BitorC, BitxorC:
      result = false
      var r = n
      var idx = 0
      r.loopInto:
        # idx 0 is the result type; only operands can carry pointer identity.
        if idx > 0 and not result and sourceMayBePointer(m, r, unresolved):
          result = true
        inc idx
        skip r
    else:
      if m != nil: result = pointerBearing(m, getType(m[], n), unresolved)
      else: result = true
  else:
    result = false

proc transfersPointer*(m: ptr MainModule; n: Cursor;
                       unresolved: UnresolvedTypeHook = nil): bool =
  ## True unless copying `n` provably moves no pointer. The type is the primary
  ## answer; when the navigator cannot recompute one (`(err)`) the expression's
  ## own shape still decides many cases (a literal, a `sizeof`, an arithmetic
  ## result over pointer-free operands), so fall through to the structural test
  ## instead of giving up. Requires a type context: callers short-circuit on
  ## `m == nil`.
  let t = getType(m[], n)
  if isUnknownType(t): sourceMayBePointer(m, n, unresolved)
  else: pointerBearing(m, t, unresolved)

proc isForgingCast*(m: ptr MainModule; c: Cursor;
                    unresolved: UnresolvedTypeHook = nil): bool =
  ## `cast[ptr T](non-pointer)`: the pointee is outside every tracked graph.
  if c.kind != TagLit or c.exprKind != CastC: return false
  var t = firstChild(c)
  var inner = t
  skip inner
  result = pointerBearing(m, t, unresolved) and
           not sourceMayBePointer(m, inner, unresolved)

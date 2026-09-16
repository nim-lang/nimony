#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The single description of what a lowered closure *value* looks like.
##
## Nimsem's type for a closure is `(proctype … (pragmas closure))`: a proc that
## also carries an environment. Hexer's `lambdalifting` turns the values of that
## type into the `(closureTuple <fn proctype> (ref RootObj))` pair the back end
## lays out, and rewrites the proctypes it walks along with them — but it only
## walks THIS module. The type of a foreign decl (a field of an imported
## object, an imported global, an imported proc's result) is answered by
## `typenav` straight from its semchecked declaration and keeps the nimsem
## spelling. Hexer therefore meets both spellings of one type and has to agree
## with itself about them: `isClosureProcType` recognizes the un-rewritten one
## and `loweredClosureType` produces the lowered one.
##
## `typenav` is nimsem's type system and stays that way: it answers what a
## declaration says, not what hexer lowered it to. The lowering — a
## lengc-facing shape — is hexer's, so it lives here, below `lifter`, which is
## the lowest pass that needs it and cannot import the passes above it.
##
## `lambdalifting`'s `toProcType` is the same lowering for the values it does
## rewrite; it differs only in recursing through `tre` (its capture-rewriting
## walk) instead of plain copying, and the two must keep producing identical
## trees, because the structural key of the type (hook names, C type names) is
## what makes a foreign decl's closure field and its defining module's agree.

import std / assertions

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, typenav, builtintypes]
include ".." / nimony / nif_annotations

export BareRootObjName  # the env slot's type; declared with `RootObj` itself

const
  ClosureEnvParamName* = "`ep.0"
    ## The env param appended to a lowered closure signature. Distinct from
    ## the coroutine's own env param (`coro_transform.EnvParamName`).

proc addRootRef*(dest: var TokenBuf; info: NifLineInfo)
  {.ensuresNif: addedType(dest).} =
  ## The `(ref RootObj)` env slot of a lowered closure tuple.
  dest.copyIntoKind RefT, info:
    dest.addSymUse pool.symId(BareRootObjName), info

proc addClosureEnvParam*(dest: var TokenBuf; info: NifLineInfo; envTyp: SymId) =
  ## Emit the trailing env `(param)` of a lowered closure signature. `envTyp == 0`
  ## uses the generic `(ref RootObj)` slot shared with iter values; a concrete env
  ## type uses a `(ptr)` (NIFC needs the pointer type here, with a cast in the body).
  dest.copyIntoKind ParamU, info:
    dest.addSymDef pool.symId(ClosureEnvParamName), info
    dest.addDotToken() # no export marker
    dest.addDotToken() # no pragmas
    if envTyp == SymId(0):
      addRootRef dest, info
    else:
      # to keep NIFC's type system happy we need a ptr type here
      # and then a cast in the body!
      dest.copyIntoKind PointerT, info: discard
    dest.addDotToken() # no default value

# ---------------------------------------------------------------------
# Predicates
# ---------------------------------------------------------------------

proc isLiftedClosureTuple*(n: Cursor): bool {.inline.} =
  ## `(closureTuple <proctype …> (ref RootObj))` is the shape both closure
  ## procs and closure-iter values get lifted to. If we encounter one
  ## while walking, it's already lifted — recursing into it would
  ## re-trigger the proctype rewrite and produce nested tuples.
  ##
  ## The tag alone answers this, because every producer — `coro_transform`'s
  ## `emitIterTupleType*`, lambdalifting's `toProcType` / closure-sym path, cps's
  ## `trProctype` and `addClosureTuple` below — emits `ClosureTupleT`. This used
  ## to probe a plain `(tuple …)` for "exactly a proctype then a ref", which
  ## answered a question about *layout* where the callers all ask about
  ## *provenance*: an ordinary `(proc (), ref RootObj)` tuple written by the
  ## user answered yes, and any drift in the lifted element shape would
  ## silently answer no.
  n.typeKind == ClosureTupleT

proc isClosureProcType*(typ: Cursor): bool {.inline.} =
  ## A `.closure` proctype that lambdalifting did NOT rewrite: the type of a
  ## FOREIGN decl, answered from its semchecked declaration. Its runtime
  ## representation is the `(closureTuple …)` pair all the same (`sizeof`
  ## sizes it as two pointers), so every pass below lambdalifting has to
  ## treat it as that pair: hook it like one (`lifter`) and lay it out like
  ## one (`lengcgen`).
  typ.typeKind == ProctypeT and procHasPragma(typ, ClosureP)

proc isClosureValueType*(typ: Cursor): bool =
  ## The (fn, env) pair in any of its spellings: the lowered `ClosureTupleT`, a
  ## `.closure` proctype whose decl has not been rewritten yet, or a `.closure`
  ## iterator. A `.passive` iterator is NOT one — it lowers to a bare wrapper
  ## proctype.
  isLiftedClosureTuple(typ) or isClosureProcType(typ) or
    (typ.typeKind == ItertypeT and not procHasPragma(typ, PassiveP))

proc containsClosureProcType*(typ: Cursor): bool =
  ## Is there an un-rewritten `.closure` proctype anywhere in `typ`'s
  ## STRUCTURE — the type itself, a tuple/array element, a param? Symbols
  ## are not followed: a nominal type's layout is its own decl's business.
  ## An already lowered `(closureTuple …)` counts as done.
  var n = typ
  if n.isTagLit:
    if isClosureProcType(n): return true
    if isLiftedClosureTuple(n): return false
    n = sub(n) # throwaway copy; bounds the walk under vpr
    while n.hasMore:
      if containsClosureProcType(n): return true
      skip n
  result = false

# ---------------------------------------------------------------------
# The lowering
# ---------------------------------------------------------------------

proc lowerClosureProcTypes(dest: var TokenBuf; n: var Cursor)

proc addClosureTuple(dest: var TokenBuf; n: var Cursor) =
  ## Consume the `.closure` proctype at `n`; emit the `(closureTuple
  ## (proctype . (params <params> <env>) <ret> <pragmas>) (ref RootObj))`
  ## lambdalifting's `toProcType` would have made of it. Params and return
  ## type are lowered recursively, as `toProcType` does through `tre`: a
  ## closure whose own parameter is a closure takes that parameter as a
  ## tuple too.
  let info = n.info
  dest.copyIntoKind ClosureTupleT, info:
    dest.copyIntoKind ProctypeT, info:
      dest.addDotToken() # nilability tag
      n.into:
        skip n # nilability tag
        dest.copyIntoKind ParamsU, info:
          if n.kind == DotToken:
            inc n
          else:
            n.into:
              while n.hasMore:
                lowerClosureProcTypes(dest, n)
          addClosureEnvParam dest, info, SymId(0)
        lowerClosureProcTypes(dest, n) # return type
        dest.takeTree n # pragmas
    addRootRef dest, info

proc lowerClosureProcTypes(dest: var TokenBuf; n: var Cursor) =
  ## Copy the tree at `n`, replacing every un-rewritten `.closure` proctype
  ## in it by its tuple. An already lowered `(closureTuple …)` is copied as
  ## is: its fn slot keeps the `closure` pragma and must stay a bare proctype.
  if n.isTagLit:
    if isClosureProcType(n):
      addClosureTuple(dest, n)
    elif isLiftedClosureTuple(n):
      dest.takeTree n
    else:
      takeInto dest, n:
        while n.hasMore:
          lowerClosureProcTypes(dest, n)
  else:
    dest.takeTree n # an atom, with its line info

proc loweredClosureType*(typ: Cursor): TokenBuf =
  ## A copy of `typ` with every un-rewritten `.closure` proctype in it
  ## replaced by the (fn, env) tuple it stands for — the same shape
  ## lambdalifting gives the values of the type it does lower, so the
  ## structural keys (hook names, C type names) agree with them.
  result = createTokenBuf(32)
  var n = typ
  lowerClosureProcTypes(result, n)

proc closureValueType*(c: var TypeCache; typ: Cursor): Cursor =
  ## `typ`, unless it holds an un-rewritten `.closure` proctype somewhere:
  ## then the lowered copy, so that a local declared with the result gets
  ## the tuple's layout in C rather than a bare function pointer — also
  ## inside a tuple of closures, or an array of them. The copy is owned by
  ## the type cache, which outlives every cursor handed out of it.
  if containsClosureProcType(typ):
    result = c.keepAlive(loweredClosureType(typ))
  else:
    result = typ

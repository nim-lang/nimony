#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nim 2 compatibility shims.
##
## Local helpers — *not* a post-sem pass. The two responsibilities below
## hook into the sites in sem that naturally emit the affected tokens, so
## the rest of the buffer is never re-walked.
##
## Responsibilities:
##
## - **Param-type annotation.** `compatAnnotateVarargsParam` runs inside
##   `semLocal` after a param's type has been emitted. If the type is
##   `(varargs T conv?)` it rewrites it in place to
##   `(varargs T conv? "openArray.0.I<key>.<mod>")` — the trailing string
##   literal names the openArray instance Sym for hexer to resolve at
##   codegen time. sem instantiates the openArray[T] instance as a side
##   effect, so the hint always resolves to a real decl in `.s.nif`.
##
## - **Call-site bundling.** `compatBundleVarargsInMatch` runs inside
##   `resolveOverloads` after the best candidate is picked. For non-
##   template calls with a typed-varargs slot it rewrites the tail of
##   `m.args` (from `m.firstVarargPosition` onward) into a single
##   `(hcall toOpenArray.0[I,T] (aconstr (array T (rangetype int 0 N-1)) e₁ … eₙ))`
##   bundle. After the rewrite `addArgsInstConverters` emits the bundle
##   normally, so derefs/hexer/NIFC see the same shape sem produces for
##   `f([1, 2, 3])` against an `openArray[int]` formal. Empty case yields
##   an empty range `0..-1`. Templates skip bundling — their `unpack` /
##   `firstVarargMatch` substitution needs the flat args in `dest`; any
##   varargs calls in the *expanded* body get re-resolved through
##   `resolveOverloads` and bundled there.
##
## Bare `(varargs)` — the `{.varargs.}` pragma form on C importc procs —
## is left untouched; NIFC's `...` ellipsis handles it.
##
## Included from sem.nim because the helpers need access to
## `semLocalType` and the existing instantiation machinery
## (`c.procRequests`, `c.typeInstDecls`).

# included from sem.nim

const OpenArrayHeadName = "openArray.0." & SystemModuleSuffix

proc compatOpenArrayInstance(c: var SemContext; elemType: Cursor;
                             info: PackedLineInfo): TypeCursor =
  ## Build `(invoke openArray elemType)` and instantiate it. Returns the
  ## resolved Symbol cursor for `openArray[T]`. Idempotent for repeated
  ## calls with the same `elemType` — `semLocalType` consults the type
  ## instantiation cache via the canonical-form key, so duplicate calls
  ## hand back the same instance Symbol.
  var invokeBuf = createTokenBuf(8)
  invokeBuf.addParLe(InvokeT, info)
  invokeBuf.add symToken(pool.syms.getOrIncl(OpenArrayHeadName), info)
  invokeBuf.addSubtree elemType
  invokeBuf.addParRi()
  var scratch = createTokenBuf(16)
  var src = cursorAt(invokeBuf, 0)
  result = semLocalType(c, scratch, src)

proc compatVarargsElem(paramType: Cursor): Cursor =
  ## If `paramType` is a typed `(varargs T …)`, return a cursor at T.
  ## Returns a nil cursor for the bare `(varargs)` pragma form so callers
  ## can leave that case alone.
  if paramType.typeKind != VarargsT: return default(Cursor)
  var elem = paramType
  inc elem
  if elem.kind == ParRi:
    return default(Cursor)
  result = elem

proc compatVarargsParamElem*(fn: FnCandidate): Cursor =
  ## Walk `fn`'s param list; if any param is a typed `(varargs T …)`,
  ## return a cursor at T. Nil cursor when the routine has no
  ## typed-varargs slot (bare `{.varargs.}` C importc procs included).
  result = default(Cursor)
  var f = fn.typ
  if f.typeKind in RoutineTypes:
    skipToParams f
  if f.substructureKind != ParamsU: return
  loopInto f:
    if f.symKind == ParamY:
      let p = asLocal(f)
      let elem = compatVarargsElem(p.typ)
      if not cursorIsNil(elem):
        return elem
    skip f

proc compatVarargsHasHint(paramType: Cursor): bool =
  ## True if `(varargs T conv? "hint")` already carries the openArray
  ## mangle hint that `compatAnnotateVarargsParam` injects. Keeps the
  ## annotation idempotent across template re-sem / generic re-instantiation.
  if paramType.typeKind != VarargsT: return false
  var c = paramType
  result = false
  loopInto c:
    if c.kind == StringLit: return true
    skip c

proc compatToOpenArrayTypevars(): (SymId, SymId) =
  ## Return the SymIds of `toOpenArray.0[I, T]`'s typevars in declaration
  ## order. Mirrors `openarrays.nim`'s `converter toOpenArray*[I, T]…`.
  result = (SymId(0), SymId(0))
  let origin = pool.syms.getOrIncl("toOpenArray.0." & SystemModuleSuffix)
  let res = tryLoadSym(origin)
  if res.status != LacksNothing: return
  let routine = asRoutine(res.decl)
  var tv = routine.typevars
  if tv.substructureKind != TypevarsU: return
  inc tv
  if tv.kind == ParLe and tv.symKind == TypevarY:
    var inner = tv
    inc inner
    result[0] = inner.symId
    skip tv
  if tv.kind == ParLe and tv.symKind == TypevarY:
    var inner = tv
    inc inner
    result[1] = inner.symId

proc compatAnnotateVarargsParam*(c: var SemContext; dest: var TokenBuf;
                                 typeStart: int) =
  ## After `semLocalType` emits a param's type at `dest[typeStart…]`,
  ## inspect it; if it's `(varargs T conv?)` without the hint, splice in
  ## `(varargs T conv? "openArray.0.I<key>.<mod>")`. No-op otherwise (the
  ## type isn't varargs, the bare `{.varargs.}` form has no element type
  ## to instantiate, or the hint is already present).
  if typeStart >= dest.len: return
  let typeCursor = cursorAt(dest, typeStart)
  if typeCursor.typeKind != VarargsT:
    endRead(dest)
    return
  if compatVarargsHasHint(typeCursor):
    endRead(dest)
    return
  let info = typeCursor.info
  var elem = typeCursor
  inc elem
  if elem.kind == ParRi:
    # bare `(varargs)` — nothing to instantiate
    endRead(dest)
    return

  # Capture children of the original `(varargs …)` into a fresh buffer so
  # the in-place `replace` can read its source without aliasing `dest`.
  # `loopInto` is required: under `-d:virtualParRi` the closing `)` is
  # elided when sealed, so a bare `while c.hasMore` walks past the
  # varargs subtree into adjacent param-decl tokens.
  var rebuilt = createTokenBuf(16)
  rebuilt.addParLe(VarargsT, info)
  var inner = typeCursor
  loopInto inner:
    takeTree rebuilt, inner
  endRead(dest)

  var elemCursor = cursorAt(rebuilt, 0)
  inc elemCursor    # past `(varargs` to the element type
  let inst = compatOpenArrayInstance(c, elemCursor, info)
  endRead(rebuilt)
  let hintStr =
    if inst.kind == Symbol: pool.syms[inst.symId]
    else: ""
  rebuilt.addStrLit hintStr, info
  rebuilt.addParRi()

  dest.replace beginRead(rebuilt), typeStart

proc compatVarargsSlotIsBundled(m: Match; start: int): bool =
  ## True if the tail at `start` starts with an
  ## `(hcall <toOpenArray.0…instance> …)` subtree — sigmatch accepted a
  ## pre-bundled `openArray[T]` arg verbatim. Bundling again would
  ## double-wrap.
  if start >= m.args.len: return false
  if m.args[start].kind != ParLe: return false
  if m.args[start].exprKind != HcallX: return false
  if start + 1 >= m.args.len or m.args[start + 1].kind != Symbol: return false
  result = pool.syms[m.args[start + 1].symId].startsWith("toOpenArray.")

proc compatBundleVarargsInMatch*(c: var SemContext; m: var Match;
                                 elemType: Cursor; info: PackedLineInfo) =
  ## Replace the flat varargs args at the tail of `m.args` (from
  ## `m.firstVarargPosition`) with the openArray bundle. Called from
  ## `resolveOverloads` after the best candidate is picked, so
  ## `addArgsInstConverters` writes the bundled form into `dest`. No-ops
  ## when no varargs slot was matched, or the slot already holds a
  ## previous `toOpenArray.0` bundle (template body re-sem).
  if m.firstVarargPosition < 0: return
  let start = m.firstVarargPosition
  if compatVarargsSlotIsBundled(m, start): return

  # Move the flat args out of `m.args` into a `(stmts …)` scratch wrapper
  # so an iterator over them terminates at the closing `)` rather than
  # walking off the end. (Without the wrapper, `hasMore` — which only
  # checks `kind != ParRi` outside `virtualParRi` — would read past
  # buffer end into undefined memory.)
  var flat = createTokenBuf(max(8, m.args.len - start + 2))
  flat.addParLe(StmtsS, info)
  for i in start ..< m.args.len:
    flat.add m.args[i]
  flat.addParRi()
  m.args.shrink start

  var argCursor = beginRead(flat)
  var argTrees: seq[TokenBuf] = @[]
  loopInto argCursor:
    var ab = createTokenBuf(8)
    takeTree ab, argCursor
    argTrees.add ab
  let n = argTrees.len

  var rangeBuf = createTokenBuf(8)
  rangeBuf.addParLe(RangetypeT, info)
  rangeBuf.addSubtree c.types.intType
  rangeBuf.addIntLit(0, info)
  rangeBuf.addIntLit(n - 1, info)
  rangeBuf.addParRi()
  let rangeType = typeToCursor(c, rangeBuf, 0)

  var elemBuf = createTokenBuf(8)
  elemBuf.addSubtree elemType
  let elemTypeC = typeToCursor(c, elemBuf, 0)

  let (iSym, tSym) = compatToOpenArrayTypevars()
  let origin = pool.syms.getOrIncl("toOpenArray.0." & SystemModuleSuffix)
  var inferred = initTable[SymId, Cursor]()
  inferred[iSym] = rangeType
  inferred[tSym] = elemTypeC
  var typeArgs = createTokenBuf(8)
  typeArgs.addSubtree rangeType
  typeArgs.addSubtree elemTypeC
  let inst = c.requestRoutineInstance(origin, typeArgs, inferred, info)

  m.args.addParLe(HcallX, info)
  m.args.add symToken(inst.targetSym, info)
  m.args.addParLe(AconstrX, info)
  m.args.addParLe(ArrayT, info)
  m.args.addSubtree elemTypeC
  m.args.addSubtree rangeType
  m.args.addParRi()
  for ab in argTrees:
    m.args.add ab
  m.args.addParRi()
  m.args.addParRi()

#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## "Can a value of this type hold a pointer?", answered over a symbol's
## **Nimony** declaration with `programs.tryLoadSym`'s on-demand loading.
##
## `funcsummary` asks this about *Leng* types, and for a type declared in the
## module being summarized the Leng body is right there in the buffer. For an
## imported one it is not: hexer runs per module, and a dependency's Leng output
## is not an input of that run — only its sem index (`.s.idx.nif`) is. Calling
## every imported type pointer-bearing would make `string` opaque, so no copy of
## one could be proved non-aliasing and code would optimize worse purely because
## it was split across modules. "Put it in one file and it gets faster" is not a
## cost model a programmer can work with, so the question is answered where the
## information *is* available on demand: the Nimony declaration, reached through
## the same loader every other hexer pass already uses.
##
## The traversal deliberately mirrors `nimony/sizeof.getSize` — same
## nominal-resolution loop, same object/case/tuple walk, same `(inheritable)`
## rule for the RTTI pointer — because the answer has to agree with what
## `lengcgen` lowers these types to, and `getSize` is the pass that already
## encodes that layout. It is one-sided on purpose: only kinds that provably
## contain no pointer answer `false`. A type that fails to load, an abstract or
## generic one, anything unrecognised — all answer `true`. Being wrong in that
## direction costs an optimization; being wrong in the other costs a missed
## invalidation, which is a miscompile.
##
## Lives apart from `funcsummary` because that module decodes **Leng** tags:
## `leng_model` and `nimony_model` both define `typeKind`, so the two cannot
## share a compilation unit. Only `SymId` and `bool` cross the boundary.

import std / [assertions, tables]
from std / os import fileExists
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser            # extractModule
import ".." / nimony / [nimony_model, decls, programs]

type
  PointerTypeCache = Table[SymId, bool]
    ## Memoizes the answer per nominal type. Worth having: without it the walk
    ## re-loads and re-walks `string`'s declaration for every parameter that
    ## mentions it, and a summary pass asks about every parameter of every proc
    ## in the module.

var gCache: PointerTypeCache = initTable[SymId, bool]()
  ## Process-wide, because the answer is one: a type symbol is fully qualified
  ## by module, and hexer compiles one module per process. Keeping it here also
  ## lets the entry point be a plain `{.nimcall.}` hook (see `pointerbearing`).

var gReadable: Table[string, bool] = initTable[string, bool]()

proc moduleIsReadable(s: SymId): bool =
  ## `tryLoadSym` opens the owning module's `.s.nif` unconditionally, and
  ## `nifreader.open` on a file that is not there is fatal. Every other hexer
  ## caller only asks about symbols the module it is compiling actually
  ## imports, so it never sees that edge; this pass asks about whatever type
  ## symbol a Leng slot happens to name, so it has to check first. Cached per
  ## module suffix — one `stat` per dependency, not per symbol.
  let m = extractModule(pool.syms[s])
  if m.len == 0: return false
  if gReadable.hasKey(m): return gReadable.getOrQuit(m)
  result = fileExists(suffixToNif(m))
  gReadable[m] = result

const MaxDepth = 20

proc isInheritableDecl(pragmas: Cursor): bool =
  ## An object with no explicit base but `(inheritable)` still gets the RTTI
  ## pointer in the backend's layout, so it is pointer-bearing even when every
  ## declared field is a scalar. `sizeof` accounts for exactly the same word.
  result = false
  var n = pragmas
  if n.substructureKind == PragmasU:
    n.into PragmasU:
      while n.hasMore:
        if n.pragmaKind == InheritableP: result = true
        skip n

proc typeMayHoldPointer(n: Cursor; cache: var PointerTypeCache;
                        inheritable: bool; depth: int): bool

proc symMayHoldPointer(s: SymId; cache: var PointerTypeCache; depth: int): bool =
  ## Resolve a nominal type — an alias, an instantiated generic, a `distinct`,
  ## an imported one — to its structural body and answer from that.
  if cache.hasKey(s): return cache.getOrQuit(s)
  if depth > MaxDepth: return true
  if not moduleIsReadable(s):
    cache[s] = true
    return true
  let sym = tryLoadSym(s)
  if sym.status != LacksNothing:
    # Not loadable: an `importc` type with no Nimony body, or a module this
    # hexer run is not allowed to read. Unknown, hence pointer-bearing.
    cache[s] = true
    return true
  let d = asTypeDecl(sym.decl)
  if d.kind != TypeY:
    cache[s] = true
    return true
  result = typeMayHoldPointer(d.body, cache, isInheritableDecl(d.pragmas),
                              depth+1)
  cache[s] = result

proc typeMayHoldPointer(n: Cursor; cache: var PointerTypeCache;
                        inheritable: bool; depth: int): bool =
  ## `inheritable` is the `(inheritable)` pragma of the declaration `n` is the
  ## body of — it only matters for a rootless object, and is false for every
  ## inline (undeclared) type.
  if depth > MaxDepth or cursorIsNil(n): return true
  if n.isSymbol: return symMayHoldPointer(n.symId, cache, depth+1)
  case n.typeKind
  of IntT, UIntT, FloatT, CharT, BoolT, VoidT, EnumT, HoleyEnumT, AnumT, SetT:
    # `set` lowers to a bitset — an array of bytes, never a pointer.
    result = false
  of RangetypeT, SinkT, DistinctT, ArrayT:
    result = typeMayHoldPointer(n.childCursor, cache, false, depth+1)
  of RefT, PtrT, MutT, OutT, LentT, NiltT, CstringT, PointerT, UarrayT,
     VarargsT:
    result = true
  of TupleT:
    result = false
    var m = n
    m.into:
      while m.hasMore:
        if typeMayHoldPointer(getTupleFieldType(m), cache, false, depth+1):
          result = true
        skip m
  of ObjectT:
    result = inheritable           # the RTTI pointer of a rootless inheritable
    var m = sub(n)                 # bounded copy; `nextField` needs the scope
    if not m.isDotToken:           # explicit base type
      if typeMayHoldPointer(m, cache, false, depth+1): result = true
    skip m
    # `keepCase = false` flattens case-object branches into one field stream: a
    # pointer in any branch makes the object pointer-bearing, and which branch
    # is active is not a question this analysis asks.
    var iter = initObjFieldIter()
    while nextField(iter, m, keepCase = false):
      let f = takeLocal(m, SkipFinalParRi)
      if typeMayHoldPointer(f.typ, cache, false, depth+1): result = true
  else:
    # RoutineTypes (a proc value is a pointer, a closure two), plus everything
    # abstract, generic or unrecognised: err.
    result = true

proc nimonyTypeMayHoldPointer*(s: SymId): bool {.nimcall.} =
  ## The `pointerbearing.UnresolvedTypeHook` `funcsummary` installs: called when
  ## a Leng type symbol resolves to no declaration the `MainModule` can reach —
  ## i.e. it belongs to another module, whose Leng output this hexer run has no
  ## business reading, but whose *Nimony* declaration is a declared input.
  symMayHoldPointer(s, gCache, 0)

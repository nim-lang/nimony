## Repro for the implications.nim seq UAF caught by valgrind during the
## stage-2 bootstrap of bin1/nimsem. The crash signature was:
##
##   Invalid write of size 4 at add_1_impkyci4d (seq[Implication].add)
##     by add_0_impkyci4d (Implications.add)
##     by traverseStore_0_conthvy43
##   Address 0x… is 0 bytes after a recently re-allocated block of size 8
##   alloc'd at popScope_0_impkyci4d → setLen → growUnsafe → resize → realloc
##
## i.e. after `popScope`'s setLen reallocates the items buffer, the next
## `add` writes one element past the end of the new buffer.

import std / [sets, syncio, assertions]
include "../../../src/lib/nifprelude"

type
  ImplKind = enum
    Always, IfTrue, IfFalse

  Implication = object
    kind: ImplKind
    cond: SymId
    sym: SymId

  Implications = object
    items: seq[Implication]
    startIdx: int

  ImplScopeIdx = int

proc createImplications(): Implications =
  Implications(items: @[], startIdx: 0)

proc pushScope(impls: var Implications): ImplScopeIdx =
  result = impls.startIdx
  impls.startIdx = impls.items.len

proc popScope(impls: var Implications; saved: ImplScopeIdx) =
  impls.items.setLen(impls.startIdx)
  impls.startIdx = saved

proc checkpoint(impls: Implications): int {.inline.} =
  impls.items.len

proc take(impls: var Implications; cp: int): seq[Implication] =
  result = @[]
  var i = cp
  while i < impls.items.len:
    result.add impls.items[i]
    inc i
  impls.items.setLen(cp)

proc add(impls: var Implications; imp: Implication) =
  for i in impls.startIdx ..< impls.items.len:
    if impls.items[i] == imp: return
  if imp.kind in {IfTrue, IfFalse}:
    let compKind = if imp.kind == IfTrue: IfFalse else: IfTrue
    for i in impls.startIdx ..< impls.items.len:
      let other = impls.items[i]
      if other.kind == compKind and other.cond == imp.cond and other.sym == imp.sym:
        impls.items[i] = Implication(kind: Always, cond: SymId(0), sym: imp.sym)
        return
  impls.items.add imp

proc always(sym: SymId): Implication {.inline.} =
  Implication(kind: Always, cond: SymId(0), sym: sym)

proc ifTrue(cond, sym: SymId): Implication {.inline.} =
  Implication(kind: IfTrue, cond: cond, sym: sym)

# Mirror traverseStmt + traverseIte + traverseProc shape: nested pushScope
# / many `add` / take at branch joins / popScope. Drive the items.cap
# through a range that includes the 1-element edge `add` is asking
# valgrind about.

proc innerWork(impls: var Implications; symBase: int; n: int) =
  for i in 0 ..< n:
    impls.add always(SymId(symBase + i))

proc iteWork(impls: var Implications; symBase: int) =
  let cp = impls.checkpoint()
  innerWork(impls, symBase, 4)
  let thenImpls = take(impls, cp)
  innerWork(impls, symBase + 100, 4)
  let elseImpls = take(impls, cp)
  for imp in thenImpls:
    impls.add imp
  for imp in elseImpls:
    impls.add imp

proc procBody(impls: var Implications; depth: int; symBase: int) =
  let scope = impls.pushScope()
  innerWork(impls, symBase, 8)
  for i in 0 ..< 4:
    iteWork(impls, symBase + 1000 + i*10)
  if depth > 0:
    procBody(impls, depth - 1, symBase + 10000)
  innerWork(impls, symBase + 50, 8)
  impls.popScope(scope)

proc main =
  var impls = createImplications()
  for round in 0 ..< 4:
    procBody(impls, 6, round * 100000)
    echo "round ", round, " ok (items=", impls.items.len, ")"

main()
echo "DONE"

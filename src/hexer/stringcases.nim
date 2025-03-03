#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from nifcgen.nim

## Transforms string `case` statements to decision trees.

proc decodeSolution(e: var EContext; s: seq[SearchNode]; i: int;
                    selector: Cursor) =
  let info = selector.info
  case s[i].kind
  of ForkedSearch:
    let f = forked(s, i)

    e.dest.copyIntoUnchecked "if", info:
      e.dest.copyIntoUnchecked "elif", info:
        e.dest.copyIntoUnchecked "call", info:
          e.dest.add symToken(pool.syms.getOrIncl("nimStrAtLe.c"), info)
          e.dest.addSubtree selector
          e.dest.add intToken(pool.integers.getOrIncl(f.best[1]), info)
          e.dest.add charToken(f.best[0], info)
        e.dest.copyIntoUnchecked "stmts", info:
          e.decodeSolution s, f.thenA, selector
      e.dest.copyIntoUnchecked "else", info:
        e.dest.copyIntoUnchecked "stmts", info:
          e.decodeSolution s, f.elseA, selector

  of LinearSearch:
    e.dest.copyIntoUnchecked "if", info:
      for x in s[i].choices:
        e.dest.copyIntoUnchecked "elif", info:
          e.dest.copyIntoUnchecked "call", info:
            e.dest.add symToken(pool.syms.getOrIncl("nimStrEq.c"), info)
            e.dest.addSubtree selector
            e.genStringLit(x[0], info)
          e.dest.copyIntoUnchecked "stmts", info:
            e.dest.copyIntoUnchecked "jmp", info:
              e.dest.add symToken(pool.syms.getOrIncl(x[1]), info)

proc transformStringCase*(e: var EContext; c: var Cursor) =
  # Prepare the list of (key, value) pairs:
  var pairs: seq[Key] = @[]
  var n = c
  inc n
  let selector = n
  assert selector.kind == Symbol # TODO: Produce a temporary for non-variable
  skip n # selector

  while n.kind != ParRi:
    if n.substructureKind == OfU:
      let labl = "`sc." & $getTmpId(e)
      inc n
      assert n.substructureKind == RangesU
      inc n
      while n.kind != ParRi:
        assert n.kind == StringLit
        pairs.add (pool.strings[n.litId], labl)
        inc n
      inc n # skip ParRi
      skip n # skip action for now
      skipParRi n
    else:
      skip n

  let solution = createSearchTree(pairs)
  decodeSolution(e, solution, 0, selector)
  var i = 0
  n = c
  inc n

  skip n # selector
  let afterwards = pool.syms.getOrIncl("`sc." & $getTmpId(e))

  while n.kind != ParRi:
    if n.substructureKind == OfU:
      let info = n.info
      e.dest.copyIntoUnchecked "lab", info:
        e.dest.add symdefToken(pool.syms.getOrIncl(pairs[i][1]), info)
      inc n
      skip n # skip string values
      traverseStmt e, n
      e.dest.copyIntoUnchecked "jmp", info:
        e.dest.add symToken(afterwards, info)
      skipParRi n
      inc i
    elif n.substructureKind == ElseU:
      inc n
      traverseStmt e, n
      skipParRi n
    else:
      error "invalid `case` statement", n
  skipParRi n
  e.dest.copyIntoUnchecked "lab", c.info:
    e.dest.add symdefToken(afterwards, c.info)
  c = n

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

proc decodeSolution(c: var EContext; s: seq[SearchNode]; i: int;
                    selector: SymId; info: PackedLineInfo) =
  case s[i].kind
  of ForkedSearch:
    let f = forked(s, i)

    c.dest.copyIntoUnchecked "if", info:
      c.dest.copyIntoUnchecked "elif", info:
        c.dest.copyIntoUnchecked "call", info:
          c.dest.add symToken(pool.syms.getOrIncl("nimStrAtLe.n"), info)
          c.dest.add symToken(selector, info)
          c.dest.add intToken(pool.integers.getOrIncl(f.best[1]), info)
          c.dest.add charToken(f.best[0], info)
        c.dest.copyIntoUnchecked "stmts", info:
          c.decodeSolution s, f.thenA, selector, info
      c.dest.copyIntoUnchecked "else", info:
        c.dest.copyIntoUnchecked "stmts", info:
          c.decodeSolution s, f.elseA, selector, info

  of LinearSearch:
    c.dest.copyIntoUnchecked "if", info:
      for x in s[i].choices:
        c.dest.copyIntoUnchecked "elif", info:
          c.dest.copyIntoUnchecked "call", info:
            c.dest.add symToken(pool.syms.getOrIncl("nimStrEq.n"), info)
            c.dest.add symToken(selector, info)
            c.genStringLit(x[0], info)
          c.dest.copyIntoUnchecked "stmts", info:
            c.dest.copyIntoUnchecked "jmp", info:
              c.dest.add symToken(pool.syms.getOrIncl(x[1]), info)

proc transformStringCase*(c: var EContext; n: var Cursor) =
  c.demand pool.syms.getOrIncl("==.17." & SystemModuleSuffix)
  c.demand pool.syms.getOrIncl("nimStrAtLe.0." & SystemModuleSuffix)

  # Prepare the list of (key, value) pairs:
  var pairs: seq[Key] = @[]
  var n = n
  inc n
  let selectorNode = n
  let sinfo = selectorNode.info
  let selector: SymId
  if selectorNode.kind == Symbol:
    selector = selectorNode.symId
  else:
    selector = pool.syms.getOrIncl(":tmp.n." & $c.getTmpId)
    c.dest.copyIntoUnchecked "var", sinfo:
      c.dest.add symdefToken(selector, sinfo)
      c.dest.addDotToken() # pragmas
      c.dest.add symToken(pool.syms.getOrIncl(StringName), sinfo)
      c.dest.addSubtree selectorNode
  skip n # selector

  while n.kind != ParRi:
    if n.substructureKind == OfU:
      let labl = "`sc." & $getTmpId(c)
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
  decodeSolution(c, solution, 0, selector, selectorNode.info)
  var i = 0
  n = n
  inc n

  skip n # selector
  let afterwards = pool.syms.getOrIncl("`sc." & $getTmpId(c))

  let elseLabel = pool.syms.getOrIncl("`sc." & $getTmpId(c))
  c.dest.copyIntoUnchecked "jmp", selectorNode.info:
    c.dest.add symToken(elseLabel, selectorNode.info)
  var hasElse = false
  while n.kind != ParRi:
    let info = n.info
    if n.substructureKind == OfU:
      c.dest.copyIntoUnchecked "lab", info:
        c.dest.add symdefToken(pool.syms.getOrIncl(pairs[i][1]), info)
      inc n
      skip n # skip string values
      traverseStmt c, n
      c.dest.copyIntoUnchecked "jmp", info:
        c.dest.add symToken(afterwards, info)
      skipParRi n
      inc i
    elif n.substructureKind == ElseU:
      c.dest.copyIntoUnchecked "lab", info:
        c.dest.add symdefToken(elseLabel, info)
      inc n
      traverseStmt c, n
      skipParRi n
      hasElse = true
    else:
      error "invalid `case` statement", n
  if not hasElse:
    c.dest.copyIntoUnchecked "lab", sinfo:
      c.dest.add symdefToken(elseLabel, sinfo)

  skipParRi n
  c.dest.copyIntoUnchecked "lab", n.info:
    c.dest.add symdefToken(afterwards, n.info)
  n = n

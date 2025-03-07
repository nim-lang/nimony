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

proc decodeSolution(ctx: var EContext; s: seq[SearchNode]; i: int;
                    selector: SymId; info: PackedLineInfo) =
  case s[i].kind
  of ForkedSearch:
    let f = forked(s, i)

    ctx.dest.copyIntoUnchecked "if", info:
      ctx.dest.copyIntoUnchecked "elif", info:
        ctx.dest.copyIntoUnchecked "call", info:
          ctx.dest.add symToken(pool.syms.getOrIncl("nimStrAtLe.n"), info)
          ctx.dest.add symToken(selector, info)
          ctx.dest.add intToken(pool.integers.getOrIncl(f.best[1]), info)
          ctx.dest.add charToken(f.best[0], info)
        ctx.dest.copyIntoUnchecked "stmts", info:
          ctx.decodeSolution s, f.thenA, selector, info
      ctx.dest.copyIntoUnchecked "else", info:
        ctx.dest.copyIntoUnchecked "stmts", info:
          ctx.decodeSolution s, f.elseA, selector, info

  of LinearSearch:
    ctx.dest.copyIntoUnchecked "if", info:
      for x in s[i].choices:
        ctx.dest.copyIntoUnchecked "elif", info:
          ctx.dest.copyIntoUnchecked "call", info:
            ctx.dest.add symToken(pool.syms.getOrIncl("nimStrEq.n"), info)
            ctx.dest.add symToken(selector, info)
            ctx.genStringLit(x[0], info)
          ctx.dest.copyIntoUnchecked "stmts", info:
            ctx.dest.copyIntoUnchecked "jmp", info:
              ctx.dest.add symToken(pool.syms.getOrIncl(x[1]), info)

proc transformStringCase*(ctx: var EContext; n: var Cursor) =
  ctx.demand pool.syms.getOrIncl("==.17." & SystemModuleSuffix)
  ctx.demand pool.syms.getOrIncl("nimStrAtLe.0." & SystemModuleSuffix)

  # Prepare the list of (key, value) pairs:
  var pairs: seq[Key] = @[]
  var nb = n
  inc nb
  let selectorNode = nb
  let sinfo = selectorNode.info
  let selector: SymId
  if selectorNode.kind == Symbol:
    selector = selectorNode.symId
  else:
    selector = pool.syms.getOrIncl(":tmp.c." & $ctx.getTmpId)
    ctx.dest.copyIntoUnchecked "var", sinfo:
      ctx.dest.add symdefToken(selector, sinfo)
      ctx.dest.addDotToken() # pragmas
      ctx.dest.add symToken(pool.syms.getOrIncl(StringName), sinfo)
      ctx.dest.addSubtree selectorNode
  skip nb # selector

  while nb.kind != ParRi:
    if nb.substructureKind == OfU:
      let labl = "`sc." & $getTmpId(ctx)
      inc nb
      assert nb.substructureKind == RangesU
      inc nb
      while nb.kind != ParRi:
        assert nb.kind == StringLit
        pairs.add (pool.strings[nb.litId], labl)
        inc nb
      inc nb # skip ParRi
      skip nb # skip action for now
      skipParRi nb
    else:
      skip nb

  let solution = createSearchTree(pairs)
  decodeSolution(ctx, solution, 0, selector, selectorNode.info)
  var i = 0
  nb = n
  inc nb

  skip nb # selector
  let afterwards = pool.syms.getOrIncl("`sc." & $getTmpId(ctx))

  let elseLabel = pool.syms.getOrIncl("`sc." & $getTmpId(ctx))
  ctx.dest.copyIntoUnchecked "jmp", selectorNode.info:
    ctx.dest.add symToken(elseLabel, selectorNode.info)
  var hasElse = false
  while nb.kind != ParRi:
    let info = nb.info
    if nb.substructureKind == OfU:
      ctx.dest.copyIntoUnchecked "lab", info:
        ctx.dest.add symdefToken(pool.syms.getOrIncl(pairs[i][1]), info)
      inc nb
      skip nb # skip string values
      traverseStmt ctx, nb
      ctx.dest.copyIntoUnchecked "jmp", info:
        ctx.dest.add symToken(afterwards, info)
      skipParRi nb
      inc i
    elif nb.substructureKind == ElseU:
      ctx.dest.copyIntoUnchecked "lab", info:
        ctx.dest.add symdefToken(elseLabel, info)
      inc nb
      traverseStmt ctx, nb
      skipParRi nb
      hasElse = true
    else:
      error "invalid `case` statement", nb
  if not hasElse:
    ctx.dest.copyIntoUnchecked "lab", sinfo:
      ctx.dest.add symdefToken(elseLabel, sinfo)

  skipParRi nb
  ctx.dest.copyIntoUnchecked "lab", n.info:
    ctx.dest.add symdefToken(afterwards, n.info)
  n = nb

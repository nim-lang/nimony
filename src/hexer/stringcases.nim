#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from nifcgen.nim

## Transforms string `case` statements to decision trees.

const
  EqStringsOp* = "equalStrings.0." & SystemModuleSuffix
  StrAtLeOp* = "nimStrAtLe.0." & SystemModuleSuffix
  BorrowCStringUnsafeOp* = "borrowCStringUnsafe.1." & SystemModuleSuffix

proc decodeSolution(c: var EContext; s: seq[SearchNode]; i: int;
                    selector: SymId; info: PackedLineInfo) =
  case s[i].kind
  of ForkedSearch:
    let f = forked(s, i)

    c.dest.copyIntoUnchecked "if", info:
      c.dest.copyIntoUnchecked "elif", info:
        c.dest.copyIntoUnchecked "call", info:
          c.dest.add symToken(pool.syms.getOrIncl(StrAtLeOp), info)
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
            c.dest.add symToken(pool.syms.getOrIncl(EqStringsOp), info)
            c.dest.add symToken(selector, info)
            c.genStringLit(x[0], info)
          c.dest.copyIntoUnchecked "stmts", info:
            c.dest.copyIntoUnchecked "jmp", info:
              c.dest.add symToken(pool.syms.getOrIncl(x[1]), info)

proc getSimpleStringLit(c: var EContext; n: var Cursor): StrId =
  if n.kind == StringLit:
    result = n.litId
    inc n
  elif n.kind == Symbol:
    var inlineValue = getInitValue(c.typeCache, n.symId)
    if not cursorIsNil(inlineValue):
      result = getSimpleStringLit(c, inlineValue)
      inc n
    else:
      bug "not a string literal"
  else:
    case n.exprKind:
    of SufX:
      inc n
      assert n.kind == StringLit
      result = n.litId
      skipToEnd n
    of HconvX, ConvX:
      inc n
      assert n.typeKind == CstringT
      skip n
      result = getSimpleStringLit(c, n)
      skipParRi n
    else:
      bug "not a string literal"

proc transformStringCase*(c: var EContext; n: var Cursor) =
  c.demand pool.syms.getOrIncl(EqStringsOp)
  c.demand pool.syms.getOrIncl(StrAtLeOp)

  # Prepare the list of (key, value) pairs:
  var pairs: seq[Key] = @[]
  var nb = n
  inc nb
  var selectorNode = nb
  let sinfo = selectorNode.info
  let selector: SymId

  let selectorType = getType(c.typeCache, selectorNode)
  if selectorType.typeKind == CstringT:
    # the other overload of `borrowCStringUnsafe`
    c.demand pool.syms.getOrIncl(BorrowCStringUnsafeOp)
    selector = pool.syms.getOrIncl("`tc." & $c.getTmpId)
    c.dest.copyIntoUnchecked "var", sinfo:
      c.dest.add symdefToken(selector, sinfo)
      c.dest.addDotToken() # pragmas
      c.dest.add symToken(pool.syms.getOrIncl(StringName), sinfo)
      c.dest.copyIntoUnchecked "call", sinfo:
        c.dest.add symToken(pool.syms.getOrIncl(BorrowCStringUnsafeOp), sinfo)
        trExpr(c, selectorNode)
  elif selectorNode.kind == Symbol:
    selector = selectorNode.symId
  else:
    selector = pool.syms.getOrIncl("`tc." & $c.getTmpId)
    c.dest.copyIntoUnchecked "var", sinfo:
      c.dest.add symdefToken(selector, sinfo)
      c.dest.addDotToken() # pragmas
      c.dest.add symToken(pool.syms.getOrIncl(StringName), sinfo)
      trExpr(c, selectorNode)
  skip nb # selector

  while nb.kind != ParRi:
    if nb.substructureKind == OfU:
      let labl = "`sc." & $getTmpId(c)
      inc nb
      assert nb.substructureKind == RangesU
      inc nb
      while nb.kind != ParRi:
        let litId = getSimpleStringLit(c, nb)
        pairs.add (pool.strings[litId], labl)
      inc nb # skip ParRi
      skip nb # skip action for now
      skipParRi nb
    else:
      skip nb

  let solution = createSearchTree(pairs)
  decodeSolution(c, solution, 0, selector, selectorNode.info)
  var i = 0
  nb = n
  inc nb

  skip nb # selector
  let afterwards = pool.syms.getOrIncl("`sc." & $getTmpId(c))

  let elseLabel = pool.syms.getOrIncl("`sc." & $getTmpId(c))
  c.dest.copyIntoUnchecked "jmp", selectorNode.info:
    c.dest.add symToken(elseLabel, selectorNode.info)
  var hasElse = false
  while nb.kind != ParRi:
    let info = nb.info
    if nb.substructureKind == OfU:
      c.dest.copyIntoUnchecked "lab", info:
        c.dest.add symdefToken(pool.syms.getOrIncl(pairs[i][1]), info)
      inc nb
      inc nb
      while nb.kind != ParRi:
        skip nb
        inc i
      inc nb # skip ParRi
      trStmt c, nb
      c.dest.copyIntoUnchecked "jmp", info:
        c.dest.add symToken(afterwards, info)
      skipParRi nb
    elif nb.substructureKind == ElseU:
      c.dest.copyIntoUnchecked "lab", info:
        c.dest.add symdefToken(elseLabel, info)
      inc nb
      trStmt c, nb
      skipParRi nb
      hasElse = true
    else:
      error "invalid `case` statement", nb
  if not hasElse:
    c.dest.copyIntoUnchecked "lab", sinfo:
      c.dest.add symdefToken(elseLabel, sinfo)

  skipParRi nb
  c.dest.copyIntoUnchecked "lab", n.info:
    c.dest.add symdefToken(afterwards, n.info)
  n = nb

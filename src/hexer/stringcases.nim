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

proc decodeSolution(c: var EContext; dest: var TokenBuf; s: seq[SearchNode]; i: int;
                    selector: SymId; info: PackedLineInfo) =
  ## Conditions are bound to fresh `(var :tcc.N . (bool) <call>)`
  ## locals before the `(if …)`. A bare `(call …)` inside an
  ## `(elif …)` is in expression position — dce2's inliner splice
  ## can't reach it, but the bound-form splice fires on the (var)
  ## wrapping. That keeps `.inline` callees like `nimStrAtLe` and
  ## `equalStrings` inlinable at string-case sites.
  case s[i].kind
  of ForkedSearch:
    let f = forked(s, i)
    let tmp = pool.syms.getOrIncl("`tcc." & $c.getTmpId)
    dest.copyIntoUnchecked "var", info:
      dest.add symdefToken(tmp, info)
      dest.addDotToken()                            # pragmas
      dest.copyIntoUnchecked "bool", info: discard  # type
      dest.copyIntoUnchecked "call", info:
        dest.add symToken(pool.syms.getOrIncl(StrAtLeOp), info)
        dest.add symToken(selector, info)
        dest.add intToken(pool.integers.getOrIncl(f.best[1]), info)
        dest.add charToken(f.best[0], info)
    dest.copyIntoUnchecked "if", info:
      dest.copyIntoUnchecked "elif", info:
        dest.add symToken(tmp, info)
        dest.copyIntoUnchecked "stmts", info:
          decodeSolution c, dest, s, f.thenA, selector, info
      dest.copyIntoUnchecked "else", info:
        dest.copyIntoUnchecked "stmts", info:
          decodeSolution c, dest, s, f.elseA, selector, info

  of LinearSearch:
    # Nested if/else so we can bind each branch's call to its own
    # temp while keeping short-circuit evaluation (later branches'
    # calls don't run when an earlier branch matched). Each non-last
    # branch opens `(if (elif tmp …) (else (stmts …)))`; the last
    # opens just `(if (elif tmp …))`. The closing parens are paid
    # off at the end.
    let choices = s[i].choices
    var pending = 0
    for idx, x in choices:
      let tmp = pool.syms.getOrIncl("`tcc." & $c.getTmpId)
      dest.copyIntoUnchecked "var", info:
        dest.add symdefToken(tmp, info)
        dest.addDotToken()
        dest.copyIntoUnchecked "bool", info: discard
        dest.copyIntoUnchecked "call", info:
          dest.add symToken(pool.syms.getOrIncl(EqStringsOp), info)
          dest.add symToken(selector, info)
          genStringLit c, dest, x[0], info
      # Open `(if (elif tmp (stmts (jmp lab))))` — elif/stmts/jmp are
      # fully closed; the surrounding (if needs a deferred close.
      dest.add parLeToken(pool.tags.getOrIncl("if"), info)
      dest.copyIntoUnchecked "elif", info:
        dest.add symToken(tmp, info)
        dest.copyIntoUnchecked "stmts", info:
          dest.copyIntoUnchecked "jmp", info:
            dest.add symToken(pool.syms.getOrIncl(x[1]), info)
      inc pending                                   # close the (if
      if idx + 1 < choices.len:
        dest.add parLeToken(pool.tags.getOrIncl("else"), info)
        dest.add parLeToken(pool.tags.getOrIncl("stmts"), info)
        inc pending, 2                              # close (else (stmts
    for _ in 0 ..< pending:
      dest.addParRi()

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
      while n.hasMore: skip n
      consumeParRi n
    of HconvX, ConvX:
      inc n
      assert n.typeKind == CstringT
      skip n
      result = getSimpleStringLit(c, n)
      skipParRi n
    of KvX:
      bug "not a string literal"
    else:
      bug "not a string literal"

proc transformStringCase*(c: var EContext; dest: var TokenBuf; n: var Cursor) =
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
    selector = pool.syms.getOrIncl("`tc." & $c.getTmpId)
    dest.copyIntoUnchecked "var", sinfo:
      dest.add symdefToken(selector, sinfo)
      dest.addDotToken() # pragmas
      dest.add symToken(pool.syms.getOrIncl(StringName), sinfo)
      dest.copyIntoUnchecked "call", sinfo:
        dest.add symToken(pool.syms.getOrIncl(BorrowCStringUnsafeOp), sinfo)
        trExpr(c, dest, selectorNode)
  elif selectorNode.kind == Symbol:
    selector = selectorNode.symId
  else:
    selector = pool.syms.getOrIncl("`tc." & $c.getTmpId)
    dest.copyIntoUnchecked "var", sinfo:
      dest.add symdefToken(selector, sinfo)
      dest.addDotToken() # pragmas
      dest.add symToken(pool.syms.getOrIncl(StringName), sinfo)
      trExpr(c, dest, selectorNode)
  skip nb # selector

  while nb.hasMore:
    if nb.substructureKind == OfU:
      let labl = "`sc." & $getTmpId(c)
      nb.into:                                # (of ...)
        assert nb.substructureKind == RangesU
        nb.into:                              # (ranges ...)
          while nb.hasMore:
            let litId = getSimpleStringLit(c, nb)
            pairs.add (pool.strings[litId], labl)
        skip nb # skip action for now
    else:
      skip nb

  let solution = createSearchTree(pairs)
  decodeSolution(c, dest, solution, 0, selector, selectorNode.info)
  var i = 0
  nb = n
  inc nb

  skip nb # selector
  let afterwards = pool.syms.getOrIncl("`sc." & $getTmpId(c))

  let elseLabel = pool.syms.getOrIncl("`sc." & $getTmpId(c))
  dest.copyIntoUnchecked "jmp", selectorNode.info:
    dest.add symToken(elseLabel, selectorNode.info)
  var hasElse = false
  while nb.hasMore:
    let info = nb.info
    if nb.substructureKind == OfU:
      dest.copyIntoUnchecked "lab", info:
        dest.add symdefToken(pool.syms.getOrIncl(pairs[i][1]), info)
      nb.into:                                # (of ...)
        nb.into:                              # (ranges ...)
          while nb.hasMore:
            skip nb
            inc i
        trStmt c, dest, nb
        dest.copyIntoUnchecked "jmp", info:
          dest.add symToken(afterwards, info)
    elif nb.substructureKind == ElseU:
      dest.copyIntoUnchecked "lab", info:
        dest.add symdefToken(elseLabel, info)
      inc nb
      trStmt c, dest, nb
      skipParRi nb
      hasElse = true
    else:
      error "invalid `case` statement", nb
  if not hasElse:
    dest.copyIntoUnchecked "lab", sinfo:
      dest.add symdefToken(elseLabel, sinfo)

  skipParRi nb
  dest.copyIntoUnchecked "lab", n.info:
    dest.add symdefToken(afterwards, n.info)
  n = nb

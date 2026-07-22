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
  case s[i].kind
  of ForkedSearch:
    let f = forked(s, i)

    # Hoist the `nimStrAtLe` probe into a `(var :tmp (bool) (call …))` instead
    # of nesting it in the `elif` condition. `nimStrAtLe` is `.inline`, but the
    # inliner (shoggoth's `intermodinliner`) only splices calls at statement /
    # var-init position — a call buried in the `elif` condition is opaque to it
    # (the same pipeline-ordering issue the array bound checks had). The bound
    # form below is exactly what the inliner's `trySpliceVarInit` recognises.
    let condTmp = pool.syms.getOrIncl("`tc." & $c.getTmpId)
    dest.copyIntoUnchecked "var", info:
      dest.addSymDef(condTmp, info)
      dest.addDotToken() # pragmas
      dest.addParLe("bool", info)
      dest.addParRi()
      dest.copyIntoUnchecked "call", info:
        dest.addSymUse(pool.syms.getOrIncl(StrAtLeOp), info)
        dest.addSymUse(selector, info)
        dest.addIntLit(f.best[1], info)
        dest.addCharLit(f.best[0], info)

    dest.copyIntoUnchecked "if", info:
      dest.copyIntoUnchecked "elif", info:
        dest.addSymUse(condTmp, info)
        dest.copyIntoUnchecked "stmts", info:
          decodeSolution c, dest, s, f.thenA, selector, info
      dest.copyIntoUnchecked "else", info:
        dest.copyIntoUnchecked "stmts", info:
          decodeSolution c, dest, s, f.elseA, selector, info

  of LinearSearch:
    dest.copyIntoUnchecked "if", info:
      for x in s[i].choices:
        dest.copyIntoUnchecked "elif", info:
          dest.copyIntoUnchecked "call", info:
            dest.addSymUse(pool.syms.getOrIncl(EqStringsOp), info)
            dest.addSymUse(selector, info)
            genStringLit c, dest, x[0], info
          dest.copyIntoUnchecked "stmts", info:
            dest.copyIntoUnchecked "jmp", info:
              dest.addSymUse(pool.syms.getOrIncl(x[1]), info)

proc getSimpleStringLit(c: var EContext; n: var Cursor): StrId =
  if n.isStringLit:
    result = n.strId
    inc n
  elif n.isSymbol:
    var inlineValue = getInitValue(c.typeCache, n.symId)
    if not cursorIsNil(inlineValue):
      result = getSimpleStringLit(c, inlineValue)
      inc n
    else:
      bug "not a string literal"
  else:
    case n.exprKind:
    of SufX:
      n.into:
        assert n.isStringLit
        result = n.strId
        while n.hasMore: skip n
    of HconvX, ConvX:
      n.into:
        assert n.typeKind == CstringT
        skip n
        result = getSimpleStringLit(c, n)
    of KvX:
      bug "not a string literal"
    else:
      bug "not a string literal"

proc transformStringCase*(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  # Prepare the list of (key, value) pairs:
  var pairs: seq[Key] = @[]
  var nb = n
  nb = sub(nb) # peek pass over the case; never left
  var selectorNode = nb
  let sinfo = selectorNode.info
  let selector: SymId

  let selectorType = getType(c.typeCache, selectorNode)
  if selectorType.typeKind == CstringT:
    # the other overload of `borrowCStringUnsafe`
    selector = pool.syms.getOrIncl("`tc." & $c.getTmpId)
    dest.copyIntoUnchecked "var", sinfo:
      dest.addSymDef(selector, sinfo)
      dest.addDotToken() # pragmas
      dest.addSymUse(pool.syms.getOrIncl(StringName), sinfo)
      dest.copyIntoUnchecked "call", sinfo:
        dest.addSymUse(pool.syms.getOrIncl(BorrowCStringUnsafeOp), sinfo)
        trExpr(c, dest, selectorNode)
  elif selectorNode.isSymbol:
    selector = selectorNode.symId
  else:
    selector = pool.syms.getOrIncl("`tc." & $c.getTmpId)
    dest.copyIntoUnchecked "var", sinfo:
      dest.addSymDef(selector, sinfo)
      dest.addDotToken() # pragmas
      dest.addSymUse(pool.syms.getOrIncl(StringName), sinfo)
      trExpr(c, dest, selectorNode)
  skip nb # selector

  while nb.hasMore:
    if nb.substructureKind == OfU:
      let labl = "`sc." & $getTmpId(c)
      nb.into:                                # (of ...)
        assert nb.substructureKind == RangesU
        nb.into:                              # (ranges ...)
          while nb.hasMore:
            let strId = getSimpleStringLit(c, nb)
            pairs.add (pool.strings[strId], labl)
        skip nb # skip action for now
    else:
      skip nb

  let solution = createSearchTree(pairs)
  decodeSolution(c, dest, solution, 0, selector, selectorNode.info)
  var i = 0
  nb = n
  let caseStart = nb
  nb = sub(nb)

  skip nb # selector
  let afterwards = pool.syms.getOrIncl("`sc." & $getTmpId(c))

  let elseLabel = pool.syms.getOrIncl("`sc." & $getTmpId(c))
  dest.copyIntoUnchecked "jmp", selectorNode.info:
    dest.addSymUse(elseLabel, selectorNode.info)
  var hasElse = false
  while nb.hasMore:
    let info = nb.info
    if nb.substructureKind == OfU:
      dest.copyIntoUnchecked "lab", info:
        dest.addSymDef(pool.syms.getOrIncl(pairs[i][1]), info)
      nb.into:                                # (of ...)
        nb.into:                              # (ranges ...)
          while nb.hasMore:
            skip nb
            inc i
        trStmt c, dest, nb
        dest.copyIntoUnchecked "jmp", info:
          dest.addSymUse(afterwards, info)
    elif nb.substructureKind == ElseU:
      dest.copyIntoUnchecked "lab", info:
        dest.addSymDef(elseLabel, info)
      nb.into:
        trStmt c, dest, nb
      hasElse = true
    else:
      error "invalid `case` statement", nb
  if not hasElse:
    dest.copyIntoUnchecked "lab", sinfo:
      dest.addSymDef(elseLabel, sinfo)

  nb = caseStart; skip nb
  dest.copyIntoUnchecked "lab", n.info:
    dest.addSymDef(afterwards, n.info)
  n = nb

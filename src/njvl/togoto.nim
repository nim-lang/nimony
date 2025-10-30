#
#
#           Nimony Goto Insertion Pass
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Undoes the effects of NJVL by turning `(jtrue X)` statements into goto statements, if possible.

## **THIS IS CURRENTLY UNUSED AND UNTESTED AND UNFINISHED!**

import std / [tables, sets, assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav]
import ".." / hexer / [mover]

import njvl_model

type
  JtrueInstr = object
    pos: int
    isJump: bool
  Context* = object
    typeCache: TypeCache
    state: Table[SymId, (bool, int)] # (value, active)
    mustMaterialize: HashSet[SymId]
    jtrueInstrs: Table[SymId, JtrueInstr]

  CfvarState = object
    s: SymId
    value: bool # we always know the value of the cfvar
                # (they start out as false and can only become
                # true and then stay true)

  CfvarMask = seq[CfvarState]

proc computeCfvarMask(c: var Context; n: var Cursor; mask: var CfvarMask) =
  case n.kind
  of Symbol:
    let s = n.symId
    if c.state.hasKey(s):
      mask.add CfvarState(s: s, value: c.state[s][0])
    inc n
  of ParLe:
    case n.exprKind
    of NotX:
      inc n
      let oldLen = mask.len
      computeCfvarMask(c, n, mask)
      # change polarity for new entries:
      for i in oldLen..<mask.len:
        mask[i].value = not mask[i].value
      skipParRi n
    of AndX, OrX:
      inc n
      computeCfvarMask(c, n, mask)
      computeCfvarMask(c, n, mask)
      skipParRi n
    else:
      skip n
  else:
    skip n

proc finishTrace(c: var Context; s: SymId) =
  if c.jtrueInstrs.hasKey(s):
    c.jtrueInstrs[s].isJump = not c.mustMaterialize.contains(s)

proc aStmt(c: var Context; n: var Cursor) =
  case n.njvlKind
  of JtrueV:
    # we now know these symbols are true:
    inc n
    while n.kind != ParRi:
      assert n.kind == Symbol
      let s = n.symId
      finishTrace(c, s)
      c.state[s][0] = true
      inc n
    inc n
  of IteV, ItecV:
    inc n
    # we mask out cfvars here. Thus the statements that are
    # under their guard cause no trouble!
    var mask: CfvarMask = @[]
    computeCfvarMask(c, n, mask)
    # then branch, we know all the entries in the mask that are `true`
    # are protecting this branch:
    for m in mask.mitems:
      if m.value:
        inc c.state[m.s][1]
    aStmt c, n
    # in the else branch we know the other cfvars are active:
    for m in mask.mitems:
      if m.value:
        dec c.state[m.s][1]
      else:
        inc c.state[m.s][1]
    aStmt c, n
    # skip join information
    skip n
    skipParRi n
  of CfvarV:
    inc n
    assert n.kind == SymbolDef
    # we know they start as false and inactive:
    c.state[n.symId] = (false, 0)
    inc n
    skipParRi n
  else:
    case n.stmtKind
    of AsgnS, CallKindsS:
      # we encountered an effect, so all cfvars not currently
      # disabled cannot be turned to jumps:
      for s, state in c.state:
        if state[1] == 0:
          c.mustMaterialize.incl s
    else:
      discard
    # ordinary recursion
    inc n
    while n.kind != ParRi:
      aStmt c, n
    inc n

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.njvlKind
  of JtrueV:
    let info = n.info
    # we now know these symbols are true:
    inc n
    var label = NoSymId
    while n.kind != ParRi:
      assert n.kind == Symbol
      let s = n.symId
      if c.mustMaterialize.contains(s):
        dest.copyIntoKind StoreV, info:
          dest.addParPair TrueX, info
          dest.addSymUse s, info
      else:
        # see `ret` construction, etc. the last label counts
        label = s
      inc n
    inc n
    if label != NoSymId:
      dest.add tagToken("jmp", info)
      dest.addSymUse label, info
      dest.addParRi()
  of CfvarV:
    inc n
    assert n.kind == SymbolDef
    # we know they start as false and inactive:
    c.state[n.symId] = (false, 0)
    inc n
    skipParRi n
  else:
    case n.kind
    of Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StringLit, DotToken, EofToken, UnknownToken, Ident:
      dest.takeToken n
    of ParLe:
      dest.takeToken n
      while n.kind != ParRi:
        trStmt c, dest, n
      dest.takeToken n
    of ParRi:
      bug "Unmatched ParRi"


proc toGoto*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(typeCache: createTypeCache())
  var n = n
  c.typeCache.openScope()
  result = createTokenBuf(300)
  assert n.stmtKind == StmtsS, $n.kind
  result.add n

  aStmt c, n

  inc n
  while n.kind != ParRi:
    trStmt c, result, n
  result.addParRi()
  c.typeCache.closeScope()

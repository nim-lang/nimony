#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## Pass 1 of code generation: analyse local-variable usage in a proc body.
##
## For every local we record how often it is defined/used (weighted so that
## uses inside loops count more) and whether its address is taken. We also
## track, per scope, whether it contains a call: variables confined to a
## call-free scope may use volatile (caller-saved) registers (`AllRegs`),
## while variables live across a call must go to callee-saved registers or
## the stack. The register allocator consumes this.
##
## Ported from `src/wip/native/analyser.nim` to the nifcore cursor API; keyed
## by symbol *name* (nifcore has no stable SymId for inline-short symbols).

import std / [tables, sets, assertions]
import nifcore
import nifcdecl
import slots

type
  VarInfo* = object
    defs*, usages*: int        ## how often the variable is defined / used
    weight*: int               ## usages, but loop bodies count `LoopWeight`×
    props*: VarProps

  ProcAnalysis* = object
    vars*: Table[string, VarInfo]
    hasCall*: bool

  Scope = object
    vars: seq[string]
    hasCall: bool

  Context = object
    inLoops, inAddr, inAsgnTarget, inArrayIndex: int
    res: ProcAnalysis
    scopes: seq[Scope]
    tvars: HashSet[string]     ## thread-local var names: a reference acts like a call

const
  LoopWeight = 3   ## assume a loop body runs ~3× for weighting purposes

proc openScope(c: var Context) =
  c.scopes.add Scope()

proc closeScope(c: var Context) =
  let finished = c.scopes.pop()
  if not finished.hasCall:
    for v in finished.vars:
      c.res.vars[v].props.incl AllRegs
  elif c.scopes.len > 0:
    # a scope "has a call" if any inner scope did
    c.scopes[^1].hasCall = true

proc analyse(c: var Context; n: var Cursor)

proc analyseChildren(c: var Context; n: var Cursor) =
  n.into:
    while n.hasMore: analyse(c, n)

proc analyseVarDecl(c: var Context; n: var Cursor) =
  ## `(var :name pragmas type value)` (also gvar/tvar/const).
  n.into:
    assert n.kind == SymbolDef
    let vn = symName(n); inc n
    skip n                       # pragmas
    skip n                       # type
    let hasValue = n.kind != DotToken
    c.res.vars[vn] = VarInfo(defs: ord(hasValue))
    c.scopes[^1].vars.add vn
    if hasValue: analyse(c, n)   # analyse the initializer
    else: inc n                  # consume the `.`

proc analyse(c: var Context; n: var Cursor) =
  case n.kind
  of Symbol:
    let vn = symName(n)
    if c.res.vars.hasKey(vn):
      let e = addr c.res.vars[vn]
      if c.inAsgnTarget > 0: inc e.defs
      else: inc e.usages
      # each use counts; uses inside loops count `LoopWeight`× per nesting level
      inc e.weight, 1 + c.inLoops * LoopWeight
      if (c.inAddr + c.inArrayIndex) > 0:
        # arrays / address-taken locals cannot live in a register
        e.props.incl AddrTaken
    elif vn in c.tvars:
      # A thread-local access lowers to the TLV thunk call (clobbers x0/lr), so
      # treat it like a call: the proc needs a frame and its locals/params must
      # avoid the volatile argument registers.
      c.scopes[^1].hasCall = true
    inc n
  of IntLit, UIntLit, FloatLit, CharLit, StrLit, Ident, SymbolDef, DotToken:
    inc n
  of TagLit:
    case n.stmtKind
    of NoStmt:
      case n.exprKind
      of AtC, PatC:
        n.into:
          inc c.inArrayIndex
          analyse(c, n)                 # the array/base
          dec c.inArrayIndex
          let oldA = c.inAddr; let oldT = c.inAsgnTarget
          c.inAddr = 0; c.inAsgnTarget = 0
          analyse(c, n)                 # the index
          c.inAddr = oldA; c.inAsgnTarget = oldT
      of AddrC:
        n.into:
          inc c.inAddr
          while n.hasMore: analyse(c, n)
          dec c.inAddr
      of NoExpr:
        # `elif`/`else`/`of` carry a condition and a statement body — recurse so
        # uses and calls inside `if`/`case` branches are seen. Other NoExpr nodes
        # (types, etc.) carry no locals and are skipped.
        case n.substructureKind
        of ElifU, ElseU, OfU: analyseChildren(c, n)
        else: skip n
      else:
        analyseChildren(c, n)           # generic expression: recurse
    of StmtsS, ScopeS:
      c.openScope()
      n.into:
        while n.hasMore: analyse(c, n)
      c.closeScope()
    of CallS:
      analyseChildren(c, n)
      c.scopes[^1].hasCall = true
    of VarS, GvarS, TvarS, ConstS:
      analyseVarDecl(c, n)
    of AsgnS:
      n.into:
        inc c.inAsgnTarget
        analyse(c, n)                   # the lvalue
        dec c.inAsgnTarget
        analyse(c, n)                   # the rvalue
    of ProcS, TypeS:
      skip n                            # nested decls: not our locals
    of WhileS:
      n.into:
        inc c.inLoops
        while n.hasMore: analyse(c, n)
        dec c.inLoops
    else:
      analyseChildren(c, n)             # if/case/ret/... : recurse
  else:
    inc n

proc analyseParams(c: var Context; params: var Cursor) =
  ## `(params (param :name pragmas type) …)` or a DotToken.
  if params.kind != TagLit: return
  params.into:
    while params.hasMore:
      params.into:                      # (param …)
        assert params.kind == SymbolDef
        let vn = symName(params); inc params
        c.res.vars[vn] = VarInfo(defs: 1)   # a parameter always has a value
        c.scopes[^1].vars.add vn
        while params.hasMore: skip params   # pragmas, type
        # (rest consumed by into epilogue)

proc analyseProc*(procDecl: Cursor; tvars: HashSet[string] = initHashSet[string]()): ProcAnalysis =
  ## `procDecl` is at a `(proc name params rettype pragmas body)`. `tvars` names
  ## the module's thread-locals so their uses force a call-like analysis.
  var c = Context(tvars: tvars)
  c.scopes.add Scope()                  # the always-present outermost scope
  var n = procDecl
  assert n.stmtKind == ProcS
  n.into:
    inc n                               # name (SymbolDef)
    analyseParams(c, n)                 # params
    skip n                              # return type
    skip n                              # pragmas
    analyse(c, n)                       # body
  c.res.hasCall = c.scopes[0].hasCall
  result = ensureMove c.res

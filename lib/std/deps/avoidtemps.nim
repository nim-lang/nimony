#
#
#           Nimony
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The `avoidtemps` type plugin: expression-template style elimination of
## temporaries for value types with in-place arithmetic.
##
## .. code-block:: nim
##   type
##     Matrix {.plugin: "std/deps/avoidtemps".} = object
##       a: array[4, array[4, float]]
##
## For every module that uses `Matrix`, a declaration
##
## .. code-block:: nim
##   let e = a*b + c - d
##
## becomes
##
## .. code-block:: nim
##   var e = a
##   e *= b
##   e += c
##   e -= d
##
## A right operand that is itself such a chain is computed into a single fresh
## temporary first, so `a - (b*c + d)` costs one temporary instead of two.
##
## When the first right operand is itself a chain, as in `a - (b*c + d)`, that
## chain is computed directly into the target and the leaf applied
## afterwards: `var e = b; e *= c; e += d; subFrom(e, a)`. For `+` this uses
## `t += a`, so the contract includes that `+` is commutative. For `-`, `*`
## and `/` it uses `subFrom(t, a)`, `mulFrom(t, a)`, `divFrom(t, a)`, meaning
## `t = a op t`; these are optional, and where one does not resolve the
## chain falls back to the in-order form with a temporary. Since the leaf is
## now read last, this only happens when the leaf and the operand contain
## nothing but symbols, literals, constructors, field accesses, conversions
## and the plugin's operators.
##
## An assignment `x = a*b + c` to a local `x` of the type becomes
## `x = a; x *= b; x += c` when the right-hand side does not mention `x`.
## Here even a single operator saves the temporary that would be moved
## into `x`.
##
## **Contract.** Attaching the plugin is a promise by the type's author: for
## every operator `op` in `+ - * /` and every `y` for which `x op y` compiles
## when `x` is of the type, `x op y` returns the type, `x op= y` exists, and
## `x op y` means `(var t = x; t op= y; t)`; also, `+` on two values of the
## type is commutative. The plugin cannot see the
## operators' declarations (they usually live in another module), so it relies
## on this promise. It only starts a chain at an operand whose type it *knows*
## is the plugin's type — a local, parameter or global declared with it, or an
## object constructor of it — and only rewrites declarations, whose fresh
## variable nothing can alias, and assignments to locals, which no parameter can
## alias either. (Globals and `var` parameters may be aliased by a parameter
## the right-hand side reads after the first in-place step has overwritten the
## target, so assignments to them are left alone.) Everything else is passed
## through untouched.
##
## For a generic type the plugin is triggered by the generic symbol while the
## code mentions its instances; an instance is recognized by having the
## generic type's name.

import std / [tables, sets, strutils]
import plugins

const
  InPlaceOps = ["+", "-", "*", "/"]

type
  Step = object
    ## One `target op= rhs` of a chain.
    op: string
    rhs: NifCursor
  Chain = object
    ## `leaf op1 rhs1 op2 rhs2 ...`, left-associated. `typ` is the leaf's type.
    leaf, typ: NifCursor
    steps: seq[Step]
  Rewriter = object
    typeSym: SymId               # the type that triggered the plugin
    typeName: string             # its basename, to recognize generic instances
    known: Table[SymId, NifCursor] # locals/params/globals of the type -> their type
    locals: HashSet[SymId]       # the subset declared by a routine-local `var`

proc isTheType(r: Rewriter; t: NifCursor): bool =
  if t.kind != Symbol: return false
  let s = t.symId
  if s == r.typeSym: return true
  # A generic instance `Vec.0.Ihash.mod` of the generic `Vec`:
  let parts = s.symText.split('.')
  result = parts.len >= 3 and parts[2].len > 0 and parts[2][0] == 'I' and
    s.symBasename == r.typeName

proc recordLocal(r: var Rewriter; n: NifCursor) =
  ## `n` is a local-like declaration `(kind :name export pragmas type value)`.
  let d = asLocal(n)
  if d.name.kind != SymbolDef: return
  if r.isTheType(d.typ):
    r.known[d.name.symId] = d.typ
    if n.stmtKind in {VarS, ResultS}:
      r.locals.incl d.name.symId
  elif d.typ.kind == TagLit and d.typ.typeKind == MutT:
    let inner = firstChild(d.typ)
    if r.isTheType(inner):
      # sem has not inserted the dereference yet: the parameter is read as is
      r.known[d.name.symId] = inner

proc unwrap(n: NifCursor): NifCursor =
  ## Looks through `(expr X)` and `(par X)` wrappers that carry a single value.
  result = n
  while result.kind == TagLit and result.exprKind in {ExprX, ParX}:
    let c = firstChild(result)
    var after = c
    skip after
    if after.hasMore: break
    result = c

proc leafType(r: Rewriter; n: NifCursor; typ: var NifCursor): bool =
  ## Is `n` an operand that is known to be of the plugin's type?
  let n = unwrap(n)
  result = false
  if n.kind == Symbol:
    if n.symId in r.known:
      typ = r.known.getOrDefault(n.symId)
      result = true
  elif n.kind == TagLit:
    if n.exprKind == OconstrX:
      let t = firstChild(n)
      if r.isTheType(t):
        typ = t
        result = true

proc asBinOp(n: NifCursor; op: var string; lhs, rhs: var NifCursor): bool =
  ## Matches `(infix op lhs rhs)` / `(call op lhs rhs)` for an in-place-able op.
  result = false
  if n.kind == TagLit and n.exprKind in {InfixX, CallX}:
    var c = firstChild(n)
    if c.kind != Symbol: return false
    let name = c.symId.symBasename
    if name notin InPlaceOps: return false
    skip c
    if not c.hasMore: return false
    lhs = c
    skip c
    if not c.hasMore: return false
    rhs = c
    skip c
    if c.hasMore: return false
    op = name
    result = true

proc matchChain(r: Rewriter; n: NifCursor; ch: var Chain): bool =
  ## Walks down the left spine of `n`. Succeeds when there is at least one
  ## operator and the leftmost operand is known to be of the plugin's type.
  ch.steps.setLen 0
  var n = unwrap(n)
  var op = ""
  var lhs = NifCursor()
  var rhs = NifCursor()
  while asBinOp(n, op, lhs, rhs):
    ch.steps.add Step(op: op, rhs: rhs)
    n = unwrap(lhs)
  result = ch.steps.len > 0 and r.leafType(n, ch.typ)
  if result:
    ch.leaf = n
    # collected outermost first; evaluation order is innermost first:
    var i = 0
    var j = ch.steps.high
    while i < j:
      swap ch.steps[i], ch.steps[j]
      inc i
      dec j

proc flattenable(r: Rewriter; rhs: NifCursor; sub: var Chain): bool =
  ## A right operand earns its own temporary only if that saves one.
  result = r.matchChain(rhs, sub) and sub.steps.len >= 2

proc pure(n: NifCursor): bool =
  ## Can evaluating `n` neither write anything nor observe a write? Symbols,
  ## literals, constructors, field accesses, conversions, and the plugin's
  ## operators (pure by the contract). Anything else may be a call with
  ## effects.
  if n.kind != TagLit: return true
  let ek = n.exprKind
  if ek in {InfixX, CallX}:
    var c = firstChild(n)
    if c.kind != Symbol or c.symId.symBasename notin InPlaceOps: return false
  elif ek notin {ParX, ExprX, OconstrX, DotX, ConvX, HconvX, HderefX, SufX} and
      n.otherKind != KvU and n.typeKind == NoType:
    return false
  var c = firstChild(n)
  while c.hasMore:
    if not pure(c): return false
    skip c
  result = true

proc reversible(r: Rewriter; ch: Chain; sub: var Chain): bool =
  ## `leaf op1 (sub) ...` can be computed as `target = sub; target = leaf op1
  ## target; ...`: the first right operand goes straight into the target and
  ## the leaf is applied afterwards, so `sub` needs no temporary. This reads
  ## the leaf after `sub` instead of before, which is only unobservable when
  ## both are pure.
  result = ch.steps.len > 0 and r.matchChain(ch.steps[0].rhs, sub) and
    pure(ch.leaf) and pure(ch.steps[0].rhs)

const
  ReverseOps = ["subFrom", "mulFrom", "divFrom"] # for `-`, `*`, `/`

proc reverseOp(op: string): string =
  case op
  of "-": ReverseOps[0]
  of "*": ReverseOps[1]
  else: ReverseOps[2]

type
  InitMode = enum
    InitDecl   # the original declaration `orig`, with its value replaced
    InitAsgn   # `target = value`
    InitTemp   # `var target: T = value`

  Init = object
    mode: InitMode
    orig: NifCursor              # InitDecl: the declaration to copy from
    typ: NifCursor               # InitTemp: the type of the temporary
    declKind: NimonyStmt         # how temporaries are declared

proc emitInit(dest: var NifBuilder; ini: Init; target: SymId; value: NifCursor;
              info: LineInfo) =
  var value = value
  case ini.mode
  of InitDecl:
    dest.withTree ini.declKind, info:
      var c = firstChild(ini.orig)
      dest.takeTree c                # :name
      dest.takeTree c                # export marker
      dest.takeTree c                # pragmas
      dest.takeTree c                # type
      dest.takeTree value
  of InitAsgn:
    dest.withTree AsgnS, info:
      dest.addSymUse target, info
      dest.takeTree value
  of InitTemp:
    dest.withTree ini.declKind, info:
      dest.addSymDef target, info
      dest.addEmptyNode2 info        # export marker, pragmas
      var t = ini.typ
      dest.takeTree t
      dest.takeTree value

proc emitChain(r: Rewriter; dest: var NifBuilder; ch: Chain; target: SymId;
               ini: Init; info: LineInfo)

proc emitSteps(r: Rewriter; dest: var NifBuilder; ch: Chain; first: int;
               target: SymId; declKind: NimonyStmt; info: LineInfo) =
  ## `target op= rhs` for the steps from `first` on. A right operand that is a
  ## chain worth flattening is computed into its own temporary first; this
  ## keeps the original evaluation order (leaf, rhs1, rhs2, ...).
  for i in first ..< ch.steps.len:
    let st = ch.steps[i]
    var rhs = st.rhs
    var sub = Chain()
    if r.flattenable(rhs, sub):
      let tmp = genSym()
      r.emitChain(dest, sub, tmp,
        Init(mode: InitTemp, typ: sub.typ, declKind: declKind), info)
      dest.withTree InfixX, info:
        dest.addIdent st.op & "="
        dest.addSymUse target, info
        dest.addSymUse tmp, info
    else:
      dest.withTree InfixX, info:
        dest.addIdent st.op & "="
        dest.addSymUse target, info
        dest.takeTree rhs

proc emitReversed(r: Rewriter; dest: var NifBuilder; ch, sub: Chain;
                  target: SymId; ini: Init; info: LineInfo) =
  ## `target = sub; target = leaf op1 target; <remaining steps>`.
  r.emitChain(dest, sub, target, ini, info)
  let op = ch.steps[0].op
  var leaf = ch.leaf
  if op == "+":
    dest.withTree InfixX, info:      # `+` commutes: `leaf + t == t + leaf`
      dest.addIdent "+="
      dest.addSymUse target, info
      dest.takeTree leaf
  else:
    dest.withTree CallX, info:
      dest.addIdent reverseOp(op)
      dest.addSymUse target, info
      dest.takeTree leaf
  r.emitSteps(dest, ch, 1, target, ini.declKind, info)

proc emitChain(r: Rewriter; dest: var NifBuilder; ch: Chain; target: SymId;
               ini: Init; info: LineInfo) =
  ## Initializes `target` with the value of `ch`.
  var sub = Chain()
  if not r.reversible(ch, sub):
    emitInit(dest, ini, target, ch.leaf, info)
    r.emitSteps(dest, ch, 0, target, ini.declKind, info)
  elif ch.steps[0].op == "+":
    r.emitReversed(dest, ch, sub, target, ini, info)
  else:
    # `subFrom` & co are optional: use them only where they resolve.
    #   when compiles((var t = leaf; subFrom(t, leaf))): <reversed>
    #   else: <in order>
    dest.withTree WhenS, info:
      dest.withTree ElifU, info:
        dest.withTree CallX, info:
          dest.addIdent "compiles"
          dest.withTree ExprX, info:
            let probe = genSym()
            dest.withTree StmtsS, info:
              dest.withTree VarS, info:
                dest.addSymDef probe, info
                dest.addEmptyNode3 info
                var init = ch.leaf
                dest.takeTree init
            dest.withTree CallX, info:
              dest.addIdent reverseOp(ch.steps[0].op)
              dest.addSymUse probe, info
              var arg = ch.leaf
              dest.takeTree arg
        dest.withTree StmtsS, info:
          r.emitReversed(dest, ch, sub, target, ini, info)
      dest.withTree ElseU, info:
        dest.withTree StmtsS, info:
          emitInit(dest, ini, target, ch.leaf, info)
          r.emitSteps(dest, ch, 0, target, ini.declKind, info)

proc mutableKind(k: NimonyStmt): NimonyStmt =
  case k
  of LetS: VarS
  of GletS: GvarS
  else: k

proc trDecl(r: var Rewriter; dest: var NifBuilder; n: var NifCursor): bool =
  ## Rewrites `let e: T = chain` in place. Returns false (consuming nothing)
  ## when the declaration does not qualify.
  let d = asLocal(n)
  var ch = Chain()
  if d.name.kind != SymbolDef or not r.isTheType(d.typ) or
      not r.matchChain(d.val, ch):
    return false
  var sub = Chain()
  var worth = ch.steps.len >= 2 or r.reversible(ch, sub)
  for st in ch.steps:
    if r.flattenable(st.rhs, sub): worth = true
  if not worth: return false

  let kind = mutableKind(n.stmtKind)
  r.emitChain(dest, ch, d.name.symId,
    Init(mode: InitDecl, orig: n, declKind: kind), n.info)
  skip n
  result = true

proc mentions(n: NifCursor; s: SymId): bool =
  if n.kind == Symbol: return n.symId == s
  if n.kind != TagLit: return false
  var c = firstChild(n)
  while c.hasMore:
    if mentions(c, s): return true
    skip c
  result = false

proc trAsgn(r: var Rewriter; dest: var NifBuilder; n: var NifCursor): bool =
  ## Rewrites `x = chain` for a local `x` that `chain` does not mention.
  ## Returns false (consuming nothing) when the assignment does not qualify.
  var c = firstChild(n)
  if c.kind != Symbol or c.symId notin r.locals: return false
  let target = c.symId
  skip c
  let rhs = c
  var ch = Chain()
  if not r.matchChain(rhs, ch) or mentions(rhs, target): return false
  r.emitChain(dest, ch, target, Init(mode: InitAsgn, declKind: VarS), n.info)
  skip n
  result = true

proc trAux(r: var Rewriter; dest: var NifBuilder; n: var NifCursor) =
  if n.kind != TagLit:
    dest.takeTree n
    return
  let sk = n.stmtKind
  if sk in {LetS, VarS, GletS, GvarS, TvarS, CursorS, ResultS} or
      n.otherKind == ParamU:
    r.recordLocal n
  # not `tvar`: its initializer runs per thread and must stay one expression
  if sk in {LetS, VarS, GletS, GvarS} and r.trDecl(dest, n):
    return
  if sk == AsgnS and r.trAsgn(dest, n):
    return
  dest.copyInto(n):
    while n.hasMore:
      r.trAux(dest, n)

proc tr(r: var Rewriter; n: NifCursor): NifBuilder =
  result = createTree()
  var n = n
  r.trAux(result, n)

proc main =
  var types = loadTypeDefinitions()
  var r = Rewriter()
  var t = firstChild(types)
  # one plugin run per triggering type, so there is exactly one symbol here:
  r.typeSym = t.symId
  r.typeName = r.typeSym.symBasename
  var inp = loadPluginInput()
  saveTree r.tr(inp)

main()

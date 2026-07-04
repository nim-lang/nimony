#
#
#           Leng Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## **JS-as-NIF** — the intermediate representation the JavaScript backend emits,
## and a tiny `jsnif -> JS text` printer.
##
## Instead of the codegen assembling JS by string concatenation (`"(" & a & " + "
## & b & ")"` — fiddly to keep correct, impossible to optimize), it builds a NIF
## tree of JS constructs with a dedicated `JsTag` enum, exactly as Araq specified
## on PR #2043: *"produce the JS as a NIF-tree with a dedicated JS enum … then a
## tiny jsnif -> js emitter. This way the code is not full of stupid string
## operations we have to keep correct all the time, plus you can easily run a
## peephole optimizer later on the nifjs."*
##
## Two pieces live here:
##   1. the `JsTag` model + a `JsBuilder` wrapper over `nifcore`'s `TokenBuf`
##      (the tree-building API — `openTag`/`closeTag`/`addStrLit`/… — that Araq
##      notes was "designed with this in mind");
##   2. `emit`, a ~one-line-per-tag printer that is the *only* place JS text is
##      produced. Parenthesization/indentation are its job, so the builder never
##      thinks about precedence.
##
## The pool is private to the backend process (build and print happen in one
## run), so tags need no master-ordinal registration: the enum's first value is
## ordinal 0 and a `+1` shim bridges to pool ids (which start at 1), the same
## trick `nifcore`'s docs describe for the `jsonnif`/`htmlnif` adapters.

import std / assertions
import ".." / lib / nifcoreparse   # re-exports nifcore: TokenBuf, cursors, builder

type
  JsTag* = enum
    ## The JS construct vocabulary. Comments give the surface syntax `emit`
    ## produces; `E` = expression child, `S` = statement child.
    jNone            ## empty slot (missing init / else / return value)
    jModule          ## (module S…)              -> the whole file
    jFunc            ## (func NAME PARAMS BODY)   -> function NAME(…) { … }
    jParams          ## (params NAME…)            -> parameter list
    jVar             ## (var NAME E?)             -> let NAME = E;   (or `let NAME;`)
    jVarFn           ## (varfn NAME E?)           -> var NAME = E;   (function-scoped)
    jAsgn            ## (asgn E_lhs E_rhs)         -> lhs = rhs;
    jIf              ## (if E BODY ELSE?)          -> if (E) { … } else { … }
    jWhile           ## (while E BODY)             -> while (E) { … }
    jSwitch          ## (switch E CASE… )          -> switch (E) { … }
    jCase            ## (case LABELS BODY)         -> case L: … break;
    jDefault         ## (default BODY)             -> default: … break;
    jLabels          ## (labels E…)               -> the label list of a `jCase`
    jRet             ## (ret E?)                   -> return E;   (or `return;`)
    jExprStmt        ## (estmt E)                  -> E;
    jBlock           ## (block S…)                 -> { … }  (an indented stmt list)
    jLabel           ## (label NAME BLOCK)         -> NAME: { … }  (a labeled block)
    jBreak           ## (break NAME?)              -> break;  (or `break NAME;`)
    jContinue        ## (continue)                 -> continue;
    jThrow           ## (throw E?)                 -> throw E;  (or throw new Error();)
    jRaw             ## (raw "text")               -> text verbatim (stmt or expr)
    jCall            ## (call E_callee E_arg…)      -> callee(arg, …)
    jBin             ## (bin "op" E E)             -> (a op b)
    jUn             ## (un "op" E)                 -> (op a)
    jIndex           ## (index E E)                -> a[b]
    jMember          ## (member E "name")          -> a.name
    jArray           ## (array E…)                 -> [e0, …]  (incl. fat pointers)
    jObject          ## (object KV…)               -> {k: v, …}
    jKv              ## (kv "key" E)               -> key: v    (bare-ident key)
    jNum             ## (num 123)                  -> integer literal
    jUNum            ## (unum 123)                 -> unsigned literal
    jStr             ## (str "…")                  -> JS string literal (escaped)
    jName            ## (name "ident")             -> bare identifier
    jCond            ## (cond E E E)               -> (c ? a : b)
    jArrow           ## (arrow PARAMS BODY)        -> (…) => { … }
    jUndef           ## (undef "note")             -> undefined/*note*/
    jBigNum          ## (bignum 123)               -> BigInt literal `123n`   (int64)
    jBigUNum         ## (bigunum 123)              -> BigInt literal `123n`   (uint64)

  JsBuilder* = object
    ## Accumulates a jsnif tree. Thin wrapper so callers say `b.tree(jBin): …`
    ## rather than juggling raw tag ids.
    buf*: TokenBuf

template tagId(t: JsTag): TagId =
  ## The pool id of `t`, *by construction*: `createTags` registers the tags in
  ## enum order and BiTable ids are 1-based, so `TagId == ord(JsTag) + 1` (that
  ## alignment is asserted in `createTags`). No per-builder `ids` array needed.
  TagId(uint32(ord(t)) + 1'u32)

proc tagOf(c: Cursor): JsTag =
  ## The inverse of `tagId`: decode the `JsTag` at `c`. A bounds-checked ordinal
  ## move — `o == 0` wraps unsigned and fails the check — not an array lookup.
  if c.kind == TagLit:
    let o = uint32(c.cursorTagId)
    if o - 1'u32 <= uint32(ord(high(JsTag))): return JsTag(o - 1'u32)
  jNone

proc initJsBuilder*(): JsBuilder =
  ## `createTags[JsTag]()` registers the JS tags in ordinal order and asserts the
  ## `TagId == ord + 1` alignment that `tagId`/`tagOf` rely on.
  result = JsBuilder(buf: createTokenBuf(64, sharedTags = createTags[JsTag]()))

# ── building ────────────────────────────────────────────────────────────────

proc open*(b: var JsBuilder; t: JsTag) {.inline.} = b.buf.openTag tagId(t)
proc close*(b: var JsBuilder) {.inline.} = b.buf.closeTag()

template tree*(b: var JsBuilder; t: JsTag; body: untyped) =
  ## Open `t`, run `body` (which appends the children), close `t`.
  b.open t
  body
  b.close()

proc leaf*(b: var JsBuilder; t: JsTag) {.inline.} =
  ## A childless tagged node (`jBreak`, `jContinue`, `jNone`).
  b.open t; b.close()

proc name*(b: var JsBuilder; ident: string) = b.tree jName: b.buf.addIdent ident
proc str*(b: var JsBuilder; s: string) = b.tree jStr: b.buf.addStrLit s
proc raw*(b: var JsBuilder; s: string) = b.tree jRaw: b.buf.addStrLit s
proc num*(b: var JsBuilder; v: int64) = b.tree jNum: b.buf.addIntLit v
proc unum*(b: var JsBuilder; v: uint64) = b.tree jUNum: b.buf.addUIntLit v
proc bignum*(b: var JsBuilder; v: int64) = b.tree jBigNum: b.buf.addIntLit v
proc bigunum*(b: var JsBuilder; v: uint64) = b.tree jBigUNum: b.buf.addUIntLit v
proc none*(b: var JsBuilder) = b.leaf jNone
proc undef*(b: var JsBuilder; note: string) = b.tree jUndef: b.buf.addStrLit note
proc addOp*(b: var JsBuilder; s: string) {.inline.} =
  ## A bare operator/field ident token — the first child of `jBin`/`jUn`/`jMember`
  ## (not wrapped in `jName`; the emitter reads it with `strVal`).
  b.buf.addIdent s

template member*(b: var JsBuilder; field: string; obj: untyped) =
  ## `obj.field` — `obj` is a body that builds the object expression.
  b.tree jMember:
    obj
    b.buf.addIdent field

proc kvKey*(b: var JsBuilder; key: string) {.inline.} =
  ## The bare-ident key token that leads a `jKv` (an object-literal entry).
  b.buf.addIdent key

proc breakTo*(b: var JsBuilder; label: string) =
  ## `break <label>;` — a labeled break (the target of a forward `jmp`).
  b.tree jBreak: b.buf.addIdent label

template labelTree*(b: var JsBuilder; label: string; body: untyped) =
  ## `<label>: { <body> }` — a labeled block; `body` builds the block's statements.
  b.tree jLabel:
    b.buf.addIdent label
    b.tree jBlock:
      body

# ── emitting (jsnif -> JS text) ───────────────────────────────────────────────

type Emitter = object
  outp: string
  indent: int

proc wr(e: var Emitter; s: string) {.inline.} = e.outp.add s
proc nl(e: var Emitter) =
  e.outp.add "\n"
  for _ in 0 ..< e.indent: e.outp.add "  "

proc jsEscape(s: string): string =
  result = "\""
  for ch in s:
    case ch
    of '\\': result.add "\\\\"
    of '"': result.add "\\\""
    of '\n': result.add "\\n"
    of '\r': result.add "\\r"
    of '\t': result.add "\\t"
    else: result.add ch
  result.add "\""

proc emitExpr(e: var Emitter; c: var Cursor)
proc emitStmt(e: var Emitter; c: var Cursor)

proc emitBlock(e: var Emitter; c: var Cursor) =
  ## Emit a `(block S…)` (or a `jNone`) as `{ … }` with an indented body.
  e.wr "{"
  inc e.indent
  if tagOf(c) == jBlock:
    c.into:
      while c.hasMore:
        e.nl(); e.emitStmt c
  else:
    skip c        # jNone or an unexpected node: empty block
  dec e.indent
  e.nl(); e.wr "}"

const jsMaxSafe = 9007199254740991'i64   ## Number.MAX_SAFE_INTEGER (2^53 - 1)

proc jsInt(v: int64): string =
  ## A JS integer literal. Values past the ±2^53 safe range are emitted as BigInt
  ## literals (`123n`) so precision survives the parse — the runtime's `setI64`/
  ## `setU64` coerce with `BigInt(v)`, which accepts a BigInt, but a plain number
  ## literal would already have rounded (e.g. a long-string's 64-bit SSO marker).
  if v > jsMaxSafe or v < -jsMaxSafe: $v & "n" else: $v

proc jsUInt(v: uint64): string =
  if v > uint64(jsMaxSafe): $v & "n" else: $v

proc emitExpr(e: var Emitter; c: var Cursor) =
  case tagOf(c)
  of jName:
    c.into: (e.wr strVal(c); inc c); (while c.hasMore: skip c)
  of jNum:
    c.into: (e.wr jsInt(intVal(c)); inc c); (while c.hasMore: skip c)
  of jUNum:
    c.into: (e.wr jsUInt(uintVal(c)); inc c); (while c.hasMore: skip c)
  of jBigNum:
    c.into: (e.wr($intVal(c) & "n"); inc c); (while c.hasMore: skip c)
  of jBigUNum:
    c.into: (e.wr($uintVal(c) & "n"); inc c); (while c.hasMore: skip c)
  of jStr:
    c.into: (e.wr jsEscape(strVal(c)); inc c); (while c.hasMore: skip c)
  of jRaw:
    c.into: (e.wr strVal(c); inc c); (while c.hasMore: skip c)
  of jUndef:
    c.into:
      e.wr "undefined/*"; e.wr strVal(c); e.wr "*/"; inc c
      while c.hasMore: skip c
  of jBin:
    c.into:
      let op = strVal(c); inc c
      e.wr "("; e.emitExpr c; e.wr " "; e.wr op; e.wr " "; e.emitExpr c; e.wr ")"
      while c.hasMore: skip c
  of jUn:
    c.into:
      let op = strVal(c); inc c
      e.wr "("; e.wr op; e.emitExpr c; e.wr ")"
      while c.hasMore: skip c
  of jIndex:
    c.into:
      e.emitExpr c; e.wr "["; e.emitExpr c; e.wr "]"
      while c.hasMore: skip c
  of jMember:
    c.into:
      e.emitExpr c; e.wr "."; e.wr strVal(c); inc c
      while c.hasMore: skip c
  of jCall:
    c.into:
      e.emitExpr c            # callee
      e.wr "("
      var i = 0
      while c.hasMore:
        if i > 0: e.wr ", "
        e.emitExpr c
        inc i
      e.wr ")"
  of jArray:
    c.into:
      e.wr "["
      var i = 0
      while c.hasMore:
        if i > 0: e.wr ", "
        e.emitExpr c
        inc i
      e.wr "]"
  of jObject:
    c.into:
      e.wr "{"
      var i = 0
      while c.hasMore:            # each child is a jKv
        if i > 0: e.wr ", "
        if tagOf(c) == jKv:
          c.into:
            e.wr strVal(c); inc c   # bare key ident
            e.wr ": "; e.emitExpr c
            while c.hasMore: skip c
        else: skip c
        inc i
      e.wr "}"
  of jCond:
    c.into:
      e.wr "("; e.emitExpr c; e.wr " ? "; e.emitExpr c; e.wr " : "; e.emitExpr c; e.wr ")"
      while c.hasMore: skip c
  of jArrow:
    # Wrap the whole arrow in parens so it is safe as a call callee — `(() => {…})()`
    # (an IIFE), never the illegal `() => {…}()`.
    c.into:
      e.wr "(("
      if tagOf(c) == jParams:
        c.into:
          var i = 0
          while c.hasMore:
            if i > 0: e.wr ", "
            e.emitExpr c; inc i
      else: skip c
      e.wr ") => "
      e.emitBlock c
      e.wr ")"
      while c.hasMore: skip c
  of jNone:
    skip c
  else:
    # A statement tag in expression position shouldn't happen; degrade visibly.
    e.wr "undefined/*jsnif:expr?*/"; skip c

proc emitStmt(e: var Emitter; c: var Cursor) =
  case tagOf(c)
  of jVar, jVarFn:
    let kw = if tagOf(c) == jVarFn: "var " else: "let "
    c.into:
      e.wr kw; e.emitExpr c                # name
      if tagOf(c) != jNone: (e.wr " = "; e.emitExpr c) else: skip c
      e.wr ";"
      while c.hasMore: skip c
  of jAsgn:
    c.into:
      e.emitExpr c; e.wr " = "; e.emitExpr c; e.wr ";"
      while c.hasMore: skip c
  of jExprStmt:
    c.into: (e.emitExpr c; e.wr ";"); (while c.hasMore: skip c)
  of jRet:
    c.into:
      if tagOf(c) != jNone: (e.wr "return "; e.emitExpr c; e.wr ";") else: (skip c; e.wr "return;")
      while c.hasMore: skip c
  of jIf:
    c.into:
      e.wr "if ("; e.emitExpr c; e.wr ") "
      e.emitBlock c
      if tagOf(c) == jBlock:
        e.wr " else "; e.emitBlock c
      elif tagOf(c) == jIf:            # else-if chain
        e.wr " else "; e.emitStmt c
      else: skip c
      while c.hasMore: skip c
  of jWhile:
    c.into:
      e.wr "while ("; e.emitExpr c; e.wr ") "
      e.emitBlock c
      while c.hasMore: skip c
  of jSwitch:
    c.into:
      e.wr "switch ("; e.emitExpr c; e.wr ") {"
      inc e.indent
      while c.hasMore:
        e.nl(); e.emitStmt c        # jCase / jDefault
      dec e.indent
      e.nl(); e.wr "}"
      while c.hasMore: skip c
  of jCase:
    c.into:
      if tagOf(c) == jLabels:     # one or more `case L:` labels
        c.into:
          var first = true
          while c.hasMore:
            if not first: e.nl()
            e.wr "case "; e.emitExpr c; e.wr ":"
            first = false
      else: skip c
      inc e.indent
      e.nl(); e.emitStmt c          # body (a jBlock)
      e.nl(); e.wr "break;"
      dec e.indent
      while c.hasMore: skip c
  of jDefault:
    c.into:
      e.wr "default:"
      inc e.indent
      e.nl(); e.emitStmt c
      e.nl(); e.wr "break;"
      dec e.indent
      while c.hasMore: skip c
  of jBlock:
    e.emitBlock c
  of jLabel:
    c.into:
      e.wr strVal(c); e.wr ": "; inc c    # label name (bare ident)
      e.emitBlock c                        # the labeled block body
      while c.hasMore: skip c
  of jBreak:
    c.into:
      if c.hasMore: (e.wr "break "; e.wr strVal(c); e.wr ";"; inc c)
      else: e.wr "break;"
      while c.hasMore: skip c
  of jContinue:
    e.wr "continue;"; skip c
  of jThrow:
    c.into:
      if tagOf(c) != jNone: (e.wr "throw "; e.emitExpr c; e.wr ";")
      else: (skip c; e.wr "throw new Error();")
      while c.hasMore: skip c
  of jFunc:
    c.into:
      e.wr "function "; e.emitExpr c      # name
      e.wr "("
      if tagOf(c) == jParams:
        c.into:
          var i = 0
          while c.hasMore:
            if i > 0: e.wr ", "
            e.emitExpr c; inc i
      else: skip c
      e.wr ") "
      e.emitBlock c
      while c.hasMore: skip c
  of jRaw:
    c.into: (e.wr strVal(c); inc c); (while c.hasMore: skip c)
  of jNone:
    skip c
  else:
    # An expression tag in statement position: emit as an expression statement.
    e.emitExpr c; e.wr ";"

proc emit*(b: var JsBuilder): string =
  ## Print the accumulated `(module …)` tree to JS text. Top-level statements are
  ## separated by a blank line, matching the per-declaration spacing the codegen
  ## used before.
  var e = Emitter(outp: "", indent: 0)
  var c = beginRead(b.buf)
  if tagOf(c) == jModule:
    c.into:
      while c.hasMore:
        e.nl(); e.emitStmt c      # each top-level decl preceded by a blank line
        e.outp.add "\n"
  endRead c
  result = e.outp

# ── peephole optimizer (jsnif -> jsnif) ───────────────────────────────────────
# A tree pass — the payoff of emitting JS as NIF rather than strings (Araq: "you
# can easily run a peephole optimizer later on the nifjs"). The byte-pointer
# lowering emits lots of `x + 0` (offset-0 fields) and `x * 1` (byte-stride
# arrays / element-size-1 pointers); folding those and constant arithmetic makes
# the output far closer to hand-written JS. Runs bottom-up so nested forms
# collapse (`(s + 0) + (i * 1)` -> `(s + i)`), and shares the input's pools so
# unchanged subtrees are bulk-copied.

proc constVal(b: JsBuilder; c: Cursor): (bool, int64) =
  ## The compile-time integer an expression folds to, resolved *recursively* so a
  ## cascade collapses in one pass — e.g. `0 * 1` is const 0, which makes its
  ## parent `(X + (0*1))` a `X + 0`. `(true, v)` for a `(num v)` or a `(bin op A B)`
  ## with `+`/`-`/`*` whose operands are both constant; `(false, _)` otherwise.
  result = (false, 0'i64)
  case tagOf(c)
  of jNum:
    var cc = c
    cc.into:
      if cc.kind == IntLit: result = (true, intVal(cc))
      while cc.hasMore: skip cc
  of jBin:
    var op = ""
    var a, bb: Cursor
    block:
      var p = c
      p.into:
        op = strVal(p); inc p
        a = p; skip p
        bb = p; skip p
        while p.hasMore: skip p
    let (ao, av) = b.constVal(a)
    let (bo, bv) = b.constVal(bb)
    if ao and bo:
      case op
      of "+": result = (true, av + bv)
      of "-": result = (true, av - bv)
      of "*": result = (true, av * bv)
      else: discard
  else: discard

proc optNode(dst: var JsBuilder; src: JsBuilder; c: var Cursor)

proc optBinFold(dst: var JsBuilder; src: JsBuilder; c: var Cursor): bool =
  ## Try to rewrite a `(bin op A B)` at `c`. On success emits the folded node,
  ## consumes `c`, returns true. Handled: `x+0`/`0+x` -> x, `x*1`/`1*x` -> x,
  ## `x-0` -> x, and constant operands -> `(num a∘b)` for +,-,*. Operand
  ## constness is resolved recursively (`constVal`) so cascades fold in one pass.
  var op = ""
  var aCur, bCur: Cursor
  block:
    var p = c
    p.into:
      op = strVal(p); inc p          # op ident
      aCur = p; skip p               # operand A
      bCur = p; skip p               # operand B
      while p.hasMore: skip p
  let (aNum, aVal) = src.constVal(aCur)
  let (bNum, bVal) = src.constVal(bCur)
  if aNum and bNum:                  # constant fold
    case op
    of "+": dst.num aVal + bVal; skip c; return true
    of "-": dst.num aVal - bVal; skip c; return true
    of "*": dst.num aVal * bVal; skip c; return true
    else: discard
  if (op == "+" or op == "-") and bNum and bVal == 0:
    var a = aCur; dst.optNode(src, a); skip c; return true   # x ± 0 -> x
  if op == "+" and aNum and aVal == 0:
    var b = bCur; dst.optNode(src, b); skip c; return true   # 0 + x -> x
  if op == "*" and bNum and bVal == 1:
    var a = aCur; dst.optNode(src, a); skip c; return true   # x * 1 -> x
  if op == "*" and aNum and aVal == 1:
    var b = bCur; dst.optNode(src, b); skip c; return true   # 1 * x -> x
  false

proc optCallFold(dst: var JsBuilder; src: JsBuilder; c: var Cursor): bool =
  ## `BigInt(<int literal>)` -> a BigInt literal (`52n`), folding the coercion the
  ## 64-bit lowering wraps around small typed literals. On success emits the folded
  ## literal, consumes `c`, returns true.
  var callee, arg: Cursor
  var nargs = 0
  block:
    var p = c
    p.into:
      callee = p; skip p             # callee
      arg = p                        # first argument
      while p.hasMore: (inc nargs; skip p)
  if nargs != 1 or tagOf(callee) != jName: return false
  var nm = ""
  block:
    var cc = callee
    cc.into: (nm = strVal(cc); inc cc); (while cc.hasMore: skip cc)
  if nm != "BigInt": return false
  # A constant argument (a `(num v)` or a fold-to-constant `(bin …)` such as the
  # `53 - 1` shift amounts the lowering produces) becomes a BigInt literal.
  let (isConst, v) = src.constVal(arg)
  if isConst:
    dst.bignum v; skip c; return true
  if tagOf(arg) == jUNum:
    var a = arg
    a.into: (dst.bigunum uintVal(a); inc a); (while a.hasMore: skip a)
    skip c; return true
  false

proc optNode(dst: var JsBuilder; src: JsBuilder; c: var Cursor) =
  ## Copy the node at `c` into `dst`, applying peephole rewrites and recursing so
  ## inner nodes are optimized too. `dst` shares `src`'s pools, so a leaf (and any
  ## subtree with no rewrite in it) is a bulk copy.
  if c.kind != TagLit:
    dst.buf.addSubtree c            # a bare payload token (ident / literal / dot)
    skip c
    return
  if tagOf(c) == jBin and dst.optBinFold(src, c):
    return
  if tagOf(c) == jCall and dst.optCallFold(src, c):
    return
  dst.buf.openTag c.cursorTagId     # same tag id (shared tag pool)
  c.into:
    while c.hasMore:
      dst.optNode(src, c)
  dst.buf.closeTag()

proc peephole*(src: var JsBuilder): JsBuilder =
  ## Return an optimized copy of the whole `src` tree. Sharing `src`'s literal and
  ## tag pools keeps unchanged subtrees a memcpy and lets the result be `emit`ed
  ## directly.
  result = JsBuilder(buf: createTokenBuf(64, sharedPool = src.buf.pool,
                                         sharedTags = src.buf.tags))
  var c = beginRead(src.buf)
  result.optNode(src, c)
  endRead c

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
    jBreak           ## (break)                    -> break;
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

  JsBuilder* = object
    ## Accumulates a jsnif tree. Thin wrapper so callers say `b.tree(jBin): …`
    ## rather than juggling raw tag ids.
    buf*: TokenBuf
    ids: array[JsTag, TagId]

proc createJsTagPool(): (TagPool, array[JsTag, TagId]) =
  ## A private pool with one tag per `JsTag`. Registration is in enum order, so
  ## (pool ids starting at 1) `id(e) == ord(e) + 1`; we still record the ids
  ## rather than assume, so `tagOf` is a lookup, not a cast that could drift.
  var pool = newTagPool()
  var ids: array[JsTag, TagId] = default(array[JsTag, TagId])
  for e in JsTag:
    ids[e] = pool.registerTag($e)
  (pool, ids)

proc initJsBuilder*(): JsBuilder =
  let (pool, ids) = createJsTagPool()
  result = JsBuilder(buf: createTokenBuf(64, sharedTags = pool), ids: ids)

# ── building ────────────────────────────────────────────────────────────────

proc open*(b: var JsBuilder; t: JsTag) {.inline.} = b.buf.openTag b.ids[t]
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

# ── emitting (jsnif -> JS text) ───────────────────────────────────────────────

type Emitter = object
  ids: array[JsTag, TagId]
  outp: string
  indent: int

proc tagOf(e: Emitter; c: Cursor): JsTag =
  ## Decode the JS tag at `c`. Linear over ~25 tags — trivial, and robust against
  ## any pool-id drift a raw ordinal cast would hide.
  if c.kind == TagLit:
    let id = c.cursorTagId
    for t in JsTag:
      if e.ids[t] == id: return t
  jNone

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
  if e.tagOf(c) == jBlock:
    c.into:
      while c.hasMore:
        e.nl(); e.emitStmt c
  else:
    skip c        # jNone or an unexpected node: empty block
  dec e.indent
  e.nl(); e.wr "}"

proc emitExpr(e: var Emitter; c: var Cursor) =
  case e.tagOf(c)
  of jName:
    c.into: (e.wr strVal(c); inc c); (while c.hasMore: skip c)
  of jNum:
    c.into: (e.wr $intVal(c); inc c); (while c.hasMore: skip c)
  of jUNum:
    c.into: (e.wr $uintVal(c); inc c); (while c.hasMore: skip c)
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
        if e.tagOf(c) == jKv:
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
      if e.tagOf(c) == jParams:
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
  case e.tagOf(c)
  of jVar:
    c.into:
      e.wr "let "; e.emitExpr c            # name
      if e.tagOf(c) != jNone: (e.wr " = "; e.emitExpr c) else: skip c
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
      if e.tagOf(c) != jNone: (e.wr "return "; e.emitExpr c; e.wr ";") else: (skip c; e.wr "return;")
      while c.hasMore: skip c
  of jIf:
    c.into:
      e.wr "if ("; e.emitExpr c; e.wr ") "
      e.emitBlock c
      if e.tagOf(c) == jBlock:
        e.wr " else "; e.emitBlock c
      elif e.tagOf(c) == jIf:            # else-if chain
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
      if e.tagOf(c) == jLabels:     # one or more `case L:` labels
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
  of jBreak:
    e.wr "break;"; skip c
  of jContinue:
    e.wr "continue;"; skip c
  of jThrow:
    c.into:
      if e.tagOf(c) != jNone: (e.wr "throw "; e.emitExpr c; e.wr ";")
      else: (skip c; e.wr "throw new Error();")
      while c.hasMore: skip c
  of jFunc:
    c.into:
      e.wr "function "; e.emitExpr c      # name
      e.wr "("
      if e.tagOf(c) == jParams:
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
  var e = Emitter(ids: b.ids, outp: "", indent: 0)
  var c = beginRead(b.buf)
  if e.tagOf(c) == jModule:
    c.into:
      while c.hasMore:
        e.nl(); e.emitStmt c      # each top-level decl preceded by a blank line
        e.outp.add "\n"
  endRead c
  result = e.outp

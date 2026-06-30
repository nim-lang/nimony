#
#
#           Leng Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## JavaScript backend for Leng.
##
## A third codegen over the same Leng IR consumed by the C backend
## (`codegen.nim`) and the LLVM backend (`llvmcodegen.nim`). JavaScript is
## dynamically typed and garbage collected, so this pass drops all type
## generation and maps Leng constructs directly to JS:
##   - `(add T a b)` -> `(a + b)`        (the leading type operand is skipped)
##   - `(asgn x e)`  -> `x = e;`
##   - `(if (elif c (stmts ...)))` -> `if (c) { ... }`
##   - `(try ...)` / `(raise e)` -> native `try`/`throw`
##
## This is the M0/M1 pure-compute slice: procs, locals/globals, assignment,
## calls, returns, arithmetic/bitwise/comparison/logic operators, `if`,
## `while`, `case`, `break`. Constructs outside this subset emit a
## `/*TODO:<tag>*/` marker and are skipped, so generation always completes and
## the coverage gap is visible in the output.

import std / [assertions, syncio, strutils, formatfloat]

import ".." / lib / nifcoreparse   # re-exports nifcore (Cursor, beginRead, intVal, ...)
import ".." / lib / nifcdecl        # stmtKind/exprKind/substructureKind + Leng enums + decl extractors
import mangler
import noptions
import nifmodules                   # MainModule + load
from ".." / lib / vfs import vfsExists, vfsRead, vfsWrite
from std / os import extractFilename

type
  JSGenFlag* = enum
    gfMainModule

  JSGen = object
    m: MainModule
    code: string
    indent: int
    flags: set[JSGenFlag]
    todos: int

proc initJSGen(m: sink MainModule; flags: set[JSGenFlag]): JSGen =
  JSGen(m: m, code: "", indent: 0, flags: flags)

# ── low-level emit helpers ───────────────────────────────────────────────────

proc wr(g: var JSGen; s: string) {.inline.} = g.code.add s

proc nl(g: var JSGen) =
  g.code.add "\n"
  for _ in 0 ..< g.indent: g.code.add "  "

proc name(g: JSGen; symId: SymId): string {.inline.} =
  mangleToC(g.m.pool.syms[symId])

proc jsString(s: string): string =
  ## minimal JS string-literal escaping.
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

proc todo(g: var JSGen; what: string; n: var Cursor) =
  g.wr "/*TODO:" & what & "*/"
  inc g.todos
  skip n

# ── expressions ──────────────────────────────────────────────────────────────

proc gx(g: var JSGen; n: var Cursor)

proc binTyped(g: var JSGen; n: var Cursor; opr: string) =
  ## `(op TYPE lhs rhs)` -> `(lhs <opr> rhs)`; the type operand is irrelevant in JS.
  n.into:
    skip n            # result type
    g.wr "("
    gx g, n
    g.wr opr
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc intDiv(g: var JSGen; n: var Cursor) =
  ## Nim integer `div` truncates toward zero; JS `/` is float division.
  n.into:
    skip n            # result type
    g.wr "Math.trunc("
    gx g, n
    g.wr " / "
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc binPlain(g: var JSGen; n: var Cursor; opr: string) =
  ## `(op lhs rhs)` (comparisons / logic): no type operand.
  n.into:
    g.wr "("
    gx g, n
    g.wr opr
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc unTyped(g: var JSGen; n: var Cursor; opr: string) =
  n.into:
    skip n            # type
    g.wr "("
    g.wr opr
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc unPlain(g: var JSGen; n: var Cursor; opr: string) =
  n.into:
    g.wr "("
    g.wr opr
    gx g, n
    g.wr ")"
    while n.hasMore: skip n

proc genCall(g: var JSGen; n: var Cursor) =
  n.into:
    gx g, n           # callee
    g.wr "("
    var i = 0
    while n.hasMore:
      if i > 0: g.wr ", "
      gx g, n
      inc i
    g.wr ")"

proc gx(g: var JSGen; n: var Cursor) =
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit:
      g.wr $intVal(n); inc n
    of UIntLit:
      g.wr $uintVal(n); inc n
    of FloatLit:
      g.wr $floatVal(n); inc n
    of CharLit:
      g.wr $ord(n.charLit); inc n
    of StrLit:
      g.wr jsString(g.m.pool.strings[strId(n)]); inc n
    of Symbol:
      g.wr g.name(n.symId); inc n
    of DotToken:
      g.wr "undefined"; inc n
    else:
      g.todo("expr:" & $n.kind, n)
  of TrueC: g.wr "true"; skip n
  of FalseC: g.wr "false"; skip n
  of NilC: g.wr "null"; skip n
  of InfC: g.wr "Infinity"; skip n
  of NegInfC: g.wr "(-Infinity)"; skip n
  of NanC: g.wr "NaN"; skip n
  of CallC: genCall g, n
  of AddC: binTyped g, n, " + "
  of SubC: binTyped g, n, " - "
  of MulC: binTyped g, n, " * "
  of DivC: intDiv g, n
  of ModC: binTyped g, n, " % "
  of ShlC: binTyped g, n, " << "
  of ShrC: binTyped g, n, " >> "
  of BitandC: binTyped g, n, " & "
  of BitorC: binTyped g, n, " | "
  of BitxorC: binTyped g, n, " ^ "
  of BitnotC: unTyped g, n, "~"
  of NegC: unTyped g, n, "-"
  of AndC: binPlain g, n, " && "
  of OrC: binPlain g, n, " || "
  of NotC: unPlain g, n, "!"
  of EqC: binPlain g, n, " === "
  of NeqC: binPlain g, n, " !== "
  of LeC: binPlain g, n, " <= "
  of LtC: binPlain g, n, " < "
  of CastC, ConvC:
    # JS is untyped: a conversion/cast is the inner value (skip the type).
    n.into:
      skip n
      gx g, n
      while n.hasMore: skip n
  of ParC:
    n.into:
      g.wr "("
      gx g, n
      g.wr ")"
      while n.hasMore: skip n
  of SufC:
    # literal-with-suffix: emit the value, drop the suffix annotation.
    n.into:
      gx g, n
      while n.hasMore: skip n
  of OconstrC:
    # `(oconstr Type (kv field value [inheritance]) …)` -> a JS object literal
    # `{field: value, …}`. The field key is the mangled field-symbol name so it
    # matches dot access. Lowered strings/seqs are objects at this level, so
    # this is the faithful, type-system-neutral mapping.
    n.into:
      skip n            # object type
      g.wr "{"
      var i = 0
      while n.hasMore:
        if n.substructureKind == KvU:
          if i > 0: g.wr ", "
          n.into:
            g.wr g.name(n.symId); inc n   # field name (Symbol) as key
            g.wr ": "
            g.gx n                         # value
            while n.hasMore: skip n        # optional inheritance depth
          inc i
        else:
          skip n
      g.wr "}"
  of AconstrC:
    # `(aconstr Type elem0 elem1 …)` -> a JS array literal `[elem0, elem1, …]`.
    n.into:
      skip n            # element/array type
      g.wr "["
      var i = 0
      while n.hasMore:
        if i > 0: g.wr ", "
        g.gx n
        inc i
      g.wr "]"
  of AddrC, DerefC:
    # JS values are references and there are no raw pointers: taking the address
    # of, or dereferencing, an object is the identity. `(addr x)`/`(deref x)`
    # therefore emit just `x`.
    n.into:
      g.gx n
      while n.hasMore: skip n
  of DotC:
    # `(dot obj field [inheritance-depth] [access-token])` -> `obj.field`. The
    # field key is the mangled field-symbol name, matching `oconstr` above.
    n.into:
      g.gx n            # object
      g.wr "."
      g.wr g.name(n.symId); inc n   # field name (Symbol)
      while n.hasMore: skip n        # inheritance depth / access token
  of AtC, PatC:
    # array / pointer indexing -> `arr[idx]`.
    n.into:
      g.gx n            # array / pointer
      g.wr "["
      g.gx n            # index
      g.wr "]"
      while n.hasMore: skip n
  else:
    g.todo("expr:" & $n.exprKind, n)

# ── statements ───────────────────────────────────────────────────────────────

proc gs(g: var JSGen; n: var Cursor)

proc genBlock(g: var JSGen; n: var Cursor) =
  ## emit a `(stmts ...)` body as a brace-delimited JS block.
  g.wr "{"
  inc g.indent
  if n.stmtKind in {StmtsS, ScopeS}:
    n.loopInto:
      g.gs n
  else:
    g.gs n
  dec g.indent
  g.nl()
  g.wr "}"

proc genVar(g: var JSGen; n: var Cursor) =
  var d = takeVarDecl(n)
  g.nl()
  g.wr "let " & g.name(d.name.symId)
  if d.value.kind != DotToken:
    g.wr " = "
    var v = d.value
    g.gx v
  g.wr ";"

proc genIf(g: var JSGen; n: var Cursor) =
  var first = true
  n.loopInto:
    case n.substructureKind
    of ElifU:
      g.nl()
      g.wr (if first: "if (" else: "else if (")
      n.into:
        g.gx n          # condition
        g.wr ") "
        g.genBlock n    # body
        while n.hasMore: skip n
      first = false
    of ElseU:
      g.nl()
      g.wr "else "
      n.into:
        g.genBlock n
        while n.hasMore: skip n
    else:
      g.todo("if-branch", n)

proc genWhile(g: var JSGen; n: var Cursor) =
  n.into:
    g.nl()
    g.wr "while ("
    g.gx n              # condition
    g.wr ") "
    g.genBlock n        # body
    while n.hasMore: skip n

proc genCase(g: var JSGen; n: var Cursor) =
  n.into:
    g.nl()
    g.wr "switch ("
    g.gx n              # selector
    g.wr ") {"
    inc g.indent
    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:
          # (of (ranges v1 v2 ...) body)
          if n.substructureKind == RangesU:
            n.loopInto:
              g.nl(); g.wr "case "; g.gx n; g.wr ":"
          else:
            g.nl(); g.wr "case "; g.gx n; g.wr ":"
          inc g.indent
          g.nl(); g.genBlock n
          g.nl(); g.wr "break;"
          dec g.indent
          while n.hasMore: skip n
      of ElseU:
        n.into:
          g.nl(); g.wr "default:"
          inc g.indent
          g.nl(); g.genBlock n
          g.nl(); g.wr "break;"
          dec g.indent
          while n.hasMore: skip n
      else:
        g.todo("case-branch", n)
    dec g.indent
    g.nl(); g.wr "}"

proc gs(g: var JSGen; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.kind == DotToken: inc n
    else: g.todo("stmt:" & $n.kind, n)
  of StmtsS:
    n.loopInto: g.gs n
  of ScopeS:
    g.nl(); g.genBlock n
  of VarS, GvarS, TvarS, ConstS:
    g.genVar n
  of AsgnS, StoreS:
    n.into:
      g.nl()
      g.gx n            # lvalue
      g.wr " = "
      g.gx n            # value
      g.wr ";"
      while n.hasMore: skip n
  of CallS:
    g.nl()
    g.genCall n
    g.wr ";"
  of RetS:
    g.nl()
    n.into:
      if n.kind != DotToken:
        g.wr "return "
        g.gx n
        g.wr ";"
      else:
        g.wr "return;"; inc n
      while n.hasMore: skip n
  of DiscardS:
    n.into:
      g.nl()
      g.gx n
      g.wr ";"
      while n.hasMore: skip n
  of IfS: g.genIf n
  of WhileS: g.genWhile n
  of CaseS: g.genCase n
  of BreakS:
    g.nl(); g.wr "break;"
    skip n
  of LabS:
    # A goto-target label. Vestigial under structured control flow (no matching
    # `jmp`); JS has no `goto`, so emit nothing. A real `jmp` still surfaces as
    # an unsupported node, making any genuine goto use visible.
    skip n
  of RaiseS:
    g.nl()
    n.into:
      if n.kind != DotToken:
        g.wr "throw "
        g.gx n
        g.wr ";"
      else:
        g.wr "throw new Error();"; inc n
      while n.hasMore: skip n
  else:
    g.nl(); g.todo("stmt:" & $n.stmtKind, n)

# ── declarations / module ────────────────────────────────────────────────────

proc genProc(g: var JSGen; n: var Cursor) =
  var prc = takeProcDecl(n)
  g.nl()
  g.wr "function " & g.name(prc.name.symId) & "("
  if prc.params.kind != DotToken:
    var p = prc.params
    var i = 0
    p.loopInto:
      var d = takeParamDecl(p)
      if i > 0: g.wr ", "
      g.wr g.name(d.name.symId)
      inc i
  g.wr ") "
  g.genBlock prc.body
  g.nl()

proc genToplevel(g: var JSGen; n: var Cursor) =
  case n.stmtKind
  of ProcS: g.genProc n
  of VarS, GvarS, TvarS, ConstS: g.genVar n
  of TypeS: skip n               # no types in JS
  of EmitS:
    # raw passthrough: string literals are emitted verbatim (JS injection).
    g.nl()
    n.loopInto:
      if n.kind == StrLit:
        g.wr g.m.pool.strings[strId(n)]; inc n
      else:
        g.gx n
  of StmtsS:
    n.loopInto: g.genToplevel n
  of CallS, AsgnS, StoreS, IfS, WhileS, CaseS, DiscardS, ScopeS:
    g.gs n
  else:
    g.nl(); g.todo("toplevel:" & $n.stmtKind, n)

proc generateJSCode*(s: var State; inp, outp: string; flags: set[JSGenFlag]) =
  var m = load(inp)
  m.config = s.config
  var g = initJSGen(m, flags)
  g.wr "// generated by lengc (js backend) from " & extractFilename(inp) & "\n"
  g.wr "\"use strict\";\n"
  var n = beginRead(g.m.src)
  if n.stmtKind == StmtsS:
    n.loopInto: g.genToplevel n
  else:
    g.todo("root", n)
  g.code.add "\n"
  if g.todos > 0:
    stdout.writeLine "[lengc js] " & inp & ": " & $g.todos & " unsupported node(s) emitted as /*TODO*/"
  if vfsExists(outp) and vfsRead(outp) == g.code:
    discard "unchanged"
  else:
    vfsWrite outp, g.code

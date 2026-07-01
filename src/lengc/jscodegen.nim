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
## **Addresses.** JS has no way to reference a variable's storage, so a pointer
## cannot be the value itself. Following the classic Nim JS backend (`mapType`'s
## `etyBaseIndex` "fat pointers"), a pointer is a `[base, key]` pair such that
## `base[key]` is the pointee's storage:
##   - a local whose address is taken is boxed into a 1-element array
##     (`let x = [init];`, every use of `x` becomes `x[0]`); its address is
##     `[x, 0]`,
##   - `(addr o.f)` becomes `[o, "f"]`, `(addr a[i])` becomes `[a, i]`,
##   - `(deref p)` becomes `p[0][p[1]]`,
##   - the pairs `(addr (deref p))` and `(deref (addr loc))` cancel, so the
##     common cases stay compact.
## A pre-pass (`scanForAddr`) finds the locals that need boxing. Writes through a
## pointer therefore mutate the underlying storage, which the earlier
## "addr/deref are identity" mapping got wrong. Two fat pointers are equal iff
## their components are (`nimPtrEq`), since a fresh `[base,key]` is never `===`
## another array.
##
## This is the M0/M1 pure-compute slice: procs, locals/globals, assignment,
## calls, returns, arithmetic/bitwise/comparison/logic operators, `if`,
## `while`, `case`, `break`. Constructs outside this subset emit a
## `/*TODO:<tag>*/` marker and are skipped, so generation always completes and
## the coverage gap is visible in the output.

import std / [assertions, syncio, strutils, formatfloat, sets]

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
    boxed: HashSet[SymId]   ## locals whose address is taken (see `scanForAddr`)
    usesPtrEq: bool         ## whether the module compares fat pointers (needs the helper)

proc initJSGen(m: sink MainModule; flags: set[JSGenFlag]): JSGen =
  JSGen(m: m, code: "", indent: 0, flags: flags, boxed: initHashSet[SymId]())

const ptrEqHelper =
  "function nimPtrEq(a, b) { return a[0] === b[0] && a[1] === b[1]; }\n"

# ── low-level emit helpers ───────────────────────────────────────────────────

proc wr(g: var JSGen; s: string) {.inline.} = g.code.add s

proc nl(g: var JSGen) =
  g.code.add "\n"
  for _ in 0 ..< g.indent: g.code.add "  "

proc name(g: var JSGen; symId: SymId): string =
  ## The JS identifier for a symbol. `importc`/`exportc` symbols use their
  ## external (C) name — resolved cross-module via the lazily-loaded foreign
  ## declarations, exactly as the C backend's `mangleSym` does — so a call into
  ## another module's `importc` proc/global lands on the runtime name (e.g.
  ## `stdout`, `fwrite`) rather than a mangled stub. Everything else mangles.
  let d = g.m.getDeclOrNil(symId)
  if d != nil and d.extern != StrId(0):
    result = g.m.pool.strings[d.extern]
  else:
    result = mangleToC(g.m.pool.syms[symId])

proc isImportc(g: var JSGen; symId: SymId): bool =
  ## True for `importc`/`importcpp` symbols: they name external entities, so the
  ## JS backend references them but emits no definition (a runtime provides them).
  let d = g.m.getDeclOrNil(symId)
  result = d != nil and d.isImport

proc fieldName(g: JSGen; symId: SymId): string {.inline.} =
  ## Object-field key: always the mangled field name (matching the C backend),
  ## never an extern name — `name`'s foreign-decl lookup is for top-level symbols.
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
  ## Placeholder for an unsupported node. It is a valid JS expression
  ## (`undefined` with a tagging comment), so the output always *parses* even
  ## where an expression is required — e.g. as a call argument — and a single
  ## unsupported node never breaks the surrounding (possibly never-called)
  ## function. The gap stays visible and is counted in `g.todos`.
  g.wr "undefined/*TODO:" & what & "*/"
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

proc captureExpr(g: var JSGen; n: var Cursor): string =
  ## Generate the expression at `n` into a string instead of the main buffer,
  ## advancing the cursor. Used where an emitted operand must be referenced more
  ## than once (a fat-pointer deref `p[0][p[1]]`, pointer-equality components).
  let start = g.code.len
  gx g, n
  result = g.code[start ..< g.code.len]
  g.code.setLen start

proc genAddrOf(g: var JSGen; n: var Cursor) =
  ## Emit `(addr LOC)` as a fat pointer `[base, key]` so that `base[key]` is
  ## LOC's storage. Locals are boxed (`[value]`), so a local's address is
  ## `[x, 0]`; a field's is `[obj, "field"]`; an element's is `[arr, idx]`.
  ## `(addr (deref p))` cancels to the pointer `p`. `n` is the location operand.
  case n.exprKind
  of DerefC:
    n.into:
      gx g, n
      while n.hasMore: skip n
  of DotC:
    n.into:
      let base = g.captureExpr n
      let key = g.fieldName(n.symId); inc n   # field symbol
      g.wr "[" & base & ", " & jsString(key) & "]"
      while n.hasMore: skip n
  of AtC:
    n.into:
      let base = g.captureExpr n
      let idx = g.captureExpr n
      g.wr "[" & base & ", " & idx & "]"
      while n.hasMore: skip n
  of NoExpr:
    if n.kind == Symbol:
      # a boxed local: the box is the storage, indexed at slot 0.
      g.wr "[" & g.name(n.symId) & ", 0]"; inc n
    else:
      g.todo("addr-of-location", n)
  else:
    # `addr (pat p i)` (pointer arithmetic) and other forms need more than a
    # single base/key pair; out of scope for this slice.
    g.todo("addr-of-location", n)

proc genDeref(g: var JSGen; n: var Cursor) =
  ## `(deref p)` reads through a fat pointer: `p[0][p[1]]`. When `p` is itself
  ## `(addr LOC)` the two cancel and LOC's storage is accessed directly. `n` is
  ## the pointer operand.
  if n.exprKind == AddrC:
    n.into:
      gx g, n   # read LOC directly: boxed sym -> x[0], field -> o.f, elem -> a[i]
      while n.hasMore: skip n
  else:
    let p = g.captureExpr n
    g.wr p & "[0][" & p & "[1]]"

proc genEq(g: var JSGen; n: var Cursor; negate: bool) =
  ## `(eq a b)` / `(neq a b)`. When either operand is an address, the operands
  ## are fat pointers and must be compared component-wise (a fresh `[base,key]`
  ## is never `===` another array); otherwise a plain strict comparison, which
  ## is also correct for `p == nil` (a nil pointer is `null`).
  n.into:
    let aAddr = n.exprKind == AddrC
    let a = g.captureExpr n
    let bAddr = n.exprKind == AddrC
    let b = g.captureExpr n
    if aAddr or bAddr:
      g.usesPtrEq = true
      if negate: g.wr "(!nimPtrEq(" & a & ", " & b & "))"
      else: g.wr "nimPtrEq(" & a & ", " & b & ")"
    else:
      g.wr "(" & a & (if negate: " !== " else: " === ") & b & ")"
    while n.hasMore: skip n

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
      # a boxed local lives in a 1-element array; read it through slot 0.
      if n.symId in g.boxed: g.wr g.name(n.symId) & "[0]"
      else: g.wr g.name(n.symId)
      inc n
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
  of OvfC:
    # The overflow flag (see `KeepovfS`). JS numbers are doubles with no 64-bit
    # overflow trap, so nothing sets it — a checked op never reports overflow.
    g.wr "false"; skip n
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
  of EqC: genEq g, n, false
  of NeqC: genEq g, n, true
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
            g.wr g.fieldName(n.symId); inc n   # field name (Symbol) as key
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
  of AddrC:
    # `(addr LOC)` -> a fat pointer `[base, key]` (see `genAddrOf`).
    n.into:
      g.genAddrOf n
      while n.hasMore: skip n
  of DerefC:
    # `(deref p)` -> `p[0][p[1]]` (see `genDeref`).
    n.into:
      g.genDeref n
      while n.hasMore: skip n
  of DotC:
    # `(dot obj field [inheritance-depth] [access-token])` -> `obj.field`. The
    # field key is the mangled field-symbol name, matching `oconstr` above.
    n.into:
      g.gx n            # object
      g.wr "."
      g.wr g.fieldName(n.symId); inc n   # field name (Symbol)
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
  if g.isImportc(d.name.symId): return   # external global: provided by the runtime
  g.nl()
  let nm = g.name(d.name.symId)
  if d.name.symId in g.boxed:
    # boxed local: store the value in a 1-element array so its address (the
    # array) is a stable pointer; all reads/writes go through slot 0.
    g.wr "let " & nm & " = ["
    if d.value.kind != DotToken:
      var v = d.value
      g.gx v
    g.wr "];"
  else:
    g.wr "let " & nm
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
  of AsgnS:
    n.into:
      g.nl()
      g.gx n            # lvalue
      g.wr " = "
      g.gx n            # value
      g.wr ";"
      while n.hasMore: skip n
  of StoreS:
    # `(store value lvalue)` — operands are reversed relative to `asgn`.
    n.into:
      let value = g.captureExpr n
      g.nl()
      g.gx n            # lvalue
      g.wr " = " & value & ";"
      while n.hasMore: skip n
  of KeepovfS:
    # `(keepovf (op TYPE lhs rhs) dest)` computes the arithmetic, stores it into
    # `dest`, and would set the overflow flag on overflow. JS has no 64-bit
    # overflow trap, so this reduces to the plain store; `(ovf)` stays `false`.
    n.into:
      let arith = g.captureExpr n
      g.nl()
      g.gx n            # dest lvalue
      g.wr " = " & arith & ";"
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
  if g.isImportc(prc.name.symId): return   # external proc: provided by the runtime
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
  g.wr ") {"
  inc g.indent
  # Prologue: a value parameter whose address is taken must be boxed too, so a
  # pointer to it is a live 1-element array. Re-bind it to its box at entry; all
  # uses inside the body then read through slot 0 (see `gx`). A `var`/pointer
  # parameter already arrives as a box from the caller, so it is never boxed.
  if prc.params.kind != DotToken:
    var p = prc.params
    p.loopInto:
      var d = takeParamDecl(p)
      if d.name.symId in g.boxed:
        let nm = g.name(d.name.symId)
        g.nl(); g.wr nm & " = [" & nm & "];"
  var body = prc.body
  if body.stmtKind in {StmtsS, ScopeS}:
    body.loopInto: g.gs body
  else:
    g.gs body
  dec g.indent
  g.nl()
  g.wr "}"
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

proc scanForAddr(g: var JSGen; n: var Cursor) =
  ## Pre-pass over the whole module: record every local that is the direct
  ## operand of `(addr <symbol>)`. Symbols are module-unique, so a single set
  ## drives boxing for both the declaration and every use.
  if n.kind == TagLit:
    if n.exprKind == AddrC:
      n.into:
        if n.hasMore and n.kind == Symbol:
          g.boxed.incl n.symId
        while n.hasMore: g.scanForAddr n
    else:
      n.loopInto: g.scanForAddr n
  else:
    inc n

proc generateJSCode*(s: var State; inp, outp: string; flags: set[JSGenFlag]) =
  var m = load(inp)
  m.config = s.config
  var g = initJSGen(m, flags)
  var probe = beginRead(g.m.src)
  g.scanForAddr probe
  # The body is generated into `g.code`; the header and any runtime helpers are
  # prepended afterwards, since whether the module needs `nimPtrEq` is only known
  # once generation has run.
  var n = beginRead(g.m.src)
  if n.stmtKind == StmtsS:
    n.loopInto: g.genToplevel n
  else:
    g.todo("root", n)
  g.code.add "\n"
  var output = "// generated by lengc (js backend) from " & extractFilename(inp) & "\n"
  output.add "\"use strict\";\n"
  if g.usesPtrEq: output.add ptrEqHelper
  output.add g.code
  if g.todos > 0:
    stdout.writeLine "[lengc js] " & inp & ": " & $g.todos & " unsupported node(s) emitted as /*TODO*/"
  if vfsExists(outp) and vfsRead(outp) == output:
    discard "unchanged"
  else:
    vfsWrite outp, output

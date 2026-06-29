# JavaScript backend (lengc `js`)

A third codegen over the **Leng** IR, alongside the C (`codegen.nim`) and LLVM
(`llvmcodegen.nim`) backends. Selected with `lengc js file.c.nif`; output is a
`.js` file in the nimcache dir.

JavaScript is dynamically typed and garbage collected, so the pass drops all
type generation and maps Leng constructs directly to JS:

| Leng | JavaScript |
|------|------------|
| `(proc :f (params (param :a . T)) RET . (stmts …))` | `function f(a) { … }` |
| `(add T a b)` (typed binop) | `(a + b)` — leading type operand skipped |
| `(div T a b)` | `Math.trunc(a / b)` (Nim int `div` truncates toward zero) |
| `(lt a b)`, `(and a b)`, `(not a)` | `(a < b)`, `(a && b)`, `(!a)` |
| `(asgn x e)` / `(store e x)` | `x = e;` |
| `(if (elif c (stmts …)) (else (stmts …)))` | `if (c) { … } else { … }` |
| `(while c (stmts …))` | `while (c) { … }` |
| `(case sel (of (ranges v) …) (else …))` | `switch (sel) { case v: … }` |
| `(raise e)` / `(try …)` | `throw e;` / `try { … }` (native) |
| `(ret e)` | `return e;` |

## Scope

This is the **M0/M1 pure-compute slice**: procs, locals/globals, assignment,
calls, returns, integer/float/bool/string literals, the arithmetic / bitwise /
comparison / logic operators, `if`, `while`, `case`, `break`. Anything outside
the subset (objects, seqs, strings as values, pointers, the GC runtime) emits a
`/*TODO:<tag>*/` marker and is skipped, so generation always completes and the
coverage gap is visible in the output and reported on stderr.

Memory management and the runtime (strings/seqs, `echo`) are **M2+** and depend
on the open design question of whether JS consumes fully-lowered Leng or a
higher-level Leng with the destructor passes gated off for the GC target.

## Test

`bash tests/jsbackend/run.sh` — golden-diffs `tcompute.c.nif` → JS against
`tcompute.expected.js`, then runs the result under Node and checks
`add(2,3)==5`, `fib(10)==55`, `fib(20)==6765`. `tcompute.c.nif` is a
hand-authored, self-contained Leng module (no system deps).

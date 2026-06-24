| Tag                    | Enums                       |   Description |
|------------------------|-----------------------------|---------------|
| `(err ...)`            | NimonyExpr, NimonyType, NiflerKind                    | indicates an error |
| `(suf LIT STR)`        | LengExpr, NimonyExpr, NiflerKind                     | literal with suffix annotation |
| `(at T X X); (at X X); (at Y T+)` | LengExpr, NimonyExpr, NimonyType, NiflerKind | array indexing operation (typed Nimony form vs untyped Leng form); also used for generic proc/type instantiation `(at callee T1 T2 ...)` |
| `(deref X)`; `(deref X (cppref)?)`            | LengExpr, NimonyExpr, NiflerKind | pointer deref operation |
| `(dot X Y INTLIT? STRLIT?)` | LengExpr, NimonyExpr, NiflerKind | object field selection; optional integer is the inheritance depth of the field; optional trailing `STRLIT` is an *access token* (carrying `"x"` like an export marker) — when present, the expression was already type-checked in a scope with access to the field, so re-check at expansion/serialization sites must accept the access even if the field is private. Emitted by sem when a template body or `.semantics` serializer is type-checked in the field's defining module and later expanded/consumed elsewhere. |
| `(pat X X)`            | LengExpr, NimonyExpr | pointer indexing operation |
| `(par X)`              | LengExpr, NimonyExpr, NiflerKind | syntactic parenthesis |
| `(addr X)`; `(addr X (cppref)?)`  | LengExpr, NimonyExpr, NiflerKind | address of operation |
| `(nil T? X?)`          | LengExpr, NimonyExpr, NimonyOther, NiflerKind | nil pointer value; closure `nil` carries the proc type and a nil environment |
| `(notnil)`             | NimonyOther | `not nil` pointer annotation |
| `(unchecked)`          | NimonyOther | `unchecked` pointer annotation (derefs do not require nil checking) |
| `(inf T?)`             | LengExpr, NimonyExpr | positive infinity floating point value |
| `(neginf T?)`          | LengExpr, NimonyExpr | negative infinity floating point value |
| `(nan T?)`             | LengExpr, NimonyExpr | NaN floating point value |
| `(false)`              | LengExpr, NimonyExpr | boolean `false` value |
| `(true)`               | LengExpr, NimonyExpr | boolean `true` value |
| `(and X X)`; `(and Y Y+)` | LengExpr, NimonyExpr, NimonyType | boolean `and` operation; `Y+` form is also used for concept parent lists with more than two parents |
| `(or X X)`             | LengExpr, NimonyExpr, NimonyType | boolean `or` operation |
| `(xor X X)`            | NimonyExpr | boolean `xor` operation |
| `(not X)`              | LengExpr, NimonyExpr, NimonyType | boolean `not` operation |
| `(neg T X); (neg X)`   | LengExpr, NimonyExpr | negation operation |
| `(sizeof T)`           | LengExpr, NimonyExpr | `sizeof` operation |
| `(alignof T)`          | LengExpr, NimonyExpr | `alignof` operation |
| `(offsetof T Y)`       | LengExpr, NimonyExpr | `offsetof` operation |
| `(oconstr T (kv Y X)*)`; `(oconstr T (oconstr...)? (kv Y X*))` | LengExpr, NimonyExpr, NiflerKind | object constructor |
| `(aconstr T X*)`       | LengExpr, NimonyExpr | array constructor |
| `(bracket X*)`         | NimonyExpr, NiflerKind | untyped array constructor |
| `(curly X*)`           | NimonyExpr, NiflerKind | untyped set constructor |
| `(curlyat X X)`        | NimonyExpr, NiflerKind | curly expression `a{i}` |
| `(kv Y X INTLIT?)`     | NimonyExpr, NimonyOther, LengOther, NiflerKind, NifIndexKind | key-value pair; optional INTLIT indicates field is in an inherited object |
| `(vv X X)`             | NimonyOther, NiflerKind, NifIndexKind | value-value pair (used for explicitly named arguments in function calls) |
| `(ovf)`                | NimonyExpr, LengExpr | access overflow flag |
| `(add T X X)`          | LengExpr, NimonyExpr | |
| `(sub T X X)`          | LengExpr, NimonyExpr | |
| `(mul T X X)`          | LengExpr, NimonyExpr | |
| `(div T X X)`          | LengExpr, NimonyExpr | |
| `(mod T X X)`          | LengExpr, NimonyExpr | |
| `(shr T X X)`          | LengExpr, NimonyExpr | |
| `(shl T X X)`          | LengExpr, NimonyExpr | |
| `(bitand T X X)`       | LengExpr, NimonyExpr | |
| `(bitor T X X)`        | LengExpr, NimonyExpr | |
| `(bitxor T X X)`       | LengExpr, NimonyExpr | |
| `(bitnot T X)`         | LengExpr, NimonyExpr | |
| `(eq T X X)`           | LengExpr, NimonyExpr | |
| `(neq T X X)`          | LengExpr, NimonyExpr | |
| `(le T X X)`           | LengExpr, NimonyExpr | |
| `(lt T X X)`           | LengExpr, NimonyExpr | |
| `(cast T X); (cast (pragmas ...))` | LengExpr, NimonyExpr, NimonyPragma, NiflerKind | `cast` operation (typed cast expression, or `{.cast(pragma).}` pragma form) |
| `(conv T X)`           | LengExpr, NimonyExpr | type conversion |
| `(call X X*)`          | LengExpr, NimonyExpr, LengStmt, NimonyStmt, NiflerKind | call operation |
| `(cmd X X*)`             | NimonyStmt, NimonyExpr, NiflerKind | command operation |
| `(range X X)`          | LengOther, NimonyOther | `(range a b)` construct |
| `(ranges (range ...)*)` | LengOther, NimonyOther, NiflerKind | |
| `(gvar D E P T .X)`; `(gvar D P T .X)` | LengStmt, NimonyStmt, NimonySym, LengSym, NifIndexKind | global variable declaration |
| `(tvar D E P T .X)`; `(tvar D P T .X)` | LengStmt, NimonyStmt, NimonySym, LengSym, NifIndexKind | thread local variable declaration |
| `(var D E P .T .X)`; `(var D P T .X)` | LengStmt, NimonyStmt, NimonySym, LengSym, NiflerKind, NifIndexKind | variable declaration; type slot may be omitted when inferred from initializer |
| `(param D E P T .X)`; `(param D P T)` | LengOther, NimonyOther, NimonySym, LengSym, NiflerKind | parameter declaration |
| `(const D E P T .X)`; `(const D P T)` | LengStmt, NimonyStmt, NimonySym, LengSym, NiflerKind, NifIndexKind | const variable declaration |
| `(result D E P T .X)` | NimonySym, NimonyStmt | result variable declaration |
| `(glet D E P T .X)` | NimonyStmt, NimonySym, NifIndexKind | global let variable declaration |
| `(tlet D E P T .X)` | NimonyStmt, NimonySym, NifIndexKind | thread local let variable declaration |
| `(let D E P .T .X)` | NimonySym, NimonyStmt, NiflerKind, NifIndexKind | let variable declaration; type is optional when used in `(unpackflat …)` |
| `(cursor D E P T .X)` | NimonySym, NimonyStmt, NimonyPragma, NifIndexKind | cursor variable declaration |
| `(patternvar D E P T .X)` | NimonySym, NimonyStmt | pattern variable declaration |
| `(typevar D E P .T .X)` | NimonySym, LengOther, NimonyOther, NiflerKind | type variable declaration; constraint `.T` is optional |
| `(efld D .X P T .X)`; `(efld D X)` | NimonySym, LengSym, LengOther, NimonyOther, NiflerKind | enum field declaration; slot 2 carries the export marker *or* the compile-time value (may be `.`) |
| `(fld D E P T .X)`; `(fld D P T)` | LengOther, NimonyOther, NimonySym, LengSym, NiflerKind | field declaration |
| `(gfld D E P T .X)` | NimonySym, NimonyOther | guarded field declaration, cannot be accessed outside an `of` branch |
| `(proc D ...)` | LengStmt, NimonyStmt, NimonySym, NimonyType, LengSym, NiflerKind, NifIndexKind | proc declaration |
| `(func D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | function declaration |
| `(iterator D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | iterator declaration |
| `(converter D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | converter declaration |
| `(method D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | method declaration |
| `(macro D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | macro declaration |
| `(template D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | template declaration |
| `(type D ...)` | LengStmt, NimonyStmt, NimonySym, LengSym, NiflerKind, NifIndexKind | type declaration |
| `(block .D X)` | NimonyStmt, NimonySym, NiflerKind | block declaration |
| `(module)` | NimonySym | module declaration |
| `(cchoice X X*)` | NimonyExpr, NimonySym | closed choice |
| `(ochoice X X*)`| NimonyExpr | open choice |
| `(emit X*)` | LengStmt, NimonyStmt, NimonyPragma | emit statement |
| `(asgn X X)` | LengStmt, NimonyStmt, NiflerKind | assignment statement |
| `(store X X)` | NjvlKind, LengStmt | `asgn` with reversed operands that reflects evaluation order |
| `(keepovf X X)` | LengStmt | keep overflow flag statement |
| `(scope S*)` | LengStmt, NimonyStmt | explicit scope annotation, like `stmts` |
| `(if (elif X X)+ (else X)?)` | LengStmt, NimonyStmt, NiflerKind | if statement header |
| `(when (elif X X)+ (else X)?)` | NimonyStmt, NimonyOther, NiflerKind | when statement header |
| `(elif X X)` | LengOther, NimonyOther, NiflerKind | pair of (condition, action) |
| `(else X)` | LengOther, NimonyOther, NiflerKind | `else` action |
| `(typevars (typevar ...)*)` | NimonyOther, NiflerKind | type variable/generic parameters |
| `(break .Y)`; `(break)` | LengStmt, NimonyStmt, NiflerKind | `break` statement |
| `(continue .Y)`; `(continue)` | NimonyStmt, NiflerKind, NjvlKind | `continue` statement |
| `(for X ... S)` | NimonyStmt, NiflerKind | for statement |
| `(while X S)` | LengStmt, NimonyStmt, NiflerKind| `while` statement |
| `(corofor X S)` | NimonyStmt | closure-iterator for loop, lowered shape used between iterinliner and cps; first child is the iterator call, second child is a `(stmts ...)` whose first inner statement is a `(var :forLoopVar T .)` declaration that receives each yielded value |
| `(case X (of (ranges...) S)+ (else X)?)` | LengStmt, NimonyStmt, NimonyOther, NiflerKind | `case` statement |
| `(of (ranges ...) S)` | LengOther, NimonyOther, NiflerKind | `of` branch within a `case` statement |
| `(lab D)` | LengStmt, LengSym, NjvlKind | label, target of a `jmp` instruction |
| `(jmp Y)` | LengStmt, NjvlKind | jump/goto instruction |
| `(ret .X)` | LengStmt, NimonyStmt, NiflerKind | `return` instruction |
| `(yld .X)` | NimonyStmt, NiflerKind | yield statement |
| `(stmts S*)` | LengStmt, NimonyStmt, NimonyOther, NiflerKind | list of statements |
| `(params (param...)*)` | LengType, NimonyOther, NiflerKind | list of proc parameters, also used as a "proc type" |
| `(union (fld ...)*)`; `(union)` | LengType, NimonyPragma | first one is Leng union declaration, second one is Nimony union pragma |
| `(object .T (fld ...)*)` | LengType, NimonyType, NiflerKind | object type declaration |
| `(enum (efld...)*)` | LengType, NimonyType, NiflerKind | enum type declaration |
| `(proctype .Any (params...) T P)`; `(proctype ...)` | NimonyType, NiflerKind, LengType | Nimony proc type. Slot 0 carries the nilability tag — either a `.` placeholder or one of `(notnil)`, `(nil)`, `(unchecked)`. Leng proc type, same shape as `(proc D ...)` with anonymous name slot (varargs spec; effects/body slots present but unused). |
| `(atomic)` | LengTypeQualifier | `atomic` type qualifier for Leng |
| `(ro)` | LengTypeQualifier | `readonly` (= `const`) type qualifier for Leng |
| `(restrict)` | LengTypeQualifier | type qualifier for Leng |
| `(cppref)` | LengTypeQualifier | type qualifier for Leng that provides a C++ reference |
| `(i INTLIT (importc/importcpp STR)? (header STR)?)` | LengType, NimonyType | `int` builtin type |
| `(u INTLIT ...)` | LengType, NimonyType | `uint` builtin type; size in bits followed by optional attributes (`(importc ...)`, `(header ...)`, etc.) |
| `(f INTLIT (importc/importcpp STR)? (header STR)?)` | LengType, NimonyType | `float` builtin type |
| `(c INTLIT (importc/importcpp STR)? (header STR)?)` | LengType, NimonyType | `char` builtin type |
| `(bool)` | LengType, NimonyType | `bool` builtin type |
| `(void)` | LengType, NimonyType | `void` return type |
| `(ptr T (unchecked)?)`; `(ptr T)` | LengType, NimonyType, NiflerKind | `ptr` type contructor; the `(unchecked)` pragma relaxes nil checking on deref |
| `(array T T)` | LengType, NimonyType | `array` type constructor (element type, index type/range) |
| `(flexarray T)` | LengType | `flexarray` type constructor |
| `(aptr T TQC*)` | LengType | "pointer to array of" type constructor |
| `(cdecl)` | CallConv | `cdecl` calling convention |
| `(stdcall)` | CallConv | `stdcall` calling convention |
| `(safecall)` | CallConv | `safecall` calling convention |
| `(syscall)` | CallConv | `syscall` calling convention |
| `(fastcall)` | CallConv | `fastcall` calling convention |
| `(thiscall)` | CallConv | `thiscall` calling convention |
| `(noconv)` | CallConv | no explicit calling convention |
| `(member)`  | CallConv | `member` calling convention |
| `(nimcall)` | CallConv | `nimcall` calling convention |
| `(inline)` | LengPragma, NimonyPragma, NifIndexKind | `inline` proc annotation |
| `(noinline)` | LengPragma, NimonyPragma | `noinline` proc annotation |
| `(closure)` | NimonyPragma | `closure` proc annotation; not a calling convention anymore, simply annotates a proc as a closure |
| `(attr STR)` | LengPragma | general attribute annotation |
| `(smry EFFECT* (param INT INT PARAMFLAG*)* RESULT?)` | LengPragma | alias-aware function-summary annotation: a Steensgaard-style partition of the parameters, the result and an implicit "outside" world. `EFFECT` idents: `writeGlobal`, `readGlobal`, `callsUnknown`, `raises`. Each `(param INDEX CLS PARAMFLAG*)` carries the parameter index, its partition class `CLS` (parameters with equal `CLS` may alias; `CLS` is the smallest param index in the class) and `PARAMFLAG` idents `reads`/`writes` (the call may read/write through the parameter's reachable graph), `slot` (a `var` parameter whose own binding is reassigned, not just its pointee) and `escapes` (the graph is stored into a global or passed to a callee with no summary). `RESULT` (`result INT (resultEscapes)?`) is the partition class the return value joins — omitted means the result is its own fresh class — and whether that graph escapes. |
| `(varargs T X. STR)`; `(varargs T Y)` | NimonyPragma, NimonyType, LengType | `varargs` type/proc annotation: Nimony carries the element type and an optional transformer symbol (e.g. `` `$` ``); Leng keeps only the element type |
| `(was STR)` | LengPragma | |
| `(selectany)` | LengPragma, NimonyPragma | |
| `(pragmas (pragma ...)*)` | LengOther, NimonyOther, NimonyStmt, NiflerKind | begin of pragma section |
| `(pragmax X (pragmas ...))` | NimonyExpr, NimonyStmt, NiflerKind | pragma expressions |
| `(align X)` | LengPragma, NimonyPragma | |
| `(bits X)`| LengPragma, NimonyPragma | |
| `(vector)` | LengPragma | |
| `(nodecl)` | LengPragma, NimonyPragma | `nodecl` annotation |
| `(incl T X X)` | NimonyStmt | `incl` set operation; first child is the set's element type |
| `(excl T X X)` | NimonyStmt | `excl` set operation; first child is the set's element type |
| `(include X+)` | NimonyStmt, NiflerKind | `include` statement |
| `(import X+)` | NimonyStmt, NiflerKind | `import` statement |
| `(importas X X)` | NimonyStmt, NiflerKind | `import as` statement |
| `(fromimport X X+)` | NimonyStmt, NiflerKind | `from import` statement |
| `(importexcept X X+)` | NimonyStmt, NiflerKind | `importexcept` statement |
| `(export X+)` | NimonyStmt, NiflerKind, NifIndexKind | `export` statement |
| `(fromexport X X+)` | NifIndexKind | specific exported symbols from a module |
| `(exportexcept X X+)` | NimonyStmt, NiflerKind, NifIndexKind | `exportexcept` statement |
| `(comment ...)` | NimonyStmt, NiflerKind | `comment` statement; also used as a variadic trailer for module metadata |
| `(discard .X)` | LengStmt, NimonyStmt, NiflerKind | `discard` statement; optional expression to discard |
| `(try X (except .X X)* (fin S)?); (try S S S)` | LengStmt, NimonyStmt, NiflerKind | `try` statement |
| `(raise .X)` | LengStmt, NimonyStmt, NiflerKind | `raise` statement; bare `(raise .)` re-raises the in-flight exception (only valid inside an `except` block) |
| `(onerr S X+)` | LengStmt | error handling statement |
| `(raises ...)` | LengPragma, NimonyPragma | proc annotation; optional list of exception types the proc may raise |
| `(errs)` | LengPragma | proc annotation |
| `(static T)`; `(static)` | LengPragma, NimonyType, NiflerKind | `static` type or annotation |
| `(ite X S S S STR_LIT?)` | ControlFlowKind, NjvlKind, LengStmt | if-then-else followed by `join` information followed by an optional label |
| `(itec X S S)` | NjvlKind, LengStmt | if-then-else (that was a `case`) |
| `(loop S X S S)` | NjvlKind, LengStmt | `loop` components are (before-cond, cond, loop-body, after) |
| `(v X INT_LIT)` | NjvlKind | `versioned` locations |
| `(etupat X INT_LIT)` | NjvlKind | tupat expression for error handling |
| `(unknown X)` | NjvlKind | location's contents is unknown at this point |
| `(jtrue Y+)` | NjvlKind, LengStmt | set variables v1, v2, ... to `(true)`; hint this should become a jump |
| `(mflag D)` | NjvlKind, LengStmt, LengSym | declare a new **materialized** control flow flag `D` of type `bool` initialized to `false` |
| `(vflag D)` | NjvlKind, LengStmt, LengSym | declare a new **virtual** control flow flag `D` of type `bool` initialized to `false` |
| `(either Y INT_LIT+)` | NimonyOther | `either` construct to combine location versions |
| `(join Y INT_LIT INT_LIT INT_LIT)` | NimonyOther | `join` construct inside `ite` |
| `(graph Y)` | ControlFlowKind | disjoint subgraph annotation |
| `(forbind ...)` | ControlFlowKind | bindings for a `for` loop but the loop itself is mapped to gotos |
| `(kill Y)` | ControlFlowKind, NjvlKind | some.var is about to disappear (scope exit) |
| `(unpackflat ...)` | NimonyOther, NiflerKind | unpack into flat variable list |
| `(unpacktup ...)` | NimonyOther, NiflerKind | unpack tuple |
| `(unpackdecl S+)` | NimonyStmt, NiflerKind | unpack var/let/const declaration |
| `(except .Y X)` | NimonyOther, NiflerKind | except subsection |
| `(fin S)` | NimonyOther, NiflerKind | finally subsection |
| `(tuple (fld ...)* <or> T*)` | NimonyType, NiflerKind | `tuple` type |
| `(onum (efld...)*)` | NimonyType | enum with holes type |
| `(anum (efld...)*)` | NimonyType | sum type discriminator enum ("auto enum") |
| `(ref T (unchecked)?)` | NimonyType, NiflerKind | `ref` type; the `(unchecked)` pragma relaxes nil checking on deref |
| `(mut T)` | NimonyType, NiflerKind | `mut` type |
| `(out T)` | NimonyType, NiflerKind | `out` type |
| `(lent T)` | NimonyType | `lent` type |
| `(sink T)` | NimonyType | `sink` type |
| `(nilt)` | NimonyType | `nilt` type |
| `(concept .X .X .T? D S*)` | NimonyType, NiflerKind | `concept` type: two reserved slots, optional parent concepts (`.` / sym / `(and ...)`), a `Self` typevar `D`, and the concept body statements `S*` (body may be empty when parents are present) |
| `(distinct T)` | NimonyType, NiflerKind | `distinct` type |
| `(itertype .Any (params...) T P)`; `(itertype ...)` | NimonyType, NiflerKind | Nimony iterator type — first-class closure-iterator value at the type level. Shape mirrors `(proctype ...)`: slot 0 carries the nilability tag (`.` placeholder or one of `(notnil)`, `(nil)`, `(unchecked)`); remaining slots are params, return type, pragmas. |
| `(rangetype T X X)` | NimonyType | `rangetype` type |
| `(uarray T)` | NimonyType | `uarray` type |
| `(set T)` | NimonyType | `set` type |
| `(auto)` | NimonyType | `auto` type |
| `(symkind UNUSED)` | NimonyType | `symkind` type |
| `(typekind UNUSED)` | NimonyType | `typekind` type |
| `(typedesc T)` | NimonyType | `typedesc` type |
| `(untyped)` | NimonyPragma, NimonyType | `untyped` type |
| `(typed)` | NimonyType | `typed` type |
| `(cstring X? (importc/importcpp STR)? (header STR)?)` | NimonyType | `cstring` type; optional first child is the string literal used in a `cstring"…"` generalized string; further attributes carry importc/header overrides inlined from `{.importc.}` aliases |
| `(pointer (nil)? (importc/importcpp STR)? (header STR)?)` | NimonyType | `pointer` type; the optional `(nil)` annotation marks a nilable pointer; further attributes carry importc/header overrides inlined from `{.importc.}` aliases |
| `(ordinal)` | NimonyType | `ordinal` type |
| `(magic X)` | NimonyPragma | `magic` pragma; argument is the magic's name as string literal or ident (e.g. `"Bool"`, `HoleyEnum`) |
| `(importc X)` | NimonyPragma, LengPragma | `importc` pragma |
| `(importcpp X)` | NimonyPragma, LengPragma | `importcpp` pragma |
| `(dynlib X)` | NimonyPragma | `dynlib` pragma |
| `(exportc X)` | NimonyPragma, LengPragma | `exportc` pragma |
| `(header X)` | NimonyPragma, LengPragma | `header` pragma |
| `(threadvar)` | NimonyPragma | `threadvar` pragma |
| `(global)` | NimonyPragma | `global` pragma |
| `(discardable)` | NimonyPragma | `discardable` pragma |
| `(noreturn)` | NimonyPragma | `noreturn` pragma |
| `(borrow)` | NimonyPragma | `borrow` pragma |
| `(noSideEffect)` | NimonyPragma | `noSideEffect` pragma |
| `(nodestroy)` | NimonyPragma | `nodestroy` pragma |
| `(plugin X)` | NimonyPragma | `plugin` pragma |
| `(bycopy)` | NimonyPragma | `bycopy` pragma |
| `(byref)` | NimonyPragma | `byref` pragma |
| `(noinit)` | NimonyPragma | `noinit` pragma |
| `(requires X)` | NimonyPragma | `requires` pragma |
| `(ensures X)` | NimonyPragma | `ensures` pragma |
| `(assume X)` | NimonyPragma, NimonyStmt, NjvlKind | `assume` pragma/annotation |
| `(assert X)` | NimonyPragma, NimonyStmt, NjvlKind | `assert` pragma/annotation |
| `(build X)`; `(build STR STR STR)` | NimonyPragma, NifIndexKind | `build` pragma |
| `(feature STR)` | NimonyPragma | `feature` pragma |
| `(string)` | NimonyPragma | `string` pragma |
| `(view)` | NimonyPragma | `view` pragma |
| `(incompleteStruct)` | NimonyPragma | `incompleteStruct` pragma |
| `(quoted X+)` | NimonyExpr, NiflerKind | name in backticks |
| `(hderef X)` | NimonyExpr | hidden pointer deref operation |
| `(ddot X Y INTLIT STRLIT?)` | NimonyExpr | deref dot: expression, field symbol, field index; optional trailing `STRLIT` is the same *access token* described on `(dot ...)` — certifies the access was type-checked with private-field visibility and must be accepted on re-check. |
| `(haddr X)` | NimonyExpr | hidden address of operation |
| `(newref T X?)` | NimonyExpr | Nim's `new` magic proc that allocates a `ref T`; optional initializer expression |
| `(newobj T (kv Y X)*)` | NimonyExpr | new object constructor |
| `(tup X+)` | NimonyExpr, NiflerKind | untyped tuple constructor |
| `(tupconstr T X+)` | NimonyExpr | tuple constructor |
| `(setconstr T X*)` | NimonyExpr | set constructor |
| `(tabconstr X*)` | NimonyExpr, NiflerKind | table constructor |
| `(ashr T X X)` | NimonyExpr | |
| `(baseobj T INTLIT X)` | NimonyExpr, LengExpr | object conversion to base type |
| `(hconv T X)` | NimonyExpr | hidden basic type conversion |
| `(dconv T X)` | NimonyExpr | conversion between `distinct` types |
| `(callstrlit X+)` | NimonyExpr, NimonyStmt, NiflerKind | |
| `(infix X X X ...)` | NimonyExpr, NimonyStmt, NiflerKind | infix call form kept verbatim inside unsem'd bodies: operator followed by two or more operands (extra args come from default parameters, e.g. `s $ 80` → `` `$`(s, 80, defaultReplacement) ``) |
| `(prefix X X)` | NimonyExpr, NimonyStmt, NiflerKind | prefix call form kept verbatim inside unsem'd bodies: operator followed by single operand |
| `(hcall X*)` | NimonyExpr, NimonyStmt | hidden converter call |
| `(compiles X)` | NimonyExpr | |
| `(declared X)` | NimonyExpr | |
| `(defined X)` | NimonyExpr | |
| `(astToStr X)` | NimonyExpr | converts AST to string |
| `(bindSym X)` | NimonyExpr | hygienic symbol reference inside a macro body: at sem time, resolves the string-literal argument in the macro's *definition* scope and replaces the call with a `(call newSymNode "<full-sym-name>")` so the plugin emits a NIF Symbol token that bypasses call-site lookup |
| `(bindSymName X)` | NimonyExpr | plugin-side cousin of `bindSym`: at sem time, resolves the string-literal argument in the surrounding module's *definition* scope and replaces the call with a `StringLit` carrying the fully-qualified symbol name. Plugin authors feed the resulting string to `addSymUse(builder, name)` so the emitted NIF contains a resolved `Symbol` token. |
| `(instanceof X T)` | NimonyExpr | only-fans operator for object privilege checking |
| `(proccall  X X*)` | NimonyExpr | like the `call` tag but always a static call (no dynamic method) dispatch |
| `(high X)` | NimonyExpr | |
| `(low X)` | NimonyExpr | |
| `(typeof X X)` | NimonyExpr, NiflerKind | `typeof` operation for accessing the type of an expression |
| `(unpack)` | NimonyExpr | magic varargs expansion — see *Tuple Unpacking* section below |
| `(fields T X X?)` | NimonyExpr | fields iterator |
| `(fieldpairs T X X?)` | NimonyExpr | fieldPairs iterator |
| `(enumtostr X)` | NimonyExpr | |
| `(ismainmodule)` | NimonyExpr | |
| `(defaultobj T)` | NimonyExpr | |
| `(defaulttup T)` | NimonyExpr | |
| `(defaultdistinct T)` | NimonyExpr | |
| `(delay X X*)` | NimonyExpr | `delay(fn args)` builtin for delayed continuation creation |
| `(delay0)` | NimonyExpr | `delay()` no-arg: capture current coroutine's own continuation |
| `(suspend)` | NimonyExpr | `suspend()` magic proc: suspends the coroutine and returns Continuation(nil, nil) |
| `(expr XS+ X)` | NimonyExpr, NiflerKind | |
| `(do (params...)+ T X)` | NimonyExpr, NiflerKind | `do` expression |
| `(arrat X X X? X?)` | NimonyExpr | two optional exprs: `high` boundary and the `low` boundary (if != 0) |
| `(tupat X X)` | NimonyExpr | |
| `(plusset T X X)` | NimonyExpr | |
| `(minusset T X X)` | NimonyExpr | |
| `(mulset T X X)` | NimonyExpr | |
| `(xorset T X X)` | NimonyExpr | |
| `(eqset T X X)` | NimonyExpr | |
| `(leset T X X)` | NimonyExpr | |
| `(ltset T X X)` | NimonyExpr | |
| `(inset T X X)` | NimonyExpr | |
| `(card T X)` | NimonyExpr | |
| `(emove X)` | NimonyExpr | |
| `(destroy X)` | NimonyExpr, HookKind | |
| `(dup X)` | NimonyExpr, HookKind| |
| `(copy X X)` | NimonyExpr, HookKind | |
| `(wasmoved X)` | NimonyExpr, HookKind | |
| `(sinkh X X)` | NimonyExpr, HookKind | |
| `(trace X X)` | NimonyExpr, HookKind | |
| `(errv)` | LengExpr | error flag for Leng |
| `(staticstmt S)` | NimonyStmt, NiflerKind | `static` statement |
| `(bind Y+)` | NimonyStmt, NiflerKind | `bind` statement |
| `(mixin Y+)` | NimonyStmt, NiflerKind | `mixin` statement |
| `(using (params...)+)` | NimonyStmt, NiflerKind | `using` statement |
| `(asm X+)` | NimonyStmt, NiflerKind | `asm` statement |
| `(defer X)` | NimonyStmt, NiflerKind | `defer` statement |
| `(index (build ...))` | NifIndexKind | index section |
| `(inject)` | NimonyPragma | `inject` pragma |
| `(gensym)` | NimonyPragma | `gensym` pragma |
| `(dirty)` | NimonyPragma | `dirty` pragma |
| `(error X?)` | NimonyPragma | `error` pragma |
| `(report X)` | NimonyPragma | `report` pragma |
| `(tags X)` | NimonyPragma | `tags` effect annotation |
| `(deprecated X?)` | NimonyPragma | `deprecated` pragma |
| `(sideEffect)` | NimonyPragma | explicit `sideEffect` pragma |
| `(keepOverflowFlag)` | NimonyPragma | keep overflow flag |
| `(semantics STR)` | NimonyPragma | proc with builtin behavior for expreval |
| `(inheritable)` | NimonyPragma | `inheritable` pragma |
| `(base)` | NimonyPragma | `base` pragma (currently ignored) |
| `(pure)` | NimonyPragma | `pure` pragma (currently ignored) |
| `(final)` | NimonyPragma | `final` pragma |
| `(acyclic)` | NimonyPragma | `acyclic` pragma (currently ignored) |
| `(pragma D)` | NimonyPragma | `pragma` pragma |
| `(internalTypeName T)` | NimonyExpr | returns compiler's internal type name |
| `(internalFieldPairs T X)` | NimonyExpr | variant of fieldPairs iterator returns compiler's internal field name |
| `(failed X)` | NimonyExpr | used to access the hidden failure flag for raising calls |
| `(is X T)` | NimonyExpr | `is` operator |
| `(envp T Y)` | NimonyExpr | `envp.Y` field access to hidden `env` parameter which is of type `T` |
| `(packed)`   | LengPragma, NimonyPragma | `packed` pragma |
| `(passive)`  | NimonyPragma | `passive` pragma |
| `(push P)`   | NimonyPragma | `push` pragma |
| `(callConv X)` | NimonyPragma | `callConv` pragma for setting calling convention |
| `(pop)`      | NimonyPragma | `pop` pragma |
| `(passL X)`  | NimonyPragma | `passL` pragma adds options to the backend linker |
| `(passC X)`  | NimonyPragma | `passC` pragma adds options to the backend compiler |
| `(methods (kv STR Y)+)`  | NimonyPragma | `methods` pragma lists vtable methods for a type |
| `(size X)`  | NimonyPragma | `size` pragma for setting the byte size of a type |
| `(uncheckedAccess)` | NimonyPragma | `uncheckedAccess` marker; only valid inside `{.cast(uncheckedAccess).}:` pragma blocks (allows for obj.guardedField outside of an `of` branch) |
| `(uncheckedAssign)` | NimonyPragma | `uncheckedAssign` marker; only valid inside `{.cast(uncheckedAssign).}:` pragma blocks (ignored for Nim compat) |
| `(profiler X)` | NimonyPragma | `profiler` pragma; accepted for Nim source compatibility, semantically ignored |
| `(stacktrace X)` | NimonyPragma | `stackTrace` pragma; accepted for Nim source compatibility, semantically ignored |
| `(gcsafe)` | NimonyPragma | `gcsafe` pragma; accepted for Nim source compatibility, semantically ignored |
| `(compile STR)`; `(compile STR STR)` | NimonyPragma | `compile` pragma (Nim-compatible alias of `build`; the source language is inferred from the file extension, e.g. `.m` → Objective-C) |

### unpackflat, unpacktup, unpackdecl

Tuple unpacking in Nim can appear in two contexts: variable declarations and `for` loop variables. NIF uses three distinct tags to encode these.

**`(unpackdecl VALUE (unpacktup DECL+))`** — a `var`/`let`/`const` statement that destructures a tuple:

```nim
let (a, b) = someTuple
```

becomes:

```
(unpackdecl (call ...) (unpacktup
  (let a.0 ... ...)
  (let b.0 ... ...)))
```

`VALUE` is the tuple expression evaluated once. Each `DECL` inside `unpacktup` is an ordinary variable declaration whose initializer is synthesized as a field access into `VALUE`.

**`(unpackflat VAR+)`** — a flat list of loop variables in a `for` statement that unpacks a tuple-yielding iterator:

```nim
for a, b in items(pairSeq):
```

becomes:

```
(for (unpackflat a.0 b.0) (call ...) ...)
```

The variables inside `unpackflat` are plain symbol references, not full declarations. The for-loop body receives `a` and `b` as separate locals bound to successive tuple fields.

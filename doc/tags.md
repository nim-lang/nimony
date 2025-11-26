| Tag                    | Enums                       |   Description |
|------------------------|-----------------------------|---------------|
| `(err ...)`            | NimonyExpr, NimonyType, NiflerKind                    | indicates an error |
| `(suf LIT STR)`        | NifcExpr, NimonyExpr, NiflerKind                     | literal with suffix annotation |
| `(at X X)`             | NifcExpr, NimonyExpr, NimonyType, NiflerKind | array indexing operation |
| `(deref X)`; `(deref X (cppref)?)`            | NifcExpr, NimonyExpr, NiflerKind | pointer deref operation |
| `(dot X Y)`; `(dot X Y INTLIT)` | NifcExpr, NimonyExpr, NiflerKind | object field selection |
| `(pat X X)`            | NifcExpr, NimonyExpr | pointer indexing operation |
| `(par X)`              | NifcExpr, NimonyExpr, NiflerKind | syntactic parenthesis |
| `(addr X)`; `(addr X (cppref)?)`  | NifcExpr, NimonyExpr, NiflerKind | address of operation |
| `(nil T?)`             | NifcExpr, NimonyExpr, NimonyOther, NiflerKind | nil pointer value |
| `(notnil)`             | NimonyOther | `not nil` pointer annotation |
| `(inf T?)`             | NifcExpr, NimonyExpr | positive infinity floating point value |
| `(neginf T?)`          | NifcExpr, NimonyExpr | negative infinity floating point value |
| `(nan T?)`             | NifcExpr, NimonyExpr | NaN floating point value |
| `(false)`              | NifcExpr, NimonyExpr | boolean `false` value |
| `(true)`               | NifcExpr, NimonyExpr | boolean `true` value |
| `(and X X)`            | NifcExpr, NimonyExpr, NimonyType | boolean `and` operation |
| `(or X X)`             | NifcExpr, NimonyExpr, NimonyType | boolean `or` operation |
| `(xor X X)`            | NimonyExpr | boolean `xor` operation |
| `(not X)`              | NifcExpr, NimonyExpr, NimonyType | boolean `not` operation |
| `(neg X)`              | NifcExpr, NimonyExpr | negation operation |
| `(sizeof T)`           | NifcExpr, NimonyExpr | `sizeof` operation |
| `(alignof T)`          | NifcExpr, NimonyExpr | `alignof` operation |
| `(offsetof T Y)`       | NifcExpr, NimonyExpr | `offsetof` operation |
| `(oconstr T (kv Y X)*)`; `(oconstr T (oconstr...)? (kv Y X*))` | NifcExpr, NimonyExpr, NiflerKind | object constructor |
| `(aconstr T X*)`       | NifcExpr, NimonyExpr | array constructor |
| `(bracket X*)`         | NimonyExpr, NiflerKind | untyped array constructor |
| `(curly X*)`           | NimonyExpr, NiflerKind | untyped set constructor |
| `(curlyat X X)`        | NimonyExpr, NiflerKind | curly expression `a{i}` |
| `(kv Y X)`             | NimonyOther, NifcOther, NiflerKind, NifIndexKind | key-value pair |
| `(vv X X)`             | NimonyOther, NiflerKind, NifIndexKind | value-value pair (used for explicitly named arguments in function calls) |
| `(ovf)`                | NimonyExpr, NifcExpr | access overflow flag |
| `(add T X X)`          | NifcExpr, NimonyExpr | |
| `(sub T X X)`          | NifcExpr, NimonyExpr | |
| `(mul T X X)`          | NifcExpr, NimonyExpr | |
| `(div T X X)`          | NifcExpr, NimonyExpr | |
| `(mod T X X)`          | NifcExpr, NimonyExpr | |
| `(shr T X X)`          | NifcExpr, NimonyExpr | |
| `(shl T X X)`          | NifcExpr, NimonyExpr | |
| `(bitand T X X)`       | NifcExpr, NimonyExpr | |
| `(bitor T X X)`        | NifcExpr, NimonyExpr | |
| `(bitxor T X X)`       | NifcExpr, NimonyExpr | |
| `(bitnot T X)`         | NifcExpr, NimonyExpr | |
| `(eq T X X)`           | NifcExpr, NimonyExpr | |
| `(neq T X X)`          | NifcExpr, NimonyExpr | |
| `(le T X X)`           | NifcExpr, NimonyExpr | |
| `(lt T X X)`           | NifcExpr, NimonyExpr | |
| `(cast T X)`           | NifcExpr, NimonyExpr, NiflerKind | `cast` operation |
| `(conv T X)`           | NifcExpr, NimonyExpr | type conversion |
| `(call X X*)`          | NifcExpr, NimonyExpr, NifcStmt, NimonyStmt, NiflerKind | call operation |
| `(cmd X X*)`             | NimonyStmt, NimonyExpr, NiflerKind | command operation |
| `(range X X)`          | NifcOther, NimonyOther | `(range a b)` construct |
| `(ranges (range ...)*)` | NifcOther, NimonyOther, NiflerKind | |
| `(gvar D E P T X)`; `(gvar D P T X)` | NifcStmt, NimonyStmt, NimonySym, NifcSym, NifIndexKind | global variable declaration |
| `(tvar D E P T X)`; `(tvar D P T X)` | NifcStmt, NimonyStmt, NimonySym, NifcSym, NifIndexKind | thread local variable declaration |
| `(var D E P T X)`; `(var D P T X)` | NifcStmt, NimonyStmt, NimonySym, NifcSym, NiflerKind, NifIndexKind | variable declaration |
| `(param D E P T X)`; `(param D P T)` | NifcOther, NimonyOther, NimonySym, NifcSym, NiflerKind | parameter declaration |
| `(const D E P T X)`; `(const D P T)` | NifcStmt, NimonyStmt, NimonySym, NifcSym, NiflerKind, NifIndexKind | const variable declaration |
| `(result D E P T X)` | NimonySym, NimonyStmt | result variable declaration |
| `(glet D E P T X)` | NimonyStmt, NimonySym, NifIndexKind | global let variable declaration |
| `(tlet D E P T X)` | NimonyStmt, NimonySym, NifIndexKind | thread local let variable declaration |
| `(let D E P T X)` | NimonySym, NimonyStmt, NiflerKind, NifIndexKind | let variable declaration |
| `(cursor D E P T X)` | NimonySym, NimonyStmt, NimonyPragma, NifIndexKind | cursor variable declaration |
| `(typevar D E P T X)` | NimonySym, NifcOther, NimonyOther, NiflerKind | type variable declaration |
| `(efld D E P T X)`; `(efld D X)` | NimonySym, NifcSym, NifcOther, NimonyOther, NiflerKind | enum field declaration |
| `(fld D E P T X)`; `(fld D P T)` | NifcOther, NimonyOther, NimonySym, NifcSym, NiflerKind | field declaration |
| `(proc D ...)` | NifcStmt, NimonyStmt, NimonySym, NimonyType, NifcSym, NiflerKind, NifIndexKind | proc declaration |
| `(func D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | function declaration |
| `(iterator D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | iterator declaration |
| `(converter D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | converter declaration |
| `(method D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | method declaration |
| `(macro D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | macro declaration |
| `(template D ...)` | NimonyStmt, NimonySym, NimonyType, NiflerKind, NifIndexKind | template declaration |
| `(type D ...)` | NifcStmt, NimonyStmt, NimonySym, NiflerKind, NifIndexKind | type declaration |
| `(block .D X)` | NimonyStmt, NimonySym, NiflerKind | block declaration |
| `(module)` | NimonySym | module declaration |
| `(cchoice X X*)` | NimonyExpr, NimonySym | closed choice |
| `(ochoice X X*)`| NimonyExpr | open choice |
| `(emit X*)` | NifcStmt, NimonyStmt, NimonyPragma | emit statement |
| `(asgn X X)` | NifcStmt, NimonyStmt, NiflerKind | assignment statement |
| `(store X X)` | NjvlKind | `asgn` with reversed operands that reflects evaluation order |
| `(keepovf X X)` | NifcStmt | keep overflow flag statement |
| `(scope S*)` | NifcStmt, NimonyStmt | explicit scope annotation, like `stmts` |
| `(if (elif X X)+ (else X)?)` | NifcStmt, NimonyStmt, NiflerKind | if statement header |
| `(when (elif X X)+ (else X)?)` | NimonyStmt, NimonyOther, NiflerKind | when statement header |
| `(elif X X)` | NifcOther, NimonyOther, NiflerKind | pair of (condition, action) |
| `(else X)` | NifcOther, NimonyOther, NiflerKind | `else` action |
| `(typevars (typevar ...)*)` | NimonyOther, NiflerKind | type variable/generic parameters |
| `(break .Y)`; `(break)` | NifcStmt, NimonyStmt, NiflerKind | `break` statement |
| `(continue)` | NimonyStmt, NiflerKind, NjvlKind | `continue` statement |
| `(for X ... S)` | NimonyStmt, NiflerKind | for statement |
| `(while X S)` | NifcStmt, NimonyStmt, NiflerKind| `while` statement |
| `(case X (of (ranges...))+ (else X)?)` | NifcStmt, NimonyStmt, NimonyOther, NiflerKind | `case` statement |
| `(of (ranges ...))` | NifcOther, NimonyOther, NiflerKind | `of` branch within a `case` statement |
| `(lab D)` | NifcStmt, NifcSym | label, target of a `jmp` instruction |
| `(jmp Y)` | NifcStmt | jump/goto instruction |
| `(ret .X)` | NifcStmt, NimonyStmt, NiflerKind | `return` instruction |
| `(yld .X)` | NimonyStmt, NiflerKind | yield statement |
| `(stmts S*)` | NifcStmt, NimonyStmt, NimonyOther, NiflerKind | list of statements |
| `(params (param...)*)` | NifcType, NimonyOther, NiflerKind | list of proc parameters, also used as a "proc type" |
| `(union (fld ...)*)`; `(union)` | NifcType, NimonyPragma | first one is Nifc union declaration, second one is Nimony union pragma |
| `(object .T (fld ...)*)` | NifcType, NimonyType, NiflerKind | object type declaration |
| `(enum (efld...)*)` | NifcType, NimonyType, NiflerKind | enum type declaration |
| `(proctype . (params...) T P)` | NifcType, NimonyType, NiflerKind | proc type declaration |
| `(atomic)` | NifcTypeQualifier | `atomic` type qualifier for NIFC |
| `(ro)` | NifcTypeQualifier | `readonly` (= `const`) type qualifier for NIFC |
| `(restrict)` | NifcTypeQualifier | type qualifier for NIFC |
| `(cppref)` | NifcTypeQualifier | type qualifier for NIFC that provides a C++ reference |
| `(i INTLIT)` | NifcType, NimonyType | `int` builtin type |
| `(u INTLIT)` | NifcType, NimonyType | `uint` builtin type |
| `(f INTLIT)` | NifcType, NimonyType | `float` builtin type |
| `(c INTLIT)` | NifcType, NimonyType | `char` builtin type |
| `(bool)` | NifcType, NimonyType | `bool` builtin type |
| `(void)` | NifcType, NimonyType | `void` return type |
| `(ptr T)` | NifcType, NimonyType, NiflerKind | `ptr` type contructor |
| `(array T X)` | NifcType, NimonyType | `array` type constructor |
| `(flexarray T)` | NifcType | `flexarray` type constructor |
| `(aptr T TQC*)` | NifcType | "pointer to array of" type constructor |
| `(cdecl)` | CallConv | `cdecl` calling convention |
| `(stdcall)` | CallConv | `stdcall` calling convention |
| `(safecall)` | CallConv | `safecall` calling convention |
| `(syscall)` | CallConv | `syscall` calling convention |
| `(fastcall)` | CallConv | `fastcall` calling convention |
| `(thiscall)` | CallConv | `thiscall` calling convention |
| `(noconv)` | CallConv | no explicit calling convention |
| `(member)`  | CallConv | `member` calling convention |
| `(nimcall)` | CallConv | `nimcall` calling convention |
| `(inline)` | NifcPragma, NimonyPragma, NifIndexKind | `inline` proc annotation |
| `(noinline)` | NifcPragma, NimonyPragma | `noinline` proc annotation |
| `(closure)` | NimonyPragma | `closure` proc annotation; not a calling convention anymore, simply annotates a proc as a closure |
| `(attr STR)` | NifcPragma | general attribute annoation |
| `(varargs)` | NifcPragma, NimonyPragma, NimonyType | `varargs` proc annotation |
| `(was STR)` | NifcPragma | |
| `(selectany)` | NifcPragma, NimonyPragma | |
| `(pragmas (pragma ...)*)` | NifcOther, NimonyOther, NimonyStmt, NiflerKind | begin of pragma section |
| `(pragmax X (pragmas ...))` | NimonyExpr, NiflerKind | pragma expressions |
| `(align X)` | NifcPragma, NimonyPragma | |
| `(bits X)`| NifcPragma, NimonyPragma | |
| `(vector)` | NifcPragma | |
| `(imp S)` | NifcStmt | import declaration |
| `(nodecl)` | NifcPragma, NimonyPragma | `nodecl` annotation |
| `(incl X X)`; `(incl STR)` | NifcStmt, NimonyStmt | `#include` statement or `incl` set operation |
| `(excl X X)` | NimonyStmt | `excl` set operation |
| `(include X+)` | NimonyStmt, NiflerKind | `include` statement |
| `(import X+)` | NimonyStmt, NiflerKind | `import` statement |
| `(importas X X)` | NimonyStmt, NiflerKind | `import as` statement |
| `(fromimport X X+)` | NimonyStmt, NiflerKind | `from import` statement |
| `(importexcept X X+)` | NimonyStmt, NiflerKind | `importexcept` statement |
| `(export X+)` | NimonyStmt, NiflerKind, NifIndexKind | `export` statement |
| `(fromexport X X+)` | NifIndexKind | specific exported symbols from a module |
| `(exportexcept X X+)` | NimonyStmt, NiflerKind, NifIndexKind | `exportexcept` statement |
| `(comment STR)` | NimonyStmt, NiflerKind | `comment` statement |
| `(discard X)` | NifcStmt, NimonyStmt, NiflerKind | `discard` statement |
| `(try X (except .X X)* (fin S)?); (try S S S)` | NifcStmt, NimonyStmt, NiflerKind | `try` statement |
| `(raise X)` | NifcStmt, NimonyStmt, NiflerKind | `raise` statement |
| `(onerr S X+)` | NifcStmt | error handling statement |
| `(raises)` | NifcPragma, NimonyPragma | proc annotation |
| `(errs)` | NifcPragma | proc annotation |
| `(static T)`; `(static)` | NifcPragma, NimonyType, NiflerKind | `static` type or annotation |
| `(ite X S S S STR_LIT?)` | ControlFlowKind, NjvlKind | if-then-else followed by `join` information followed by an optional label |
| `(itec X S S)` | NjvlKind | if-then-else (that was a `case`) |
| `(loop S X S S)` | NjvlKind | `loop` components are (before-cond, cond, loop-body, after) |
| `(v X INT_LIT)` | NjvlKind | `versioned` locations |
| `(unknown X)` | NjvlKind | location's contents is unknown at this point |
| `(jtrue Y+)` | NjvlKind | set variables v1, v2, ... to `(true)`; hint this should become a jump |
| `(cfvar D)` | NjvlKind | declare a new control flow variable `D` of type `bool` initialized to `false` |
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
| `(ref T)` | NimonyType, NiflerKind | `ref` type |
| `(mut T)` | NimonyType, NiflerKind | `mut` type |
| `(out T)` | NimonyType, NiflerKind | `out` type |
| `(lent T)` | NimonyType | `lent` type |
| `(sink T)` | NimonyType | `sink` type |
| `(nilt)` | NimonyType | `nilt` type |
| `(concept S*)` | NimonyType, NiflerKind | `concept` type |
| `(distinct T)` | NimonyType, NiflerKind | `distinct` type |
| `(itertype . (params...) T)` | NimonyType, NiflerKind | `itertype` type |
| `(rangetype T X X)` | NimonyType | `rangetype` type |
| `(uarray T)` | NimonyType | `uarray` type |
| `(set T)` | NimonyType | `set` type |
| `(auto)` | NimonyType | `auto` type |
| `(symkind UNUSED)` | NimonyType | `symkind` type |
| `(typekind UNUSED)` | NimonyType | `typekind` type |
| `(typedesc T)` | NimonyType | `typedesc` type |
| `(untyped)` | NimonyPragma, NimonyType | `untyped` type |
| `(typed)` | NimonyType | `typed` type |
| `(cstring)` | NimonyType | `cstring` type |
| `(pointer)` | NimonyType | `pointer` type |
| `(ordinal)` | NimonyType | `ordinal` type |
| `(magic STR)` | NimonyPragma | `magic` pragma |
| `(importc X)` | NimonyPragma | `importc` pragma |
| `(importcpp X)` | NimonyPragma | `importcpp` pragma |
| `(dynlib X)` | NimonyPragma | `dynlib` pragma |
| `(exportc X)` | NimonyPragma | `exportc` pragma |
| `(header X)` | NimonyPragma | `header` pragma |
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
| `(string)` | NimonyPragma | `string` pragma |
| `(view)` | NimonyPragma | `view` pragma |
| `(incompleteStruct)` | NimonyPragma | `incompleteStruct` pragma |
| `(quoted X+)` | NimonyExpr, NiflerKind | name in backticks |
| `(hderef X)` | NimonyExpr | hidden pointer deref operation |
| `(ddot X)` | NimonyExpr | deref dot |
| `(haddr X)` | NimonyExpr | hidden address of operation |
| `(newref T)` | NimonyExpr | Nim's `new` magic proc that allocates a `ref T` |
| `(newobj T (kv Y X)*)` | NimonyExpr | new object constructor |
| `(tup X+)` | NimonyExpr, NiflerKind | untyped tuple constructor |
| `(tupconstr T X+)` | NimonyExpr | tuple constructor |
| `(setconstr T X*)` | NimonyExpr | set constructor |
| `(tabconstr X*)` | NimonyExpr, NiflerKind | table constructor |
| `(ashr T X X)` | NimonyExpr | |
| `(baseobj T INTLIT X)` | NimonyExpr, NifcExpr | object conversion to base type |
| `(hconv T X)` | NimonyExpr | hidden basic type conversion |
| `(dconv T X)` | NimonyExpr | conversion between `distinct` types |
| `(callstrlit X+)` | NimonyExpr, NimonyStmt, NiflerKind | |
| `(infix X X)` | NimonyExpr, NimonyStmt, NiflerKind | |
| `(prefix X)` | NimonyExpr, NimonyStmt, NiflerKind | |
| `(hcall X*)` | NimonyExpr, NimonyStmt | hidden converter call |
| `(compiles X)` | NimonyExpr | |
| `(declared X)` | NimonyExpr | |
| `(defined X)` | NimonyExpr | |
| `(astToStr X)` | NimonyExpr | converts AST to string |
| `(instanceof X T)` | NimonyExpr | only-fans operator for object privilege checking |
| `(proccall X)` | NimonyExpr | turns method call into a proc call aka a "static" call |
| `(high X)` | NimonyExpr | |
| `(low X)` | NimonyExpr | |
| `(typeof X X)` | NimonyExpr, NiflerKind | `typeof` operation for accessing the type of an expression |
| `(unpack)` | NimonyExpr | |
| `(fields T X X?)` | NimonyExpr | fields iterator |
| `(fieldpairs T X X?)` | NimonyExpr | fieldPairs iterator |
| `(enumtostr X)` | NimonyExpr | |
| `(ismainmodule)` | NimonyExpr | |
| `(defaultobj T)` | NimonyExpr | |
| `(defaulttup T)` | NimonyExpr | |
| `(defaultdistinct T)` | NimonyExpr | |
| `(delay T X)` | NimonyExpr | `delay` builtin for delayed continuation creation |
| `(expr S+ X)` | NimonyExpr, NiflerKind | |
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
| `(destroy X)` | NimonyExpr, NifIndexKind, HookKind | |
| `(dup X)` | NimonyExpr, NifIndexKind, HookKind| |
| `(copy X X)` | NimonyExpr, NifIndexKind, HookKind | |
| `(wasmoved X)` | NimonyExpr, NifIndexKind, HookKind | |
| `(sinkh X X)` | NimonyExpr, NifIndexKind, HookKind | |
| `(trace X X)` | NimonyExpr, NifIndexKind, HookKind | |
| `(errv)` | NifcExpr | error flag for `NIFC` |
| `(staticstmt S)` | NimonyStmt, NiflerKind | `static` statement |
| `(bind Y+)` | NimonyStmt, NiflerKind | `bind` statement |
| `(mixin Y+)` | NimonyStmt, NiflerKind | `mixin` statement |
| `(using (params...)+)` | NimonyStmt, NiflerKind | `using` statement |
| `(asm X+)` | NimonyStmt, NiflerKind | `asm` statement |
| `(defer X)` | NimonyStmt, NiflerKind | `defer` statement |
| `(index (public ...) (private ...) (hooks ...) (converter ...) (method ...) (build ...))` | NifIndexKind | index section |
| `(public (kv Y INTLIT*)` | NifIndexKind | public section |
| `(private (kv Y INTLIT*))` | NifIndexKind | private section |
| `(inject)` | NimonyPragma | `inject` pragma |
| `(gensym)` | NimonyPragma | `gensym` pragma |
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
| `(pragma D)` | NimonyPragma | `pragma` pragma |
| `(internalTypeName T)` | NimonyExpr | returns compiler's internal type name |
| `(internalFieldPairs T X)` | NimonyExpr | variant of fieldPairs iterator returns compiler's internal field name |
| `(failed X)` | NimonyExpr | used to access the hidden failure flag for raising calls |
| `(is X T)` | NimonyExpr | `is` operator |
| `(envp T Y)` | NimonyExpr | `envp.Y` field access to hidden `env` parameter which is of type `T` |
| `(packed)`   | NifcPragma, NimonyPragma | `packed` pragma |
| `(passive)`  | NimonyPragma | `passive` pragma |
| `(push P)`   | NimonyPragma | `push` pragma |
| `(pop)`      | NimonyPragma | `pop` pragma |
| `(passL X)`  | NimonyPragma | `passL` pragma adds options to the backend linker |
| `(passC X)`  | NimonyPragma | `passC` pragma adds options to the backend compiler |

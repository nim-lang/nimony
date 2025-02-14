| Tag                    | Enums                       |   Description |
|------------------------|-----------------------------|---------------|
| `(err ...)`            | X, TN, L                    | indicates an error |
| `(suf LIT STR)`        | C, X, L                     | literal with suffix annotation |
| `(at X X)`             | C, X, TN, L | array indexing operation |
| `(deref X)`; `(deref X (cppref)?)`            | C, X, L | pointer deref operation |
| `(dot X Y)`; `(dot X Y INTLIT)` | C, X, L | object field selection |
| `(pat X X)`            | C, X | pointer indexing operation |
| `(par X)`              | C, X, L | syntactic parenthesis |
| `(addr X)`; `(addr X (cppref)?)`  | C, X, L | address of operation |
| `(nil T?)`             | C, X, L | nil pointer value |
| `(inf T?)`             | C, X | positive infinity floating point value |
| `(neginf T?)`          | C, X | negative infinity floating point value |
| `(nan T?)`             | C, X | NaN floating point value |
| `(false)`              | C, X | boolean `false` value |
| `(true)`               | C, X | boolean `true` value |
| `(and X X)`            | C, X, TN | boolean `and` operation |
| `(or X X)`             | C, X, TN | boolean `or` operation |
| `(not X)`              | C, X, TN | boolean `not` operation |
| `(neg X)`              | C, X | negation operation |
| `(sizeof T)`           | C, X | `sizeof` operation |
| `(alignof T)`          | C, X | `alignof` operation |
| `(offsetof T Y)`       | C, X | `offsetof` operation |
| `(oconstr T (kv Y X)*)`; `(oconstr T (oconstr...)? (kv Y X*))` | C, X, L | object constructor |
| `(aconstr T X*)`       | C, X | array constructor |
| `(bracket X*)`         | X, L | untyped array constructor |
| `(curly X*)`           | X, L | untyped set constructor |
| `(curlyat X X)`        | X, L | curly expression `a{i}` |
| `(kv Y X)`             | SU, L | key-value pair |
| `(vv X X)`             | SUN, L | value-value pair (used for explicitly named arguments in function calls) | 
| `(add T X X)`          | C, X | |
| `(sub T X X)`          | C, X | |
| `(mul T X X)`          | C, X | |
| `(div T X X)`          | C, X | |
| `(mod T X X)`          | C, X | |
| `(shr T X X)`          | C, X | |
| `(shl T X X)`          | C, X | |
| `(bitand T X X)`       | C, X | |
| `(bitor T X X)`        | C, X | |
| `(bitxor T X X)`       | C, X | |
| `(bitnot T X)`         | C, X | |
| `(eq T X X)`           | C, X | |
| `(neq T X X)`          | C, X | |
| `(le T X X)`           | C, X | |
| `(lt T X X)`           | C, X | |
| `(cast T X)`           | C, X, L | `cast` operation |
| `(conv T X)`           | C, X | type conversion |
| `(call X X*)`          | C, X, SC, SN, L | call operation |
| `(cmd X X*)`             | SN, X, L | command operation |
| `(range X X)`          | SU | `(range a b)` construct |
| `(ranges (range ...)*)` | SU, L | |
| `(gvar D P T X)` | SC, Z | global variable declaration |
| `(tvar D P T X)` | SC, Z | thread local variable declaration |
| `(var D E P T X)`; `(var D P T X)` | SC, SN, Y, Z, L | variable declaration |
| `(param D E P T X)`; `(param D P T)` | SU, Y, Z, L | parameter declaration |
| `(const D E P T X)`; `(const D P T)` | SC, SN, Y, Z, L | const variable declaration |
| `(result D E P T X)` | Y, SN | result variable declaration |
| `(let D E P T X)` | Y, SN, L | let variable declaration |
| `(cursor D E P T X)` | Y, SN | cursor variable declaration |
| `(typevar D E P T X)` | Y, SU, L | type variable declaration |
| `(efld D E P T X)`; `(efld D X)` | Y, Z, SU, L | enum field declaration |
| `(fld D E P T X)`; `(fld D P T)` | SU, Y, Z, L | field declaration |
| `(proc D ...)` | SC, SN, Y, Z, L | proc declaration |
| `(func D ...)` | SN, Y, L | function declaration |
| `(iterator D ...)` | SN, Y, TN, L | iterator declaration |
| `(converter D ...)` | SN, Y, L | converter declaration |
| `(method D ...)` | SN, Y, L | method declaration |
| `(macro D ...)` | SN, Y, L | macro declaration |
| `(template D ...)` | SN, Y, L | template declaration |
| `(type D ...)` | SC, SN, Y, L | type declaration |
| `(block .D X)` | SN, Y, L | block declaration |
| `(module)` | Y | module declaration |
| `(cchoice X X*)` | X, Y | closed choice |
| `(ochoice X X*)`| X | open choice |
| `(emit X*)` | SC, SN, PN | emit statement |
| `(asgn X X)` | SC, SN, L | assignment statement |
| `(scope S*)` | SC, SN | explicit scope annotation, like `stmts` |
| `(if (elif X X)+ (else X)?)` | SC, SN, L | if statement header |
| `(when (elif X X)+ (else X)?)` | SN, L | when statement header |
| `(elif X X)` | SU, L | pair of (condition, action) |
| `(else X)` | SU, L | `else` action |
| `(typevars (typevar ...)*)` | SUN, L | type variable/generic parameters |
| `(break .Y)`; `(break)` | SC, SN, L | `break` statement |
| `(continue)` | SN, L | `continue` statement |
| `(for X ... S)` | SN, L | for statement |
| `(while X S)` | SC, SN, L| `while` statement |
| `(case X (of (ranges...))+ (else X)?)` | SC, SN, L | `case` statement |
| `(of (ranges ...))` | SU, L | `of` branch within a `case` statement |
| `(lab D)` | SC, Z | label, target of a `jmp` instruction |
| `(jmp Y)` | SC | jump/goto instruction |
| `(ret .X)` | SC, SN, L | `return` instruction |
| `(yld .X)` | SN, L | yield statement |
| `(stmts S*)` | SC, SN, L | list of statements |
| `(params (param...)*)` | TC, TN, L | list of proc parameters, also used as a "proc type" |
| `(union (fld ...)*)` | TC | union declaration |
| `(object .T (fld ...)*)` | TC, TN, L | object type declaration |
| `(enum (efld...)*)` | TC, TN, L | enum type declaration |
| `(proctype . (params...) T P)` | TC, TN, L | proc type declaration (soon obsolete, use params instead) |
| `(atomic)` | TQC | `atomic` type qualifier for NIFC |
| `(ro)` | TQC | `readonly` (= `const`) type qualifier for NIFC |
| `(restrict)` | TQC | type qualifier for NIFC |
| `(cppref)` | TQC | type qualifier for NIFC that provides a C++ reference |
| `(i INTLIT)` | TC, TN | `int` builtin type |
| `(u INTLIT)` | TC, TN | `uint` builtin type |
| `(f INTLIT)` | TC, TN | `float` builtin type |
| `(c INTLIT)` | TC, TN | `char` builtin type |
| `(bool)` | TC, TN | `bool` builtin type |
| `(void)` | TC, TN | `void` return type |
| `(ptr T)` | TC, TN, L | `ptr` type contructor |
| `(array T X)` | TC, TN | `array` type constructor |
| `(flexarray T)` | TC | `flexarray` type constructor |
| `(aptr T TQC*)` | TC | "pointer to array of" type constructor |
| `(cdecl)` | CC | `cdecl` calling convention |
| `(stdcall)` | CC | `stdcall` calling convention |
| `(safecall)` | CC | `safecall` calling convention |
| `(syscall)` | CC | `syscall` calling convention |
| `(fastcall)` | CC | `fastcall` calling convention |
| `(thiscall)` | CC | `thiscall` calling convention |
| `(noconv)` | CC | no explicit calling convention |
| `(member)`  | CC | `member` calling convention |
| `(nimcall)` | CC | `nimcall` calling convention |
| `(inline)` | PP | `inline` proc annotation |
| `(noinline)` | PP | `noinline` proc annotation |
| `(attr STR)` | PC | general attribute annoation |
| `(varargs)` | PP, TN | `varargs` proc annotation |
| `(was STR)` | PC | |
| `(selectany)` | PP | |
| `(pragmas (pragma ...)*)` | SU, SN, L | begin of pragma section |
| `(pragmax X (pragmas ...))` | X, L | pragma expressions |
| `(align X)` | PP | |
| `(bits X)`| PP | |
| `(vector)` | PC | |
| `(imp S)` | SC | import declaration |
| `(nodecl)` | P | `nodecl` annotation |
| `(incl X X)`; `(incl STR)` | SC, SN | `#include` statement or `incl` set operation |
| `(excl X X)` | SN | `excl` set operation |
| `(include X+)` | SN, L | `include` statement |
| `(import X+)` | SN, L | `import` statement |
| `(importas X X)` | SN, L | `import as` statement |
| `(from X X)` | SN, L | `from` statement |
| `(importexcept X+)` | SN, L | `importexcept` statement |
| `(export X+)` | SN, L | `export` statement |
| `(exportexcept X+)` | SN, L | `exportexcept` statement |
| `(comment STR)` | SN, L | `comment` statement |
| `(discard X)` | SC, SN, L | `discard` statement |
| `(try X (except .X X)* (fin S)?); (try S S S)` | SC, SN, L | `try` statement |
| `(raise X)` | SC, SN, L | `raise` statement |
| `(onerr S X+)` | SC | error handling statement |
| `(raises)` | PP | proc annotation |
| `(errs)` | PPC | proc annotation |
| `(static T)`; `(static)` | PC, TN, L | `static` type or annotation |
| `(ite X S S)` | G | if-then-else |
| `(graph Y)` | G | disjoint subgraph annotation |
| `(forbind ...)` | G | bindings for a `for` loop but the loop itself is mapped to gotos |
| `(kill Y)` | G | some.var is about to disappear (scope exit) |
| `(unpackflat ...)` | SUN, L | unpack into flat variable list |
| `(unpacktup ...)` | SUN, L | unpack tuple |
| `(unpackdecl S+)` | SN, L | unpack var/let/const declaration |
| `(except .Y X)` | SUN, L | except subsection |
| `(fin S)` | SUN, L | finally subsection |
| `(refobj .T (fld ...)*)` | TN, L | `ref object` type |
| `(ptrobj .T (fld ...)*)` | TN, L | `ptr object` type |
| `(tuple (fld ...)* <or> T*)` | TN, L | `tuple` type |
| `(onum (efld...)*)` | TN | enum with holes type |
| `(ref T)` | TN, L | `ref` type |
| `(mut T)` | TN, L | `mut` type |
| `(out T)` | TN, L | `out` type |
| `(lent T)` | TN | `lent` type |
| `(sink T)` | TN | `sink` type |
| `(nilt)` | TN | `nilt` type |
| `(concept S*)` | TN, L | `concept` type |
| `(distinct T)` | TN, L | `distinct` type |
| `(itertype . (params...) T)` | TN, L | `itertype` type |
| `(rangetype T X X)` | TN | `rangetype` type |
| `(uarray T)` | TN | `uarray` type |
| `(openarray T)` | TN | `openarray` type |
| `(set T)` | TN | `set` type |
| `(auto)` | TN | `auto` type |
| `(symkind UNUSED)` | TN | `symkind` type |
| `(typekind UNUSED)` | TN | `typekind` type |
| `(typedesc T)` | TN | `typedesc` type |
| `(untyped)` | TN | `untyped` type |
| `(typed)` | TN | `typed` type |
| `(cstring)` | TN | `cstring` type |
| `(pointer)` | TN | `pointer` type |
| `(ordinal)` | TN | `ordinal` type |
| `(magic STR)` | PN | `magic` pragma |
| `(importc X)` | PN | `importc` pragma |
| `(importcpp X)` | PN | `importcpp` pragma |
| `(exportc X)` | PN | `exportc` pragma |
| `(header X)` | PN | `header` pragma |
| `(threadvar)` | PN | `threadvar` pragma |
| `(global)` | PN | `global` pragma |
| `(discardable)` | PN | `discardable` pragma |
| `(noreturn)` | PN | `noreturn` pragma |
| `(borrow)` | PN | `borrow` pragma |
| `(noSideEffect)` | PN | `noSideEffect` pragma |
| `(nodestroy)` | PN | `nodestroy` pragma |
| `(plugin X)` | PN | `plugin` pragma |
| `(bycopy)` | PN | `bycopy` pragma |
| `(byref)` | PN | `byref` pragma |
| `(noinit)` | PN | `noinit` pragma |
| `(requires X)` | PN | `requires` pragma |
| `(ensures X)` | PN | `ensures` pragma |
| `(build X)` | PN | `build` pragma |
| `(string)` | PN | `string` pragma |
| `(quoted X+)` | X, L | name in backticks |
| `(hderef X)` | X | hidden pointer deref operation |
| `(ddot X)` | X | deref dot |
| `(haddr X)` | X | hidden address of operation |
| `(newobj T (kv Y X)*)` | X | new object constructor |
| `(tup X+)` | X, L | tuple constructor |
| `(setconstr X*)` | X | set constructor |
| `(tabconstr X*)` | X, L | table constructor |
| `(ashr T X X)` | X | |
| `(oconv T X)` | X | object conversion |
| `(hconv T X)` | X | hidden basic type conversion |
| `(dconv T X)` | X | conversion between `distinct` types |
| `(callstrlit X+)` | X, L | |
| `(infix X X)` | X, L | |
| `(prefix X)` | X, L | |
| `(hcall X*)` | X | hidden converter call |
| `(compiles X)` | X | |
| `(declared X)` | X | |
| `(defined X)` | X | |
| `(high X)` | X | |
| `(low X)` | X | |
| `(typeof X)` | X, L | |
| `(unpack)` | X | |
| `(enumtostr X)` | X | |
| `(ismainmodule)` | X | |
| `(defaultobj T)` | X | |
| `(defaulttup T)` | X | |
| `(expr S+ X)` | X, L | |
| `(do (params...)+ T X)` | X, L | `do` expression |
| `(arrat X X)` | X | |
| `(tupat X X)` | X | |
| `(plusset X X)` | X | |
| `(minusset X X)` | X | |
| `(mulset X X)` | X | |
| `(xorset X X)` | X | |
| `(eqset X X)` | X | |
| `(leset X X)` | X | |
| `(ltset X X)` | X | |
| `(inset X X)` | X | |
| `(card X)` | X | |
| `(emove X)` | X | |
| `(destroy X)` | X | |
| `(dup X)` | X | |
| `(copy X X)` | X | |
| `(wasmoved X)` | X | |
| `(sinkh X X)` | X | |
| `(trace X X)` | X | |
| `(errv)` | C | error flag for `NIFC` |
| `(staticstmt S)` | SN, L | `static` statement |
| `(bind Y+)` | SN, L | `bind` statement |
| `(mixin Y+)` | SN, L | `mixin` statement |
| `(using (params...)+)` | SN, L | `using` statement |
| `(asm X+)` | SN, L | `asm` statement |
| `(defer X)` | SN, L | `defer` statement |

| Name                 | Enums                       |   Description |
|----------------------|-----------------------------|---------------|
| err                  | X, TN, L                    | indicates an error |
| suf                  | C, X, L                     | literal with suffix annotation |
| at                   | C, X, TN, L | array indexing operation |
| deref                | C, X, L | pointer deref operation |
| dot                  | C, X, L | object field selection |
| pat                  | C, X | pointer indexing operation |
| par                  | C, X, L | syntactic parenthesis |
| addr                 | C, X, L | address of operation |
| nil                  | C, X, L | nil pointer value |
| inf                  | C, X | positive infinity floating point value |
| neginf               | C, X | negative infinity floating point value |
| nan                  | C, X | NaN floating point value |
| false                | C, X | boolean `false` value |
| true                 | C, X | boolean `true` value |
| and                  | C, X, TN | boolean `and` operation |
| or                   | C, X, TN | boolean `or` operation |
| not                  | C, X, TN | boolean `not` operation |
| neg                  | C, X | negation operation |
| sizeof               | C, X | `sizeof` operation |
| alignof              | C, X | `alignof` operation |
| offsetof             | C, X | `offsetof` operation |
| oconstr              | C, X, L | object constructor |
| aconstr              | C, X | array constructor |
| bracket              | X, L | untyped array constructor |
| curly                | X, L | untyped set constructor |
| curlyat              | X, L | curly expression `a{i}` |
| kv                   | SU, L | key-value pair |
| vv                   | SUN, L | value-value pair (used for explicitly named arguments in function calls) | 
| add                  | C, X | |
| sub | C, X | |
| mul | C, X | |
| div | C, X | |
| mod | C, X | |
| shr | C, X | |
| shl | C, X | |
| bitand | C, X | |
| bitor | C, X | |
| bitxor | C, X | |
| bitnot | C, X | |
| eq | C, X | |
| neq | C, X | |
| le | C, X | |
| lt | C, X | |
| cast | C, X, L | `cast` operation |
| conv | C, X | type conversion |
| call | C, X, SC, SN, L | call operation |
| cmd | SN, X, L | command operation |
| range | SU | `(range a b)` construct |
| ranges | SU, L | |
| gvar | SC, Z | global variable declaration |
| tvar | SC, Z | thread local variable declaration |
| var | SC, SN, Y, Z, L | variable declaration |
| param | SU, Y, Z, L | parameter declaration |
| const | SC, SN, Y, Z, L | const variable declaration |
| result | Y, SN | result variable declaration |
| let | Y, SN, L | let variable declaration |
| cursor | Y, SN | cursor variable declaration |
| typevar | Y, SU, L | type variable declaration |
| efld | Y, Z, SU, L | enum field declaration |
| fld | SU, Y, Z, L | field declaration |
| proc | SC, SN, Y, Z, L | proc declaration |
| func | SN, Y, L | function declaration |
| iterator | SN, Y, TN, L | iterator declaration |
| converter | SN, Y, L | converter declaration |
| method | SN, Y, L | method declaration |
| macro | SN, Y, L | macro declaration |
| template | SN, Y, L | template declaration |
| type | SC, SN, Y, L | type declaration |
| block | SN, Y, L | block declaration |
| module | Y | module declaration |
| cchoice | X, Y | closed choice |
| ochoice | X | open choice |
| emit | SC, SN, PN | emit statement |
| asgn | SC, SN, L | assignment statement |
| scope | SC, SN | explicit scope annotation, like `stmts` |
| if | SC, SN, L | if statement header |
| when | SN, L | when statement header |
| elif | SU, L | pair of (condition, action) |
| else | SU, L | `else` action |
| typevars | SUN, L | type variable/generic parameters |
| break | SC, SN, L | `break` statement |
| continue | SN, L | `continue` statement |
| for | SN, L | for statement |
| while | SC, SN, L| `while` statement |
| case | SC, SN, L | `case` statement |
| of | SU, L | `of` branch within a `case` statement |
| lab | SC, Z | label, target of a `jmp` instruction |
| jmp | SC | jump/goto instruction |
| ret | SC, SN, L | `return` instruction |
| yld | SN, L | yield statement |
| stmts | SC, SN, L | list of statements |
| params | TC, TN, L | list of proc parameters, also used as a "proc type" |
| union | TC | union declaration |
| object | TC, TN, L | object type declaration |
| enum | TC, TN, L | enum type declaration |
| proctype | TC, TN, L | proc type declaration (soon obsolete, use params instead) |
| atomic | TQC | `atomic` type qualifier for NIFC |
| ro | TQC | `readonly` (= `const`) type qualifier for NIFC |
| restrict | TQC | type qualifier for NIFC |
| cppref | TQC | type qualifier for NIFC that provides a C++ reference |
| i | TC, TN | `int` builtin type |
| u | TC, TN | `uint` builtin type |
| f | TC, TN | `float` builtin type |
| c | TC, TN | `char` builtin type |
| bool | TC, TN | `bool` builtin type |
| void | TC, TN | `void` return type |
| ptr | TC, TN, L | `ptr` type contructor |
| array | TC, TN | `array` type constructor |
| flexarray | TC | `flexarray` type constructor |
| aptr | TC | "pointer to array of" type constructor |
| cdecl | CC | `cdecl` calling convention |
| stdcall | CC | `stdcall` calling convention |
| safecall | CC | `safecall` calling convention |
| syscall | CC | `syscall` calling convention |
| fastcall | CC | `fastcall` calling convention |
| thiscall | CC | `thiscall` calling convention |
| noconv | CC | no explicit calling convention |
| member  | CC | `member` calling convention |
| nimcall | CC | `nimcall` calling convention |
| inline | PP | `inline` proc annotation |
| noinline | PP | `noinline` proc annotation |
| attr | PC | general attribute annoation |
| varargs | PP, TN | `varargs` proc annotation |
| was | PC | |
| selectany | PP | |
| pragmas | SU, SN, L | begin of pragma section |
| pragmax | X, L | pragma expressions |
| align | PP | |
| bits | PP | |
| vector | PC | |
| imp | SC | import declaration |
| nodecl | P | `nodecl` annotation |
| incl | SC, SN | `#include` statement or `incl` set operation |
| excl | SN | `excl` set operation |
| include | SN, L | `include` statement |
| import | SN, L | `import` statement |
| importas | SN, L | `import as` statement |
| from | SN, L | `from` statement |
| importexcept | SN, L | `importexcept` statement |
| export | SN, L | `export` statement |
| exportexcept | SN, L | `exportexcept` statement |
| comment | SN, L | `comment` statement |
| discard | SC, SN, L | `discard` statement |
| try | SC, SN, L | `try` statement |
| raise | SC, SN, L | `raise` statement |
| onerr | SC | error handling statement |
| raises | PP | proc annotation |
| errs | PPC | proc annotation |
| static | PC, TN, L | `static` type or annotation |
| ite | G | if-then-else |
| graph | G | disjoint subgraph annotation |
| forbind | G | bindings for a `for` loop but the loop itself is mapped to gotos |
| kill | G | some.var is about to disappear (scope exit) |
| unpackflat | SUN, L | unpack into flat variable list |
| unpacktup | SUN, L | unpack tuple |
| unpackdecl | SN, L | unpack var/let/const declaration |
| except | SUN, L | except subsection |
| fin | SUN, L | finally subsection |
| refobj | TN, L | `ref object` type |
| ptrobj | TN, L | `ptr object` type |
| tuple | TN, L | `tuple` type |
| onum | TN | enum with holes type |
| ref | TN, L | `ref` type |
| mut | TN, L | `mut` type |
| out | TN, L | `out` type |
| lent | TN | `lent` type |
| sink | TN | `sink` type |
| nilt | TN | `nilt` type |
| concept | TN, L | `concept` type |
| distinct | TN, L | `distinct` type |
| itertype | TN, L | `itertype` type |
| rangetype | TN | `rangetype` type |
| uarray | TN | `uarray` type |
| openarray | TN | `openarray` type |
| set | TN | `set` type |
| auto | TN | `auto` type |
| symkind | TN | `symkind` type |
| typekind | TN | `typekind` type |
| typedesc | TN | `typedesc` type |
| untyped | TN | `untyped` type |
| typed | TN | `typed` type |
| cstring | TN | `cstring` type |
| pointer | TN | `pointer` type |
| ordinal | TN | `ordinal` type |
| magic | PN | `magic` pragma |
| importc | PN | `importc` pragma |
| importcpp | PN | `importcpp` pragma |
| exportc | PN | `exportc` pragma |
| header | PN | `header` pragma |
| threadvar | PN | `threadvar` pragma |
| global | PN | `global` pragma |
| discardable | PN | `discardable` pragma |
| noreturn | PN | `noreturn` pragma |
| borrow | PN | `borrow` pragma |
| noSideEffect | PN | `noSideEffect` pragma |
| nodestroy | PN | `nodestroy` pragma |
| plugin | PN | `plugin` pragma |
| bycopy | PN | `bycopy` pragma |
| byref | PN | `byref` pragma |
| noinit | PN | `noinit` pragma |
| requires | PN | `requires` pragma |
| ensures | PN | `ensures` pragma |
| build | PN | `build` pragma |
| string | PN | `string` pragma |
| quoted | X, L | name in backticks |
| hderef | X | hidden pointer deref operation |
| ddot | X | deref dot |
| haddr | X | hidden address of operation |
| newobj | X | new object constructor |
| tup | X, L | tuple constructor |
| setconstr | X | set constructor |
| tabconstr | X, L | table constructor |
| ashr | X | |
| oconv | X | object conversion |
| hconv | X | hidden basic type conversion |
| dconv | X | conversion between `distinct` types |
| callstrlit | X, L | |
| infix | X, L | |
| prefix | X, L | |
| hcall | X | hidden converter call |
| compiles | X | |
| declared | X | |
| defined | X | |
| high | X | |
| low | X | |
| typeof | X, L | |
| unpack | X | |
| enumtostr | X | |
| ismainmodule | X | |
| defaultobj | X | |
| defaulttup | X | |
| expr | X, L | |
| do | X, L | `do` expression |
| arrat | X | |
| tupat | X | |
| plusset | X | |
| minusset | X | |
| mulset | X | |
| xorset | X | |
| eqset | X | |
| leset | X | |
| ltset | X | |
| inset | X | |
| card | X | |
| emove | X | |
| destroy | X | |
| dup | X | |
| copy | X | |
| wasmoved | X | |
| sinkh | X | |
| trace | X | |
| errv | C | error flag for `NIFC` |
| staticstmt | SN, L | `static` statement |
| bind | SN, L | `bind` statement |
| mixin | SN, L | `mixin` statement |
| using | SN, L | `using` statement |
| asm | SN, L | `asm` statement |
| defer | SN, L | `defer` statement |

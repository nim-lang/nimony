| Name                 | Enums                       |   Description |
|----------------------|-----------------------------|---------------|
| err                  | C, X, TN                    | indicates an error |
| suf                  | C, X                        | literal with suffix annotation |
| at                   | C, X, TN | array indexing operation |
| deref                | C, X | pointer deref operation |
| dot                  | C, X | object field selection |
| pat                  | C, X | pointer indexing operation |
| par                  | C, X | syntactic parenthesis |
| addr                 | C, X | address of operation |
| nil                  | C, X | nil pointer value |
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
| oconstr              | C | object constructor |
| obj                  | X | object constructor |
| aconstr              | C | array constructor |
| arr                  | X | array constructor |
| bracket              | X | untyped array constructor |
| curly                | X | untyped set constructor |
| kv                   | SU | key-value pair |
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
| cast | C, X | |
| conv | C, X | type conversion |
| call | C, X, SN | call operation |
| cmd | SN, X | command operation |
| range | SU | `(range a b)` construct |
| ranges | SU | |
| gvar | SC | global variable declaration |
| tvar | SC | thread local variable declaration |
| var | SC, SN, Y | variable declaration |
| param | SU, Y | parameter declaration |
| const | SC, SN, Y | const variable declaration |
| result | Y, SN | result variable declaration |
| let | Y, SN | let variable declaration |
| cursor | Y, SN | cursor variable declaration |
| typevar | Y, SU | type variable declaration |
| efld | Y, SU | enum field declaration |
| fld | SU, Y | field declaration |
| proc | SC, SN, Y | proc declaration |
| func | SN, Y | function declaration |
| iterator | SN, Y, TN | iterator declaration |
| converter | SN, Y | converter declaration |
| method | SN, Y | method declaration |
| macro | SN, Y | macro declaration |
| template | SN, Y | template declaration |
| type | SC, SN, Y | type declaration |
| block | SN, Y | block declaration |
| module | Y | module declaration |
| cchoice | X, Y | closed choice |
| ochoice | X | open choice |
| emit | SC, SN, PN | emit statement |
| asgn | SC, SN | assignment statement |
| scope | SC, SN | explicit scope annotation, like `stmts` |
| if | SC, SN | if statement header |
| when | SN | when statement header |
| elif | SU | pair of (condition, action) |
| else | SU | `else` action |
| typevars | SUN | type variable/generic parameters |
| break | SC, SN | `break` statement |
| continue | SN | `continue` statement |
| for | SN | for statement |
| while | SC, SN | `while` statement |
| case | SC, SN | `case` statement |
| of | SU | `of` branch within a `case` statement |
| lab | SC | label, target of a `jmp` instruction |
| jmp | SC | jump/goto instruction |
| ret | SC, SN | `return` instruction |
| yld | SN | yield statement |
| stmts | SC, SN | list of statements |
| params | TC, TN | list of proc parameters, also used as a "proc type" |
| union | TC | union declaration |
| object | TC, TN | object type declaration |
| enum | TC, TN | enum type declaration |
| proctype | TC, TN | proc type declaration (soon obsolete, use params instead) |
| atomic | TQC | `atomic` type qualifier for NIFC |
| ro | TQC | `readonly` (= `const`) type qualifier for NIFC |
| restrict | TQC | type qualifier for NIFC |
| i | TC, TN | `int` builtin type |
| u | TC, TN | `uint` builtin type |
| f | TC, TN | `float` builtin type |
| c | TC, TN | `char` builtin type |
| bool | TC, TN | `bool` builtin type |
| void | TC, TN | `void` return type |
| ptr | TC, TN | `ptr` type contructor |
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
| inline | PP | `inline` proc annotation |
| noinline | PP | `noinline` proc annotation |
| attr | SUC | general attribute annoation |
| varargs | PP, TN | `varargs` proc annotation |
| was | PC | |
| selectany | PP | |
| pragmas | SU, SN | begin of pragma section |
| align | PP | |
| bits | PP | |
| vector | PC | |
| imp | SC | import declaration |
| nodecl | P | `nodecl` annotation |
| incl | SC, SN | `#include` statement or `incl` set operation |
| excl | SN | `excl` set operation |
| include | SN | `include` statement |
| import | SC, SN | `import` statement |
| from | SC, SN | `from` statement |
| importexcept | SC, SN | `importexcept` statement |
| export | SC, SN | `export` statement |
| comment | SC, SN | `comment` statement |
| discard | SC, SN | `discard` statement |
| try | SC, SN | `try` statement |
| raise | SC, SN | `raise` statement |
| onerr | SC | error handling statement |
| raises | PP | proc annotation |
| errs | PPC | proc annotation |
| static | PC, TN | `static` type or annotation |
| ite | G | if-then-else |
| graph | G | disjoint subgraph annotation |
| forbind | G | bindings for a `for` loop but the loop itself is mapped to gotos |
| kill | G | some.var is about to disappear (scope exit) |
| unpackflat | SUN | unpack into flat variable list |
| unpacktup | SUN | unpack tuple |
| except | SUN | except subsection |
| fin | SUN | finally subsection |
| refobj | TN | `ref object` type |
| ptrobj | TN | `ptr object` type |
| tuple | TN | `tuple` type |
| onum | TN | enum with holes type |
| ref | TN | `ref` type |
| mut | TN | `mut` type |
| out | TN | `out` type |
| lent | TN | `lent` type |
| sink | TN | `sink` type |
| nilt | TN | `nilt` type |
| concept | TN | `concept` type |
| distinct | TN | `distinct` type |
| itertype | TN | `itertype` type |
| rangetype | TN | `rangetype` type |
| uarray | TN | `uarray` type |
| openarray | TN | `openarray` type |
| sett | TN | `sett` type |
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
| quoted | X | name in backticks |
| hderef | X | hidden pointer deref operation |
| ddot | X | deref dot |
| haddr | X | hidden address of operation |
| newobj | X | new object constructor |
| tup | X | tuple constructor |
| set | X | set constructor |
| ashr | X | |
| oconv | X | object conversion |
| hconv | X | hidden basic type conversion |
| dconv | X | conversion between `distinct` types |
| callstrlit | X | |
| infix | X | |
| prefix | X | |
| hcall | X | hidden converter call |
| compiles | X | |
| declared | X | |
| defined | X | |
| high | X | |
| low | X | |
| typeof | X | |
| unpack | X | |
| enumtostr | X | |
| ismainmodule | X | |
| defaultobj | X | |
| defaulttup | X | |
| expr | X | |
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

| Name                 | Enums                       |   Description |
|----------------------|-----------------------------|---------------|
| err                  | C, X                        | indicates an error |
| suf                  | C, X                        | literal with suffix annotation |
| at                   | C, X | array indexing operation |
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
| and                  | C, X | boolean `and` operation |
| or                   | C, X | boolean `or` operation |
| not                  | C, X | boolean `not` operation |
| neg                  | C, X | negation operation |
| sizeof               | C, X | `sizeof` operation |
| alignof              | C, X | `alignof` operation |
| offsetof             | C, X | `offsetof` operation |
| oconstr              | C, X | object constructor |
| aconstr              | C, X | array constructor |
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
| call | C, X | call operation |
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
| emit | SC, SN | emit statement |
| asgn | SC, SN | assignment statement |
| scope | SC, SN | explicit scope annotation, like `stmts` |
| if | SC, SN | if statement header |
| elif | SU | pair of (condition, action) |
| else | SU | `else` action |
| break | SC, SN | `break` statement |
| while | SC, SN | `while` statement |
| case | SC, SN | `case` statement |
| of | SU | `of` branch within a `case` statement |
| lab | SC | label, target of a `jmp` instruction |
| jmp | SC | jump/goto instruction |
| ret | SC, SN | `return` instruction |
| stmts | SC, SN | list of statements |
| params | TC, TN | list of proc parameters, also used as a "proc type" |
| proc | SC, SN, Y | proc declaration |
| fld | SU | field declaration |
| union | TC | union declaration |
| object | TC, TN | object type declaration |
| efld | SU | enum field declaration |
| enum | TC, TN | enum type declaration |
| proctype | TC, TN | proc type declaration (soon obsolete, use params instead) |
| atomic | TQC | `atomic` type qualifier for NIFC |
| ro | TQC | `readonly` (= `const`) type qualifier for NIFC |
| restrict | TQC | type qualifier for NIFC |
| i | TC, TN | `int` builtin type |
| u | TC, TN | `uint` builtin type |
| f | TC, TN | `float` builtin type |
| c | TC, TN | `char` builtin type |
| bool | TC | `bool` builtin type |
| void | TC | `void` return type |
| ptr | TC, TN | `ptr` type contructor |
| array | TC, TN | `array` type constructor |
| flexarray | TC | `flexarray` type constructor |
| aptr | TC | "pointer to array of" type constructor |
| type | SC, SN | `type` declaration statement |
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
| varargs | PP | `varargs` proc annotation |
| was | PC | |
| selectany | PP | |
| pragmas | SU | begin of pragma section |
| align | PC | |
| bits | PC | |
| vector | PC | |
| imp | SC | import declaration |
| nodecl | P | `nodecl` annotation |
| incl | SC | `#include` statement |
| discard | SC, SN | `discard` statement |
| try | SC, SN | `try` statement |
| raise | SC, SN | `raise` statement |
| onerr | SC | error handling statement |
| raises | PP | proc annotation |
| errs | PPC | proc annotation |
| static | PC | `static` annotation |

NIFC dialect
============

NIFC is a dialect of [NIF](https://github.com/nim-lang/nifspec/blob/master/doc/nif-spec.md) designed to be very close to C. Its benefits are:

- Easier to generate than generating C/C++ code directly.
- Has all the NIF related tooling support.
- NIFC improves upon C's quirky array and pointer type confusion by clearly distinguishing
  between `array` which is always a value type, `ptr` which always points to a
  single element and `aptr` which points to an array of elements.
- Inheritance is modelled directly in the type system as opposed to C's quirky type aliasing
  rule that is concerned with aliasings between a struct and its first element.
- Leverages NIF's module system to avoid C/C++'s possible "one definition rule" violations: A symbol has one definition and can be used in client modules by simply referring to it by name.


Module system
-------------

NIFC uses NIF's module system. Read the [nifspec.md](https://github.com/nim-lang/nifspec/blob/master/doc/nif-spec.md) for more details.


Name mangling
-------------

Name mangling is performed by NIFC. The following assumptions are made:

- A NIF symbol has the form `identifier.<number>.modulesuffix` (if it is a top level entry)
  or `identifier.<number>` (for a local symbol). For example `replace.2.strutils` would be the
  2nd `replace` from `strutils`. Generic instances get a `.g` suffix.
- Symbols that are imported from C or C++ have `.c` as the `modulesuffix`. Note that via `\xx`
  a name can contain `::` which is required for C++ support. These symbols can have different
  names in Nim. The original names can be made available via a `was` annotation. See the
  grammar for further details.

For other names the `.` is replaced by `_` and `_` is encoded as `Q_`.

By design names not imported from C contain a digit somewhere and thus cannot conflict with
a keyword from C or C++.

Other characters or character combinations (whether they are valid in C/C++ identifiers
or not!) are encoded via this table:

| Character combination | Encoded as |
| --------- | -------------- |
| `Q`       | `QQ` (thanks to this rule `Q` is now available as an escape character) |
| `_`       | `Q_` |
| `[]=`    | `putQ` |
| `[]`     | `getQ` |
| `$`      | `dollarQ` |
| `%`      |  `percentQ` |
| `&`      | `ampQ` |
| `^` | `roofQ` |
| `!` | `emarkQ` |
| `?` | `qmarkQ` |
| `*` | `starQ` |
| `+` | `plusQ` |
| `-` | `minusQ` |
| `/` | `slashQ` |
| `\` | `bslashQ` |
| `==` | `eqQ` |
| `=` | `eQ` |
| `<=` | `leQ` |
| `>=` | `geQ` |
| `<` | `ltQ` |
| `>` | `gtQ` |
| `~` | `tildeQ` |
| `:` | `colonQ` |
| `@` | `atQ` |
| `\|` | `barQ` |
| Other | `XxxQ` where `xx` is the hexadecimal value |


Grammar
-------

Generated NIFC code must adhere to this grammar. For better readability `'('` and `')'` are written
without quotes and `[]` is used for grouping.

```
Empty ::= <according to NIF's spec>
Identifier ::= <according to NIF's spec>
Symbol ::= <according to NIF's spec>
SymbolDef ::= <according to NIF's spec>
Number ::= <according to NIF's spec>
CharLiteral ::= <according to NIF's spec>
StringLiteral ::= <according to NIF's spec>
IntBits ::= ('+' | '-') [0-9]+

Lvalue ::= Symbol | (deref Expr (cppref)?) |
             (at Expr Expr) | # array indexing
             (dot Expr Symbol Number) | # field access
             (pat Expr Expr) | # pointer indexing
             (errv) | (ovf)

Call ::= (call Expr+)
CallCanRaise ::= (onerr Stmt Expr+)

ArithExpr ::= (add Type Expr Expr) |
         (sub Type Expr Expr) |
         (mul Type Expr Expr) |
         (div Type Expr Expr) |
         (mod Type Expr Expr)

Expr ::= Number | CharLiteral | StringLiteral |
         Lvalue |
         (par Expr) | # wraps the expression in parentheses
         (addr Lvalue (cppref)?) | # "address of" operation
         (nil) | (false) | (true) |
         (inf) | (neginf) | (nan) |
         (and Expr Expr) | # "&&"
         (or Expr Expr) | # "||"
         (not Expr) | # "!"
         (neg Type Expr) |
         (sizeof Type) |
         (alignof Type) |
         (offsetof Type SYMBOL) |
         (oconstr Type (kv Symbol Expr)*) |  # (object constructor){...}
         (aconstr Type Expr*) |              # array constructor
         ArithExpr |
         (shr Type Expr Expr) |
         (shl Type Expr Expr) |
         (bitand Type Expr Expr) |
         (bitor Type Expr Expr) |
         (bitnot Type Expr) |
         (bitxor Type Expr Expr) |
         (eq Expr Expr) |
         (neq Expr Expr) |
         (le Expr Expr) |
         (lt Expr Expr) |
         (cast Type Expr) |
         (conv Type Expr) |
         Call |
         CallCanRaise

BranchValue ::= Number | CharLiteral | Symbol | (true) | (false)
BranchRange ::= BranchValue | (range BranchValue BranchValue)
BranchRanges ::= (ranges BranchRange+)

VarDeclCommon ::= SymbolDef VarPragmas Type [Empty | Expr]
VarDecl ::= (var VarDeclCommon) | # local variable
            (gvar VarDeclCommon) | # global variable
            (tvar VarDeclCommon) # thread local variable

ConstDecl ::= (const SymbolDef VarPragmas Type Expr)
EmitStmt ::= (emit Expr+)
TryStmt ::= (try StmtList StmtList StmtList)
RaiseStmt ::= (raise [Empty | Expr])
AsgnStmt ::= (asgn Lvalue Expr)
KeepOverflowStmt ::= (keepovf ArithExpr Lvalue)
IfStmt ::= (if (elif Expr StmtList)+ (else StmtList)? )
WhileStmt ::= (while Expr StmtList)
LoopStmt ::= (loop StmtList Expr StmtList) # pre-condition stmts, condition, body
CaseStmt ::= (case Expr (of BranchRanges StmtList)* (else StmtList)?)
LabelStmt ::= (lab SymbolDef)
JumpStmt ::= (jmp Symbol)
ScopeStmt ::= (scope StmtList)
DiscardStmt ::= (discard Expr)
StoreStmt ::= (store Expr Lvalue) # like AsgnStmt but with reversed operands
IteStmt ::= (ite Expr StmtList [Empty | StmtList]) # if-then-else
ItecStmt ::= (itec Expr StmtList [Empty | StmtList]) # same as ite
MflagDecl ::= (mflag SymbolDef) # materialized control flow flag (bool)
VflagDecl ::= (vflag SymbolDef) # virtual control flow flag (bool)
JtrueStmt ::= (jtrue Symbol+) # set flags to true; last flag becomes a goto target

Stmt ::= Call |
         CallCanRaise |
         VarDecl |
         ConstDecl |
         EmitStmt |
         TryStmt |
         RaiseStmt |
         AsgnStmt |
         StoreStmt |
         KeepOverflowStmt |
         IfStmt |
         IteStmt |
         ItecStmt |
         WhileStmt |
         LoopStmt |
         (break) |
         CaseStmt |
         LabelStmt |
         JumpStmt |
         ScopeStmt |
         MflagDecl |
         VflagDecl |
         JtrueStmt |
         (ret [Empty | Expr]) | # return statement
         DiscardStmt


StmtList ::= (stmts SCOPE Stmt*)

Param ::= (param SymbolDef ParamPragmas Type)
Params ::= Empty | (params Param*)

ProcDecl ::= (proc SymbolDef Params Type ProcPragmas [Empty | StmtList])

FieldDecl ::= (fld SymbolDef FieldPragmas Type)

UnionDecl ::= (union [FieldDecl | AnonObjDecl | UnionDecl]*)
AnonObjDecl ::= (object Empty [FieldDecl | AnonObjDecl | UnionDecl]*)
ObjDecl ::= (object [Empty | Type] [FieldDecl | AnonObjDecl | UnionDecl]*)
EnumFieldDecl ::= (efld SymbolDef Number)
EnumDecl ::= (enum Type EnumFieldDecl+)

ProcType ::= (proctype Empty Params Type ProcTypePragmas)

IntQualifier ::= (atomic) | (ro)
PtrQualifier ::= (atomic) | (ro) | (restrict)

Type ::= Symbol |
         (i IntBits IntQualifier*) |
         (u IntBits IntQualifier*) |
         (f IntBits IntQualifier*) |
         (c IntBits IntQualifier*) | # character types
         (bool IntQualifier*) |
         (void) |
         (varargs) |
         (ptr Type PtrQualifier* (cppref)?) | # pointer to a single object
         (flexarray Type) |
         (aptr Type PtrQualifier*) | # pointer to an array of objects
         ProcType
ArrayDecl ::= (array Type Expr)
TypeDecl ::= (type SymbolDef TypePragmas [ProcType | ObjDecl | UnionDecl | EnumDecl | ArrayDecl])

CallingConvention ::= (cdecl) | (stdcall) | (safecall) | (syscall)  |
                      (fastcall) | (thiscall) | (noconv) | (member)

Attribute ::= (attr StringLiteral)

CommonPragmas ::= (was Identifier) | Attribute | (importc StringLiteral?) |
                  (importcpp StringLiteral?) | (exportc StringLiteral?) | (nodecl)

ProcPragma ::= CommonPragmas | (inline) | (noinline) | CallingConvention | (selectany) |
            | (raises) | (errs)

ProcTypePragma ::= CallingConvention | Attribute

ProcTypePragmas ::= Empty | (pragmas ProcTypePragma+)
ProcPragmas ::= Empty | (pragmas ProcPragma+)

CommonPragma ::= (align Number) | CommonPragmas
VarPragma ::= CommonPragma | (static)
VarPragmas ::= Empty | (pragmas VarPragma+)

ParamPragma ::= CommonPragmas
ParamPragmas ::= Empty | (pragmas ParamPragma+)

FieldPragma ::= CommonPragma | (bits Number)
FieldPragmas ::= (pragmas FieldPragma+)

TypePragma ::= CommonPragma | (vector Number)
TypePragmas ::= Empty | (pragmas TypePragma+)


TopLevelConstruct ::= ProcDecl | VarDecl | ConstDecl |
                      TypeDecl | EmitStmt | Call | CallCanRaise |
                      TryStmt | RaiseStmt | AsgnStmt | KeepOverflowStmt |
                      IfStmt | WhileStmt | CaseStmt | LabelStmt | JumpStmt |
                      ScopeStmt | DiscardStmt

Module ::= (stmts TopLevelConstruct*)

```

Notes:

- `IntBits` is either 8, 16, 32, 64, etc. or `-1` which stands
  for machine word size.
- There can be more calling conventions than only `cdecl` and `stdcall`.
- `case` is mapped to a `switch` but the generation of `break` is handled
  automatically.
- `ro` stands for `readonly` and is C's notion of the `const` type qualifier.
  Not to be confused with NIFC's `const` which introduces a named constant.
- C allows for `typedef` within proc bodies. NIFC does not, a type declaration must
  always occur at the top level.
- String literals within `emit` produce verbatim C code, not a C string literal.
- For `array` the element type comes before the number of elements.
  Reason: Consistency with pointer types.
- `proctype` has an Empty node where `proc` has a name so that the parameters are
  always the 2nd child followed by the return type and calling convention. This
  makes the node structure more regular and can simplify a type checker.
- `varargs` is modelled as a special type but it must be combined with a named parameter
  just like any other parameters.
- The type `flexarray` can only be used for a last field in an object declaration.
- The pragma `selectany` can be used to merge proc bodies that have the same name.
  It is used for generic procs so that only one generic instances remains in the
  final executable file.
- `attr "abc"` annotates a symbol with `__attribute__(abc)`.
- `cast` is mapped to a C type cast. Bit reinterpretation casts (e.g. `int` to `float`)
  are lowered to `copyMem` calls before reaching NIFC.
- `conv` is a value preserving type conversion between numeric types.
- `array` is mapped to a struct with an array inside so that arrays gain value semantics.
  Hence arrays can only be used within a `type` environment and are nominal types.
  A NIFC code generator has to ensure that e.g. `(type :MyArray.T . (array T 4))` is only
  emitted once.
- `type` can only be used to introduce a name for a nominal type (that is a type which
  is only compatible to itself) or for a proc type for code compression purposes. Arbitrary
  aliases for types **cannot** be used! Rationale: Implementation simplicity.
- `nodecl` is a pragma that indicates that the declaration should not be emitted in the resulting C/C++ code.
- `var` is always a local variable, `gvar` is a global variable and `tvar` a thread local
  variable.
- `SCOPE` indicates the construct introduces a new local scope for variables.
- `cppref` is a pragma that indicates that the pointer should be translated into a C++
  reference (`T&`). The `(deref)` and `(addr)` operations are still mandatory then and
  must be annotated with `(cppref)` too. `(cppref)` can be combined with `(ro)` to produce
  a `const T&` reference.


Scopes
------

NIFC effectively has the same scoping rules as C: C's `{ ... }` curly brackets introduce
a new local scope and so does NIFC's `StmtList` non-terminal. The grammar indicates with
a `SCOPE` keyword that otherwise has no meaning and should be ignored by a parser.

Extra scopes can be introduced with the `(scope ...)` construct which produces `{}`. Note
that a `scope` cannot be exited via `break`, `scope` is not like Nim's `block` statement
in this regard!

Scopes can be used by primitive code generators to produce significantly better code.


Inheritance
-----------

NIFC directly supports inheritance in its object system. However, no runtime checks are implied
and RTTI must be implemented manually, if required.

- The `object` declaration allows for inheritance. Its first child is the
  parent class (or empty).
- The `dot` operation takes a 3rd parameter which is an "inheritance" number. Usually
  it is `0` but if the field is in the parent's object it is `1` and it is `2`
  for the parent's parent and so on.


Declaration order
-----------------

NIFC allows for an arbitrary order of declarations without the need for forward declarations.


Include files
-------------

NIFC generates the required include files by inspecting the `(header)` pragmas.


Exceptions
----------

NIFC supports two kinds of exception handling primitives.

- `try` and `raise` Constructs: These must be used in C++ mode and are translated into their C++ equivalents. It is the NIFC caller’s responsibility to ensure they are not emitted when C++ support is disabled. The `try` construct follows the format `(try <actions> <onerr> <finally>)`. The `raise` construct can generate a C++ `throw` statement.

- `errv` and `onerr` Constructs: These have to be used when C++ code is not generated. Calls that may raise an exception must be wrapped in `(onerr)`. The format is `(onerr <action> <f> <args>)`, where action is typically a `jmp` instruction. In C++ exception handling mode, action should always be a dot `.`. The special variable `(errv)` of type `bool` can be set using `(asgn)` and queried like other locations, e.g., `(asgn (errv) (true)) # set the error bit`.

Functions can and must be annotated with a `(raises)` pragma to indicate that they can raise a C++ exception. Likewise, they need to use the `errs` pragma if they use the `(errv)` mechanism. A function can use both annotations at the same time. That would be a C++ function that uses both `(raise)` and `(errv)`.


Overflow checking
-----------------

NIFC supports overflow checking for arithmetic operations. The `(ovf)` tag is used
to access the overflow flag. Arithmetic operations subject to overflow checking must
be done with the `(keepovf)` construct:

```
(var :x.0 . (i +32) .)
(var :a.0 . (i +32) +90)
(var :b.0 . (i +32) +223231343)
(keepovf (add (i +32) a.0 b.0) x.0)
(if (elif (ovf) (stmts (asgn (ovf) (false)) (call printf.c "overflow")))
    (else (call printf.c "no overflow")))
```

`keepovf` can be read as a form of tuple assignment: `(overflowFlag, x.0) = a.0 + b.0`.
As `keepovf` is a statement and not an expression, a code generator typically has to
introduce temporaries for nested expressions. This is also required for
GCC's `__builtin_saddll_overflow` construct and is the real reason for this strange design.

The `(ovf)` flag is an lvalue and can be set to `(false)` to reset the flag:

```
(asgn (ovf) (false))
```

This is not optional! It is required for reliable native code generation.


NJ instructions
---------------

NIFC has grown support for NJ ("No Jumps") instructions. These are lower-level control flow
primitives that can be more efficient than structured `if`/`while` statements.

### store

`(store Expr Lvalue)` is like `(asgn Lvalue Expr)` but with reversed operand order.
The RHS is listed first so that the evaluation order matches the textual order, which
is important when the RHS has side effects.

### ite and itec

`(ite Expr StmtList [Empty | StmtList])` is a simplified if-then-else. Unlike `(if)` which
supports multiple `elif` branches, `ite` has exactly one condition, one then-branch and an
optional else-branch.

`(itec)` has the same structure and semantics as `(ite)`.

### loop

`(loop StmtList Expr StmtList)` is a loop with a pre-condition block. The first `StmtList`
runs before the condition is tested. If the condition is false, the loop breaks. Then the
body `StmtList` runs.

### mflag and vflag

`(mflag SymbolDef)` declares a **materialized** control flow flag. It is a boolean variable
initialized to `false`. Its value is observable at runtime and generates a real C variable
of type `NB8`.

`(vflag SymbolDef)` declares a **virtual** control flow flag. It is also a boolean initialized
to `false`, but the code generator can optimize it away entirely. When `(ite (not vflagSym) ...)`
is used with a vflag, the code generator knows the condition is always true and emits only the
then-branch, eliminating the `if` statement.

### jtrue

`(jtrue Symbol+)` sets the listed flag symbols to `true`. The last symbol in the list is
special: it becomes a `goto` target. This allows the code generator to turn a flag-set
followed by a conditional check into a direct jump.

### The (lab) marker in ite

The last usage of a virtual flag in an `(ite)` condition must be annotated with `(lab)`.
This tells the code generator that the virtual flag's elimination is complete and a label
should be emitted at this point. This label serves as the target for the `goto` generated
by `(jtrue)`.

Example:

```
(vflag :x.0)
(jtrue x.0)           ;; becomes: goto x_0;
;; ...
(ite (not (lab) x.0)  ;; (lab) marks last usage, emits label x_0:
  (stmts ...)         ;; then-branch is always taken
  .)                  ;; else-branch is ignored
```

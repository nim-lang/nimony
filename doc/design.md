>      I know a great deal, body guard. I recognise those marks on the back of your hand...
>      A gift from your friend. The one who talks to you in the dark. Talks to you when you
>      visit his shrines. I've visited those shrines, too. But I don't know you, who you are,
>      and who you fight for. You're a mystery, and I can't allow that.
>     -- "Daud"

# Goals

Version 3 will be achieved via a combination of compiler phase rewrites, code reuse, refactorings and porting of compiler code. The primary goal is to finally give us a Nim that offers:

1. Incremental recompilations.
2. No forward declarations for procs and types required.
3. Allow for explicit cyclic module dependencies.
4. Type-checked generics.
5. Avoid the phase ordering problems that plagued Nim for a long time: Destructors and other `=hooks` can be invoked before they have been synthesized successfully which is hard for users to understand.



# Implementation

The implementation uses [NIF](https://github.com/nim-lang/nifspec/blob/master/doc/nif-spec.md) for everything: It is the data format used for communication between the different compiler phases.

The typical compiler pass works on streams of NIF tokens, no tree constructions are required.

Arguably a token stream enforces a principled approach to compiler development where by design subtrees cannot be forgotten to be traversed and handled.

The phases of compilation are:

1. Pure parsing (nifler): Turn Nim code into a dialect of NIF.
2. Semantic checking phase 1 (nimony): symbol lookups, type checking, template&macro expansions.
3. Semantic checking phase 2 (nimony): Effect inference.
4. Inject derefs (and the corresponding mutation checking) (nimony).
5. Iterator inlining (hexer).
6. Lambda lifting (hexer). **Not implemented yet.**
7. Inject dups (hexer).
8. Lower control flow expressions to control flow statements (elminate the expr/nkStmtListExpr construct) (hexer).
9. Inject destructors (hexer).
10. Map builtins like `new` and `+` to "compiler procs" (hexer).
11. Translate exception handling (hexer). **Not implemented yet.**
12. Generate NIFC code (hexer).

These phases have been collected into different tools with dedicated names.


## NIF

NIF is a general purpose text format designed for compiler construction. Think of it as a "JSON for compilers". NIF has a short precise specification and a Nim library implementation.

While NIF is almost a classical Lisp, it innovates in these aspects:
1. It uses a separate namespace for tags ("builtins") and source code identifiers. It is most  extensible and supports a wide range of abstraction levels. Code that is very high level can be represented effectively as well as code that is close to machine code.
2. Line information is carried around during all phases of compilation for easy debugging and code introspection tools. The line information is based on the difference between a parent and its child node so that the resulting text representation is kept small.
3. Declaration and use of a symbol are clearly distinguished in the syntax allowing for many different tasks to be implemented with a fraction of the usual complexity: "find definition", "find all uses" and "inline this code snippet" are particularly easy to implement.
4. There is an additional format called Nif-index that allows for the lazy on-demand loading of symbols. This is most essential for incremental compilations.


## Nifler

The Nifler tool encapsulates the initial Nim-to-NIF translation step and is generally useful for other tools that want to process Nim code without importing the Nim compiler as a library.

Nifler can also evaluate Nim's configuration system, the `nim.cfg` files and the NimScript files so that tools like `nimsuggest` get precise `--path` information and configuration settings without having to import the Nim compiler as a library.

<div style="page-break-after: always;"></div>

## Nimony

The primary point of Nifler is to shield "Nimony" from Nim's compiler internals. Nimony is a new frontend for Nim, designed for:

- Low memory consumption.
- Incremental compilation.
- Tooling. The compiler continues after an error and supports "find all usages" and "goto definition"
  which work much more reliably since generics and macros are type-checked too.
- Efficient handling of code bases that make heavy use of **generics, type computations and macros**.
- Reducing the bug count by orders of magnitute. A "Minimal Redundancy Internal Representation" is used, ensuring that there is only one access path to a piece of data. The internally used data structures cannot get out of sync.


## Hexer

The hexer's job is to "lower" high level Nim code to low level Nim code that does not use features such as closures, iterators and automatic memory management. It is planned to only support Nim's ARC/ORC scheme of doing memory management. In the old compiler ARC/ORC was very complex to support as the problem was not as well understood as it is now: A key insight here is to split up the tasks into multiple well-defined subtasks:

1. Inject dups/copies: This can produce weird constructs like `while (;let tmp = f(); tmp.value)`.
2. Lower control flow *expressions* to *statements*. This means `if` and `case` do not produce values anymore.
3. Inject destructors: Now that values have been bound to temporaries explicitly and the control flow has been simplified it is rather easy to inject `=destroy(x)` calls at scope exists.

As previously mentioned, the hexer also does:

- Map builtins to Nim's runtime.
- Iterator inlining.
- Eliminate closures by performing "lambda lifting".
- Inject pointer derefs and implement "pass by reference".
- Translate exception handling constructs to NIFC's supported error handling.
- The final NIFC code generation.

<div style="page-break-after: always;"></div>

### NIFC code generator

The primary task left for the NIFC generator is "expansion". It performs backend tasks that need to operate on multiple NIF files at once:

- It copies used imported symbols into the current NIF file. As a fix point operation
  until no foreign symbols are left.
- `importc`'ed symbols are replaced by their `.c` variants.
- `importc`'ed symbols might lead to `(incl "file.h")` injections.
- Nim types must be translated to NIFC types.
- Types and procs must be moved to toplevel statements.


## NIFC: C/C++ Backends based on NIF

NIFC is a dialect of NIF designed to be very close to C. Its benefits are:

- NIFC is easier to generate than generating C/C++ code directly because:
  1. It uses NIF's regular syntax.
  2. It allows for an arbitrary order of declarations without the need for forward declarations.
- NIFC improves upon C's quirky array and pointer type confusion by clearly distinguishing
  between `array` which is always a value type, `ptr` which always points to a
  single element and `aptr` which points to an array of elements.
- Inheritance is modelled directly in the type system as opposed to C's quirky type aliasing
  rule that is concerned with aliasings between a struct and its first element.
- NIFC can also produce C++ code without information loss because inheritance and exception handling are directly supported.

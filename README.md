# Nimony

Nimony is a new Nim implementation that is in heavy development. The goal is a production-ready compiler this year, for projects that can live with the documented feature set -- don't assume Nim 2's feature set! Read what is available first!

## Highlights

- Incremental recompilations.
- Fully parallel builds.
- No forward declarations for procs and types required.
- Type-checked generics.
- Unified concurrency and parallelism model via continuations, so-called [passive procs](doc/passive_procs.md).
- Good editor support.

Read more about the [design philosophy](https://nim-lang.org/araq/nimony.html).


## Installation

Clone Nimony and build with `hastur`:

```
git clone https://github.com/nim-lang/nimony.git
cd nimony
nim c -r src/hastur build all
```

This builds the compiler and all supporting tools into `bin/`. Add `bin/` to your `PATH` to use the compiler and tools.


## First program

`echo` lives in `std/syncio`, so the hello world program is:

```nim
import std/syncio

echo "hello, world"
```

Compile and run:

```
bin/nimony c -r hello.nim
```


## Standard library

The stdlib source is in [lib/std/](lib/std/) and is designed to be read directly — Nim is readable enough that type signatures often suffice as documentation.

For the parts where signatures aren't enough — behavioral contracts, performance traps, design rationale — there is a curated [**Standard Library Guide**](doc/stdlib.md).

The [examples/](examples/) directory contains runnable Nim files that serve as both usage examples and tests. Run them with `hastur examples`.


## Language manual

The [language manual](doc/manual.md) covers syntax, semantics, and the type system.

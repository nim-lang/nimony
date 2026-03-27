# nimony

Nimony is a new Nim implementation that is in heavy development. See the [manual](doc/manual.md) for up to date documentation.

There is a [blog post](https://nim-lang.org/araq/nimony.html) about its design principles.

The current focus is on developing a compiler for a Nim dialect that offers:

- Incremental recompilations.
- Fully parallel builds.
- No forward declarations for procs and types required.
- Type-checked generics.
- Good editor support.

AI has created a good overview of our [compiler architecture](https://deepwiki.com/nim-lang/nimony).


## Getting started

Clone Nimony:

```
git clone https://github.com/nim-lang/nimony.git
cd nimony
```

Nimony uses a tool called `hastur` to build:

```
nim c -r src/hastur build all
```


## Hello World

`echo` is not part of `system.nim` anymore, so the hello world program is:

```nim

import std / syncio

echo "hi"
```


## Documentation

- [**Standard Library Guide**](doc/stdlib.md) — covers the non-obvious parts of the stdlib: behavioral contracts, performance guidance, gotchas, and design decisions that aren't apparent from the type signatures alone.
- [**Examples**](examples/) — runnable Nim files that serve as both usage examples and tests. Run them with `hastur examples`.
- **Source code** — Nim is readable by design. The [lib/std/](lib/std/) directory contains the stdlib source; the guide links to it directly.

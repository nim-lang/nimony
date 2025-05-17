# nimony

Nimony is a new Nim implementation that is in heavy development. See [design.md](https://github.com/nim-lang/nimony/blob/master/doc/design.md) for the big picture.
There is a [blog post](https://nim-lang.org/araq/nimony.html) about its design principles.

The current focus is on developing a compiler for a Nim dialect that offers:

- Incremental recompilations.
- No forward declarations for procs and types required.
- Type-checked generics.

AI has created a good overview of our [compiler architecture](https://deepwiki.com/nim-lang/nimony).


## Getting started

Nimony uses a tool called `hastur` to build:

```
nim c src/hastur
src/hastur build nifler
src/hastur build nimony
src/hastur build hexer
src/hastur build nifc
```



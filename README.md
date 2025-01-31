# nimony

Nimony is a new Nim implementation that is in heavy development. See [design.md](https://github.com/nim-lang/nimony/blob/master/doc/design.md) for the big picture.

The current focus is on developing a minimal compiler for a Nim dialect that offers: 

- Incremental recompilations.
- No forward declarations for procs and types required.
- Allow for explicit cyclic module dependencies.
- Type-checked generics.

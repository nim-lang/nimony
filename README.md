# nimony

Nimony is a new Nim implementation that is in heavy development. See https://github.com/nim-lang/RFCs/issues/556 for the big picture.

The current focus is on developing a minimal compiler for a Nim dialect that offers: 

- Incremental recompilations.
- No forward declarations for procs and types required.
- Allow for explicit cyclic module dependencies.
- Type-checked generics.

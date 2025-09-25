# nimony

Nimony is a new Nim implementation that is in heavy development. See the [manual](https://nim-lang.github.io/nimony-website/) for up to date documentation. See [design.md](https://github.com/nim-lang/nimony/blob/master/doc/design.md) for lots of implementation details.
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

# Installation

Nimony must be built from source. You need to have Nim version 2.0 or later. For example:

```
nim --version
Nim Compiler Version 2.3.1
```

To build it run these commands:

```
nim c src/hastur
src/hastur build nifler
src/hastur build nimony
src/hastur build hexer
src/hastur build nifc
src/hastur build nifmake
```

These are also the names of the binaries that should be in `bin/`.

# Usage

```
nimony c <program.nim>
```


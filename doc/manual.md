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

# Configuration

With the `--compat` switch Nimony completely supports Nim's `nim.cfg` and NimScript configuration mode. However, as it is a messy legacy system its usage for new projects is discouraged. Instead the new configuration system should be used. It has been designed with tooling in mind. There are different files that manage different aspects of the configuration:

1. `nimony.args` is a text file that contains command line arguments that are processed as if they were passed to the command line.
2. `nimony.paths` is a text file where every line is an entry to the search `--path`. This is so important for tooling that it became a separate file.
3. `nimony.system` is a text file that contains the path to `system.nim`.
4. `gcc.args` is a text file that contains command line arguments that are passed to GCC. Likewise `clang.args` exists for Clang and in general `$tool.args` exists for other tools that are invoked via `nifmake`.

`.args` files are processed before the real command line arguments are processed so that they can be overridden.

These files are searched for in the directory of the `<program>.nim` file and if not found in its parent directories. Only the first file found is used. The idea here is that nobody (neither humans nor tools) needs to perform a "merge" operation of different configuration files.

# Installation

Nimony must be built from source. You need to have Nim version 2.0 or later. For example:

```
nim --version
Nim Compiler Version 2.3.1
```

Clone Nimony from github:

```
git clone https://github.com/nim-lang/nimony.git
cd nimony
```


To build it run this command:

```
nim c -r src/hastur build all
```


# Usage

```
nimony c <program.nim>
```

For example:

```nim
import std / syncio

echo "Hello from Nimony!"
```


# Configuration

## `--compat` switch

With the `--compat` switch Nimony completely supports Nim's `nim.cfg` and NimScript configuration mode. However, as it is a messy legacy system its usage for new projects is discouraged. Instead the "args configuration system" should be used. It has been designed with tooling in mind.


## Args configuration system

There are different files that manage different aspects of the configuration:

1. `nimony.args` is a text file that contains command line arguments that are processed as if they were passed on the command line.
2. `nimony.paths` is a text file where every line is an entry to the search `--path`. This is so important for tooling that it became a separate file.
3. `$cc.args` is a text file that contains command line arguments that are passed to the used C compiler. `$cc` here stands for a general C compiler key. This key is extracted from the `--cc` command line option.
4. `$linker.args` is a text file that contains command line arguments that are passed to the used linker. `$linker` here stands for a general linker key. This key is extracted from the `--linker` command line option. If `--linker` is not used the C compiler command is used for linking.

`.args` files are processed before the real command line arguments are processed so that they can be overridden. An `.args` file can contain newlines as whitespace. Lines starting with `#` are comments. The POSIX command line parsing rules are used: Whitespace enclosed within single or double quotes is kept as is and the quotes are removed.

These files are searched for in the directory of the `<program>.nim` file and if not found in its parent directories. Only the first file found is used. The idea here is that nobody (neither humans nor tools) needs to perform a "merge" operation of different configuration files.


## The `--cc` command line option

The `--cc` switch supports a general command prefix that can be as simple as `gcc` or as complex as `/usr/bin/arm-linux-gnueabihf-gcc -Wall`.

From the command prefix a "key" is extracted automatically. This key is then used to construct an `.args` file that can be used to configure the toolchain further. Examples:

- `/usr/bin/gcc` → `gcc.args`
- `/usr/bin/arm-linux-gnueabihf-gcc` → `arm-linux-gnueabihf-gcc.args`
- `/usr/bin/x86_64-w64-mingw32-gcc.exe -Wall` → `x86_64-w64-mingw32-gcc.args`
- `/usr/local/bin/clang` → `clang.args`

This way there is no hardcoded list of C compilers. In fact, C++ or an LLVM based backend is naturally supported too.


## The `--linker` command line option

If the `--linker` command line option **is not** used, the C compiler's executable will also be used for linking. The used `.args` file is then `$cc.linker.args`.

If the `--linker` command line option **is** used, the specified executable will be used for linking. As for the `--cc` option, the used `.args` file will then be `$linkerName.linker.args`. For example `--linker:gold.exe` will look for a configuration file `gold.linker.args`.

.. include:: language.md
.. include:: stdlib.md

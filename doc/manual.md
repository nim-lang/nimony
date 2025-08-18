# Installation

Nimony must be built from source. You need to have Nim version 2.0 or later. For example:

```
nim --version
Nim Compiler Version 2.3.1
```

To build it run this command:

```
nim c -r src/hastur build all
```


# Usage

```
nimony c <program.nim>
```

# Configuration

## `--compat` switch

With the `--compat` switch Nimony completely supports Nim's `nim.cfg` and NimScript configuration mode. However, as it is a messy legacy system its usage for new projects is discouraged. Instead the "args configuration system" should be used. It has been designed with tooling in mind.


## Args configuration system

There are different files that manage different aspects of the configuration:

1. `nimony.args` is a text file that contains command line arguments that are processed as if they were passed on the command line.
2. `nimony.paths` is a text file where every line is an entry to the search `--path`. This is so important for tooling that it became a separate file.
3. `$cc.args` is a text file that contains command line arguments that are passed to the used C compiler. `$cc` here stands for a general C compiler key. This key is extraced from the `--cc` command line option.
4. `$linker.args` is a text file that contains command line arguments that are passed to the used linker. `$linker` here stands for a general linker key. This key is extraced from the `--linker` command line option. If `--linker` is not used the C compiler command is used for linking.

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


# Language guide

## Lexical Analysis & Syntax

*Nimony* is based on Nim version 2.0, so all of its lexing rules and syntax rules apply here too. In fact Nim's manual starting from its [lexical analysis](https://nim-lang.org/docs/manual.html#lexical-analysis) and up to and including its [grammar](https://nim-lang.org/docs/manual.html#syntax-grammar) section are valid.

There is one important exception: **Nimony is case sensitive** like most other modern programming languages. The reason for this is implementation simplicity. This might also be changed in the future.


## Modules

A program consists of one or more modules. A module is a file that contains code. A module can `import` other modules. The `system` module is the only exception: It is always automatically imported. `system` contains all the "builtin" names like `int` or `string`.
A module's name can be used to disambiguate names. For example if both the `system` and a `foreign` module contain a `delay` proc, `system.delay` can be used to clarify that it is the `system` module's `delay` proc.


## Order of evaluation

The order of evaluation is currently undefined.

Rationale: Implementation simplicity.



## Constants and Constant Expressions

A constant is a symbol that is bound to the value of a constant expression. Constant expressions are restricted to depend only on the following categories of values and operations, because these are either built into the language or declared and evaluated before semantic analysis of the constant expression:

* literals
* built-in operators
* previously declared constants

Nimony offers no complex evalution engine for arbitrary compile-time computations. Instead the `.plugin` mechanism can be used.



## Types

All expressions have a single type that is inferred by the compiler. These inferred types are then checked against an explicitly given type:

```nim
var x: int = 1
```

For this example the compiler checks that `1` (which is of type `int`) is compatible with the explicitly given type `int`.

Users can define new types, for example:

```nim
type Coord = (int, int)
```

Defines a new type named `Coord` that is a tuple of two `int`s.


## Integer types

These integer types are pre-defined:

`int`
: the signed integer type; its has the
  same size as a pointer. This type should be used in general. An integer
  literal that has no type suffix is of this type if it is in the range
  `low(int32)..high(int32)` otherwise the literal's type is `int64`.

`int`\ XX
: additional signed integer types of XX bits use this naming scheme
  (example: int16 is a 16-bit wide integer).
  The current implementation supports `int8`, `int16`, `int32`, `int64`.
  Literals of these types have the suffix 'iXX.

`uint`
: the generic `unsigned integer`:idx: type; its size is platform-dependent and
  has the same size as a pointer. An integer literal with the type
  suffix `'u` is of this type.

`uint`\ XX
: additional unsigned integer types of XX bits use this naming scheme
  (example: uint16 is a 16-bit wide unsigned integer).
  The current implementation supports `uint8`, `uint16`, `uint32`,
  `uint64`. Literals of these types have the suffix 'uXX.
  Unsigned operations all wrap around; they cannot lead to over- or
  underflow errors.


`Automatic type conversion`:idx: is performed in expressions where different
kinds of integer types are used: the smaller type is converted to the larger.

A `narrowing type conversion`:idx: converts a larger to a smaller type (for
example `int32 -> int16`). A `widening type conversion`:idx: converts a
smaller type to a larger type (for example `int16 -> int32`). Only
widening type conversions are *implicit*:

  ```nim
  var myInt16 = 5i16
  var myInt: int
  myInt16 + 34     # of type `int16`
  myInt16 + myInt  # of type `int`
  myInt16 + 2i32   # of type `int32`
  ```



## Floating-point types

The following floating-point types are pre-defined:

`float`
: the generic floating-point type; its size is always 64 bits.
  This type should be used in general.

`float`\ XX
: an implementation may define additional floating-point types of XX bits using
  this naming scheme (example: `float64` is a 64-bit wide float). The current
  implementation supports `float32` and `float64`. Literals of these types
  have the suffix 'fXX.


Automatic type conversion in expressions with different kinds of floating-point
types is performed: See [Convertible relation] for further details. Arithmetic
performed on floating-point types follows the IEEE standard. Integer types are
not converted to floating-point types automatically and vice versa.

The IEEE exceptions (invalid, division by zero, overflow, underflow, inexact) are ignored during execution.

The compiler always uses the maximum precision available to evaluate
floating-point values during semantic analysis; this means expressions like
`0.09'f32 + 0.01'f32 == 0.09'f64 + 0.01'f64` are true.


## Boolean type

The boolean type is named `bool`:idx: in Nimony and can be one of the two
pre-defined values `true` and `false`. Conditions in `while`,
`if`, `elif`, `when`-statements need to be of type `bool`.

This condition holds:

  ```nim
  ord(false) == 0 and ord(true) == 1
  ```

The operators `not, and, or, xor, <, <=, >, >=, !=, ==` are defined
for the bool type. The `and` and `or` operators perform short-cut
evaluation. Example:

  ```nim
  while p != nil and p.name != "xyz":
    # p.name is not evaluated if p == nil
    p = p.next
  ```


The size of the bool type is one byte.


## Character type

The character type is named `char` in Nimony. Its size is one byte.
Thus, it cannot represent a UTF-8 character, but a part of it.



## Enumerations

Enumeration types define a new type whose values consist of the ones
specified. The values are ordered. Example:

  ```nim
  type
    Direction = enum
      north, east, south, west
  ```


Now the following holds:

  ```nim
  ord(north) == 0
  ord(east) == 1
  ord(south) == 2
  ord(west) == 3

  # Also allowed:
  ord(Direction.west) == 3
  ```

The implied order is: north < east < south < west. The comparison operators can be used
with enumeration types. Instead of `north` etc., the enum value can also
be qualified with the enum type that it resides in, `Direction.north`.

For better interfacing to other programming languages, the fields of enum
types can be assigned an explicit ordinal value. However, the ordinal values
have to be in ascending order. A field whose ordinal value is not
explicitly given is assigned the value of the previous field + 1.

An explicit ordered enum can have *holes*:

  ```nim
  type
    TokenType = enum
      a = 2, b = 4, c = 89 # holes are valid
  ```

However, it is then not ordinal anymore, so it is impossible to use these
enums as an index type for arrays. The procedures `inc`, `dec`, `succ`
and `pred` are not available for them either.


The compiler supports the built-in stringify operator `$` for enumerations.
The stringify's result can be controlled by explicitly giving the string
values to use:

  ```nim
  type
    MyEnum = enum
      valueA = (0, "my value A"),
      valueB = "value B",
      valueC = 2,
      valueD = (3, "abc")
  ```

As can be seen from the example, it is possible to both specify a field's
ordinal value and its string value by using a tuple. It is also
possible to only specify one of them.

Enum value names are overloadable, much like routines. If both of the enums
`T` and `U` have a member named `foo`, then the identifier `foo` corresponds
to a choice between `T.foo` and `U.foo`. During overload resolution,
the correct type of `foo` is decided from the context. If the type of `foo` is
ambiguous, a static error will be produced.

  ```nim  test = "nim c $1"

  type
    E1 = enum
      value1,
      value2
    E2 = enum
      value1,
      value2 = 4

  const
    Lookuptable = [
      E1.value1: "1",
      # no need to qualify value2, known to be E1.value2
      value2: "2"
    ]

  proc p(e: E1) =
    # disambiguation in 'case' statements:
    case e
    of value1: echo "A"
    of value2: echo "B"

  p value2
  ```


## String type

All string literals are of the type `string`. A string in Nimony is very
similar to a sequence of characters. One can retrieve the length with the
builtin `len` proc.

The assignment operator for strings always copies the string.
The `&` operator concatenates strings.

Strings are compared by their lexicographical order. All comparison operators
are available. Strings can be indexed like arrays (lower bound is 0). Unlike
arrays, they can be used in case statements:

  ```nim
  case paramStr(i)
  of "-v": incl(options, optVerbose)
  of "-h", "-?": incl(options, optHelp)
  else: write(stdout, "invalid command line option!\n")
  ```

Per convention, all strings are UTF-8 strings, but this is not enforced. For
example, when reading strings from binary files, they are merely a sequence of
bytes. The index operation `s[i]` means the i-th *char* of `s`, not the
i-th *unichar*.


## cstring type

The `cstring` type meaning `compatible string` is the native representation
of a string for the compilation backend. For the C backend the `cstring` type
represents a pointer to a zero-terminated char array
compatible with the type `char*` in ANSI C. Its primary purpose lies in easy
interfacing with C. The index operation `s[i]` means the i-th *char* of
`s`; however no bounds checking for `cstring` is performed making the
index operation unsafe.

A `$` proc is defined for cstrings that returns a string. Thus, to get a nim
string from a cstring:

  ```nim
  var str: string = "Hello!"
  var cstr: cstring = str
  var newstr: string = $cstr
  ```

`cstring` literals shouldn't be modified.

  ```nim
  var x = cstring"literals"
  x[1] = 'A' # This is wrong!!!
  ```

If the `cstring` originates from a regular memory (not read-only memory),
it can be modified:

  ```nim
  var x = "123456"
  prepareMutation(x) # call `prepareMutation` before modifying the strings
  var s: cstring = cstring(x)
  s[0] = 'u' # This is ok
  ```

`cstring` values may also be used in case statements like strings.


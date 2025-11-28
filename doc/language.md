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
* the call of a routine as long as the passed arguments could be evaluated at compile-time.

Nimony offers a complex evaluation engine for arbitrary compile-time computations. The restrictions for this mechanism are currently undocumented.



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

The following integer types are pre-defined:

| Type | Size | Range | Literal Suffix | Description |
|------|------|-------|----------------|-------------|
| `int` | platform-dependent | `low(int32)..high(int32)` or `low(int64)..high(int64)` etc. | none | Signed integer with same size as a pointer. This type should be used in general. |
| `int8` | 8 bits | -128..127 | `'i8` | 8-bit signed integer |
| `int16` | 16 bits | -32,768..32,767 | `'i16` | 16-bit signed integer |
| `int32` | 32 bits | -2,147,483,648..2,147,483,647 | `'i32` | 32-bit signed integer |
| `int64` | 64 bits | -9,223,372,036,854,775,808..9,223,372,036,854,775,807 | `'i64` | 64-bit signed integer |
| `uint` | platform-dependent | 0..`high(uint)` | `'u` | Unsigned integer with same size as a pointer |
| `uint8` | 8 bits | 0..255 | `'u8` | 8-bit unsigned integer |
| `uint16` | 16 bits | 0..65,535 | `'u16` | 16-bit unsigned integer |
| `uint32` | 32 bits | 0..4,294,967,295 | `'u32` | 32-bit unsigned integer |
| `uint64` | 64 bits | 0..18,446,744,073,709,551,615 | `'u64` | 64-bit unsigned integer |

**Note:** Unsigned operations all wrap around; they cannot lead to over- or underflow errors.


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

| Type | Size | Range | Literal Suffix | Description |
|------|------|-------|----------------|-------------|
| `float` | 64 bits | IEEE 754 double precision | none | Generic floating-point type. This type should be used in general. |
| `float32` | 32 bits | IEEE 754 single precision | `'f32` | 32-bit floating-point number |
| `float64` | 64 bits | IEEE 754 double precision | `'f64` | 64-bit floating-point number |


Automatic type conversion in expressions with different kinds of floating-point
types is performed: See [Convertible relation] for further details. Arithmetic
performed on floating-point types follows the IEEE standard. Integer types are
not converted to floating-point types automatically and vice versa.

The IEEE exceptions (invalid, division by zero, overflow, underflow, inexact) are ignored during execution.

The compiler always uses the maximum precision available to evaluate
floating-point values during semantic analysis; this means expressions like
`0.09'f32 + 0.01'f32 == 0.09'f64 + 0.01'f64` are true.


## Boolean type

The boolean type is named `bool`:idx: and can be one of the two
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

The character type is named `char`. Its size is one byte.
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
enums as an index type for arrays. The procs `inc`, `dec`, `succ`
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

  ```nim test

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

All string literals are of the type `string`. A string is very
similar to a sequence of characters. One can retrieve the length with the
builtin `len` proc.

Strings are value types, the assignment operator for strings copies or moves the string (depending on the context) but they do not have reference semantics.

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


The `&` operator concatenates strings. It is currently not efficient to concatenate many strings as this operation generates many intermediate strings (copy operations), use `system.concat` instead: `concat("abc", a, "def")` instead of `"abc" & a & "def"`.


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
  var cstr: cstring
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


### Array type

An array is a fixed sized container of elements. Each element in the array has the
same type.

Arrays always have a fixed length specified as a constant expression.

They can be indexed by any ordinal type.

An array type can be defined using the `array[size, T]` syntax, or using
`array[lo..hi, T]` for arrays that start at an index other than zero.

An array expression may be constructed by the array constructor `[]`. The element
type of this array expression is inferred from the type of the first element. All
other elements need to be implicitly convertible to this type.

An array constructor can have explicit indexes for readability:

  ```nim
  type
    Values = enum
      valA, valB, valC

  const
    lookupTable = [
      valA: "A",
      valB: "B",
      valC: "C"
    ]
  ```


### seq type

Sequences can be constructed by the array constructor `[]` in conjunction
with the array to sequence operator `@`. Another way to allocate space for a
sequence is to call the built-in `newSeq` proc.

A sequence may be passed to a parameter that is of type *open array*.

Example:

  ```nim
  type
    IntArray = array[0..5, int] # an array that is indexed with 0..5
    IntSeq = seq[int] # a sequence of integers
  var
    x: IntArray
    y: IntSeq
  x = [1, 2, 3, 4, 5, 6]  # [] is the array constructor
  y = @[1, 2, 3, 4, 5, 6] # the @ turns the array into a sequence

  let z = [1.0, 2, 3, 4] # the type of z is array[0..3, float]
  ```

The lower bound of an array or sequence may be received by the built-in proc
`low()`, the higher bound by `high()`. The length may be
received by `len()`. `low()` for a sequence or an open array always returns
0, as this is the first valid index.
One can append elements to a sequence with the `add()` proc,
and remove (and get) the last element of a sequence with the `pop()` proc.

The notation `x[i]` can be used to access the i-th element of `x`.



### openArray type

An `openArray` is a *slice* into a contiguous container such as an array or
a `seq`. As a slice it does not own the data and so it must be used carefully.
An `openArray` is most often used as a parameter. It is indexed by
integers from 0 to `len(A)-1`.

`array` and `seq` have attached `converters` that allow these to be passed
to open arrays without any explicit operation:

  ```nim
  proc testOpenArray(x: openArray[int]) = echo repr(x)

  testOpenArray([1,2,3])  # array[]
  testOpenArray(@[1,2,3]) # seq[]
  ```

### Unchecked arrays
The `UncheckedArray[T]` type is a special kind of `array` where its bounds
are not checked. This is often useful to implement customized flexibly sized
arrays. Additionally, an unchecked array is translated into a C array of
undetermined size:

  ```nim
  type
    MySeq = object
      len, cap: int
      data: UncheckedArray[int]
  ```

Produces roughly this C code:

  ```C
  typedef struct {
    NI len;
    NI cap;
    NI data[];
  } MySeq;
  ```


### varargs

`varargs` is a pragma that can be applied to `importc` procs to indicate
the proc takes a variable number of arguments (`printf` being the typical
example). It can also be applied to a template. Inside a template the args
can then be unpacked like so:

```nim
template concat(): string {.varargs.} =
  var res = ""
  for a in unpack():
    res.add a
  res
```

Nimony's `echo` also uses `varargs`:

```nim
template echo*() {.varargs} =
  for a in unpack(): stdout.write a
  stdout.writeLine()
```



### Tuples and object types
A variable of a tuple or object type is a heterogeneous storage
container.
A tuple or object defines various named *fields* of a type. A tuple also
defines a lexicographic *order* of the fields. Tuples are meant to be
heterogeneous storage types with few abstractions. The `()` syntax
can be used to construct tuples. The order of the fields in the constructor
must match the order of the tuple's definition. Different tuple-types are
*equivalent* if they specify the same fields of the same type in the same
order. The *names* of the fields also have to be the same.

  ```nim
  type
    Person = tuple[name: string, age: int] # type representing a person:
                                           # it consists of a name and an age.
  var person: Person
  person = (name: "Peter", age: 30)
  assert person.name == "Peter"
  # the same, but less readable:
  person = ("Peter", 30)
  assert person[0] == "Peter"
  ```

A tuple with one unnamed field can be constructed with the parentheses and a
trailing comma:

  ```nim
  proc echoUnaryTuple(a: (int,)) =
    echo a[0]

  echoUnaryTuple (1,)
  ```


In fact, a trailing comma is allowed for every tuple construction.

The implementation aligns the fields for the best access performance. The alignment
is compatible with the way the C compiler does it.

For consistency  with `object` declarations, tuples in a `type` section
can also be defined with indentation instead of `[]`:

  ```nim
  type
    Person = tuple   # type representing a person
      name: string   # a person consists of a name
      age: Natural   # and an age
  ```

Objects provide many features that tuples do not. Objects provide inheritance
and the ability to hide fields from other modules. Objects with inheritance
enabled have information about their type at runtime so that the `of` operator
can be used to determine the object's type. The `of` operator is similar to
the `instanceof` operator in Java.

  ```nim
  type
    Person = object of RootObj
      name*: string   # the * means that `name` is accessible from other modules
      age: int        # no * means that the field is hidden

    Student = ref object of Person # a student is a person
      id: int                      # with an id field

  var
    student: Student
    person: Person
  assert(student of Student) # is true
  assert(student of Person) # also true
  ```

Object fields that should be visible from outside the defining module have to
be marked by `*`. In contrast to tuples, different object types are
never *equivalent*, they are nominal types whereas tuples are structural.
Objects that have no ancestor are implicitly `final` and thus have no hidden
type information. One can use the `inheritable` pragma to
introduce new object roots apart from `system.RootObj`.

  ```nim
  type
    Person = object # example of a final object
      name*: string
      age: int

    Student = ref object of Person # Error: inheritance only works with non-final objects
      id: int
  ```

The assignment operator for tuples and objects copies each component.
The methods to override this copying behavior are described [here][Lifetime-tracking hooks].


### Object construction

Objects can also be created with an `object construction expression`:idx: that
has the syntax `T(fieldA: valueA, fieldB: valueB, ...)` where `T` is
an `object` type or a `ref object` type:

  ```nim
  type
    Student = object
      name: string
      age: int
    PStudent = ref Student
  var a1 = Student(name: "Anton", age: 5)
  var a2 = PStudent(name: "Anton", age: 5)
  # this also works directly:
  var a3 = (ref Student)(name: "Anton", age: 5)
  # not all fields need to be mentioned, and they can be mentioned out of order:
  var a4 = Student(age: 5)
  ```

Note that, unlike tuples, objects require the field names along with their values.
For a `ref object` type `system.new` is invoked implicitly.


### Case in object

Objects can contain a `case` section to create variant types, where different object variants have different fields:

```nim
type
  ShapeKind = enum
    Circle, Rectangle, Triangle

  Shape = object
    case kind: ShapeKind
    of Circle:
      radius: float
    of Rectangle:
      width, height: float
    of Triangle:
      a, b, c: float

proc area(s: Shape): float =
  case s.kind
  of Circle:
    PI * s.radius * s.radius
  of Rectangle:
    s.width * s.height
  of Triangle:
    # Heron's formula
    let p = (s.a + s.b + s.c) / 2
    sqrt(p * (p - s.a) * (p - s.b) * (p - s.c))

var circle = Shape(kind: Circle, radius: 5.0)
var rect = Shape(kind: Rectangle, width: 3.0, height: 4.0)
echo area(circle)  # 78.54...
echo area(rect)    # 12.0
```

The discriminator field (in this case `kind`) must be an ordinal type, typically an enum. The case section must be exhaustive - all possible values of the discriminator must be handled or an `else` has to be used.

When constructing an object with a case section, only the fields for the specified variant need to be provided:

```nim
var triangle = Shape(kind: Triangle, a: 3.0, b: 4.0, c: 5.0)
```

The case section can also be nested within other object fields:

```nim
type
  NodeType = enum
    Leaf, Branch

  Node = object
    id: int
    case nodeType: NodeType
    of Leaf:
      value: string
    of Branch:
      children: seq[Node]
```


### Default values for object fields

Object fields are allowed to have a constant default value.

```nim test
type
  Foo = object
    a: int = 2
    b: float = 3.14
    c: string = "I can have a default value"

  Bar = ref object
    a: int = 2
    b: float = 3.14
    c: string = "I can have a default value"
```

The explicit initialization uses these defaults which includes an `object` created
with an object construction expression or the proc `default`; a `ref object` created
with an object construction expression or the proc `new`.


```nim test
type
  Foo = object
    a: int = 2
    b: float = 3.0
  Bar = ref object
    a: int = 2
    b: float = 3.0

block: # created with an object construction expression
  let x = Foo()
  assert x.a == 2 and x.b == 3.0

  let y = Bar()
  assert y.a == 2 and y.b == 3.0

block: # created with an object construction expression
  let x = default(Foo)
  assert x.a == 2 and x.b == 3.0

  let y = default(array[1, Foo])
  assert y[0].a == 2 and y[0].b == 3.0

  let z = default(tuple[x: Foo])
  assert z.x.a == 2 and z.x.b == 3.0

block: # created with the proc `new`
  let y = new Bar
  assert y.a == 2 and y.b == 3.0
```


### Set type

The set type models the mathematical notion of a set. The set's basetype can
only be an ordinal type of a certain size, namely:

* `int8`-`int16`
* `uint8`/`byte`-`uint16`
* `char`
* `enum`

or equivalent. When constructing a set with signed integer literals, the set's
base type is defined to be in the range `0 .. DefaultSetElements-1` where
`DefaultSetElements` is currently always 2^8. The maximum range length for the
base type of a set is `MaxSetElements` which is currently always 2^16. Types
with a bigger range length are coerced into the range `0 .. MaxSetElements-1`.

The reason is that sets are implemented as high performance bit vectors.
Attempting to declare a set with a larger type will result in an error:

```nim
  var s: set[int64] # Error: set is too large; use `std/sets` for ordinal types
                    # with more than 2^16 elements
```


**Note:** The standard library also offers [hash sets](sets.html) (which you need to import
with `import std/sets`), which have no such restrictions.

Sets can be constructed via the set constructor: `{}` is the empty set. The
empty set is type compatible with any concrete set type. The constructor
can also be used to include elements (and ranges of elements):

  ```nim
  type
    CharSet = set[char]
  var
    x: CharSet
  x = {'a'..'z', '0'..'9'} # This constructs a set that contains the
                           # letters from 'a' to 'z' and the digits
                           # from '0' to '9'
  ```

These operations are supported by sets:

==================    ========================================================
operation             meaning
==================    ========================================================
`A + B`               union of two sets
`A * B`               intersection of two sets
`A - B`               difference of two sets (A without B's elements)
`A == B`              set equality
`A <= B`              subset relation (A is subset of B or equal to B)
`A < B`               strict subset relation (A is a proper subset of B)
`e in A`              set membership (A contains element e)
`e notin A`           A does not contain element e
`contains(A, e)`      A contains element e
`incl(A, elem)`       same as `A = A + {elem}`
`excl(A, elem)`       same as `A = A - {elem}`
==================    ========================================================



### Reference and pointer types

References (similar to pointers in other programming languages) are a
way to introduce many-to-one relationships. This means different references can
point to and modify the same location in memory (also called `aliasing`:idx:).

Nim distinguishes between `traced`:idx: and `untraced`:idx: references.
Untraced references are also called *pointers*. Traced references point to
objects of a garbage-collected heap, untraced references point to
manually allocated objects or objects somewhere else in memory. Thus,
untraced references are *unsafe*. However, for certain low-level operations
(accessing the hardware) untraced references are unavoidable.

Traced references are declared with the **ref** keyword, untraced references
are declared with the **ptr** keyword. In general, a `ptr T` is implicitly
convertible to the `pointer` type.

An empty subscript `[]` notation can be used to de-refer a reference,
the `addr` proc returns the address of an item. If `x` has the type `T`
then `addr(x)` has the type `ptr T`.
Thus, the usage of `addr` is an *unsafe* feature.

The `.` (access a tuple/object field operator)
and `[]` (array/string/sequence index operator) operators perform implicit
dereferencing operations for reference types:

  ```nim
  type
    Node = ref NodeObj
    NodeObj = object
      le, ri: Node
      data: int

  var n = Node()
  n.data = 9
  # no need to write n[].data; in fact n[].data is highly discouraged!
  ```

In order to simplify structural type checking, recursive tuples are not valid:

  ```nim
  # invalid recursion
  type MyTuple = tuple[a: ref MyTuple]
  ```

Likewise `T = ref T` is an invalid type.

As a syntactical extension, `object` types can be anonymous if
declared in a type section via the `ref object` or `ptr object` notations.
This feature is useful if an object should only gain reference semantics:

  ```nim
  type
    Node = ref object
      le, ri: Node
      data: int
  ```


To allocate a new traced object, use the object construction syntax.
To deal with untraced memory, the procs `alloc`, `dealloc` and
`realloc` can be used. The documentation of the [system](system.html) module
contains further information.


### Nil

If a reference points to *nothing*, it has the value `nil`. `nil` is the
default value for all `ref` and `ptr` types. The `nil` value can also be
used like any other literal value. For example, it can be used in an assignment
like `myRef = nil`.

Dereferencing `nil` is prevented at compile-time.


### Not nil tracking

Nimony supports `not nil` tracking, which allows the compiler to track when a reference is guaranteed to be non-nil. This is done by appending `not nil` to a reference type:

```nim
type
  Node = ref object
    data: int
    next: Node not nil

proc processNode(n: Node not nil) =
  # n is guaranteed to be non-nil here
  echo n.data
  if n.next != nil:
    processNode(n.next)
```

When a variable is declared as `not nil`, the compiler ensures it is never assigned `nil` and can be used without nil checks.

The compiler can track when a variable becomes non-nil through control flow:

```nim
var node: Node = nil
if someCondition():
  node = Node(data: 42, next: nil)
  # node is now tracked as not nil in this branch
  processNode(node) # OK
else:
  # node is still potentially nil here
  if node != nil:
    processNode(node) # OK after nil check
```


### Proc type

A procedural type is internally a pointer to a proc. `nil` is
an allowed value for a variable of a procedural type.

Examples:

  ```nim
  proc printItem(x: int) = ...

  proc forEach(c: proc (x: int) {.cdecl.}) =
    ...

  forEach(printItem)  # this will NOT compile because calling conventions differ
  ```

A subtle issue with procedural types is that the calling convention of the
proc influences the type compatibility: procedural types are only
compatible if they have the same calling convention.

Nim supports these `calling conventions`:idx:\:

`nimcall`:idx:
:   is the default convention used for a Nim **proc**. It is the
    same as `fastcall`, but only for C compilers that support `fastcall`.

`closure`:idx:
:   is the default calling convention for a **procedural type** that lacks
    any pragma annotations. It indicates that the proc has a hidden
    implicit parameter (an *environment*). Proc vars that have the calling
    convention `closure` take up two machine words: One for the proc pointer
    and another one for the pointer to implicitly passed environment.

`stdcall`:idx:
:   This is the stdcall convention as specified by Microsoft. The generated C
    proc is declared with the `__stdcall` keyword.

`cdecl`:idx:
:   The cdecl convention means that a proc shall use the same convention
    as the C compiler. Under Windows the generated C proc is declared with
    the `__cdecl` keyword.

`safecall`:idx:
:   This is the safecall convention as specified by Microsoft. The generated C
    proc is declared with the `__safecall` keyword. The word *safe*
    refers to the fact that all hardware registers shall be pushed to the
    hardware stack.

`inline`:idx:
:   The inline convention means the caller should not call the proc,
    but inline its code directly. Note that Nim does not inline, but leaves
    this to the C compiler; it generates `__inline` procs. This is
    only a hint for the compiler: it may completely ignore it, and
    it may inline procs that are not marked as `inline`.

`noinline`:idx:
:   The backend compiler may inline procs that are not marked as `inline`.
    The noinline convention prevents it.

`fastcall`:idx:
:   Fastcall means different things to different C compilers. One gets whatever
    the C `__fastcall` means.

`thiscall`:idx:
:   This is the thiscall calling convention as specified by Microsoft, used on
    C++ class member functions on the x86 architecture.

`syscall`:idx:
:   The syscall convention is the same as `__syscall`:c: in C. It is used for
    interrupts.

`noconv`:idx:
:   The generated C code will not have any explicit calling convention and thus
    use the C compiler's default calling convention. This is needed because
    Nim's default calling convention for procs is `fastcall` to
    improve speed.

Most calling conventions exist only for the Windows 32-bit platform.

The default calling convention is `nimcall`.


### Distinct type

A `distinct` type is a new type derived from a `base type`:idx: that is
incompatible with its base type. In particular, it is an essential property
of a distinct type that it **does not** imply a subtype relation between it
and its base type. Explicit type conversions from a distinct type to its
base type and vice versa are allowed.

A distinct type is an ordinal type if its base type is an ordinal type.



## Type relations

The following section defines several relations on types that are needed to
describe the type checking done by the compiler.


### Type equality

Nim uses structural type equivalence for most types. Only for objects,
enumerations and distinct types and for generic types name equivalence is used.


### Subtype relation

If object `a` inherits from `b`, `a` is a subtype of `b`.

This subtype relation is extended to the types `var`, `ref`, `ptr`.
If `A` is a subtype of `B` and `A` and `B` are `object` types then:

- `var A` is a subtype of `var B`
- `ref A` is a subtype of `ref B`
- `ptr A` is a subtype of `ptr B`.

**Note**: One of the above pointer-indirections is required for assignment from
a subtype to its parent type to prevent "object slicing".


### Convertible relation

A type `a` is **implicitly** convertible to type `b` iff the following
algorithm returns true:

  ```nim
  proc isImplicitlyConvertible(a, b: PType): bool =
    if isSubtype(a, b):
      return true
    if isIntLiteral(a):
      return b in {int8, int16, int32, int64, int, uint, uint8, uint16,
                   uint32, uint64, float32, float64}
    case a.kind
    of int:     result = b in {int32, int64}
    of int8:    result = b in {int16, int32, int64, int}
    of int16:   result = b in {int32, int64, int}
    of int32:   result = b in {int64, int}
    of uint:    result = b in {uint32, uint64}
    of uint8:   result = b in {uint16, uint32, uint64}
    of uint16:  result = b in {uint32, uint64}
    of uint32:  result = b in {uint64}
    of float32: result = b in {float64}
    of float64: result = b in {float32}
    of seq:
      result = b == openArray and typeEquals(a.baseType, b.baseType)
    of array:
      result = b == openArray and typeEquals(a.baseType, b.baseType)
      if a.baseType == char and a.indexType.rangeA == 0:
        result = b == cstring
    of cstring, ptr:
      result = b == pointer
    of string:
      result = b == cstring
    of proc:
      result = typeEquals(a, b) or compatibleParametersAndEffects(a, b)
  ```

We used the predicate `typeEquals(a, b)` for the "type equality" property
and the predicate `isSubtype(a, b)` for the "subtype relation".
`compatibleParametersAndEffects(a, b)` is currently not specified.

Implicit conversions are also performed for Nim's `range` type
constructor.

Let `a0`, `b0` of type `T`.

Let `A = range[a0..b0]` be the argument's type, `F` the formal
parameter's type. Then an implicit conversion from `A` to `F`
exists if `a0 >= low(F) and b0 <= high(F)` and both `T` and `F`
are signed integers or if both are unsigned integers.


A type `a` is **explicitly** convertible to type `b` iff the following
algorithm returns true:

  ```nim
  proc isIntegralType(t: PType): bool =
    result = isOrdinal(t) or t.kind in {float, float32, float64}

  proc isExplicitlyConvertible(a, b: PType): bool =
    result = false
    if isImplicitlyConvertible(a, b): return true
    if typeEquals(a, b): return true
    if a == distinct and typeEquals(a.baseType, b): return true
    if b == distinct and typeEquals(b.baseType, a): return true
    if isIntegralType(a) and isIntegralType(b): return true
    if isSubtype(a, b) or isSubtype(b, a): return true
  ```

The convertible relation can be relaxed by a user-defined type
`converter`:idx:.

  ```nim
  converter toInt(x: char): int = result = ord(x)

  var
    x: int
    chr: char = 'a'

  # implicit conversion magic happens here
  x = chr
  echo x # => 97
  # one can use the explicit form too
  x = chr.toInt
  echo x # => 97
  ```

The type conversion `T(a)` is an L-value if `a` is an L-value and
`typeEqualsOrDistinct(T, typeof(a))` holds.


### Assignment compatibility

An expression `b` can be assigned to an expression `a` iff `a` is an
`l-value` and `isImplicitlyConvertible(b.typ, a.typ)` holds.


## Overload resolution

In a call `p(args)` where `p` may refer to more than one
candidate, it is said to be a symbol choice. Overload resolution will attempt to
find the best candidate, thus transforming the symbol choice into a resolved symbol.
The routine `p` that matches best is selected following a series of trials explained below.

If multiple candidates match equally well after all trials have been tested, the ambiguity
is reported during semantic analysis.

### First Trial: Category matching

Every arg in `args` needs to match and there are multiple different categories of matches.
Let `f` be the formal parameter's type and `a` the type of the argument.

1. Exact match: `a` and `f` are of the same type.
2. Literal match: `a` is an integer literal of value `v`
   and `f` is a signed or unsigned integer type and `v` is in `f`'s
   range. Or:  `a` is a floating-point literal of value `v`
   and `f` is a floating-point type and `v` is in `f`'s
   range.
3. Generic match: `f` is a generic type and `a` matches, for
   instance `a` is `int` and `f` is a generic (constrained) parameter
   type (like in `[T]` or `[T: int|char]`).
4. Subrange or subtype match: `a` is a `range[T]` and `T`
   matches `f` exactly. Or: `a` is a subtype of `f`.
5. Integral conversion match: `a` is convertible to `f` and `f` and `a`
   is some integer or floating-point type.
6. Conversion match: `a` is convertible to `f`, possibly via a user
   defined `converter`.

Each operand may fall into one of the categories above; the operand's
highest priority category. The list above is in order or priority.
If a candidate has more priority matches than all other candidates, it is selected as the
resolved symbol.

For example, if a candidate with one exact match is compared to a candidate with multiple
generic matches and zero exact matches, the candidate with an exact match will win.

Below is a pseudocode interpretation of category matching, `count(p, m)` counts the number
of matches of the matching category `m` for the routine `p`.

A routine `p` matches better than a routine `q` if the following
algorithm returns true:

  ```nim
  for each matching category m in ["exact match", "literal match",
                                  "generic match", "subtype match",
                                  "integral match", "conversion match"]:
    if count(p, m) > count(q, m): return true
    elif count(p, m) == count(q, m):
      discard "continue with next category m"
    else:
      return false
  return "ambiguous"
  ```

### Second Trial: Structural type comparisons

The "nesting" of a type's generic parameters is used in order to form a relation between two types. A type `G[A[T]]` is more specific than a type `G[T]` as it has more information about the type `T`. Thus for overload resolution the more specific type is preferred.


### Some Examples

  ```nim
  proc takesInt(x: int) = echo "int"
  proc takesInt[T](x: T) = echo "T"
  proc takesInt(x: int16) = echo "int16"

  takesInt(4) # "int"
  var x: int32
  takesInt(x) # "T"
  var y: int16
  takesInt(y) # "int16"
  var z: range[0..4] = 0
  takesInt(z) # "T"
  ```


If the argument `a` matches both the parameter type `f` of `p`
and `g` of `q` via a subtyping relation, the inheritance depth is taken
into account:

  ```nim
  type
    A = object of RootObj
    B = object of A
    C = object of B

  proc p(obj: A) =
    echo "A"

  proc p(obj: B) =
    echo "B"

  var c = C()
  # not ambiguous, calls 'B', not 'A' since B is a subtype of A
  # but not vice versa:
  p(c)

  proc pp(obj: A, obj2: B) = echo "A B"
  proc pp(obj: B, obj2: A) = echo "B A"

  # but this is ambiguous:
  pp(c, c)
  ```


Likewise, for generic matches, the most specialized generic type (that still
matches) is preferred:

  ```nim
  proc gen[T](x: ref ref T) = echo "ref ref T"
  proc gen[T](x: ref T) = echo "ref T"
  proc gen[T](x: T) = echo "T"

  var ri: ref int
  gen(ri) # "ref T"
  ```


## Overload disambiguation

For routine calls "overload resolution" is performed. There is a weaker form of
overload resolution called *overload disambiguation* that is performed when an
overloaded symbol is used in a context where there is additional type information
available. Let `p` be an overloaded symbol. These contexts are:

- In a function call `q(..., p, ...)` when the corresponding formal parameter
  of `q` is a `proc` type. If `q` itself is overloaded then the cartesian product
  of every interpretation of `q` and `p` must be considered.
- In an object constructor `Obj(..., field: p, ...)` when `field` is a `proc`
  type. Analogous rules exist for array/set/tuple constructors.
- In a declaration like `x: T = p` when `T` is a `proc` type.

As usual, ambiguous matches produce a compile-time error.



## Statements and expressions

Statements do not produce a value in contrast to expressions. However, some expressions are statements.

Statements are separated into `simple statements`:idx: and
`complex statements`:idx:.
Simple statements are statements that cannot contain other statements like
assignments, calls, or the `return` statement; complex statements can
contain other statements. To avoid the `dangling else problem`:idx:, complex
statements always have to be indented. The details can be found in the grammar.


### Statement list expression

Statements can also occur in an expression context that looks
like `(stmt1; stmt2; ...; ex)`. This is called
a statement list expression or `(;)`. The type
of `(stmt1; stmt2; ...; ex)` is the type of `ex`. All the other statements
must be of type `void`. (One can use `discard` to produce a `void` type.)
`(;)` does not introduce a new scope.


### Discard statement

Example:

  ```nim
  proc p(x, y: int): int =
    result = x + y

  discard p(3, 4) # discard the return value of `p`
  ```

The `discard` statement evaluates its expression for side-effects and
throws the expression's resulting value away, and should only be used
when ignoring this value is known not to cause problems.

Ignoring the return value of a proc without using a discard statement is
a static error.

An empty `discard` statement is often used as a null statement:

  ```nim
  proc classify(s: string) =
    case s[0]
    of SymChars, '_': echo "an identifier"
    of '0'..'9': echo "a number"
    else: discard
  ```


### Var statement

Var statements declare new local and global variables and
initialize them. A comma-separated list of variables can be used to specify
variables of the same type:

  ```nim
  var
    a: int = 0
    x, y, z: int
  ```

If an initializer is given, the type can be omitted: the variable is then of the
same type as the initializing expression. The compiler enforces that local
variables are initialized before they are used.

The check for initialization can be disabled for optimization reasons with the
`noinit`:idx: pragma:

  ```nim
  var
    a {.noinit.}: array[0..1023, char]
  ```

If a proc is annotated with the `noinit` pragma, this refers to its implicit
`result` variable:

  ```nim
  proc returnUndefinedValue: int {.noinit.} = discard
  ```

The compiler does a `control flow analysis`:idx: to prove
the variable has been initialized:

  ```nim
  type
    MyObject = object

  proc p() =
    # the following is valid:
    var x: MyObject
    if someCondition():
      x = a()
    else:
      x = a()
    use x
  ```


### Let statement

A `let` statement declares new local and global `single assignment`:idx:
variables and binds a value to them. The syntax is the same as that of the `var`
statement, except that the keyword `var` is replaced by the keyword `let`.
Let variables are not l-values and can thus not be passed to `var` parameters
nor can their address be taken. They cannot be assigned new values.

For let variables, the same pragmas are available as for ordinary variables.

As `let` statements are immutable after creation they need to define a value
when they are declared. The only exception to this is if the `{.importc.}`
pragma (or any of the other `importX` pragmas) is applied, in this case the
value is expected to come from native code, typically a C/C++ `const`.


### Special identifier `_` (underscore)

The identifier `_` has a special meaning in declarations.
Any definition with the name `_` will not be added to scope, meaning the
definition is evaluated, but cannot be used. As a result the name `_` can be
indefinitely redefined.

  ```nim
  let _ = 123
  echo _ # error
  let _ = 456 # compiles
  ```

### Tuple unpacking

In a `var`, `let` or `const` statement tuple unpacking can be performed.
The special identifier `_` can be used to ignore some parts of the tuple:

  ```nim
  proc returnsTuple(): (int, int, int) = (4, 2, 3)

  let (x, _, z) = returnsTuple()
  ```

This is treated as syntax sugar for roughly the following:

  ```nim
  let
    tmpTuple = returnsTuple()
    x = tmpTuple[0]
    z = tmpTuple[2]
  ```

For `var` or `let` statements, if the value expression is a tuple literal,
each expression is directly expanded into an assignment without the use of
a temporary variable.

  ```nim
  let (x, y, z) = (1, 2, 3)
  # becomes
  let
    x = 1
    y = 2
    z = 3
  ```

Tuple unpacking can also be nested:

  ```nim
  proc returnsNestedTuple(): (int, (int, int), int, int) = (4, (5, 7), 2, 3)

  let (x, (_, y), _, z) = returnsNestedTuple()
  ```


### Const section

A const section declares constants whose values are constant expressions:

  ```nim
  import std/[strutils]
  const
    roundPi = 3.1415
    constEval = contains("abc", 'b') # computed at compile time!
  ```

Once declared, a constant's symbol can be used as a constant expression.

The value part of a constant declaration opens a new scope for each constant,
so no symbols declared in the constant value are accessible outside of it.

  ```nim
  const foo = (var a = 1; a)
  const bar = a # error
  let baz = a # error
  ```

See [Constants and Constant Expressions] for details.



### If statement

Example:

  ```nim
  var name = readLine(stdin)

  if name == "Andreas":
    echo "What a nice name!"
  elif name == "":
    echo "Don't you have a name?"
  else:
    echo "Boring name..."
  ```

The `if` statement is a simple way to make a branch in the control flow:
The expression after the keyword `if` is evaluated, if it is true
the corresponding statements after the `:` are executed. Otherwise,
the expression after the `elif` is evaluated (if there is an
`elif` branch), if it is true the corresponding statements after
the `:` are executed. This goes on until the last `elif`. If all
conditions fail, the `else` part is executed. If there is no `else`
part, execution continues with the next statement.

In `if` statements, new scopes begin immediately after
the `if`/`elif`/`else` keywords and ends after the
corresponding *then* block.
For visualization purposes the scopes have been enclosed
in `{|  |}` in the following example:

  ```nim
  if {| (let m = input =~ re"(\w+)=\w+"; m.isMatch):
    echo "key ", m[0], " value ", m[1]  |}
  elif {| (let m = input =~ re""; m.isMatch):
    echo "new m in this scope"  |}
  else: {|
    echo "m not declared here"  |}
  ```

### Case statement

Example:

  ```nim
  let line = readline(stdin)
  case line
  of "delete-everything", "restart-computer":
    echo "permission denied"
  of "go-for-a-walk":
    echo "please yourself"
  elif line.len == 0:
    echo "empty" # optional, must come after `of` branches
  else:
    echo "unknown command" # ditto
  ```


The `case` statement is similar to the `if` statement, but it represents
a multi-branch selection. The expression after the keyword `case` is
evaluated and if its value is in a *slicelist* the corresponding statements
(after the `of` keyword) are executed. If the value is not in any
given *slicelist*, trailing `elif` and `else` parts are executed using same
semantics as for `if` statement, and `elif` is handled just like `else: if`.
If there are no `else` or `elif` parts and not
all possible values that `expr` can hold occur in a *slicelist*, a static error occurs.
This holds only for expressions of ordinal types.
"All possible values" of `expr` are determined by `expr`'s type.
To suppress the static error an `else: discard` should be used.

Only ordinal types, floats, strings and cstrings are allowed as values
in case statements.

For non-ordinal types, it is not possible to list every possible value and so
these always require an `else` part.
An exception to this rule is for the `string` type, which does not
require a trailing `else` or `elif` branch.

Because case statements are checked for exhaustiveness during semantic analysis,
the value in every `of` branch must be a constant expression.
This restriction also allows the compiler to generate more performant code.

As a special semantic extension, an expression in an `of` branch of a case
statement may evaluate to a set or array constructor; the set or array is then
expanded into a list of its elements:

  ```nim
  const
    SymChars: set[char] = {'a'..'z', 'A'..'Z', '\x80'..'\xFF'}

  proc classify(s: string) =
    case s[0]
    of SymChars, '_': echo "an identifier"
    of '0'..'9': echo "a number"
    else: echo "other"

  # is equivalent to:
  proc classify(s: string) =
    case s[0]
    of 'a'..'z', 'A'..'Z', '\x80'..'\xFF', '_': echo "an identifier"
    of '0'..'9': echo "a number"
    else: echo "other"
  ```

The `case` statement doesn't produce an l-value, so the following example
won't work:

  ```nim
  type
    Foo = ref object
      x: seq[string]

  proc getX(x: Foo): var seq[string] =
    # doesn't work
    case true
    of true:
      x.x
    else:
      x.x

  var foo = Foo(x: @[])
  foo.getX().add("asd")
  ```

This can be remedied by explicitly using `result` or `return`:

  ```nim
  proc getX(x: Foo): var seq[string] =
    case true
    of true:
      result = x.x
    else:
      result = x.x
  ```


### When statement

Example:

  ```nim
  when sizeof(int) == 2:
    echo "running on a 16 bit system!"
  elif sizeof(int) == 4:
    echo "running on a 32 bit system!"
  elif sizeof(int) == 8:
    echo "running on a 64 bit system!"
  else:
    echo "cannot happen!"
  ```

The `when` statement is almost identical to the `if` statement with some
exceptions:

* Each condition (`expr`) has to be a constant expression (of type `bool`).
* The statements do not open a new scope.
* The statements that belong to the expression that evaluated to true are
  translated by the compiler, the other statements are not checked for
  semantics! However, each condition is checked for semantics.

The `when` statement enables conditional compilation techniques.



### Return statement

Example:

  ```nim
  return 40 + 2
  ```

The `return` statement ends the execution of the current proc.
It is only allowed in procs, funcs, methods, converters and templates. If used in a template
it causes the routine in which the template is expanded into to return.

If there is an `expr`, this is syntactic
sugar for:

  ```nim
  result = expr
  return result
  ```


`return` without an expression is a short notation for `return result` if
the proc has a return type. The `result`:idx: variable is always the return
value of the proc. It is automatically declared by the compiler.



### Yield statement

Example:

  ```nim
  yield (1, 2, 3)
  ```

The `yield` statement is used instead of the `return` statement in
iterators. It is only valid in iterators. Execution is returned to the body
of the for loop that called the iterator. Yield does not end the iteration
process, but the execution is passed back to the iterator if the next iteration
starts. See the section about iterators ([Iterators and the for statement])
for further information.


### Block statement

Example:

  ```nim
  var found = false
  block myblock:
    for i in 0..3:
      for j in 0..3:
        if a[j][i] == 7:
          found = true
          break myblock # leave the block, in this case both for-loops
  echo found
  ```

The block statement is a means to group statements to a (named) `block`.
Inside the block, the `break` statement is allowed to leave the block
immediately. A `break` statement can contain a name of a surrounding
block to specify which block is to be left.


### Break statement

Example:

  ```nim
  break
  ```

The `break` statement is used to leave a block immediately. If `symbol`
is given, it is the name of the enclosing block that is to be left. If it is
absent, the innermost block is left.


### While statement

Example:

  ```nim
  echo "Please tell me your password:"
  var pw = readLine(stdin)
  while pw != "12345":
    echo "Wrong password! Next try:"
    pw = readLine(stdin)
  ```


The `while` statement is executed until the `expr` evaluates to false.
Endless loops are no error. `while` statements open an `implicit block`
so that they can be left with a `break` statement.


### Continue statement

A `continue` statement leads to the immediate next iteration of the
surrounding loop construct. It is only allowed within a loop. A continue
statement is syntactic sugar for a nested block:

  ```nim
  while expr1:
    stmt1
    continue
    stmt2
  ```

Is equivalent to:

  ```nim
  while expr1:
    block myBlockName:
      stmt1
      break myBlockName
      stmt2
  ```


### Using statement

The `using` statement provides syntactic convenience in modules where the same parameter names and types are used over and over. Instead of repeating type annotations:

```nim
proc foo(c: Context; n: Node) = ...
proc bar(c: Context; n: Node, counter: int) = ...
proc baz(c: Context; n: Node) = ...
```

One can tell the compiler about the convention that a parameter of name `c` should default to type `Context`, `n` should default to `Node` etc.:

```nim
using
  c: Context
  n: Node
  counter: int

proc foo(c, n) = ...
proc bar(c, n, counter) = ...
proc baz(c, n) = ...

proc mixedMode(c, n; x, y: int) =
  # 'c' is inferred to be of the type 'Context'
  # 'n' is inferred to be of the type 'Node'
  # But 'x' and 'y' are of type 'int'.
```

The `using` section uses the same indentation based grouping syntax as a `var` or `let` section.

Note that `using` is not applied for `template` since the untyped template parameters default to the type `system.untyped`.

Mixing parameters that should use the `using` declaration with parameters that are explicitly typed is possible and requires a semicolon between them.


### If expression

An `if` expression is almost like an if statement, but it is an expression.
This feature is similar to *ternary operators* in other languages.
Example:

  ```nim
  var y = if x > 8: 9 else: 10
  ```

An `if` expression always results in a value, so the `else` part is
required. `elif` parts are also allowed.

### When expression

Just like an `if` expression, but corresponding to the `when` statement.

### Case expression

The `case` expression is again very similar to the case statement:

  ```nim
  var favoriteFood = case animal
    of "dog": "bones"
    of "cat": "mice"
    elif animal.endsWith"whale": "plankton"
    else:
      echo "I'm not sure what to serve, but everybody loves ice cream"
      "ice cream"
  ```

As seen in the above example, the case expression can also introduce side
effects. When multiple statements are given for a branch, Nim will use
the last expression as the result value.


### Block expression

A `block` expression is almost like a block statement, but it is an expression
that uses the last expression under the block as the value.
It is similar to the statement list expression, but the statement list expression
does not open a new block scope.

  ```nim
  let a = block:
    var fib = @[0, 1]
    for i in 0..10:
      fib.add fib[^1] + fib[^2]
    fib
  ```


### Table constructor

A table constructor is syntactic sugar for an array constructor:

  ```nim
  {"key1": "value1", "key2", "key3": "value2"}

  # is the same as:
  [("key1", "value1"), ("key2", "value2"), ("key3", "value2")]
  ```


The empty table can be written `{:}` (in contrast to the empty set
which is `{}`) which is thus another way to write the empty array
constructor `[]`.


### Type conversions

Syntactically a *type conversion* is like a proc call, but a
type name replaces the proc name. A type conversion is always
safe in the sense that a failure to convert a type to another
results in an exception (if it cannot be determined statically).

Ordinary procs are often preferred over type conversions in Nim: For instance,
`$` is the `toString` operator by convention and `toFloat` and `toInt`
can be used to convert from floating-point to integer or vice versa.

Type conversion can also be used to disambiguate overloaded routines:

  ```nim
  proc p(x: int) = echo "int"
  proc p(x: string) = echo "string"

  let procVar = (proc(x: string))(p)
  procVar("a")
  ```

Since operations on unsigned numbers wrap around and are unchecked so are
type conversions to unsigned integers and between unsigned integers. The
rationale for this is mostly better interoperability with the C Programming
language when algorithms are ported from C to Nim.


### Type casts

*Type casts* are a crude mechanism to interpret the bit pattern of an expression
as if it would be of another type. Type casts are only needed for low-level
programming and are inherently unsafe.

  ```nim
  cast[int](x)
  ```

The target type of a cast must be a concrete type, for instance, a target type
that is a type class (which is non-concrete) would be invalid:

  ```nim
  type Foo = int or float
  var x = cast[Foo](1) # Error: cannot cast to a non concrete type: 'Foo'
  ```

Type casts should not be confused with *type conversions,* as mentioned in the
prior section. Unlike type conversions, a type cast cannot change the underlying
bit pattern of the data being cast (aside from that the size of the target type
may differ from the source type). Casting resembles *type punning* in other
languages or C++'s `reinterpret_cast`:cpp: and `bit_cast`:cpp: features.

If the size of the target type is larger than the size of the source type,
the remaining memory is zeroed.


### The addr operator

The `addr` operator returns the address of an l-value. If the type of the
location is `T`, the `addr` operator result is of the type `ptr T`. An
address is always an untraced reference. Taking the address of an object that
resides on the stack is **unsafe**, as the pointer may live longer than the
object on the stack and can thus reference a non-existing object.
For easier interoperability with other compiled languages
such as C, retrieving the address of a `let` variable, a parameter,
or a `for` loop variable is allowed:

  ```nim
  let t1 = "Hello"
  var t2: pointer = addr(t1)
  echo cast[ptr string](t2)[]
  ```


## Procs

What most programming languages call `methods`:idx: or `functions`:idx: are
called `procs`:idx: in Nim. A proc
declaration consists of an identifier, zero or more formal parameters, a return
value type and a block of code. Formal parameters are declared as a list of
identifiers separated by either comma or semicolon. A parameter is given a type
by `: typename`. The type applies to all parameters immediately before it,
until either the beginning of the parameter list, a semicolon separator, or an
already typed parameter, is reached. The semicolon can be used to make
separation of types and subsequent identifiers more distinct.

  ```nim
  # Using only commas
  proc foo(a, b: int, c, d: bool): int

  # Using semicolon for visual distinction
  proc foo(a, b: int; c, d: bool): int

  # Will fail: a is untyped since ';' stops type propagation.
  proc foo(a; b: int; c, d: bool): int
  ```

A parameter may be declared with a default value which is used if the caller
does not provide a value for the argument. The value will be reevaluated
every time the function is called.

  ```nim
  # b is optional with 47 as its default value.
  proc foo(a: int; b: int = 47): int
  ```

If the proc returns a value, the proc body can access an implicitly declared
variable named `result`:idx: that represents the return value. Procs can be
overloaded. The overloading resolution algorithm determines which proc is the
best match for the arguments. Example:

  ```nim
  proc toLower(c: char): char = # toLower for characters
    if c in {'A'..'Z'}:
      result = char(int(c) + (int('a') - int('A')))
    else:
      result = c

  proc toLower(s: string): string = # toLower for strings
    result = newString(len(s))
    for i in 0..len(s) - 1:
      result[i] = toLower(s[i]) # calls toLower for characters; no recursion!
  ```

Calling a proc can be done in many ways:

  ```nim
  proc callme(x, y: int, s: string = "", c: char, b: bool = false) = ...

  # call with positional arguments      # parameter bindings:
  callme(0, 1, "abc", '\t', true)       # (x=0, y=1, s="abc", c='\t', b=true)
  # call as a command statement: no () needed:
  callme 0, 1, "abc", '\t'              # (x=0, y=1, s="abc", c='\t', b=false)
  # call with dot notation:
  0.callme(1, "abc", '\t', true)
  ```

A proc may call itself recursively.


## Operators

Operators are routines with a special operator symbol as identifier:

  ```nim
  proc `$`(x: int): string =
    # converts an integer to a string; this is a prefix operator.
    result = intToStr(x)
  ```

Operators with one parameter are prefix operators, operators with two
parameters are infix operators. (However, the parser distinguishes these from
the operator's position within an expression.) There is no way to declare
postfix operators: all postfix operators are built-in and handled by the
grammar explicitly.

Any operator can be called like an ordinary proc with the \`opr\`
notation. (Thus an operator can have more than two parameters):

  ```nim
  proc `*+`(a, b, c: int): int =
    # Multiply and add
    result = a * b + c

  assert `*+`(3, 4, 6) == `+`(`*`(3, 4), 6)
  ```


## Strict definitions

Every local variable must be initialized explicitly before it can be used:

  ```nim
  proc test =
    var s: seq[string]
    s.add "abc" # invalid!
  ```

Needs to be written as:

  ```nim
  proc test =
    var s: seq[string] = @[]
    s.add "abc" # valid!
  ```

A control flow analysis is performed in order to prove that a variable has been written to
before it is used. Thus the following is valid:

  ```nim
  proc test(cond: bool) =
    var s: seq[string]
    if cond:
      s = @["y"]
    else:
      s = @[]
    s.add "abc" # valid!
  ```

In this example every path does set `s` to a value before it is used.

  ```nim
  proc test(cond: bool) =
    let s: seq[string]
    if cond:
      s = @["y"]
    else:
      s = @[]
  ```

`let` statements are allowed to not have an initial value, but every path should set `s` to a value before it is used.


## Var parameters

The type of a parameter may be prefixed with the `var` keyword:

  ```nim
  proc divmod(a, b: int; res, remainder: var int) =
    res = a div b
    remainder = a mod b

  var
    x, y: int

  divmod(8, 5, x, y) # modifies x and y
  assert x == 1
  assert y == 3
  ```

In the example, `res` and `remainder` are `var parameters`.
Var parameters can be modified by the proc and the changes are
visible to the caller. The argument passed to a var parameter has to be
an l-value. Var parameters are implemented as hidden pointers. The
above example is equivalent to:

  ```nim
  proc divmod(a, b: int; res, remainder: ptr int) =
    res[] = a div b
    remainder[] = a mod b

  var
    x, y: int
  divmod(8, 5, addr(x), addr(y))
  assert x == 1
  assert y == 3
  ```

In the examples, var parameters or pointers are used to provide two
return values. This can be done in a cleaner way by returning a tuple:

  ```nim
  proc divmod(a, b: int): tuple[res, remainder: int] =
    (a div b, a mod b)

  var t = divmod(8, 5)

  assert t.res == 1
  assert t.remainder == 3
  ```

One can use `tuple unpacking`:idx: to access the tuple's fields:

  ```nim
  var (x, y) = divmod(8, 5) # tuple unpacking
  assert x == 1
  assert y == 3
  ```


**Note**: `var` parameters are never necessary for efficient parameter
passing. Since non-var parameters cannot be modified the compiler is always
free to pass arguments by reference if it considers it can speed up execution.


## Var return type

A proc, converter, or iterator may return a `var` type which means that the
returned value is an l-value and can be modified by the caller:

  ```nim
  var g = 0

  proc writeAccessToG(): var int =
    result = g

  writeAccessToG() = 6
  assert g == 6
  ```

It is a static error if the implicitly introduced pointer could be
used to access a location beyond its lifetime:

  ```nim
  proc writeAccessToG(): var int =
    var g = 0
    result = g # Error!
  ```

For iterators, a component of a tuple return type can have a `var` type too:

  ```nim
  iterator mpairs(a: var seq[string]): (int, var string) =
    for i in 0..a.high:
      yield (i, a[i])
  ```

In the standard library every name of a routine that returns a `var` type
starts with the prefix `m` per convention.


Memory safety for returning by `var T` is ensured by a simple borrowing
rule: If `result` does not refer to a location pointing to the heap
(that is in `result = X` the `X` involves a `ptr` or `ref` access)
then it has to be derived from the routine's first parameter:

  ```nim
  proc forward[T](x: var T): var T =
    result = x # ok, derived from the first parameter.

  proc p(param: var int): var int =
    var x: int
    # we know 'forward' provides a view into the location derived from
    # its first argument 'x'.
    result = forward(x) # Error: location is derived from `x`
                        # which is not p's first parameter and lives
                        # on the stack.
  ```

In other words, the lifetime of what `result` points to is attached to the
lifetime of the first parameter and that is enough knowledge to verify
memory safety at the call site.


### Future directions

Later versions of Nim can be more precise about the borrowing rule with
a syntax like:

  ```nim
  proc foo(other: Y; container: var X): var T from container
  ```

Here `var T from container` explicitly exposes that the
location is derived from the second parameter (called
'container' in this case).



## `out` parameters

An `out` parameter is like a `var` parameter but it must be written to before it can be used:

  ```nim
  proc myopen(f: out File; name: string): bool =
    f = default(File)
    result = open(f, name)
  ```

While it is usually the better style to use the return type in order to return results API and ABI
considerations might make this infeasible. Like for `var T` Nim maps `out T` to a hidden pointer.
For example POSIX's `stat` routine can be wrapped as:

  ```nim
  proc stat*(a1: cstring, a2: out Stat): cint {.importc, header: "<sys/stat.h>".}
  ```

When the implementation of a routine with output parameters is analysed, the compiler
checks that every path before the (implicit or explicit) return does set every output
parameter:

  ```nim
  proc p(x: out int; y: out string; cond: bool) =
    x = 4
    if cond:
      y = "abc"
    # error: not every path initializes 'y'
  ```


### Out parameters and inheritance

It is not valid to pass an lvalue of a supertype to an `out T` parameter:

  ```nim
  type
    Superclass = object of RootObj
      a: int
    Subclass = object of Superclass
      s: string

  proc init(x: out Superclass) =
    x = Superclass(a: 8)

  var v: Subclass
  init v
  use v.s # the 's' field was never initialized!
  ```

However, in the future this could be allowed and provide a better way to write object
constructors that take inheritance into account.


## Export marker

If a declared symbol is marked with an `asterisk`:idx: it is exported from the
current module:

  ```nim
  proc exportedEcho*(s: string) = echo s
  proc `*`*(a: string; b: int): string =
    result = newStringOfCap(a.len * b)
    for i in 1..b: result.add a

  var exportedVar*: int
  const exportedConst* = 78
  type
    ExportedType* = object
      exportedField*: int
  ```


## Method call syntax

For object-oriented programming, the syntax `obj.methodName(args)` can be used
instead of `methodName(obj, args)`. The parentheses can be omitted if
there are no remaining arguments: `obj.len` (instead of `len(obj)`).

This method call syntax is not restricted to objects, it can be used
to supply any type of first argument for procs:

  ```nim
  echo "abc".len # is the same as echo len "abc"
  echo "abc".toUpper()
  echo {'a', 'b', 'c'}.card
  stdout.writeLine("Hallo") # the same as writeLine(stdout, "Hallo")
  ```

Another way to look at the method call syntax is that it provides the missing
postfix notation.

The method call syntax conflicts with explicit generic instantiations:
`p[T](x)` cannot be written as `x.p[T]` because `x.p[T]` is always
parsed as `(x.p)[T]`.

See also: [Limitations of the method call syntax].

The `[: ]` notation has been designed to mitigate this issue: `x.p[:T]`
is rewritten by the parser to `p[T](x)`, `x.p[:T](y)` is rewritten to
`p[T](x, y)`. Note that `[: ]` has no AST representation, the rewrite
is performed directly in the parsing step.



## Command invocation syntax

Routines can be invoked without the `()` if the call is syntactically
a statement. This command invocation syntax also works for
expressions, but then only a single argument may follow. This restriction
means `echo f 1, f 2` is parsed as `echo(f(1), f(2))` and not as
`echo(f(1, f(2)))`. The method call syntax may be used to provide one
more argument in this case:

  ```nim
  proc optarg(x: int, y: int = 0): int = x + y
  proc singlearg(x: int): int = 20*x

  echo optarg 1, " ", singlearg 2  # prints "1 40"

  let fail = optarg 1, optarg 8   # Wrong. Too many arguments for a command call
  let x = optarg(1, optarg 8)  # traditional proc call with 2 arguments
  let y = 1.optarg optarg 8    # same thing as above, w/o the parenthesis
  assert x == y
  ```

The command invocation syntax also can't have complex expressions as arguments.
For example: [anonymous procs], `if`,
`case` or `try`. Function calls with no arguments still need () to
distinguish between a call and the function itself as a first-class value:

  ```nim
  proc optarg(x: int, y: int = 0): int = x + y
  proc singlearg(x: int): int = 20*x

  echo optarg 1, " ", singlearg 2  # prints "1 40"

  let fail = optarg 1, optarg 8   # Wrong. Too many arguments for a command call
  let x = optarg(1, optarg 8)  # traditional proc call with 2 arguments
  let y = 1.optarg optarg 8    # same thing as above, w/o the parenthesis
  assert x == y
  ```



## Anonymous procs

Unnamed procs can be used as lambda expressions to pass into other
procs:

  ```nim
  var cities = @["Frankfurt", "Tokyo", "New York", "Kyiv"]

  cities.sort(proc (x, y: string): int =
    cmp(x.len, y.len))
  ```


Procs as expressions can appear both as nested procs and inside top-level
executable code.


## Do notation

As a special convenience notation that keeps most elements of a
regular proc expression, the `do` keyword can be used to pass
anonymous procs to routines:

  ```nim
  var cities = @["Frankfurt", "Tokyo", "New York", "Kyiv"]

  sort(cities) do (x, y: string) -> int:
    cmp(x.len, y.len)

  # Less parentheses using the method plus command syntax:
  cities = cities.map do (x: string) -> string:
    "City of " & x
  ```

`do` is written after the parentheses enclosing the regular proc parameters.
The proc expression represented by the `do` block is appended to the routine
call as the last argument. In calls using the command syntax, the `do` block
will bind to the immediately preceding expression rather than the command call.

`do` with a parameter list or pragma list corresponds to an anonymous `proc`,
however `do` without parameters or pragmas is treated as a normal statement
list. This allows templates to receive both indented statement lists as an
argument in inline calls:

```nim
import std/syncio

template myif(cond: bool; thenPart, elsePart: untyped) =
  if cond:
    thenPart
  else:
    elsePart

myif true:
  echo "bar"
do:
  echo "baz"
```


## Closures

A closure is a proc that captures variables from its surrounding scope. In Nimony, closures are created by declaring a proc inside another proc or block:

```nim
proc createCounter(): proc(): int =
  var count = 0
  result = proc(): int =
    inc count
    return count

let counter = createCounter()
echo counter() # 1
echo counter() # 2
echo counter() # 3
```

Closures capture variables by reference, so modifications to captured variables are visible to all instances of the closure:

```nim
proc createAdder(x: int): proc(y: int): int =
  result = proc(y: int): int =
    return x + y

let add5 = createAdder(5)
let add10 = createAdder(10)
echo add5(3)  # 8
echo add10(3) # 13
```

Closures can capture multiple variables and can modify them:

```nim
proc createAccumulator(): proc(x: int): int =
  var total = 0
  result = proc(x: int): int =
    total += x
    return total

let acc = createAccumulator()
echo acc(5)  # 5
echo acc(3)  # 8
echo acc(7)  # 15
```

The lifetime of captured variables extends beyond the scope where they were declared, as long as the closure exists. This is handled automatically by the garbage collector for traced references.


## Func

A `func` is currently simply a different spelling for a `proc`. This will be changed in the future. A `func` will be strict about what it can do.



## Lifetime-tracking hooks

A type bound operator is a `proc` or `func` whose name starts with `=` but isn't an operator
(i.e. containing only symbols, such as `==`).
A type bound operator declared for a type applies to the type regardless of whether
the operator is in scope (including if it is private).


Type bound operators are:
`=destroy`, `=copy`, `=sink`, `=trace`, `=wasMoved`, `=dup`.

These operations can be *overridden* instead of *overloaded*. This means that
the implementation is automatically lifted to structured types. For instance,
if the type `T` has an overridden assignment operator `=`, this operator is
also used for assignments of the type `seq[T]`.

Since these operations are bound to a type, they have to be bound to a
nominal type for reasons of simplicity of implementation.

Type bound operations are the foundation for Nim's *scope based memory management*.


### Motivating example

With the language mechanisms described here, a custom seq could be written as:

  ```nim test
  type
    myseq*[T] = object
      len, cap: int
      data: ptr UncheckedArray[T]

  proc `=destroy`*[T](x: myseq[T]) =
    if x.data != nil:
      for i in 0..<x.len: `=destroy`(x.data[i])
      dealloc(x.data)

  proc `=wasMoved`*[T](x: var myseq[T]) =
    x.data = nil
    x.len = 0

  proc `=trace`[T](x: var myseq[T]; env: pointer) =
    # `=trace` allows the cycle collector `--mm:orc`
    # to understand how to trace the object graph.
    if x.data != nil:
      for i in 0..<x.len: `=trace`(x.data[i], env)

  proc `=copy`*[T](a: var myseq[T]; b: myseq[T]) =
    # do nothing for self-assignments:
    if a.data == b.data: return
    `=destroy`(a)
    a.len = b.len
    a.cap = b.cap
    a.data = nil
    if b.data != nil:
      a.data = cast[typeof(a.data)](alloc(a.cap * sizeof(T)))
      for i in 0..<a.len:
        a.data[i] = b.data[i]

  proc `=dup`*[T](a: myseq[T]): myseq[T] {.nodestroy.} =
    # an optimized version of `=wasMoved(tmp); `=copy(tmp, src)`
    # must be present if a custom `=copy` hook exists
    result = myseq[T](len: a.len, cap: a.cap, data: nil)
    if a.data != nil:
      result.data = cast[typeof(result.data)](alloc(result.cap * sizeof(T)))
      for i in 0..<result.len:
        result.data[i] = `=dup`(a.data[i])

  proc `=sink`*[T](a: var myseq[T]; b: myseq[T]) =
    # move assignment, optional.
    # Compiler is using `=destroy` and `copyMem` when not provided
    `=destroy`(a)
    a.len = b.len
    a.cap = b.cap
    a.data = b.data

  proc add*[T](x: var myseq[T]; y: sink T) =
    if x.len >= x.cap:
      x.cap = max(x.len + 1, x.cap * 2)
      x.data = cast[typeof(x.data)](realloc(x.data, x.cap * sizeof(T)))
    x.data[x.len] = y
    inc x.len

  proc `[]`*[T](x: myseq[T]; i: Natural): var T =
    assert i < x.len
    result = x.data[i]

  proc `[]=`*[T](x: var myseq[T]; i: Natural; y: sink T) =
    assert i < x.len
    x.data[i] = y

  proc createSeq*[T](elems: openArray[T]): myseq[T] =
    result = myseq[T](
      len: elems.len,
      cap: elems.len,
      data: cast[typeof(result.data)](alloc(result.cap * sizeof(T))))
    for i in 0..<result.len: result.data[i] = elems[i]

  proc len*[T](x: myseq[T]): int {.inline.} = x.len
  ```


### `=destroy` hook

A `=destroy` hook frees the object's associated memory and releases
other associated resources. Variables are destroyed via this hook when
they go out of scope or when the routine they were declared in is about
to return.

A `=destroy` hook is allowed to have a parameter of a `var T` or `T` type.
Taking a `var T` type is usually not required. The prototype of this hook for a type `T` needs to be:

  ```nim
  proc `=destroy`(x: T)
  # -- or --
  proc `=destroy`(x: var T)
  ```

The general pattern in `=destroy` looks like:

  ```nim
  proc `=destroy`(x: T) =
    # first check if 'x' was moved to somewhere else:
    if x.field != nil:
      freeResource(x.field)
  ```


### `=wasMoved` hook

A `=wasMoved` hook sets the object to a state that signifies to the destructor there is nothing to destroy.

The prototype of this hook for a type `T` needs to be:

  ```nim
  proc `=wasMoved`(x: var T)
  ```

Usually some pointer field inside the object is set to `nil`:

  ```nim
  proc `=wasMoved`(x: var T) =
    x.field = nil
  ```


### `=sink` hook

A `=sink` hook moves an object around, the resources are stolen from the source
and passed to the destination. It is ensured that the source's destructor does
not free the resources afterward by setting the object to its default value
(the value the object's state started in). Setting an object `x` back to its
default value is written as `wasMoved(x)`. When not provided the compiler
is using a combination of `=destroy` and `copyMem` instead. This is efficient
hence users rarely need to implement their own `=sink` operator, it is enough to
provide `=destroy` and `=copy`, the compiler will take care of the rest.

The prototype of this hook for a type `T` needs to be:

  ```nim
  proc `=sink`(dest: var T; source: T)
  ```

The general pattern in `=sink` looks like:

  ```nim

  proc `=sink`(dest: var T; source: T) =
    `=destroy`(dest)
    wasMoved(dest)
    dest.field = source.field
  ```

**Note**: `=sink` does not need to check for self-assignments.
How self-assignments are handled is explained later in this document.


### `=copy` hook

The ordinary assignment in Nim conceptually copies the values. The `=copy` hook
is called for assignments that couldn't be transformed into `=sink`
operations.

The prototype of this hook for a type `T` needs to be:

  ```nim
  proc `=copy`(dest: var T; source: T)
  ```

The general pattern in `=copy` looks like:

  ```nim
  proc `=copy`(dest: var T; source: T) =
    # protect against self-assignments:
    if dest.field != source.field:
      `=destroy`(dest)
      wasMoved(dest)
      dest.field = duplicateResource(source.field)
  ```

The `=copy` proc can be marked with the `{.error.}` pragma. Then any assignment
that otherwise would lead to a copy is prevented at compile-time. This looks like:

  ```nim
  proc `=copy`(dest: var T; source: T) {.error.}
  ```

A custom error message (e.g., `{.error: "custom error".}`) will not be emitted
by the compiler. Notice that there is no `=` before the `{.error.}` pragma.


### `=dup` hook

A `=dup` hook duplicates an object. `=dup(x)` can be regarded as an optimization replacing a `wasMoved(dest); =copy(dest, x)` operation.

The prototype of this hook for a type `T` needs to be:

  ```nim
  proc `=dup`(x: T): T
  ```

The general pattern in implementing `=dup` looks like:

  ```nim
  type
    Ref[T] = object
      data: ptr T
      rc: ptr int

  proc `=dup`[T](x: Ref[T]): Ref[T] =
    result = x
    if x.rc != nil:
      inc x.rc[]
  ```



### `=trace` hook

A custom **container** type can support a cycle collector via
the `=trace` hook. If the container does not implement `=trace`, cyclic data
structures which are constructed with the help of the container might leak
memory or resources, but memory safety is not compromised.

The prototype of this hook for a type `T` needs to be:

  ```nim
  proc `=trace`(dest: var T; env: pointer)
  ```

`env` can be used by a cycle collector to keep track of its internal state, it should be passed around
to calls of the built-in `=trace` operation.


### Move semantics

A "move" can be regarded as an optimized copy operation. If the source of the
copy operation is not used afterward, the copy can be replaced by a move. This
document uses the notation `lastReadOf(x)` to describe that `x` is not
used afterward. This property is computed by a static control flow analysis
but can also be enforced by using `system.move` explicitly.

One can query if the analysis is able to perform a move with `system.ensureMove`.
`move` enforces a move operation and calls `=wasMoved` whereas `ensureMove` is
an annotation that implies no runtime operation. An `ensureMove` annotation leads to a static error
if the compiler cannot prove that a move would be safe.

For example:

  ```nim
  proc main(normalParam: string; sinkParam: sink string) =
    var x = "abc"
    # valid:
    let valid = ensureMove x
    # invalid:
    let invalid = ensureMove normalParam
    # valid:
    let alsoValid = ensureMove sinkParam
  ```


#### Swap

The need to check for self-assignments and also the need to destroy previous
objects inside `=copy` and `=sink` is a strong indicator to treat
`system.swap` as a builtin primitive of its own that simply swaps every
field in the involved objects via `copyMem` or a comparable mechanism.
In other words, `swap(a, b)` is **not** implemented
as `let tmp = move(b); b = move(a); a = move(tmp)`.

This has further consequences:

* Objects that contain pointers that point to the same object are not supported
  by Nim's model. Otherwise swapped objects would end up in an inconsistent state.
* Seqs can use `realloc` in the implementation.


#### Sink parameters

To move a variable into a collection usually `sink` parameters are involved.
A location that is passed to a `sink` parameter should not be used afterward.
This is ensured by a static analysis over a control flow graph. If it cannot be
proven to be the last usage of the location, a copy is done instead and this
copy is then passed to the sink parameter.

A sink parameter
*may* be consumed once in the proc's body but doesn't have to be consumed at all.
The reason for this is that signatures
like `proc put(t: var Table; k: sink Key, v: sink Value)` should be possible
without any further overloads and `put` might not take ownership of `k` if
`k` already exists in the table. Sink parameters enable an affine type system,
not a linear type system.

The employed static analysis is limited and only concerned with local variables;
however, object and tuple fields are treated as separate entities:

  ```nim
  proc consume(x: sink Obj) = discard "no implementation"

  proc main =
    let tup = (Obj(), Obj())
    consume tup[0]
    # ok, only tup[0] was consumed, tup[1] is still alive:
    echo tup[1]
  ```

Sometimes it is required to explicitly `move` a value into its final position:

  ```nim
  proc main =
    var dest, src: array[10, string]
    # ...
    for i in 0..high(dest): dest[i] = move(src[i])
  ```

An implementation is allowed, but not required to implement even more move
optimizations (and the current implementation does not).



#### Rewrite rules

**Note**: Resources are destroyed at the scope exit.


    var x: T; stmts
    ---------------             (destroy-var)
    var x: T; try stmts
    finally: `=destroy`(x)
    # implied destroy at scope exit


    g(f(...))
    ------------------------    (nested-function-call)
    g(let tmp;
    bitwiseCopy tmp, f(...);
    tmp)
    finally: `=destroy`(tmp)


    x = f(...)
    ------------------------    (function-sink)
    `=sink`(x, f(...))


    x = lastReadOf z
    ------------------          (move-optimization)
    `=sink`(x, z)
    `=wasMoved`(z)


    v = v
    ------------------   (self-assignment-removal)
    discard "nop"


    x = y
    ------------------          (copy)
    `=copy`(x, y)


    f_sink(g())
    -----------------------     (call-to-sink)
    f_sink(g())


    f_sink(notLastReadOf y)
    --------------------------     (copy-to-sink)
    (let tmp = `=dup`(y);
    f_sink(tmp))


    f_sink(lastReadOf y)
    -----------------------     (move-to-sink)
    f_sink(y)
    `=wasMoved`(y)


#### Object and array construction

Object and array construction is treated as a function call where the
function has `sink` parameters.


#### Destructor elision

`=wasMoved(x)` followed by a `=destroy(x)` operation cancel each other
out. An implementation is encouraged to exploit this in order to improve
efficiency and code sizes. The current implementation does perform this
optimization.


#### Self assignments

`=sink` in combination with `=wasMoved` can handle self-assignments but
it's subtle.

The simple case of `x = x` cannot be turned
into `=sink(x, x); =wasMoved(x)` because that would lose `x`'s value.
The solution is that simple self-assignments that consist of

- Symbols: `x = x`
- Field access: `x.f = x.f`

are transformed into an empty statement that does nothing.
The compiler is free to optimize further cases.

The complex case looks like a variant of `x = f(x)`, we consider
`x = select(rand() < 0.5, x, y)` here:


  ```nim
  proc select(cond: bool; a, b: sink string): string =
    if cond:
      result = a # moves a into result
    else:
      result = b # moves b into result

  proc main =
    var x = "abc"
    var y = "xyz"
    # possible self-assignment:
    x = select(true, x, y)
  ```

Is transformed into:

  ```nim
  proc select(cond: bool; a, b: sink string): string {.nodestroy.} =
    if cond:
      result = a
      `=wasMoved`(a)
    else:
      result = b
      `=wasMoved`(b)
    `=destroy`(b)
    `=destroy`(a)

  proc main {.nodestroy.} =
    var
      x: string
      y: string
    x = "abc"
    y = "xyz"
    `=sink`(x, select(true,
      let blitTmp = x
      `=wasMoved`(x)
      blitTmp,
      let blitTmp = y
      `=wasMoved`(y)
      blitTmp))
    echo [x]
    `=destroy`(y)
    `=destroy`(x)
  ```

As can be manually verified, this transformation is correct for self-assignments.



#### The cursor pragma

The `ref` type is implemented
via the same runtime "hooks" and thus via reference counting.
This means that cyclic structures cannot be freed immediately.
With the `cursor` pragma one can break up cycles declaratively:

  ```nim
  type
    Node = ref object
      left: Node # owning ref
      right {.cursor.}: Node # non-owning ref
  ```

But please notice that this is not C++'s weak_ptr, it means the right field is not
involved in the reference counting, it is a raw pointer without runtime checks.

Automatic reference counting also has the disadvantage that it introduces overhead
when iterating over linked structures. The `cursor` pragma can also be used
to avoid this overhead:

  ```nim
  var it {.cursor.} = listRoot
  while it != nil:
    use(it)
    it = it.next
  ```

In fact, `cursor` more generally prevents object construction/destruction pairs
and so can also be useful in other contexts. The alternative solution would be to
use raw pointers (`ptr`) instead which is more cumbersome and also more dangerous
for the language's evolution: Later on, the compiler can try to prove `cursor` pragmas
to be safe, but for `ptr` the compiler has to remain silent about possible
problems.



#### Hook lifting

The hooks of a tuple type `(A, B, ...)` are generated by lifting the
hooks of the involved types `A`, `B`, ... to the tuple type. In
other words, a copy `x = y` is implemented
as `x[0] = y[0]; x[1] = y[1]; ...`, likewise for `=sink` and `=destroy`.

Other value-based compound types like `object` and `array` are handled
correspondingly. For `object` however, the compiler-generated hooks
can be overridden. This can also be important to use an alternative traversal
of the involved data structure that is more efficient or in order to avoid
deep recursions.



#### nodestroy pragma

The `nodestroy`:idx: pragma inhibits hook injections. This can be
used to specialize the object traversal in order to avoid deep recursions:


  ```nim test
  type Node = ref object
    x, y: int32
    left, right: Node

  type Tree = object
    root: Node

  proc `=destroy`(t: Tree) {.nodestroy.} =
    # use an explicit stack so that we do not get stack overflows:
    var s: seq[Node] = @[t.root]
    while s.len > 0:
      let x = s.pop
      if x.left != nil: s.add(x.left)
      if x.right != nil: s.add(x.right)
      # free the memory explicitly:
      `=dispose`(x)
    # notice how even the destructor for 's' is not called implicitly
    # anymore thanks to .nodestroy, so we have to call it on our own:
    `=destroy`(s)
  ```


As can be seen from the example, this solution is hardly sufficient and
should eventually be replaced by a better solution.


#### Copy on write

String literals are implemented as "copy on write".
When assigning a string literal to a variable, a copy of the literal won't be created.
Instead the variable simply points to the literal.
The literal is shared between different variables which are pointing to it.
The copy operation is deferred until the first write.

For example:

  ```nim
  var x = "abc"  # no copy
  var y = x      # no copy
  y[0] = 'h'     # copy
  ```

The abstraction fails for `addr x` because whether the address is going to be used for mutations is unknown.
`prepareMutation` needs to be called before the "address of" operation. For example:

  ```nim
  var x = "abc"
  var y = x

  prepareMutation(y)
  moveMem(addr y[0], addr x[0], 3)
  assert y == "abc"
  ```



### Nonoverloadable builtins

The following built-in procs cannot be overloaded for reasons of implementation
simplicity (they require specialized semantic checking):

    declared, defined, definedInScope, compiles, sizeof,
    is, getAst, astToStr, procCall

Thus, they act more like keywords than like ordinary identifiers; unlike a
keyword however, a redefinition may `shadow`:idx: the definition in
the [system](system.html) module.
From this list the following should not be written in dot
notation `x.f` since `x` cannot be type-checked before it gets passed
to `f`:

    declared, defined, definedInScope, compiles, getAst, astToStr




## Overloading of the subscript operator

The `[]` subscript operator can be overloaded
for any type by defining a routine with the name `[]`.

  ```nim
  type Foo = object
    data: seq[int]

  proc `[]`(foo: Foo; i: int): int =
    result = foo.data[i]

  let foo = Foo(data: @[1, 2, 3])
  echo foo[1] # 2
  ```

Assignment to subscripts can also be overloaded by naming a routine `[]=`,
which has precedence over assigning to the result of `[]`.

  ```nim
  type Foo = object
    data: seq[int]

  proc `[]`(foo: Foo; i: int): int =
    result = foo.data[i]
  proc `[]=`(foo: var Foo; i, val: int) =
    foo.data[i] = val

  var foo = Foo(data: @[1, 2, 3])
  echo foo[1] # 2
  foo[1] = 5
  echo foo.data # @[1, 5, 3]
  echo foo[1] # 5
  ```

Overloads of the subscript operator cannot be applied to routine or type
symbols themselves, as this conflicts with the syntax for instantiating
generic parameters, i.e. `foo[int](1, 2, 3)` or `Foo[int]`.


## Methods

Procedures always use static dispatch. Methods use dynamic
dispatch. For dynamic dispatch to work on an object it should be a reference
type.

  ```nim
  type
    Expression = ref object of RootObj ## abstract base class for an expression
    Literal = ref object of Expression
      x: int
    PlusExpr = ref object of Expression
      a, b: Expression

  method eval(e: Expression): int =
    # override this base method
    quit "abstract method without override"

  method eval(e: Literal): int = return e.x

  method eval(e: PlusExpr): int =
    # watch out: relies on dynamic binding
    result = eval(e.a) + eval(e.b)

  proc newLit(x: int): Literal = Literal(x: x)
  proc newPlus(a, b: Expression): PlusExpr = PlusExpr(a: a, b: b)

  echo eval(newPlus(newPlus(newLit(1), newLit(2)), newLit(4)))
  ```

In the example the constructors `newLit` and `newPlus` are procs
because they should use static binding, but `eval` is a method because it
requires dynamic binding.


### Inhibit dynamic method resolution via procCall

Dynamic method resolution can be inhibited via the builtin `system.procCall`:idx:.
This is somewhat comparable to the `super`:idx: keyword that traditional OOP
languages offer.

  ```nim  test = "nim c $1"
  type
    Thing = ref object of RootObj
    Unit = ref object of Thing
      x: int

  method m(a: Thing) =
    echo "base"

  method m(a: Unit) =
    # Call the base method:
    procCall m(Thing(a))
    echo "1"
  ```


## Iterators and the for statement

The `for`:idx: statement is an abstract mechanism to iterate over the elements
of a container. It relies on an `iterator`:idx: to do so. Like `while`
statements, `for` statements open an `implicit block`:idx: so that they
can be left with a `break` statement.

The `for` loop declares iteration variables - their scope reaches until the
end of the loop body. The iteration variables' types are inferred by the
return type of the iterator.

An iterator is similar to a proc, except that it can be called in the
context of a `for` loop. Iterators provide a way to specify the iteration over
an abstract type. The `yield` statement in the called iterator plays a key
role in the execution of a `for` loop. Whenever a `yield` statement is
reached, the data is bound to the `for` loop variables and control continues
in the body of the `for` loop. The iterator's local variables and execution
state are automatically saved between calls. Example:

  ```nim
  iterator items*(a: string): char {.inline.} =
    var i = 0
    while i < len(a):
      yield a[i]
      inc i

  for ch in items("hello world"): # `ch` is an iteration variable
    echo ch
  ```

The compiler generates code as if the programmer had written this:

  ```nim
  var i = 0
  while i < len(a):
    var ch = a[i]
    echo ch
    inc i
  ```

If the iterator yields a tuple, there can be as many iteration variables
as there are components in the tuple. The i'th iteration variable's type is
the type of the i'th component. In other words, implicit tuple unpacking in a
for loop context is supported.


## Converters

A converter is like an ordinary proc except that it enhances
the "implicitly convertible" type relation (see [Convertible relation]):

  ```nim
  type
    MyInt = distinct int

  # bad style ahead: Nim is not C.
  converter toBool(x: MyInt): bool = x != 0

  if MyInt(4):
    echo "compiles"
  ```


A converter can also be explicitly invoked for improved readability. Note that
implicit converter chaining is not supported: If there is a converter from
type A to type B and from type B to type C, the implicit conversion from A to C
is not provided.


## Error handling

The error handling is based on a standardized error enum called `ErrorCode`. It is supposed to cover all possible errors that can occur in a program. Among its possible values are `Success`, `Failure`, `OverflowError`, `IndexError` and `SyntaxError`. As it is a normal enum, it can be used as a return type:

```nim
proc problem(): ErrorCode = return Failure
```

But it can also be "raised" which then influences the control flow of the caller:

```nim
proc problem() {.raises.} = raise Failure
```

A routine that can raise an error must always be annotated with `{.raises.}`. In order to handle an error use a `try` statement:

```nim
try:
  problem()
except ErrorCode as e:
  echo "Error: ", e
```

### Try statement

A try statement has the general form: `try <statements> except <statements> finally <statements>`.

The statements after the `try` are executed in sequential order unless an error is raised. If an error is raised the `except` block is executed. Regardless of whether an error is raised or not, the `finally` block is executed, it runs after the `except` block.

For Example:

```nim
# read the first two lines of a text file that should contain numbers
# and tries to add them
var f: File
if open(f, "numbers.txt"):
  try:
    var a = readLine(f)
    var b = readLine(f)
    echo "sum: " & $(parseInt(a) + parseInt(b))
  except ErrorCode as e:
    case e
    of OverflowError:
      echo "overflow!"
    of ValueError:
      echo "value error!"
    of IOError:
      echo "io error!"
    of SyntaxError:
      echo "syntax error!"
    of RangeError:
      echo "range error!"
    else:
      echo "unknown error! ", e
  finally:
    close(f)
```

### Try expression

Try can also be used as an expression; the type of the `try` branch then needs to fit the types of `except` branches, but the type of the `finally` branch always has to be `void`:

```nim test
from std/strutils import parseInt

let x = try: parseInt("133a")
        except: -1
        finally: echo "hi"
```


To prevent confusing code there is a parsing limitation; if the `try` follows a `(` it has to be written as a one liner:

```nim test
from std/strutils import parseInt
let x = (try: parseInt("133a") except: -1)
```


### Defer statement

The `defer` statement is syntactic sugar for a `finally` section of a `try` statement.

For example:

```nim
proc p() =
  setup()
  try:
    use()
  finally:
    atLast()
```

Can be written as:

```nim
proc p() =
  setup()
  defer: atLast()
  use()
```

The `defer` statement schedules a block of code to be executed when the current scope exits, regardless of how it exits (normal completion, exception, or early return). This is useful for cleanup operations:

```nim
proc processFile(filename: string) =
  var f: File
  if open(f, filename):
    defer: close(f)  # Will be called when proc exits
    # Process the file...
    if someError():
      return  # close(f) is still called
    # More processing...
  # close(f) is called here if file was opened
```

The `defer` statement can be used in any block scope (procs, methods, iterators, etc.). Multiple `defer` statements in the same scope are executed in reverse order (last in, first out):

```nim
proc example() =
  defer: echo "third"
  defer: echo "second"
  defer: echo "first"
  echo "body"
  # Output: body, first, second, third
```



## Type sections

Example:

  ```nim
  type # example demonstrating mutually recursive types
    Node = ref object  # an object managed by the garbage collector (ref)
      le, ri: Node     # left and right subtrees
      sym: ref Sym     # leaves contain a reference to a Sym

    Sym = object       # a symbol
      name: string     # the symbol's name
      line: int        # the line the symbol was declared in
      code: Node       # the symbol's abstract syntax tree
  ```

A type section begins with the `type` keyword. It contains multiple
type definitions. A type definition binds a type to a name. Type definitions
can be recursive or even mutually recursive. Nominal types like `objects`
or `enums` can only be defined in a `type` section.



## Concepts

A concept is a description of a constraint, it describes what operations a type must provide so that the type fulfills the concept. For example:

```nim
type
  Comparable = concept
    proc `<=`(a, b: Self): bool
    proc `==`(a, b: Self): bool
    proc `<`(a, b: Self): bool
```

`Self` stands for the currently defined concept itself. It is used to avoid a recursion, `proc <=(a, b: Comparable): bool` is invalid.

A concept is a pure compile-time mechanism that is required to type-check generic code, it is not a runtime mechanism! It is **not** comparable to a C#/Java interface.

### Atoms and containers

Concepts come in two forms: Atoms and containers. A container is a generic concept like `Iterable[T]`, an atom always lacks any kind of generic parameter (as in `Comparable`).

Syntactically a concept consists of a list of proc and iterator declarations.


### Atomic concepts

More examples for atomic concepts:

```nim
type
  Comparable = concept # no T, an atom
    proc cmp(a, b: Self): int

  ToStringable = concept
    proc `$`(a: Self): string

  Hashable = concept
    proc hash(x: Self): int
    proc `==`(x, y: Self): bool

  Swapable = concept
    proc swap(x, y: var Self)
```

### Containers concepts

A container has at least one generic parameter (most often called `T`). The first syntactic usage of the generic parameter specifies how to infer and bind `T`. Other usages of T are then checked to match what it was bound to.

For example:

```nim
type
  Indexable[T] = concept # has a T, a collection
    proc `[]`(a: Self; index: int): var T # we need to describe how to infer 'T'
    # and then we can use the 'T' and it must match:
    proc `[]=`(a: var Self; index: int; value: T)
    proc len(a: Self): int
```

Nothing interesting happens when we use multiple generic parameters:

```nim
type
  Dictionary[K, V] = concept
    proc `[]`(a: Self; key: K): V
    proc `[]=`(a: var Self; key: K; value: V)
```

The usual ": Constraint" syntax can be used to add generic constraints to the involved generic parameters:

```nim
type
  Dictionary[K: Hashable; V] = concept
    proc `[]`(a: Self; key: K): V
    proc `[]=`(a: var Self; key: K; value: V)
```


## Generics

Generics are a means to parametrize procs, iterators or types with `type parameters`:idx:. Depending on the context, the brackets are used either to introduce type parameters or to instantiate a generic proc, iterator, or type.

The following example describes a generic proc `find` that can be used to look for an element in any container that fulfills the `Findable` constraints:

```nim
type
  Findable[T] = concept
    iterator items(x: Self): T
    proc `==`(a, b: T): bool

proc find[T](x: Findable[T]; elem: T): int =
  var i = 0
  for a in items(x):
    if a == elem: return i
    inc i
  return -1
```

Thanks to the `x` being declared as `Findable[T]`, it is known that the element `a` of the collection is of type `T` and that `T` supports equality comparisons via `==`.

This find function can be used with any collection that fulfills the `Findable` concept, for example:

```nim
type
  MyCollection = object
    data: seq[int]

proc items(x: MyCollection): int =
  return x.data

var myCollection = MyCollection(data: @[1, 2, 3, 4, 5])
echo find(myCollection, 3) # 2
```

These form of generics are called "checked generics" because the typing rules are checked at the point of definition and also at the point of instantiation.


### Untyped generics

There are also "unchecked generics" which are only checked at the point of instantiation. These can be accessed via the `{.untyped.}` pragma:

```nim
proc processUntyped[T](x: T): string {.untyped.} =
  when T is string:
    "String: " & x
  elif T is int:
    "Integer: " & $x
  else:
    "Unknown type: " & $x

echo process("hello")        # Works with default behavior
echo processUntyped("hello") # Works with untyped pragma
```


### Generic inference restrictions

The types `var T` and `typedesc[T]` cannot be inferred in a generic
instantiation. The following is not allowed:

  ```nim  test = "nim c $1"  status = 1
  proc g[T](f: proc(x: T); x: T) =
    f(x)

  proc c(y: int) = echo y
  proc v(y: var int) =
    y += 100
  var i: int

  # allowed: infers 'T' to be of type 'int'
  g(c, 42)

  # not valid: 'T' is not inferred to be of type 'var int'
  g(v, i)

  # also not allowed: explicit instantiation via 'var int'
  g[var int](v, i)
  ```


## Templates

A template is a simple form of a macro: It is a simple substitution
mechanism that operates on Nim's abstract syntax trees. It is processed in
the semantic pass of the compiler.

The syntax to *invoke* a template is the same as calling a proc.

Example:

  ```nim
  template `!=` (a, b: untyped): untyped =
    # this definition exists in the system module
    not (a == b)

  assert(5 != 6) # the compiler rewrites that to: assert(not (5 == 6))
  ```

The `!=`, `>`, `>=`, `in`, `notin`, `isnot` operators are in fact
templates:

| `a > b` is transformed into `b < a`.
| `a in b` is transformed into `contains(b, a)`.
| `notin` and `isnot` have the obvious meanings.

The "types" of templates can be the symbols `untyped`,
`typed` or `typedesc`. These are "meta types", they can only be used in certain
contexts. Regular types can be used too; this implies that `typed` expressions
are expected.



### Passing a code block to a template

One can pass a block of statements as the last argument to a template
following the special `:` syntax:

  ```nim  test = "nim c $1"
  template withFile(f, fn, mode, actions: untyped): untyped =
    var f: File
    if open(f, fn, mode):
      try:
        actions
      finally:
        close(f)
    else:
      quit("cannot open: " & fn)

  withFile(txt, "ttempl3.txt", fmWrite):  # special colon
    txt.writeLine("line 1")
    txt.writeLine("line 2")
  ```

In the example, the two `writeLine` statements are bound to the `actions`
parameter.


Usually, to pass a block of code to a template, the parameter that accepts
the block needs to be of type `untyped`. Because symbol lookups are then
delayed until template instantiation time:

  ```nim  test = "nim c $1"  status = 1
  template t(body: typed) =
    proc p = echo "hey"
    block:
      body

  t:
    p()  # fails with 'undeclared identifier: p'
  ```

The above code fails with the error message that `p` is not declared.
The reason for this is that the `p()` body is type-checked before getting
passed to the `body` parameter and type checking in Nim implies symbol lookups.
The same code works with `untyped` as the passed body is not required to be
type-checked:

  ```nim  test = "nim c $1"
  template t(body: untyped) =
    proc p = echo "hey"
    block:
      body

  t:
    p()  # compiles
  ```



### typeof operator

One can obtain the type of a given expression by constructing a `typeof`
value from it:

  ```nim
  var x = 0
  var y: typeof(x) # y has type int
  ```


If `typeof` is used to determine the result type of a proc/iterator/converter
call `c(X)` (where `X` stands for a possibly empty list of arguments), the
interpretation, where `c` is an iterator, is preferred over the
other interpretations, but this behavior can be changed by
passing `typeOfProc` as the second argument to `typeof`:

  ```nim  test = "nim c $1"
  iterator split(s: string): string = discard
  proc split(s: string): seq[string] = discard

  # since an iterator is the preferred interpretation, this has the type `string`:
  assert typeof("a b c".split) is string

  assert typeof("a b c".split, typeOfProc) is seq[string]
  ```




### Import statement

After the `import` keyword, a list of module names can follow or a single
module name followed by an `except` list to prevent some symbols from being
imported:

  ```nim  test = "nim c $1"  status = 1
  import std/strutils except `%`, toUpperAscii

  # doesn't work then:
  echo "$1" % "abc".toUpperAscii
  ```


It is not checked that the `except` list is really exported from the module.
This feature allows us to compile against different versions of the module,
even when one version does not export some of these identifiers.

The `import` statement is only allowed at the top level.

String literals can be used for import/include statements.


### Include statement

The `include` statement does something fundamentally different than
importing a module: it merely includes the contents of a file. The `include`
statement is useful to split up a large module into several files:

  ```nim
  include fileA, fileB, fileC
  ```


### Module names in imports

A module alias can be introduced via the `as` keyword, after which the original module name is inaccessible:

  ```nim
  import std/strutils as su, std/sequtils as qu

  echo su.format("$1", "lalelu")
  ```

The notations `path/to/module` or `"path/to/module"` can be used to refer to a module in subdirectories:

  ```nim
  import lib/pure/os, "lib/pure/times"
  ```

Note that the module name is still `strutils` and not `lib/pure/strutils`,
thus one **cannot** do:

  ```nim
  import lib/pure/strutils
  echo lib/pure/strutils.toUpperAscii("abc")
  ```

Likewise, the following does not make sense as the name is `strutils` already:

  ```nim
  import lib/pure/strutils as strutils
  ```


### From import statement

After the `from` keyword, a module name followed by
an `import` to list the symbols one likes to use without explicit
full qualification:

  ```nim  test = "nim c $1"
  from std/strutils import `%`

  echo "$1" % "abc"
  # always possible: full qualification:
  echo strutils.replace("abc", "a", "z")
  ```

It's also possible to use `from module import nil` if one wants to import
the module but wants to enforce fully qualified access to every symbol
in `module`.


### Export statement

An `export` statement can be used for symbol forwarding so that client
modules don't need to import a module's dependencies:

  ```nim
  # module B
  type MyObject* = object
  ```

  ```nim
  # module A
  import B
  export B.MyObject

  proc `$`*(x: MyObject): string = "my object"
  ```


  ```nim
  # module C
  import A

  # B.MyObject has been imported implicitly here:
  var x: MyObject
  echo $x
  ```

When the exported symbol is another module, all of its definitions will
be forwarded. One can use an `except` list to exclude some of the symbols.

Notice that when exporting, one needs to specify only the module name:

  ```nim
  import foo/bar/baz
  export baz
  ```



### Scope rules

Identifiers are valid from the point of their declaration until the end of
the block in which the declaration occurred. The range where the identifier
is known is the scope of the identifier. The exact scope of an
identifier depends on the way it was declared.

#### Block scope

The *scope* of a variable declared in the declaration part of a block
is valid from the point of declaration until the end of the block. If a
block contains a second block, in which the identifier is redeclared,
then inside this block, the second declaration will be valid. Upon
leaving the inner block, the first declaration is valid again. An
identifier cannot be redefined in the same block, except if valid for
proc or iterator overloading purposes.


#### Tuple or object scope

The field identifiers inside a tuple or object definition are valid in the
following places:

* To the end of the tuple/object definition.
* Field designators of a variable of the given tuple/object type.
* In all descendant types of the object type.

#### Module scope

All identifiers of a module are valid from the point of declaration until
the end of the module. Identifiers from indirectly dependent modules are *not*
available. The `system`:idx: module is automatically imported in every module.

If a module imports the same identifier from two different modules, the
identifier is considered ambiguous, which can be resolved in the following ways:

* Qualifying the identifier as `module.identifier` resolves ambiguity
  between modules. (See below for the case that the module name itself
  is ambiguous.)
* Calling the identifier as a routine makes overload resolution take place,
  which resolves ambiguity in the case that one overload matches stronger
  than the others.
* Using the identifier in a context where the compiler can infer the type
  of the identifier resolves ambiguity in the case that one definition
  matches the type stronger than the others.

  ```nim
  # Module A
  var x*: string
  proc foo*(a: string) =
    echo "A: ", a
  ```

  ```nim
  # Module B
  var x*: int
  proc foo*(b: int) =
    echo "B: ", b
  ```

  ```nim
  # Module C
  import A, B

  foo("abc") # A: abc
  foo(123) # B: 123
  let inferred: proc (x: string) = foo
  foo("def") # A: def

  write(stdout, x) # error: x is ambiguous
  write(stdout, A.x) # no error: qualifier used

  proc bar(a: int): int = a + 1
  assert bar(x) == x + 1 # no error: only A.x of type int matches

  var x = 4
  write(stdout, x) # not ambiguous: uses the module C's x
  ```
Modules can share their name, however, when trying to qualify an identifier with the module name the compiler will fail with ambiguous identifier error. One can qualify the identifier by aliasing the module.


```nim
# Module A/C
proc fb* = echo "fizz"
```


```nim
# Module B/C
proc fb* = echo "buzz"
```


```nim
import A/C
import B/C

C.fb() # Error: ambiguous identifier: 'C'
```


```nim
import A/C as fizz
import B/C

fizz.fb() # Works
```



## Pragmas

Pragmas give the compiler additional information /
commands without introducing a massive number of new keywords. Pragmas are
processed on the fly during semantic checking. Pragmas are enclosed in the
special `{.` and `.}` curly brackets.



### noreturn pragma
The `noreturn` pragma is used to mark a proc that never returns.



### final pragma
The `final` pragma can be used for an object type to specify that it
cannot be inherited from. Note that inheritance is only available for
objects that inherit from an existing object (via the `object of SuperType`
syntax) or that have been marked as `inheritable`.



### error pragma

The `error` pragma can be used to
annotate a symbol (like an iterator or proc). The *usage* of the symbol then
triggers a static error. This is especially useful to rule out copy operations:

  ```nim
  proc `=copy`(dest: var MyType, source: MyType) {.error.}
  ```



### nodecl pragma
The `nodecl` pragma can be applied to almost any symbol (variable, proc,
type, etc.) and is sometimes useful for interoperability with C:

  ```nim
  var
    EACCES {.importc, nodecl.}: cint # pretend EACCES was a variable, as
                                     # Nim does not know its value
  ```

However, the `header` pragma is often the better alternative.


### Header pragma

The `header` pragma is very similar to the `nodecl` pragma: It can be
applied to almost any symbol and specifies that it should not be declared
and instead, the generated code should contain an `#include`:c:\:

  ```nim
  type
    PFile {.importc: "FILE*", header: "<stdio.h>".} = distinct pointer
      # import C's FILE* type; Nim will treat it as a new pointer type
  ```

The `header` pragma always expects a string constant. The string constant
contains the header file: As usual for C, a system header file is enclosed
in angle brackets: `<>`:c:. If no angle brackets are given, Nim
encloses the header file in `""`:c: in the generated C code.



### Bycopy pragma

The `bycopy` pragma can be applied to an object or a proc param. It instructs the compiler to pass the type by value to procs:

  ```nim
  type
    Vector {.bycopy.} = object
      x, y, z: float
  ```

The compiler automatically determines whether a parameter is passed by value or
by reference based on the parameter type's size. If a parameter must be passed by value
or by reference, (such as when interfacing with a C library) use the bycopy or byref pragmas.
Notice that a parameter marked as `byref` takes precedence over a type marked as `bycopy`.


### Byref pragma

The `byref` pragma can be applied to an object or a proc param.
When applied to a type it instructs the compiler to pass the type by reference
(hidden pointer) to procs. When applied to a param it will take precedence, even
if the the type was marked as `bycopy`. When an `importc` type has a `byref` pragma or
parameters are marked as `byref` in an `importc` proc, these params translate to pointers.


### Varargs pragma
The `varargs` pragma can be applied to procs, proc types and templates.



### Threadvar pragma

A variable can be marked with the `threadvar` pragma, which makes it a
`thread-local`:idx: variable.

  ```nim
  var checkpoints* {.threadvar.}: seq[string]
  ```

Due to implementation restrictions, thread-local variables cannot be
initialized within the `var` section. (Every thread-local variable needs to
be replicated at thread creation.)


## Plugins

Plugins are a way to extend the language with new functionality. A plugin is a template that lacks a body. Instead it has a `{.plugin.}` pragma listing the Nim program that implements the plugin.

For example, rewriting the following template as a plugin:

```nim
template generateEcho(s: string) = echo s
```

Becomes:

```nim
import std / syncio

template generateEcho(s: string) {.plugin: "deps/mplugin1".}

generateEcho("Hello, world!")
```

In "deps/mplugin1.nim" there is the implementation:

```nim
import nimonyplugins

proc tr(n: Node): Tree =
  result = createTree()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.withTree CallS, info:
    result.addIdent "echo"
    result.takeTree n

var inp = loadTree()
saveTree tr(beginRead inp)
```

**Note that plugins are compiled with Nim 2, not Nimony as Nimony is not considered stable enough.**

Plugins that are attached to a template receive only the code that is related to the template invocation. But `.plugin` can also be a statement of its own, then it is a so called "module plugin".


### Plugins search

The filename that is passed to the `.plugin` pragma is relative to the directory the string literal originates from.

For example:
`/dirA/a.nim`

```nim
template foo =
  {.plugin: "myplugin".}
```

`/dirB/b.nim`

```nim
import ../dirA/a
foo()
```

will search for `myplugin.nim` in `dirA` (and in `std`).


### Module plugins

A module plugin receives the full code of a module. It needs to output back the complete module with some of its transformations locally applied.

To call a module plugin, use the `.plugin` pragma as a statement on its own:

```nim
{.plugin: "stdplugins/cps".}
```

### Type plugins

Module plugins can also be attached to a nominal type (or a generic type that becomes a nominal type after instantiation). These plugins are invoked for every module that uses the type. This mechanism can replace Nim's "term rewriting macros":

```nim
type
  Matrix {.plugin: "avoidtemps".} = object
    a: array[4, array[4, float]]

proc `*=`(x: var Matrix; y: Matrix) = ...
proc `+=`(x: var Matrix; y: Matrix) = ...
proc `-=`(x: var Matrix; y: Matrix) = ...

proc `*`(x, y: Matrix): Matrix =
  result = x; result *= y
proc `+`(x, y: Matrix): Matrix =
  result = x; result += y
proc `-`(x, y: Matrix): Matrix =
  result = x; result -= y
```

Code like `let e = a*b + c - d` is then rewritten to:

```nim
var e = a
e *= b
e += c
e -= d
```

Avoiding the creation of temporary matrices entirely.

While the code for the avoidtemps plugin is beyond the scope of this document, this is a classical compiler transformation.


### Import plugins

The `import` statement can be combined with a plugin pragma to load a module that is the result of a plugin output:

```nim
import (path/foo) {.plugin: "std/v2".}
```

This syntax imports the module `path/foo` **from the plugin** `std/v2`. This mechanism can be used to import code from a foreign programming language.

The plugin does not receive Nim code but only the path `path/foo`.

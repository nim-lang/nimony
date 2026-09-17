# Nil Safety in Nimony

Nimony makes all pointer-like types (`ref`, `ptr`, `pointer`, `cstring`, procedural types)
**not-nil by default**. This eliminates null pointer dereferences at compile time.


## Not-nil by default

A plain `ref T` or `ptr T` can never hold `nil`. The compiler rejects any attempt to
assign `nil` to it or leave it uninitialized:

```nim
type
  Node = ref object
    data: int

var n: Node       # Error at module level: no default value to initialize with
var m: Node = nil # Error: cannot assign nil to a not-nil type
```


## No default value

A not-nil pointer is the one leaf type with no value in zeroed storage: zeroed
storage reads as `nil`, which is exactly what the type rules out. Anything built
out of one inherits that — an array element, a tuple slot, an object field
without a default of its own:

```nim
type
  Node = ref object
    data: int
  Pair = object
    n: Node
    count: int

var a: array[8, Node]   # Error: `array[8, Node]` has no default value
var p: Pair             # Error: so has `Pair`, through its `n` field
var q = default(Node)   # Error: `default` has no value to hand back either
```

A field that states a default of its own answers for itself, and the type is
default-initializable again:

```nim
type Seeded = object
  n: Node = Node(data: 0)
  count: int

var s: Seeded           # fine
```

The rule bites at **module level**, where a variable's storage is simply zeroed:
a global is readable from another module and another thread, so no flow analysis
can stand behind it. A **local** owes nothing at its declaration — `var r: Node`
is fine as long as a write reaches it before any read, which the initialization
analysis checks:

```nim
proc p(): Node = Node(data: 1)

proc ok =
  var r: Node           # fine
  r = p()
  echo r.data

proc notOk =
  var r: Node
  echo r.data           # Error: cannot prove that r has been initialized
```

The same rule is why `newSeq[T](n)` and `setLen` ask for a `T` that has a
default; `shrink` and `add` do not.


## Nullable types with `nil` prefix

To declare a type that can hold `nil`, prefix it with `nil`:

```nim
type
  Node = ref object
    data: int
    next: nil Node  # can be nil

var x: nil ptr int = nil
var y: nil ref int = nil
var z: nil cstring = nil
var t: nil pointer = nil
```

The compiler then enforces that every dereference is guarded by a nil check:

```nim
proc process(x: nil ref int) =
  if x != nil:
    echo x[]  # OK, guarded
  # echo x[]  # Error: x might be nil
```

This tracking works through control flow:

```nim
var node: nil Node = nil
if someCondition():
  node = Node(data: 42)

if node != nil:
  echo node.data  # OK after nil check
```


## Unchecked pointer types

For low-level code and FFI, the `unchecked` prefix disables nil tracking entirely.
The compiler performs no nil-checking on `unchecked` pointer types:

```nim
type
  MemFile = object
    mem: unchecked pointer
    size: int
```

`unchecked` can be applied to `ref`, `ptr`, `pointer` and `cstring`. Use this where a pointer may or may not be nil and you want to manage the checks yourself. This is useful for FFI or when you have an invariant such as "x.status == Success implies x.field != nil" which are currently beyond the modelling power of the type system.


## Procedural types

Procedural types follow the same rules. A bare `proc (x: int)` cannot be `nil`:

```nim
var callback: proc (x: int) # must be initialized to a valid proc

var optCallback: nil proc (x: int) = nil # nullable proc var
if optCallback != nil:
  optCallback(42)
```


## Porting Nim 2 code: the `lenientnils` feature

The `.feature: "lenientnils"` pragma restores classic Nim behavior for a module,
making `ref`, `ptr`, `pointer`, `cstring` and proc types (`proc`) nilable by default:

```nim
{.feature: "lenientnils".}

# In this module, all pointer-like types can be nil, just like in Nim 2.
var x: ref int = nil  # OK
var y: ptr int = nil   # OK
```

This is intended for **incremental porting** of existing Nim code. New Nimony code
should use explicit `nil T` annotations where nilability is needed.


## Common patterns

### Nilable fields in data structures

Tree and list nodes typically have nilable child pointers:

```nim
type
  Node = ref object
    data: int
    left, right: nil Node

proc depth(n: nil Node): int =
  if n == nil: return 0
  return 1 + max(depth(n.left), depth(n.right))
```

### Resource handles

Types that manage resources often use a nilable pointer to track whether the
resource has been released:

```nim
type
  MyResource = object
    handle: nil ptr Handle

proc `=destroy`(x: MyResource) =
  if x.handle != nil:
    freeHandle(x.handle)

proc `=wasMoved`(x: var MyResource) =
  x.handle = nil
```


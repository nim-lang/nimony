# Borrow Checking in Nimony

## Introduction

Nimony provides memory safety through a simple borrow checking system. Unlike Rust's complex lifetime system, Nimony uses straightforward path-based rules that leverage Nim's existing semantics.

**Key principles:**
- Safety by default for value types
- Simple, teachable rules without lifetime annotations
- Graceful degradation when analysis is uncertain
- Leverages Nim's value semantics and `var` keyword

## Core Concepts

### Borrowing

When you iterate over a collection or take a reference to part of a data structure, you're **borrowing** from it:

```nim
type Data = object
  items: seq[Item]
  metadata: Metadata

var data: Data

for item in mitems(data.items):
  # You're borrowing from 'data.items'
  item.value = 10
```

During the borrow, certain operations on the borrowed path are restricted to ensure safety.

### The Witness Path

When you borrow, you establish a **witness path** - a path expression that must remain stable for the borrow's duration:

```nim
for item in mitems(data.items):
  # Witness path: 'data.items'
  # This path witnesses that the structure stays valid
```

### Simple Path Expressions

Nimony only allows borrowing from **simple paths** - expressions that are locally analyzable:

```nim
# ✅ Simple paths (borrowable):
data
data.items
data.nested.field
data.items[i]
ptr[]              # Pointer with dereference at the end

# ❌ Complex paths (not directly borrowable):
ptr[].items        # Dereference in the middle
getPtr()[].field   # Function call in path
table[key].items   # Dynamic lookup
```

**Why?** Simple paths let the compiler verify safety locally without global analysis.

## Borrow Checking Rules

### Rule 1: Prefix Exclusion

During a borrow, you cannot mutate the borrowed path or any of its **prefixes**:

```nim
for item in mitems(data.items):
  # ❌ Forbidden:
  data = newData           # Prefix of 'data.items'
  data.items = @[]         # The borrowed path itself

  # ✅ Allowed:
  data.metadata = x        # Disjoint sibling
  item.value = 10          # Mutation through the borrow (the point!)
```

**The rule:** If you borrow from `a.b.c`, you cannot mutate `a`, `a.b`, or `a.b.c`.

### Rule 2: Read-Only Access is Allowed

Read-only access to borrowed paths is fine:

```nim
for item in mitems(data.items):
  echo data.name           # ✅ Read-only
  process(data)            # ✅ Non-var parameter (gets copy)
  let n = data.items.len   # ✅ Reading

  # ❌ Still forbidden:
  mutate(var data)         # var parameter
  data.items = @[]         # Direct mutation
```

### Rule 3: Disjoint Siblings Don't Conflict

Fields at the same level can be independently borrowed:

```nim
for row in mitems(data.rows):
  for col in mitems(data.cols):
    # Both borrows active simultaneously - they're disjoint!
    row.value = compute(col)
```

### Rule 4: Nested Borrowing Works Naturally

```nim
for item in mitems(data.items):
  # Borrow: data.items

  for part in mitems(item.parts):
    # Borrow: item.parts (from loop variable)
    # Works! Prefix exclusion prevents issues
    part.active = true
```

The borrow from `item.parts` makes `item` immutable (prefix exclusion), which is exactly what we need for safety.

## The `with` Statement

When paths are too complex for static analysis, use `with` to safely **move** the data temporarily:

```nim
# Template (user-level code):
template with(path: typed, name: untyped, body: untyped) =
  block:
    var name = move(path)
    defer: path = move(name)
    body

# Usage:
with ptr[].nested.items as items:
  for item in mitems(items):
    complexOperation()
  # items automatically moved back
```

**Cost:** Two pointer swaps (O(1)), not a full copy!

Compare to other languages:
- **Rust:** Must clone the entire collection (O(n))
- **C++:** Easy but unsafe (iterator invalidation)
- **Nimony:** Move temporarily (O(1) and safe)

### When to Use `with`

```nim
# Simple path - no `with` needed:
for x in mitems(data.items):
  process(x)

# Complex path - use `with`:
with getPointer()[].items as items:
  for x in mitems(items):
    process(x)

# During borrow, need to call unknown function:
with data.items as items:
  for x in mitems(items):
    unknownFunc()  # Safe: items was moved
```

## Complete Examples

### Example 1: Simple Iteration

```nim
type Config = object
  users: seq[User]
  settings: Settings

var config: Config

for user in mitems(config.users):
  user.active = true                # ✅ Mutating through borrow
  echo config.settings.timeout      # ✅ Reading disjoint field
  config.settings.updated = now()   # ✅ Mutating disjoint field
```

### Example 2: Nested Iteration

```nim
type Matrix = object
  rows: seq[Row]
  cols: seq[Col]

var matrix: Matrix

for row in mitems(matrix.rows):
  for col in mitems(matrix.cols):
    # Both borrows active, disjoint paths
    row.data[col.index] = compute(row, col)
```

### Example 3: Complex Path with `with`

```nim
proc processData(db: ptr Database) =
  with db[].cache.entries as entries:
    for entry in mitems(entries):
      db[].stats.increment()     # OK: entries was moved
      entry.lastAccess = now()
```

## Comparison to Rust

| Aspect | Rust | Nimony |
|--------|------|---------|
| Annotations | Lifetime parameters in signatures | None |
| Nested iteration | Complex lifetime management | Just works |
| Mental model | Ownership + lifetimes | Path prefix exclusion |
| Function analysis | Often needed | Black box (signature sufficient) |
| Escape hatch | `.clone()` (expensive O(n)) | `with` (cheap O(1) move) |
| Learning curve | Steep | Gentle |

## Teaching Guide

### For Beginners

Start with the simple rule:

> "When you borrow from `a.b.c`, you can't change `a`, `a.b`, or `a.b.c`. You can change other fields like `a.other` or `a.b.other`."

```nim
for x in mitems(data.items):
  # data.items is borrowed, so:
  data.items = @[]     # ❌ Can't change it
  data = newData       # ❌ Can't change its parent
  data.other = 5       # ✅ Can change siblings
```

### For Intermediate Users

Introduce `with` for complex cases:

```nim
# When the compiler says "path too complex":
for x in mitems(ptr[].items):  # ERROR

# Use with:
with ptr[].items as items:
  for x in mitems(items):      # OK!
```

## Implementation Notes

> **This section is primarily for compiler implementers.**

### Path Analysis

Implement a simple grammar checker for borrowable paths:

```
borrowablePath ::= simplePath deref?
simplePath ::= symbol ('.' field | '[' index ']')*
deref ::= '[]'
```

Check that:
1. No function calls appear in the path
2. At most one dereference, and only at the end
3. No pointer dereferences in the middle of the path

Paths inside `addr(...)` are not checked (unsafe escape hatch). But this is not a special rule, it merely reflects that `addr` does not introduce a borrow operation as it's unsafe.

### Borrow Tracking

Maintain a stack of active borrows during tree traversal:

```nim
type
  BorrowInfo = object
    path: Cursor           # NIF expression being borrowed
    borrower: SymId        # the local variable doing the borrowing
                           # upon `(kill <borrower>)` we know the borrow is over
    isRef: bool            # Ref vs value borrowing

  BorrowChecker = object
    activeBorrows: seq[BorrowInfo]
    errors: seq[BorrowError]
```

On entering a `for`/`mitems` loop or taking an `addr`:
1. Parse the borrowed path
2. Check it's analyzable
3. Add to active borrows

On `(kill ...)` node: remove corresponding borrow

### Conflict Detection

For each mutation during active borrows:

```nim
proc checkConflict(mutation: Cursor, borrows: seq[BorrowInfo]) =
  for borrow in borrows:
    if overlaps(mutation, borrow.path):
      if isPrefix(mutation, borrow.path) or
         isPrefix(borrow.path, mutation):
        error("mutation conflicts with borrow")

      # Check if disjoint siblings
      if areDisjoint(mutation, borrow.path):
        continue  # OK

      error("mutation conflicts with borrow")
```

For value borrowing, check mutations (`StoreJ`, `UnknownJ`).

For ref borrowing (see [cursor-optimization.md](cursor-optimization.md)), check **any use** of the witness path (even reads).

### Integration with NJVL

NJVL provides the infrastructure needed:

- **`(kill x)`** - explicit end of lifetime for borrow tracking
- **`(unknown x)`** - marks mutations for conflict detection
- **Structured control flow** - simplifies lifetime computation

### Loop Handling

Loops work naturally without special handling:

```nim
for x in mitems(data.rows):
  for y in mitems(data.cols):
    # Two simultaneous borrows - check disjointness
```

The prefix exclusion rule prevents problematic mutations:

```nim
for item in mitems(data.items):
  for part in mitems(item.parts):
    # Borrow from item.parts makes 'item' immutable (prefix)
    # This is exactly what we need!
```

### Error Messages

Provide clear, actionable messages:

```
error: cannot assign to 'data' because it is borrowed
  at line 5: for x in mitems(data.items)
  at line 7: data = newData
note: 'data' is a prefix of the borrowed path 'data.items'
hint: if you need to mutate, use 'with':
  with data.items as items:
    for x in mitems(items):
      data = newData  # Now safe - items was moved
```

## Summary

Nimony's borrow checking achieves memory safety through:

1. **Simple path-based rules** - no complex lifetime tracking
2. **Prefix exclusion** - one rule handles most cases
3. **Leveraging Nim's semantics** - value types, var vs non-var
4. **Graceful degradation** - move when analysis is hard
5. **No annotations** - functions stay clean, analysis is local

For automatic performance optimizations with ref types, see [Cursor Optimization](cursor-optimization.md).

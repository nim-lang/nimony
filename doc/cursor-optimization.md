# Cursor Optimization for Ref Types

## Introduction

> **Note:** This document describes an automatic performance optimization. You don't need to understand it to use Nimony safely - it just makes your ref-based code faster!

For `ref` types (linked data structures), Nimony can automatically optimize reference counting (RC) operations away, turning them into "cursors" - zero-cost traversal pointers.

This optimization is **automatic** when the compiler can prove it's safe. You don't need to request it or annotate your code.

## The Problem: RC Overhead in Traversal

```nim
type Node = ref object
  data: int
  next: Node

# Without optimization (expensive):
var cursor = root     # RC increment
while cursor != nil:
  process(cursor)
  cursor = cursor.next  # RC decrement + increment per iteration!
# RC decrement
```

Each loop iteration performs two RC operations (decrement old, increment new). For large lists, this is significant overhead.

## The Solution: Cursor Optimization

```nim
# With optimization (zero-cost):
var cursor = root     # No RC
while cursor != nil:
  process(cursor)
  cursor = cursor.next  # Just pointer assignment
# No RC
```

The compiler proves that `cursor` only points within `root`'s structure and that `root` stays alive, so RC operations are unnecessary.

## When Does Cursor Optimization Apply?

A ref variable can be optimized to a cursor when:

1. **Source is a simple path:** `var cursor = root` (not `var cursor = getNode()`)
2. **Source is a local owner:** Not a parameter (parameters are already borrowed)
3. **Witness path is never used:** During cursor's lifetime, the source path isn't touched
4. **Cursor not passed by var:** `f(cursor)` is fine, `f(var cursor)` blocks optimization

## The Manual Idiom for Parameters

Since parameters are already borrowed (no RC increment on entry), you need to establish a local owner:

```nim
iterator items(root: Node): Node =
  var root = root      # Shadow parameter: establish local ownership (RC++)
  var cursor = root    # Now cursor can be optimized! (no RC)
  while cursor != nil:
    yield cursor
    cursor = cursor.next
  # cursor destroyed (no RC)
  # root destroyed (RC--)
```

**Cost analysis:**
- One RC increment (shadowing parameter)
- Zero RC operations in the loop (cursor optimization)
- One RC decrement (exiting function)
- **Result:** O(1) RC overhead instead of O(n)

This idiom is mainly needed in **standard library iterators** - most user code doesn't need it.

## The Witness Concept

For cursor optimization, the source path acts as a **witness** that keeps the data structure alive:

```nim
var cursor = tree.leftSubtree
# Witness: tree.leftSubtree
# Must not be used at all during cursor's lifetime
```

The witness guarantees:
1. The structure stays allocated (not freed)
2. The structure isn't modified (no reallocation)

## Stricter Than Value Borrowing

For refs, even **read-only** access to the witness is forbidden:

```nim
var cursor = root.leftSubtree
while cursor != nil:
  # ❌ Forbidden (even readonly!):
  echo root                    # Might call methods that mutate
  process(root)                # Non-var, but gets shared ref!

  # ✅ Allowed:
  process(cursor)              # Using cursor, not witness
  root.rightSubtree = x        # Disjoint path
  cursor = cursor.next
```

**Why?** `proc f(node: Node)` receives a **reference** (not a copy), so `f` can mutate the structure through the reference:

```nim
proc innocent(node: Node) =
  node.next = newNode()  # Legal! Changes structure

var cursor = root
cursor = cursor.next
innocent(root)  # Dangerous! Restructures root's graph
cursor.data = x  # cursor might be invalid now
```

## Disjoint Access is Still Allowed

The path precision from borrow checking applies to cursor optimization:

```nim
type Tree = ref object
  leftSubtree: Node    # witness path
  rightSubtree: Node   # disjoint
  metadata: Data       # disjoint

var cursor = tree.leftSubtree
while cursor != nil:
  # ✅ Can access disjoint parts:
  tree.rightSubtree = balance(tree.rightSubtree)
  tree.metadata.updateStats()

  # ❌ Cannot touch witness:
  # tree.leftSubtree = ...

  cursor = cursor.left
```

Mutations to `rightSubtree` don't affect `leftSubtree`, so cursor optimization is still valid!

## Complete Examples

### Example 1: Simple List Traversal (Optimizable)

```nim
iterator items(root: Node): Node =
  var root = root      # Establish local ownership
  var cursor = root    # cursor optimized (zero RC)
  while cursor != nil:
    yield cursor
    cursor = cursor.next

# Usage:
for node in items(myList):
  node.data = process(node.data)
  # Efficient: no RC operations in loop!
```

### Example 2: Using Witness (Not Optimizable)

```nim
var cursor = root
while cursor != nil:
  echo root.size       # ❌ Uses witness - blocks optimization
  cursor = cursor.next
# Falls back to RC operations (still correct, just slower)
```

### Example 3: Tree with Disjoint Access (Optimizable)

```nim
type Tree = ref object
  leftSubtree: Node
  rightSubtree: Node
  metadata: Data

var cursor = tree.leftSubtree

while cursor != nil:
  # ✅ Can access disjoint parts:
  tree.rightSubtree = balance(tree.rightSubtree)
  tree.metadata.updateStats()

  # ❌ Cannot touch witness:
  # tree.leftSubtree = ...

  cursor = cursor.left
```

## Why This Optimization Matters

For performance-critical code with linked structures:

```nim
# Game entity list (thousands of entities):
for entity in world.entities:
  entity.update(deltaTime)
# Without cursor: thousands of RC ops per frame
# With cursor: zero RC ops

# Document processing (large tree):
for node in document.traverse():
  node.process()
# Without cursor: RC overhead on every node
# With cursor: traversal is just pointer chasing
```

## Comparison to Rust

| Aspect | Rust | Nimony |
|--------|------|---------|
| Linked lists | Notoriously difficult | Iterator idiom, then automatic |
| Traversal cost | Zero overhead (ownership) | Zero overhead (cursor optimization) |
| User burden | Must use specific patterns | Write natural code |
| Escape hatch | Complex workarounds | Falls back to RC |

## When Optimization Doesn't Apply

```nim
# Case 1: Parameter without shadowing
iterator items(root: Node): Node =
  var cursor = root    # ❌ root is parameter, can't optimize
  while cursor != nil:
    yield cursor
    cursor = cursor.next
# Falls back to RC (still correct)

# Case 2: Function result
var cursor = getList()  # ❌ Function creates owner
while cursor != nil:
  cursor = cursor.next
# Must use RC (cursor owns the data)

# Case 3: Witness is used
var cursor = root
while cursor != nil:
  log(root)             # ❌ Uses witness
  cursor = cursor.next
# Falls back to RC
```

In all these cases, the code **still works correctly** - it just doesn't get the cursor optimization.

## Implementation Notes

> **This section is primarily for compiler implementers.**

### Cursor Inference Algorithm

After borrow checking, perform cursor optimization:

```nim
proc findCursorCandidates(body: NifNode): seq[CursorCandidate] =
  for node in body.walk():
    if node.kind == nkVar and node.typ.kind == tyRef:
      let source = node.source

      # Must be simple symbol (not field, not call result)
      if source.kind != nkSym:
        continue

      # Must be local owner (not parameter)
      if source.sym.kind == skParam:
        continue

      result.add CursorCandidate(
        sym: node.sym,
        source: source
      )

proc checkCursorSafety(candidate: CursorCandidate): bool =
  # Treat as ref borrowing - witness must not be used at all
  let witness = candidate.source

  for use in findUses(witness, candidate.lifetime):
    if overlaps(use, witness):
      if not areDisjoint(use, witness):
        return false  # Any use of witness or prefixes

  # Check cursor not passed by var
  for use in candidate.sym.uses:
    if use.isVarParam or use.isUnknown:
      return false

  return true
```

Mark successful candidates as cursors (skip RC operations in code generation).

### Integration with Borrow Checking

Cursor optimization **reuses** the borrow checking infrastructure:

1. Use the same path analysis (simple paths)
2. Use the same prefix/disjoint logic
3. Just apply stricter rules (no uses vs no mutations)

This means most of the implementation is shared!

### Code Generation

When a variable is marked as a cursor:

```nim
# Normal ref variable:
var cursor = root    # emit: cursor = nimIncRef(root)
cursor = cursor.next # emit: nimDecRef(cursor); cursor = nimIncRef(cursor.next)
# destroy cursor     # emit: nimDecRef(cursor)

# Cursor-optimized:
var cursor = root    # emit: cursor = root (no RC)
cursor = cursor.next # emit: cursor = cursor.next (no RC)
# destroy cursor     # emit: nothing (no RC)
```

### Fallback Behavior

If cursor optimization fails for any reason:
- ✅ Code still compiles
- ✅ Code still runs correctly
- ⚠️ Just has RC overhead

This graceful degradation means the optimization is always safe to attempt.

## Teaching Cursor Optimization

### For Users

Most users don't need to think about this! Key points:

1. Write natural traversal code
2. Compiler optimizes automatically when safe
3. If not optimized, code still works (just slightly slower)
4. Don't pass the source around during traversal

### For Library Authors

When writing iterators over ref types:

```nim
iterator items(root: Node): Node =
  var root = root      # This line enables cursor optimization
  var cursor = root    # cursor will be zero-cost
  while cursor != nil:
    yield cursor
    cursor = cursor.next
```

This one-line idiom (`var root = root`) unlocks zero-cost iteration for all users of your iterator!

### For Performance Engineers

Profile your code. If you see RC overhead in hot loops:

1. Check if witness is being used in the loop
2. If yes, refactor to avoid using it
3. If no, file a bug - optimization should apply!

## Summary

Cursor optimization provides:

1. **Automatic** - no annotations needed
2. **Zero-cost** - eliminates all RC operations in traversal
3. **Safe** - only applies when provably correct
4. **Graceful** - falls back to RC if unsure

Combined with borrow checking, this gives you:
- **Safety** from borrow checking
- **Performance** from cursor optimization
- **Simplicity** from no annotations

The result: fast linked data structures without manual memory management!

---

For the safety aspects of borrowing, see [Borrow Checking](borrowing.md).


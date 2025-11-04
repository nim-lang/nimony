# SSU (Static Single Use) for NJVL

## Overview

NJVL currently uses SSA (Static Single Assignment) via versioned locations. Since versioning is separate from control flow analysis, we can attach an SSU (Static Single Use) mechanism instead or alongside SSA.

SSU tracks **uses** of variables rather than **assignments**. This is the dual of SSA and is useful for:
- Move semantics (tracking which values can be moved)
- Linear types (ensuring single use)
- Ownership analysis (knowing when ownership transfers)
- Precise lifetime tracking (when values are actually consumed)

## Core Concept

While SSA versions on **writes**:
```
(asgn (v x.0 +1) +9)    # Write increments version
(call use.0 (v x.0 +0))  # Read uses current version
```

SSU versions on **reads/uses**:
```
(var x.0 +0)
(call use.0 (u x.0 +1))  # Use increments use-version
(call use2.0 (u x.0 +2)) # Next use increments again
```

## IR Representation

### `u` (use-version)

The `u` tag represents the use-version of a location. Similar to `v`, but tracks uses instead of writes:

```
(stmts
  (var x.0 +0)
  (call use.0 (u x.0 +1))  # First use
  (call use2.0 (u x.0 +2)) # Second use
  (kill x.0)                # End of lifetime
)
```

### Use-Versioning Rules

1. **Variable declaration**: Creates initial use-version +0 (not yet used)
2. **Variable use**: Each read/use increments the use-version
3. **Variable write**: Does NOT increment use-version (only SSA does)
4. **Address taken**: Use-version stays same (address-of is not a consuming use)


### `dup` instruction

The `dup` instruction duplicates a variable. It is a very special instruction that allows us to create a copy of a variable without "using" it. It is an abstract instruction that has no runtime effect. It is only used to track the use-version of a variable.

**Key property**: `dup` does NOT increment the use-version of the source variable. This allows a variable to be duplicated and then both the original and duplicate can be used independently.

**Example**:

```nim
var x = allocate()
var y = x        # Copy x to y (using dup)
use(x)           # Use x
use2(y)          # Use y
```

Translated to:
```
(stmts
  (var :x.0 (call allocate.0))
  (var :y.0 (dup (u x.0 +0)))        # dup: x use-version stays +0 (NOT incremented!)
  (call use.0 (u x.0 +1))            # x use-version becomes +1
  (call use2.0 (u y.0 +1))           # y use-version becomes +1 (independent of x)
)
```

**Key points**:
- `dup` preserves the source variable's use-version (x.0 stays at +0)
- The duplicate (y.0) gets its own independent use-version tracking
- After `dup`, both variables can be used independently with their own use-version counters
- Without `dup`, we couldn't express "copy x to y" without consuming x's use

**Without `dup`** (impossible in SSU):
```
# This would be invalid:
(var :y.0 (u x.0 +1))      # x is "used" here, but we want to use it again later!
(call use.0 (u x.0 +2))    # x used again - but we already consumed it!
```

**With `dup`** (correct):
```
(var :y.0 (dup (u x.0 +0)))  # x NOT consumed, can still be used
(call use.0 (u x.0 +1))      # x used here
(call use2.0 (u y.0 +1))     # y used independently
```

Without `dup` it would be impossible to create an SSU annotation for a program that uses a variable multiple times. `dup` has no direct SSA equivalent.


### Dual Versioning: Both SSA and SSU

We can track **both** write versions (`v`) and use versions (`u`) simultaneously:

```
(stmts
  (var (v :x.0 +0) +42)           # Write-version +0, use-version implied +0
  (call use.0 (v (u x.0 +1) +0))  # Use-version +1, write-version +0
  (asgn (v x.0 +1) +99)           # Write-version +1, use-version stays +1
  (call use2.0 (v (u x.0 +2) +1)) # Use-version +2, write-version +1
)
```

Or more concisely, we could track use-versions only when needed (for move analysis):

```
(stmts
  (var (v :x.0 +0) +42)
  (call use.0 (v (u x.0 +1) +0))
  (asgn (v x.0 +1) +99)
  (call use2.0 (v (u x.0 +2) +1))
)
```

## Control Flow Handling

### Branching (`ite`)

At control flow splits, uses are **split** (the dual of `join` for SSA):

```nim
var x = allocate()
if cond:
  use1(x)  # Use version +1 in then branch
else:
  use2(x)  # Use version +1 in else branch (independent!)
after
```

Translated to:
```
(stmts
  (var (v :x.0 +0) (call allocate.0))
  (ite
    cond
    (stmts
      (call use1.0 (v (u x.0 +1) +0))
    )
    (stmts
      (call use2.0 (v (u x.0 +1) +0))
    )
    (stmts
      (split x.0 +2 +1 +1)  # New use-version +2, from +1 (then) and +1
    )
  )
  (call after.0)
)
```

The `split` instruction is the dual of `join`:
- `join` merges write-versions from two branches
- `split` merges use-versions from two branches (tracking that both paths consumed a use)

### `split` instruction

```
(split location result-use-version branch1-use-version branch2-use-version)
```

**Semantics:**
- Documents that the variable was used in both branches
- Creates a new use-version that represents the merged state
- Similar to `join` but for uses instead of writes

**For move semantics:**
- Tracks that both branches consumed a use
- Enables analysis of whether move is safe (if variable is dead after both branches)

### Loops (`loop`)

Loops require special handling. The back-edge means a variable could be used multiple times:

```nim
var x = allocate()
while cond:
  use(x)  # Uses x each iteration
after
```

Translated to:
```
(stmts
  (var (v :x.0 +0) (call allocate.0))
  (loop
    (stmts)
    cond
    (stmts
      (call use.0 (v (u x.0 +1) +0))
      (continue
        (split x.0 +1 +0 +1)  # Use-version +1 comes from initial (+0) or prev iteration (+1)
      )
    )
    (stmts)
  )
  (call after.0)
)
```

**For linear types:**
- A loop would violate single-use requirement (unless the loop is guaranteed to run exactly once)
- Would need to either error or allow multiple uses in loops explicitly

**For move semantics:**
- Tracks that x was used in the loop
- After the loop, x's use-version reflects all uses within the loop

## Implementation

### Use-Version Table

Similar to `VersionTab`, we'd need a `UseVersionTab`:

```nim
type
  UseVersionTab* = object
    currentUseVersion: Table[SymId, int]  # Track current use-version per symbol
    useHistory: TokenBuf                  # Track uses for split generation

proc newUseFor*(v: var UseVersionTab, symId: SymId) =
  ## Increment use-version when a variable is used
  v.useHistory.addSymUse symId, NoLineInfo
  v.currentUseVersion.mgetOrPut(symId, -1) += 1

proc getUseVersion*(v: UseVersionTab, symId: SymId): int =
  v.currentUseVersion.getOrDefault(symId, -1)
```

### VL Pass Modifications

In `vl.nim`, modify `trExpr` to track uses:

```nim
proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    let s = n.symId
    let writeV = c.vt.getVersion(s)
    let useV = c.ut.getUseVersion(s)  # Get use-version
    if useV >= 0:
      c.ut.newUseFor(s)  # Increment use-version

    if writeV < 0:
      if useV >= 0:
        dest.add tagToken("u", info)
        dest.addSymUse s, info
        dest.addIntLit useV + 1, info  # Use the incremented version
        dest.addParRi()
      else:
        dest.addSymUse s, info
    else:
      # Both write and use versions
      dest.add tagToken("v", info)
      if useV >= 0:
        dest.add tagToken("u", info)
        dest.addSymUse s, info
        dest.addIntLit useV + 1, info
        dest.addParRi()
      dest.addSymUse s, info
      dest.addIntLit writeV, info
      dest.addParRi()
    inc n
```

### Split Generation

Modify `trIte` to generate `split` instructions:

```nim
proc trIte(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # ... existing code ...

  # Generate splits for use-versions
  let splitData = combineSplit(c.ut, IfSplit)
  for s, splitVar in splitData:
    if isValid(splitVar):
      dest.add tagToken("split", info)
      dest.addSymUse s, info
      dest.addIntLit splitVar.newUse, info    # New merged use-version
      dest.addIntLit splitVar.branch1, info   # Use-version from then branch
      dest.addIntLit splitVar.branch2, info   # Use-version from else branch
      dest.addParRi()
```

## Use Cases

### 1. Move Analysis

SSU enables precise move analysis by tracking uses. However, `dup` complicates "last use" detection because it creates copies without consuming the source.

**The Core Question:**
With `dup`, what is the "last use" of a variable? Since `dup` doesn't increment use-version, the last use-version might not correspond to the actual last use needed for move analysis.

**Key Insight:**
SSU tracks **consuming uses** (operations that need the value), while move analysis needs to know the **last place the variable is needed**. These are different concepts:

1. **Consuming use**: An operation that uses the value (like a function call). This increments use-version.
2. **Last use for move**: The last place where the variable is needed before it can be moved. This depends on:
   - The last consuming use of the variable itself
   - Whether copies (`dup`) have been made and are still needed

**Example 1: Simple case without `dup`**

```nim
var x = allocate()
use(x)           # Use x
# x is dead here, can move
```

IR:
```
(stmts
  (var (v :x.0 +0) (call allocate.0))
  (call use.0 (v (u x.0 +1) +0))  # x use-version +1 - this is the last use
  (kill x.0)                       # x dead, move is safe
)
```

Here, use-version +1 directly corresponds to the last use. Move is safe after the use.

**Example 2: With `dup` - the complication**

```nim
var x = allocate()
var y = dup(x)   # Copy x to y (x use-version stays +0)
use(x)           # Use x (use-version +1)
use2(y)          # Use y (y use-version +1)
# Question: When is x "last used"?
```

IR:
```
(stmts
  (var (v :x.0 +0) (call allocate.0))
  (var (v :y.0 +0) (dup (v (u x.0 +0) +0)))  # x use-version stays +0
  (call use.0 (v (u x.0 +1) +0))             # x use-version +1
  (call use2.0 (v (u y.0 +1) +0))            # y use-version +1 (independent)
  (kill x.0)
)
```

**Analysis:**
- `use(x)` is the last **consuming use** of x (use-version +1)
- But `dup(x)` created a **copy** of x's value into y
- Since `dup` is a copy (not a move), x and y are independent after the copy
- Therefore, x's last use is indeed `use(x)`, not `use2(y)`
- Move is safe after `use(x)` because y has its own copy

**The Answer:**
SSU use-versions correctly identify the last consuming use **of the variable itself**, not of its copies. For move analysis:

1. **Find the last use-version** of the variable (e.g., x use-version +1)
2. **Find the last `dup`** of the variable before that use
3. **After the last use-version**, the variable can be moved (if dead)
4. **Copies created by `dup`** are tracked independently - they don't affect when the original can be moved

**Why `dup` is needed:**
Without `dup`, we couldn't express "copy x to y" without consuming x. With `dup`, we can:
- Create the copy without consuming x
- Track x and y's uses independently
- Identify that x's last use is separate from y's uses

**Move analysis algorithm:**
```
For variable x:
1. Find all uses of x (use-versions +1, +2, ..., +N)
2. Find the last use-version (e.g., +N at position P)
3. Check: is x dead after position P? (check for kill)
4. If yes, can move x after position P
5. Copies created by dup(x) don't affect this - they're independent
```

**Example 3: Move after copy**

```nim
var x = allocate()
var y = dup(x)   # Copy x
use(x)           # Last use of x
# x can be moved here (y is independent)
use2(y)          # Uses y (x already moved)
```

IR:
```
(stmts
  (var (v :x.0 +0) (call allocate.0))
  (var (v :y.0 +0) (dup (v (u x.0 +0) +0)))  # Copy
  (call use.0 (v (u x.0 +1) +0))             # Last use of x
  # x can be moved here - y is independent
  (call use2.0 (v (u y.0 +1) +0))            # Uses y
  (kill x.0)
)
```

The use-version +1 correctly identifies the last use of x, even though y is used later.

### Simplifying the Duplifier Pass

With SSU, the `duplifier.nim` pass can be significantly simplified. Currently, it performs complex `isLastRead` analysis to determine when to use moves vs `=dup` calls.

**Current approach (without SSU):**
The duplifier uses `isLastUse` (line 59, 413, 839, etc.) which performs control flow analysis to determine if a variable is used again after the current point. This is expensive and complex.

**With SSU:**
The use-version information already tells us:
- If use-version is at the maximum for a variable → this is the last use
- If use-version is not at maximum → variable is used again later

**Simplified algorithm:**
```nim
# Instead of:
if isLastRead(c, arg):
  # Use move
  tr c, arg, WillBeOwned
  callWasMoved c, arg, typ
else:
  # Use dup
  callDup c, arg

# With SSU, we can do:
let useV = getUseVersion(arg)  # Get current use-version
let maxUseV = getMaxUseVersion(arg)  # Get maximum use-version for this variable
if useV == maxUseV:
  # This is the last use - can move!
  tr c, arg, WillBeOwned
  callWasMoved c, arg, typ
else:
  # Variable is used again - must dup
  callDup c, arg
```

**Key benefits:**
1. **No control flow analysis needed**: SSU already computed use-versions
2. **Direct lookup**: Just check if current use-version == max use-version
3. **Simpler code**: No need for complex `isLastUse` traversal
4. **More efficient**: O(1) lookup vs O(n) control flow traversal

**The mapping:**
- `dup` instruction in IR → call `=dup` hook (if needed)
- Last use (use-version == max) → use move semantics
- Not last use → use `=dup` to create copy

**Example transformation:**

Current IR (with SSU annotations):
```
(var (v :x.0 +0) (call allocate.0))
(var (v :y.0 +0) (dup (v (u x.0 +0) +0)))  # dup instruction - map to =dup
(call use.0 (v (u x.0 +1) +0))             # Last use of x (use-version +1 is max)
```

After duplifier:
```
(var (v :x.0 +0) (call allocate.0))
(var (v :y.0 +0) (call =dup.0 (v (u x.0 +0) +0)))  # dup → =dup call
(call use.0 (v (u x.0 +1) +0))                      # Last use - can move
(call =wasMoved.0 (haddr (v x.0 +1)))               # Mark as moved
```

**Important note:**
The duplifier doesn't need to analyze anything - SSU already did the analysis! It just needs to:
1. Check use-versions to determine last use
2. Map `dup` instructions to `=dup` calls
3. Insert `=wasMoved` calls for last uses

This makes the duplifier much simpler and more efficient.

### The `dup` Placement Problem

There's a subtle but important issue with `dup` instructions: **they are abstract and don't increment use-version, but when mapped to `=dup` hooks, they become real operations that do read the variable.**

**The Problem:**
- `dup` in IR: Abstract instruction, doesn't increment use-version, appears "non-consuming"
- `=dup` hook: Real function call that reads/copies the variable, IS consuming
- Danger: If optimizations move `dup` around (thinking it's non-consuming), we might get invalid code after mapping to `=dup`

**Example of the danger:**

```nim
var x = allocate()
use(x)           # Use x
var y = dup(x)   # dup inserted here (use-version stays +0)
```

If an optimizer moves `dup` after `use(x)`:
```
(var (v :x.0 +0) (call allocate.0))
(var (v :y.0 +0) (dup (v (u x.0 +0) +0)))  # dup can be moved?
(call use.0 (v (u x.0 +1) +0))             # use x
```

After moving `dup` after `use`:
```
(var (v :x.0 +0) (call allocate.0))
(call use.0 (v (u x.0 +1) +0))             # use x
(var (v :y.0 +0) (dup (v (u x.0 +0) +0)))  # dup moved here - but x was already used!
```

When mapped to `=dup`:
```
(call use.0 (v (u x.0 +1) +0))             # use x (might invalidate it)
(call =dup.0 (v (u x.0 +0) +0))            # try to dup x - but x may be invalid!
```

**The Solution:**

`dup` instructions must be **semantically constrained** to only appear where copies are actually needed. They cannot be moved freely by optimizations.

**Constraints on `dup` placement:**
1. **`dup` must appear before any uses of the source variable** in the same scope
2. **`dup` cannot be moved across uses** - it's tied to the copy operation location
3. **`dup` appears at the point of copy initialization** (e.g., `var y = dup(x)`)

**Why this works:**
- `dup` is inserted by the SSU pass at the point where a copy is semantically needed
- The SSU construction algorithm should insert `dup` at the correct location (before uses)
- Optimizations should respect that `dup` is tied to variable initialization/assignment

**The SSU construction algorithm must:**
1. Insert `dup` at the point where the copy is needed (e.g., `var y = x`)
2. Ensure `dup` appears before any uses of the source variable
3. Not allow `dup` to be moved across uses by treating it specially in optimization passes

**Example of correct insertion:**

```nim
var x = allocate()
var y = x        # Copy needed here
use(x)           # Use x
use2(y)          # Use y
```

SSU construction inserts `dup` at the copy point:
```
(var (v :x.0 +0) (call allocate.0))
(var (v :y.0 +0) (dup (v (u x.0 +0) +0)))  # dup inserted at copy point
(call use.0 (v (u x.0 +1) +0))             # use x
(call use2.0 (v (u y.0 +1) +0))            # use y
```

This is correct - `dup` appears before `use(x)`, so when mapped to `=dup`, it's safe.

**Verification:**
The duplifier can verify this by checking that `dup` appears before any uses of the source variable (by checking use-versions). If a `dup` appears after a use, that's an error in the SSU construction.



## Comparison: SSA vs SSU

| Aspect | SSA | SSU |
|--------|-----|-----|
| Versions on | Writes | Uses |
| Control flow merge | `join` (combines writes) | `split` (tracks uses from branches) |
| Control flow split | No special handling | Track uses in each branch |
| Useful for | CSE, copy propagation | Move analysis, linear types |
| Can combine | Yes (both together) | Yes (both together) |

## Integration with Existing `kill`

The `kill` instruction already marks end of lifetime. With SSU, we can verify:
- For linear types: variable must have use-version >= 1 (was used)
- For move semantics: variable can be moved if it's dead (after `kill`)

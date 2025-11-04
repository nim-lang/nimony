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

SSU enables precise move analysis by tracking uses:

```nim
var x = allocate()
var y = x        # Copy
use(y)           # Use y (use-version +1)
# Can we move x? Check if x has been used
use2(x)          # Use x (use-version +1)
# x has been used once, move is safe if x is dead after this
```

IR:
```
(stmts
  (var (v :x.0 +0) (call allocate.0))
  (var (v :y.0 +0) (v (u x.0 +0) +0))  # Copy: x use-version stays +0
  (call use.0 (v (u y.0 +1) +0))
  (call use2.0 (v (u x.0 +1) +0))      # x used here
  (kill x.0)
)
```

### 2. Linear Types

For linear types, we can check that each variable is used exactly once:

```
- Variable declaration: use-version +0
- After single use: use-version +1
- Error if used again: use-version would become +2
- Error if unused: use-version still +0 at scope exit
```


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

# NJVL - "No jumps, versioned locations"

This document describes NJVL's benefits, algorithms and design considerations.

## Peephole optimizations

The structured nature of NJVL makes peephole optimizations significantly easier than in traditional unstructured control flow. Two key properties enable this:

### No Labels Between Instructions

In traditional assembly or unstructured IR, labels can appear anywhere, making it difficult to identify instruction sequences that can be optimized together. NJVL's structured control flow ensures that:

- Instructions are grouped into logical blocks (`stmts`, `if`, `loop`, etc.)
- Labels only appear at block boundaries, not between individual instructions
- This creates clear "peephole windows" where consecutive instructions can be analyzed together

Example:
```nim
# Traditional unstructured (hard to optimize):
label1:
  mov eax, 0
  add eax, 5
  cmp eax, 5
  je label2
  mov ebx, eax
label2:
  ret

# NJVL structured (easy to optimize):
stmts:
  (asgn (v eax.0 +1) 0)
  (asgn (v eax.0 +2) (+ (v eax.0 +1) 5))
  (asgn (v tmp.0 +1) (= (v eax.0 +2) 5))
  (ite (v tmp.0 +1)
    (stmts (asgn (v ebx.0 +1) (v eax.0 +2)))
    (stmts))
```

### Kill Instructions Enable Precise Dead Code Detection

NJVL's versioning system with explicit `kill` instructions makes it trivial to determine when a variable becomes dead:

```nim
# Clear peephole optimization opportunity:
(asgn (v x.0 +1) (call expensive_function))
(kill (v x.0 +1))  # Explicit kill - x.0 +1 is now dead!
# Can eliminate the expensive_function call
```

The explicit `kill` instruction immediately marks the variable version as dead, enabling aggressive dead code elimination. This is much more precise than traditional approaches that need to analyze liveness across complex control flow graphs.

### Combined Benefits

Together, these properties enable powerful peephole optimizations:

1. **Instruction fusion**: Consecutive operations can be combined
2. **Dead code elimination**: Kill instructions provide immediate feedback
3. **Constant folding**: Structured blocks make it easy to track constant propagation
4. **Register allocation hints**: Clear variable lifetimes enable better register reuse
5. **Arbitrary pattern detection**: All effects are localized in the representation, enabling detection and simplification of complex patterns

The structured form essentially provides a more "peephole-friendly" representation that traditional unstructured control flow cannot offer. Since all effects are localized and explicit, arbitrary patterns can be detected and simplified without the complexity of analyzing cross-cutting concerns in traditional unstructured representations.

## Separation Logic and Memory Aliasing

NJVL's treatment of memory aliasing through `(unknown)` and the lack of versioning for address-taken locations creates a natural separation logic framework.


### Address-Taken Locations Lack Versioning

When a variable's address is taken, it loses versioning because it may be modified through aliases:

```nim
var x = 42
let p = addr x  # x loses versioning
# x can now be modified through *p, so we can't track versions
```

This is exactly the separation logic principle: **if you can't prove separation, you must assume interference**.

### Separation Logic Properties

1. **Disjointness**: Versioned variables are disjoint from the unknown heap
2. **Frame Property**: Optimizations on versioned variables don't affect unknown locations
3. **Isolation Property**: Versioned variables are not aliased and cannot be affected by unknown locations

### Practical Benefits

This separation enables:
- **Aggressive optimization** of versioned variables (they're "separate" from aliasing concerns)
- **Conservative treatment** of address-taken locations (they're in the "unknown" heap)
- **Clear reasoning** about when optimizations are safe vs. when they must be conservative

The versioning system essentially implements a form of **memory separation** where tracked variables are "owned" by the optimizer and untracked variables are in the "shared unknown heap".

## Path Duplication and Instruction Scheduling

While NJVL's structured control flow enables powerful optimizations, it can initially seem to limit instruction scheduling flexibility compared to CFG-based IRs. However, **path duplication** restores this flexibility while maintaining structured benefits.

### The Path Duplication Transformation

The key insight is that `(if a b c)(d)` can always be transformed into `(if a (b d) (c d))`:

```nim
# Original structured form:
(if condition
  (stmts:
    then_branch)
  (stmts:
    else_branch))
(stmts:
  common_code)

# After path duplication:
(if condition
  (stmts:
    then_branch
    common_code)  # Duplicated
  (stmts:
    else_branch
    common_code)) # Duplicated
```

### Specialization Opportunities

Once paths are duplicated, each copy of `common_code` can be **specialized** based on the path context:

```nim
# In the then branch, we know condition is true
(if condition
  (stmts:
    then_branch
    # common_code specialized knowing condition=true
    specialized_common_code)
  (stmts:
    else_branch
    # common_code specialized knowing condition=false
    specialized_common_code))
```

### Restored Scheduling Flexibility

Path duplication restores CFG-like scheduling capabilities:

1. **Instruction reordering**: Within each path, instructions can be reordered freely
2. **Cross-path optimization**: Common subexpressions can be hoisted to the condition
3. **Path-specific optimizations**: Each path can be optimized based on its specific context
4. **Register allocation**: Each path can have different register allocation strategies

### Example: Loop Unrolling with Path Duplication

```nim
# Original loop:
loop:
  if i < 10:
    body
  i += 1

# After path duplication and specialization:
loop:
  if i < 10:
    # Specialized for i < 10
    body_specialized
    i += 1
  else:
    # Specialized for i >= 10
    # Can eliminate the increment
    pass
```


## Simplified Inlining

NJVL's elimination of `return` and `break` statements makes inlining simpler than traditional approaches.

Since function calls can only be statements or of the form `let/var dest = call()` or `dest = call()` we can replace the entire assignment with an inlined body where the parameters have been replaced by their arguments and `result` by `dest`.

This makes inlining a **simple substitution operation** rather than a complex control flow transformation.


## Move analysis

Move analysis optimizes `copy dest, src` into `move dest, src` when `src` is not used afterward. NJVL makes this optimization significantly simpler than traditional CFG-based approaches.

### Versioning Enables Precise Tracking

In NJVL, the question "is this the last use of x?" becomes "is this the last use of version `(v x.0 +N)`?" This is much more precise because each version has a single definition point (SSA-like).


### Forward-Only Scanning

Because NJVL is structured, move analysis can use simple forward traversal without constructing a CFG:

1. Start at the copy/assignment
2. Scan forward through the structured control flow
3. Stop when finding: another use, a new version, or scope exit


### Loop Back-Edges: The `(continue)` Marker

Loops are the only place where forward scanning becomes tricky due to back-edges. NJVL solves this by requiring explicit `(continue)` markers at the end of loop bodies.

The `(continue)` marker makes the back-edge explicit in the forward scan. Combined with destructor placement, it encodes whether a value is live across the back-edge:

**Safe to move (loop-local variable):**

```
(loop
  (stmts ...)
  cond
  (stmts
    (var (v x.0 +N) allocate)
    (var (v y.0 +0) (v x.0 +N))  # Can we move?
    (call use.0 (v y.0 +0))

    (destroy (v y.0 +0))
    (destroy (v x.0 +N))  # x destroyed before back-edge!
    (continue)            # Back-edge marker
  )
  (after)
)
```

Forward scan sees `(destroy (v x.0 +N))` before `(continue)` → **safe to move**! The value is dead before the back-edge.

**Cannot move (loop-carried variable):**

```
(var (v x.0 +0) init)
(loop
  (stmts)
  cond
  (stmts
    (var (v y.0 +0) (v x.0 +1))  # Copy from loop header version
    (call use.0 (v y.0 +0))

    (destroy (v y.0 +0))
    (asgn (v x.0 +2) newValue)
    # NO (destroy x) here - x is live!
    (continue
      (either x.0 +1 +0 +2))  # x version +1 flows back from +2
  )
  (stmts
    (call use (v x.0 +1))
    (destroy (v x.0 +1))
  )
)
```

Forward scan: No `(destroy x)` before `(continue)` → **cannot move**. The location is live across the back-edge.


### Benefits Over CFG-Based Analysis

1. **No CFG Construction**: Just tree traversal. NJVL's structure IS the control flow.
2. **Version Numbers Guide Search**: Not "is x used again?" but "is x.0 version +3 used again?" - much more precise.
3. **Join Points Are Explicit**: `(join)` nodes mark where versions die and new ones are born. CFG-based analysis must infer this from merge points.
4. **Works for Complex Locations**: The same algorithm handles fields and array elements: `(v (dot obj.0 field) +2)` or `(v (at arr.0 (v i.0 +3)) +5)`.


## Destructor Injection

The structured nature of NJVL makes destructor injection significantly simpler and produces less code than traditional approaches.

### Single Exit Point = Single Cleanup Point

With structured control flow, all paths converge to a single exit point, which means destructors need to be inserted only once per scope.

**Traditional approach (with multiple returns):**

```nim
proc process() =
  var file = openFile("data.txt")
  defer: file.close()

  if not file.valid(): return

  let data = file.read()
  if data.isEmpty(): return

  processData(data)
```

Becomes (in unstructured IR):

```nim
proc process() =
  var file = openFile("data.txt")

  if not file.valid():
    file.close()  # Duplicate 1
    return

  let data = file.read()
  if data.isEmpty():
    file.close()  # Duplicate 2
    return

  processData(data)
  file.close()    # Duplicate 3
```

Three copies of cleanup code! For complex destructors (network cleanup, database transactions, resource deallocation), this is significant code bloat.

**NJVL approach:**

```nim
proc process() =
  var file = openFile("data.txt")

  cfvar ret = false
  if not file.valid():
    ret = true

  if not ret:
    let data = file.read()
    if data.isEmpty():
      ret = true

    if not ret:
      processData(data)

  file.close()  # SINGLE cleanup point!
```

All paths eventually reach the single exit. One destructor call. Done.

### Multiple Resources

The benefit compounds with multiple resources. Traditional approach with n resources leads to O(n²) code size due to nested cleanup:

**Traditional:**

```nim
var a = acquire1()
if fail1:
  a.cleanup()
  return

var b = acquire2()
if fail2:
  b.cleanup()
  a.cleanup()
  return

var c = acquire3()
if fail3:
  c.cleanup()
  b.cleanup()
  a.cleanup()
  return

# ... work ...
c.cleanup()
b.cleanup()
a.cleanup()
```

**NJVL:**

```nim
var a = acquire1()
cfvar ret = false

if fail1: ret = true

if not ret:
  var b = acquire2()
  if fail2: ret = true

  if not ret:
    var c = acquire3()
    if fail3: ret = true

    if not ret:
      # ... work ...

    c.cleanup()  # Each appears once
  b.cleanup()
a.cleanup()
```

O(n) code size. Each cleanup appears exactly once, in its proper scope.


### Correctness by Construction

With a single exit point per scope, it's impossible to forget cleanup on an exit path. The destructor injection pass simply walks scopes and inserts cleanup at scope exit.

### Loops with Destructors

Structured loops also simplify destructor placement:

**Traditional (requires manual tracking):**

```nim
while condition:
  var temp = allocate()

  if earlyExit:
    temp.free()  # Must manually cleanup before break
    break

  work(temp)
  temp.free()  # Also cleanup at loop end
```

**NJVL:**

```nim
loop:
  stmts:
    var temp = allocate()

    var exitLoop = false
    if earlyExit: exitLoop = true

    if not exitLoop:
      work(temp)

    temp.free()  # Always runs at scope exit
    (continue)
  not exitLoop
  after
```

Single cleanup point runs on every iteration regardless of how the iteration ends. Note how we avoided the `jtrue` instruction here as the destruction would prevent us from performing a jump anyway.


### Exception Handling

NJVL's structured nature mirrors try/finally blocks naturally:

```nim
var x = acquire()
var raised = false
# ... code that might set raised = true ...
if not raised:
  # normal path
x.cleanup()  # Always runs
if raised: reraise
```

The cleanup appears once and always executes, just like a finally block.



# NJVL vs LLVM

| Feature |	NJVL	| LLVM IR |
|---------|---------|---------|
| Structured CF | ✅ Yes | ❌ No (has goto) |
| SSA | ✅ Easy variant | ✅ Full SSA |
| Location versioning | ✅ Innovative! | ❌ No |
| Assumptions | ✅ Built-in | ⚠️ Via metadata |
| Easy exit from SSA | ✅ Drop versions | ⚠️ Complex |
| GPU-friendly | ✅ Very | ⚠️ Needs lowering |
| Destructor injection | ✅ Trivial | ⚠️ Complex |
| Move analysis | ✅ Forward scan | ❌ Needs CFG |


# Design Considerations

## Why Not a Single Program Counter Variable?

An alternative design might use a single integer "program counter" variable instead of multiple boolean cfvars. The intuition is that cfvars encode a state machine, so why not make that explicit with states as integers?

```nim
# Hypothetical pc-based design:
var pc = 0
pc = stateForConditionTrue   # Set state based on condition
if (eq pc stateForConditionTrue):  # Test the state
  ...
```

Each `pc = N` assignment could become `goto label_N`, and `if (eq pc N)` becomes `label_N:`, achieving the same zero-cost lowering as cfvars.

**Why this doesn't work:**

The fundamental problem is that you still need to **evaluate the actual boolean condition** to know which state to transition to:

```nim
# You must compute the condition first
tmp = (ge a 3)

# Then test tmp to set pc
if tmp:        # ← Conditional branch on tmp!
  pc = 5
else:
  pc = 6

# Then test pc again
if (eq pc 5):  # ← Redundant branch!
  body
```

The issue: **You've already performed a conditional branch** when deciding which pc state to enter. Testing pc afterward adds nothing - it's a redundant check of information you already tested.

With cfvars, there is no redundancy:

```nim
cfvar = (ge a 3)  # Compute and store the condition
if cfvar:          # Test it once
  body
```

The cfvar **is** the boolean result you need. It's not an encoding of "which condition was true" - it's the actual truth value. The single-use property ensures each condition is tested exactly once, and structured control flow ensures that a single test can become a goto during code generation.

**Conclusion:**

Cfvars are not "modeling a program counter" - they're **naming boolean values** so we can:
1. Reference them exactly once (single-use → goto optimization)
2. Deduplicate them via CSE when they compute the same expression
3. Track assumptions about them in control flow branches

The pc model looked appealing but solves the wrong problem. Control flow requires testing boolean conditions, and cfvars are the minimal abstraction for that.

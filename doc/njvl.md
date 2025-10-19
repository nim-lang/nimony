# NJVL - "No jumps, versioned locations"

NJVL is an intermediate representation for Nimony.

It is a structured IR, there is no unstructured control flow like `return` or `break` ("no jumps").
Instead control flow variables of type `bool` are used to represent the control flow. All variables, including control flow variables, are versioned! This gives us a variant of SSA that is easy to get rid of later on -- a code generator can just ignore the variables' versions and map every variable to a register or memory slot.

NJVL is good for:

- Computing the validity of Nimony's `requires` and `ensures` contracts.
- Control flow sensitive alias analysis. ("Cursor optimizer".)
- Copy propagation.
- Common subexpression elimination ("CSE").
- Move analysis.
- Induction variables.
- Loop invariant code motion.

NJVL is bad for:

- Continuation passing style ("CPS"). For this `goto` is fundamental.

## Tags

### `v` (version)

The `v` tag is used to represent the version of a variable. For example:

```
(stmts
  (var :x.0 . . . +4)
  (call use.0 x.0)
  (asgn x.0 +9)
  (call use.0 x.0)
)
```

is translated into:

```
(stmts
  (var (v :x.0 +0) . . . +4)
  (call use.0 (v x.0 +0))
  (asgn (v x.0 +1) +9)
  (call use.0 (vx.0 +1))
)
```

In fact, the `v` modifier applies to **locations**, not just variables. The system can reason about independent object field accesses as well: `(v (dot obj field) +0)` is a valid construction. Likewise array indexing: `(v (at arr (v index +0)) +0)` is valid. As you can see the array index is also versioned!

Locations that cannot be reasoned about (due to their address being taken) stay without version information. An algorithm like CSE can only simplify expressions that are versioned.


### `unknown`

The `unknown` tag is used to represent an unknown value. It is crucial to mark mutated variables as `unknown` so that these get a new version number. This happens after it is passed as a parameter to a function by `var`:

```nim

proc maybeMutate(x: var int) = discard

var x = 0
maybeMutate x
```

is translated into:

```
(stmts
  (var (v :x.0 +0) . . . +4)
  (call maybeMutate.0 (haddr (v x.0 +0)))
  (unknown (v x.0 +1))
)
```

### `ite`

`ite` is the `if-then-else` control flow. A `case` statement is also translated into a series of `ite`, however the tag `itec` is used so that eventually a `case` statement can be reconstructed. Every `ite` has exactly 4 children: the condition, the then branch, the else branch, and the join point.

#### `join`

The `join` instruction is used to represent a join point for variable versions:

```nim
var x = 0
if cond:
  x = 1
else:
  x = 2
use x
```

is translated into:

```
(stmts
  (var (v :x.0 +0) . . . +0)
  (ite
    cond
    (stmts
      (asgn (v x.0 +1) 1)
    )
    (stmts
      (asgn (v x.0 +2) 2)
    )
    (stmts
      (join (v x.0 +3) (v x.0 +1) (v x.0 +2))
    )
  )
  (call use.0 (v x.0 +3))
)
```

A `join`'ed variable can be turned into an `(or ...)` construct inside conditions. This will be important later for converting control flow variables back into jumps.


### `loop`

Every `while` loop is transformed into a `loop` construct. The structure of `loop` is:

```
(loop
  <before cond statements>
  <cond>
  <body> # statements that should be executed if the condition is true; afterwards goto the loop start!
  <after> # everything that comes after the loop! The loop is over at this point!
)
```

So a `loop` has always 4 sections.

The `after` section is logical but uncommon for compiler IRs. The idea is to capture most naturally occurring information like:

```nim
  var i = 0
  while i < 10:
    body
  weKnowThat: i >= 10
```

This also means that an entire block like `(stmts ... (while cond actions) afterwards...)` is translated into a single `loop` construct.

This implies that a `loop` is always the last construct in a statement list!

### `either`

Loop headers require special handling for versioned variables because of the back-edge: a variable at the loop header could come from either before the loop or from the previous iteration. The `either` construct declares this explicitly:

```
(either location result-version initial-version back-edge-version)
```

The `either` construct is part of the `(continue)` statement at the loop back-edge. This placement is practical: the IR constructor only knows which variables are loop-carried after processing the entire loop body. When it reaches the back-edge, it can emit the `either` clauses for all variables that flow back to the loop header.

For example:

```nim
var i = 0
var sum = 0
while i < 10:
  sum = sum + i
  i = i + 1
```

is translated into:

```
(var (v i.0 +0) +0)
(var (v sum.0 +0) +0)
(loop
  (stmts)
  (lt (v i.0 +1) +10)

  (stmts
    (asgn (v sum.0 +2) (add (v sum.0 +1) (v i.0 +1)))
    (asgn (v i.0 +2) (add (v i.0 +1) +1))
    (continue
      (either i.0 +1 +0 +2)      # i version +1 is either +0 (initial) or +2 (from back-edge)
      (either sum.0 +1 +0 +2))   # sum version +1 is either +0 (initial) or +2 (from back-edge)
  )
  (after)
)
```

The `either` construct appears only in `(continue)` statements at loop back-edges. It does not appear anywhere else in NJVL.

**NOTE**: `join` introduces a new variable version, `either` merely documents where a new variable came from.


**Execution semantics:**
For execution purposes, `either` can be ignored or treated as equivalent to `unknown` - the runtime value is simply whatever flows through control flow. The `either` construct is primarily for optimization analyses.

**Induction variable analysis:**
The `either` construct makes induction variable detection straightforward. An induction variable has the pattern:

```
(loop
  ...
  (stmts
    (asgn (v i.0 recurrent-version) (add (v i.0 +N) constant))
    (continue
      (either i.0 +N initial-version recurrent-version))
  )
  (after)
)
```

The analysis can detect:
- Initial value: look at `initial-version` in the `either`
- Step: the constant added to produce `recurrent-version`
- The variable is an induction variable if `recurrent-version` is defined as `version +N ± constant`

This enables loop optimizations like:
- Strength reduction (replace multiplication by induction variable with addition)
- Loop unrolling (knowing the iteration count)
- Bounds check elimination (proving array indices are in range)



### `assume`

`assume` can be used to express further knowledge about the program. For example we could use `(assume (and (ge (v x.0 +3) +1) (le (v x.0 +3) +2)))` to express that `x.0 version 3` is always between 1 and 2.

### `kill`

The `kill` instruction marks the end of a variable's lifetime:

```
(kill <variable>)
```

**Semantics:**
- Marks the end of a variable's lifetime
- Required for move optimization (can't move after kill)
- Required for borrow checking (borrows end at kill)
- Can be optimized to destructor calls or eliminated if trivial

**Scope injection:**
Every local variable gets a `kill` instruction at scope exit:

```nim
proc example() =
  var x = allocate()
  var y = x
  use(y)
  # x goes out of scope
```

Becomes:
```
(var (v x.0 +0) (call allocate.0))
(var (v y.0 +0) (v x.0 +0))
(call use.0 (v y.0 +0))
(kill (v x.0 +0))  # x dies here
(kill (v y.0 +0))  # y dies here
```

**Move optimization:**
The `kill` instruction enables precise move analysis:

```
(var (v x.0 +0) (call allocate.0))
(var (v y.0 +0) (v x.0 +0))  # Copy - can we move?
(call use.0 (v y.0 +0))
(kill (v x.0 +0))            # x dies - can move!
```

**Borrow checking:**
The `kill` instruction marks when borrows end:

```
(var (v s.0 +0) ...)
(var (v x.0 +0) (haddr (at (v s.0 +0) +0)))  # Borrow established
(call use.0 (hderef (v x.0 +0)))             # Borrow used
(kill (v x.0 +0))                            # Borrow ends
(call mutate.0 (haddr (v s.0 +0)))          # OK - x is dead
```

**Register allocation:**
The `kill` instruction enables precise lifetime tracking for register allocation. `kill` instructions can be moved upward to end lifetimes as early as possible:

```
(var (v x.0 +0) (call allocate.0))
(call use.0 (v x.0 +0))  # Last use of x
(kill (v x.0 +0))        # Can move this up to after last use
# x is now free for other variables
```

This is more precise than lexical scoping and enables earlier register deallocation, improving register pressure in tight loops.

**Destructor transformation:**
`kill` instructions are later transformed to destructor calls:

```
(kill (v x.0 +0))  # Trivial destructor - can eliminate
(kill (v y.0 +0))  # Complex destructor - becomes (call y.destroy())
```

The `kill` instruction serves as the proper abstraction for lifetime management, separate from the implementation details of destructors.


## Control Flow Variables

In general `raise`, `return` and `break` are turned into a mixture of data flow plus guards (`if cfVar: ...`). In fact even `and` and `or` with their short-circuiting semantics tend to cause trouble and so get the same treatment!

Function calls cannot be used in conditions either, because they are not side-effect free.

For readability reason we use Nim syntax here where `x@v` stands for `(v x +v)`, but the actual IR uses NIF, of course.

For example:

```nim

if cond1 and (cond2 or fn(cond3)):
  body
else:
  otherwise
```

Becomes:

```nim
var t@0 = cond1
if t@0:
  t@1 = cond2
  if not t@1:
    t@2 = fn(cond3)
  t@3 = (join t@1 t@2)
t@4 = (join t@0 t@3)

if t@4:
  body
else:
  otherwise
```


### Return elimination

```nim
proc p(cond: bool) =
  before
  if not cond: return
  after
```

Is translated into:

```nim
proc p(cond: bool) =
  before
  cfvar v = cond
  if v:
    after
```

### Break elimination

```nim
proc p() =
  while true:
    before
    if condA:
      more
      if condB: break
    after
  eventually
```

Is translated into:

```nim
proc p() =
  loop:
    stmts:
      before
      cfvar loopExit = false

      if condA:
        more
        if condB: loopExit = true
    exitif loopExit
    if not loopExit: # redundant but usually the result of the transformation
      after
    eventually
```

### Exception handling

`raise` is translated like a `return`, function calls that can raise are transformed into `let tmp = call(); if failed(tmp): raise tmp`. This can produce tedious long-winded code but the benefit is that all the anticipated optimizations run in one or two passes over the resulting trees, no fixpoint computations are required.


### Translating cfvar back to unstructured control flow

While cfvars appear to introduce data flow overhead, proper construction and code generation can eliminate this entirely.

**Single-use invariant:** Each cfvar should be tested exactly once. This enables direct translation back to unstructured control flow:
- `(asgn cfvar true)` → `goto label`
- `(ite cfvar ...)` → `label:`

**Condition forms:** Loop and if conditions must be:
- `cfvar` (single variable), or
- `(or cfvar1 cfvar2 ...)` (disjunction of cfvars)

This restriction ensures that multiple early-exit conditions can be merged efficiently while preserving the single-use property.

**Example round-trip:**

Input:
```nim
while cond:
  if earlyExit: break
  work()
```

NJVL (optimization passes work here):
```nim
loop:
  stmts:
    cfvar exitLoop = earlyExit
  exitif exitLoop
  work()
```

Output (code generation):
```nim
loop_start:
  if earlyExit: goto loop_end
  work()
  goto loop_start
loop_end:
```

The structured form exists purely to enable optimization. Code generation recovers the original unstructured form with no runtime overhead.

#### Problem: Interim Statements Break Jump Conversion

**Converting `cfvar=true` to a jump is only valid if no interim statements occur.**

To avoid this problem "path specialization" can be used: `(if a b d)(d)` can always be converted to `(if a (stmts b d) (stmts c d))` and then in the duplicated `d` we know whether `a` is true or not and can specialize the code.


## Move analysis

Move analysis optimizes `copy dest, src` into `move dest, src` when `src` is not used afterward. NJVL makes this optimization significantly simpler than traditional CFG-based approaches.

### Versioning Enables Precise Tracking

In NJVL, the question "is this the last use of x?" becomes "is this the last use of version `(v x.0 +N)`?" This is much more precise because each version has a single definition point (SSA-like).

Example:

```
(var (v dest.0 +0) ...)
(asgn (v dest.0 +1) (v src.0 +2))  # Copy from src version +2
(call foo (v src.0 +2))            # Another use! Can't move
```

vs.

```
(asgn (v dest.0 +1) (v src.0 +2))  # Copy from src version +2
(asgn (v src.0 +3) ...)            # New version! +2 is dead → CAN MOVE
```

### Forward-Only Scanning

Because NJVL is structured, move analysis can use simple forward traversal without constructing a CFG:

1. Start at the copy/assignment
2. Scan forward through the structured control flow
3. Stop when finding: another use, a new version, or scope exit

For branches, both paths must be checked:

```
(var (v x.0 +0) +42)
(ite cond
  (call foo (v x.0 +0))  # Use in then-branch
  (call bar (v x.0 +0))  # Use in else-branch
)
(unknown (v x.0 +1))  # Join creates new version
# Version +0 is dead here - both branches used it, join created new version
```

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
3. **Join Points Are Explicit**: `(unknown)` nodes mark where versions die and new ones are born. CFG-based analysis must infer this from merge points.
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

    cfvar exitLoop = false
    if earlyExit: exitLoop = true

    if not exitLoop:
      work(temp)

    temp.free()  # Always runs at scope exit
    (continue)
  exitif exitLoop
  after
```

Single cleanup point runs on every iteration regardless of how the iteration ends.

### Exception Handling

NJVL's structured nature mirrors try/finally blocks naturally:

```nim
var x = acquire()
cfvar raised = false
# ... code that might set raised = true ...
if not raised:
  # normal path
x.cleanup()  # Always runs
if raised: reraise
```

The cleanup appears once and always executes, just like a finally block.


### Implementation Simplicity

The destructor injection pass becomes trivial:

```
proc injectDestructors(scope: Cursor):
  for each variable `v` in scope:
    insert `(destroy v)` at scope exit

  recursively process nested scopes
```

No need to track all possible exit paths. The structure ensures there's only one exit per scope.


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


## Example: Complete Translation

To illustrate NJVL in practice, here's a realistic function with exception handling, loops, and early returns:

**Source code:**

```nim
proc processItems(items: seq[string]): int =
  var count = 0
  for item in items:
    if item.len == 0:
      raise newException(ValueError, "Empty item")
    let processed = transform(item)
    if processed.isNil:
      return -1
    count += 1
  return count
```

**Translated to NJVL:**

```
(proc processItems.0
  (params
    (param items.0 seq[string]))
  (result (v result.0 +0) int)

  (stmts
    # Initialize variables
    (var (v count.0 +0) +0)
    (var (v i.0 +0) +0)
    (var (v len.0 +0) (call len.0 (v items.0 +0)))

    # Exception flag (raised becomes cfvar retflag for return)
    (var (v :raisedEx.0 +0) nil)
    (var (v cfvar.retflag +0) false)

    # Loop over items
    (loop
      (stmts)

      # Condition: i < len
      (lt (v i.0 +1) (v len.0 +0))

      # Loop body
      (stmts
        # Get current item
        (var (v item.0 +0) (at (v items.0 +0) (v i.0 +1)))

        # Check if item is empty
        (var (v itemLen.0 +0) (call len.0 (v item.0 +0)))
        (asgn cfvar.0 (eq (v itemLen.0 +0) +0))

        (ite cfvar.0
          # Then: raise exception
          (stmts
            (asgn (v :raisedEx.0 +1)
              (call newException.0 ValueError.0 "Empty item"))
            (asgn cfvar.retflag +1) true)
          # Else: continue
          (stmts))

        # Guard: if not returning/raising, continue processing
        (ite cfvar.retflag
          (stmts)  # Early exit path, do nothing
          (stmts
            # Transform the item
            (var (v processed.0 +0) (call transform.0 (v item.0 +0)))

            # Check if processed is nil
            (asgn cfvar.1 (call isNil.0 (v processed.0 +0)))

            (ite cfvar.1
              # Then: early return with -1
              (stmts
                (asgn (v result.0 +1) -1)
                (asgn cfvar.retflag +2) true)
              # Else: increment count
              (stmts
                (asgn (v count.0 +2) (add (v count.0 +1) +1))))

            (unknown (v count.0 +3))  # Join point from if-else
          ))

        # Increment loop counter
        (asgn (v i.0 +2) (add (v i.0 +1) +1))

        # Continue or exit loop
        (ite cfvar.retflag
          (stmts)  # Will break out of loop
          (stmts
            (continue
              (either i.0 +1 +0 +2)
              (either count.0 +1 +0 +3))))
      )

      # After loop
      (stmts
        # Set result to count if we didn't return early
        (ite cfvar.retflag
          (stmts)  # Result already set
          (stmts
            (asgn (v result.0 +2) (v count.0 +1))))

        (unknown (v result.0 +3))

        # Re-raise if exception occurred
        (ite (call isNotNil.0 (v :raisedEx.0 +1))
          (stmts (raise (v :raisedEx.0 +1)))
          (stmts))

        # Return result
        (ret (v result.0 +3))
      )
    )
  )
)
```

**Comparison: Traditional SSA IR with Labels and Jumps:**

For comparison, here's the same function in a traditional unstructured SSA IR with `lab`, `jmp`, and `phi` nodes:

```
(proc processItems.0
  (params
    (param items.0 seq[string]))
  (result result.0 int)

  (stmts
    # Initialize variables
    (var count.0 +0)
    (var i.0 +0)
    (var len.0 (call len.0 items.0))
    (var raisedEx.0 nil)

    (lab loop_start)
    # Phi nodes for loop header
    (phi count.1 count.0 count.2)  # count.1 = phi(count.0 from entry, count.2 from backedge)
    (phi i.1 i.0 i.2)              # i.1 = phi(i.0 from entry, i.2 from backedge)

    # Loop condition
    (jmpif (ge i.1 len.0) loop_end)

    # Get current item
    (var item.0 (at items.0 i.1))

    # Check if empty
    (var itemLen.0 (call len.0 item.0))
    (jmpif (neq itemLen.0 +0) not_empty)
    (asgn raisedEx.1 (call newException.0 ValueError.0 "Empty item"))
    (jmp exception_exit)

    (lab not_empty)
    # Transform item
    (var processed.0 (call transform.0 item.0))
    (jmpif (call isNotNil.0 processed.0) not_nil)
    (var result.1 -1)
    (ret result.1)

    (lab not_nil)
    # Increment count
    (var count.2 (add count.1 +1))

    # Increment loop counter
    (var i.2 (add i.1 +1))
    (jmp loop_start)

    (lab loop_end)
    # Return count
    (var result.2 count.1)
    (ret result.2)

    (lab exception_exit)
    (phi raisedEx.2 raisedEx.1)
    (raise raisedEx.2)
  )
)
```

**Code size comparison:**
- **Source**: 10 lines
- **Traditional SSA IR with goto**: 150 tokens
- **NJVL**: 293 tokens

**Observations:**

1. **Code size**: NJVL is **2x larger** than traditional SSA IR (293 vs 150 tokens). Both have versioning overhead, so the difference is purely from structured control flow.

2. **Both use versioning**: Both IRs version variables (`count.0`, `count.1`, `count.2`). The versioning cost is similar.

3. **Different merge mechanisms**:
   - **Traditional SSA**: Uses `phi` nodes at labels to merge versions from different predecessors
   - **NJVL**: Uses `either` at `continue` (explicit merge) or `unknown` at joins (abstract merge)

   Both are explicit about where versions merge, but phi nodes require knowing predecessor labels while NJVL merges are self-contained

4. **Control flow representation**:
   - **Traditional SSA**: Direct jumps (`jmp`, `jmpif`) to labeled blocks - explicit CFG
   - **NJVL**: Control flow variables + structured nesting - implicit CFG from structure

5. **Early exits**:
   - **Traditional SSA**: Direct `ret` or `jmp exception_exit` - requires duplicate cleanup at each exit
   - **NJVL**: Set `cfvar.retflag`, guard subsequent code, single cleanup point - verbose but reduces duplication

6. **Loop back-edges**:
   - **Traditional SSA**: `(jmp loop_start)` + `(phi ...)` at label - CFG edges + merge
   - **NJVL**: `(continue (either ...))` - merge encoded in back-edge

**What does NJVL buy us for 1.6x code size?**

Both IRs have SSA, so they're similar in optimization power. The key differences:

- **Move analysis**:
  - **NJVL**: Forward tree scan, structure guides traversal
  - **Traditional SSA**: Must build CFG first, then scan

- **CSE**: Both can deduplicate based on versions. Roughly equal.

- **Contract verification**:
  - **NJVL**: `(assume cfvar)` nodes automatically injected in branches
  - **Traditional SSA**: Must analyze CFG dominance to infer which conditions hold

- **Destructor injection**:
  - **NJVL**: One destructor per scope at structured exit point
  - **Traditional SSA**: Must insert destructors before every `ret`/`raise`/`jmp` that exits scope

- **Induction variables**:
  - **NJVL**: `(either i.0 +1 +0 +2)` explicitly shows loop-carried deps
  - **Traditional SSA**: `(phi i.1 i.0 i.2)` also explicit, equivalent information

**The trade-off:**

NJVL is **2x larger** than traditional SSA IR (measured in tokens). The expansion buys:

**The key benefit: No fixpoint computations needed.**

Traditional SSA with CFG requires iterative dataflow analysis for many optimizations:
- Dataflow facts propagate along CFG edges
- Must iterate until reaching fixpoint (no more changes)
- Each iteration re-analyzes the entire function
- Convergence speed depends on CFG structure

NJVL enables **single-pass or two-pass algorithms**:
- Structured tree traversal sees all paths in order
- Versioning makes dependencies explicit (no need to discover them)
- Forward scan collects all needed information
- No iteration needed - one traversal is sufficient

Example: **Redundant condition elimination**
- **Traditional SSA**: Iterate dataflow until all redundancies discovered
- **NJVL**: Single CSE pass - versions immediately show when expressions are equivalent

Example: **Move analysis**
- **Traditional SSA**: Build CFG, compute live ranges, iterate until stable
- **NJVL**: Forward scan from copy until version dies - done

Other benefits:
- **No CFG construction needed**: The tree structure IS the control flow
- **Simpler destructor placement**: One exit point per scope
- **Built-in assumptions**: Control flow automatically injects knowledge about conditions

The 2x code size buys **algorithmic simplicity** - optimization passes are straightforward tree walks instead of complex fixpoint computations over CFGs.


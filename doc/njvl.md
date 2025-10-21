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

`ite` is the `if-then-else` control flow. A `case` statement is also translated into a series of `ite`, however the tag `itec` is used so that eventually a `case` statement can be reconstructed. Every `ite` consists of: the condition, the then branch, the else branch, and the join point. Optionally an `ite` statement can have a label. The label can be referred to by a control flow variable via an assignment, `(asgn cfvar <label>)` where for analysis purposes the `<label>` stands for the value `(true)` but the code generator can exploit it and turn the assignment into a `goto` statement. This mechanism ensures that the overhead can of control flow variables can be eliminated cheaply and reliably.


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
(kill x.0)  # x (all versions of x) dies here
(kill y.0)  # y (all versions of y) dies here
```

**Move optimization:**
The `kill` instruction enables precise move analysis:

```
(var (v x.0 +0) (call allocate.0))
(var (v y.0 +0) (v x.0 +0))  # Copy - can we move?
(call use.0 (v y.0 +0))
(kill x.0)            # x dies - can move!
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

Moving these `kill` instructions imply that they are not used anymore to denote a scope exit. As such they should then refer to versioned variables only.


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

While the above transformation is correct, in practice code like the following will be produced:

```nim
var t@0 = false
if cond1:
  if cond2:
    jtrue t@1 "label"
  else:
    let tmp = fn(cond3)
    if tmp: jtrue t@2 "label"
  t@3 = (join t@1 t@2)
t@4 = (join t@0 t@3)

if t@4 "label":
  body
else:
  otherwise
```

This ensures that during code generation the control flow variables collapse into control flow and cause no overhead!

We have to watch out though: **Converting `jtrue x "label"` or `jfalse x "label"` to a jump is only valid if no interim statements occur.**


### `jtrue` and `jfalse` instructions

```nim
jtrue x "label"
jfalse x "label"
```

The semantics of `jtrue x "label"` is that `x` is set to `true` and the control flow is allowed to jump to `"label"` directly but this property can safely be ignored as the following code is properly guarded by a condition like `if not x: ...`.


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
  var ret = false
  if not cond:
    jtrue ret <label A>
  if not ret:
    after
  else <label A>:
    discard
```

### Break elimination

`break` is handled similarly to `return`.

### Exception handling

`raise` is translated like a `return`, function calls that can raise are transformed into `let tmp = call(); if failed(tmp): raise tmp`. This can produce tedious long-winded code but the benefit is that all the anticipated optimizations run in one or two passes over the resulting trees, no fixpoint computations are required.

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
    (stmts: (asgn (v ebx.0 +1) (v eax.0 +2)))
    (stmts:))
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

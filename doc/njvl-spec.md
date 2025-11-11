# NJVL - "No jumps, versioned locations"

NJVL is an intermediate representation for Nimony.

**Note**: This document covers the new constructs and concepts NJVL introduces. For more examples how these can be put into use see the [njvl-details.md](njvl-details.md) file.

NJVL is a structured IR, there is no unstructured control flow like `return` or `break` ("no jumps").
Instead control flow variables (`cfvar`) of type `bool` are used to represent the control flow. All variables including cfvars are versioned! This gives us a variant of SSA that is easy to get rid of later on -- a code generator can just ignore the variables' versions and map every variable to a register or memory slot.

NJVL is good for:

- Computing the validity of Nimony's `requires` and `ensures` contracts.
- Control flow sensitive alias analysis. ("Cursor optimizer".)
- Copy propagation.
- Common subexpression elimination ("CSE").
- Move analysis.
- Induction variables.
- Loop invariant code motion.
- Scalar replacement of aggregates.
- Vectorization, apparently.


NJVL is subtle, but works for:

- Continuation passing style ("CPS"). The required labels are implied only but remain recoverable.


**NJVL is a two-phase transformation, it does not work well as a single pass.**

The first phase is the `nj` pass. It translates the `return` and `break` statements into control flow variables and guards. This restores the inherent tree-like structure of the control flow and means we always have clean `join` points for the second phase which is the `vl` pass.

The `vl` pass adds version information to all locations. A location is a generalization of a variable. It can be a variable, a field access, an array index, etc.

## NJ - "No jumps"

NJ simplifies control flow, VL simplifies data flow.

### Control Flow Variables

In general `raise`, `return` and `break` are turned into a mixture of data flow plus guards (`if cfVar: ...`). In fact even `and` and `or` with their short-circuiting semantics tend to cause trouble and so get the same treatment!

Function calls cannot be used in conditions either, because they are not side-effect free.

The NIF tag `cfvar` is used to declare a new control flow variable. It is always of type `bool` and initialized to `false`.
A `cfvar` can only be set to `true` by a `jtrue` instruction and tested inside a condition of an `ite` or `loop` statement!

**A cfvar has a monotonic behavior - once set to true, cfvars stay true!**

Right before code generation the `cfvar`s can be replaced by jumps without much analysis effort. For example we know that they do not have to be materialized as they cannot be passed to functions etc.

For readability reason we use Nim syntax here, but the actual IR uses NIF, of course.

For example:

```nim
if cond2 or fn(cond3):
  body
else:
  otherwise
```

Becomes:

```nim
cfvar tmp = false
if cond2:
  jtrue tmp
else:
  if fn(cond3):
    jtrue tmp
if tmp:
  body
else:
  otherwise
```

This ensures that during code generation the control flow variables collapse into control flow and cause no overhead!

We have to watch out though: **Converting `jtrue x` to a jump is only valid if no interim statements occur.**


### `jtrue` instruction

```nim
jtrue x (y, z, ...)
```

The semantics of `jtrue x, y, z` is that `x`, `y`, `z` are set to `true`. It is a multi assignment instruction making it easier to analyze and convert to jumps.


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
  cfvar ret = false
  if not cond:
    jtrue ret
  if not ret:
    after
```

### Break elimination

`break` is handled similarly to `return`.

### Exception handling

`raise` is translated like a `return`, function calls that can raise are transformed into `let tmp = call(); if failed(tmp): raise tmp`. This can produce tedious long-winded code but the benefit is that all the anticipated optimizations run in one or two passes over the resulting trees, no fixpoint computations are required.


## VL - "Versioned Locations"

The VL pass adds version information to all locations. The current implementation uses **SSA (Static Single Assignment)** where versions track writes. Since versioning is separate from control flow analysis, we can also attach an **SSU (Static Single Use)** mechanism instead or alongside SSA. See [njvl-ssu-ideas.md](njvl-ssu-ideas.md) for details on how SSU could be implemented.

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

`ite` is the `if-then-else` control flow. A `case` statement is also translated into a series of `ite`, however the tag `itec` is used so that eventually a `case` statement can be reconstructed. Every `ite` consists of: the condition, the then branch, the else branch, and the join point.


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
      (join x.0 +3 +1 +2)
    )
  )
  (call use.0 (v x.0 +3))
)
```



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

**NOTE**: The current implementation generates `join` instead of `either` for loop back-edges. It is still unclear what is better.


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

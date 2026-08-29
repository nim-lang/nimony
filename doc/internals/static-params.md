# Static parameters (value generics) for Nimony

Status: design. This document records the design for compile-time *value*
parameters — the replacement for Nim 2's `static[T]` type modifier.

## Goal

Allow constant values to appear in a generic argument list,
e.g. `Matrix[3, 4, int]` or `Vector[N, T]`, and have the value threaded to
wherever it is used (an array length, a field default, arithmetic on lengths)
or dropped if unused.

We explicitly want to avoid the two Nim 2 problems:

1. A `static[T]` **type modifier** that the whole compiler has to be aware of
   and constantly *skip*. Every `sameType`, every sigmatch path, every hexer
   lowering learns to unwrap it. That pervasiveness is the disease.
2. The overload-resolution asymmetry where an integer literal `3` has type
   `int` yet matches a `static int` parameter *better* than it matches `int`.
   That surprise comes entirely from solving a value parameter out of the
   *value* of a runtime argument.

## Core decisions

### 1. A value parameter is an ordinary value; its type is the plain type

The type of a value parameter `N` is `int`, full stop. There is no `static[int]`
*type* in the checked type system. `static[int]` is **declaration syntax** only:

```nim
type Matrix[M, N: static[int]; T] = object
  data: array[M * N, T]
```

`static[int]` desugars to a value parameter whose type slot holds plain `int`.
The element type must be spelled — bare `N: static` is rejected — because we now
*compute* with `N` (see arithmetic below) and declaration-time checking of
`N + M` needs to know both operands are `int` before any instantiation exists.
A pure value parameter has no other inference source for its element type.

Because the type slot is always `int`, `sameType`, type-directed sigmatch, and
hexer never see anything but `int`. The "static-ness" is not in the type, so
there is no wrapper to skip anywhere in the type algebra. That property is the
whole point; everything below is arranged to preserve it.

### 2. Static-ness lives in the NIF **tag**, not in a flag or a pragma

An out-of-band boolean flag has no NIF representation, so every pass would have
to reconstruct it — exactly the hacky pervasiveness we are avoiding. A `.static`
pragma would push a *structural* distinction into the soft attribute bag, where
dispatch is a pragma scan that any pass can silently forget.

Instead, introduce sibling tags:

| ordinary        | static variant        | meaning                                   |
|-----------------|-----------------------|-------------------------------------------|
| `typevar`       | `staticTypevar`       | a generic parameter that is a value       |
| `param`         | `staticParam`         | a term parameter whose argument must be constant |

This is the continuation of an existing convention, not a new idea: Nimony
already encodes binding *kind* as a distinct tag rather than as a modifier —
`gvar`/`tvar`/`var`, `glet`/`tlet`/`let`, `cursor`, `gfld`/`fld`. Global-ness,
thread-locality and cursor-ness are tags, not `(var … (pragmas global))`. Value
parameters follow the same rule.

Both tags keep `int` in the type slot:

```
(staticTypevar N . . int .)     # value generic parameter, type int
(staticParam x  . . int .)      # term parameter, argument must be constant
```

Consequences:

* Dispatch is exhaustive. sigmatch and the instantiator already `case` on the
  declaration tag; the static variant is a new arm the compiler forces you to
  write. Omission is a missing-case error, not a silent bug.
* The passes that do *not* care about static-ness stay one-liners via helpers
  `isTypevarLike(tag)` / `isParamLike(tag)` that accept both variants. Only
  sigmatch, const-eval and the instantiator branch on the static form.

### 3. Binding is by unification only — never by literal promotion

A `staticTypevar` is bound either explicitly in the bracket (`Matrix[3, 4, int]`)
or by unifying a formal type that embeds it against an argument's type
(`array[N, T]` vs `array[3, int]` binds `N = 3`). This is the *same* machinery
that binds `T`. There is never a contest between `f(x: int)` and
`f(x: static int)`, because a value is not solved out of a runtime argument's
value. That deletes the Nim 2 overload asymmetry at the root.

`staticParam` (case below) is bound from a term argument that is *required to be
constant*, but its type for matching purposes is still `int`, so it does not win
a specificity contest either (see the tiebreaker rule).

### 4. Two kinds of `static`, sharing one representation

`static` is restricted to exactly two roles, which have different ergonomics but
the same underlying shape (a value of type `T`, flagged by its tag):

* **a) Value generic parameter** (`staticTypevar`) — declares a generic value,
  solved by bracket or type unification. Checked symbolically at declaration
  time.
* **b) Constant parameter restriction** (`staticParam`) — the call-site argument
  must be a constant expression.

Whether a generic body is checked at declaration time or instantiation time is
**orthogonal** to `static`: that is governed by how `when` behaves in generics,
with `.untyped` selecting the Nim 2 "check at instantiation" behavior. It applies
to type parameters just as much and is not a static question.

### 5. Overload tiebreaker is ambiguous-by-default

Two candidates that differ *only* in whether a slot is static are **ambiguous**
(an error). This is what removes the "`3` matches `static int` better than
`int`" surprise. A `.feature: "staticOverloads"` restores the Nim 2 preference for the
more-constant candidate, and when enabled it is applied **last**, after all
genuine specificity ordering, so it only breaks otherwise-exact ties.

### 6. Type-level arithmetic: forward-evaluate, never invert

Value parameters are ordinary `int`s, so simple arithmetic on them is allowed in
type positions: `array[M * N, T]`, `Matrix[M, N1 + N2, T]`. The NIF
representation already exists and the rule is:

> **Bind only from bare positions; arithmetic positions are check-only.**
> When all operands of an arithmetic expression are bound, evaluate it (a tiny
> fixed-opcode evaluator in sigmatch, `+ - * ` and comparisons — nothing more).
> When any operand is still symbolic, compare the expression **syntactically**.

So `array[N + M, T]` unifies with `array[N + M, T]` but **not** with
`array[M + N, T]`: there is no commutativity, associativity, or normalization.
Concrete operands fold — `Matrix[2 + 3, T]` matches `Matrix[5, T]`.

The reason inversion is forbidden is decidability, not implementation effort.
Binding `N` out of a formal `Foo[N + 1]` matched against `Foo[5]` means *solving*
`N + 1 = 5`; `N * M = 12` is factoring; `N + M = 5` has infinitely many
solutions. Solving turns matching into integer constraint solving (Diophantine),
which is undecidable. Forward evaluation keeps matching a terminating structural
walk. sigmatch never links against the real CTFE engine for this — it only runs
the tier-1 opcodes.

### 7. Two tiers of compile-time evaluation

1. **Trivial folding** — literals, `const`/`staticParam`/`staticTypevar`
   references, and the fixed primitive opcode set. Automatic and decidable.
   Shared by three consumers: length arithmetic in types, `staticParam` argument
   acceptance, and concept checks.
2. **General CTFE** — running arbitrary `func`s at compile time. **Never
   implicit.** The Nim 2 behavior where passing an argument to a `static`
   parameter silently triggered a compile-time evaluation that would not
   otherwise happen is *not* repeated. General CTFE is opt-in via an explicit
   `static(expr)` / `static f(x)` call-site annotation, and produces a value that
   then feeds a tier-1 slot.

A `staticParam` therefore accepts anything in tier 1 without surprise; anything
requiring tier 2 must be written `static g(x)` at the call site.

## Concepts

Concepts may carry value parameters in their head, and the arithmetic rules
above apply unchanged. The rules that keep concept satisfaction **decidable**:

* **Bind bare, check with arithmetic.** A value parameter the concept intends to
  bind must appear in a bare position at least once. Occurrences inside `N + 1`,
  `M * N`, etc. are *checks*, evaluated after the bare position binds the
  operands. A concept whose only occurrence of a parameter is inside an
  arithmetic expression is ill-formed: it declared a parameter it can never bind.
* **A repeated parameter is an equality check.** The same symbol in two positions
  unifies structurally — no arithmetic needed.
* **Concrete folds, symbolic compares syntactically.** As in §6. Concept authors
  must know there is no algebraic normalization and write the canonical form.
* **Value predicates are not concepts.** A restriction like `N > 0` or
  `M + N == 10` is a `where`-clause / predicate, not concept membership. With
  symbolic operands it cannot be discharged at declaration time and **defers to
  instantiation**. That seam is acceptable: it gates *admissibility of the
  instantiation*, not the *typing of the body*, so it does not reintroduce
  instantiation-time type checking — only instantiation-time value validation,
  which is unavoidable.

## Worked example: matrix multiplication

The concrete matrix type:

```nim
type Matrix[M, N: static[int]; T] = object
  data: array[M * N, T]        # M*N is a construct position: symbolic at
                               # declaration time, folded at instantiation
```

### What works

**Dimension-checked multiply.** This is the "typical" example, and it works with
no inversion at all:

```nim
proc `*`[M, K, N: static[int]; T](a: Matrix[M, K, T];
                                  b: Matrix[K, N, T]): Matrix[M, N, T] = ...
```

* `a: Matrix[M, K, T]` binds `M`, `K`, `T` from bare positions.
* `b: Matrix[K, N, T]` — `K` is already bound, so its position is a **check**:
  the candidate's first dimension must structurally equal the bound `K`. `N`
  binds from a bare position.
* The inner-dimension agreement (`A.cols == B.rows`) is just the shared symbol
  `K` — an equality check, decidable, no arithmetic.
* The result `Matrix[M, N, T]` is *constructed* from already-bound parameters.

`Matrix[3, 4, int] * Matrix[4, 2, int]` binds `M=3, K=4, N=2` and yields
`Matrix[3, 2, int]`. `Matrix[3, 4, int] * Matrix[5, 2, int]` fails to match
because the bound `K=4` does not equal `5`. All of this is inside the design's
"forward-evaluate, bind-bare-only" envelope.

**A "square matrix" concept** — a repeated parameter as an equality check:

```nim
type Square[N: static[int]; T] = concept x
  x is Matrix[N, N, T]
```

Matching `Matrix[3, 3, int]`: the first bare position binds `N = 3`, the second
is a check `3 == 3` ✓. Matching `Matrix[3, 4, int]`: `N` binds `3`, the check
`3 == 4` fails, correctly rejecting the non-square matrix.

**Arithmetic in construct positions** — Kronecker product and column concat both
work, because every parameter is bound from a bare argument position and the
arithmetic only appears in the *result*:

```nim
proc kron[M, N, P, Q: static[int]; T](a: Matrix[M, N, T];
                                      b: Matrix[P, Q, T]): Matrix[M * P, N * Q, T]

proc concatCols[M, N1, N2: static[int]; T](a: Matrix[M, N1, T];
                                           b: Matrix[M, N2, T]): Matrix[M, N1 + N2, T]
```

### Where it stops (cannot work yet, by design)

**Recovering dimensions by factoring.** A concept that stores a matrix flat and
tries to *derive* both dimensions from the total length requires inverting `*`:

```nim
type FlatMatrix[M, N: static[int]; T] = concept x
  x.data is array[M * N, T]     # ill-formed for binding M and N
```

To bind `M` and `N` here, matching would have to solve `M * N == len(x.data)` —
factoring, with many or no solutions. `M` and `N` never appear in a bare
position, so this concept declares parameters it cannot bind. It is rejected.
A `FlatMatrix` must expose its dimensions in bare positions (e.g. carry `M` and
`N` as its own value parameters) so they can be bound directly and `M * N` used
only as a *check*.

**No algebraic normalization across expressions.** `concatCols` returns
`Matrix[M, N1 + N2, T]`. If a caller feeds that result into a context expecting
`Matrix[M, N2 + N1, T]`, the match **fails** — `N1 + N2` and `N2 + N1` are
compared syntactically and are not equal. There is no commutativity. The
work-around is to pick and consistently use a canonical order; the compiler will
not prove the identity. Type-level arithmetic that would require such proofs
(associativity chains, distributing a product, cancelling terms) is out of scope
and would reintroduce the undecidability we are avoiding.

## Implementation notes

* New tags `staticTypevar` and `staticParam`, both with `int` (or the spelled
  element type) in the type slot. Add them to the tag table alongside `typevar`
  and `param`.
* Helpers `isTypevarLike` / `isParamLike` for the majority of walkers that treat
  the static and non-static forms identically.
* sigmatch gains a tiny `LengExpr` evaluator (`+ - *`, comparisons) and a
  match/bind decision that distinguishes **bare position** (may bind) from
  **arithmetic/check position** (evaluate-if-bound, else compare syntactically;
  never solve).
* The instantiator substitutes a value node for a `staticTypevar` exactly as it
  substitutes a type node for a `typevar`; threading into an `array` length is a
  plain node substitution, and an unused value parameter is simply dropped after
  it contributes to the instantiation's identity.
* Overload resolution: static-only differences are ambiguous unless the
  backwards-compat `.feature` is enabled, in which case the static preference is
  the last tiebreaker.

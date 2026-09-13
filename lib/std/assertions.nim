import std/syncio

const assertionsEnabled* = not defined(danger) and not defined(noAssertions)
  ## Whether `assert` expands to anything at all. Follows Nim's convention:
  ## `-d:release` KEEPS assertions, `-d:danger` removes them, and
  ## `-d:noAssertions` removes them on its own — the last one so a build can drop
  ## the checks without also turning off bound checks and overflow checks, which
  ## is what you want when profiling: an `assert` costs a compare, a branch and a
  ## string literal at every call site, and those are not what is being measured.

proc raiseAssert*(msg: string) {.noreturn.} =
  echo "[Assertion Failure] ", msg
  quit 1

template assert*(cond: bool; msg = "") =
  when assertionsEnabled:
    # The failure path is a single `noreturn` CALL, never inlined code. Expanding
    # `echo`+`quit` at every site added ~30 instructions of cold code to procs
    # whose hot body is two — which bloats the image, wrecks I-cache density and
    # (worse) pushes tiny accessors like `nifcore.kind` over the inliner's size
    # cap, so they stay real calls forever.
    if not cond:
      raiseAssert(msg)
  else:
    # Not `discard`: switching the CHECK off must not withdraw the STATEMENT.
    # An `assert` is how the programmer discharges an obligation the prover
    # cannot — an index bound, a precondition — so compiling it out as nothing
    # would take a module that compiles and make it stop compiling under
    # `staticContracts`. `assume` keeps the proposition and drops only the test,
    # which is what "assertions off" should mean and is exactly as (un)safe as
    # `-d:danger` has always been: the programmer's word, taken. The trailing
    # `discard` is what keeps the branch a statement — a lone pragma is typed
    # `auto`, which is its own small sem wart.
    {.assume: cond.}
    discard

template assertRc*[T](r: ref T; expected: int; tag: string = "") =
  ## Diagnostic for ref-count tracking. `r` is a `ref T`, internally a
  ## pointer to `{rc: int, d: T}` (see `arc.nim`); reads the rc field at
  ## offset 0 and aborts when it differs from `expected`. `arcDec` returns
  ## true when rc goes negative, so a fresh ref has rc=0 (= 1 logical
  ## reference); each `=dup` adds +1, each `=destroy` adds -1. Use
  ## `assertRc(myRef, 0)` after construction or `assertRc(myRef, n)` after
  ## n duplications to catch over- or under-counting before the symptom
  ## (UAF, double-free, leak) drifts far from the cause.
  let actualRc = cast[ptr int](r)[]
  if actualRc != expected:
    echo "[assertRc] ", tag, " rc=", actualRc, " expected=", expected,
         " ptr=", cast[int](r)
    quit 1

template assertRcAlive*[T](r: ref T; tag: string = "") =
  ## Looser assertRc: only fail when rc has already gone negative (i.e. the
  ## block has been freed and we're walking dangling memory). Useful when
  ## the exact rc varies with =dup/=destroy interleaving but you still want
  ## to catch use-after-free quickly. Also rejects bogus pointers (large
  ## absolute rc) since arcDec'd-into-positive-billions still indicates the
  ## block was reused for an unrelated allocation.
  if r != nil:
    let actualRc = cast[ptr int](r)[]
    # Plausible rc values are in [-1, ~10000]. Anything else is corruption.
    if actualRc < -1 or actualRc > 10_000:
      echo "[assertRcAlive] ", tag, " rc=", actualRc, " (corrupt)",
           " ptr=", cast[int](r)
      quit 1

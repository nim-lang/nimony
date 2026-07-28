#
#           The intrinsic table
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## The single source of truth for `{.instruction: X.}` / `{.intrinsic: X.}`.
##
## An intrinsic is declared as an ordinary proc prototype, so overload
## resolution, `sigmatch` and `getType` need no special cases. What a proc
## signature *cannot* express — operand roles, two-address ties, target
## availability, purity — lives here, in one row per opcode, and is checked
## **once, at the declaration**: after `sempragmas` has unified the row's shape
## against the declared signature, the symbol is an ordinary typed proc and
## nothing downstream ever consults the shape again.
##
## Two repositories read this file:
##
## * nimony's `sempragmas` resolves the pragma's ident to an `IntrinsicOp`,
##   unifies the signature against the row and rejects a mismatch at the
##   declaration site.
## * arkham (`nativenif`) reads `targets` to answer "does this target have that
##   instruction?" and `tie` / `effects` to drive allocation. The per-target
##   *encoding* is not here — that is `doc/instructions.md` plus the backend's
##   `emitInstr`, and nifasm (a typed assembler) is the backstop that catches a
##   row which is wrong or too loose.
##
## The file is deliberately dependency-free (plain data, no NIF API), so both
## the nifstreams-based front end and the nifcore-based back end can use it
## without dragging in the other's cursor type.

type
  IntrinsicOp* = enum
    NoIntrinsicOp
    # ── portable (`{.intrinsic: X.}`): a lowering exists on every target,
    #    possibly as several instructions.
    CtzOp
    ClzOp
    PopcountOp
    BswapOp
    # ── target-pinned (`{.instruction: X.}`): exactly one machine instruction.
    #    The name is the nifasm instruction tag.
    BsfOp
    BsrOp
    PopcntOp
    BswapPinnedOp
    RolOp
    RorOp
    ClzPinnedOp
    RbitOp
    RevOp

  IntrinsicClass* = enum
    ## What kind of NAME the opcode is — fixed when the row is authored, and NOT
    ## derivable from `targets`, which is availability and grows as the backends
    ## do. A pinned row can be available everywhere (`mov`/`add`/`cmp` are both
    ## `X64Inst` and `A64Inst`) and is still pinned: `{.instruction: add.}` means
    ## the machine's two-address add, not "a portable addition". A portable row
    ## can be missing on a target (`Popcount`, until a64 gets a lowering) and is
    ## still portable. Deriving the class from `targets` would also make a
    ## declaration's legal spelling flip the day a backend gains an expansion.
    icPortable   ## `{.intrinsic: X.}` — target-neutral opcode, any expansion
    icPinned     ## `{.instruction: X.}` — one named machine instruction

  IntrinsicTarget* = enum
    tgX64        ## x86-64
    tgA64        ## AArch64

  OperandRole* = enum
    roIn         ## a pure source
    roOut        ## a pure destination
    roInout      ## read *and* written (needs the `var`-first spelling)

  IntrinsicEffect* = enum
    efPure       ## no memory effect, no trap: CSE / fold / DCE eligible
    efReads      ## reads memory through a pointer operand
    efWrites     ## writes memory through a pointer operand
    efBarrier    ## no memory reordering across it

  PatKind* = enum
    ## The type-pattern vocabulary. `…W` binds (or matches) the row's single
    ## width variable `W`, so `(u W) -> (u W)` says "same width in and out"
    ## without enumerating widths. This is a first-order pattern language;
    ## unification is the `matchPat` each front end writes over its own cursors.
    ptNone       ## unused operand slot
    ptVoid       ## no result
    ptBool
    ptIntW       ## `(i W)`
    ptUIntW      ## `(u W)`
    ptAnyIntW    ## `(i W)` | `(u W)` | `(c W)`
    ptInt32      ## `(i 32)` — fixed width, does not bind `W`
    ptAnyInt     ## any integer, width unconstrained and unbound

const
  MaxOperands* = 4
    ## Enough for every row here. `(cmpxchg)`-shaped rows and the atomics will
    ## raise it; nothing depends on the exact value.

type
  IntrinsicRow* = object
    cls*: IntrinsicClass
    targets*: set[IntrinsicTarget]  ## where the opcode exists at all
    arity*: int                     ## number of *source* operands (= params)
    params*: array[MaxOperands, PatKind]
    roles*: array[MaxOperands, OperandRole]
    ret*: PatKind
    widths*: set[uint8]             ## the widths `W` may bind to
    tie*: int                       ## source-operand index the destination must
                                    ## alias (a two-address form), or -1. The
                                    ## allocator inserts the copy this needs —
                                    ## the same service the accumulator model
                                    ## already provides for built-in operators.
    effects*: set[IntrinsicEffect]

const
  IntrinsicNames*: array[IntrinsicOp, string] = [
    "", "Ctz", "Clz", "Popcount", "Bswap",
    "bsf", "bsr", "popcnt", "bswap", "rol", "ror", "clz", "rbit", "rev"]

  AllIn = [roIn, roIn, roIn, roIn]
  NoOps = [ptNone, ptNone, ptNone, ptNone]        ## no operands at all
  Un = [ptAnyIntW, ptNone, ptNone, ptNone]        ## one integer source
  UnCount = [ptAnyIntW, ptAnyInt, ptNone, ptNone] ## a value plus a count

  IntrinsicRows*: array[IntrinsicOp, IntrinsicRow] = [
    # The `NoIntrinsicOp` placeholder. Every field is spelled out: this file also
    # bootstraps under nimony, whose const evaluator has no `default(array[…])`.
    IntrinsicRow(cls: icPortable, targets: {}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptNone,
                 widths: {}, tie: -1, effects: {}),

    # ── portable ───────────────────────────────────────────────────────────
    # `Ctz`/`Clz`/`Popcount` count bits, so the result is an `int32` on every
    # target regardless of the operand width — matching `__builtin_ctz` and
    # nimony's `countTrailingZeroBits` family.
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptInt32,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptInt32,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}),
    # `Popcount` is x86-64 only for now: it lowers to SSE4.2 `POPCNT` there, while
    # AArch64 has no scalar population count (the NEON `cnt`/`addv` pair needs FP
    # register handling nifasm does not model yet, and the scalar SWAR expansion
    # needs a scratch register the operand model cannot request yet). `targets` is
    # exactly the field that says so, and a call on a64 is a compile error naming
    # the target rather than a silent fallback.
    IntrinsicRow(cls: icPortable, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptInt32,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}),
    # A byte swap preserves the width, so `W` appears on both sides.
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {16'u8, 32'u8, 64'u8}, tie: -1, effects: {efPure}),

    # ── x86-64 ─────────────────────────────────────────────────────────────
    # `bsf`/`bsr` leave the destination unmodified when the source is zero, so
    # they are strictly `inout` on the machine. The row declares the result a
    # pure `out` with "src = 0 is undefined", matching `__builtin_ctz`: a
    # per-row judgement recorded once here instead of rediscovered by every
    # caller. (`tzcnt`/`lzcnt` define the zero case but need a CPU-feature gate
    # the target model does not have yet.)
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {16'u8, 32'u8, 64'u8}, tie: -1, effects: {efPure}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {16'u8, 32'u8, 64'u8}, tie: -1, effects: {efPure}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {16'u8, 32'u8, 64'u8}, tie: -1, effects: {efPure}),
    # x86 `bswap` reverses a register IN PLACE, so the destination must already
    # hold the source: `tie = 0`. The source spelling stays `d = bswap(x)` and
    # the allocator inserts the copy — which it elides whenever `d` and `x`
    # share a home.
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {32'u8, 64'u8}, tie: 0, effects: {efPure}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,
                 params: UnCount, roles: AllIn, ret: ptAnyIntW,
                 widths: {8'u8, 16'u8, 32'u8, 64'u8}, tie: 0, effects: {efPure}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,
                 params: UnCount, roles: AllIn, ret: ptAnyIntW,
                 widths: {8'u8, 16'u8, 32'u8, 64'u8}, tie: 0, effects: {efPure}),

    # ── AArch64 ────────────────────────────────────────────────────────────
    # Three-address, so no tie. `clz(rbit(x))` is the a64 `Ctz`.
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure})
  ]

const LastIntrinsicOp* = RevOp
  ## The final row. Spelled out rather than `high(IntrinsicOp)` because this file
  ## is also compiled by nimony (it bootstraps `nimsem`), which has no iteration
  ## over an enum *type* — hence the ordinal loop below too.

proc intrinsicOpByName*(name: string; cls: IntrinsicClass): IntrinsicOp =
  ## Resolve a pragma argument to its row. The class is part of the key: the
  ## portable `Bswap` and the x86 instruction `bswap` are different rows, and
  ## `{.instruction: Bswap.}` must not silently mean the portable one.
  result = NoIntrinsicOp
  for i in 1 .. ord(LastIntrinsicOp):
    let op = IntrinsicOp(i)
    if IntrinsicNames[op] == name and IntrinsicRows[op].cls == cls:
      return op

proc hasInoutOperand*(r: IntrinsicRow): bool =
  ## A row the `d = ins(x)` spelling cannot express — it needs `ins(var d, x)`,
  ## which v1 rejects at the declaration rather than lowering half-way.
  result = false
  for i in 0 ..< r.arity:
    if r.roles[i] == roInout: return true

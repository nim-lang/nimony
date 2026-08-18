#
#           The intrinsic table
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## The single source of truth for `{.instruction: "X".}` / `{.intrinsic: "X".}`.
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
## * nimony's `sempragmas` resolves the pragma's name to an `IntrinsicOp`,
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
    # ── portable (`{.intrinsic: "X".}`): a lowering exists on every target,
    #    possibly as several instructions.
    CtzOp
    ClzOp
    PopcountOp
    BswapOp
    # ── target-pinned (`{.instruction: "X".}`): exactly one machine instruction.
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
    # ── flags. Two shapes: an instruction that DEFINES flags and returns
    #    nothing, and a zero-operand `bool` that READS one. There is no flag
    #    type and no flag variable — see `doc/intrinsics.md` §6.
    CmpOp
    TestOp
    ZfOp
    NotZfOp
    CfOp
    NotCfOp
    SfOp
    NotSfOp
    OfOp
    NotOfOp
    PfOp
    NotPfOp
    # ── two-address arithmetic: `roInout` on operand 0, no result. These are the
    #    rows the `d = ins(x)` spelling cannot express, so §4.1 gives them the
    #    `ins(var d, x)` form instead.
    AddOp
    SubOp
    AndOp
    OrOp
    XorOp
    ShlOp
    ShrOp
    SarOp
    NegOp
    NotOp
    IncOp
    DecOp
    # ── atomics (`{.intrinsic: "AtomicX".}`). Portable: every target has a
    #    lock-free lowering, though rarely as ONE instruction — x86-64 spells an
    #    RMW `lock xadd` / a `lock cmpxchg` retry loop, AArch64 an LL/SC
    #    `ldaxr`/`stlxr` loop. That is precisely what `icPortable` means, and it
    #    is why these are rows here rather than `importc: "__atomic_*"` calls:
    #    an atomic is an OPCODE, and calling it a call made every consumer treat
    #    an inline instruction sequence as an ABI call point.
    AtomicLoadOp
    AtomicStoreOp
    AtomicExchangeOp
    AtomicCompareExchangeOp
    AtomicFetchAddOp
    AtomicFetchSubOp
    AtomicFetchAndOp
    AtomicFetchOrOp
    AtomicFetchXorOp
    AtomicAddFetchOp
    AtomicSubFetchOp
    AtomicTestAndSetOp
    AtomicClearOp
    AtomicThreadFenceOp
    AtomicSignalFenceOp
    # ── AdvSIMD/NEON vector rows (`{.instruction.}`, AArch64). The shoggoth
    #    vectorizer synthesizes their declarations and emits `(instr …)`
    #    applications; user code can also declare them. The vector VALUE type is
    #    the opaque `(f 128)` (see `ptVec128`); lane meaning rides the opcode
    #    plus its trailing lane-bits literal.
    FldrqOp      # vec = fldrq(p: pointer; off: int)  — 16-byte load  [p+off]
    FstrqOp      # fstrq(p: pointer; off: int; v: vec) — 16-byte store [p+off]
    VfaddOp      # vec = vfadd(a, b: vec; lanebits)   — lane-wise fp add
    VfsubOp      # vec = vfsub(a, b: vec; lanebits)
    VfmulOp      # vec = vfmul(a, b: vec; lanebits)
    VfmlaOp      # vec = vfmla(acc, a, b: vec; lanebits) — fused acc + a*b; the
                 # result is tied to `acc` (the machine op accumulates in place)
    VdupOp       # vec = vdup(x: F; lanebits)         — broadcast x to every lane
    # ── the machine facts a stack walk needs (`lib/std/stacktraces`). Both are
    #    portable rows: the CONCEPT is target-neutral, the instruction is not.
    StackPointerOp
    TraceTableOp

  IntrinsicClass* = enum
    ## What kind of NAME the opcode is — fixed when the row is authored, and NOT
    ## derivable from `targets`, which is availability and grows as the backends
    ## do. A pinned row can be available everywhere (`mov`/`add`/`cmp` are both
    ## `X64Inst` and `A64Inst`) and is still pinned: `{.instruction: "add".}` means
    ## the machine's two-address add, not "a portable addition". A portable row
    ## can be missing on a target (`Popcount`, until a64 gets a lowering) and is
    ## still portable. Deriving the class from `targets` would also make a
    ## declaration's legal spelling flip the day a backend gains an expansion.
    icPortable   ## `{.intrinsic: "X".}` — target-neutral opcode, any expansion
    icPinned     ## `{.instruction: "X".}` — one named machine instruction

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

  MachineFlag* = enum
    ## The condition-code bits both targets expose. x86-64 has all five; AArch64's
    ## NZCV covers the same ground under other names (Z, C, N, V — no parity).
    ## They are named here so a row can say WHICH flag it defines or reads, which
    ## is what makes `zf()` checkable without a flag type: the column carries what
    ## a `bool` return cannot.
    mfZF, mfCF, mfSF, mfOF, mfPF

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
    ptValW       ## the type an atomic operates ON — an integer (binding `W`) or a
                 ## POINTER, which is a machine word and binds 64. A free-list head
                 ## and an ARC counter are both atomic cells; only one of them is a
                 ## number, so this is deliberately wider than `ptAnyIntW`.
    ptPtrW       ## `ptr X` where X matches `ptValW` — an atomic's memory operand.
                 ## The pointee is what fixes the ACCESS WIDTH, and getting that
                 ## wrong is not a slow program but a corrupt neighbouring field,
                 ## so the row names the relationship instead of leaving it to the
                 ## back end to guess.
    ptRawPtr     ## a bare `pointer` — the byte-wide flag cell of `AtomicTestAndSet`
                 ## / `AtomicClear`, which have no pointee type to read a width from
                 ## (C says a flag is a byte, and both back ends agree).
    ptMemOrder   ## a `__ATOMIC_*` memory-order argument. See `matchPat`: v1 accepts
                 ## any type here because v1 reads none.
    ptWeak       ## `AtomicCompareExchange`'s `weak` flag: a `bool` that says a
                 ## spurious failure is acceptable. v1 reads none either — both
                 ## lowerings are the strong form, which is always a legal answer.
    ptVec128     ## a 128-bit SIMD register value, spelled `(f 128)` in Leng: an
                 ## opaque bag of bits whose LANE interpretation lives entirely in
                 ## the opcode (and its trailing lane-bits operand), exactly as it
                 ## does in the machine's register file. Only the vector rows below
                 ## use it, and only the native back ends lower them.
    ptFloatW     ## `(f W)` — a scalar float binding the width variable (the lane
                 ## width a `vdup` broadcast replicates).
    ptAnyPtr     ## any pointer (`ptr`/`aptr` of anything): a vector load/store's
                 ## address operand. The ACCESS width is the instruction's own 16
                 ## bytes — deliberately not derived from the pointee, matching the
                 ## hardware (and `movdqu`'s documented behavior in nifasm).
    ptLaneBits   ## the trailing lane-width knob of the vector rows: an int LITERAL
                 ## (32 or 64) the back end reads at the call site to pick the
                 ## `.4s`/`.2d` arrangement. Like `ptMemOrder` it is not evaluated —
                 ## see `evaluatedOperands` — but unlike it, the back end DOES read
                 ## its literal value.

const
  MaxOperands* = 6
    ## `AtomicCompareExchange` is the widest row: pointer, expected, desired,
    ## `weak`, and a memory order for each of the success and failure paths.
    ## Nothing depends on the exact value.

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
    uses*: set[MachineFlag]         ## flags the row READS. A row with a `ptBool`
                                    ## result and a non-empty `uses` IS a flag
                                    ## read — the only shape that can produce a
                                    ## value with no register behind it.
    defs*: set[MachineFlag]         ## flags the row WRITES. Documentation in v1
                                    ## (nothing consults it yet), and the column a
                                    ## v2 needs to prove no instruction clobbered
                                    ## a flag between its definition and its read
                                    ## — the check §6 defers.

const
  IntrinsicNames*: array[IntrinsicOp, string] = [
    "", "Ctz", "Clz", "Popcount", "Bswap",
    "bsf", "bsr", "popcnt", "bswap", "rol", "ror", "clz", "rbit", "rev",
    "cmp", "test",
    # nifasm's own condition tags, verbatim — `of` and `no` included. They were
    # once `ovf`/`novf` because the pragma argument was an IDENT, and a Nim keyword
    # cannot be written where an expression is expected; inside a string literal a
    # keyword is just text, so the cover names are gone.
    "zf", "nz", "cf", "nc", "sf", "ns", "of", "no", "pf", "np",
    # The rule, now without exceptions: THE SOURCE NAME IS THE NIFASM TAG. `and`,
    # `or`, `xor`, `shl`, `shr` and `not` needed cover names (`bitand`, `shiftl`, …)
    # for exactly the reason `of` did, and lost them for the same reason. Nothing
    # in this table is a rename any more — so no consumer has two vocabularies to
    # reconcile, and a reader of one file can trust the other.
    "add", "sub", "and", "or", "xor", "shl", "shr", "sar",
    "neg", "not", "inc", "dec",
    # The atomics are PORTABLE rows, so — like `Ctz`/`Bswap` — the name is a
    # capitalised opcode rather than a mnemonic: there is no single instruction to
    # name after. It reads as the C builtin with the `__atomic_` prefix dropped and
    # the words joined, which is the mapping the C back end then undoes.
    "AtomicLoad", "AtomicStore", "AtomicExchange", "AtomicCompareExchange",
    "AtomicFetchAdd", "AtomicFetchSub", "AtomicFetchAnd", "AtomicFetchOr",
    "AtomicFetchXor", "AtomicAddFetch", "AtomicSubFetch",
    "AtomicTestAndSet", "AtomicClear", "AtomicThreadFence", "AtomicSignalFence",
    # The vector rows: THE SOURCE NAME IS THE NIFASM TAG, as everywhere above.
    "fldrq", "fstrq", "vfadd", "vfsub", "vfmul", "vfmla", "vdup"]
    "StackPointer", "TraceTable"]

  AllIn = [roIn, roIn, roIn, roIn, roIn, roIn]
  InoutFirst = [roInout, roIn, roIn, roIn, roIn, roIn]  ## operand 0 read AND written
  NoOps = [ptNone, ptNone, ptNone, ptNone, ptNone, ptNone]     ## no operands at all
  Un = [ptAnyIntW, ptNone, ptNone, ptNone, ptNone, ptNone]     ## one integer source
  UnCount = [ptAnyIntW, ptAnyInt, ptNone, ptNone, ptNone, ptNone] ## a value plus a count
  Bin = [ptAnyIntW, ptAnyIntW, ptNone, ptNone, ptNone, ptNone] ## two same-width int sources

  # ── atomic operand shapes ────────────────────────────────────────────────
  # Every one of them ends in a memory order, because the C builtins do and the
  # declarations these rows check are the same declarations the C back end still
  # compiles. v1 IGNORES the order (see `ptMemOrder`), but dropping the operand
  # would fork the source, so the row carries it and the lowering discards it.
  AtomLoad = [ptPtrW, ptMemOrder, ptNone, ptNone, ptNone, ptNone]
  AtomRmw = [ptPtrW, ptValW, ptMemOrder, ptNone, ptNone, ptNone]
    ## `(cell, operand, order)` — store, exchange and every fetch-op form
  AtomCas = [ptPtrW, ptPtrW, ptValW, ptWeak, ptMemOrder, ptMemOrder]
    ## `(cell, expected, desired, weak, success order, failure order)`. `expected`
    ## is a POINTER because a failed compare writes the observed value back through
    ## it — the one atomic with an output that is not the result.
  AtomFlag = [ptRawPtr, ptMemOrder, ptNone, ptNone, ptNone, ptNone]
  AtomFence = [ptMemOrder, ptNone, ptNone, ptNone, ptNone, ptNone]

  # ── vector operand shapes ────────────────────────────────────────────────
  VecLoad = [ptAnyPtr, ptAnyInt, ptNone, ptNone, ptNone, ptNone]
    ## `(address, byte offset)` — the offset is an int literal folded into the
    ## instruction's addressing mode (a multiple of 16), so an unrolled loop
    ## needs no extra pointer bumps.
  VecStore = [ptAnyPtr, ptAnyInt, ptVec128, ptNone, ptNone, ptNone]
  VecBin = [ptVec128, ptVec128, ptLaneBits, ptNone, ptNone, ptNone]
  VecFma = [ptVec128, ptVec128, ptVec128, ptLaneBits, ptNone, ptNone]
  VecDup = [ptFloatW, ptLaneBits, ptNone, ptNone, ptNone, ptNone]

  AllArith = {mfZF, mfCF, mfSF, mfOF, mfPF}
    ## What an x86 arithmetic/compare instruction leaves defined. `test` clears CF
    ## and OF rather than computing them, which is still a definition — the column
    ## says "do not expect the previous value here", and that is what a reader of
    ## `defs` needs to know.
  AllButCarry = {mfZF, mfSF, mfOF, mfPF}
    ## `inc`/`dec` deliberately PRESERVE the carry flag — that is the whole reason
    ## they exist next to `add r, 1`. A row that got this wrong would silently
    ## break a multi-word add loop, so the column records it.
  IntWidths = {8'u8, 16'u8, 32'u8, 64'u8}

  # An atomic is never `efPure`: the whole point is the memory traffic, and a
  # "pure" one would be DCE's to delete. `efBarrier` is on all of them including
  # the plain load and store — v1 emits every sequence at sequential-consistency
  # strength on both targets, so the barrier is the truth about what is emitted,
  # not an aspiration. A v2 that honours the order operand relaxes this per call,
  # which is a property of the CALL, not of the row.
  AtomRead = {efReads, efBarrier}
  AtomWrite = {efWrites, efBarrier}
  AtomModify = {efReads, efWrites, efBarrier}

  IntrinsicRows*: array[IntrinsicOp, IntrinsicRow] = [
    # The `NoIntrinsicOp` placeholder. Every field is spelled out: this file also
    # bootstraps under nimony, whose const evaluator has no `default(array[…])`.
    IntrinsicRow(cls: icPortable, targets: {}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptNone,
                 widths: {}, tie: -1, effects: {}, uses: {}, defs: {}),

    # ── portable ───────────────────────────────────────────────────────────
    # `Ctz`/`Clz`/`Popcount` count bits, so the result is an `int32` on every
    # target regardless of the operand width — matching `__builtin_ctz` and
    # nimony's `countTrailingZeroBits` family.
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptInt32,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptInt32,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    # `Popcount` is x86-64 only for now: it lowers to SSE4.2 `POPCNT` there, while
    # AArch64 has no scalar population count (the NEON `cnt`/`addv` pair needs FP
    # register handling nifasm does not model yet, and the scalar SWAR expansion
    # needs a scratch register the operand model cannot request yet). `targets` is
    # exactly the field that says so, and a call on a64 is a compile error naming
    # the target rather than a silent fallback.
    IntrinsicRow(cls: icPortable, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptInt32,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    # A byte swap preserves the width, so `W` appears on both sides.
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {16'u8, 32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),

    # ── x86-64 ─────────────────────────────────────────────────────────────
    # `bsf`/`bsr` leave the destination unmodified when the source is zero, so
    # they are strictly `inout` on the machine. The row declares the result a
    # pure `out` with "src = 0 is undefined", matching `__builtin_ctz`: a
    # per-row judgement recorded once here instead of rediscovered by every
    # caller. (`tzcnt`/`lzcnt` define the zero case but need a CPU-feature gate
    # the target model does not have yet.)
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {16'u8, 32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {16'u8, 32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {16'u8, 32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    # x86 `bswap` reverses a register IN PLACE, so the destination must already
    # hold the source: `tie = 0`. The source spelling stays `d = bswap(x)` and
    # the allocator inserts the copy — which it elides whenever `d` and `x`
    # share a home.
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {32'u8, 64'u8}, tie: 0, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,
                 params: UnCount, roles: AllIn, ret: ptAnyIntW,
                 widths: {8'u8, 16'u8, 32'u8, 64'u8}, tie: 0, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,
                 params: UnCount, roles: AllIn, ret: ptAnyIntW,
                 widths: {8'u8, 16'u8, 32'u8, 64'u8}, tie: 0, effects: {efPure}, uses: {}, defs: {}),

    # ── AArch64 ────────────────────────────────────────────────────────────
    # Three-address, so no tie. `clz(rbit(x))` is the a64 `Ctz`.
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 1,
                 params: Un, roles: AllIn, ret: ptAnyIntW,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),

    # ── flags ──────────────────────────────────────────────────────────────
    # `cmp`/`test` compute nothing and return nothing: their whole output is in
    # `defs`. That is exactly why they are NOT `efPure` — a "pure" row with a void
    # result is dead by definition, and DCE would be right to delete it. The flag
    # columns are what makes a flag-only instruction non-removable.
    IntrinsicRow(cls: icPinned, targets: {tgX64, tgA64}, arity: 2,
                 params: Bin, roles: AllIn, ret: ptVoid,
                 widths: {8'u8, 16'u8, 32'u8, 64'u8}, tie: -1, effects: {},
                 uses: {}, defs: AllArith),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,
                 params: Bin, roles: AllIn, ret: ptVoid,
                 widths: {8'u8, 16'u8, 32'u8, 64'u8}, tie: -1, effects: {},
                 uses: {}, defs: AllArith),

    # The reads. Zero operands, `bool` result, and a `uses` naming the one bit —
    # the third field is what distinguishes these from an ordinary predicate, and
    # what lets the rule of §6 ("legal only where it needs no materialisation") be
    # checked without a flag type infecting the type system. `zf`/`nz` exist on
    # both targets (AArch64's Z under another name); the rest are x86-64, which is
    # what the nifasm `(ite …)` vocabulary offers there.
    IntrinsicRow(cls: icPinned, targets: {tgX64, tgA64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfZF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64, tgA64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfZF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfCF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfCF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfSF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfSF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfOF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfOF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfPF}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 0,
                 params: NoOps, roles: AllIn, ret: ptBool,
                 widths: {}, tie: -1, effects: {}, uses: {mfPF}, defs: {}),

    # ── two-address arithmetic ─────────────────────────────────────────────
    # `roInout` on operand 0 is NOT `tie`. `tie` says the RESULT aliases a source
    # (`d = bswap(x)` — still a value-producing form, and the allocator inserts
    # the copy). Here there is no result at all: the output goes back through
    # operand 0, which is why §4.1 gives these the `ins(var d, x)` spelling. A
    # `var` parameter reaches Leng as `(haddr d)`, and that tag is exactly what
    # tells the back end "bind d's location" rather than "materialise a pointer".
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,           # add
                 params: Bin, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,           # sub
                 params: Bin, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    # AND/OR/XOR clear CF and OF and set SF/ZF/PF — all five are DEFINED.
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,           # and
                 params: Bin, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,           # or
                 params: Bin, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,           # xor
                 params: Bin, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    # The shifts take a COUNT, not a same-width operand, hence `UnCount`. A
    # variable count must live in `cl`; v1 takes a literal, like `rol`/`ror`.
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,           # shl
                 params: UnCount, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,           # shr
                 params: UnCount, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 2,           # sar
                 params: UnCount, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,           # neg
                 params: Un, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllArith),
    # x86 NOT touches no flags at all — the one row here whose `defs` is empty.
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,           # not
                 params: Un, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,           # inc
                 params: Un, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllButCarry),
    IntrinsicRow(cls: icPinned, targets: {tgX64}, arity: 1,           # dec
                 params: Un, roles: InoutFirst, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {}, uses: {}, defs: AllButCarry),

    # ── atomics ────────────────────────────────────────────────────────────
    # `tie` stays -1 throughout even though several x86 lowerings ARE two-address
    # (`lock xadd [p], r` returns the old value in `r`). `tie` names an alias the
    # ALLOCATOR must arrange between a result and a declared operand; here the
    # register that gets destroyed is the sequence's own, seeded from an operand by
    # the back end. Spelling that as a tie would over-constrain AArch64, whose LL/SC
    # loop has no such relationship at all.
    #
    # `widths` is `IntWidths` because the machine sizes the access to the cell —
    # `ldaxrb`/`ldaxrh` and the 8/16-bit `lock` forms all exist. Note that `W`
    # usually stays UNBOUND at the declaration: these are generic over the cell
    # type, so the width is the instantiation's and the back end reads it off the
    # pointee at the call site. See `matchPat`.

    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 2,  # AtomicLoad
                 params: AtomLoad, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomRead, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicStore
                 params: AtomRmw, roles: AllIn, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: AtomWrite, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicExchange
                 params: AtomRmw, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    # The result is the SUCCESS FLAG, not the cell's type — so `ptBool`, and the
    # observed value leaves through the `expected` pointer instead.
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 6,  # AtomicCompareExchange
                 params: AtomCas, roles: AllIn, ret: ptBool,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    # The fetch-ops return the value BEFORE the update, the `*_fetch` forms the
    # value after. Two rows rather than one plus a flag: the difference is which
    # register the sequence ends up reading, which is the lowering's whole shape.
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicFetchAdd
                 params: AtomRmw, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicFetchSub
                 params: AtomRmw, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicFetchAnd
                 params: AtomRmw, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicFetchOr
                 params: AtomRmw, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicFetchXor
                 params: AtomRmw, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicAddFetch
                 params: AtomRmw, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 3,  # AtomicSubFetch
                 params: AtomRmw, roles: AllIn, ret: ptValW,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    # `targets: {}` — neither back end lowers the flag pair yet, and `targets` is
    # exactly the column that says so: a call is then a compile error naming the
    # target, never a silent fallback. The C back end does not consult `targets`
    # (it has `__atomic_test_and_set`), so the rows are usable there today.
    IntrinsicRow(cls: icPortable, targets: {}, arity: 2,              # AtomicTestAndSet
                 params: AtomFlag, roles: AllIn, ret: ptBool,
                 widths: IntWidths, tie: -1, effects: AtomModify, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {}, arity: 2,              # AtomicClear
                 params: AtomFlag, roles: AllIn, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: AtomWrite, uses: {}, defs: {}),
    # A fence has no operand but the order and no result at all: its entire content
    # is `efBarrier`, which is what keeps it from being deleted for producing
    # nothing — the same argument `cmp`'s `defs` makes for a flag-only instruction.
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 1,  # AtomicThreadFence
                 params: AtomFence, roles: AllIn, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {efBarrier}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 1,  # AtomicSignalFence
                 params: AtomFence, roles: AllIn, ret: ptVoid,
                 widths: IntWidths, tie: -1, effects: {efBarrier}, uses: {}, defs: {}),

    # ── AdvSIMD/NEON (AArch64) ─────────────────────────────────────────────
    # All pinned: one machine instruction each, named by its nifasm tag. The
    # loads/stores carry their memory effect so nothing reorders or deletes
    # them; the arithmetic is pure and CSE-eligible like any other value.
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 2,           # fldrq
                 params: VecLoad, roles: AllIn, ret: ptVec128,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efReads}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 3,           # fstrq
                 params: VecStore, roles: AllIn, ret: ptVoid,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efWrites}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 3,           # vfadd
                 params: VecBin, roles: AllIn, ret: ptVec128,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 3,           # vfsub
                 params: VecBin, roles: AllIn, ret: ptVec128,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 3,           # vfmul
                 params: VecBin, roles: AllIn, ret: ptVec128,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    # `tie: 0` — the machine op accumulates IN PLACE (fmla Vd += Vn*Vm), so the
    # result must land where operand 0 lives. The vectorizer spells every use as
    # `(asgn acc (instr vfmla acc a b bits))`, which satisfies the tie with no
    # copy; the a64 back end asserts it rather than inserting a 128-bit move.
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 4,           # vfmla
                 params: VecFma, roles: AllIn, ret: ptVec128,
                 widths: {32'u8, 64'u8}, tie: 0, effects: {efPure}, uses: {}, defs: {}),
    IntrinsicRow(cls: icPinned, targets: {tgA64}, arity: 2,           # vdup
                 params: VecDup, roles: AllIn, ret: ptVec128,
                 widths: {32'u8, 64'u8}, tie: -1, effects: {efPure}, uses: {}, defs: {})

    # ── stack walking ──────────────────────────────────────────────────────
    # `StackPointer` reads SP into a register. `efPure` is not a shrug: SP is
    # CONSTANT between a proc's prologue and its epilogue (`arkham/design.md`),
    # so within one body the value really does not change, and CSE-ing two reads
    # of it is exact rather than merely harmless. The interesting caller is a
    # `{.naked.}` proc, which has no prologue at all — there SP still points at
    # the return address the `call` pushed, and that is the seed of a stack walk.
    IntrinsicRow(cls: icPortable, targets: {tgX64, tgA64}, arity: 0,  # StackPointer
                 params: NoOps, roles: AllIn, ret: ptRawPtr,
                 widths: {}, tie: -1, effects: {efPure}, uses: {}, defs: {}),
    # `TraceTable` is the address of the per-proc metadata nifasm lays down at the
    # end of `.text` (`nifasm/tracetable.nim`): the same code ranges and CFA
    # offsets `.eh_frame` carries, in a form the running program can read without
    # a DWARF interpreter. A link-time constant, hence `efPure`. x86-64 only for
    # now — the table is emitted on every target, but only the x86-64 walk is
    # written and tested (`lib/std/stacktraces`).
    IntrinsicRow(cls: icPortable, targets: {tgX64}, arity: 0,         # TraceTable
                 params: NoOps, roles: AllIn, ret: ptRawPtr,
                 widths: {}, tie: -1, effects: {efPure}, uses: {}, defs: {})
  ]

const LastIntrinsicOp* = TraceTableOp
  ## The final row. Spelled out rather than `high(IntrinsicOp)` because this file
  ## is also compiled by nimony (it bootstraps `nimsem`), which has no iteration
  ## over an enum *type* — hence the ordinal loop below too.

proc intrinsicOpByName*(name: string; cls: IntrinsicClass): IntrinsicOp =
  ## Resolve a pragma argument to its row. The class is part of the key: the
  ## portable `Bswap` and the x86 instruction `bswap` are different rows, and
  ## `{.instruction: "Bswap".}` must not silently mean the portable one.
  result = NoIntrinsicOp
  for i in 1 .. ord(LastIntrinsicOp):
    let op = IntrinsicOp(i)
    if IntrinsicNames[op] == name and IntrinsicRows[op].cls == cls:
      return op

proc isFlagRead*(r: IntrinsicRow): bool {.inline.} =
  ## A row whose value IS a machine flag rather than a register: `bool` result,
  ## no operands, and a `uses` naming the bit. Such a value cannot be stored,
  ## passed or returned — the instruction that would materialise it (`setcc`) is
  ## itself a flag reader, and anything emitted between the definition and the
  ## read may have clobbered the bit. v1 therefore allows it in exactly one
  ## place: an `if` condition. See `doc/intrinsics.md` §6.
  r.ret == ptBool and r.uses != {}

proc inoutOperand*(r: IntrinsicRow): int =
  ## The index of the row's `inout` operand, or -1. Such a row has no result: it
  ## writes through that operand, which is spelled `var` in the declaration and
  ## arrives as `(haddr d)` at the call site.
  result = -1
  for i in 0 ..< r.arity:
    if r.roles[i] == roInout: return i

proc isFlagWrite*(r: IntrinsicRow): bool {.inline.} =
  ## A row whose ONLY output is flags (`cmp`, `test`): it must be a STATEMENT,
  ## since there is no value to bind, and it must not be deleted for having none.
  ## The `inout` exclusion matters: `add` is also void and also sets every flag,
  ## but its result goes to a register, so it is an ordinary instruction that
  ## happens to define flags — not a flag instruction.
  r.ret == ptVoid and r.defs != {} and r.inoutOperand < 0

const IgnoredPats* = {ptMemOrder, ptWeak, ptLaneBits}
  ## Operand patterns the back ends do not EVALUATE into a register. See
  ## `evaluatedOperands`. (`ptLaneBits` is still READ — as a literal, at the
  ## call site — it just never needs a register.)

proc evaluatedOperands*(r: IntrinsicRow): int =
  ## How many LEADING operands a back end must actually evaluate. Everything past
  ## them is a compile-time knob v1 ignores — the memory orders, and
  ## `AtomicCompareExchange`'s `weak` — and evaluating one is not merely wasted
  ## work: `__ATOMIC_SEQ_CST` and its siblings are `importc` globals with no
  ## definition in a C-runtime-free native program, so a load of one would not
  ## link. Every row is arranged so the ignored operands are the TRAILING ones,
  ## which is what lets a count stand in for a per-operand test.
  ##
  ## The C back end is the exception and evaluates all of them: it hands the
  ## arguments straight to the real `__atomic_*` builtin, which does read them.
  result = r.arity
  while result > 0 and r.params[result-1] in IgnoredPats: dec result

proc isVoidResult*(r: IntrinsicRow): bool {.inline.} =
  ## A row that yields no value AND writes no declared operand: `AtomicStore`,
  ## `AtomicClear`, the fences. Distinct from both neighbours — a flag row's output
  ## IS the flags (`isFlagWrite`) and a two-address row's goes back through operand
  ## 0 (`inoutOperand`), while here there is nothing to read afterwards at all. The
  ## whole content is in `effects`, and it is `effects` that keeps such a row alive.
  ## The back ends need this to know NOT to home a result register for the node.
  r.ret == ptVoid and r.defs == {} and r.inoutOperand < 0

proc isMachineQuery*(op: IntrinsicOp): bool {.inline.} =
  ## A row that reads a machine or link-time fact instead of computing on
  ## operands: no sources, a pointer result, and a lowering that is one
  ## instruction with nothing to place. The back ends need the distinction
  ## because every other zero-operand row is a FLAG read, whose result cannot be
  ## materialised at all — these produce an ordinary register value.
  op in {StackPointerOp, TraceTableOp}

proc isAtomic*(op: IntrinsicOp): bool {.inline.} =
  ## An atomic row. The back ends lower these as a self-contained instruction
  ## SEQUENCE rather than a transliteration, so they branch on this once instead of
  ## listing the opcodes at every site.
  ord(op) >= ord(AtomicLoadOp) and ord(op) <= ord(AtomicSignalFenceOp)

proc hasInoutOperand*(r: IntrinsicRow): bool =
  ## A row the `d = ins(x)` spelling cannot express — it needs `ins(var d, x)`,
  ## which v1 rejects at the declaration rather than lowering half-way.
  result = false
  for i in 0 ..< r.arity:
    if r.roles[i] == roInout: return true

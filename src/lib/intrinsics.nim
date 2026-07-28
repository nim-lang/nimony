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
    # nifasm's own condition tags, except the overflow pair: its tags are `of`
    # and `no`, and `of` is a Nim keyword, so `{.instruction: of.}` does not
    # parse (a pragma entry is an expression — the same collision that made
    # `{.asm.}` impossible). `ovf`/`novf` are the source spellings; the backend
    # maps them back, so the assembler still sees `(of)`/`(no)`.
    "zf", "nz", "cf", "nc", "sf", "ns", "ovf", "novf", "pf", "np"]

  AllIn = [roIn, roIn, roIn, roIn]
  NoOps = [ptNone, ptNone, ptNone, ptNone]        ## no operands at all
  Un = [ptAnyIntW, ptNone, ptNone, ptNone]        ## one integer source
  UnCount = [ptAnyIntW, ptAnyInt, ptNone, ptNone] ## a value plus a count
  Bin = [ptAnyIntW, ptAnyIntW, ptNone, ptNone]    ## two same-width integer sources

  AllArith = {mfZF, mfCF, mfSF, mfOF, mfPF}
    ## What an x86 arithmetic/compare instruction leaves defined. `test` clears CF
    ## and OF rather than computing them, which is still a definition — the column
    ## says "do not expect the previous value here", and that is what a reader of
    ## `defs` needs to know.

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
                 widths: {}, tie: -1, effects: {}, uses: {mfPF}, defs: {})
  ]

const LastIntrinsicOp* = NotPfOp
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

proc isFlagRead*(r: IntrinsicRow): bool {.inline.} =
  ## A row whose value IS a machine flag rather than a register: `bool` result,
  ## no operands, and a `uses` naming the bit. Such a value cannot be stored,
  ## passed or returned — the instruction that would materialise it (`setcc`) is
  ## itself a flag reader, and anything emitted between the definition and the
  ## read may have clobbered the bit. v1 therefore allows it in exactly one
  ## place: an `if` condition. See `doc/intrinsics.md` §6.
  r.ret == ptBool and r.uses != {}

proc isFlagWrite*(r: IntrinsicRow): bool {.inline.} =
  ## A row whose only output is flags (`cmp`, `test`): it must be a STATEMENT,
  ## since there is no value to bind, and it must not be deleted for having none.
  r.ret == ptVoid and r.defs != {}

proc hasInoutOperand*(r: IntrinsicRow): bool =
  ## A row the `d = ins(x)` spelling cannot express — it needs `ins(var d, x)`,
  ## which v1 rejects at the declaration rather than lowering half-way.
  result = false
  for i in 0 ..< r.arity:
    if r.roles[i] == roInout: return true

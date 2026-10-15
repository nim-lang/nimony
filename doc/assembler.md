# Assembler procs

An `{.assembler.}` proc is machine code written in Nimony syntax. Every
statement of its body is **one instruction**, in source order; nothing is
inserted, reordered, hoisted or materialized, and every value lives where a
pragma says it does. The native backend (arkham) checks the body and
transliterates it; nifasm, a typed assembler, type-checks the result.

What that buys over an ordinary proc is a guarantee about *which instructions
run*. The reason to want one is code whose correctness is a property of the
instruction stream rather than of its result — above all **constant-time
cryptography**: the field arithmetic of `std/crypto/x25519` and the block
functions of `std/crypto/chacha20poly1305` are assembler procs on the native
backend (see `lib/std/crypto/private/`).

The C backend cannot compile an assembler proc and says so. Code that wants an
assembler path keeps a portable one beside it:

```nim
when defined(nimNativeBackend) and defined(amd64):
  import std/crypto/private/fe25519x64     # assembler
else:
  ...                                      # portable Nimony
```

`nimNativeBackend` is defined whenever arkham compiles the program
(`nimony n`). Everything below is x86-64; the AArch64, Cortex-M and RV32
backends implement a subset of the same mode.


## Locations

Every parameter and local declares its location. A parameter's register is
its ABI register — the pragma is an *assertion*, checked:

```nim
proc feAdd(r {.register: "rdi".}: FePtr; a {.register: "rsi".}: FePtr;
           b {.register: "rdx".}: FePtr) {.assembler.} =
  var t0 {.register: "r8".}: uint64
  var m {.register: "rax".}: uint64
  var spill {.stack.}: uint64              # a stack slot
```

* The 16 general-purpose registers except `rsp` and `rbp`, which the frame
  owns. Callee-saved ones are pushed and popped by the prologue/epilogue.
* `result` is the ABI return register without a pragma.
* Several locals may share a register: they are names for the same machine
  register. When the *type* differs — `hi: uint64` landing in `rdx` after the
  pointer that arrived there was copied away — the register is re-declared,
  and every later use is typed as what it now holds. Declare such a local only
  once the previous value is dead.
* A local's declared type is the operand width: a `uint32` local takes 32-bit
  two-address operations and rotates.

## Statements

| Statement | Instruction |
|---|---|
| `x = y`, `x = 5` | `mov` |
| `x = p[i]`, `x = p[]`, `x = p.f` | `mov` from memory |
| `p[i] = x`, `p[i] = 5` | `mov` to memory |
| `x = rol(y, 3)` (a value row) | the row's instruction, `mov` first if `x` is not `y` |
| `add(x, y)`, `add(p[i], y)`, `add(x, p[i])` (a two-address row) | `add x, y` |
| `cmp(x, y)` (a flag row) | `cmp x, y` |
| `if zf(): …` / `else: …` | a conditional jump on one flag |
| `while true: … break` | a loop |
| `return x` | `mov` to the return register, epilogue |

Memory is reached only through a pointer that is already in a register:
`p[i]` with `p: ptr UncheckedArray[T]` and `i` a literal or an `int` register
(the element size must be a legal scale, 1/2/4/8, for a register index),
`p[]` and `p.f` through a `ptr`. A memory-to-memory move and a computed
operand are rejected — either would need a register nobody declared.

## Instructions

An instruction is declared as an ordinary proc carrying
`{.instruction: "mnemonic".}` and checked against its row in
`src/lib/intrinsics.nim` at the declaration. The operand roles fix the
signature: an operand the instruction writes is `var`.

```nim
proc addq(d: var uint64; s: uint64) {.instruction: "add".}
proc adcq(d: var uint64; s: uint64) {.instruction: "adc".}
proc mulq(lo: var uint64; hi: var uint64; s: uint64) {.instruction: "mul".}
proc cmovnzq(d: var uint64; s: uint64) {.instruction: "cmovnz".}
proc zf(): bool {.instruction: "zf".}
```

The x86-64 vocabulary: `add sub and or xor shl shr sar neg not inc dec`,
`adc sbb` (64-bit), `imul`, `mul`, `cmp test`, the conditional moves
`cmovz cmovnz cmovc cmovnc cmovs cmovns cmovo cmovno cmova cmovbe cmovl
cmovge cmovg cmovle`, the value rows `bsf bsr popcnt bswap rol ror`, the flag
reads `zf nz cf nc sf ns of no pf np`, and `syscall`.

`mul(lo, hi, s)` is x86's one-operand multiply, `rdx:rax = rax * s`: `lo` must
be a local in `rax` and `hi` one in `rdx`, and `s` may not be `rdx`. The
signature names both halves so the reader sees them; the pins are checked.

Rows that read a flag (`adc`, `sbb`, the `cmov`s, the flag reads) and `mul`
are legal only inside an assembler proc. In compiled code an instruction the
compiler inserted could sit between the flag's definition and its use.

## Flags

A flag is a value with no register, written by most ALU instructions as a
side effect — and in a carry chain or a conditional move it IS the data. The
classic bug is an instruction between definition and use that overwrites it.
arkham tracks which flags hold a known definition along the body and rejects
a read of any other:

```nim
add(t, a)          # defines CF
mul(lo, hi, b)     # redefines CF ("the high half is not zero")
adc(t, hi)         # Error: `adc` reads CF as a carry, but the last
                   # instruction to set CF was `mul`, whose CF is not one
```

* Nothing is known on entry to the proc, at a label, at a loop head (the
  back edge brings whatever the body's end left) or after a loop.
* After an `if`, what both arms leave defined is defined.
* `mov`, loads, stores, `not`, `bswap`, the `cmov`s and flag reads leave flags
  alone; `inc`/`dec` keep CF; `mul`/`imul` define only CF and OF; a rotate
  defines CF; `bsf`/`bsr` only ZF; a shift by a literal non-zero count
  defines all but OF, by a register none.
* `adc`/`sbb` additionally need CF from an arithmetic carry: `add`, `sub`,
  `adc`, `sbb`, `neg`, `cmp`, or a logic operation, which clears it (`xor`
  of a register with itself is the idiom for a carry-in of zero).

## A worked example

Two-limb addition through memory, and a branch-free select:

```nim
type Words = ptr UncheckedArray[uint64]

proc test(a, b: uint64) {.instruction: "test".}

proc add2(r {.register: "rdi".}: Words; a {.register: "rsi".}: Words;
          b {.register: "rdx".}: Words) {.assembler.} =
  var x {.register: "rax".}: uint64
  var y {.register: "rcx".}: uint64
  x = a[0]
  addq(x, b[0])
  r[0] = x
  y = a[1]
  adcq(y, b[1])        # CF from the add: mov and the stores keep it
  r[1] = y

proc select(c {.register: "rdi".}: uint64; x {.register: "rsi".}: uint64;
            y {.register: "rdx".}: uint64): uint64 {.assembler.} =
  var r {.register: "rax".}: uint64
  r = y
  test(c, c)
  cmovnzq(r, x)        # no branch, whatever c is
  result = r
```

`tests/nimony/asmnative/` holds more, and `lib/std/crypto/private/` the
production uses: 4x64-bit field multiplication with a `mul`/`adc` carry chain
and reduction by `2^256 = 38 (mod p)`, Poly1305's 130-bit accumulator with
the final `h >= p` decided by `shr` and acted on with `cmovnz`, and the
ChaCha20 rounds in 32-bit registers.

#
#           Arkham — shared machine model for the native code generators
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## Architecture-neutral register slots and the `MachineDesc` the register
## allocator is parameterized over.
##
## `Reg` / `FReg` are an abstract, arch-neutral enumeration of physical
## registers — numbered slots, not hardware names (every modern ISA numbers its
## registers, conflates pointers with integers, and keeps the floating-point
## file separate; that is exactly the GPR `Reg` vs FP `FReg` split here). The
## enum is sized for the widest target (AArch64: 31 GPRs, 32 FP regs). A backend
## reuses a *subset* of the slots — x86-64, with 16 GPRs / 16 XMM, simply never
## allocates `R16..R30` / `F16..F31` — and renders each slot to its own spelling
## through its own `regName` shim (`R0` → `"x0"` on AArch64, `"rax"` on x86-64).
##
## The allocator only ever sees the slots and the `MachineDesc`; it has no
## knowledge of any concrete ABI. A backend describes its register file and
## calling convention by populating a `MachineDesc`.

import slots

type
  Reg* = enum   ## abstract GPR slot; a backend maps it to a hardware register
    R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15,
    R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30,
    SP, NoReg

  FReg* = enum  ## abstract FP/SIMD slot
    F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15,
    F16, F17, F18, F19, F20, F21, F22, F23, F24, F25, F26, F27, F28, F29, F30, F31,
    NoFReg

  MachineDesc* = object
    ## A target's register file + calling convention, as the allocator needs it.
    ## All registers are slots from a *subset* of `Reg`/`FReg` (a narrower ISA
    ## like x86-64 leaves the high slots unused).
    intArgRegs*: seq[Reg]            ## integer/pointer argument registers, ABI order
    floatArgRegs*: seq[FReg]         ## float argument registers, ABI order
    intTempRegs*: seq[Reg]           ## caller-saved scratch (call-free locals)
    intCalleeSaved*: seq[Reg]        ## callee-saved (locals live across a call)
    floatTempRegs*: seq[FReg]        ## caller-saved FP scratch
    floatCalleeSaved*: seq[FReg]     ## callee-saved FP regs
    intCalleeSavedSet*: set[Reg]     ## membership form of `intCalleeSaved`
    floatCalleeSavedSet*: set[FReg]  ## membership form of `floatCalleeSaved`
    aggrByRefThreshold*: int         ## aggregates larger than this go by reference

type
  LocKind* = enum
    Undef          ## the dontCare target (fill me in)
    InReg          ## value in a GPR
    InFReg         ## value in an FP/SIMD register
    OnStack        ## value in a frame slot at `offset` (from the frame base)
    NamedStack     ## an aggregate stack var managed by nifasm, addressed by name
    Imm            ## a known immediate (constant / target hint)

  Location* = object
    typ*: AsmSlot
    case kind*: LocKind
    of Undef: discard
    of InReg: r*: Reg
    of InFReg: f*: FReg
    of OnStack: offset*: int
    of NamedStack: name*: string
    of Imm: ival*: int64

const
  dontCare* = Location(kind: Undef)

proc regLoc*(r: Reg; typ: AsmSlot): Location {.inline.} =
  Location(kind: InReg, r: r, typ: typ)
proc fregLoc*(f: FReg; typ: AsmSlot): Location {.inline.} =
  Location(kind: InFReg, f: f, typ: typ)
proc stackLoc*(offset: int; typ: AsmSlot): Location {.inline.} =
  Location(kind: OnStack, offset: offset, typ: typ)
proc namedStackLoc*(name: string; typ: AsmSlot): Location {.inline.} =
  Location(kind: NamedStack, name: name, typ: typ)
proc immLoc*(ival: int64; typ: AsmSlot): Location {.inline.} =
  Location(kind: Imm, ival: ival, typ: typ)

proc sameReg*(a, b: Location): bool {.inline.} =
  ## True if both name the same physical register (for move coalescing).
  (a.kind == InReg and b.kind == InReg and a.r == b.r) or
  (a.kind == InFReg and b.kind == InFReg and a.f == b.f)

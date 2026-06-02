#
#           Arkham — x86-64 / System V backend machine model
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## x86-64 / System V (Linux) backend register model: the shared register slots
## (`machinedesc.Reg`) reinterpreted as the x86-64 GPRs, the `regName` shim that
## renders them to AT&T-less x86 spellings, and the `x64Machine` description fed
## to the (arch-neutral) register allocator. x86-64 has 16 GPRs, so it uses only
## the low 16 `Reg` slots; the slot↔hardware mapping follows the ModRM encoding
## order (R0=rax, R1=rcx, …, R7=rdi, R8..R15=r8..r15).

import machinedesc

const
  RAX* = R0
  RCX* = R1
  RDX* = R2
  RBX* = R3
  RSP* = R4
  RBP* = R5
  RSI* = R6
  RDI* = R7
  # R8..R15 are the slots of the same name.

proc x64RegName*(r: Reg): string =
  case r
  of R0: "rax"
  of R1: "rcx"
  of R2: "rdx"
  of R3: "rbx"
  of R4: "rsp"
  of R5: "rbp"
  of R6: "rsi"
  of R7: "rdi"
  of R8: "r8"
  of R9: "r9"
  of R10: "r10"
  of R11: "r11"
  of R12: "r12"
  of R13: "r13"
  of R14: "r14"
  of R15: "r15"
  of SP: "rsp"
  else: "<noreg>"

const
  ## System V AMD64 calling convention, as the arch-neutral allocator needs it.
  ##  * integer args:   rdi, rsi, rdx, rcx, r8, r9
  ##  * integer return: rax
  ##  * callee-saved:   rbx, r12–r15 (rbp/rsp reserved for the frame)
  ##  * volatile scratch arkham manages: r10, r11 (the non-arg caller-saved GPRs;
  ##    rax + the arg registers are reserved for return/argument shuffling, as on
  ##    AArch64 where x0–x7 are kept out of the temp pool)
  ##  * float: xmm0–7 args/return (unused by the v0 scalar path)
  x64Machine* = MachineDesc(
    intArgRegs: @[RDI, RSI, RDX, RCX, R8, R9],
    floatArgRegs: @[F0, F1, F2, F3, F4, F5, F6, F7],
    intTempRegs: @[R10, R11],
    intCalleeSaved: @[RBX, R12, R13, R14, R15],
    floatTempRegs: @[F8, F9, F10, F11, F12, F13, F14, F15],
    floatCalleeSaved: @[],
    intCalleeSavedSet: {RBX, R12, R13, R14, R15},
    floatCalleeSavedSet: {},
    aggrByRefThreshold: 16)

  ## The GPRs a SysV call clobbers — the caller-saved volatiles arkham manages
  ## (rax + the arg registers + r10/r11). Emitted as the proc's `(clobber …)`.
  x64ClobbersGpr* = [RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11]

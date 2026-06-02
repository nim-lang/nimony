#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## AArch64 / Darwin backend machine model: the AAPCS64 register classes, the
## `regName` shim that renders the abstract register slots to AArch64 spellings,
## and the `aarch64Machine` description handed to the (arch-neutral) register
## allocator. The slot enums and `MachineDesc`/`Location` types live in
## `machinedesc` and are re-exported, so downstream modules keep importing
## `machine`.

import slots, machinedesc
export machinedesc

const
  FP* = R29   ## frame pointer
  LR* = R30   ## link register

  # ── AAPCS64 / Darwin register classes ──────────────────────────────────
  IntArgRegs*   = [R0, R1, R2, R3, R4, R5, R6, R7]   ## int args + return (x0)
  FloatArgRegs* = [F0, F1, F2, F3, F4, F5, F6, F7]   ## fp args + return (v0)
  IntRet*   = R0
  FloatRet* = F0
  IndirectResultReg* = R8

  ## Volatile (caller-saved) scratch usable for temporaries that do not
  ## need to survive a call. (x0–x7 are also volatile but reserved here for
  ## arg/return shuffling; x16/x17/x18 are reserved by the platform.)
  IntTempRegs* = [R9, R10, R11, R12, R13, R14, R15]
  ## Callee-saved: for values that must survive a call.
  IntCalleeSaved* = [R19, R20, R21, R22, R23, R24, R25, R26, R27, R28]
  ## Caller-saved set clobbered across any call (incl. an extcall).
  IntCallerSaved* = {R0..R17}
  ## Caller-saved SIMD/FP scratch: v16–v31 are fully caller-saved (and v0–v7
  ## are argument/return registers, also caller-saved). arkham keeps float
  ## values in these; a float that must survive a call (v8–v15 callee-saved)
  ## is not yet supported.
  FloatTempRegs* = [F16, F17, F18, F19, F20, F21, F22, F23,
                    F24, F25, F26, F27, F28, F29, F30, F31]
  ## Callee-saved (low 64 bits of v8–v15) — reserved for a future FP frame.
  FloatCalleeSaved* = [F8, F9, F10, F11, F12, F13, F14, F15]

  ## Never allocate: x16/x17 (IP0/IP1 veneers), x18 (Darwin platform reg),
  ## x8 (indirect result), fp/lr/sp.
  ReservedRegs* = {R8, R16, R17, R18, R29, R30, SP, NoReg}

  ## The GPRs a call clobbers under the C/AAPCS64 convention — the caller-saved
  ## volatiles arkham manages (args x0–x7 + temps x9–x15, and x8). Emitted as the
  ## proc's `(clobber …)` so the ABI is declared at the signature rather than
  ## re-derived; x16/x17 (assembler veneers) and x18 (platform) are excluded.
  ConvClobbersGpr* = [R0, R1, R2, R3, R4, R5, R6, R7, R8,
                      R9, R10, R11, R12, R13, R14, R15]

  ## The AArch64 / AAPCS64 register file and calling convention, as the
  ## arch-neutral register allocator consumes it.
  aarch64Machine* = MachineDesc(
    intArgRegs: @IntArgRegs,
    floatArgRegs: @FloatArgRegs,
    intTempRegs: @IntTempRegs,
    intCalleeSaved: @IntCalleeSaved,
    floatTempRegs: @FloatTempRegs,
    floatCalleeSaved: @FloatCalleeSaved,
    intCalleeSavedSet: {R19..R28},
    floatCalleeSavedSet: {F8..F15},
    aggrByRefThreshold: 16)

proc regName*(r: Reg): string =
  case r
  of SP: "sp"
  of NoReg: "<noreg>"
  else: "x" & $ord(r)

proc regName*(f: FReg): string =
  if f == NoFReg: "<nofreg>" else: "v" & $ord(f)

proc `$`*(loc: Location): string =
  case loc.kind
  of Undef: "undef"
  of InReg: regName(loc.r)
  of InFReg: regName(loc.f)
  of OnStack: "[fp," & $loc.offset & "]"
  of NamedStack: "&" & loc.name
  of Imm: "#" & $loc.ival

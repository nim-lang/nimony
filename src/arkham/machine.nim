#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## AArch64 / Darwin machine model: registers, the AAPCS64 register classes,
## and the typed `Location` abstraction the register allocator fills in.

import slots

type
  Reg* = enum   ## general-purpose registers; ord matches the hardware number
    X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15,
    X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30,
    SP, NoReg

  FReg* = enum  ## FP/SIMD registers
    V0, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15,
    V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31,
    NoFReg

const
  FP* = X29   ## frame pointer
  LR* = X30   ## link register

  # ── AAPCS64 / Darwin register classes ──────────────────────────────────
  IntArgRegs*   = [X0, X1, X2, X3, X4, X5, X6, X7]   ## int args + return (x0)
  FloatArgRegs* = [V0, V1, V2, V3, V4, V5, V6, V7]   ## fp args + return (v0)
  IntRet*   = X0
  FloatRet* = V0
  IndirectResultReg* = X8

  ## Volatile (caller-saved) scratch usable for temporaries that do not
  ## need to survive a call. (x0–x7 are also volatile but reserved here for
  ## arg/return shuffling; x16/x17/x18 are reserved by the platform.)
  IntTempRegs* = [X9, X10, X11, X12, X13, X14, X15]
  ## Callee-saved: for values that must survive a call.
  IntCalleeSaved* = [X19, X20, X21, X22, X23, X24, X25, X26, X27, X28]
  ## Caller-saved set clobbered across any call (incl. an extcall).
  IntCallerSaved* = {X0..X17}
  FloatTempRegs* = [V9, V10, V11, V12, V13, V14, V15]
  FloatCalleeSaved* = [V8, V9, V10, V11, V12, V13, V14, V15]

  ## Never allocate: x16/x17 (IP0/IP1 veneers), x18 (Darwin platform reg),
  ## x8 (indirect result), fp/lr/sp.
  ReservedRegs* = {X8, X16, X17, X18, X29, X30, SP, NoReg}

proc regName*(r: Reg): string =
  case r
  of SP: "sp"
  of NoReg: "<noreg>"
  else: "x" & $ord(r)

proc regName*(f: FReg): string =
  if f == NoFReg: "<nofreg>" else: "v" & $ord(f)

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

proc `$`*(loc: Location): string =
  case loc.kind
  of Undef: "undef"
  of InReg: regName(loc.r)
  of InFReg: regName(loc.f)
  of OnStack: "[fp," & $loc.offset & "]"
  of NamedStack: "&" & loc.name
  of Imm: "#" & $loc.ival

proc sameReg*(a, b: Location): bool {.inline.} =
  ## True if both name the same physical register (for move coalescing).
  (a.kind == InReg and b.kind == InReg and a.r == b.r) or
  (a.kind == InFReg and b.kind == InFReg and a.f == b.f)

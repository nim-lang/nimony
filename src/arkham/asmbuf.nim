#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## Enum-based builder for the typed asm-NIF that arkham feeds to `nifasm`.
##
## The asm-NIF tag vocabulary is **nifasm's own generated enums** (`A64Inst`
## for instructions/branches/`lab`/`stmts`, `NifasmDecl` for `proc`/`params`/
## `extproc`/… — both from nativenif's `model.nim`, generated from
## `doc/instructions.md`). Reusing them makes the assembler the single source
## of truth and compile-time-enforces that arkham only emits tags nifasm
## accepts. Registers use arkham's own `machine.Reg`, rendered via `regName`.
## Built as a `nifcore` `TokenBuf` (the flexible NIF API) and serialized with
## `toString`.

import std / tables
import nifcore, nifcoreparse
import model                 # nifasm: A64Inst, NifasmDecl, NifasmType, NifasmExpr
import machine               # arkham: Reg, regName
export A64Inst, NifasmDecl, NifasmType, NifasmExpr, X64Flag

type
  AsmBuf* = object
    buf: TokenBuf
    ids: Table[string, TagId]   ## spelling → interned tag id (cache)

proc initAsmBuf*(): AsmBuf =
  AsmBuf(buf: createTokenBuf(256), ids: initTable[string, TagId]())

proc openS(a: var AsmBuf; spelling: string) {.inline.} =
  a.buf.openTag a.ids.mgetOrPut(spelling, a.buf.tags.registerTag(spelling))

proc open*[T: enum](a: var AsmBuf; t: T) {.inline.} = a.openS($t)
proc close*(a: var AsmBuf) {.inline.} = a.buf.closeTag()

template tree*[T: enum](a: var AsmBuf; t: T; body: untyped) =
  ## Open a tagged node named after the enum value's spelling (`$t`), run
  ## `body` to emit its children, then close. Use with `A64Inst`/`NifasmDecl`.
  a.openS($t)
  body
  a.close()

proc keyword*[T: enum](a: var AsmBuf; t: T) {.inline.} =
  ## A childless tag, e.g. `(extcall)` / `(params)`.
  a.openS($t); a.close()

proc reg*(a: var AsmBuf; r: Reg) {.inline.} =
  ## A register operand `(xN)` — a childless tag named after the register.
  a.openS(regName r); a.close()

proc dreg*(a: var AsmBuf; f: FReg) {.inline.} =
  ## A double-precision fp register operand `(dN)` (the 64-bit view of `vN`).
  a.openS("d" & $ord(f)); a.close()

proc sreg*(a: var AsmBuf; f: FReg) {.inline.} =
  ## A single-precision fp register operand `(sN)` (the 32-bit view of `vN`).
  a.openS("s" & $ord(f)); a.close()

proc freg*(a: var AsmBuf; f: FReg; bits: int) {.inline.} =
  ## An fp register operand sized by `bits` (32 → `(sN)`, else `(dN)`).
  if bits == 32: a.sreg f else: a.dreg f

proc sym*(a: var AsmBuf; s: string) {.inline.} = a.buf.addSymUse s     # use
proc symDef*(a: var AsmBuf; s: string) {.inline.} = a.buf.addSymDef s  # :def
proc str*(a: var AsmBuf; s: string) {.inline.} = a.buf.addStrLit s
proc intLit*(a: var AsmBuf; v: int64) {.inline.} = a.buf.addIntLit v
proc ident*(a: var AsmBuf; s: string) {.inline.} = a.buf.addIdent s

# ── type emission (NifasmType tags) ─────────────────────────────────────────
# Kept here (not in codegen) because nifasm's NifasmType shares spellings with
# nimony's NifcType — referencing the nifasm enum values is unambiguous only in
# this module, which doesn't import nifcdecl.

proc intType*(a: var AsmBuf; bits: int) = a.tree IT: a.intLit bits
proc uintType*(a: var AsmBuf; bits: int) = a.tree UT: a.intLit bits
proc charType*(a: var AsmBuf; bits: int) = a.tree CT: a.intLit bits
proc floatType*(a: var AsmBuf; bits: int) = a.tree FT: a.intLit bits
proc boolType*(a: var AsmBuf) = a.keyword BoolT

template objectType*(a: var AsmBuf; body: untyped) = a.tree ObjectT: body
template ptrType*(a: var AsmBuf; body: untyped) = a.tree PtrT: body
template arrayType*(a: var AsmBuf; body: untyped) = a.tree ArrayT: body
template fldDef*(a: var AsmBuf; name: string; body: untyped) =
  a.openS($FldT)
  a.symDef name
  body
  a.close()

# ── inline assembler (NIF-text fragments) ───────────────────────────────────
# NIF text *is* the DSL: rather than invent a separate assembly syntax, a
# multi-instruction lowering can be written as the very asm-NIF nifasm already
# consumes, parsed and spliced straight into the buffer. Operands are filled in
# by the caller with ordinary string building (`regName r`, `$bits`, …) — e.g.
#   g.ab.splice "(fmov (" & regName tmp & ") (d0)) (lsr (x0) (x0) 32)"
# This is the analogue of TCC's inline-asm escape hatch, but with zero new
# grammar and no runtime/helper-call dependency (the bytes stay inline, so the
# linker/DCE story is unchanged).

proc splice*(a: var AsmBuf; nifText: string) =
  ## Parse one or more sibling asm-NIF nodes from `nifText` and append them to
  ## the buffer. The fragment uses nifasm's tag vocabulary and arkham's register
  ## spellings (`(fadd (d0) (d1) (d2))`, `(x9)`, …) — i.e. exactly what `render`
  ## would have produced via the enum builder. Parsing shares the buffer's pool
  ## and tag namespace, so splicing is a bulk copy (no re-interning).
  var frag = parseFromBuffer("(stmts " & nifText & ")", "arkham.inline",
                             sizeHint = 32, sharedPool = a.buf.pool,
                             sharedTags = a.buf.tags)
  var c = frag.beginRead()                   # at the throwaway `(stmts …)` wrapper
  c.into:                                    # splice only its children
    while c.hasMore:
      a.buf.addSubtree c
      skip c

proc render*(a: var AsmBuf): string =
  ## Serialize to NIF text (with the `(.nif27)` header) for nifasm.
  "(.nif27)\n" & toString(a.buf)

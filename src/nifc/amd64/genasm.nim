#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# We produce PreASM code as a list of NIF tokens.

import std / [assertions, syncio, tables, sets, intsets, strutils]
from std / os import changeFileExt, splitFile, extractFilename

import .. / .. / lib / [bitabs, packedtrees, lineinfos, nifstreams, nifcursors]
import ".." / [nifc_model, typenav]
import ".." / native / [slots, analyser]
import asm_model, machine, emitter

type
  Label = distinct int
  TempVar = distinct int

type
  Scope = object
    vars: seq[LitId]

  GeneratedCode* = object
    m: Module
    data: TokenBuf
    code: TokenBuf
    init: TokenBuf
    rega: RegAllocator
    intmSize, inConst, labels, prologAt: int
    loopExits: seq[Label]
    generatedTypes: IntSet
    requestedSyms: HashSet[string]
    fields: Table[LitId, AsmSlot]
    types: Table[LitId, AsmSlot]
    locals: Table[LitId, Location]
    strings: Table[string, int]
    floats: Table[LitId, int]
    scopes: seq[Scope]
    returnLoc: Location
    exitProcLabel: Label
    globals: Table[LitId, Location]

  LitId = nifc_model.StrId

proc initGeneratedCode*(m: sink Module; intmSize: int): GeneratedCode =
  result = GeneratedCode(m: m, intmSize: intmSize)

proc error(m: Module; msg: string; tree: PackedTree[NifcKind]; n: NodePos) {.noreturn.} =
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(tree, n, m)
  when defined(debug):
    writeStackTrace()
  quit 1

# Atoms

proc genIntLit(c: var GeneratedCode; litId: LitId; info: PackedLineInfo) =
  c.code.addIntLit parseBiggestInt(c.m.lits.strings[litId]), info

proc genIntLit(c: var GeneratedCode; i: BiggestInt; info: PackedLineInfo) =
  c.code.addIntLit i, info

proc genIntLit(c: var TokenBuf; i: BiggestInt; info: PackedLineInfo) =
  c.addIntLit i, info

proc genUIntLit(c: var GeneratedCode; litId: LitId; info: PackedLineInfo) =
  let i = parseBiggestUInt(c.m.lits.strings[litId])
  let id = pool.uintegers.getOrIncl(i)
  c.code.add uintToken(id, info)

proc genUIntLit(c: var GeneratedCode; i: BiggestUInt; info: PackedLineInfo) =
  let id = pool.uintegers.getOrIncl(i)
  c.code.add uintToken(id, info)

proc genFloatLit(c: var GeneratedCode; litId: LitId; info: PackedLineInfo) =
  let i = parseFloat(c.m.lits.strings[litId])
  let id = pool.floats.getOrIncl(i)
  c.code.add floatToken(id, info)

proc genFloatLit(c: var GeneratedCode; i: float; info: PackedLineInfo) =
  let id = pool.floats.getOrIncl(i)
  c.code.add floatToken(id, info)

proc genCharLit(c: var GeneratedCode; ch: char; info: PackedLineInfo) =
  c.code.add charToken(ch, info)

proc addIdent(c: var GeneratedCode; s: string; info: PackedLineInfo) =
  c.code.add identToken(pool.strings.getOrIncl(s), info)

proc addEmpty(c: var GeneratedCode; info: PackedLineInfo) =
  c.code.add dotToken(info)

proc addKeyw(c: var GeneratedCode; keyw: TagId; info = NoLineInfo) =
  c.code.buildTree keyw, info: discard

proc addKeywUnchecked(c: var GeneratedCode; keyw: string; info = NoLineInfo) =
  c.code.buildTree pool.tags.getOrIncl(keyw), info: discard

proc addSymDef(c: var TokenBuf; s: string; info: PackedLineInfo) =
  c.add symdefToken(pool.syms.getOrIncl(s), info)

proc addStrLit(c: var TokenBuf; s: string; info: PackedLineInfo) =
  c.addStrLit s, info

proc addSym(c: var GeneratedCode; s: string; info: PackedLineInfo) =
  c.code.add symToken(pool.syms.getOrIncl(s), info)

proc getLabel(c: var GeneratedCode): Label =
  result = Label(c.labels)
  inc c.labels

proc useLabel(c: var GeneratedCode; lab: Label; info: PackedLineInfo) =
  c.addSym "L." & $int(lab), info

proc defineTemp(c: var GeneratedCode; tmp: TempVar; info: PackedLineInfo) =
  c.code.addSymDef "v." & $int(tmp), info

proc useTemp(c: var GeneratedCode; tmp: TempVar; info: PackedLineInfo) =
  c.addSym "v." & $int(tmp), info

template buildTree(c: var GeneratedCode; keyw: TagId; body: untyped) =
  c.code.buildTree keyw, NoLineInfo:
    body

template buildTreeI(c: var GeneratedCode; keyw: TagId; info: PackedLineInfo; body: untyped) =
  c.code.buildTree keyw, info:
    body

proc defineLabel(c: var GeneratedCode; lab: Label; info: PackedLineInfo; opc = LabT) =
  c.code.buildTree opc, info:
    c.code.addSymDef "L." & $int(lab), info

# Type graph

include ".." / preasm / genpreasm_t

# Procs

proc genWas(c: var GeneratedCode; t: Tree; ch: NodePos) =
  c.code.buildTree(CommentT, t[ch].info):
    c.addIdent toString(t, ch.firstSon, c.m), t[ch].info

type
  ProcFlag = enum
    isSelectAny, isVarargs

proc genProcPragmas(c: var GeneratedCode; t: Tree; n: NodePos;
                    flags: var set[ProcFlag]) =
  # ProcPragma ::= (inline) | (noinline) | CallingConvention | (varargs) | (was Identifier) |
  #               (selectany) | Attribute
  if t[n].kind == Empty:
    discard
  elif t[n].kind == PragmasC:
    for ch in sons(t, n):
      case t[ch].kind
      of CdeclC, StdcallC, NoconvC: discard "supported calling convention"
      of SafecallC, SyscallC, FastcallC, ThiscallC, MemberC:
        error c.m, "unsupported calling convention: ", t, ch
      of VarargsC:
        flags.incl isVarargs
      of SelectanyC:
        flags.incl isSelectAny
      of InlineC, AttrC, NoinlineC:
        # Ignore for PreASM
        discard " __attribute__((noinline))"
      of WasC: genWas(c, t, ch)
      of RaiseC, ErrsC: discard
      else:
        error c.m, "invalid proc pragma: ", t, ch
  else:
    error c.m, "expected proc pragmas but got: ", t, n

proc genSymDef(c: var GeneratedCode; t: Tree; n: NodePos): string =
  if t[n].kind == SymDef:
    let lit = t[n].litId
    result = c.m.lits.strings[lit]
    c.code.addSymDef result, t[n].info
  else:
    error c.m, "expected SymbolDef but got: ", t, n
    result = ""

proc genParamPragmas(c: var GeneratedCode; t: Tree; n: NodePos) =
  # ProcPragma ::= (was Identifier) | Attribute
  if t[n].kind == Empty:
    discard
  elif t[n].kind == PragmasC:
    for ch in sons(t, n):
      case t[ch].kind
      of AttrC:
        discard "Ignore for now"
      of WasC:
        genWas c, t, ch
      else:
        error c.m, "invalid pragma: ", t, ch
  else:
    error c.m, "expected pragmas but got: ", t, n

proc genVarPragmas(c: var GeneratedCode; t: Tree; n: NodePos; alignOverride: var int) =
  if t[n].kind == Empty:
    discard
  elif t[n].kind == PragmasC:
    for ch in sons(t, n):
      case t[ch].kind
      of AlignC:
        let intId = t[ch.firstSon].litId
        alignOverride = parseInt(c.m.lits.strings[intId])
      of AttrC:
        discard "ignore attribute"
      of WasC:
        genWas c, t, ch
      else:
        error c.m, "invalid pragma: ", t, ch
  else:
    error c.m, "expected pragmas but got: ", t, n

include genasm_e

template moveToDataSection(body: untyped) =
  let oldLen = c.code.len
  body
  for i in oldLen ..< c.code.len:
    c.data.add c.code[i]
  shrink c.code, oldLen

include register_allocator
include genasm_s

proc genProcDecl(c: var GeneratedCode; t: Tree; n: NodePos) =
  c.labels = 0 # reset so that we produce nicer code
  c.exitProcLabel = Label(-1)
  let prc = asProcDecl(t, n)
  if t[prc.body].kind == Empty: return # ignore procs without body
  # (proc SYMBOLDEF Params Type ProcPragmas (OR . StmtList)
  c.openScope() # open scope for the parameters
  c.rega = initRegAllocator()
  c.buildTreeI TextT, t[n].info:
    discard genSymDef(c, t, prc.name)

    c.genProlog()

    if t[prc.returnType].kind != VoidC:
      let returnSlot = typeToSlot(c, prc.returnType)
      allocResultWin64 c.rega, returnSlot, c.returnLoc

    if t[prc.params].kind != Empty:
      var paramTypes: seq[AsmSlot] = @[]
      var paramLocs: seq[Location] = @[]
      for ch in sons(t, prc.params):
        let d = asParamDecl(t, n)
        if t[d.name].kind == SymDef:
          paramTypes.add typeToSlot(c, d.typ)
          paramLocs.add Location(kind: Undef)
        else:
          error c.m, "expected SymbolDef but got: ", t, n
      allocParamsWin64 c.rega, paramTypes, paramLocs
      var i = 0
      for ch in sons(t, prc.params):
        let d = asParamDecl(t, n)
        if t[d.name].kind == SymDef:
          let lit = t[d.name].litId
          c.locals[lit] = paramLocs[i]
          inc i

    var flags: set[ProcFlag] = {}
    genProcPragmas c, t, prc.pragmas, flags
    allocateVars c, t, prc.body
    genStmt c, t, prc.body
    if c.exitProcLabel.int >= 0:
      c.defineLabel(c.exitProcLabel, t[n].info)
    c.genEpilog()
    c.fixupProlog()
  c.closeScope() # close parameter scope

proc genToplevel(c: var GeneratedCode; t: Tree; n: NodePos) =
  # ExternDecl ::= (imp ProcDecl | VarDecl | ConstDecl)
  # Include ::= (incl StringLiteral)
  # TopLevelConstruct ::= ExternDecl | ProcDecl | VarDecl | ConstDecl |
  #                       TypeDecl | Include | EmitStmt
  case t[n].kind
  of ImpC: discard "ignore imp"
  of NodeclC: discard "ignore nodecl"
  of InclC: discard "genInclude c, t, n"
  of ProcC: genProcDecl c, t, n
  of VarC: genStmt c, t, n
  of ConstC: genStmt c, t, n
  of TypeC: discard "handled in a different pass"
  of EmitC: genEmitStmt c, t, n
  else:
    error c.m, "expected top level construct but got: ", t, n

proc traverseCode(c: var GeneratedCode; t: Tree; n: NodePos) =
  case t[n].kind
  of StmtsC:
    for ch in sons(t, n): genToplevel(c, t, ch)
  else:
    error c.m, "expected `stmts` but got: ", t, n

proc generateAsm*(inp, outp: string) =
  registerTags()
  var c = initGeneratedCode(load(inp), 8)

  var co = TypeOrder()
  traverseTypes(c.m, co)

  generateTypes(c, co)

  traverseCode c, c.m.code, StartPos
  var f = ""
  f.add "(.nif24)\n(stmts"
  f.add toString(c.data)
  f.add toString(c.code)
  f.add ")\n"

  when defined(debug):
    echo f

  if c.init.len > 0:
    quit "no init code implemented"
  produceAsmCode f, outp

when isMainModule:
  # How to test following code:
  # Uncomment registerTags() in produceAsmCode proc in emitter.nim.
  # Produce foobar.s with: nim c -r genasm.nim
  # Assemble and link it with: goo -o foobar foobar.s

  const
    CallPrintf = when defined(windows): """
 (lea
  (rcx)
  (rel str.1))
 (lea
  (rdx)
  (rel str.2))
 (call printf.c)
 (lea
  (rcx)
  (rel str.3))
 (mov
  (rdx) +12)
 (call printf.c)
 (lea
  (rcx)
  (rel str.3))
 (mov
  (rdx)
  (rbx))
 (call printf.c)"""
                 else: """
 (lea
  (rdi)
  (rel str.1))
 (lea
  (rsi)
  (rel str.2))
 (call printf.c)
 (lea
  (rdi)
  (rel str.3))
 (mov
  (rsi) +12)
 (call printf.c)
 (lea
  (rdi)
  (rel str.3))
 (mov
  (rsi)
  (rbx))
 (call printf.c)"""

    TestCode = """(.nif24)
(stmts
(rodata
  :str.1 (string "str.1 %s\0A")
  :str.2 (string "str.2 ")
  :str.3 (string "str.3 value=%d\0A") )
(global main.c)
(text :main.c
 (push (rbp))
 (mov (rbp) (rsp))
 (mov
  (rbx) +12) """ & CallPrintf & """
 (mov
  (rax) +0)
 (jmp L.0)
 (lab :L.0)
 (mov (rsp) (rbp))
 (pop (rbx))
 (ret)))"""

  produceAsmCode TestCode, "foobar.s"

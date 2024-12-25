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
from std / os import changeFileExt, splitFile, extractFileName

import .. / .. / lib / [bitabs, packedtrees, lineinfos, nifstreams, nifcursors]
import ".." / [nifc_model, typenav]
import ".." / native / slots
import preasm_model

type
  Label = distinct int
  TempVar = distinct int

type
  Scope = object
    # usedRegisters: RegisterSet # XXX later once we have a `frame` abstraction
    # usedMem: (int, int)
    syms: Table[string, AsmSlot]
  GeneratedCode* = object
    m: Module
    data: TokenBuf
    code: TokenBuf
    init: TokenBuf
    intmSize, inConst, stmtBegin, labels, temps: int
    loopExits: seq[Label]
    generatedTypes: IntSet
    requestedSyms: HashSet[string]
    fields: Table[LitId, AsmSlot]
    types: Table[LitId, AsmSlot]
    strings: Table[string, int]
    scopes: seq[Scope]
    threadLocalsSize, globalsSize: int
    globals: Table[string, AsmSlot]

  LitId = nifc_model.StrId

proc openScope(c: var GeneratedCode) =
  c.scopes.add Scope()

proc closeScope(c: var GeneratedCode) =
  discard c.scopes.pop()

proc initGeneratedCode*(m: sink Module; intmSize: int): GeneratedCode =
  result = GeneratedCode(m: m, intmSize: intmSize)

proc error(m: Module; msg: string; tree: PackedTree[NifcKind]; n: NodePos) {.noreturn.} =
  when defined(debug):
    writeStackTrace()
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(tree, n, m)
  quit 1

# Atoms

proc genIntLit(c: var GeneratedCode; litId: LitId; info: PackedLineInfo) =
  let i = parseBiggestInt(c.m.lits.strings[litId])
  let id = pool.integers.getOrIncl(i)
  c.code.add intToken(id, info)

proc genIntLit(c: var GeneratedCode; i: BiggestInt; info: PackedLineInfo) =
  let id = pool.integers.getOrIncl(i)
  c.code.add intToken(id, info)

proc genUIntLit(c: var GeneratedCode; litId: LitId; info: PackedLineInfo) =
  let i = parseBiggestUInt(c.m.lits.strings[litId])
  let id = pool.uintegers.getOrIncl(i)
  c.code.add uintToken(id, info)

proc genFloatLit(c: var GeneratedCode; litId: LitId; info: PackedLineInfo) =
  let i = parseFloat(c.m.lits.strings[litId])
  let id = pool.floats.getOrIncl(i)
  c.code.add floatToken(id, info)

proc genCharLit(c: var GeneratedCode; ch: char; info: PackedLineInfo) =
  c.code.add charToken(ch, info)

proc addIdent(c: var GeneratedCode; s: string; info: PackedLineInfo) =
  c.code.add identToken(pool.strings.getOrIncl(s), info)

proc addEmpty(c: var GeneratedCode; info: PackedLineInfo) =
  c.code.add dotToken(info)

proc addKeyw(c: var GeneratedCode; keyw: TagId; info: PackedLineInfo) =
  c.code.buildTree keyw, info: discard

proc addSymDef(c: var TokenBuf; s: string; info: PackedLineInfo) =
  c.add symdefToken(pool.syms.getOrIncl(s), info)

proc addStrLit(c: var TokenBuf; s: string; info: PackedLineInfo) =
  c.add strToken(pool.strings.getOrIncl(s), info)

proc addSym(c: var GeneratedCode; s: string; info: PackedLineInfo) =
  c.code.add symToken(pool.syms.getOrIncl(s), info)

proc getLabel(c: var GeneratedCode): Label =
  result = Label(c.labels)
  inc c.labels

proc getTempVar(c: var GeneratedCode): TempVar =
  result = TempVar(c.temps)
  inc c.temps

proc defineLabel(c: var GeneratedCode; lab: Label; info: PackedLineInfo) =
  c.code.addSymDef "L." & $int(lab), info

proc useLabel(c: var GeneratedCode; lab: Label; info: PackedLineInfo) =
  c.addSym "L." & $int(lab), info

proc defineTemp(c: var GeneratedCode; tmp: TempVar; info: PackedLineInfo) =
  c.code.addSymDef "v." & $int(tmp), info

proc useTemp(c: var GeneratedCode; tmp: TempVar; info: PackedLineInfo) =
  c.addSym "v." & $int(tmp), info

template buildTree(c: var GeneratedCode; keyw: TagId; info: PackedLineInfo; body: untyped) =
  c.code.buildTree keyw, info:
    body

# Type graph

include genpreasm_t

# Procs
proc genSlot(c: var GeneratedCode; dest: AsmSlot; info: PackedLineInfo) =
  let tag =
    case dest.kind
    of ABool: BT
    of AInt: IT
    of AUInt: UT
    of AFloat: FT
    of AMem: MT

  c.buildTree tag, info:
    if tag != BT:
      c.genIntLit dest.size*8, info
      if dest.align != dest.size:
        c.genIntLit dest.align*8, info

proc genType(c: var GeneratedCode; n: NodePos; alignOverride = -1) =
  var dest = AsmSlot()
  fillTypeSlot c, typeFromPos(n), dest
  if alignOverride >= 0:
    dest.align = alignOverride
  genSlot c, dest, c.m.code[n].info

proc genTypeof(c: var GeneratedCode; n: NodePos) =
  let t = getType(c.m, c.m.code, n)
  if isError(t):
    error c.m, "cannot compute type of expression: ", c.m.code, n
  else:
    var dest = AsmSlot()
    fillTypeSlot c, t, dest
    genSlot c, dest, c.m.code[n].info

proc genWas(c: var GeneratedCode; t: Tree; ch: NodePos) =
  c.code.buildTree(WasT, t[ch].info):
    c.code.add identToken(pool.strings.getOrIncl(toString(t, ch.firstSon, c.m)), t[ch].info)

type
  ProcFlag = enum
    isSelectAny, isVarargs

proc genProcPragmas(c: var GeneratedCode; t: Tree; n: NodePos;
                    flags: var set[ProcFlag]) =
  # ProcPragma ::= (inline) | (noinline) | CallingConvention | (varargs) | (was Identifier) |
  #               (selectany) | Attribute
  if t[n].kind == Empty:
    c.addEmpty t[n].info
  elif t[n].kind == PragmasC:
    c.buildTree PragmasT, t[n].info:
      for ch in sons(t, n):
        case t[ch].kind
        of CdeclC: c.addKeyw CdeclT, t[n].info
        of StdcallC: c.addKeyw StdcallT, t[n].info
        of SafecallC: c.addKeyw SafecallT, t[n].info
        of SyscallC: c.addKeyw SyscallT, t[n].info
        of FastcallC: c.addKeyw FastcallT, t[n].info
        of ThiscallC: c.addKeyw ThiscallT, t[n].info
        of NoconvC: c.addKeyw NoconvT, t[n].info
        of MemberC: c.addKeyw MemberT, t[n].info
        of VarargsC:
          flags.incl isVarargs
        of SelectanyC:
          flags.incl isSelectAny
        of InlineC, AttrC, NoinlineC:
          # Ignore for PreASM
          discard " __attribute__((noinline))"
        of WasC: genWas(c, t, ch)
        of RaiseC, ErrsC:
          discard
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
    c.addEmpty t[n].info
  elif t[n].kind == PragmasC:
    c.buildTree PragmasT, t[n].info:
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

proc genParam(c: var GeneratedCode; t: Tree; n: NodePos) =
  let d = asParamDecl(t, n)
  if t[d.name].kind == SymDef:
    c.buildTree ParamT, t[n].info:
      let lit = t[d.name].litId
      c.code.addSymDef c.m.lits.strings[lit], t[d.name].info
      genType c, d.typ
      genParamPragmas c, t, d.pragmas
  else:
    error c.m, "expected SymbolDef but got: ", t, n

proc genVarPragmas(c: var GeneratedCode; t: Tree; n: NodePos; alignOverride: var int) =
  if t[n].kind == Empty:
    c.addEmpty t[n].info
  elif t[n].kind == PragmasC:
    c.buildTree PragmasT, t[n].info:
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

include genpreasm_e

type
  VarKind = enum
    IsLocal, IsGlobal, IsThreadlocal

proc allocGlobal(size: var int; dest: var AsmSlot) =
  size = align(size, dest.align)
  dest.offset = size
  inc size, dest.size

proc genVarDecl(c: var GeneratedCode; t: Tree; n: NodePos; vk: VarKind) =
  let d = asVarDecl(t, n)
  if t[d.name].kind == SymDef:
    let lit = t[d.name].litId
    let name = c.m.lits.strings[lit]
    let opc =
      case vk
      of IsLocal: VarT
      of IsGlobal: GvarT
      of IsThreadlocal: TvarT
    c.buildTree opc, t[n].info:
      c.code.addSymDef name, t[d.name].info
      var alignOverride = -1
      genVarPragmas c, t, d.pragmas, alignOverride

      # genType inlined:
      var dest = AsmSlot()
      fillTypeSlot c, typeFromPos(d.typ), dest
      if alignOverride >= 0:
        dest.align = alignOverride
      genSlot c, dest, c.m.code[n].info
    case vk
    of IsLocal:
      c.scopes[c.scopes.high].syms[name] = dest
    of IsGlobal:
      allocGlobal c.globalsSize, dest
      c.globals[name] = dest
    of IsThreadlocal:
      allocGlobal c.threadLocalsSize, dest
      c.globals[name] = dest

    if t[d.value].kind != Empty:
      c.buildTree AsgnT, t[d.value].info:
        genType c, d.typ, alignOverride
        c.addSym name, t[d.name].info
        genx c, t, d.value, WantValue
  else:
    error c.m, "expected SymbolDef but got: ", t, n

proc genConstDecl(c: var GeneratedCode; t: Tree; n: NodePos) =
  let d = asVarDecl(t, n)
  if t[d.name].kind == SymDef:
    let lit = t[d.name].litId
    let name = c.m.lits.strings[lit]

    c.buildTree ConstT, t[n].info:
      c.code.addSymDef name, t[d.name].info
      var alignOverride = -1
      genVarPragmas c, t, d.pragmas, alignOverride
      genType c, d.typ, alignOverride

      if t[d.value].kind != Empty:
        c.buildTree ValuesT, t[d.value].info:
          inc c.inConst
          genx c, t, d.value, WantValue
          dec c.inConst
      else:
        error c.m, "const needs a value: ", t, n
  else:
    error c.m, "expected SymbolDef but got: ", t, n

template moveToDataSection(body: untyped) =
  let oldLen = c.code.len
  body
  for i in oldLen ..< c.code.len:
    c.data.add c.code[i]
  shrink c.code, oldLen

include genpreasm_s

proc genProcDecl(c: var GeneratedCode; t: Tree; n: NodePos) =
  c.labels = 0 # reset so that we produce nicer code
  c.temps = 0
  let prc = asProcDecl(t, n)
  if t[prc.body].kind == Empty: return # ignore procs without body
  # (proc SYMBOLDEF Params Type ProcPragmas (OR . StmtList)
  c.buildTree ProcT, t[n].info:
    discard genSymDef(c, t, prc.name)

    if t[prc.params].kind != Empty:
      c.buildTree ParamsT, t[prc.params].info:
        for ch in sons(t, prc.params):
          genParam c, t, ch
    else:
      c.addEmpty t[prc.params].info
    if t[prc.returnType].kind == VoidC:
      c.addEmpty t[prc.returnType].info
    else:
      genType c, prc.returnType

    var flags: set[ProcFlag] = {}
    genProcPragmas c, t, prc.pragmas, flags
    genStmt c, t, prc.body

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

proc generatePreAsm*(inp, outp: string; intmSize: int) =
  registerTags()
  var c = initGeneratedCode(load(inp), intmSize)

  var co = TypeOrder()
  traverseTypes(c.m, co)

  generateTypes(c, co)

  traverseCode c, c.m.code, StartPos
  var f = open(outp, fmWrite)
  f.write "(.nif24)\n(stmts"
  f.write toString(c.data)
  f.write toString(c.code)
  f.write ")\n"
  if c.init.len > 0:
    quit "no init code implemented"
  f.close

#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# We produce C code as a list of tokens.

when defined(nimony):
  # `moveToDataSection` & co. are `.dirty` templates whose bodies reach for the
  # caller's `c`; sem needs the feature to leave those idents untyped until the
  # expansion site (see doc/porting.md, "Dirty templates").
  {.feature: "untyped".}

import std / [assertions, syncio, tables, sets, intsets, formatfloat, packedsets]
from std / syncio import readFile, writeFile
from std / os import changeFileExt, splitFile, extractFilename, fileExists
import ".." / lib / vfs
from std / sequtils import insert

import ".." / lib / nifcoreparse   # re-exports nifcore
import ".." / lib / nifcdecl        # leng_model replacement (stmtKind/decls/tags)
import ".." / lib / intrinsics      # the shared `{.instruction.}` / `{.intrinsic.}` table
import mangler
import cprelude
import noptions
import ".." / lib / symparser
import typenav, nifmodules                 # nifcore MainModule + getType (local)

type
  Token = distinct uint32

proc `==`(a, b: Token): bool {.borrow.}

type
  PredefinedToken = enum
    IgnoreMe = "<unused>"
    EmptyToken = ""
    CurlyLe = "{"
    CurlyRi = "}"
    ParLe = "("
    ParRi = ")"
    BracketLe = "["
    BracketRi = "]"
    NewLine = "\n"
    Semicolon = ";"
    Comma = ", "
    Space = " "
    Colon = ": "
    Dot = "."
    Arrow = "->"
    Star = "*"
    Amp = "&"
    DoubleQuote = "\""
    AsgnOpr = " = "
    ScopeOpr = "::"
    ConstKeyword = "const "
    StaticKeyword = "static "
    ExternKeyword = "extern "
    WhileKeyword = "while "
    GotoKeyword = "goto "
    IfKeyword = "if "
    ElseKeyword = "else "
    SwitchKeyword = "switch "
    CaseKeyword = "case "
    DefaultKeyword = "default:"
    BreakKeyword = "break"
    NullPtr = "NIM_NIL"
    ReturnKeyword = "return"
    TypedefStruct = "typedef struct "
    TypedefUnion = "typedef union "
    TypedefKeyword = "typedef "
    IncludeKeyword = "#include "
    LineDirKeyword = "#line "
    DiscardToken = "(void) "
    TryKeyword = "try "
    CatchKeyword = "catch ("
    ThrowKeyword = "throw"
    ErrToken = "LENGC_ERR_"
    OvfToken = "LENGC_OVF_"
    ThreadVarToken = "NIM_THREADVAR "
    AnonStruct = "struct "
    AnonUnion = "union "

# nifcore compatibility shims: the nifcursors world had a global `pool` and
# packed line info; here a StrLit carries its StrId in its own buffer's pool,
# and line info is a plain NifLineInfo on the cursor.
proc litId(c: Cursor): StrId {.inline.} = strId(c)
proc info(c: Cursor): NifLineInfo {.inline.} = rawLineInfo(c)
proc firstSon(c: Cursor): Cursor {.inline.} =
  result = c
  inc result
proc toString(c: Cursor; spaces: bool): string =
  ## nifcore render shim for the nifcursors `toString(Cursor, bool)`.
  var buf = createTokenBuf(8, c.pool, c.tags)
  buf.addSubtree c
  result = nifcoreparse.toString(buf)

proc fillTokenTable(tab: var BiTable[Token, string]) =
  for e in EmptyToken..high(PredefinedToken):
    let id = tab.getOrIncl $e
    assert id == Token(e), $uint32(id) & " " & $ord(e)

type
  GenFlag* = enum
    gfMainModule # isMainModule
    gfHasError   # already generated the error variable
    gfInCallImportC  # in importC call context
    gfInFlexArray    # initializing a flexible-array-member field (suppress NC8* cast)

  CurrentProc* = object
    needsOverflowFlag: bool
    nextTemp: int
    vflags: HashSet[SymId] # name and label token's position in the produced code

  GeneratedCode* = object
    m: MainModule
    includes: seq[Token]
    includedHeaders: IntSet
    protos: seq[Token]
    data: seq[Token]
    code: seq[Token]
    init: seq[Token]
    fileIds: PackedSet[FileId]
    tokens: BiTable[Token, string]
    headerFile: seq[Token]
    generatedTypes: HashSet[SymId]
    requestedSyms: HashSet[SymId]
    flags: set[GenFlag]
    inToplevel: bool
    objConstrNeedsType: bool
    bits: int
    currentProc: CurrentProc

proc initGeneratedCode*(m: sink MainModule, flags: set[GenFlag]; bits: int): GeneratedCode =
  result = GeneratedCode(m: m, code: @[], tokens: initBiTable[Token, string](),
      fileIds: initPackedSet[FileId](), flags: flags, inToplevel: true,
      objConstrNeedsType: true, bits: bits)
  fillTokenTable(result.tokens)

proc add*(c: var GeneratedCode; t: PredefinedToken) {.inline.} =
  c.code.add Token(t)

proc add*(c: var GeneratedCode; s: string) {.inline.} =
  c.code.add c.tokens.getOrIncl(s)

type
  CppFile = object
    buf: string  # write to buffer, then writeFileIfChanged at end

proc write(f: var CppFile; s: string) = f.buf.add s
proc write(f: var CppFile; c: char) = f.buf.add c

proc writeTokenSeq(f: var CppFile; s: seq[Token]; c: GeneratedCode) =
  var indent = 0
  for i in 0..<s.len:
    let x = s[i]
    case x
    of Token(CurlyLe):
      inc indent
      write f, c.tokens[x]
      write f, "\n"
      for i in 1..indent*2: write f, ' '
    of Token(CurlyRi):
      dec indent
      write f, c.tokens[x]
      if i+1 < s.len and s[i+1] == Token(CurlyRi):
        discard
      else:
        write f, "\n"
        for i in 1..indent*2: write f, ' '
    of Token(Semicolon):
      write f, c.tokens[x]
      if i+1 < s.len and s[i+1] == Token(CurlyRi):
        discard "no newline before }"
      else:
        write f, "\n"
        for i in 1..indent*2: write f, ' '
    of Token(NewLine):
      write f, c.tokens[x]
      for i in 1..indent*2: write f, ' '
    else:
      write f, c.tokens[x]

proc render(m: MainModule; n: Cursor): string =
  var buf = createTokenBuf(8, m.pool, m.tags)
  buf.addSubtree n
  result = toString(buf)

proc errorAt(m: MainModule; msg: string; n: Cursor) {.noreturn.} =
  ## `error` without the trailing render of `n`. For a message that already names
  ## what is wrong in prose: rendering the node would append its raw NIF spelling
  ## (mangled symbol plus embedded line info), which says nothing a reader wants.
  let info = rawLineInfo(n)
  if info.isValid:
    write stdout, m.pool.filenames[info.file]
    write stdout, "(" & $info.line & ", " & $(info.col+1) & ") "
  # `Error: `, not the `[Error] ` of the rendering `error` above: this is a
  # user-facing diagnostic, and that is the spelling every other user-facing
  # nimony error uses (the test harness keys the expected exit code off it).
  write stdout, "Error: "
  writeLine stdout, msg
  when defined(debug):
    echo getStackTrace()
  quit 1

proc error(m: MainModule; msg: string; n: Cursor) {.noreturn.} =
  let info = rawLineInfo(n)
  if info.isValid:
    write stdout, m.pool.filenames[info.file]
    write stdout, "(" & $info.line & ", " & $(info.col+1) & ") "
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, render(m, n)
  when defined(debug):
    echo getStackTrace()
  quit 1

# Atoms

proc genIntLit(c: var GeneratedCode; i: int64) =
  if i > low(int32) and i <= high(int32) and c.bits != 64:
    c.add $i
  elif i == low(int32) and c.bits != 64:
    # Nim has the same bug for the same reasons :-)
    c.add "(-2147483647 -1)"
  elif i > low(int64):
    c.add "IL64("
    c.add $i
    c.add ")"
  else:
    c.add "(IL64(-9223372036854775807) - IL64(1))"

proc genUIntLit(c: var GeneratedCode; i: uint64) =
  if i <= high(uint32) and c.bits != 64:
    c.add $i
    c.add "u"
  else:
    c.add $i
    c.add "ull"

# Type graph

proc callingConvToStr(cc: CallConv): string =
  case cc
  of NoCallConv: ""
  of Cdecl: "N_CDECL"
  of Stdcall: "N_STDCALL"
  of Safecall: "N_SAFECALL"
  of Syscall: "N_SYSCALL"
  of Fastcall: "N_FASTCALL"
  of Thiscall: "N_THISCALL"
  of Noconv: "N_NOCONV"
  of Member: "N_NOCONV"
  of Nimcall: "N_NIMCALL"

proc inclHeader(c: var GeneratedCode; lit: StrId) =
  let headerAsStr {.cursor.} = c.m.pool.strings[lit]
  let header = c.tokens.getOrIncl(headerAsStr)
  if headerAsStr.len > 0 and not c.includedHeaders.containsOrIncl(int header):
    if headerAsStr[0] == '#':
      # keeps the #include statements as they are
      c.includes.add header
    else:
      c.includes.add Token(IncludeKeyword)
      if headerAsStr[0] == '<':
        c.includes.add header
      else:
        c.includes.add Token(DoubleQuote)
        c.includes.add header
        c.includes.add Token(DoubleQuote)

    c.includes.add Token NewLine

include gentypes

# Procs

type
  PragmaInfo = object
    flags: set[LengPragma]
    extern, attr: StrId
    callConv: CallConv

proc parseProcPragmas(c: var GeneratedCode; n: var Cursor): PragmaInfo =
  result = PragmaInfo()
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    n.loopInto:
      let pk = n.pragmaKind
      case pk
      of NoPragma, AlignP, BitsP, VectorP, StaticP, PackedP:
        if n.callConvKind != NoCallConv:
          result.callConv = n.callConvKind
          skip n
        else:
          error c.m, "invalid proc pragma: ", n
      of NodeclP:
        result.flags.incl NodeclP
        skip n
      of DynlibP:
        # Static-import library annotation for the NATIVE backend (arkham);
        # meaningless for C output — the linker resolves the symbol.
        skip n
      of ConstrefP:
        # A PARAMETER pragma; never legal on a proc.
        error c.m, "invalid proc pragma: ", n
      of ImportcppP, ImportcP, ExportcP:
        n.into:
          if n.hasMore and n.kind == StrLit:
            result.extern = n.litId
            inc n
          result.flags.incl pk
          while n.hasMore: skip n
      of HeaderP:
        n.into:
          if n.kind != StrLit:
            error c.m, "expected string literal in header pragma but got: ", n
          else:
            inclHeader(c, n.litId)
            result.flags.incl pk
            inc n
          while n.hasMore: skip n
      of SelectanyP:
        result.flags.incl pk
        skip n
      of InstructionP, IntrinsicP:
        # An intrinsic declares a machine instruction, not a callable: there is
        # no definition anywhere, and every call to it is an `(instr …)` the
        # expression generator lowers directly. Recording the flag is enough —
        # `genProcDecl` then emits nothing for the declaration itself.
        result.flags.incl pk
        skip n
      of AssemblerP, NakedP:
        # `{.assembler.}`/`{.naked.}` bodies are transliterated by arkham, not
        # compiled to C.
        # Routing them into a C build means assembling them separately and
        # linking the object — see `nativenif/doc/asm-c-interop.md`, which is not
        # built yet. Reject loudly rather than emit a prototype that will fail to
        # link with no explanation.
        result.flags.incl pk
        skip n
      of InterruptP:
        # A vector-table entry, and a C build has no vector table to install it
        # in. Emitting the function anyway would compile and link and simply
        # never be reached — a device that does not respond to the interrupt,
        # with nothing at the failure site to say why. `{.exportc: "…".}` is
        # what binds a handler by name against a vendor startup file.
        result.flags.incl pk
        skip n
      of RegisterP, StackP:
        # Location pins. They are assertions inside an `{.assembler.}` proc
        # (rejected above) and allocator hints outside one; C has no way to
        # honour either (its own `register` keyword is advisory), so drop them.
        result.flags.incl pk
        skip n
      of WasP:
        genWasPragma c, n
      of ErrsP, RaisesP, SmryP:
        skip n
      of InlineP:
        result.flags.incl pk
        skip n
      of AlwaysInlineP, NoinlineP:
        result.flags.incl pk
        skip n
      of AttrP:
        n.into:
          if n.kind != StrLit:
            error c.m, "expected string literal in attr pragma but got: ", n
          else:
            result.attr = n.litId
          inc n
          while n.hasMore: skip n
  else:
    error c.m, "expected proc pragmas but got: ", n

proc isBareImportProc(prag: PragmaInfo): bool {.inline.} =
  ## `importc` proc with neither `header` nor `nodecl`: declared by US, under
  ## its mangled name + `__asm__` label (collision-proof against header
  ## prototypes for the same libc identifier — see `mangleSym`).
  ImportcP in prag.flags and {HeaderP, NodeclP} * prag.flags == {}

proc genSymDef(c: var GeneratedCode; n: Cursor; prag: PragmaInfo;
               isProc = false): string =
  if n.kind == SymbolDef:
    let lit = n.symId
    if {ImportcP, ImportcppP, ExportcP} * prag.flags != {}:
      if isProc and isBareImportProc(prag):
        result = mangleToC(c.m.pool.syms[lit])
      elif prag.extern != StrId(0):
        result = c.m.pool.strings[prag.extern]
      else:
        result = c.m.pool.syms[lit]
        extractBasename(result)
    else:
      result = mangleToC(c.m.pool.syms[lit])
    c.add result
  else:
    result = ""
    error c.m, "expected SymbolDef but got: ", n

proc genParamPragmas(c: var GeneratedCode; n: var Cursor) =
  # ProcPragma ::= (was Identifier) | Attribute
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    n.loopInto:
      case n.pragmaKind
      of AttrP:
        n.into:
          c.add " __attribute__((" & c.m.pool.strings[n.litId] & "))"
          inc n
          while n.hasMore: skip n
      of WasP:
        genWasPragma c, n
      of ConstrefP:
        # Provenance only (see `doc/tags.md`): the pointer stands for a
        # by-value source parameter. Nothing to emit — it exists for
        # `funcsummary`/the optimizer.
        skip n
      else:
        error c.m, "invalid pragma: ", n
  else:
    error c.m, "expected pragmas but got: ", n

proc genParam(c: var GeneratedCode; n: var Cursor) =
  var d = takeParamDecl(n)
  if d.name.kind == SymbolDef:
    let s = d.name.symId
    c.m.registerLocal(s, d.typ)
    var skipDecl = false
    let name = mangleDecl(c, d.name, d.pragmas, skipDecl)
    genType c, d.typ, name
    genParamPragmas c, d.pragmas
  else:
    error c.m, "expected SymbolDef but got: ", d.name

proc genVarPragmas(c: var GeneratedCode; n: var Cursor): set[LengPragma] =
  result = {}
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    n.loopInto:
      let pk = n.pragmaKind
      case pk
      of AlignP:
        n.into:
          c.add " NIM_ALIGN(" & $intVal(n) & ")"
          inc n
          while n.hasMore: skip n
      of AttrP:
        n.into:
          c.add " __attribute__((" & c.m.pool.strings[n.litId] & "))"
          skip n
          while n.hasMore: skip n
      of WasP:
        genWasPragma c, n
      of HeaderP:
        n.into:
          if n.kind != StrLit:
            error c.m, "expected string literal in header pragma but got: ", n
          else:
            inclHeader(c, n.litId)
            result.incl pk
            skip n
          while n.hasMore: skip n
      of StaticP, ImportcP, ImportcppP, ExportcP, NodeclP:
        result.incl pk
        skip n
      else:
        error c.m, "invalid pragma: ", n
  else:
    error c.m, "expected pragmas but got: ", n

proc genCLineDir(c: var GeneratedCode; info: NifLineInfo) =
  if optLineDir in c.m.config.options and info.isValid:
    let id = info.file
    let line = info.line
    let name = "FX_" & $(int id)
    c.add LineDirKeyword
    c.add $line
    c.add Space
    c.add name
    c.add NewLine
    if id.isValid:
      c.fileIds.incl id

template moveToDataSection(body: untyped) {.dirty.} =
  let oldLen = c.code.len
  body
  for i in oldLen ..< c.code.len:
    c.data.add c.code[i]
  setLen c.code, oldLen

template moveToInitSection(body: untyped) {.dirty.} =
  let oldLen = c.code.len
  body
  for i in oldLen ..< c.code.len:
    c.init.add c.code[i]
  setLen c.code, oldLen

include genexprs

type
  VarKind = enum
    IsLocal, IsGlobal, IsThreadlocal, IsConst, IsMflag

proc isLiteral(n: var Cursor): bool =
  case n.kind
  of IntLit, UIntLit, FloatLit, CharLit, StrLit, DotToken:
    result = true
    inc n
  else:
    case n.exprKind
    of FalseC, TrueC, InfC, NeginfC, NanC, SufC, NilC:
      result = true
      skip n
    of AconstrC, OconstrC, CastC, ConvC:
      result = true
      n.into:
        skip n # type
        while n.hasMore:
          if n.substructureKind == KvU:
            n.into:
              skip n # key (field name Symbol - not a value to check)
              if not isLiteral(n): return false # check the value
              if n.hasMore:
                skip n # optional inheritance
              while n.hasMore: skip n
          else:
            if not isLiteral(n): return false
    else:
      result = false

proc genStmt(c: var GeneratedCode; n: var Cursor)

proc genOnError(c: var GeneratedCode; n: var Cursor) =
  c.add IfKeyword
  c.add ParLe
  c.add ErrToken
  c.add ParRi
  c.add Space
  c.add CurlyLe
  c.genStmt n
  c.add CurlyRi

proc genVarInitValue(c: var GeneratedCode; n: var Cursor) =
  if n.kind == DotToken:
    inc n
    c.add Semicolon
  elif n.stmtKind == OnerrS:
    var onErrAction = n
    inc onErrAction
    c.add AsgnOpr
    genCallCanRaise c, n
    c.add Semicolon
    if onErrAction.kind != DotToken:
      genOnError(c, onErrAction)
  else:
    c.add AsgnOpr
    genx c, n
    c.add Semicolon

proc genVarDecl(c: var GeneratedCode; n: var Cursor; vk: VarKind; toExtern = false; useStatic = false) =
  genCLineDir(c, info(n))
  var d = takeVarDecl(n)
  if d.name.kind == SymbolDef:
    let lit = d.name.symId
    # Infer the variable's type from its initializer when the explicit type
    # slot is empty. Leng has no general var-type inference; the tree
    # optimizers (cse / induction_variables) synthesize `(var :t . . (addr
    # expr))` without spelling out the pointer type, and without this the
    # empty slot degrades to `void`, so the `(deref t)` uses produce invalid C.
    var typ = d.typ
    if typ.kind == DotToken and d.value.kind != DotToken:
      typ = getNominalType(c.m, d.value)
    c.m.registerLocal(lit, typ)
    var skipDecl = false
    let name = mangleDecl(c, d.name, d.pragmas, skipDecl)
    let beforeDecl = c.code.len

    if toExtern or isImportC(c.m, d.name):
      c.add ExternKeyword

    if vk == IsThreadlocal:
      c.add "__thread "
    genType c, typ, name, isConst = vk == IsConst
    let flags = genVarPragmas(c, d.pragmas)
    if not toExtern and (StaticP in flags or useStatic):
      c.code.insert([Token(StaticKeyword)], beforeDecl)
    let beforeInit = c.code.len

    var value = d.value
    let mustMoveToInit = (vk == IsGlobal and not isLiteral(value))
    if toExtern:
      c.add Semicolon
    else:
      if vk != IsLocal and not mustMoveToInit: c.objConstrNeedsType = false
      genVarInitValue c, d.value
      if vk != IsLocal and not mustMoveToInit: c.objConstrNeedsType = true

    if skipDecl:
      setLen c.code, beforeDecl
    elif vk == IsLocal and c.inToplevel:
      for i in beforeDecl ..< c.code.len:
        c.init.add c.code[i]
      setLen c.code, beforeDecl
      c.add Semicolon
    elif mustMoveToInit and not toExtern:
      c.init.add c.tokens.getOrIncl(name)
      for i in beforeInit ..< c.code.len:
        c.init.add c.code[i]
      setLen c.code, beforeInit
      c.add Semicolon
  else:
    error c.m, "expected SymbolDef but got: ", d.name

include genstmts

proc overflowDeclTokens(c: var GeneratedCode): seq[Token] =
  ## The `NB8 ovf = NIM_FALSE;` prologue. Returned instead of inserted by the
  ## callee: the destination is always a field of `c` itself, and passing it as
  ## a separate `var seq` parameter next to `c` aliases the two.
  result = @[
    c.tokens.getOrIncl("NB8"),
    Token(Space),
    Token(OvfToken),
    Token(AsgnOpr),
    c.tokens.getOrIncl("NIM_FALSE"),
    Token(Semicolon)
  ]

proc genProcDecl(c: var GeneratedCode; n: var Cursor; isExtern: bool) =
  c.m.openScope()
  c.inToplevel = false
  let oldProc = c.currentProc
  c.currentProc = CurrentProc(needsOverflowFlag: false)
  let signatureBegin = c.code.len
  var prc = takeProcDecl(n)

  let prag = parseProcPragmas(c, prc.pragmas)
  if prag.flags * {AssemblerP, NakedP} != {}:
    errorAt c.m, "the C backend cannot compile an `{.assembler.}` proc; it must " &
      "be assembled by arkham and linked as an object (see doc/asm-c-interop.md)",
      prc.name
  if InterruptP in prag.flags:
    errorAt c.m, "the C backend has no vector table to install an `{.interrupt.}` " &
      "handler in; use `{.exportc: \"...\".}` to bind one by name against a " &
      "startup file, or compile it with arkham",
      prc.name
  if prag.flags * {InstructionP, IntrinsicP} != {}:
    # No C declaration: an intrinsic has no definition to link against, and
    # every application of it is an `(instr …)` lowered at the use site.
    # Undo EVERY piece of entry state, `inToplevel` included: the normal exit
    # restores it, and leaving it false here would make the rest of the module
    # think it is inside a proc — so a later module-scope `(var …)` would stop
    # being routed into `c.init` and land at file scope with its initializer.
    c.code.setLen signatureBegin
    c.m.closeScope()
    c.inToplevel = true
    c.currentProc = oldProc
    return
  if InlineP in prag.flags:
    c.add StaticKeyword
    c.add "inline "
  else:
    if isExtern:
      c.add ExternKeyword
    if NoinlineP in prag.flags:
      c.add "N_NOINLINE "

  let name: string
  if prag.callConv != NoCallConv:
    c.add callingConvToStr(prag.callConv)
    c.add ParLe
    if prc.returnType.kind == DotToken:
      c.add "void"
    else:
      genType c, prc.returnType
    c.add Comma
    if prag.attr != StrId(0):
      c.add "__attribute__((" & c.m.pool.strings[prag.attr] & ")) "
    name = genSymDef(c, prc.name, prag, isProc = true)
    c.add ParRi
  else:
    if prc.returnType.kind == DotToken:
      c.add "void"
    else:
      genType c, prc.returnType
    c.add Space
    if prag.attr != StrId(0):
      c.add "__attribute__((" & c.m.pool.strings[prag.attr] & ")) "
    name = genSymDef(c, prc.name, prag, isProc = true)

  c.add ParLe

  var params = 0
  if prc.params.kind != DotToken:
    var p = prc.params
    p.loopInto:
      if params > 0: c.add Comma
      genParam c, p
      inc params

  if params == 0:
    c.add "void"
  c.add ParRi

  if {NodeclP, HeaderP} * prag.flags != {}:
    c.code.setLen signatureBegin
  elif InlineP notin prag.flags and (isExtern or {ImportcP, ImportcppP} * prag.flags != {}):
    # External/imported function without body - just prototype
    if isBareImportProc(prag):
      # Bind the mangled identifier to the real symbol. The identifier never
      # collides with a header's prototype for the same libc function, which
      # matters since inliner splices carry bare-importc references into
      # arbitrary modules (measured: `write` vs <unistd.h> in threads/cps).
      var asmName = ""
      if prag.extern != StrId(0):
        asmName = c.m.pool.strings[prag.extern]
      else:
        asmName = c.m.pool.syms[prc.name.symId]
        extractBasename(asmName)
      c.add " __asm__(NIM_ASM_PREFIX "
      c.add makeCString(asmName)
      c.add ")"
    for i in signatureBegin ..< c.code.len:
      c.protos.add c.code[i]
    c.protos.add Token Semicolon
    c.code.setLen signatureBegin  # Remove signature from code since it's now in protos
  else:
    # Local function with body - generate prototype if requested by other modules
    if c.requestedSyms.contains(prc.name.symId):
      for i in signatureBegin ..< c.code.len:
        c.protos.add c.code[i]
      c.protos.add Token Semicolon

    c.add CurlyLe
    let beforeBody = c.code.len
    genStmt c, prc.body
    if c.currentProc.needsOverflowFlag:
      let ovfDecl = overflowDeclTokens(c)
      c.code.insert(ovfDecl, beforeBody)
    c.add CurlyRi
  c.m.closeScope()
  c.inToplevel = true
  c.currentProc = oldProc

template genForeignDataDecl(body: untyped) {.dirty.} =
  # Foreign var/const/gvar/tvar `extern` declarations are emitted by `genVar`
  # into `c.data` via `moveToDataSection`. But `genImportedSyms` runs *after*
  # `genToplevel` has already filled `c.data` with this module's own consts —
  # so the `extern` would land after a sibling const that takes its address
  # (e.g. `more = &strlit…` for a deduped long-string literal). C rejects the
  # forward reference. Relocate the just-emitted declaration to `c.protos`,
  # which is written before `c.data`, so the `extern` always precedes its use.
  let before = c.data.len
  body
  for i in before ..< c.data.len:
    c.protos.add c.data[i]
  setLen c.data, before

proc genImportedSyms(c: var GeneratedCode) =
  # needs a good old fixpoint iteration as we expand the graph of imported symbols.
  while true:
    let fsyms = move c.m.requestedForeignSyms
    if fsyms.len == 0: break
    for fsym in fsyms:
      var n = fsym
      case fsym.stmtKind
      of ProcS:
        genProcDecl c, n, true
      of VarS:
        discard "we need to ignore local variables of the form x.0.suffix here which are still produced sometimes by Nimony..."
      of GvarS:
        genForeignDataDecl: genVar c, n, IsGlobal, true
      of TvarS:
        genForeignDataDecl: genVar c, n, IsThreadlocal, true
      of ConstS:
        genForeignDataDecl: genVar c, n, IsConst, true
      else:
        discard "uninteresting symbol"

proc genNodecl(c: var GeneratedCode; n: var Cursor) =
  let signatureBegin = c.code.len
  n.into:
    case n.stmtKind
    of ProcS: genProcDecl c, n, false
    of VarS: genStmt c, n
    of ConstS: genStmt c, n
    else:
      error c.m, "expected declaration for `nodecl` but got: ", n
    while n.hasMore: skip n
  c.code.setLen signatureBegin

proc genToplevel(c: var GeneratedCode; n: var Cursor) =
  # ExternDecl ::= (imp ProcDecl | VarDecl | ConstDecl)
  # Include ::= (incl StringLiteral)
  # TopLevelConstruct ::= ExternDecl | ProcDecl | VarDecl | ConstDecl |
  #                       TypeDecl | Include | EmitStmt
  case n.stmtKind
  of ProcS: genProcDecl c, n, false
  of VarS, GvarS, TvarS: genStmt c, n
  of ConstS: genVar c, n, IsConst
  of DiscardS, AsgnS, KeepovfS, ScopeS, IfS,
      WhileS, CaseS, LabS, JmpS, TryS, RaiseS, CallS, OnerrS:
    moveToInitSection:
      genStmt c, n
  of TypeS:
    discard "handled in a different pass"
    skip n
  of EmitS: genEmitStmt c, n
  of StmtsS:
    n.loopInto: genToplevel c, n
  else:
    error c.m, "expected top level construct but got: ", n

proc traverseCode(c: var GeneratedCode; n: var Cursor) =
  if n.stmtKind == StmtsS:
    n.loopInto: genToplevel(c, n)
    genImportedSyms c
  else:
    error c.m, "expected `stmts` but got: ", n

proc writeLineDir(f: var CppFile, c: var GeneratedCode) =
  for id in items(c.fileIds):
    let name = "FX_" & $(int id)
    let def = "#define " & name & " \"" & c.m.pool.filenames[id] & "\""
    write f, def
    write f, "\n"

proc generateCode*(s: var State, inp, outp: string; flags: set[GenFlag]) =
  var m = load(inp)
  m.config = s.config
  var c = initGeneratedCode(m, flags, s.bits)
  c.m.openScope()

  var n = beginRead(c.m.src)
  traverseCode c, n

  let realCode = move c.code
  # now that we have seen the full code, we also know all the involved types:
  var co = TypeOrder()
  traverseTypes(c.m, co)

  generateTypes(c, co)
  let typeDecls = move c.code

  var f = CppFile()
  f.write "#define NIM_INTBITS " & $s.bits & "\n"
  f.write Prelude
  if gfMainModule in c.flags:
    f.write $ThreadVarToken & "NB8 " & $ErrToken & $Semicolon & "\n"

  writeTokenSeq f, c.includes, c
  if optLineDir in c.m.config.options:
    writeLineDir f, c
  writeTokenSeq f, typeDecls, c
  # so that v-tables can be generated protos must be written before data:
  writeTokenSeq f, c.protos, c
  writeTokenSeq f, c.data, c
  writeTokenSeq f, realCode, c

  if c.init.len > 0:
    f.write "static void __attribute__((constructor)) init(void) {"
    if c.currentProc.needsOverflowFlag:
      let ovfDecl = overflowDeclTokens(c)
      c.init.insert(ovfDecl, 0)
    writeTokenSeq f, c.init, c
    f.write "}\n\n"

  if vfsExists(outp) and vfsRead(outp) == f.buf:
    discard "unchanged, keep mtime for incremental builds"
  else:
    vfsWrite outp, f.buf

  if c.headerFile.len > 0:
    let selectHeader = outp.changeFileExt(".h")
    var hbuf = ""
    for x in items(c.headerFile):
      hbuf.add c.tokens[x]
    vfsWrite selectHeader, hbuf

  c.m.closeScope()

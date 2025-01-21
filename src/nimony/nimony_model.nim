#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / assertions
include nifprelude
import stringviews, keymatcher

type
  StmtKind* = enum
    NoStmt
    StmtsS = "stmts"
    ScopeS = "scope" # to make it easier for the backend phases to get the scoping right
    VarS = "var"
    LetS = "let"
    CursorS = "cursor"
    ResultS = "result"
    ConstS = "const"
    EmitS = "emit"
    AsgnS = "asgn"
    BlockS = "block"
    IfS = "if"
    WhenS = "when"
    BreakS = "break"
    ContinueS = "continue"
    WhileS = "while"
    ForS = "for"
    CaseS = "case"
    TryS = "try"
    RetS = "ret"
    YieldS = "yld"
    RaiseS = "raise"
    ProcS = "proc"
    FuncS = "func"
    IterS = "iterator"
    ConverterS = "converter"
    MethodS = "method"
    MacroS = "macro"
    TemplateS = "template"
    TypeS = "type"
    CallS = "call"
    CmdS = "cmd"
    DiscardS = "discard"
    IncludeS = "include"
    ImportS = "import"
    FromImportS = "from"
    ImportExceptS = "importexcept"
    ExportS = "export"
    CommentS = "comment"
    ClonerS = "cloner"
    TracerS = "tracer"
    DisarmerS = "disarmer"
    MoverS = "mover"
    DtorS = "dtor"
    PragmasLineS = "pragmas"

  SymKind* = enum
    NoSym
    VarY = "var"
    LetY = "let"
    CursorY = "cursor"
    ResultY = "result"
    ConstY = "const"
    ParamY = "param"
    TypevarY = "typevar"
    EfldY = "efld"
    FldY = "fld"
    ProcY = "proc"
    FuncY = "func"
    IterY = "iterator"
    ConverterY = "converter"
    MethodY = "method"
    MacroY = "macro"
    TemplateY = "template"
    TypeY = "type"
    LabelY = "block"
    ModuleY = "module"
    CchoiceY = "cchoice"

  ExprKind* = enum
    NoExpr
    QuotedX = "quoted"
    AtX = "at"
    DerefX = "deref"
    HderefX = "hderef"
    DotX = "dot"
    DerefDotX = "ddot"
    PatX = "pat"
    ParX = "par"
    AddrX = "addr"
    HaddrX = "haddr"
    NilX = "nil"
    FalseX = "false"
    TrueX = "true"
    AndX = "and"
    OrX = "or"
    NotX = "not"
    NegX = "neg"
    SizeofX = "sizeof"
    OconstrX = "obj"
    NewOconstrX = "newobj"
    TupleConstrX = "tup"
    AconstrX = "arr"
    SetX = "set"
    OchoiceX = "ochoice"
    CchoiceX = "cchoice"
    KvX = "kv"
    AddX = "add"
    SubX = "sub"
    MulX = "mul"
    DivX = "div"
    ModX = "mod"
    ShrX = "shr"
    ShlX = "shl"
    AshrX = "ashr"
    BitandX = "bitand"
    BitorX = "bitor"
    BitxorX = "bitxor"
    BitnotX = "bitnot"
    EqX = "eq"
    NeqX = "neq"
    LeX = "le"
    LtX = "lt"
    CastX = "cast"
    ConvX = "conv"
    OconvX = "oconv" # object conversion
    HconvX = "hconv" # hidden basic type conversion
    DconvX = "dconv" # conversion between `distinct` types
    CallX = "call"
    CallStrLitX = "callstrlit"
    InfixX = "infix"
    PrefixX = "prefix"
    HcallX = "hcall" # hidden converter call
    CmdX = "cmd"
    InfX = "inf"
    NegInfX = "neginf"
    NanX = "nan"
    SufX = "suf"
    RangeX = "range"
    RangesX = "ranges"
    CompilesX = "compiles"
    DeclaredX = "declared"
    DefinedX = "defined"
    HighX = "high"
    LowX = "low"
    TypeofX = "typeof"
    UnpackX = "unpack"
    EnumToStrX = "enumtostr"
    IsMainModuleX = "ismainmodule"
    DefaultObjX = "defaultobj"
    DefaultTupX = "defaulttup"
    ExprX = "expr" # was nkStmtListExpr in the old Nim
    ArrAtX = "arrat"
    TupAtX = "tupat" # tup[0] syntax
    EnsureMoveX = "emove" # note that `move` can be written in standard Nim

  TypeKind* = enum
    NoType
    ObjectT = "object"
    RefObjectT = "refobj"
    PtrObjectT = "ptrobj"
    TupleT = "tuple"
    EnumT = "enum"
    HoleyEnumT = "onum"
    IntT = "i"
    UIntT = "u"
    FloatT = "f"
    CharT = "c"
    BoolT = "bool"
    VoidT = "void"
    PtrT = "ptr"
    RefT = "ref"
    MutT = "mut"
    OutT = "out"
    LentT = "lent"
    SinkT = "sink"
    #FlexarrayT = "flexarray"
    StringT = "string"
    VarargsT = "varargs"
    NilT = "nilt"
    OrT = "or"
    AndT = "and"
    NotT = "not"
    ConceptT = "concept"
    DistinctT = "distinct"
    StaticT = "static"
    ProcT = "proctype"
    IterT = "itertype"
    InvokeT = "at" # might not be the best idea to do it this way...
    ArrayT = "array"
    RangeT = "rangetype"
    UncheckedArrayT = "uarray"
    OpenArrayT = "openarray"
    SetT = "sett"
    AutoT = "auto"
    SymKindT = "symkind"
    TypeKindT = "typekind"
    TypedescT = "typedesc"
    UntypedT = "untyped"
    TypedT = "typed"
    CstringT = "cstring"
    PointerT = "pointer"
    OrdinalT = "ordinal"

  PragmaKind* = enum
    NoPragma
    Magic = "magic"
    ImportC = "importc"
    ImportCpp = "importcpp"
    ExportC = "exportc"
    Nodecl = "nodecl"
    Header = "header"
    Align = "align"
    Bits = "bits"
    Selectany = "selectany"
    Threadvar = "threadvar"
    Globalvar = "global"
    Discardable = "discardable"
    NoReturn = "noreturn"
    Varargs = "varargs"
    Borrow = "borrow"
    NoSideEffect = "noSideEffect"
    NoDestroy = "nodestroy"
    Plugin = "plugin"
    ByCopy = "bycopy"
    ByRef = "byref"
    Inline = "inline"
    NoInit = "noinit"
    Requires = "requires"
    Ensures = "ensures"
    BuildP = "build"
    EmitP = "emit"

  SubstructureKind* = enum
    NoSub
    ElifS = "elif"
    ElseS = "else"
    OfS = "of"
    ParamS = "param"
    ParamsS = "params"
    FldS = "fld"
    EfldS = "efld"
    AtomicS = "atomic"
    TypevarsS = "typevars"
    RoS = "ro"
    RestrictS = "restrict"
    PragmasS = "pragmas"
    UnpackFlatS = "unpackflat"
    UnpackTupS = "unpacktup"
    ExceptS = "except"
    FinallyS = "fin"

  CallConv* = enum
    NoCallConv
    CdeclC = "cdecl"
    StdcallC = "stdcall"
    SafecallC = "safecall"
    SyscallC = "syscall"
    FastcallC = "fastcall"
    ThiscallC = "thiscall"
    NoconvC = "noconv"
    MemberC = "member"
    NoinlineC = "noinline"
    NimcallC = "nimcall"

  AttachedOp* = enum
    attachedDestroy,
    attachedWasMoved,
    attachedDup,
    attachedCopy,
    attachedSink,
    attachedTrace

declareMatcher parseStmtKind, StmtKind

proc stmtKind*(c: Cursor): StmtKind {.inline.} =
  assert c.kind == ParLe
  parseStmtKind pool.tags[tag(c)]

declareMatcher parsePragmaKind, PragmaKind

proc pragmaKind*(c: Cursor): PragmaKind {.inline.} =
  if c.kind == ParLe:
    result = parsePragmaKind pool.tags[tag(c)]
  elif c.kind == Ident:
    result = parsePragmaKind pool.strings[c.litId]
  else:
    result = NoPragma

declareMatcher parseSubstructureKind, SubstructureKind

proc substructureKind*(c: Cursor): SubstructureKind {.inline.} =
  if c.kind == ParLe:
    result = parseSubstructureKind pool.tags[tag(c)]
  else:
    result = NoSub

declareMatcher parseTypeKind, TypeKind

proc typeKind*(c: Cursor): TypeKind {.inline.} =
  if c.kind == ParLe:
    result = parseTypeKind pool.tags[tag(c)]
  elif c.kind == DotToken:
    result = VoidT
  else:
    result = NoType

declareMatcher parseCallConvKind, CallConv

proc callConvKind*(c: Cursor): CallConv {.inline.} =
  if c.kind == ParLe:
    result = parseCallConvKind pool.tags[tag(c)]
  elif c.kind == Ident:
    result = parseCallConvKind pool.strings[c.litId]
  else:
    result = NoCallConv

declareMatcher parseExprKind, ExprKind

proc exprKind*(c: Cursor): ExprKind {.inline.} =
  if c.kind == ParLe:
    result = parseExprKind pool.tags[tag(c)]
  else:
    result = NoExpr

declareMatcher parseSymKind, SymKind

proc symKind*(c: Cursor): SymKind {.inline.} =
  if c.kind == ParLe:
    result = parseSymKind pool.tags[tag(c)]
  else:
    result = NoSym

template `==`*(n: Cursor; s: string): bool = n.kind == ParLe and pool.tags[n.tagId] == s

const
  RoutineKinds* = {ProcY, FuncY, IterY, TemplateY, MacroY, ConverterY, MethodY}
  CallKinds* = {CallX, CallStrLitX, CmdX, PrefixX, InfixX, HcallX}
  ConvKinds* = {HconvX, ConvX, OconvX, DconvX, CastX}

proc addParLe*(dest: var TokenBuf; kind: TypeKind|SymKind|ExprKind|StmtKind|SubstructureKind; info = NoLineInfo) =
  dest.add parLeToken(pool.tags.getOrIncl($kind), info)

proc parLeToken*(kind: TypeKind|SymKind|ExprKind|StmtKind|SubstructureKind|PragmaKind; info = NoLineInfo): PackedToken =
  parLeToken(pool.tags.getOrIncl($kind), info)

template copyIntoKind*(dest: var TokenBuf; kind: TypeKind|SymKind|ExprKind|StmtKind|SubstructureKind|PragmaKind;
                       info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(kind, info)
  body
  dest.addParRi()

template copyIntoKinds*(dest: var TokenBuf; kinds: array[2, StmtKind]; info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(kinds[0], info)
  dest.add parLeToken(kinds[1], info)
  body
  dest.addParRi()
  dest.addParRi()

template copyInto*(dest: var TokenBuf; n: var Cursor; body: untyped) =
  assert n.kind == ParLe
  dest.add n
  inc n
  body
  wantParRi dest, n

proc isAtom*(n: Cursor): bool {.inline.} = n.kind >= ParLe

proc copyIntoSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.add symToken(s, info)

proc copyTree*(dest: var TokenBuf; src: TokenBuf) {.inline.} =
  dest.add src

proc copyTree*(dest: var TokenBuf; src: Cursor) {.inline.} =
  dest.addSubtree src

proc addSymDef*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.add symdefToken(s, info)

proc addEmpty*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.add dotToken(info)

proc addEmpty2*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.add dotToken(info)
  dest.add dotToken(info)

proc addEmpty3*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.add dotToken(info)
  dest.add dotToken(info)
  dest.add dotToken(info)

proc takeTree*(dest: var TokenBuf; n: var Cursor) =
  if n.kind != ParLe:
    dest.add n
    inc n
  else:
    var nested = 0
    while true:
      dest.add n
      case n.kind
      of ParLe: inc nested
      of ParRi:
        dec nested
        if nested == 0:
          inc n
          break
      of EofToken:
        raiseAssert "expected ')', but EOF reached"
      else: discard
      inc n

proc sameTrees*(a, b: Cursor): bool =
  var a = a
  var b = b
  var nested = 0
  let isAtom = a.kind != ParLe
  while true:
    if a.kind != b.kind: return false
    case a.kind
    of ParLe:
      if a.tagId != b.tagId: return false
      inc nested
    of ParRi:
      dec nested
      if nested == 0: return true
    of Symbol, SymbolDef:
      if a.symId != b.symId: return false
    of IntLit:
      if a.intId != b.intId: return false
    of UIntLit:
      if a.uintId != b.uintId: return false
    of FloatLit:
      if a.floatId != b.floatId: return false
    of StringLit, Ident:
      if a.litId != b.litId: return false
    of CharLit, UnknownToken:
      if a.uoperand != b.uoperand: return false
    of DotToken, EofToken: discard "nothing else to compare"
    if isAtom: return true
    inc a
    inc b
  return false

proc isDeclarative*(n: Cursor): bool =
  case n.stmtKind
  of FromImportS, ImportS, ExportS, IncludeS, ImportExceptS, TypeS, CommentS, TemplateS:
    result = true
  else:
    case n.substructureKind
    of PragmasS, TypevarsS:
      result = true
    else:
      case n.exprKind
      of TypeofX:
        result = true
      else:
        result = false

proc isCompileTimeType*(n: Cursor): bool {.inline.} =
  n.typeKind in {TypeKindT, TypedescT, SymKindT, OrT, AndT, NotT, ConceptT, StaticT}

proc firstSon*(n: Cursor): Cursor {.inline.} =
  result = n
  inc result

proc hookName*(op: AttachedOp): string =
  case op
  of attachedDestroy: "destroy"
  of attachedWasMoved: "wasMoved"
  of attachedDup: "dup"
  of attachedCopy: "copy"
  of attachedSink: "sink"
  of attachedTrace: "trace"

const
  NoSymId* = SymId(0)

proc hasBuiltinPragma*(n: Cursor; kind: PragmaKind): bool =
  result = false
  var n = n
  if n.kind == DotToken:
    discard
  else:
    inc n
    while n.kind != ParRi:
      if pragmaKind(n) == kind:
        result = true
        break
      skip n

proc addSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) =
  dest.add symToken(s, info)

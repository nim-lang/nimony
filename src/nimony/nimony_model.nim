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
    RetS = "ret"
    YieldS = "yld"
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
    TupAtX = "tupat"

  TypeKind* = enum
    NoType
    ObjectT = "object"
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
    Plugin = "plugin"

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
    InlineC = "inline"
    NoinlineC = "noinline"

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
  CallKinds* = {CallX, CallStrLitX, CmdX, PrefixX, InfixX}

proc addParLe*(dest: var TokenBuf; kind: TypeKind|SymKind|ExprKind|StmtKind; info = NoLineInfo) =
  dest.add parLeToken(pool.tags.getOrIncl($kind), info)

proc parLeToken*(kind: TypeKind|SymKind|ExprKind|StmtKind|SubstructureKind; info = NoLineInfo): PackedToken =
  parLeToken(pool.tags.getOrIncl($kind), info)

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

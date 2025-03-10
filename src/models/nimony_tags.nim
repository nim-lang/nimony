# Generated by tools/gen_tags.nim from doc/tags.md. DO NOT EDIT!


type
  NimonyExpr* = enum
    NoExpr
    ErrX = (1, "err")  ## indicates an error
    SufX = (2, "suf")  ## literal with suffix annotation
    AtX = (3, "at")  ## array indexing operation
    DerefX = (4, "deref")  ## pointer deref operation
    DotX = (5, "dot")  ## object field selection
    PatX = (6, "pat")  ## pointer indexing operation
    ParX = (7, "par")  ## syntactic parenthesis
    AddrX = (8, "addr")  ## address of operation
    NilX = (9, "nil")  ## nil pointer value
    InfX = (10, "inf")  ## positive infinity floating point value
    NeginfX = (11, "neginf")  ## negative infinity floating point value
    NanX = (12, "nan")  ## NaN floating point value
    FalseX = (13, "false")  ## boolean `false` value
    TrueX = (14, "true")  ## boolean `true` value
    AndX = (15, "and")  ## boolean `and` operation
    OrX = (16, "or")  ## boolean `or` operation
    NotX = (17, "not")  ## boolean `not` operation
    NegX = (18, "neg")  ## negation operation
    SizeofX = (19, "sizeof")  ## `sizeof` operation
    AlignofX = (20, "alignof")  ## `alignof` operation
    OffsetofX = (21, "offsetof")  ## `offsetof` operation
    OconstrX = (22, "oconstr")  ## object constructor
    AconstrX = (23, "aconstr")  ## array constructor
    BracketX = (24, "bracket")  ## untyped array constructor
    CurlyX = (25, "curly")  ## untyped set constructor
    CurlyatX = (26, "curlyat")  ## curly expression `a{i}`
    AddX = (29, "add")
    SubX = (30, "sub")
    MulX = (31, "mul")
    DivX = (32, "div")
    ModX = (33, "mod")
    ShrX = (34, "shr")
    ShlX = (35, "shl")
    BitandX = (36, "bitand")
    BitorX = (37, "bitor")
    BitxorX = (38, "bitxor")
    BitnotX = (39, "bitnot")
    EqX = (40, "eq")
    NeqX = (41, "neq")
    LeX = (42, "le")
    LtX = (43, "lt")
    CastX = (44, "cast")  ## `cast` operation
    ConvX = (45, "conv")  ## type conversion
    CallX = (46, "call")  ## call operation
    CmdX = (47, "cmd")  ## command operation
    CchoiceX = (73, "cchoice")  ## closed choice
    OchoiceX = (74, "ochoice")  ## open choice
    PragmaxX = (129, "pragmax")  ## pragma expressions
    QuotedX = (205, "quoted")  ## name in backticks
    HderefX = (206, "hderef")  ## hidden pointer deref operation
    DdotX = (207, "ddot")  ## deref dot
    HaddrX = (208, "haddr")  ## hidden address of operation
    NewobjX = (209, "newobj")  ## new object constructor
    TupX = (210, "tup")  ## untyped tuple constructor
    TupconstrX = (211, "tupconstr")  ## tuple constructor
    SetconstrX = (212, "setconstr")  ## set constructor
    TabconstrX = (213, "tabconstr")  ## table constructor
    AshrX = (214, "ashr")
    OconvX = (215, "oconv")  ## object conversion
    HconvX = (216, "hconv")  ## hidden basic type conversion
    DconvX = (217, "dconv")  ## conversion between `distinct` types
    CallstrlitX = (218, "callstrlit")
    InfixX = (219, "infix")
    PrefixX = (220, "prefix")
    HcallX = (221, "hcall")  ## hidden converter call
    CompilesX = (222, "compiles")
    DeclaredX = (223, "declared")
    DefinedX = (224, "defined")
    HighX = (225, "high")
    LowX = (226, "low")
    TypeofX = (227, "typeof")
    UnpackX = (228, "unpack")
    EnumtostrX = (229, "enumtostr")
    IsmainmoduleX = (230, "ismainmodule")
    DefaultobjX = (231, "defaultobj")
    DefaulttupX = (232, "defaulttup")
    ExprX = (233, "expr")
    DoX = (234, "do")  ## `do` expression
    ArratX = (235, "arrat")
    TupatX = (236, "tupat")
    PlussetX = (237, "plusset")
    MinussetX = (238, "minusset")
    MulsetX = (239, "mulset")
    XorsetX = (240, "xorset")
    EqsetX = (241, "eqset")
    LesetX = (242, "leset")
    LtsetX = (243, "ltset")
    InsetX = (244, "inset")
    CardX = (245, "card")
    EmoveX = (246, "emove")
    DestroyX = (247, "destroy")
    DupX = (248, "dup")
    CopyX = (249, "copy")
    WasmovedX = (250, "wasmoved")
    SinkhX = (251, "sinkh")
    TraceX = (252, "trace")

proc rawTagIsNimonyExpr*(raw: uint32): bool {.inline.} =
  let r = raw
  r <= 255'u32 and r.uint8 in {1'u8, 2'u8, 3'u8, 4'u8, 5'u8, 6'u8, 7'u8, 8'u8, 9'u8, 10'u8, 11'u8, 12'u8, 13'u8, 14'u8, 15'u8, 16'u8, 17'u8, 18'u8, 19'u8, 20'u8, 21'u8, 22'u8, 23'u8, 24'u8, 25'u8, 26'u8, 29'u8, 30'u8, 31'u8, 32'u8, 33'u8, 34'u8, 35'u8, 36'u8, 37'u8, 38'u8, 39'u8, 40'u8, 41'u8, 42'u8, 43'u8, 44'u8, 45'u8, 46'u8, 47'u8, 73'u8, 74'u8, 129'u8, 205'u8, 206'u8, 207'u8, 208'u8, 209'u8, 210'u8, 211'u8, 212'u8, 213'u8, 214'u8, 215'u8, 216'u8, 217'u8, 218'u8, 219'u8, 220'u8, 221'u8, 222'u8, 223'u8, 224'u8, 225'u8, 226'u8, 227'u8, 228'u8, 229'u8, 230'u8, 231'u8, 232'u8, 233'u8, 234'u8, 235'u8, 236'u8, 237'u8, 238'u8, 239'u8, 240'u8, 241'u8, 242'u8, 243'u8, 244'u8, 245'u8, 246'u8, 247'u8, 248'u8, 249'u8, 250'u8, 251'u8, 252'u8}

type
  NimonyStmt* = enum
    NoStmt
    CallS = (46, "call")  ## call operation
    CmdS = (47, "cmd")  ## command operation
    GvarS = (50, "gvar")  ## global variable declaration
    TvarS = (51, "tvar")  ## thread local variable declaration
    VarS = (52, "var")  ## variable declaration
    ConstS = (54, "const")  ## const variable declaration
    ResultS = (55, "result")  ## result variable declaration
    GletS = (56, "glet")  ## global let variable declaration
    TletS = (57, "tlet")  ## thread local let variable declaration
    LetS = (58, "let")  ## let variable declaration
    CursorS = (59, "cursor")  ## cursor variable declaration
    ProcS = (63, "proc")  ## proc declaration
    FuncS = (64, "func")  ## function declaration
    IteratorS = (65, "iterator")  ## iterator declaration
    ConverterS = (66, "converter")  ## converter declaration
    MethodS = (67, "method")  ## method declaration
    MacroS = (68, "macro")  ## macro declaration
    TemplateS = (69, "template")  ## template declaration
    TypeS = (70, "type")  ## type declaration
    BlockS = (71, "block")  ## block declaration
    EmitS = (75, "emit")  ## emit statement
    AsgnS = (76, "asgn")  ## assignment statement
    ScopeS = (77, "scope")  ## explicit scope annotation, like `stmts`
    IfS = (78, "if")  ## if statement header
    WhenS = (79, "when")  ## when statement header
    BreakS = (83, "break")  ## `break` statement
    ContinueS = (84, "continue")  ## `continue` statement
    ForS = (85, "for")  ## for statement
    WhileS = (86, "while")  ## `while` statement
    CaseS = (87, "case")  ## `case` statement
    RetS = (91, "ret")  ## `return` instruction
    YldS = (92, "yld")  ## yield statement
    StmtsS = (93, "stmts")  ## list of statements
    PragmasS = (128, "pragmas")  ## begin of pragma section
    InclS = (135, "incl")  ## `#include` statement or `incl` set operation
    ExclS = (136, "excl")  ## `excl` set operation
    IncludeS = (137, "include")  ## `include` statement
    ImportS = (138, "import")  ## `import` statement
    ImportasS = (139, "importas")  ## `import as` statement
    FromS = (140, "from")  ## `from` statement
    ImportexceptS = (141, "importexcept")  ## `importexcept` statement
    ExportS = (142, "export")  ## `export` statement
    ExportexceptS = (143, "exportexcept")  ## `exportexcept` statement
    CommentS = (144, "comment")  ## `comment` statement
    DiscardS = (145, "discard")  ## `discard` statement
    TryS = (146, "try")  ## `try` statement
    RaiseS = (147, "raise")  ## `raise` statement
    UnpackdeclS = (158, "unpackdecl")  ## unpack var/let/const declaration
    StaticstmtS = (254, "staticstmt")  ## `static` statement
    BindS = (255, "bind")  ## `bind` statement
    MixinS = (256, "mixin")  ## `mixin` statement
    UsingS = (257, "using")  ## `using` statement
    AsmS = (258, "asm")  ## `asm` statement
    DeferS = (259, "defer")  ## `defer` statement

proc rawTagIsNimonyStmt*(raw: uint32): bool {.inline.} =
  let r = raw - 46'u32
  r <= 255'u32 and r.uint8 in {0'u8, 1'u8, 4'u8, 5'u8, 6'u8, 8'u8, 9'u8, 10'u8, 11'u8, 12'u8, 13'u8, 17'u8, 18'u8, 19'u8, 20'u8, 21'u8, 22'u8, 23'u8, 24'u8, 25'u8, 29'u8, 30'u8, 31'u8, 32'u8, 33'u8, 37'u8, 38'u8, 39'u8, 40'u8, 41'u8, 45'u8, 46'u8, 47'u8, 82'u8, 89'u8, 90'u8, 91'u8, 92'u8, 93'u8, 94'u8, 95'u8, 96'u8, 97'u8, 98'u8, 99'u8, 100'u8, 101'u8, 112'u8, 208'u8, 209'u8, 210'u8, 211'u8, 212'u8, 213'u8}

type
  NimonyType* = enum
    NoType
    ErrT = (1, "err")  ## indicates an error
    AtT = (3, "at")  ## array indexing operation
    AndT = (15, "and")  ## boolean `and` operation
    OrT = (16, "or")  ## boolean `or` operation
    NotT = (17, "not")  ## boolean `not` operation
    IteratorT = (65, "iterator")  ## iterator declaration
    ParamsT = (94, "params")  ## list of proc parameters, also used as a "proc type"
    ObjectT = (96, "object")  ## object type declaration
    EnumT = (97, "enum")  ## enum type declaration
    ProctypeT = (98, "proctype")  ## proc type declaration (soon obsolete, use params instead)
    IT = (103, "i")  ## `int` builtin type
    UT = (104, "u")  ## `uint` builtin type
    FT = (105, "f")  ## `float` builtin type
    CT = (106, "c")  ## `char` builtin type
    BoolT = (107, "bool")  ## `bool` builtin type
    VoidT = (108, "void")  ## `void` return type
    PtrT = (109, "ptr")  ## `ptr` type contructor
    ArrayT = (110, "array")  ## `array` type constructor
    VarargsT = (125, "varargs")  ## `varargs` proc annotation
    StaticT = (151, "static")  ## `static` type or annotation
    TupleT = (161, "tuple")  ## `tuple` type
    OnumT = (162, "onum")  ## enum with holes type
    RefT = (163, "ref")  ## `ref` type
    MutT = (164, "mut")  ## `mut` type
    OutT = (165, "out")  ## `out` type
    LentT = (166, "lent")  ## `lent` type
    SinkT = (167, "sink")  ## `sink` type
    NiltT = (168, "nilt")  ## `nilt` type
    ConceptT = (169, "concept")  ## `concept` type
    DistinctT = (170, "distinct")  ## `distinct` type
    ItertypeT = (171, "itertype")  ## `itertype` type
    RangetypeT = (172, "rangetype")  ## `rangetype` type
    UarrayT = (173, "uarray")  ## `uarray` type
    SetT = (174, "set")  ## `set` type
    AutoT = (175, "auto")  ## `auto` type
    SymkindT = (176, "symkind")  ## `symkind` type
    TypekindT = (177, "typekind")  ## `typekind` type
    TypedescT = (178, "typedesc")  ## `typedesc` type
    UntypedT = (179, "untyped")  ## `untyped` type
    TypedT = (180, "typed")  ## `typed` type
    CstringT = (181, "cstring")  ## `cstring` type
    PointerT = (182, "pointer")  ## `pointer` type
    OrdinalT = (183, "ordinal")  ## `ordinal` type

proc rawTagIsNimonyType*(raw: uint32): bool {.inline.} =
  let r = raw
  r <= 255'u32 and r.uint8 in {1'u8, 3'u8, 15'u8, 16'u8, 17'u8, 65'u8, 94'u8, 96'u8, 97'u8, 98'u8, 103'u8, 104'u8, 105'u8, 106'u8, 107'u8, 108'u8, 109'u8, 110'u8, 125'u8, 151'u8, 161'u8, 162'u8, 163'u8, 164'u8, 165'u8, 166'u8, 167'u8, 168'u8, 169'u8, 170'u8, 171'u8, 172'u8, 173'u8, 174'u8, 175'u8, 176'u8, 177'u8, 178'u8, 179'u8, 180'u8, 181'u8, 182'u8, 183'u8}

type
  NimonyOther* = enum
    NoSub
    KvU = (27, "kv")  ## key-value pair
    VvU = (28, "vv")  ## value-value pair (used for explicitly named arguments in function calls)
    RangeU = (48, "range")  ## `(range a b)` construct
    RangesU = (49, "ranges")
    ParamU = (53, "param")  ## parameter declaration
    TypevarU = (60, "typevar")  ## type variable declaration
    EfldU = (61, "efld")  ## enum field declaration
    FldU = (62, "fld")  ## field declaration
    ElifU = (80, "elif")  ## pair of (condition, action)
    ElseU = (81, "else")  ## `else` action
    TypevarsU = (82, "typevars")  ## type variable/generic parameters
    OfU = (88, "of")  ## `of` branch within a `case` statement
    ParamsU = (94, "params")  ## list of proc parameters, also used as a "proc type"
    PragmasU = (128, "pragmas")  ## begin of pragma section
    UnpackflatU = (156, "unpackflat")  ## unpack into flat variable list
    UnpacktupU = (157, "unpacktup")  ## unpack tuple
    ExceptU = (159, "except")  ## except subsection
    FinU = (160, "fin")  ## finally subsection

proc rawTagIsNimonyOther*(raw: uint32): bool {.inline.} =
  let r = raw - 27'u32
  r <= 255'u32 and r.uint8 in {0'u8, 1'u8, 21'u8, 22'u8, 26'u8, 33'u8, 34'u8, 35'u8, 53'u8, 54'u8, 55'u8, 61'u8, 67'u8, 101'u8, 129'u8, 130'u8, 132'u8, 133'u8}

type
  NimonyPragma* = enum
    NoPragma
    EmitP = (75, "emit")  ## emit statement
    InlineP = (122, "inline")  ## `inline` proc annotation
    NoinlineP = (123, "noinline")  ## `noinline` proc annotation
    VarargsP = (125, "varargs")  ## `varargs` proc annotation
    SelectanyP = (127, "selectany")
    AlignP = (130, "align")
    BitsP = (131, "bits")
    NodeclP = (134, "nodecl")  ## `nodecl` annotation
    RaisesP = (149, "raises")  ## proc annotation
    UntypedP = (179, "untyped")  ## `untyped` type
    MagicP = (184, "magic")  ## `magic` pragma
    ImportcP = (185, "importc")  ## `importc` pragma
    ImportcppP = (186, "importcpp")  ## `importcpp` pragma
    ExportcP = (187, "exportc")  ## `exportc` pragma
    HeaderP = (188, "header")  ## `header` pragma
    ThreadvarP = (189, "threadvar")  ## `threadvar` pragma
    GlobalP = (190, "global")  ## `global` pragma
    DiscardableP = (191, "discardable")  ## `discardable` pragma
    NoreturnP = (192, "noreturn")  ## `noreturn` pragma
    BorrowP = (193, "borrow")  ## `borrow` pragma
    NoSideEffectP = (194, "noSideEffect")  ## `noSideEffect` pragma
    NodestroyP = (195, "nodestroy")  ## `nodestroy` pragma
    PluginP = (196, "plugin")  ## `plugin` pragma
    BycopyP = (197, "bycopy")  ## `bycopy` pragma
    ByrefP = (198, "byref")  ## `byref` pragma
    NoinitP = (199, "noinit")  ## `noinit` pragma
    RequiresP = (200, "requires")  ## `requires` pragma
    EnsuresP = (201, "ensures")  ## `ensures` pragma
    BuildP = (202, "build")  ## `build` pragma
    StringP = (203, "string")  ## `string` pragma
    ViewP = (204, "view")  ## `view` pragma
    InjectP = (263, "inject")  ## `inject` pragma
    GensymP = (264, "gensym")  ## `gensym` pragma
    ErrorP = (265, "error")  ## `error` pragma

proc rawTagIsNimonyPragma*(raw: uint32): bool {.inline.} =
  let r = raw - 75'u32
  r <= 255'u32 and r.uint8 in {0'u8, 47'u8, 48'u8, 50'u8, 52'u8, 55'u8, 56'u8, 59'u8, 74'u8, 104'u8, 109'u8, 110'u8, 111'u8, 112'u8, 113'u8, 114'u8, 115'u8, 116'u8, 117'u8, 118'u8, 119'u8, 120'u8, 121'u8, 122'u8, 123'u8, 124'u8, 125'u8, 126'u8, 127'u8, 128'u8, 129'u8, 188'u8, 189'u8, 190'u8}

type
  NimonySym* = enum
    NoSym
    GvarY = (50, "gvar")  ## global variable declaration
    TvarY = (51, "tvar")  ## thread local variable declaration
    VarY = (52, "var")  ## variable declaration
    ParamY = (53, "param")  ## parameter declaration
    ConstY = (54, "const")  ## const variable declaration
    ResultY = (55, "result")  ## result variable declaration
    GletY = (56, "glet")  ## global let variable declaration
    TletY = (57, "tlet")  ## thread local let variable declaration
    LetY = (58, "let")  ## let variable declaration
    CursorY = (59, "cursor")  ## cursor variable declaration
    TypevarY = (60, "typevar")  ## type variable declaration
    EfldY = (61, "efld")  ## enum field declaration
    FldY = (62, "fld")  ## field declaration
    ProcY = (63, "proc")  ## proc declaration
    FuncY = (64, "func")  ## function declaration
    IteratorY = (65, "iterator")  ## iterator declaration
    ConverterY = (66, "converter")  ## converter declaration
    MethodY = (67, "method")  ## method declaration
    MacroY = (68, "macro")  ## macro declaration
    TemplateY = (69, "template")  ## template declaration
    TypeY = (70, "type")  ## type declaration
    BlockY = (71, "block")  ## block declaration
    ModuleY = (72, "module")  ## module declaration
    CchoiceY = (73, "cchoice")  ## closed choice

proc rawTagIsNimonySym*(raw: uint32): bool {.inline.} =
  raw >= 50'u32 and raw <= 73'u32

type
  HookKind* = enum
    NoHook
    DestroyH = (247, "destroy")
    DupH = (248, "dup")
    CopyH = (249, "copy")
    WasmovedH = (250, "wasmoved")
    SinkhH = (251, "sinkh")
    TraceH = (252, "trace")

proc rawTagIsHookKind*(raw: uint32): bool {.inline.} =
  raw >= 247'u32 and raw <= 252'u32

type
  ControlFlowKind* = enum
    NoControlFlow
    IteF = (152, "ite")  ## if-then-else
    GraphF = (153, "graph")  ## disjoint subgraph annotation
    ForbindF = (154, "forbind")  ## bindings for a `for` loop but the loop itself is mapped to gotos
    KillF = (155, "kill")  ## some.var is about to disappear (scope exit)

proc rawTagIsControlFlowKind*(raw: uint32): bool {.inline.} =
  raw >= 152'u32 and raw <= 155'u32


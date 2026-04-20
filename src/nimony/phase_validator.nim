#
#
#           Nimony Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Phase-aware grammar validator.
##
## The validator walks a `TokenBuf` produced by a compiler pass and checks
## that every ParLe node conforms to the grammar defined in `doc/tags.md`
## *and* belongs to the subset of tags allowed at the current phase.
##
## The grammar is loaded from `doc/tags.md` at compile time via `staticRead`
## so the validator carries no runtime parsing cost.
##
## Inspiration: CompCert validates that each pass produces IR conforming to
## the (formalized) grammar of the next stage. `tags.md` is our grammar spec.

import std / [tables, strutils, assertions, sets, syncio]
include ".." / lib / nifprelude
import ".." / models / [tags, nimony_tags, nifc_tags, callconv_tags]
import nimony_model
import tags_grammar
import reporters  # infoToStr

# ---------------------------------------------------------------------------
# Embedded grammar
# ---------------------------------------------------------------------------

const
  rawTagsMd = staticRead("../../doc/tags.md")
  ## The grammar for Nimony/NIFC tags; single source of truth.

let
  grammar* = parseTagsMdFromString(rawTagsMd)
    ## Parsed form of `doc/tags.md`.

# ---------------------------------------------------------------------------
# Phase description
# ---------------------------------------------------------------------------

type
  PhaseKind* = enum
    phasePostSem        ## after `src/nimony/sem.nim`
    phasePostNifcgen    ## after hexer -> NIFC

  Phase* = object
    name*: string
    kind*: PhaseKind
    allowed*: proc (raw: TagEnum): bool {.nimcall.}
    ## Returns true if the given tag may appear anywhere in a buffer that is
    ## the output of this phase. Finer per-position rules are encoded in the
    ## grammar itself (tags.md).

proc postSemAllowed(raw: TagEnum): bool =
  rawTagIsNimonyExpr(raw) or rawTagIsNimonyStmt(raw) or
    rawTagIsNimonyType(raw) or rawTagIsNimonyOther(raw) or
    rawTagIsNimonySym(raw) or rawTagIsNimonyPragma(raw) or
    rawTagIsCallConv(raw)

proc postNifcgenAllowed(raw: TagEnum): bool =
  rawTagIsNifcExpr(raw) or rawTagIsNifcStmt(raw) or
    rawTagIsNifcType(raw) or rawTagIsNifcOther(raw) or
    rawTagIsNifcSym(raw) or rawTagIsNifcPragma(raw) or
    rawTagIsNifcTypeQualifier(raw) or rawTagIsCallConv(raw)

proc postSemPhase*(): Phase =
  Phase(name: "post-sem", kind: phasePostSem, allowed: postSemAllowed)

proc postNifcgenPhase*(): Phase =
  Phase(name: "post-nifcgen", kind: phasePostNifcgen, allowed: postNifcgenAllowed)

# ---------------------------------------------------------------------------
# Cursor classification
# ---------------------------------------------------------------------------

type
  Violation* = object
    info*: PackedLineInfo
    tag*: string
    msg*: string
    parents*: string   ## "/"-joined parent tag chain (root first)

  ValidatorCtx = object
    phase: Phase
    violations: seq[Violation]
    parents: seq[string]
    # Limit noise: once we hit this, stop collecting.
    maxViolations: int

const
  typeCtxTagsLiteral = [
    # boolean operators used as type set combinators
    "or", "and", "not", "xor",
    # type constructors and type-kind markers
    "ptr", "array", "set", "sink", "lent", "uarray", "typedesc",
    "rangetype", "tuple", "ref", "mut", "out", "distinct", "flexarray",
    "aptr", "cstring", "pointer", "auto", "void", "bool", "untyped", "typed",
    "ordinal", "nilt", "static", "proctype", "itertype",
    # nullary type-kind markers for object/tuple/enum/ref concepts
    "object", "enum", "concept",
    # `varargs` appears nullary as a type-kind marker (in the `(type)`
    # declaration body of the builtin `varargs` type) and also as the
    # proc-signature marker in `(param ... (varargs) .)`.
    "varargs",
    # the `of` inheritance marker inside an object type
    "of",
    # generic operator kind markers appearing in compiler-builtin proc sigs
    "low", "high", "add", "sub", "mul", "div", "mod", "shr", "shl", "ashr",
    "bitand", "bitor", "bitxor", "bitnot", "eq", "neq", "le", "lt", "conv",
    "cast", "deref", "pat", "tupat", "arrat",
    # decl kinds that may appear nullary as kind markers
    "const"
  ]

proc classifyCursor(c: Cursor; preferStmt: bool; inType: bool): ChildKind =
  ## Map a cursor position to a `ChildKind` suitable for grammar matching.
  ## `preferStmt` biases the classification of tags that belong to both the
  ## expression and statement categories (e.g. `call`, `cmd`, `and`).
  ## `inType` biases type-combinator tags (`or`, `and`, ...) and builtin
  ## operator kind-markers (`add`, `low`, ...) toward `ckT`, since in a type
  ## slot they *are* type expressions.
  case c.kind
  of DotToken:
    ckDot
  of IntLit, UIntLit, FloatLit, CharLit, StringLit:
    ckLit
  of SymbolDef:
    ckD
  of Symbol, Ident:
    ckY
  of ParLe:
    let raw = tagEnum(c)
    let isExpr = rawTagIsNimonyExpr(raw) or rawTagIsNifcExpr(raw)
    let isStmt = rawTagIsNimonyStmt(raw) or rawTagIsNifcStmt(raw)
    let isType = rawTagIsNimonyType(raw) or rawTagIsNifcType(raw)
    if inType:
      if isType:
        return ckT
      let tagStr = pool.tags[c.tag]
      for t in typeCtxTagsLiteral:
        if t == tagStr: return ckT
    if isExpr and isStmt:
      if preferStmt: ckS else: ckXS
    elif isExpr:
      ckX
    elif isStmt:
      ckS
    elif isType:
      ckT
    elif rawTagIsNimonyOther(raw) or rawTagIsNifcOther(raw) or
         rawTagIsNimonyPragma(raw) or rawTagIsNifcPragma(raw) or
         rawTagIsNifcTypeQualifier(raw) or rawTagIsCallConv(raw):
      ckNested
    elif rawTagIsNimonySym(raw) or rawTagIsNifcSym(raw):
      ckY
    else:
      ckAny
  else:
    ckAny

proc tagName(c: Cursor): string =
  if c.kind == ParLe:
    result = pool.tags[c.tag]
  else:
    result = ""

# ---------------------------------------------------------------------------
# Matching
# ---------------------------------------------------------------------------

proc collectChildKinds(parent: Cursor; preferStmtContext: bool;
                       typeSlots: HashSet[int]; allKidsType: bool;
                       outKinds: var seq[ChildKind]) =
  ## Walks the children of `parent` (which must point at ParLe) and appends
  ## their classifications to `outKinds`. Does not recurse into children.
  assert parent.kind == ParLe
  var c = parent
  inc c  # past the ParLe
  var idx = 0
  while c.kind != ParRi:
    let inTypeSlot = allKidsType or (idx in typeSlots)
    outKinds.add classifyCursor(c, preferStmtContext, inTypeSlot)
    skip c
    inc idx

proc isStmtContext(tagStr: string): bool =
  ## Rough heuristic: tags whose direct children are statements.
  tagStr in ["stmts", "scope", "block", "if", "elif", "else", "when",
             "while", "for", "try", "except", "fin", "of", "case",
             "defer", "ite", "itec", "loop"]

# ---------------------------------------------------------------------------
# Context-sensitive grammar overlays
# ---------------------------------------------------------------------------
#
# `tags.md` uses the `;` separator strictly for "Nimony form; NIFC form", so it
# cannot encode purely-contextual alternatives (e.g. `(or X X)` in expression
# position vs. `(or T+)` in type position). We therefore enrich the validator
# with two overlays:
#
#  * a type-context overlay (`typeCtxTags`): tags that, when appearing in a T
#    slot, accept either a nullary form (type-kind marker used in generic
#    proc-type declarations) or a variadic type-child form.
#
#  * a pragma-context overlay (`pragmaCtxHooks`): hook tags that, when
#    appearing inside a `(pragmas ...)`, take a single symbol argument
#    identifying the procedure implementing the hook.

const
  ## Tags whose children are *always* types, regardless of the parent's
  ## own context (pure type constructors).
  alwaysTypeParents = [
    "ptr", "ref", "lent", "sink", "uarray", "array", "typedesc", "distinct",
    "mut", "out", "flexarray", "typekind", "symkind", "static", "aptr",
    "tupconstr", "newref", "set", "rangetype", "itertype", "proctype"
  ]

  ## Tags whose children are in the *same* context as their own
  ## (they propagate: set operators, logical operators).
  propagatesType = ["or", "and", "not", "xor"]

  ## Hook tags: inside `(pragmas ...)` they take a single symbol argument.
  pragmaCtxHooks = ["copy", "destroy", "dup", "wasmoved", "sinkh", "trace"]

proc typeCtxSet(): HashSet[string] =
  result = initHashSet[string]()
  for t in typeCtxTagsLiteral: result.incl t

let typeCtxTagSet = typeCtxSet()

proc typeChildIndices(parentTag: string): seq[int] =
  ## Returns the child indices whose slot is a type (`T`) for a given parent
  ## tag. Used to decide whether to descend into type context.
  case parentTag
  # Declarations `(<kind> D E P T .X)` or `(<kind> D P T .X)`:
  of "var", "let", "const", "gvar", "tvar", "glet", "tlet",
     "cursor", "patternvar", "result", "typevar", "param", "fld":
    @[3]
  of "efld":
    @[3]
  # `(type D E TV P BODY)` — BODY (slot 4) is a type:
  of "type":
    @[4]
  # `(proc/func D E Pattern TV Params T P Effects Body)` — T (slot 5) is a type:
  of "proc", "func":
    @[5]
  # `(object .T (fld ...)*)`:
  of "object":
    @[0]
  # `(rangetype T X X)`, `(set T)`, `(sink T)`, `(lent T)`, `(uarray T)`:
  of "rangetype", "set", "sink", "lent", "uarray", "typedesc",
     "ptr", "ref", "mut", "out", "distinct", "flexarray", "aptr":
    @[0]
  # `(array T T)`:
  of "array":
    @[0, 1]
  # `(proctype . (params...) T P)` / `(itertype . (params...) T)`:
  of "proctype", "itertype":
    @[2]
  # typed operators and conversions whose first child is a type:
  of "cast", "conv", "hconv", "dconv", "baseobj",
     "add", "sub", "mul", "div", "mod", "shr", "shl", "ashr",
     "bitand", "bitor", "bitxor", "bitnot",
     "eq", "neq", "le", "lt",
     "card", "sizeof", "alignof", "offsetof",
     "oconstr", "aconstr", "setconstr", "newobj", "tupconstr",
     "defaultobj", "defaulttup", "defaultdistinct",
     "plusset", "minusset", "mulset", "xorset",
     "eqset", "leset", "ltset", "inset",
     "nil", "inf", "neginf", "nan",
     "internalTypeName", "internalFieldPairs", "fields", "fieldpairs",
     "envp":
    @[0]
  of "is", "instanceof":
    @[1]
  else:
    @[]

proc kindMarkerSlot(parentTag: string; childIdx: int): bool =
  ## Returns true if `childIdx` of `parentTag` is the "export / kind marker"
  ## slot that may hold a nullary or underspecified kind marker such as
  ## `(typedesc)`, `(pointer)`, `(not)` or `(add -3)` instead of the usual
  ## `x` / `.` export marker. Grammar arity validation is skipped for the
  ## tag that fills such a slot.
  case parentTag
  of "type", "proc", "func", "iterator", "converter", "method", "macro",
     "template":
    childIdx == 1
  else:
    false

proc matchesTypeContext(tag: string; kinds: openArray[ChildKind]): bool =
  ## Best-effort match for a tag appearing in type context.
  ## Accepts nullary form, or any number of type-like children.
  if tag notin typeCtxTagSet: return false
  if kinds.len == 0: return true        # nullary kind marker
  for k in kinds:
    if k notin {ckT, ckY, ckLit, ckAny, ckNested, ckDot}:
      return false
  true

proc matchesPragmaContext(tag: string; kinds: openArray[ChildKind];
                          isPragmaKind: bool): bool =
  ## Hook-in-pragma form: `(hook Y)`.
  ## Also accepts a nullary form for any tag classified as a pragma: bare-word
  ## annotations like `{.varargs.}` / `{.nodecl.}` are written as
  ## `(varargs)` / `(nodecl)` with zero children.
  if tag in pragmaCtxHooks and kinds.len == 1 and
     kinds[0] in {ckY, ckLit, ckAny}:
    return true
  if isPragmaKind and kinds.len == 0:
    return true
  false

proc addViolation(ctx: var ValidatorCtx; info: PackedLineInfo;
                  tag, msg: string) =
  if ctx.violations.len >= ctx.maxViolations: return
  ctx.violations.add Violation(
    info: info, tag: tag, msg: msg,
    parents: ctx.parents.join("/"))

proc checkTree(ctx: var ValidatorCtx; c: var Cursor;
               parentTag: string; childIdx: int; inType: bool)

proc checkParLe(ctx: var ValidatorCtx; c: var Cursor;
                parentTag: string; childIdx: int; inType: bool) =
  assert c.kind == ParLe
  let raw = tagEnum(c)
  let tag = tagName(c)
  let info = c.info

  # Phase-allowed check:
  if not ctx.phase.allowed(raw):
    ctx.addViolation info, tag,
      "tag not permitted in phase '" & ctx.phase.name & "'"

  # Determine per-child type context (needed both for children classification
  # and for recursion).
  let allKidsType = tag in alwaysTypeParents
  let propagate = tag in propagatesType
  let kidsTypeByThisTag = allKidsType or (propagate and inType)
  let typeIdxSeq = if kidsTypeByThisTag: newSeq[int]() else: typeChildIndices(tag)
  var typeSlots: HashSet[int] = initHashSet[int]()
  for i in typeIdxSeq: typeSlots.incl i

  # Collect children classification once (shared between overlays and main
  # grammar), with per-slot type-context awareness.
  var kinds: seq[ChildKind] = @[]
  collectChildKinds(c, isStmtContext(tag), typeSlots, kidsTypeByThisTag, kinds)

  # Kind-marker slot: the tag standing in for the export marker may be a
  # nullary or underspecified kind marker (e.g. `(typedesc)`, `(add -3)`).
  # We skip arity/grammar validation for it — but we still recurse into its
  # children so nested tags are validated normally.
  let isKindMarker = kindMarkerSlot(parentTag, childIdx)

  # Grammar check: try overlays first, then fall back to tags.md grammar.
  var matched = isKindMarker
  var bestErrors: seq[string] = @[]

  if not matched and inType and matchesTypeContext(tag, kinds):
    matched = true
  if not matched and parentTag == "pragmas":
    let isPragmaKind = rawTagIsNimonyPragma(raw) or rawTagIsNifcPragma(raw)
    if matchesPragmaContext(tag, kinds, isPragmaKind):
      matched = true
  if not matched and tag in grammar:
    for form in grammar[tag]:
      let errs = tryMatchSpec(kinds, form)
      if errs.len == 0:
        matched = true
        break
      if bestErrors.len == 0 or errs.len < bestErrors.len:
        bestErrors = errs
    if not matched:
      for e in bestErrors:
        ctx.addViolation info, tag, e

  # Recurse into children:
  ctx.parents.add tag
  var child = c
  inc child
  var idx = 0
  while child.kind != ParRi:
    let childInType = kidsTypeByThisTag or (idx in typeSlots)
    checkTree(ctx, child, tag, idx, childInType)
    inc idx
  discard ctx.parents.pop()
  skip c

proc checkTree(ctx: var ValidatorCtx; c: var Cursor;
               parentTag: string; childIdx: int; inType: bool) =
  case c.kind
  of ParLe:
    checkParLe(ctx, c, parentTag, childIdx, inType)
  of ParRi:
    discard
  else:
    inc c

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

proc validate*(buf: var TokenBuf; phase: Phase;
               maxViolations = 200): seq[Violation] =
  ## Validate `buf` against `phase`. `buf` must hold a single rooted tree
  ## (conventionally `(stmts ...)`). Returns all violations found.
  var ctx = ValidatorCtx(phase: phase, maxViolations: maxViolations)
  var c = beginRead(buf)
  let info = c.info
  if c.kind == ParLe:
    checkTree(ctx, c, "", 0, false)
  # After the root subtree has been consumed, the cursor must be exhausted.
  # Anything remaining is content that a pass walking the IR with nested
  # counters / recursive descent would silently ignore — a spec-level bug the
  # validator has a duty to surface. We use `hasCurrentToken` here (and only
  # here) exactly for this purpose.
  if hasCurrentToken(c):
    ctx.addViolation info, "",
      "trailing content after root subtree: " &
      "the pass would silently drop these tokens"
  endRead(buf)
  result = move ctx.violations

proc reportViolations*(phaseName: string; vs: openArray[Violation]): bool =
  ## Print violations in the standard compiler format; returns true if any.
  ## Writes to stderr so it doesn't collide with a subprocess's NIF stdout.
  for v in vs:
    stderr.writeLine infoToStr(v.info) & " [" & phaseName &
      "] in " & v.parents & ": (" & v.tag & ") " & v.msg
  result = vs.len > 0

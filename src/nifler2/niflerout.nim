#
#
#           Nifler2: Nim to NIF
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Writes the parsed buffer as nifler writes it, byte for byte.
##
## `nifcoreparse`'s writer is the canonical NIF one and cannot be used: it
## gives a token without line info the info of the token before it, and it
## writes an index. nifler's rules are simpler and they are what the rest of
## the toolchain has seen so far:
##
## * The header is `(.nif27)`, `(.vendor "Nifler")`, `(.dialect "nim-parsed")`
##   and there is no index.
## * A position is written relative to the enclosing tag and left out when it
##   is the same. A token that has no position of its own -- a `.`, the `x`
##   export marker -- writes none and so has its tag's. nifler computes the
##   difference against the node's parent in *Nim's* AST, which is not always
##   the enclosing NIF tag; but a written difference decodes to exactly one
##   absolute position, so storing the position nifler's output decodes to and
##   encoding it against the NIF parent gives the same bytes.
## * The root carries the file, relative to the current directory
##   (`--portablePaths`, which is how nimony runs nifler).
## * `addEmpty(n)` writes `n` dots without separators, and bridge.nim calls it
##   with `n > 1` in exactly three places: the first four and the last two
##   slots of a `proctype`/`itertype`, the four empty slots of a `for` loop's
##   tuple variable, and the type and value slots of every other `let` that
##   unpacks a tuple.

import std / [syncio, assertions]
import ".." / lib / [nifbuilder, nifpools]
import parserrt

proc writeIfChanged(path, content: string) {.raises.} =
  ## nifler's `OnlyIfChanged`: an output that would come out the same is left
  ## alone, so its modification time does not change and nimony does not
  ## re-run `nimsem` after a `touch` or a comment-only edit.
  var old = ""
  try:
    old = readFile(path)
  except:
    old = ""                 # no previous output
  if old != content or old.len == 0:
    writeFile(path, content)

type
  Writer = object
    b: Builder
    file: string          ## the root's file, as it is written
    rootFile: FileId
    pool: Pool            ## where a cell's names were interned
    tags: TagPool         ## where a cell's `TagId` gets its spelling
    noLineInfo: bool      ## the deps file carries no positions

proc lineInfo(w: var Writer; info, reference: NifLineInfo) =
  ## bridge.nim's `relLineInfo`: absolute with the file at the root, else the
  ## difference to the reference, and nothing when that is zero.
  if w.noLineInfo or not info.file.isValid: return
  if not reference.file.isValid or info.file != reference.file:
    # absolute, with the file: the root, and an empty node's `???`
    let name = if info.file == w.rootFile: w.file else: w.pool.filenames[info.file]
    # an empty node's position is `unknownLineInfo`, column -1; nifcore
    # clamps a stored column at 0, so it is restored here
    let col = if name == "???": -1'i32 else: info.col
    w.b.attachLineInfo(col, info.line, name)
  else:
    w.b.attachLineInfo(info.col - reference.col, info.line - reference.line, "")

proc tagName(w: Writer; n: Node): string {.inline.} = w.tags.tags[tag(n)]

proc tagKind(n: Node): NiflerKind {.inline.} =
  ## Every tag in the tree came from a `NiflerKind` (`parserrt.tagId`).
  cast[NiflerKind](tag(n))

type
  EmptyRuns = object
    ## Where `addEmpty(n)` wrote more than one dot in a single call. Two slots
    ## are enough: `emptyRuns` never marks more, and every other child index
    ## is one dot of its own.
    s1, n1, s2, n2: int

proc runAt(r: EmptyRuns; i: int): int =
  ## How many dots the call starting at child `i` wrote; 0 when `i` was
  ## swallowed by an earlier run. The runs are tested before the ranges they
  ## span, because a later run may start inside an earlier one.
  if i == r.s1: r.n1
  elif i == r.s2: r.n2
  elif i > r.s1 and i < r.s1 + r.n1: 0
  elif i > r.s2 and i < r.s2 + r.n2: 0
  else: 1


proc emptyRuns(n: Node; tag, parentTag: NiflerKind; inForTuple: bool): EmptyRuns =
  result = EmptyRuns(s1: -1, n1: 0, s2: -1, n2: 0)
  case tag
  of ProctypeL, ItertypeL:
    let kids = childCount(n)
    if kids >= 4:
      result.s1 = 0
      result.n1 = 4
    if kids >= 2:
      result.s2 = kids - 2
      result.n2 = 2
  of LetL, VarL, ConstL:           # the section of a tuple unpacking
    if parentTag == UnpacktupL and inForTuple:
      if childCount(n) == 5:
        result.s1 = 1
        result.n1 = 4
    elif parentTag == UnpacktupL or parentTag == UnpackflatL:
      if childCount(n) == 5:
        result.s1 = 3
        result.n1 = 2
  else: discard

proc isDecl(k: NiflerKind): bool =
  case k
  of TypeL, VarL, LetL, ConstL, FldL, ParamL, TypevarL, ProcL, FuncL,
     IteratorL, MethodL, MacroL, TemplateL, ConverterL: true
  else: false

proc isRoutine(k: NiflerKind): bool =
  case k
  of ProcL, FuncL, IteratorL, MethodL, MacroL, TemplateL, ConverterL,
     ProctypeL, ItertypeL, DoL: true
  else: false

proc emit(w: var Writer; n: Node; reference: NifLineInfo;
          parentTag: NiflerKind; inForTuple, forLoop: bool) =
  ## `reference` is what bridge.nim computes this node's position against: its
  ## parent in Nim's AST, which is usually but not always the enclosing tag.
  let info = n.info
  case kind(n)
  of TagLit:
    let tg = tagKind(n)
    let first = n.down
    # `nkLambda` writes its position after the name placeholder:
    # `(proc .@5,1 . . . (params) ...)`
    let lambda = tg == ProcL and first != nil and kind(first) == DotToken
    # `nkStmtListExpr` writes none on `expr` and its own on the `stmts`
    let stmtListExpr = tg == ExprL
    w.b.addTree tagName(w, n)
    if not lambda and not stmtListExpr:
      w.lineInfo(info, reference)
    let own = if info.isValid: info else: reference
    let childInForTuple = tg == UnpacktupL and forLoop
    let childForLoop = tg == ForL or tg == UnpackflatL
    let routine = isRoutine(tg)
    let runs = emptyRuns(n, tg, parentTag, inForTuple)
    var i = 0
    var prevParams = NoLineInfo
    var k = n.down
    while k != nil:
      # the child's reference in bridge.nim
      var childRef = own
      if routine and prevParams.isValid:
        childRef = prevParams          # the result type is `n[0]` of the params
      elif tg == TupleL:
        childRef = reference           # `(kv` uses the tuple's own parent
      elif stmtListExpr and i == 0:
        childRef = reference
      prevParams = NoLineInfo
      if kind(k) == Ident and i == 1 and isDecl(tg) and
          w.pool.strings[strId(k)] == "x":
        w.b.addRaw " x"                # bridge.nim's export marker, verbatim:
                                       # a space even after a `)`
      elif kind(k) == DotToken:
        let dots = runAt(runs, i)
        if dots > 0:
          w.b.addEmpty dots
          if lambda and i == 0: w.lineInfo(info, reference)
      else:
        if routine and kind(k) == TagLit and tagKind(k) == ParamsL:
          let pinfo = k.info
          emit(w, k, childRef, tg, childInForTuple, childForLoop)
          prevParams = pinfo
        else:
          emit(w, k, childRef, tg, childInForTuple, childForLoop)
      inc i
      k = k.next
    w.b.endTree()
  of DotToken:
    w.b.addEmpty
  of Ident:
    w.b.addIdent w.pool.strings[strId(n)]
    w.lineInfo(info, reference)
  of StrLit:
    w.b.addStrLit w.pool.strings[strId(n)]
    w.lineInfo(info, reference)
  of CharLit:
    w.b.addCharLit charVal(n)
    w.lineInfo(info, reference)
  of IntLit:
    w.b.addIntLit intVal(n)
    w.lineInfo(info, reference)
  of UIntLit:
    w.b.addUIntLit uintVal(n)
    w.lineInfo(info, reference)
  of FloatLit:
    # nifbuilder puts the position inside `(inf)` itself
    if info.isValid and reference.isValid:
      w.b.addFloatLit(floatVal(n), info.col - reference.col, info.line - reference.line)
    else:
      w.b.addFloatLit floatVal(n)
  else:
    assert false, "nifler2 writes no " & $kind(n)

proc writeNifler*(first: Node; pool: Pool; tags: TagPool; sizeHint: int;
                  outfile, file: string) {.raises.} =
  ## `file` is the module's path as nifler writes it: relative to the current
  ## directory.
  var w = Writer(b: nifbuilder.open(sizeHint), file: file, pool: pool, tags: tags)
  w.b.addHeader "Nifler", "nim-parsed"
  if first != nil: w.rootFile = first.info.file
  var n = first
  while n != nil:
    emit(w, n, NoLineInfo, NiflerKind.None, false, false)
    n = n.next
  writeIfChanged(outfile, w.b.extract())

# --------------------------------------------------------------- deps file
#
# `nifler --deps` writes a second file next to the parsed module: the module's
# dependencies, which is what nimony's build graph is made of. bridge.nim
# emits it while it translates, into a second builder with positions off;
# everything it needs is in the finished tree, so here it is a walk over that.
#
# * `import`, `importexcept`, `fromimport`, `include`, `export` and
#   `exportexcept` are copied wherever they occur, a proc body included.
# * Inside the branches of a `when` each of them carries a `(when COND...)`
#   marker right after its tag: the conditions of every enclosing branch, an
#   `else` branch contributing `(prefix not COND)` for each earlier condition
#   -- so the dependency scanner can skip what is statically dead.
# * `{.plugin: "name".}` is `(plugin (when...)? "name")`: a program the build
#   has to produce before the module can be checked.
# * Nothing inside `runnableExamples` counts.

proc isDep(k: NiflerKind): bool =
  case k
  of ImportL, ImportexceptL, FromimportL, IncludeL, ExportL, ExportexceptL: true
  else: false

type
  WhenCond = object
    cond: Node
    negated: bool

  DepsWalker = object
    w: Writer
    conds: seq[WhenCond]

proc whenMarker(d: var DepsWalker) =
  if d.conds.len == 0: return
  d.w.b.addTree "when"
  for entry in d.conds:
    if entry.negated:
      d.w.b.addTree "prefix"
      d.w.b.addIdent "not"
      emit(d.w, entry.cond, NoLineInfo, NiflerKind.None, false, false)
      d.w.b.endTree()
    else:
      emit(d.w, entry.cond, NoLineInfo, NiflerKind.None, false, false)
  d.w.b.endTree()

proc isPlugin(d: DepsWalker; kv: Node; name: var string): bool =
  ## `plugin: "name"`; a raw or triple-quoted string is written plain.
  if kind(kv) != TagLit or tagKind(kv) != KvL: return false
  let k = kv.down
  if k == nil or kind(k) != Ident or d.w.pool.strings[strId(k)] != "plugin":
    return false
  let v = k.next
  if v == nil: return false
  if kind(v) == StrLit:
    name = d.w.pool.strings[strId(v)]
    return true
  if kind(v) == TagLit and tagKind(v) == SufL:
    let s = v.down
    if s != nil and kind(s) == StrLit:
      name = d.w.pool.strings[strId(s)]
      return true
  false

proc walkDeps(d: var DepsWalker; n: Node; inObject: bool) =
  if kind(n) != TagLit: return
  let tg = tagKind(n)
  if isDep(tg):
    d.w.b.addTree tagName(d.w, n)
    d.whenMarker()
    var k = n.down
    while k != nil:
      emit(d.w, k, NoLineInfo, tg, false, false)
      k = k.next
    d.w.b.endTree()
    return
  if tg == CallL or tg == CmdL:
    let f = n.down
    if f != nil and kind(f) == Ident and
        d.w.pool.strings[strId(f)] == "runnableExamples": return
  if tg == PragmasL:
    var k = n.down
    while k != nil:
      var name = ""
      if isPlugin(d, k, name):
        d.w.b.addTree "plugin"
        d.whenMarker()
        d.w.b.addStrLit name
        d.w.b.endTree()
      k = k.next
    return
  if tg == WhenL and not inObject:
    # `nkWhenStmt`: each branch's condition covers its body, and an `else`
    # is covered by the negation of all of them
    var prior: seq[Node] = @[]
    var br = n.down
    while br != nil:
      if kind(br) == TagLit and tagKind(br) == ElifL:
        let cond = br.down
        if cond != nil:
          walkDeps(d, cond, inObject)
          d.conds.add WhenCond(cond: cond, negated: false)
          var k = cond.next
          while k != nil:
            walkDeps(d, k, inObject)
            k = k.next
          d.conds.setLen d.conds.len - 1
          prior.add cond
      else:
        for cond in prior: d.conds.add WhenCond(cond: cond, negated: true)
        walkDeps(d, br, inObject)
        d.conds.setLen d.conds.len - prior.len
      br = br.next
    return
  let nowInObject = inObject or tg == ObjectL
  var k = n.down
  while k != nil:
    walkDeps(d, k, nowInObject)
    k = k.next

proc writeDeps*(first: Node; pool: Pool; tags: TagPool;
                outfile: string) {.raises.} =
  var d = DepsWalker(w: Writer(b: nifbuilder.open(1024), pool: pool, tags: tags,
                               noLineInfo: true),
                     conds: @[])
  d.w.b.addHeader "Nifler", "nim-deps"
  d.w.b.addTree "stmts"
  var n = first
  while n != nil:
    walkDeps(d, n, false)
    n = n.next
  d.w.b.endTree()
  writeIfChanged(outfile, d.w.b.extract())

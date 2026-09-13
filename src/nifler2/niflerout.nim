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
    noLineInfo: bool      ## the deps file carries no positions

proc lineInfo(w: var Writer; info, reference: NifLineInfo) =
  ## bridge.nim's `relLineInfo`: absolute with the file at the root, else the
  ## difference to the reference, and nothing when that is zero.
  if w.noLineInfo or not info.file.isValid: return
  if not reference.file.isValid or info.file != reference.file:
    # absolute, with the file: the root, and an empty node's `???`
    let name = if info.file == w.rootFile: w.file else: pool.filenames[info.file]
    # an empty node's position is `unknownLineInfo`, column -1; nifcore
    # clamps a stored column at 0, so it is restored here
    let col = if name == "???": -1'i32 else: info.col
    w.b.attachLineInfo(col, info.line, name)
  else:
    w.b.attachLineInfo(info.col - reference.col, info.line - reference.line, "")

proc tagName(c: Cursor): string {.inline.} = c.tags.tags[resolvedTagId(c)]

proc tagKind(c: Cursor): NiflerKind {.inline.} =
  ## Every tag in the buffer came from a `NiflerKind` (`parserrt.tagId`).
  cast[NiflerKind](resolvedTagId(c))

proc emptyRuns(tag, parentTag: NiflerKind; inForTuple: bool; kids: int): seq[int] =
  ## For each child index, how many dots `addEmpty` wrote in one call starting
  ## there (0: part of an earlier run).
  result = newSeq[int](kids)
  for i in 0 ..< kids: result[i] = 1
  proc run(r: var seq[int]; start, n: int) =
    if start + n <= r.len:
      r[start] = n
      for i in start+1 ..< start+n: r[i] = 0
  case tag
  of ProctypeL, ItertypeL:
    if kids > 0:
      run(result, 0, 4)
      run(result, kids - 2, 2)
  of LetL, VarL, ConstL:           # the section of a tuple unpacking
    if parentTag == UnpacktupL and inForTuple:
      if kids == 5: run(result, 1, 4)
    elif parentTag == UnpacktupL or parentTag == UnpackflatL:
      if kids == 5: run(result, 3, 2)
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

proc emit(w: var Writer; c: var Cursor; reference: NifLineInfo;
          parentTag: NiflerKind; inForTuple, forLoop: bool) =
  ## `reference` is what bridge.nim computes this node's position against: its
  ## parent in Nim's AST, which is usually but not always the enclosing tag.
  let info = rawLineInfo(c)
  case c.kind
  of TagLit:
    let tag = tagKind(c)
    var kids: seq[NifKind] = @[]
    var k = childCursor(c)
    while k.hasMore:
      kids.add k.kind
      skip k
    # `nkLambda` writes its position after the name placeholder:
    # `(proc .@5,1 . . . (params) ...)`
    let lambda = tag == ProcL and kids.len > 0 and kids[0] == DotToken
    # `nkStmtListExpr` writes none on `expr` and its own on the `stmts`
    let stmtListExpr = tag == ExprL
    w.b.addTree tagName(c)
    if not lambda and not stmtListExpr:
      w.lineInfo(info, reference)
    let own = if info.isValid: info else: reference
    let childInForTuple = tag == UnpacktupL and forLoop
    let childForLoop = tag == ForL or tag == UnpackflatL
    let routine = isRoutine(tag)
    let runs = emptyRuns(tag, parentTag, inForTuple, kids.len)
    var i = 0
    var prevParams = NoLineInfo
    c.into:
      while c.hasMore:
        # the child's reference in bridge.nim
        var childRef = own
        if routine and prevParams.isValid:
          childRef = prevParams          # the result type is `n[0]` of the params
        elif tag == TupleL:
          childRef = reference           # `(kv` uses the tuple's own parent
        elif stmtListExpr and i == 0:
          childRef = reference
        prevParams = NoLineInfo
        if c.kind == Ident and i == 1 and isDecl(tag) and strVal(c) == "x":
          w.b.addRaw " x"                # bridge.nim's export marker, verbatim:
                                         # a space even after a `)`
          c.inc
        elif c.kind == DotToken:
          if runs[i] > 0:
            w.b.addEmpty runs[i]
            if lambda and i == 0: w.lineInfo(info, reference)
          c.inc
        else:
          if routine and c.kind == TagLit and tagKind(c) == ParamsL:
            let pinfo = rawLineInfo(c)
            emit(w, c, childRef, tag, childInForTuple, childForLoop)
            prevParams = pinfo
          else:
            emit(w, c, childRef, tag, childInForTuple, childForLoop)
        inc i
    w.b.endTree()
  of DotToken:
    w.b.addEmpty
    c.inc
  of Ident:
    w.b.addIdent strVal(c)
    w.lineInfo(info, reference)
    c.inc
  of StrLit:
    w.b.addStrLit strVal(c)
    w.lineInfo(info, reference)
    c.inc
  of CharLit:
    w.b.addCharLit charLit(c)
    w.lineInfo(info, reference)
    c.inc
  of IntLit:
    w.b.addIntLit intVal(c)
    w.lineInfo(info, reference)
    c.inc
  of UIntLit:
    w.b.addUIntLit uintVal(c)
    w.lineInfo(info, reference)
    c.inc
  of FloatLit:
    # nifbuilder puts the position inside `(inf)` itself
    if info.isValid and reference.isValid:
      w.b.addFloatLit(floatVal(c), info.col - reference.col, info.line - reference.line)
    else:
      w.b.addFloatLit floatVal(c)
    c.inc
  else:
    assert false, "nifler2 writes no " & $c.kind
    c.inc

proc writeNifler*(buf: var TokenBuf; outfile, file: string) {.raises.} =
  ## `file` is the module's path as nifler writes it: relative to the current
  ## directory.
  var w = Writer(b: nifbuilder.open(buf.len * 8), file: file)
  w.b.addHeader "Nifler", "nim-parsed"
  var c = beginRead(buf)
  w.rootFile = rawLineInfo(c).file
  while c.hasMore:
    emit(w, c, NoLineInfo, NiflerKind.None, false, false)
  endRead c
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
    cond: Cursor
    negated: bool

  DepsWalker = object
    w: Writer
    conds: seq[WhenCond]

proc whenMarker(d: var DepsWalker) =
  if d.conds.len == 0: return
  d.w.b.addTree "when"
  for entry in d.conds:
    var c = entry.cond
    if entry.negated:
      d.w.b.addTree "prefix"
      d.w.b.addIdent "not"
      emit(d.w, c, NoLineInfo, NiflerKind.None, false, false)
      d.w.b.endTree()
    else:
      emit(d.w, c, NoLineInfo, NiflerKind.None, false, false)
  d.w.b.endTree()

proc firstChild(c: Cursor): Cursor {.inline.} = childCursor(c)

proc isPlugin(kv: Cursor; name: var string): bool =
  ## `plugin: "name"`; a raw or triple-quoted string is written plain.
  if kv.kind != TagLit or tagKind(kv) != KvL: return false
  var k = childCursor(kv)
  if k.kind != Ident or strVal(k) != "plugin": return false
  skip k
  if not k.hasMore: return false
  if k.kind == StrLit:
    name = strVal(k)
    return true
  if k.kind == TagLit and tagKind(k) == SufL:
    let s = childCursor(k)
    if s.kind == StrLit:
      name = strVal(s)
      return true
  false

proc walkDeps(d: var DepsWalker; c: Cursor; inObject: bool) =
  if c.kind != TagLit: return
  let tag = tagKind(c)
  if isDep(tag):
    d.w.b.addTree tagName(c)
    d.whenMarker()
    var k = childCursor(c)
    while k.hasMore:
      emit(d.w, k, NoLineInfo, tag, false, false)
    d.w.b.endTree()
    return
  if tag == CallL or tag == CmdL:
    let f = firstChild(c)
    if f.hasMore and f.kind == Ident and strVal(f) == "runnableExamples": return
  if tag == PragmasL:
    var k = childCursor(c)
    while k.hasMore:
      var name = ""
      if isPlugin(k, name):
        d.w.b.addTree "plugin"
        d.whenMarker()
        d.w.b.addStrLit name
        d.w.b.endTree()
      skip k
    return
  if tag == WhenL and not inObject:
    # `nkWhenStmt`: each branch's condition covers its body, and an `else`
    # is covered by the negation of all of them
    var prior: seq[Cursor] = @[]
    var br = childCursor(c)
    while br.hasMore:
      if br.kind == TagLit and tagKind(br) == ElifL:
        var k = childCursor(br)
        let cond = k
        walkDeps(d, k, inObject)
        skip k
        d.conds.add WhenCond(cond: cond, negated: false)
        while k.hasMore:
          walkDeps(d, k, inObject)
          skip k
        d.conds.setLen d.conds.len - 1
        prior.add cond
      else:
        for cond in prior: d.conds.add WhenCond(cond: cond, negated: true)
        walkDeps(d, br, inObject)
        d.conds.setLen d.conds.len - prior.len
      skip br
    return
  let nowInObject = inObject or tag == ObjectL
  var k = childCursor(c)
  while k.hasMore:
    walkDeps(d, k, nowInObject)
    skip k

proc writeDeps*(buf: var TokenBuf; outfile: string) {.raises.} =
  var d = DepsWalker(w: Writer(b: nifbuilder.open(1024), noLineInfo: true))
  d.w.b.addHeader "Nifler", "nim-deps"
  d.w.b.addTree "stmts"
  var c = beginRead(buf)
  while c.hasMore:
    walkDeps(d, c, false)
    skip c
  endRead c
  d.w.b.endTree()
  writeIfChanged(outfile, d.w.b.extract())

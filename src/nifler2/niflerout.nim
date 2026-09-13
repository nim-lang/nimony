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

type
  Writer = object
    b: Builder
    file: string          ## the root's file, as it is written
    rootFile: FileId

proc lineInfo(w: var Writer; info, reference: NifLineInfo) =
  ## bridge.nim's `relLineInfo`: absolute with the file at the root, else the
  ## difference to the reference, and nothing when that is zero.
  if not info.file.isValid: return
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

proc emptyRuns(tag, parentTag: string; kids: int): seq[int] =
  ## For each child index, how many dots `addEmpty` wrote in one call starting
  ## there (0: part of an earlier run).
  result = newSeq[int](kids)
  for i in 0 ..< kids: result[i] = 1
  proc run(r: var seq[int]; start, n: int) =
    if start + n <= r.len:
      r[start] = n
      for i in start+1 ..< start+n: r[i] = 0
  case tag
  of "proctype", "itertype":
    if kids > 0:
      run(result, 0, 4)
      run(result, kids - 2, 2)
  of "let", "var", "const":        # the section of a tuple unpacking
    case parentTag
    of "unpacktup", "unpackflat":
      if kids == 5: run(result, 3, 2)
    of "fortuple":
      if kids == 5: run(result, 1, 4)
    else: discard
  else: discard

const DeclTags = ["type", "var", "let", "const", "fld", "param", "typevar",
                  "proc", "func", "iterator", "method", "macro", "template",
                  "converter"]

const RoutineTags = ["proc", "func", "iterator", "method", "macro", "template",
                     "converter", "proctype", "itertype", "do"]

proc emit(w: var Writer; c: var Cursor; reference: NifLineInfo; parentTag: string;
          forLoop: bool) =
  ## `reference` is what bridge.nim computes this node's position against: its
  ## parent in Nim's AST, which is usually but not always the enclosing tag.
  let info = rawLineInfo(c)
  case c.kind
  of TagLit:
    let tag = tagName(c)
    var kids: seq[NifKind] = @[]
    var k = childCursor(c)
    while k.hasMore:
      kids.add k.kind
      skip k
    # `nkLambda` writes its position after the name placeholder:
    # `(proc .@5,1 . . . (params) ...)`
    let lambda = tag == "proc" and kids.len > 0 and kids[0] == DotToken
    # `nkStmtListExpr` writes none on `expr` and its own on the `stmts`
    let stmtListExpr = tag == "expr"
    w.b.addTree tag
    if not lambda and not stmtListExpr:
      w.lineInfo(info, reference)
    let own = if info.isValid: info else: reference
    let childParent = if tag == "unpacktup" and forLoop: "fortuple" else: tag
    let runs = emptyRuns(tag, parentTag, kids.len)
    var i = 0
    var prevParams = NoLineInfo
    c.into:
      while c.hasMore:
        # the child's reference in bridge.nim
        var childRef = own
        if tag in RoutineTags and prevParams.isValid:
          childRef = prevParams          # the result type is `n[0]` of the params
        elif tag == "tuple":
          childRef = reference           # `(kv` uses the tuple's own parent
        elif stmtListExpr and i == 0:
          childRef = reference
        prevParams = NoLineInfo
        if c.kind == Ident and i == 1 and tag in DeclTags and strVal(c) == "x":
          w.b.addRaw " x"                # bridge.nim's export marker, verbatim:
                                         # a space even after a `)`
          c.inc
        elif c.kind == DotToken:
          if runs[i] > 0:
            w.b.addEmpty runs[i]
            if lambda and i == 0: w.lineInfo(info, reference)
          c.inc
        else:
          if c.kind == TagLit and tagName(c) == "params" and tag in RoutineTags:
            let pinfo = rawLineInfo(c)
            emit(w, c, childRef, childParent, tag in ["for", "unpackflat"])
            prevParams = pinfo
          else:
            emit(w, c, childRef, childParent, tag in ["for", "unpackflat"])
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
    emit(w, c, NoLineInfo, "", false)
  endRead c
  writeFile(outfile, w.b.extract())

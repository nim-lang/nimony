#       Nifler2
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Tree diff between `nifler` and `nifler2`.
##
## `parsesweep.sh` measures whether nifler2 *accepts* a file; this measures
## whether it builds the *same tree*. Line information is ignored -- it is the
## last thing to get right and would otherwise drown every structural
## difference -- and so are the header directives.
##
##   nim c -o:bin/treediff src/nifler2/tools/treediff.nim
##   bin/treediff file.nim                  # one file, every difference
##   bin/treediff --sweep lib src tests     # whole trees, grouped
##
## The comparison is a walk over both trees in step. When the children of a
## node stop lining up, the difference is recorded and the *rest of that
## node* is skipped on both sides, so one mismatch does not turn every
## following token into a report -- but the node's siblings are still
## compared, so a file with three unrelated bugs reports three.
##
## The sweep groups differences by a signature: the enclosing tag and the
## shape of what each side has at that position. That is what makes the
## fidelity work enumerable in the same way the parse sweep made acceptance
## work enumerable.

import std / [os, osproc, strutils, tables, algorithm, formatfloat, syncio]
import ".." / ".." / lib / [nifreader, stringviews]

type
  Tok = object
    kind: NifKind
    text: string          ## tag name for `ParLe`, decoded value otherwise

  Diff* = object
    path: string          ## tags from the root to the enclosing node
    want, got: string     ## short renderings, nifler first
    sig: string           ## what the sweep groups by

proc readTree(filename: string): seq[Tok] =
  ## The first top-level tree of a NIF file, without line info. nifler2
  ## writes a `(.index ...)` after it that nifler does not, which is why this
  ## stops at the matching `)` instead of at EOF.
  result = @[]
  var r = nifreader.open(filename)
  var t = default(ExpandedToken)
  var depth = 0
  while true:
    r.next(t)
    case t.tk
    of EofToken, UnknownToken: break
    of ParLe:
      inc depth
      result.add Tok(kind: ParLe, text: $t.data)
    of ParRi:
      result.add Tok(kind: ParRi)
      dec depth
      if depth == 0: break
    of DotToken: result.add Tok(kind: DotToken)
    of Ident, Symbol, SymbolDef, StringLit:
      result.add Tok(kind: t.tk, text: decodeStr(r, t))
    of CharLit: result.add Tok(kind: CharLit, text: $decodeChar(t))
    of IntLit: result.add Tok(kind: IntLit, text: $decodeInt(t))
    of UIntLit: result.add Tok(kind: UIntLit, text: $decodeUInt(t))
    of FloatLit: result.add Tok(kind: FloatLit, text: $decodeFloat(t))
    else: result.add Tok(kind: t.tk, text: $t.data)
  r.close()
  if depth != 0:
    # an unbalanced tree is an output bug in its own right; say so instead of
    # letting the walk run off the end
    raise newException(ValueError, filename & ": unbalanced tree (depth " &
                       $depth & " at end)")

proc skipNode(s: seq[Tok]; i: int): int =
  ## Index just past the node that starts at `i`.
  if s[i].kind != ParLe: return i + 1
  var depth = 0
  result = i
  while true:
    if s[result].kind == ParLe: inc depth
    elif s[result].kind == ParRi:
      dec depth
      if depth == 0: return result + 1
    inc result

proc render(s: seq[Tok]; i: int; limit = 70): string =
  ## A node as text, cut at `limit` characters.
  result = ""
  if i >= s.len: return "<end>"
  if s[i].kind == ParRi: return ")"
  let e = skipNode(s, i)
  for k in i ..< e:
    if result.len >= limit:
      result.add " …"
      break
    case s[k].kind
    of ParLe:
      if result.len > 0 and result[^1] != '(': result.add ' '
      result.add "(" & s[k].text
    of ParRi: result.add ")"
    else:
      if result.len > 0 and result[^1] != '(': result.add ' '
      case s[k].kind
      of DotToken: result.add "."
      of StringLit: result.add escape(s[k].text)
      of CharLit: result.add "'" & s[k].text & "'"
      else: result.add s[k].text

proc shape(s: seq[Tok]; i: int): string =
  ## What kind of thing sits at `i`, coarse enough to group by: a tag is its
  ## name, an atom is its kind -- `x` and `y` are the same difference.
  if i >= s.len: return "<end>"
  case s[i].kind
  of ParLe: "(" & s[i].text
  of ParRi: ")"
  of DotToken: "."
  of Ident: "ident"
  of Symbol, SymbolDef: "sym"
  of StringLit: "str"
  of CharLit: "char"
  of IntLit, UIntLit: "int"
  of FloatLit: "float"
  else: $s[i].kind

proc sameAtom(a, b: Tok): bool = a.kind == b.kind and a.text == b.text

proc compare(a: seq[Tok]; ai: var int; b: seq[Tok]; bi: var int;
             path: string; diffs: var seq[Diff]): bool =
  ## Compares the node at `a[ai]` with the node at `b[bi]` and advances both
  ## past it. Returns false when the two nodes are not even the same kind of
  ## node -- that is what makes the following siblings unreliable. A
  ## difference *inside* a node that otherwise matches does not.
  proc record(path: string; a: seq[Tok]; ai: int; b: seq[Tok]; bi: int;
              diffs: var seq[Diff]) =
    let parent = if path.len == 0: "<root>" else: path.rsplit('/', 1)[^1]
    diffs.add Diff(path: path, want: render(a, ai), got: render(b, bi),
                   sig: "in (" & parent & "): nifler " & shape(a, ai) &
                        "  nifler2 " & shape(b, bi))

  if a[ai].kind == ParLe and b[bi].kind == ParLe and a[ai].text == b[bi].text:
    let here = (if path.len == 0: a[ai].text else: path & "/" & a[ai].text)
    inc ai
    inc bi
    result = true
    while a[ai].kind != ParRi and b[bi].kind != ParRi:
      if not compare(a, ai, b, bi, here, diffs):
        # the children no longer line up; the siblings of this node still do
        while a[ai].kind != ParRi: ai = skipNode(a, ai)
        while b[bi].kind != ParRi: bi = skipNode(b, bi)
    if a[ai].kind != b[bi].kind:
      record(here, a, ai, b, bi, diffs)   # one side has children left
      while a[ai].kind != ParRi: ai = skipNode(a, ai)
      while b[bi].kind != ParRi: bi = skipNode(b, bi)
    inc ai
    inc bi
  elif a[ai].kind != ParLe and b[bi].kind != ParLe and sameAtom(a[ai], b[bi]):
    inc ai
    inc bi
    result = true
  else:
    result = false
    record(path, a, ai, b, bi, diffs)
    ai = skipNode(a, ai)
    bi = skipNode(b, bi)

proc diffFiles*(niflerOut, nifler2Out: string): seq[Diff] =
  result = @[]
  let a = readTree(niflerOut)
  let b = readTree(nifler2Out)
  if a.len == 0 or b.len == 0:
    result.add Diff(path: "", want: $a.len & " tokens", got: $b.len & " tokens",
                    sig: "empty output")
    return
  var ai = 0
  var bi = 0
  discard compare(a, ai, b, bi, "", result)

type
  Outcome = enum
    Same, Different, Nifler2Rejects, BothReject, NiflerRejects,
    Unreadable            ## one side wrote NIF the reader cannot walk

proc run(tool, input, output: string): bool =
  let (_, code) = execCmdEx(quoteShell(tool) & " p " & quoteShell(input) & " " &
                            quoteShell(output))
  code == 0 and fileExists(output)

proc check(input, tmp: string; diffs: var seq[Diff]): Outcome =
  let o1 = tmp / "nifler.nif"
  let o2 = tmp / "nifler2.nif"
  removeFile o1
  removeFile o2
  let ok1 = run("bin/nifler", input, o1)
  let ok2 = run("bin/nifler2", input, o2)
  if not ok1 and not ok2: return BothReject
  if not ok1: return NiflerRejects
  if not ok2: return Nifler2Rejects
  try:
    diffs = diffFiles(o1, o2)
  except ValueError as e:
    diffs = @[Diff(path: "", want: "", got: e.msg, sig: "unreadable output")]
    return Unreadable
  result = if diffs.len == 0: Same else: Different

proc single(input: string) =
  let tmp = getTempDir() / "treediff"
  createDir tmp
  var diffs: seq[Diff] = @[]
  let r = check(input, tmp, diffs)
  case r
  of Same: echo "same tree"
  of BothReject: echo "both reject the file"
  of NiflerRejects: echo "nifler rejects the file, nifler2 accepts it"
  of Nifler2Rejects: echo "nifler2 rejects the file, nifler accepts it"
  of Unreadable: echo diffs[0].got
  of Different:
    for d in diffs:
      echo "at ", (if d.path.len == 0: "<root>" else: d.path)
      echo "  nifler:  ", d.want
      echo "  nifler2: ", d.got

proc sweep(dirs: seq[string]; top: int) =
  let tmp = getTempDir() / "treediff"
  createDir tmp
  var files: seq[string] = @[]
  for d in dirs:
    for f in walkDirRec(d):
      if f.endsWith(".nim"): files.add f
  sort files
  var counts: array[Outcome, int]
  var groups = initCountTable[string]()
  var byNode = initCountTable[string]()
  var nodeFiles = initTable[string, int]()
  var firstGroups = initCountTable[string]()
  var example = initTable[string, (string, Diff)]()
  var total = 0
  for f in files:
    var diffs: seq[Diff] = @[]
    let r = check(f, tmp, diffs)
    inc counts[r]
    case r
    of Unreadable: echo "unreadable: ", f, ": ", diffs[0].got
    of Nifler2Rejects: echo "rejected by nifler2 only: ", f
    of NiflerRejects: echo "rejected by nifler only: ", f
    else: discard
    if r == Different:
      total += diffs.len
      firstGroups.inc diffs[0].sig
      var seen: seq[string] = @[]
      for d in diffs:
        let node = if d.path.len == 0: "<root>" else: d.path.rsplit('/', 1)[^1]
        byNode.inc node
        if node notin seen:
          seen.add node
          nodeFiles[node] = nodeFiles.getOrDefault(node) + 1
        groups.inc d.sig
        if d.sig notin example: example[d.sig] = (f, d)
  echo counts[Same], " same, ", counts[Different], " different (", total,
       " differences), ", counts[Nifler2Rejects], " rejected by nifler2 only, ",
       counts[NiflerRejects], " by nifler only, ", counts[BothReject],
       " by both, ", counts[Unreadable], " unreadable"
  if byNode.len > 0:
    byNode.sort()
    echo "--- differences by enclosing node (count, files)"
    for node, n in byNode:
      echo align($n, 6), " ", align($nodeFiles[node], 5), "  (", node
  if groups.len > 0:
    groups.sort()
    firstGroups.sort()
    echo "--- differences by signature (count, files where it is the first)"
    var shown = 0
    for sig, n in groups:
      if shown >= top: break
      inc shown
      let (f, d) = example[sig]
      echo align($n, 6), " ", align($firstGroups.getOrDefault(sig), 5), "  ", sig
      echo "               e.g. ", f
      echo "               nifler:  ", d.want
      echo "               nifler2: ", d.got

proc main =
  var dirs: seq[string] = @[]
  var sweepMode = false
  var top = 25
  for i in 1 .. paramCount():
    let a = paramStr(i)
    if a == "--sweep": sweepMode = true
    elif a.startsWith("--top:"): top = parseInt(a.substr(6))
    else: dirs.add a
  if dirs.len == 0:
    quit "usage: treediff file.nim | treediff --sweep [--top:N] dir..."
  if sweepMode: sweep(dirs, top)
  else:
    for f in dirs: single(f)

main()

# Port of tests/gc/gctest.nim to Nimony.
# Threading, GC_fullCollect, GC_getStatistics and repr are omitted.

import std / [syncio, strutils, assertions]

type
  PNode = ref TNode
  TNode {.final.} = object
    le, ri: PNode
    data: string

  TTable {.final.} = object
    counter, max: int
    data: seq[string]

  TBNode {.final.} = object
    other: PNode  # a completely different tree
    data: string
    sons: seq[TBNode] # directly embedded!
    t: TTable

  TCaseKind = enum nkStr, nkWhole, nkList
  PCaseNode = ref TCaseNode
  TCaseNode {.final.} = object
    case kind: TCaseKind
    of nkStr: data: string
    of nkList: sons: seq[PCaseNode]
    else: unused: seq[string]

  TIdObj* = object of RootObj
    id*: int  # unique id; use this for comparisons and not the pointers

  PIdObj* = ref TIdObj
  PIdent* = ref TIdent
  TIdent* = object of TIdObj
    s*: string
    next*: PIdent             # for hash-table chaining
    h*: int                   # hash value of s

var
  flip: int

proc newCaseNode(data: string): PCaseNode =
  if flip == 0:
    result = PCaseNode(kind: nkStr, data: data)
  else:
    result = PCaseNode(kind: nkWhole, unused: @["", "abc", "abdc"])
  flip = 1 - flip

proc newCaseNode(a, b: PCaseNode): PCaseNode =
  result = PCaseNode(kind: nkList, sons: @[a, b])

proc caseTree(lvl: int = 0): PCaseNode =
  if lvl == 3: result = newCaseNode("data item")
  else: result = newCaseNode(caseTree(lvl+1), caseTree(lvl+1))

var
  id: int = 1

proc buildTree(depth = 1): PNode =
  if depth == 7: return nil
  new(result)
  result.le = buildTree(depth+1)
  result.ri = buildTree(depth+1)
  result.data = $id
  inc(id)

proc returnTree(): PNode =
  if id mod 10 == 0: writeLine(stdout, "creating id: " & $id)
  new(result)
  result.data = $id
  new(result.le)
  result.le.data = $id & ".1"
  new(result.ri)
  result.ri.data = $id & ".2"
  inc(id)

proc printTree(t: PNode) =
  if t == nil: return
  writeLine(stdout, "printing")
  writeLine(stdout, t.data)
  printTree(t.le)
  printTree(t.ri)

proc unsureNew(result: var PNode) =
  if id mod 10 == 0: writeLine(stdout, "creating unsure id: " & $id)
  new(result)
  result.data = $id
  new(result.le)
  result.le.data = $id & ".a"
  new(result.ri)
  result.ri.data = $id & ".b"
  inc(id)

proc setSons(n: var TBNode) =
  n.sons = @[] # free memory of the sons
  n.t.data = @[]
  # setLen on seq not supported; n.t.data is empty here so the loop is a no-op
  n.t.data = @[]

proc buildBTree(father: var TBNode) =
  father.data = "father"
  father.other = nil
  father.sons = @[]
  for i in 1..10:
    if i mod 5 == 0: write(stdout, "next iteration!\n")
    var n = TBNode()
    n.other = returnTree()
    n.data = "B node: " & $i
    if i mod 2 == 0: n.sons = @[] # nil and [] need to be handled correctly!
    add father.sons, n
    father.t.counter = 0
    father.t.max = 3
    father.t.data = @["ha", "lets", "stress", "it"]
  setSons(father)

proc getIdent(identifier: cstring, length: int, h: int): PIdent =
  new(result)
  result.h = h
  result.s = newString(length)

proc main() =
  discard getIdent("addr", 4, 0)
  discard getIdent("hall", 4, 0)
  discard getIdent("echo", 4, 0)
  discard getIdent("huch", 4, 0)

  var father = TBNode()
  for i in 1..1_00:
    buildBTree(father)

  for i in 1..1_00:
    var t = returnTree()
    var t2: PNode = nil
    unsureNew(t2)
  write(stdout, "now building bigger trees: ")
  var t2: PNode = nil
  for i in 1..100:
    t2 = buildTree()
  printTree(t2)
  write(stdout, "now test sequences of strings:")
  var s: seq[string] = @[]
  for i in 1..100:
    add s, "hohoho" # test reallocation
  writeLine(stdout, s[89])
  write(stdout, "done!\n")

var
  fatherAsGlobal: TBNode

proc start =
  var s: string = ""
  s = ""
  discard caseTree()
  var father = TBNode()
  father.t.data = @["ha", "lets", "stress", "it"]
  father.t.data = @["ha", "lets", "stress", "it"]
  var t = buildTree()
  var tg = buildTree()
  buildBTree(father)

  write(stdout, "starting main...\n")
  main()

  write(stdout, "finished\n")

fatherAsGlobal.t.data = @["ha", "lets", "stress", "it"]
buildBTree(fatherAsGlobal)
start()

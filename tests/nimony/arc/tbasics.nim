import std/[syncio, assertions]

block: # issue #24080
  var a = (s: "a")
  var b = "a"
  a.s.setLen 0
  b = a.s
  assert b == ""

block: # issue #24080, longer string
  var a = (s: "abc")
  var b = "abc"
  a.s.setLen 2
  b = a.s
  assert b == "ab"

proc f(): seq[int] =
  @[1, 2, 3]

proc main(cond: bool) =
  var a, b: seq[seq[int]] = @[]
  var x = f()
  if cond:
    a.add x
  else:
    b.add x

# all paths move 'x' so no wasMoved(x); destroy(x) pair should be left in the
# AST.

main(false)


proc tfor(cond: bool) =
  var a, b: seq[seq[int]] = @[]

  var x = f()

  for i in 0 ..< 4:
    if i == 2: return
    a.add x

  if cond:
    a.add x
  else:
    b.add x

tfor(false)

type
  Node = ref object
    le, ri: Node
    s: string

proc traverse(root: Node) =
  var it = root
  while it != nil:
    echo it.s
    it = it.ri

  var jt = root
  while jt != nil:
    let ri = jt.ri
    echo jt.s
    jt = ri

traverse(nil)

# XXX: This optimization is not sound

type
  ErrorNodeKind = enum Branch, Leaf
  Error = ref object
    case kind: ErrorNodeKind
      of Branch:
        left: Error
        right: Error
      of Leaf:
        leafError: string
    input: string

proc ret(input: string, lefterr, righterr: Error): Error =
  result = Error(kind: Branch, left: lefterr, right: righterr, input: input)

proc parser() =
  var rerrors: Error
  let lerrors = Error(
    kind: Leaf,
    leafError: "first error",
    input: "123 ;"
  )
  # If you remove "block" - everything works
  block:
    let rresult = Error(
      kind: Leaf,
      leafError: "second error",
      input: ";"
    )
    # this assignment is needed too
    rerrors = rresult

  # Returns Error(kind: Branch, left: lerrors, right: rerrors, input: "some val")
  # needs to be a proc call for some reason, can't inline the result
  var data = ret(input = "some val", lefterr = lerrors, righterr = rerrors)

  assert data.left.leafError == "first error"
  assert data.left.input == "123 ;"
  # stacktrace shows this line
  assert data.right.leafError == "second error"
  assert data.right.input == ";"
  assert data.input == "some val"

parser()

type
  MyEnum = enum
    One = 1

var x = cast[MyEnum](0)
let s = $x
assert s == ""

# bug #18002
type
  TTypeAttachedOp = enum
    attachedAsgn
    attachedSink
    attachedTrace

  PNode = ref object
    discard

proc genAddrOf(n: PNode) =
  assert n != nil, "moved?!"

proc atomicClosureOp =
  let x = PNode()

  genAddrOf:
    block:
      x

  case attachedTrace
  of attachedSink: discard
  of attachedAsgn: discard
  of attachedTrace: genAddrOf(x)

atomicClosureOp()

# misc
proc smoltest(x: bool): bool =
  result = false
  while true:
    if true: return x

discard smoltest(true)


# block: # bug #23627
type
  TestObj = object of RootObj

  Test2 = object of RootObj
    foo: TestObj

  TestTestObj = object of RootObj
    bar: TestObj

proc `=destroy`(x: TestTestObj) =
  echo "Destructor for TestTestObj"
  let test = Test2(foo: TestObj())

proc testCaseT() =
  let tt1 = TestTestObj(bar: TestObj())


proc mainxx() =
  testCaseT()

mainxx()



proc takeSink(x: sink string): bool = true

proc b(x: sink string): string =
  if takeSink(x):
    return x & "abc"
  else:
    result = ""

proc bbb(inp: string) =
  let y = inp & "xyz"
  assert b(y) == "123xyzabc"

bbb("123")

type
  Ref = ref object
    id: int

block:
  proc inc(x: sink Ref) =
    inc x.id


  proc foo =
    var x = Ref(id: 8)
    inc(x)
    inc(x)

  foo()


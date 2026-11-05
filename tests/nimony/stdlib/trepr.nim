import std/[assertions, repr, syncio]

type
  Color = enum
    red, green, blue

  Plain = object
    x: int
    y: string

  RefPlain = ref object
    value: int

  Box[T] = object
    value: T

  RefBox[T] = ref object
    value: T

  Node = object
    name: string
    next: RefBox[int]

  Empty = object

  E = enum
    ea, eb

  Inner = object
    case active: bool
    of true:
      iv: int
    of false:
      sv: string

  VarNode = object
    name: string
    case kind: Color
    of red:
      bits: int
    of green:
      inner: Inner
      text: string
    of blue:
      discard

  Maybe[T] = object
    case
    of None:
      discard
    of Some:
      val: T

  SharedSum = object
    shared: int
    case
    of First:
      a: int
    of Second:
      b: string
    of Third:
      discard

  RefSum = ref object
    case
    of RValue:
      val: int
    of RAdd:
      a, b: RefSum

  MyDistinct = distinct int
  DString = distinct string
  GenD[T] = distinct T
  MyRange = range[1..5]
  CharRange = range['a'..'z']

  BaseObj = object
    a: int

  DObj = distinct BaseObj
  DSeq = distinct seq[int]

  DistinctHolder = object
    value: MyDistinct

  A = object
    c: ptr int

  B = object
    c: pointer

let plain = Plain(x: 1, y: "a")
let refPlain = RefPlain(value: 2)
var nilRef: nil RefPlain
let box = Box[int](value: 3)
let refBox = RefBox[int](value: 4)
let node = Node(name: "x", next: RefBox[int](value: 1))
let empty = Empty()
let nestedTup = (1, @[2], Plain(x: 3, y: "z"))

let nums = @[1, 2, 3]
let emptyNums: seq[int] = @[]
let nested = @[@[1], @[2]]
let arr = [4, 5]
let named = (a: 1, b: "x")
let nestedNamed = (1, (a: 2, b: "x"))
let unnamed = (1, "x")
let single = (1,)
let slice = 1 .. 3
let strSlice = "a" .. "c"
let mixedSlice = 1 .. "c"
var emptySet: set[char]
let charSet = {'a', 'b'}
let enumSet: set[E] = {ea, eb}
let enumArr: array[2, E] = [ea, eb]

let varRed = VarNode(name: "n", kind: red, bits: 7)
let varGreen = VarNode(name: "s", kind: green,
  inner: Inner(active: true, iv: 8), text: "hi")
let varBlue = VarNode(name: "e", kind: blue)
let maybeSome = Maybe[int](Some(val: 42))
let maybeNone: Maybe[int] = None()
let sharedFirst = First(shared: 10, a: 1)
let sharedSecond = Second(shared: 20, b: "y")
let sharedThird = Third(shared: 30)
let refValue: RefSum = RValue(val: 5)
let refAdd: RefSum = RAdd(a: refValue, b: RValue(val: 7))
let myDistinct = MyDistinct(3)
let distinctStr = DString("hi")
let genericDistinct = GenD[int](6)
let myRange: MyRange = 3
let charRange: CharRange = 'c'
let namedSynthetic = (Field1: 1, Field2: 2)
let distinctHolder = DistinctHolder(value: myDistinct)
let distinctBox = Box[MyDistinct](value: myDistinct)
let distinctRefBox = RefBox[MyDistinct](value: myDistinct)
let distinctObj = DObj(BaseObj(a: 4))
let distinctSeq = DSeq(@[1, 2])

let cstr: cstring = "a"
var nilCstr: nil cstring = nil
let highStr = "\192\128"

var nilPtr: nil pointer = nil
var n = 7
let intPtr = addr n
var d = 1

proc x() =
  discard

proc b(x: int): proc() =
  return proc() =
    discard

let closureValue = b(0)

proc showOpen(x: openArray[int]): string =
  repr(x)

proc show[T: Reprable](x: T): string =
  repr(x)

proc showDistinct[T: distinct](x: T): string =
  repr(x)

proc showRange[T: range](x: T): string =
  repr(x)

proc showField[T: distinct](x: Box[T]): string =
  repr(x.value)

func isAddressRepr(s: string): bool =
  result = s.len == sizeof(pointer) * 2
  if result:
    for c in s:
      if c notin {'0'..'9', 'A'..'F'}:
        return false

assert repr(42) == "42"
assert repr(1.5) == "1.5"
assert repr(true) == "true"
assert repr(red) == "red"
assert repr('a') == "'a'"
assert repr('\n') == "'\\10'"
assert repr('\\') == "'\\\\'"
assert repr("a") == "\"a\""
assert repr("a\tb") == "\"a\\9b\""
assert repr("a\nb") == "\"a\\n\nb\""
assert repr(highStr) == "\"\\192\\128\""
assert repr('\255') == "'\\255'"
assert repr('\'') == "'''"
assert repr(nums) == "@[1, 2, 3]"
assert repr(emptyNums) == "@[]"
assert repr(nested) == "@[@[1], @[2]]"
assert repr(arr) == "[4, 5]"
assert repr(charSet) == "{'a', 'b'}"
assert repr(emptySet) == "{}"
assert repr(enumSet) == "{ea, eb}"
assert repr(enumArr) == "[ea, eb]"
assert repr(named) == "(a: 1, b: \"x\")"
assert repr(nestedNamed) == "(1, (a: 2, b: \"x\"))"
assert repr(unnamed) == "(1, \"x\")"
assert repr(single) == "(1,)"
assert repr(slice) == "1 .. 3"
assert repr(strSlice) == "\"a\" .. \"c\""
assert repr(mixedSlice) == "1 .. \"c\""
assert repr(plain) == "Plain(x: 1, y: \"a\")"
assert repr(refPlain) == "RefPlain(value: 2)"
assert repr(nilRef) == "nil"
assert repr(box) == "Box[int](value: 3)"
assert show(box) == "Box[int](value: 3)"
assert repr(refBox) == "RefBox[int](value: 4)"
assert repr(node) == "Node(name: \"x\", next: RefBox[int](value: 1))"
assert repr(empty) == "Empty()"
assert repr(nestedTup) == "(1, @[2], Plain(x: 3, y: \"z\"))"
assert repr(varRed) == "VarNode(name: \"n\", kind: red, bits: 7)"
assert repr(varGreen) ==
  "VarNode(name: \"s\", kind: green, inner: Inner(active: true, iv: 8), text: \"hi\")"
assert repr(varBlue) == "VarNode(name: \"e\", kind: blue)"
assert repr(maybeSome) == "Maybe[int](Some(val: 42))"
assert repr(maybeNone) == "Maybe[int](None())"
assert repr(sharedFirst) == "SharedSum(First(shared: 10, a: 1))"
assert repr(sharedSecond) == "SharedSum(Second(shared: 20, b: \"y\"))"
assert repr(sharedThird) == "SharedSum(Third(shared: 30))"
assert repr(refValue) == "RefSum(RValue(val: 5))"
assert repr(refAdd) == "RefSum(RAdd(a: RefSum(RValue(val: 5)), b: RefSum(RValue(val: 7))))"
assert repr(myDistinct) == "3"
assert repr(distinctStr) == "\"hi\""
assert repr(genericDistinct) == "6"
assert repr(myRange) == "3"
assert repr(charRange) == "'c'"
assert show(myDistinct) == "3"
assert repr(@[MyDistinct(1), MyDistinct(2)]) == "@[1, 2]"
assert repr([MyRange(1), MyRange(2)]) == "[1, 2]"
assert repr(namedSynthetic) == "(Field1: 1, Field2: 2)"
assert repr(distinctHolder) == "DistinctHolder(value: 3)"
assert repr(distinctBox) == "Box[MyDistinct](value: 3)"
assert repr(distinctRefBox) == "RefBox[MyDistinct](value: 3)"
assert repr(distinctObj) == "BaseObj(a: 4)"
assert repr(distinctSeq) == "@[1, 2]"
assert showDistinct(myDistinct) == "3"
assert showRange(myRange) == "3"
assert showField(distinctBox) == "3"
assert repr(Box[int]) == "Box[int]"
assert repr(RefBox[int]) == "RefBox[int]"
assert repr(RefPlain) == "RefPlain"
assert repr(seq[int]) == "seq[int]"
assert repr(ref int) == "ref int"
assert repr(ptr int) == "ptr int"
assert repr(ref Plain) == "ref Plain"
assert repr(ptr Plain) == "ptr Plain"
assert repr(cstr) == "\"a\""
assert repr(nilCstr) == "\"\""
assert repr(nilPtr) == "nil"
assert repr(intPtr) == "ptr 7"
assert repr(A(c: d.addr)) == "A(c: ptr 1)"
assert repr(B(c: d.addr)) == "B(c: " & repr(cast[pointer](d.addr)) & ")"
assert isAddressRepr(repr(x))
assert isAddressRepr(repr(b))
assert repr(closureValue) ==
  repr(cast[ptr pointer](addr closureValue)[])
assert showOpen([6, 7]) == "[6, 7]"

echo repr(42)
echo repr(1.5)
echo repr(true)
echo repr(red)
echo repr('a')
echo repr('\n')
echo repr('\\')
echo repr("a")
echo repr("a\tb")
echo repr("a\nb")
echo repr(highStr)
echo repr('\255')
echo repr('\'')
echo repr(nums)
echo repr(emptyNums)
echo repr(nested)
echo repr(arr)
echo repr(charSet)
echo repr(emptySet)
echo repr(enumSet)
echo repr(enumArr)
echo repr(named)
echo repr(nestedNamed)
echo repr(unnamed)
echo repr(single)
echo repr(slice)
echo repr(strSlice)
echo repr(mixedSlice)
echo repr(plain)
echo repr(refPlain)
echo repr(nilRef)
echo repr(box)
echo show(box)
echo repr(refBox)
echo repr(node)
echo repr(empty)
echo repr(nestedTup)
echo repr(varRed)
echo repr(varGreen)
echo repr(varBlue)
echo repr(maybeSome)
echo repr(maybeNone)
echo repr(sharedFirst)
echo repr(sharedSecond)
echo repr(sharedThird)
echo repr(refValue)
echo repr(refAdd)
echo repr(myDistinct)
echo repr(distinctStr)
echo repr(genericDistinct)
echo repr(myRange)
echo repr(charRange)
echo show(myDistinct)
echo repr(@[MyDistinct(1), MyDistinct(2)])
echo repr([MyRange(1), MyRange(2)])
echo repr(namedSynthetic)
echo repr(distinctHolder)
echo repr(distinctBox)
echo repr(distinctRefBox)
echo repr(distinctObj)
echo repr(distinctSeq)
echo showDistinct(myDistinct)
echo showRange(myRange)
echo showField(distinctBox)
echo repr(Box[int])
echo repr(RefBox[int])
echo repr(RefPlain)
echo repr(seq[int])
echo repr(ref int)
echo repr(ptr int)
echo repr(ref Plain)
echo repr(ptr Plain)
echo repr(cstr)
echo repr(nilCstr)
echo repr(nilPtr)
echo repr(intPtr)
echo repr(A(c: d.addr))
echo showOpen([6, 7])

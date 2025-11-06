import std/[assertions, syncio]

var value = @[ "hello", "world", "!" ]

assert high( value ) == 2
assert  low( value ) == 0

assert high( "value" ) == 4
assert low( "value" ) == 0


let x = if true: 1 else: quit(0)


proc sort[T](s: var seq[T], f: proc( a: T, b: T ): bool ) =
  for j in 0..high(s):
    for i in 0..high(s)-1:
      if f( s[i], s[i+1]):
        swap s[i], s[i+1]

var arr = @[ "a", "f", "d", "c", "b", "e" ]  

# works in both nim and nimony
proc f( a: string, b: string ): bool =
  return a < b

sort( arr, f )

# works only in nim
let f2 = proc( a: string, b: string ): bool = a < b 
sort( arr, f2 )

# works only in nim 
sort( arr, proc( a: string, b: string ): bool = a < b )

var s = ""
for item in arr:
  s.add item

assert s == "fedcba"


proc hello(): proc (name: string): string =
  result = proc (name: string): string =
    "Hello "

var mm = hello()
discard mm("World")


proc hello2(): int =
  result = 123

iterator hello2(): int =
  yield 1


for i in hello2():
  assert i == 1

let s2 = hello2()
assert s2 == 123


block:
  proc hello3(): int =
    result = 123

  iterator hello3(): int =
    yield 1


  for i in hello3():
    assert i == 1

  let s = hello3()
  assert s == 123

block:
  proc foo =
    let a = 999

    proc abend(): int =
      return a

    let res = abend

  foo()


type
  E = enum
    a, b, c, d
  X = object
    v: int
  O = object
    case kind: E
    of a:
      a: int
    of b, c:
      b: float
    else:
      d: X

proc `=destroy`(x: var X) =
  echo "x destroyed"

var o = O(kind: d, d: X(v: 12345))
assert o.d.v == 12345

block:
  proc foo(): int =
    result = 12
    ## 1234


  assert foo() == 12

proc herz(s1: char) =
  assert char('\255') == s1

proc hand =
  let s1 = char('\255')
  let s2 = '\255'
  assert s1 == s2
  assert s1 == '\255'
  assert '\255' == s1
  herz('\255')

hand()

block:
  proc foo(): int =
    result = 1
    return


  assert foo() == 1


block:
  proc foo(x: seq[int] = @[1, 2, 3]): int =
    let s = x[1]
    result = s

  assert foo() == 2

block:

  proc foo(x: openArray[int]): int =
    let s = x[1]
    result = s.int


  template herz(sep: openArray[int]): int =
    foo(sep)

  proc bar =
    assert herz([1, 2, 3]) == 2

  bar()

block:
  proc foo(x: openArray[int] = [1, 2, 3]): int =
    let s = x[1]
    result = s.int

  assert foo() == 2

block:
  var s = [1, 2, 3]

  assert s.low == 0
  assert s.high == 2

# Test escaping behavior
block:
  var s = ""
  s.addQuoted('\0')
  s.addQuoted('\31')
  s.addQuoted('\127')
  assert s == "'\\x00''\\x1F''\\x7F'"
block:
  var s = ""
  s.addQuoted('\\')
  s.addQuoted('\'')
  s.addQuoted('\"')
  assert s == """'\\''\'''\"'"""
block:
  var s = ""
  s.addQuoted("å")
  s.addQuoted("ä")
  s.addQuoted("ö")
  s.addEscapedChar('\xFF')
  assert s == """"å""ä""ö"\xFF"""


block:
  var s = "foo"
  s[0] = 'F'

when defined(posix):
  type
    SysLockObj {.importc: "pthread_mutex_t", pure, final,
                header: """#include <sys/types.h>
                          #include <pthread.h>""", byref.} = object

block:
  const MAX_PATH: int = 260

  type
    Foo = object
      cFileName: array[0..MAX_PATH - 1, int]

block:
  proc foo(x: var string) =
    var m = 2
    var s = addr x[1]


  var ss = "dsjfhg"
  foo(ss)

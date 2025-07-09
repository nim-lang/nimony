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

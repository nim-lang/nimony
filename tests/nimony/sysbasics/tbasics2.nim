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
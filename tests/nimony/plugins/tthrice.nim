import std / syncio

# A plugin template and a template body with the SAME signature must behave the
# same way (nim-lang/nimony#2485). `untyped` means untyped in both spellings: a
# plugin template that stands for a type has to say `typedesc`, the way
# `std/typetraits` spells `distinctBase`. Since the plugin cannot answer while
# the argument is still a type variable, the parked call is a value of unchecked
# type and everything reading it defers until instantiation.

template thrice(n: int): untyped {.plugin: "deps/mthrice".}
template thriceExpanded(n: int): untyped = n * 3

type
  PluginArray[N: static[int]; T] = object
    elems: array[thrice(N), T]
  BodyArray[N: static[int]; T] = object
    elems: array[thriceExpanded(N), T]

func countByCall[N: static[int]; T](a: PluginArray[N, T]): int =
  result = 0
  for i in 0 ..< thrice(N): result = result + 1

func countByHigh[N: static[int]; T](a: PluginArray[N, T]): int =
  result = 0
  for i in 0 ..< high(a.elems): result = result + 1

func countByCallB[N: static[int]; T](a: BodyArray[N, T]): int =
  result = 0
  for i in 0 ..< thriceExpanded(N): result = result + 1

func countByHighB[N: static[int]; T](a: BodyArray[N, T]): int =
  result = 0
  for i in 0 ..< high(a.elems): result = result + 1

var p: PluginArray[2, int]
var b: BodyArray[2, int]

echo sizeof(p) == sizeof(b)          # true, 6 * 8
echo countByCall(p) == countByCallB(b)
echo countByHigh(p) == countByHighB(b)
echo countByCall(p)                  # 6
echo countByHigh(p)                  # 5

# outside a generic the plugin folds immediately, as it always did:
echo high(p.elems)                   # 5

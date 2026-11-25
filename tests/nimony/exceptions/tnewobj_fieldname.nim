# In a routine that can raise, a field of a `ref` object constructor named
# like a local initialised by a raising call keeps its own name.
import std/syncio

type
  Box = ref object
    value: int
  Base = ref object of RootObj
    value: int
  Child = ref object of Base
    label: string

proc check(n: int): int {.raises.} =
  if n < 0: raise Failure
  result = n

proc make(n: int): Box {.raises.} =
  let value = check(n)
  result = Box(value: value)

proc makeChild(n: int): Child {.raises.} =
  let value = check(n)
  result = Child(value: value, label: "child")

try:
  echo make(4).value
  let c = makeChild(5)
  echo c.value, " ", c.label
except:
  echo "raised"

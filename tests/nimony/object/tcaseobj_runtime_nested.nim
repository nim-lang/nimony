# A run-time selector of a case nested in a branch the constructor names
# constantly: the hidden init proc dispatches on the inner selector only, and
# each arm keeps its branch's declared defaults. (Not on the native list:
# arkham does not handle nested case objects yet.)
import std/syncio

type
  Nested = object
    case outer: bool
    of true:
      case inner: range[0..9]
      of 0..3: lo: int = 10
      else: hi: string = "hi"
    of false:
      plain: int = -5

proc mkNested(i: range[0..9]): Nested {.noinline.} = Nested(outer: true, inner: i)
proc mkNestedOuter(o: bool): Nested {.noinline.} = Nested(outer: o)

proc showNested(n: Nested) =
  if n.outer:
    if n.inner <= 3: echo "lo ", n.lo
    else: echo "hi ", n.hi
  else: echo "plain ", n.plain

showNested mkNested(2)
showNested mkNested(7)
showNested mkNestedOuter(true)
showNested mkNestedOuter(false)

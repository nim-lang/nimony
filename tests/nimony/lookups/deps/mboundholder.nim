import std/opt
import mboundbox

type
  Holder* = object
    child*: Opt[int]

proc holderBox*(): Box[int] = Box[int](v: 1)
proc childIsNone*(h: Holder): bool = h.child.isNone
proc describe*[T](x: T): string = "any"

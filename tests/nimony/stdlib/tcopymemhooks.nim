# issue #3090: a type with only a `=copy` or only a `=dup` is not plain data,
# nor is an object holding one; byte copies would skip the hook
import std/[syncio, assertions, typetraits]

type
  WithCopy = object
    x: int
  WithDup = object
    x: int
  HoldsCopy = object
    w: WithCopy

var copies = 0

proc `=copy`(dst: var WithCopy; src: WithCopy) =
  {.cast(assumeSync).}:
    inc copies
  dst.x = src.x

proc `=dup`(src: WithDup): WithDup = WithDup(x: src.x)

proc copyValue[T](dst: var T; src: T) =
  when supportsCopyMem(T):
    copyMem(addr dst, addr src, sizeof(T))
  else:
    dst = src

var dst, src: WithCopy
src.x = 42
copyValue(dst, src)
echo "supportsCopyMem=", supportsCopyMem(WithCopy)
echo "copies=", copies
assert dst.x == 42
echo supportsCopyMem(WithDup), " ", supportsCopyMem(HoldsCopy)

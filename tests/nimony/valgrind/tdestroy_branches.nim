## Heap-owning values assigned to `result` in branches, twice in a branch,
## in a loop and from a raising call: everything is freed exactly once, with
## arcopt dropping the destroys of `result`'s moved-from state.

import std/syncio

type Resp = object
  reason: string
  body: string

proc heap(tag: string; n: int): string =
  result = tag & ": a string long enough to live on the heap " & $n

proc branchy(n: int): Resp =
  if n > 0:
    result = Resp(reason: heap("ok", n), body: heap("body", n))
  else:
    result = Resp(reason: heap("neg", n))

proc twice(n: int): Resp =
  if n > 0:
    result = Resp(reason: heap("first", n))
    result = Resp(reason: heap("second", n), body: heap("b", n))
  else:
    result = Resp(body: heap("else", n))

proc looped(n: int): Resp =
  result = Resp(body: heap("start", n))
  var i = 0
  while i < n:
    result = Resp(reason: heap("pass", i))
    inc i

proc mk(n: int): Resp {.raises.} =
  if n < 0: raise Failure
  Resp(reason: heap("made", n))

proc guarded(n: int): Resp =
  try:
    result = mk(n)
  except ErrorCode:
    result = Resp(reason: heap("error", n))

proc main =
  var total = 0
  for n in [-2, -1, 0, 1, 2]:
    total += branchy(n).reason.len + twice(n).reason.len +
             looped(n + 2).reason.len + guarded(n).reason.len
  echo total

main()

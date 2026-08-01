# The destroyer used to append a scope's `=destroy` sequence AFTER a
# `return` that already ran it. In straight-line output that tail is
# unreachable, so nobody noticed — but `eliminateJumps` runs after the
# destroyer and rewrites `return` into `(jtrue ´r)` plus FALLTHROUGH, so in
# a `.passive` proc the dead tail became live code. Every escaping local
# was then destroyed twice, and the coroutine-frame field the result had
# just been moved out of was destroyed as well (arcopt had elided the
# paired `=wasMoved` along with the first, genuinely dead, `=destroy`).
#
# Shape that matters, from a production `.passive` Keycloak invite proc
# that double-freed its frame: several string locals live ACROSS
# suspension points, a `while` containing both a suspension and an early
# `return`, a further suspension after the loop, and a success tail that
# builds a string.
#
# Two exits are checked because they went wrong differently:
#   - falling off the end   -> the string came back EMPTY (its buffer had
#     been freed under `result`).
#   - explicit `return`     -> same, plus glibc double-free abort, because
#     the frontend's implicit trailing `(ret result)` made the destructor
#     sequence run a second time.

import std/syncio

proc step(s: string): bool {.passive.} =
  result = s.len > 0

proc fallOff(a, b: string): string {.passive.} =
  result = ""
  let clientId = a & "-client"
  let userId = b & "-user"
  var i = 0
  while i < 2:
    let name = "g" & $i
    let ok = step(name)
    if not ok:
      result = "group failed " & name & clientId
      return
    i = i + 1
  let ok2 = step(userId)
  if not ok2:
    result = "email failed " & clientId
    return
  result = "sent " & userId & " " & clientId

proc explicitReturn(a, b: string): string {.passive.} =
  result = ""
  let clientId = a & "-client"
  let userId = b & "-user"
  var i = 0
  while i < 2:
    let name = "g" & $i
    let ok = step(name)
    if not ok:
      result = "group failed " & name & clientId
      return
    i = i + 1
  let ok2 = step(userId)
  if not ok2:
    result = "email failed " & clientId
    return
  result = "sent " & userId & " " & clientId
  return

echo fallOff("a", "b")
echo explicitReturn("c", "d")

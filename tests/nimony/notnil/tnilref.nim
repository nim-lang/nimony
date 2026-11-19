
import std / syncio

type
  Myref = ref object
    x: int

proc p(): Myref not nil {.raises.} = Myref(x: 5)

proc hah2 {.raises.} =
  var r: Myref not nil
  r = p()
  echo r[].x

proc other =
  var r: nil Myref = nil
  if r != nil:
    echo r[].x
  else:
    echo "is nil"

proc touch(value: var int) = inc value

proc receive(input: nil ptr int) =
  # a call mutating the target keeps the pointer non-nil (#2809)
  if input == nil: return
  let receiver = input
  touch(receiver[])
  touch(receiver[])

proc receiveRef(o: nil Myref) =
  if o == nil: return
  touch(o.x)
  touch(o.x)
  touch(o[].x)

# A check on one of several equal locations covers them all, after the
# checked one moves on, at a join and through a loop.
type
  Link = ref object
    x: int
    next: nil Link

proc afterWalk(head: nil Link): int =
  var it = head
  var prev = it
  result = 0
  if it != nil:
    it = it.next
    result = prev.x

proc checkedOnEitherSide(a: nil Link; b: bool): int =
  var x = a
  var y = x
  if b:
    if y == nil: return 0
  else:
    if x == nil: return 0
  result = x.x + y.x

proc throughLoop(head: nil Link; n: int): int =
  var first = head
  var it = first
  if it == nil: return 0
  result = 0
  for i in 0..<n:
    result += first.x
    if it != nil: it = it.next

try:
  hah2()
  other()
  var value = 1
  receive(addr value)
  echo value
  var o = Myref(x: 1)
  receiveRef(o)
  echo o.x
  echo afterWalk(Link(x: 6)), " ", checkedOnEitherSide(Link(x: 7), false), " ",
    throughLoop(Link(x: 2), 3)
except:
  echo "god riddance"

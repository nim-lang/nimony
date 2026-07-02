import std/syncio

type
  TT = ref object
    x: int
    next: nil TT

proc main =
  var a: nil TT = TT()
  var b: nil TT = TT()

  if a != nil:
    a.next = b
    a.x += 1
    if a.next != nil:
      a.next.x += 1

try:
  main()
except:
  echo "god riddance"

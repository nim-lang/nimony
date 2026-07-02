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
    echo a.x
    if a.next != nil:
      a.next.x += 1
      echo a.next.x

try:
  main()
except:
  echo "god riddance"

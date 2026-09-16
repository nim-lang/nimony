# A seq of a FOREIGN object with a closure field, destroyed in the importing
# module. sizeof used to size the closure as one pointer, so this module saw
# the object as 24 bytes (by value) while the defining module had lowered it
# to 32 bytes (by const ref): the generated =destroy hook took a pointer, the
# call here passed the value, and the C compiler rejected the program.
import std/syncio
import deps/mclosuredestroy

proc build(n: int): seq[Sector] =
  result = @[]
  for i in 0 ..< n:
    result.add mkSector("sector " & $i)

proc consume() =
  var sectors = build(3)
  echo sectors.len
  sectors = @[]
  echo sectors.len

consume()
echo "ok"

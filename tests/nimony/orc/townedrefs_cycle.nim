# a cycle built through owned locals is collected: storing an owned local into
# an unowned field copies it once, as the unowned reference it converts to
{.feature: "ownedRefs".}
{.feature: "assumeSync".}
import std/syncio
var destroyed = 0
type
  PairObj = object
    name: string
    other: nil Pair
  Pair = ref PairObj
proc `=destroy`(p: PairObj) =
  inc destroyed
  `=destroy`(p.name)
  `=destroy`(p.other)
proc pairs() =
  var a = Pair(name: "a")   # owned
  var b = Pair(name: "b")   # owned
  a.other = b
  b.other = a
for i in 0..<100: pairs()
GC_fullCollect()
echo "pairs freed: ", destroyed

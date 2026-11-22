# issue #3044: a `seq` of a move-only type from another module, or an object
# holding one declared in a third module, cannot be copied either
import deps / [mcopyerrorhandle, mcopyerrorholder]

proc main =
  var a = handles(7)
  var b = a
  var m = move a
  var h = mk()
  var h2 = h
  var hm = move h
  discard b.len + m.len + h2.hs.len + hm.hs.len
main()

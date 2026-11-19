# #3044 item 1: the seq instance (and a hook calling it) that another module
# emitted for a move-only element cannot copy either
import std/syncio
import deps/[mcopyerrhandle, mcopyerrholder]

proc main() =
  var a = handles(7)
  var b = a
  echo a.len, " ", b.len
  var h = mkHolder()
  var h2 = h
  echo h2.hs.len, " ", h.hs.len

main()

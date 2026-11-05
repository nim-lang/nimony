# String paths that write after asking for storage. A refused allocation turns
# the string into the OOM marker (`isOom`), whose 7 chars live inline with no
# heap block, and nothing may then be written through a pointer to the storage
# that was not allocated: not by `beginStore`'s callers (`addInt`), not by
# `[]=` or `setLen` on a string that first needs a private copy.
#
# Each source below takes 600 KB of the 1 MiB budget (see `nimony.args`), so a
# second block of that size is refused.
# That arithmetic needs a heap of its own, hence the `.nojoin` sidecar.
import std / [syncio]

const N = 600_000

proc addIntGrow() =
  let big = newString(N)
  var s = ""
  var refused = false
  for i in 0 ..< 200_000:
    s.addInt i       # `addInt` copies through `beginStore`'s pointer
    if isOom(s):
      refused = true
      break
  echo "addInt: ", refused, " ", s.len, " ", big.len

proc beginStoreNil() =
  let big = newString(N)
  var s = "x"
  let p = beginStore(s, N, 1)
  echo "beginStore: ", p == nil, " ", isOom(s), " ", big.len
  endStore(s)

proc indexShared() =
  var a = newString(N)
  a[3] = 'a'
  var b = a          # shares `a`'s block until written
  b[3] = 'b'         # the private copy is refused
  echo "[]=: ", isOom(b), " ", b.len, " src ", a[3], " ", a.len

proc shrinkShared() =
  var a = newString(N)
  var b = a
  b.setLen 100       # shrinking a shared block copies it first: refused
  echo "setLen: ", isOom(b), " ", b.len, " src ", a.len

proc main() =
  addIntGrow()
  beginStoreNil()
  indexShared()
  shrinkShared()
  echo "still running"

main()

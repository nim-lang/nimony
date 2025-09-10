
import std/syncio
import deps/bases

type
  CustomDerived* = object of CustomBase
    s: string

method reportIdentity(d: CustomDerived) =
  echo "CustomDerived"

proc test(o: CustomBase) =
  echo o of CustomBase
  echo o of CustomDerived
  o.reportIdentity()

test(CustomDerived(s: "test"))
test(CustomBase())

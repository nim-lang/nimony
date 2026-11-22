# a type whose hooks are derived from a `seq` of another module's move-only
# type (#3044)
import mcopyerrorhandle

type
  Holder* = object
    hs*: seq[Handle]

proc mk*(): Holder = Holder(hs: handles(3))

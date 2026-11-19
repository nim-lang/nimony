import ./mcopyerrhandle
type
  Holder* = object
    hs*: seq[Handle]
proc mkHolder*(): Holder = Holder(hs: handles(3))

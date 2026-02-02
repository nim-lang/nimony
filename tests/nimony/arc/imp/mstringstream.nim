
type
  StringStream* = ref object of RootObj
    ## A string stream object.
    data*: string ## A string data.

proc newStringStream*(): StringStream =
  result = StringStream()

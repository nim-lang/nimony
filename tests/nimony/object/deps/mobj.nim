type
  FileStream* = object of RootObj


proc newFileStream*(): FileStream =
  result = FileStream()

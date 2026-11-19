type
  FileVersion* = object
    case
    of Missing:
      discard
    of Version:
      mtime*: int

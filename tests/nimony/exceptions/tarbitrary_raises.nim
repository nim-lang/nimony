# `.raises` and `except` take `ErrorCode` or a `ref` object type: an error
# travels in a one byte `ErrorCode` slot, a `ref` one in the threadvar `exc`.

type
  Code = enum none, bad, worse = 256     # would be truncated (#2676)
  Plain = object                         # has no "no error" value (#2677)
    pos: int
  Wide = distinct int32
  RefInt = ref int

proc a() {.raises: Code.} = discard
proc b() {.raises: Plain.} = discard
proc c() {.raises: Wide.} = discard
proc d() {.raises: RefInt.} = discard

proc e() {.raises.} = raise IOError

proc f() =
  try: e()
  except Code: discard
  except: discard

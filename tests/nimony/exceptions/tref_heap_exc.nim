import std/[syncio, assertions]

# Heap-based exceptions via `ref object of Exception`. The `.raises` ABI
# stays ErrorCode-based; derefs lowers `raise X(...)` into setting the
# threadvar `exc` and signalling Failure to the caller.

type
  IOError = ref object of Exception
    code: int

proc raiseIO(msg: string; code: int) {.raises: IOError.} =
  raise IOError(msg: msg, code: code)

proc roundTrip() =
  try:
    raiseIO("disk", 42)
    echo "FAIL: should have raised"
    assert false
  except IOError as e:
    echo "Caught IOError: ", e.msg, " code=", e.code
    assert e.msg == "disk"
    assert e.code == 42
  except:
    echo "FAIL: catchall fired"
    assert false

roundTrip()
echo "done"

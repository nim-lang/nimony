
import std / syncio

proc willFail() {.raises.} =
  raise Failure

try:
  willFail()
except:
  echo "failed"

proc anotherFailure(): string {.raises.} =
  raise Failure

proc shield() =
  try:
    let x = anotherFailure()
    echo x
  except:
    echo "failed"

shield()

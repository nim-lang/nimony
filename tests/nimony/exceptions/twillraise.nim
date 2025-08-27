
import std / syncio

proc willFail() {.raises.} =
  raise Failure

try:
  willFail()
except:
  echo "failed"

proc anotherFailure(): string {.raises.} =
  raise OutOfMemError

proc shield() =
  try:
    let x = anotherFailure()
    echo x
  except ErrorCode as e:
    echo "correct: ", e

shield()

# A branch that leaves through `raise`, `break` or `continue` has no value;
# the other branches type the if/case expression or the try's handler.
import std/syncio

proc pick(v: int): int {.raises.} =
  let a = if v == 0: raise ValueError else: 4
  let b = case v
    of 1: raise ValueError
    else: a
  result = try: b except: raise ValueError

proc loop() =
  for i in 0..5:
    let a = if i < 3: i else: break
    let b = case i
      of 1: continue
      else: i * 10
    echo a, " ", b

try: echo pick(2) except ErrorCode as e: echo "unexpected ", e
try: discard pick(0) except ErrorCode as e: echo "if ", e
try: discard pick(1) except ErrorCode as e: echo "case ", e
loop()

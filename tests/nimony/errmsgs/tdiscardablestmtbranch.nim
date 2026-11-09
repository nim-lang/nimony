# A statement's branches are statements: a value that may not be discarded
# is an error in any branch, beside a discardable call or not.

proc f(): int {.discardable.} = 1

var x = 0
let c = x == 0
if c: f() elif x == 1: 2 else: x = 3
try: 4
except: x = 5
case x
of 0: x = 6
else: "seven"
proc p() =
  if c: x = 8 else: 9
iterator it(): int =
  yield 1
  if c: 10 else: 11

import std/[syncio, tables]

# The arms of one `try` see `ErrorCode` raises and `ref` raises apart: a plain
# code never reaches an `of` test (#2679), the arms are tried in order, and a
# raise no arm takes goes on with its own code.

type
  ParseError = ref object of Exception
  OtherError = ref object of Exception

proc leaf() {.raises: ParseError.} = raise ParseError(msg: "bad")
proc lookupRef(): int {.raises: ParseError.} = raise ParseError(msg: "from ref")

# #2678: every raise is caught, so a non-raising routine needs no way out.
proc run(): string =
  result = "none"
  try: leaf()
  except OtherError: result = "other"
  except ParseError as e: result = e.msg

echo run()

proc mixed() =
  let t = initTable[string, int]()
  for which in 0 .. 1:
    try:
      if which == 0: echo t["missing"]
      else: echo lookupRef()
    except ParseError as e: echo "ref: ", e.msg
    except ErrorCode as c: echo "code: ", c

mixed()

# A code passes a `try` with only `ref` arms unchanged ...
proc passThrough(t: Table[string, int]): int {.raises.} =
  try:
    result = t["missing"]
  except ParseError:
    result = -1

try: discard passThrough(initTable[string, int]())
except ErrorCode as c: echo "passed on: ", c

# ... and a bare `raise` re-raises the code it caught, not `Failure`.
proc reraise() {.raises.} =
  try:
    raise IOError
  except ErrorCode:
    raise

try: reraise()
except ErrorCode as c: echo "re-raised: ", c

# The dispatcher of a `try` expression yields the value of the arm it takes.
proc refOrValue(x: int): int {.raises: ParseError.} =
  if x == 0: raise ParseError(msg: "zero")
  result = x

proc catchAll(v: int): int =
  result = (try: refOrValue(v) except: v + 3)

proc typedArms(v: int): string {.raises: ParseError.} =
  let s = try: $refOrValue(v)
          except OtherError: "other"
          except ParseError as e:
            let prefix = "caught "
            prefix & e.msg
  result = s

# Every raise is caught (#2678): the last typed arm takes what is left.
proc onlyArm(v: int): int =
  result = (try: refOrValue(v) except ParseError as e: e.msg.len)

proc main() {.raises: ParseError.} =
  echo catchAll(1), " ", catchAll(0)
  echo typedArms(0), ", ", typedArms(7)
  echo onlyArm(0), " ", onlyArm(9)

try: main() except: echo "unreachable"

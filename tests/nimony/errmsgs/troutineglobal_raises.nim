{.feature: "assumeSync".}

# The initializer of a routine's `.global` runs once, before the module's
# toplevel statements: it is module level code, outside of the routine's `try`
# statements, and nothing it raises can leave through the routine.

proc mayFail(s: string): int {.raises.} =
  if s == "bad": raise Failure
  result = 5

proc raisingInit(): int {.raises.} =
  var g {.global.} = mayFail("ok")
  result = g

proc raisingInitInTry(): int =
  try:
    let g {.global.} = mayFail("ok")
    result = g
  except:
    result = 0

proc catchingInit(): int =
  var g {.global.} = (try: mayFail("bad") except: 0)
  result = g

try:
  discard raisingInit()
except:
  discard
discard raisingInitInTry()
discard catchingInit()

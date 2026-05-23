## A heap-based-exception `try` with a `finally` clause cannot currently
## host a nested `try`-with-`except` inside any of its handler bodies:
## destroyer.nim's `trRaise` walks every enclosing scope and inlines the
## outer finally before the nested raise jump, but the nested raise is
## actually caught locally by the inner `except`. The outer finally
## would fire spuriously. Refusing the pattern up front is the v1 fix;
## the proper solution (routing codegen through NJ's `eliminateJumps`)
## is parked behind 0.4.

{.feature: "canraise".}

import std/syncio

type KeyExc = ref object of Exception

proc bad {.raises: ref Exception.} =
  try:
    raise KeyExc(msg: "outer")
  except KeyExc as ex:
    echo ex.msg
    try:
      raise KeyExc(msg: "inner")
    except KeyExc as iy:
      echo iy.msg
  finally:
    echo "outer-finally"

try: bad()
except: discard

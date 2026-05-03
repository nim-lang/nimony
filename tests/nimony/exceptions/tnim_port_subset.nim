## Subset of a Nim 2 exception-handling regression test (originally bug 7204)
## that already works under Nimony's heap-exception lowering. The two patterns
## still missing — outer `try/except/finally` with a *nested* try inside the
## except body, and `try/finally` (no except) propagating a raise — are
## blocked by destroyer.nim's scope-walk: it cannot tell which raises are
## caught locally and which escape, so it either over-duplicates the outer
## finally or skips it entirely. That needs an earlier pass to annotate the
## scope walk; tracked separately.

{.feature: "canraise".}

import std/syncio

type
  IOExc = ref object of Exception

proc p {.raises: ref Exception.} =
  try:
    raise (ref Exception)(msg: "Hello")
  except:
    echo "except1"
    raise
  finally:
    echo "finally1"

try:
  p()
except:
  echo "caught! 2"


proc noException {.raises: ref Exception.} =
  try:
    echo "BEFORE"

  except:
    echo "EXCEPT"
    raise

  finally:
    echo "FINALLY"

try: noException()
except: echo "RECOVER"

proc reraise_in_except {.raises: ref Exception.} =
  try:
    echo "BEFORE"
    raise IOExc(msg: "")

  except IOExc:
    echo "EXCEPT"
    raise

  finally:
    echo "FINALLY"

try: reraise_in_except()
except: echo "RECOVER"

proc return_in_except {.raises: ref Exception.} =
  try:
    echo "BEFORE"
    raise IOExc(msg: "hi")

  except:
    echo "EXCEPT: RETURN"
    return

  finally:
    echo "FINALLY"

try: return_in_except()
except: echo "RECOVER"

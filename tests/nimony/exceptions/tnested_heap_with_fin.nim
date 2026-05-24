## Pattern that used to mis-compile in destroyer: an outer `try` with a
## `finally`, whose `except` body contains a nested `try`-with-`except`.
## The fix lives in destroyer.nim — body of a `try` with an `except` clause
## is now `CaughtLocally`, which makes `trRaise` stop walking outward at
## that scope (so the outer finally is not inlined before a raise that
## will be caught locally) and `leaveScope` skip the own-finally for that
## scope (it runs naturally after the matched `except`). Also exercises
## the `try`/`finally`-without-`except` propagation path, which previously
## skipped the finally entirely.

{.feature: "canraise".}

import std/syncio

type
  KeyExc = ref object of Exception
  ValueExc = ref object of Exception

proc nestedFinally {.raises: ref Exception.} =
  try:
    raise KeyExc(msg: "msg1")
  except KeyExc as ex:
    echo ex.msg
    try:
      raise ValueExc(msg: "msg2")
    except (ref Exception) as e:
      echo e.msg
    finally:
      echo "finally2"
  finally:
    echo "finally1"

try: nestedFinally()
except: discard


proc raiser {.raises: ref Exception.} =
  raise KeyExc(msg: "bad")

proc tryFinallyPropagates {.raises: ref Exception.} =
  try:
    echo "before"
    raiser()
    echo "unreachable"
  finally:
    echo "fin"

try: tryFinallyPropagates()
except: echo "caught"

#
#
#            Nim's Runtime Library
#        (c) Copyright 2026 Ryan Walklin
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## opt — an optional value built on Nimony's native sum types.
##
## The sum-type optional (`Opt[T]`), distinct from a Nim 2-compatible
## `Option[T]`. `None` is the absent (fieldless) case, `Some` carries a value.
## `none[T]()` constructs through `result` so the fieldless `None` takes its
## `Opt[T]` type from the typed result slot (a bare `None()` in expression
## position has nothing to infer `T` from).
##
## `unsafeGet` raises `BugError` on `None` (a system `ErrorCode` — calling it
## without guarding is a programming error); guard with `isSome`, or use the total
## `get(default)`.
##
## ```nim
## import std/opt
## proc find(k: string): Opt[int] =
##   if k == "x": result = some(1)
##   else: result = none[int]()
## let r = find("x")
## if r.isSome: echo r.get(0)
## ```

type
  Opt*[T] = object
    case
    of None:
      discard
    of Some:
      val: T

func some*[T](v: T): Opt[T] =
  Some(val: v)

template default*[T](x: typedesc[Opt[T]]): Opt[T] =
  ## `None` — makes `Opt` fields work in enclosing objects' `default`/`T()`
  ## construction (the DefaultObj expansion resolves `default` per field, and
  ## sum types have no compiler-provided default). A template so it out-ranks
  ## the DefaultObj magic in overload resolution at construction sites.
  none[T]()

func none*[T](): Opt[T] =
  result = None()

func isSome*[T](o: Opt[T]): bool =
  case o
  of Some(_): result = true
  of None(): result = false

func isNone*[T](o: Opt[T]): bool =
  not o.isSome

func get*[T](o: Opt[T]; default: T): T =
  case o
  of Some(val): result = val
  of None(): result = default

proc getOrQuit*[T](o: Opt[T]): T =
  case o
  of Some(val): result = val
  of None(): panic "Opt.getOrQuit: value is None"

proc unsafeGet*[T](o: Opt[T]): T {.raises.} =
  case o
  of Some(val): result = val
  of None(): raise BugError

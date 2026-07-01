## opt — an optional value built on Nimony's native sum types.
##
## The sum-type optional (`Opt[T]`), distinct from a Nim 2-compatible
## `Option[T]`. `None` is the absent (fieldless) case, `Some` carries a value.
## `none[T]()` constructs through `result` so the fieldless `None` takes its
## `Opt[T]` type from the typed result slot (a bare `None()` in expression
## position has nothing to infer `T` from).
##
## `unsafeGet` raises `BadOperation` on `None` (a system `ErrorCode`); guard with
## `isSome`, or use the total `get(default)`.
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

proc some*[T](v: T): Opt[T] =
  Some(val: v)

proc none*[T](): Opt[T] =
  ## The absent `Opt[T]`. Constructed through `result` for type inference.
  result = None()

proc isSome*[T](o: Opt[T]): bool =
  case o
  of Some(_): result = true
  of None(): result = false

proc isNone*[T](o: Opt[T]): bool =
  not o.isSome

proc get*[T](o: Opt[T]; default: T): T =
  ## The value if present, else `default` (total — never raises).
  case o
  of Some(val): result = val
  of None(): result = default

proc unsafeGet*[T](o: Opt[T]): T {.raises.} =
  ## The value if present; raises `BadOperation` on `None`. Guard with `isSome`
  ## (the call site must be in a `{.raises.}` context, like any raising proc).
  case o
  of Some(val): result = val
  of None(): raise BadOperation

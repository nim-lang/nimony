#
#
#            Nim's Runtime Library
#        (c) Copyright 2026 Ryan Walklin
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## result — a success-or-error sum type built on Nimony's native sum types.
##
## `Result[T, E]` holds exactly one of `Ok` (the value, type `T`) or `Err`
## (the error, type `E`).
##
## `ok(v)` / `err(e)` infer the parameter their argument does not fix from the
## call's expected type, so an annotated target (a typed `result`, `let`, proc
## return, etc.) is enough — `result = ok(8080)` in a `Result[int, string]`
## context binds `E = string`. Spell both out (`ok[int, string](v)`) only where
## there is no expected type to infer from.
##
## `unsafeGet` raises `BugError` on the wrong variant (a system `ErrorCode` —
## calling it unguarded is a programming error); guard with `isOk`, or use the
## total `get(default)`.
##
## ```nim
## import std/result
## proc parsePort(s: string): Result[int, string] =
##   if s == "": result = err("empty")
##   else: result = ok(8080)
## let r = parsePort("80")
## if r.isOk: echo r.get(0) else: echo r.error("?")
## ```

type
  Result*[T, E] = object
    case
    of Ok:
      okVal: T
    of Err:
      errVal: E

proc ok*[T, E](v: T): Result[T, E] =
  ## A success `Result` (the `Ok` case). `E` is inferred from the expected type.
  result = Ok(okVal: v)

proc err*[T, E](e: E): Result[T, E] =
  ## A failure `Result` (the `Err` case). `T` is inferred from the expected type.
  result = Err(errVal: e)

proc isOk*[T, E](r: Result[T, E]): bool =
  case r
  of Ok(_): result = true
  of Err(_): result = false

proc isErr*[T, E](r: Result[T, E]): bool =
  not r.isOk

proc get*[T, E](r: Result[T, E]; default: T): T =
  case r
  of Ok(val): result = val
  of Err(_): result = default

proc error*[T, E](r: Result[T, E]; default: E): E =
  case r
  of Err(val): result = val
  of Ok(_): result = default

proc unsafeGet*[T, E](r: Result[T, E]): T {.raises.} =
  case r
  of Ok(val): result = val
  of Err(_): raise BugError

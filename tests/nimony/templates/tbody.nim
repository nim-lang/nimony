
import std / syncio

type
  X = object
    s: seq[string]

proc use(x: int) = discard

template copyInto(x: var X; body: untyped) =
  let oldLen = x.s.len
  body
  use oldLen

type SymId = distinct int

template `[]`*(syms: seq[string]; s: SymId): string = syms[s.int]

proc main =
  var x = X(s: @["abc", "def"])
  copyInto x:
    copyInto x:
      echo "yes ", x.s[SymId(1)]

main()

# nim-lang/nimony#2625: a bare `typedesc` template parameter used as a
# generic argument.
type Res[T] = object
  ok: bool

proc err[T](): Res[T] =
  Res[T](ok: false)

template guard(T: typedesc; x: int) =
  if x < 0:
    return err[T]()

proc check(x: int): Res[float] =
  guard(float, x)
  Res[float](ok: true)

proc checkPassive(x: int): Res[float] {.passive.} =
  guard(float, x)
  Res[float](ok: true)

echo check(3).ok, " ", check(-3).ok
echo checkPassive(3).ok, " ", checkPassive(-3).ok

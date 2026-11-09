# issue #975

type
  Utf16Char* = distinct int16

var x = Utf16Char(0xFFFD'i16) # Compiles without errors
const X = Utf16Char(0xFFFD'i16) # Error: cannot evaluate expression at compile time


from std / math import pow, sqrt
import std/[syncio, assertions]

func fibonacciClosedFormApproximation*(nth: Natural): int =
  # `int`, not `Natural`: `Natural` is a real `range` type, so binding a value
  # to one owes `0 <= value` at compile time and no range check is emitted. The
  # result here is a float expression, which carries no such proof — the
  # conversion has to name the type it can actually justify.
  const Sqrt5 = math.sqrt(5'f)
  const Phi = (Sqrt5 + 1) / 2 # golden ratio
  let powPhi = math.pow(Phi, nth.float)
  int(powPhi/Sqrt5 + 0.5)

assert fibonacciClosedFormApproximation(3) == 2

block:
  const s1 = float(1.3)
  const s2 = float(5)

  assert s1 == 1.3
  assert s2 == 5.0
# issue #2926: compile-time execution of a call with a distinct result type
import std/assertions

type
  Dur = distinct float32
  Freq = distinct float32
  Count = distinct int
  Flag = distinct bool

func `/`(a: float32, f: Freq): Dur = Dur(a / float32(f))
func twice(c: int): Count = Count(c * 2)
func flip(b: bool): Flag = Flag(not b)

const SampleDur = 1'f32 / 44100.Freq
const C = twice(21)
const F = flip(false)

assert float32(SampleDur) > 0'f32
assert int(C) == 42
assert bool(F)

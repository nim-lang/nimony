# issue #2926: const evaluation of a call with a distinct result type

import std/syncio

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

echo float32(SampleDur) > 0'f32
echo int(C)
echo bool(F)

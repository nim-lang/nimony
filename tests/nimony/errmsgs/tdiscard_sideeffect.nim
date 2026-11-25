# A `.noSideEffect` violation in a discarded expression is reported like any
# other: the temp the lowering binds the discarded `(err)` to has no hooks.

import std/syncio

var counter = 0

proc noisy(): bool =
  echo "noisy"
  result = true

proc noisyStr(): string =
  echo "noisy"
  result = "x"

proc raising(): int {.raises.} =
  echo "raising"
  result = 1

type O = ref object of RootObj
method m(o: O): int {.base.} = 1

func f(): int =
  discard noisy()
  result = 1

func managed(): int =
  discard noisyStr()
  result = 1

func global(): int =
  discard counter
  result = 1

func raises(): int {.raises.} =
  discard raising()
  result = 1

func dispatch(o: O): int =
  discard o.m()
  result = 1

echo f()

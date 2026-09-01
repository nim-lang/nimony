## Standalone concepts with ``typedesc[Self]`` requirements must not
## satisfy types that lack the required proc.

import std/assertions

type
  HasTrait = concept
    proc trait(_: typedesc[Self]): Self

type Plain = object
  x: int

assert not (Plain is HasTrait)

import std/syncio

type
  State = object
    generation: int

proc waitForGeneration(state: ptr State; generation: int) {.passive.} =
  while state[].generation == generation:
    suspend()

proc main =
  var state = State(generation: 1)
  waitForGeneration(addr state, 0)
  echo "field collision ok"

main()

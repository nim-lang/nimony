# An `except <ref type>:` arm in a generic routine (instantiated or not) and in
# a template: their bodies are published unlowered, and the arm holds a type.
import std/syncio

type Problem = ref object of Exception

proc check(n: int) {.raises: Problem.} =
  if n < 0: raise Problem(msg: "negative")

proc caught[T](x: T): bool =
  result = false
  try: check(-1)
  except Problem: result = true

proc unused[T](x: T) =
  try: discard
  except Problem: discard

template tryIt() =
  try: discard
  except Problem: discard

tryIt()
echo caught(0)

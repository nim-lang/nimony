
import std / [syncio]




proc main() {.taga: [RootEffect, IOEffect], raisea: [], deprecatea: "Use other instead", sideEffeca.} =
  echo "This one is real"

main()

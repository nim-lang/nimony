
import std / [syncio]

proc main() {.tags: [RootEffect, IOEffect], raises: [], deprecated: "Use other instead", sideEffect.} =
  runnableExamples:
    echo "Hello, world!"
  echo "This one is real"

main()

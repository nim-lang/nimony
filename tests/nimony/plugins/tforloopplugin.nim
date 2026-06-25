import std / syncio

iterator unroll3(): int {.plugin: "deps/mforloop".}

for x in unroll3():
  echo "iteration"

echo "after loop"

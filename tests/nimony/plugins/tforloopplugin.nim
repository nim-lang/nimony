import std / syncio

iterator unroll3(): int {.plugin: "deps/mforloop".}

# The plugin unrolls this into `total = total + 1/2/3`, substituting the
# (typed) loop variable. This only works because the for-loop plugin now
# receives a fully typed body in which `x` is a resolved symbol.
var total = 0
for x in unroll3():
  total = total + x
echo total

echo "after loop"

import std/[syncio, monotimes]

# getMonoTime under `nimony w` (wasm32/standalone) reads the tick fallback,
# which must pass the restricted-globals check; on both legs the clock is
# monotonic, so successive reads never go backwards.
let t1 = getMonoTime()
let t2 = getMonoTime()
let t3 = getMonoTime()
echo t2 >= t1
echo t3 >= t2

# OOM during a Table expansion through `[]=`.
#
# A Table grows two seqs: `data` (the pairs) and `hashes` (the open-addressing
# index). Under `-d:maxMem=1` (see `nimony.args`) the index runs out of room to
# grow partway through filling a big table, which is the interesting case: the
# probe loops assume a free slot exists, so `data` must not be allowed to run
# ahead of an index that can no longer be resized. It used to be, and a lookup
# for an absent key then spun forever.
#
# What this asserts is not WHERE the table stops -- that depends on the
# allocator -- but that whatever it ends up holding is consistent.
import std / [syncio, tables]

proc main() =
  var t = initTable[int, int]()
  for i in 0 ..< 200_000:
    t[i] = i * 2

  # The budget refused an expansion, so the table is short of what was asked.
  echo "stopped short: ", t.len < 200_000
  echo "not empty: ", t.len > 0

  # Everything it reports holding must be findable and correct. `pairs` walks
  # `data` directly; `hasKey`/`getOrDefault` go through the index (or the linear
  # fallback when there is none), so agreement between them is the real check.
  var walked = 0
  var missing = 0
  var wrong = 0
  for k, v in t.pairs:
    inc walked
    if v != k * 2: inc wrong
    if not t.hasKey(k): inc missing
    if t.getOrDefault(k, -1) != k * 2: inc wrong
  echo "walked all: ", walked == t.len
  echo "none missing: ", missing == 0
  echo "none wrong: ", wrong == 0

  # A key that was refused must read as absent rather than as a stale neighbour.
  echo "refused key absent: ", not t.hasKey(199_999)

  # Updating an existing key allocates nothing, so it must still work.
  t[0] = 99
  echo "update works: ", t.getOrDefault(0, -1) == 99
  echo "len unchanged by update: ", walked == t.len

  echo "still running"

main()

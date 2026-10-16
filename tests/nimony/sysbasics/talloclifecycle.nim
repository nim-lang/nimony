# Exercises the allocator selected by the driver. Illumos always uses native
# allocation, including with -d:useLibc. Large allocations bypass small bins.
import std/[assertions, syncio]

for n in [1, 16, 1024, 65536, 1024 * 1024, 32 * 1024 * 1024 + 1]:
  var p = cast[ptr UncheckedArray[byte]](alloc0(n))
  for i in 0..<n:
    assert p[i] == 0
    p[i] = byte(i mod 251)
  p = cast[ptr UncheckedArray[byte]](realloc(p, n + 4096))
  for i in 0..<n: assert p[i] == byte(i mod 251)
  for i in n..<n + 4096: p[i] = byte(123)
  p = cast[ptr UncheckedArray[byte]](realloc(p, n))
  for i in 0..<n: assert p[i] == byte(i mod 251)
  dealloc(p)

echo "allocator lifecycle ok"

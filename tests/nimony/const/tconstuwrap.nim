# Unsigned constant arithmetic wraps around exactly as at run time.
import std/syncio

proc rt[T](x: T): T {.noinline.} = x

const
  a = (127'u8 * 200'u8) div 2'u8
  b = 1'u8 - 2'u8
  c = high(uint64) + 1'u64
  d = 0xFFFFFFFFFFFFFFFF'u64 * 2'u64
  e = pred(0'u16)
  f = succ(high(uint64))

echo a, " ", (rt(127'u8) * rt(200'u8)) div rt(2'u8)
echo b, " ", rt(1'u8) - rt(2'u8)
echo c, " ", rt(high(uint64)) + rt(1'u64)
echo d, " ", rt(0xFFFFFFFFFFFFFFFF'u64) * rt(2'u64)
echo e, " ", pred(rt(0'u16))
echo f, " ", succ(rt(high(uint64)))

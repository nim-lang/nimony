import std/syncio
import std/crypto/sysrand

proc main() =
  var empty: array[0, byte] = default(array[0, byte])
  echo urandom(empty)

  # crosses the 256-byte limits of getentropy and an uninterrupted getrandom
  var a = newSeq[byte](1000)
  var b = newSeq[byte](1000)
  echo urandom(a), " ", urandom(b)
  echo a != b

  # not all zeros, and every byte value plausible: 1000 bytes leave on
  # average ~20 values unseen; far more than that means a broken source
  var seen = newSeq[bool](256)
  var different = 0
  for x in a:
    if not seen[int(x)]:
      seen[int(x)] = true
      inc different
  echo different > 200

main()

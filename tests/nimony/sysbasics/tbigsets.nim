var s, s1: set[uint8]
s = {0'u8..3'u8}
s1 = {0'u8..2'u8}
discard s1 < s
let y = s1 * s
let z: set[uint8] = y
discard y == {0'u8..2'u8}
discard z == {0'u8..2'u8}
discard s - s1 == {3'u8}
discard s1 <= s
let val = 3'u8
s1 = {0'u8..1, val, 5}

template resem() =
  s = {0'u8, 2'u8..4'u8, 5'u8}
  s1 = {0'u8..1, val, 5}

resem()

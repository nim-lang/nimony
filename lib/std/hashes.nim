type
  Hash* = uint

  Hashable* = concept
    proc hash(a: Self): Hash

proc `!&`*(h: Hash; val: uint): Hash {.inline.} =
  ## Mixes a hash value `h` with `val` to produce a new hash value.
  result = h + val
  result = result + (result shl 10)
  result = result xor (result shr 6)

proc `!$`*(h: Hash): Hash {.inline.} =
  ## Finishes the computation of the hash value.
  result = h + h shl 3
  result = result xor (result shr 11)
  result = result + result shl 15

proc hash*(s: string): Hash =
  result = 0'u
  for c in items(s):
    result = result !& uint(c)
  result = !$result

proc hash*(u: uint): Hash {.inline.} = u
when not defined(nimony):
  proc hash*(x: int): Hash {.inline.} = cast[Hash](x)

proc hash*(x: int64): Hash {.inline.} = cast[Hash](x)
proc hash*(x: int32): Hash {.inline.} = cast[Hash](int x)
proc hash*(x: char): Hash {.inline.} = Hash(x)
proc hash*(x: bool): Hash {.inline.} = Hash(x)
proc hash*[T: enum](x: T): Hash {.inline.} = Hash(x)

#[
proc hash*[T: object](x: T): Hash {.inline.} =
  result = 0'u
  for y in fields(x):
    result = result !& hash(y)
  result = !$result
]#

proc nextTry*(h: Hash; maxHash: int): Hash {.inline.} =
  result = (h + 1'u) and maxHash.uint

proc hashIgnoreStyle*(x: string): Hash =
  ## Efficient hashing of strings; style is ignored.
  ##
  ## **Note:** This uses a different hashing algorithm than `hash(string)`.
  ##
  ## **See also:**
  ## * `hashIgnoreCase <#hashIgnoreCase,string>`_
  # runnableExamples:
  #   doAssert hashIgnoreStyle("aBr_aCa_dAB_ra") == hashIgnoreStyle("abracadabra")
  #   doAssert hashIgnoreStyle("abcdefghi") != hash("abcdefghi")

  var h: Hash = 0
  var i = 0
  let xLen = x.len
  while i < xLen:
    var c = x[i]
    if c == '_':
      inc(i)
    else:
      if c in {'A'..'Z'}:
        c = chr(ord(c) + (ord('a') - ord('A'))) # toLower()
      h = h !& uint(ord(c))
      inc(i)
  result = !$h

proc hashIgnoreStyle*(sBuf: string, sPos, ePos: int): Hash =
  ## Efficient hashing of a string buffer, from starting
  ## position `sPos` to ending position `ePos` (included); style is ignored.
  ##
  ## **Note:** This uses a different hashing algorithm than `hash(string)`.
  ##
  ## `hashIgnoreStyle(myBuf, 0, myBuf.high)` is equivalent
  ## to `hashIgnoreStyle(myBuf)`.
  # runnableExamples:
  #   var a = "ABracada_b_r_a"
  #   doAssert hashIgnoreStyle(a, 0, 3) == hashIgnoreStyle(a, 7, a.high)

  var h: Hash = 0
  var i = sPos
  while i <= ePos:
    var c = sBuf[i]
    if c == '_':
      inc(i)
    else:
      if c in {'A'..'Z'}:
        c = chr(ord(c) + (ord('a') - ord('A'))) # toLower()
      h = h !& uint(ord(c))
      inc(i)
  result = !$h

proc hashIgnoreCase*(x: string): Hash =
  ## Efficient hashing of strings; case is ignored.
  ##
  ## **Note:** This uses a different hashing algorithm than `hash(string)`.
  ##
  ## **See also:**
  ## * `hashIgnoreStyle <#hashIgnoreStyle,string>`_
  # runnableExamples:
  #   doAssert hashIgnoreCase("ABRAcaDABRA") == hashIgnoreCase("abRACAdabra")
  #   doAssert hashIgnoreCase("abcdefghi") != hash("abcdefghi")

  var h: Hash = 0
  for i in 0..x.len-1:
    var c = x[i]
    if c in {'A'..'Z'}:
      c = chr(ord(c) + (ord('a') - ord('A'))) # toLower()
    h = h !& uint(ord(c))
  result = !$h

proc hashIgnoreCase*(sBuf: string, sPos, ePos: int): Hash =
  ## Efficient hashing of a string buffer, from starting
  ## position `sPos` to ending position `ePos` (included); case is ignored.
  ##
  ## **Note:** This uses a different hashing algorithm than `hash(string)`.
  ##
  ## `hashIgnoreCase(myBuf, 0, myBuf.high)` is equivalent
  ## to `hashIgnoreCase(myBuf)`.
  # runnableExamples:
  #   var a = "ABracadabRA"
  #   doAssert hashIgnoreCase(a, 0, 3) == hashIgnoreCase(a, 7, 10)

  var h: Hash = 0
  for i in sPos..ePos:
    var c = sBuf[i]
    if c in {'A'..'Z'}:
      c = chr(ord(c) + (ord('a') - ord('A'))) # toLower()
    h = h !& uint(ord(c))
  result = !$h

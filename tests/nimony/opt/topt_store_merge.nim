# Adjacent constant field stores that shoggoth's `memidioms` pass merges into
# one wider store on the native back end (#2350), and the near misses it must
# leave alone. Every field is read back afterwards, so a merged store that
# writes the wrong byte order, clobbers a neighbour or misses a field shows up
# as a diff against the C backend, which never merges.

import std / syncio

type
  Bytes = object
    a, b, c, d: uint8
    e: uint16
    f: bool
    g: char
    w: uint32
    x: uint32
  Gap = object
    a: uint8
    keep: uint8            # NOT stored: a merge over it would clobber it
    b: uint8
    c: uint8
  Padded = object
    a: uint8               # then one byte of padding
    b: uint16
    c: uint32
  Node = ref object
    tag, kind: uint8
    flags: uint16
    id: int32

proc dumpBytes(o: Bytes): string =
  $o.a & " " & $o.b & " " & $o.c & " " & $o.d & " " & $o.e & " " & $o.f & " " &
    $ord(o.g) & " " & $o.w & " " & $o.x

proc initBytes(o: var Bytes) =
  # one run: everything constant, in field order
  o.a = 1
  o.b = 2
  o.c = 3
  o.d = 0xFF
  o.e = 0xBEEF
  o.f = true
  o.g = 'z'
  o.w = 0xDEADBEEF'u32
  o.x = 7

proc reversed(o: var Bytes) =
  # out of field order: merging must not care
  o.d = 4
  o.c = 3
  o.b = 2
  o.a = 1

proc gap(o: var Gap) =
  o.a = 10
  o.b = 11
  o.c = 12

proc padded(o: var Padded) =
  o.a = 0x11
  o.b = 0x2233
  o.c = 0x44556677

proc interrupted(o: var Bytes; v: uint8) =
  # a non-constant store between them: nothing may move across it
  o.a = 9
  o.b = v
  o.c = 8
  o.d = 7

proc twice(o: var Bytes) =
  # the same field stored twice: the later store must win
  o.a = 1
  o.b = 2
  o.a = 3
  o.b = 4

proc initNode(n: Node) =
  n.tag = 0xAA
  n.kind = 0xBB
  n.flags = 0xCCDD
  n.id = -2

var globalBytes {.threadvar.}: Bytes

proc initGlobal() =
  globalBytes.a = 5
  globalBytes.b = 6
  globalBytes.c = 7
  globalBytes.d = 8

proc localOnly(): int =
  # a local stays as it is (taking its address would demote it to memory),
  # and the answer must be the same either way
  var o = Bytes()
  o.a = 1
  o.b = 2
  o.c = 3
  o.d = 4
  result = int(o.a) + int(o.b) * 10 + int(o.c) * 100 + int(o.d) * 1000

var o = Bytes()
initBytes(o)
echo "init: ", dumpBytes(o)
reversed(o)
echo "reversed: ", dumpBytes(o)
interrupted(o, 42)
echo "interrupted: ", dumpBytes(o)
twice(o)
echo "twice: ", dumpBytes(o)

var g = Gap(a: 0, keep: 99, b: 0, c: 0)
gap(g)
echo "gap: ", g.a, " ", g.keep, " ", g.b, " ", g.c

var p = Padded()
padded(p)
echo "padded: ", p.a, " ", p.b, " ", p.c

var n = Node()
initNode(n)
echo "node: ", n.tag, " ", n.kind, " ", n.flags, " ", n.id

initGlobal()
echo "global: ", dumpBytes(globalBytes)

echo "local: ", localOnly()

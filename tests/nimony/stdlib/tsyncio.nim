import std/[syncio, assertions]

stdout.write "test"

stdout.write false
stdout.write '\n'
stdout.write true
stdout.write '\n'

stdout.write 0
stdout.write '\n'
stdout.write 1
stdout.write '\n'
stdout.write 1234567890
stdout.write '\n'
stdout.write -1234567890
stdout.write '\n'
stdout.write 0'u64
stdout.write '\n'
stdout.write 1'u64
stdout.write '\n'
stdout.write 1234567890'u64
stdout.write '\n'
stdout.write 0'u32
stdout.write '\n'
stdout.write 1'u32
stdout.write '\n'
stdout.write int32.low
stdout.write '\n'
stdout.write int32.high
stdout.write '\n'
stdout.write uint32.high

stdout.write '\n'
stdout.write 'a'
stdout.write ' '
stdout.write '\n'

stdout.write -10.0
stdout.write '\n'
stdout.write -2.0
stdout.write '\n'
stdout.write -1.0
stdout.write '\n'
stdout.write -0.5
stdout.write '\n'
stdout.write 0.0
stdout.write '\n'
stdout.write 0.5
stdout.write '\n'
stdout.write 1.0
stdout.write '\n'
stdout.write 2.0
stdout.write '\n'
stdout.write 10.0
stdout.write '\n'

stdout.writeLine "Test Line"

echo false
echo true
echo "hello"
echo "abc", "123"
echo 123456789
echo 'a'
echo "xyz", 111'u, true, 'b'
echo 0.0, " ", 1.0

type
  DummyObject0 = object
    o, b, j, n: char
  DummyObject1 = object
    a, b, c, n: char

let obj0 = DummyObject0(o: 'o', b: 'b', j: 'j', n: '\n')
let obj1 = DummyObject1(a: 'a', b: 'b', c: 'c', n: '\n')
let n0 = stdout.writeBuffer(addr obj0, sizeof DummyObject0)
let n1 = stdout.writeBuffer(addr obj1, sizeof DummyObject1)

assert n0 == sizeof DummyObject0
assert n1 == sizeof DummyObject1
assert true

type MyTuple = tuple[a, b, c: char]
assert sizeof(MyTuple) == 3 # MyTuple not declared in c generated code

type
  MyObject = object
    a, b, c, d: int64

let my = default(MyObject)
assert sizeof(my) == 32

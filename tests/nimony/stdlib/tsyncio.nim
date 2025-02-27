import std/syncio

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

assert true

import std/[assertions, cstrutils, syncio]

assert startsWith(cstring"Hello, Nimion", cstring"Hello")
assert not startsWith(cstring"Hello, Nimion", cstring"Nimion")
assert startsWith(cstring"Hello", cstring"")
assert not startsWith(cstring"", cstring"Hello")

assert endsWith(cstring"Hello, Nimion", cstring"Nimion")
assert not endsWith(cstring"Hello, Nimion", cstring"Hello")
assert endsWith(cstring"Hello", cstring"")
assert not endsWith(cstring"", cstring"Hello")
assert not endsWith(cstring"Hi", cstring"Hello")

assert cmpIgnoreStyle(cstring"hello", cstring"H_e_L_Lo") == 0
assert cmpIgnoreStyle(cstring"hello", cstring"hello_") == 0
assert cmpIgnoreStyle(cstring"a", cstring"b") < 0
assert cmpIgnoreStyle(cstring"b", cstring"a") > 0

assert cmpIgnoreCase(cstring"hello", cstring"HeLLo") == 0
assert cmpIgnoreCase(cstring"echo", cstring"hello") < 0
assert cmpIgnoreCase(cstring"yellow", cstring"hello") > 0

echo "ok"

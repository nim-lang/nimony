import std/[assertions]

assert $0 == "0"
assert $1 == "1"
assert $(-1) == "-1"
assert $123 == "123"
assert $1234567890 == "1234567890"

assert $0'u == "0"
assert $1'u == "1"
assert $123'u == "123"
assert $1234567890'u == "1234567890"

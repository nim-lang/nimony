import std/assertions

var value = @[ "hello", "world", "!" ]

assert high( value ) == 2
assert  low( value ) == 0

assert high( "value" ) == 4
assert low( "value" ) == 0

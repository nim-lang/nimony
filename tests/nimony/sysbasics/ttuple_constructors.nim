import std / assertions

let positional = (1, "two")
assert positional[0] == 1
assert positional[1] == "two"

let named = (first: 1, second: "two")
assert named.first == 1
assert named.second == "two"

let escaped = (`a b`: 3, `type`: "four")
assert escaped.`a b` == 3
assert escaped.`type` == "four"

let nested = (outer: (inner: 5, other: "six"), tail: 7)
assert nested.outer.inner == 5
assert nested.outer.other == "six"
assert nested.tail == 7

let annotated: tuple[first: int, second: string] = (first: 8, second: "nine")
assert annotated.first == 8
assert annotated.second == "nine"

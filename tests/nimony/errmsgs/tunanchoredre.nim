# A `re` that a search could have to restart at every position, rereading the
# same bytes each time, is rejected while the program is compiled. `^` is
# relative to the attempt, so `^\d+$` is such a pattern; `\w+@\w+` (a literal
# every match contains) and `\d+` (an attempt cannot fail after reading many
# bytes) are not. `reUnanchored` opts out.

import std / rex

let a = re"\w+@\w+"
let b = re"\d+"
let c = re(r"^\d+$", {reExtended, reUnanchored})
let d = re"^\d+$"
let e = re"a|a*b"

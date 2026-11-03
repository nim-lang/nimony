# A `re` in which the same character could belong to two parts of the
# pattern is rejected while the program is compiled. Branches that merely
# start alike are factored first, and `reAllowEvil` opts out.

import std / rex

let a = re"\w+\s*=\s*\w+"
let b = re"if|in|int"
let c = re(r".*x", {reExtended, reAllowEvil})
let d = re".*x"
let e = re"a|a*b"

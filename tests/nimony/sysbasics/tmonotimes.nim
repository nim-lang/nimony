import std/[monotimes, assertions]


let a = getMonoTime()
let b = getMonoTime()
assert a <= b

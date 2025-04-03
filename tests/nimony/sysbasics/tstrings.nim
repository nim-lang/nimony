import std/assertions

proc main() =
  discard "abc" == "abc"

main()

var mytext = ""
for i in 0..<10:
  mytext.add "a"

assert mytext.len == 10

# --- cstring tests ---

func strlen(a: cstring): csize_t {.importc: "strlen", header: "<string.h>".}
proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

const abc = cstring "abc\n"
const hello = cstring "hello\n"
printf(abc)

let nimHello = fromCString(hello)
assert len(nimHello) == strlen(hello).int
assert cast[cstring](nimHello.rawData) != hello

var s = default(string)
s.add "nim string "
s.add nimHello
printf(s.toCString)

var zerotext = mytext.terminatingZero()
let ctext0 = cast[cstring](zerotext.rawData)
let ctext1 = zerotext.toCString()
# check redundant copy when already has zero terminated
assert ctext0 == ctext1 
assert zerotext.len == strlen(ctext0).int
assert zerotext.len == mytext.len
printf("%s\n", ctext0)

mytext.setLen(5)
# check terminating zero
assert s.isTerminatingZero() == true
assert nimHello.isTerminatingZero() == true
assert zerotext.isTerminatingZero() == true
assert mytext.isTerminatingZero() == false

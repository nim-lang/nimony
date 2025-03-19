import std/syncio

proc main() =
  discard "abc" == "abc"

main()

var mytext = ""
for i in 0..<10:
  mytext.add "a"

assert mytext.len == 10

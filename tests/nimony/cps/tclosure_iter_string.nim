import std / syncio

iterator names(): string {.closure.} =
  var s = "alice"
  yield s
  s.add "-bob"
  yield s
  s.add "-carol"
  yield s

proc main() =
  for n in names():
    echo n

main()

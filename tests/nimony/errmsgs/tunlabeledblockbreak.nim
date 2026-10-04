proc main() =
  block:
    break
  while true:
    block named:
      break named
    break

main()

block:
  var counter = 0
  let p = proc () {.closure.} =
    inc counter

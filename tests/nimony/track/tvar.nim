proc main() =
  var x = 42
  let a = x * 2
  #       ^usages
  let b = x + 1
  discard a + b

main()

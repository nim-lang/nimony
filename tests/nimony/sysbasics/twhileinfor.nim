# issue #703

proc main() =
  for i in 0 ..< 1:
    # Compile
    while true: discard i

    # could not find symbol: i.0
    while i == 0: discard

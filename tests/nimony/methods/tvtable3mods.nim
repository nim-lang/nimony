import std/syncio
import deps/mvtbase
import deps/mvtmid

type
  Poodle = object of Dog

# overrides only speak; move and identify are inherited from Dog/Animal
method speak*(p: Poodle): string = "Yip"

proc main() =
  var a = Animal()
  var d = Dog(name: "Rex")
  var p = Poodle()

  # dispatch through Animal vtable: each type should use its own overrides
  describe(a)  # ... / crawl / Animal
  describe(d)  # Woof / run / Animal   (identify falls back to Animal)
  describe(p)  # Yip / run / Animal    (move falls back to Dog, identify to Animal)

main()

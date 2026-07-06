
import std / [syncio]

type
  Animal = ref object of RootObj
    name: string
  Dog = ref object of Animal

proc main =
  # implicit base-object upcast of a non-nil ref: `ref Dog` widened to `ref Animal`
  var a: Animal = Dog(name: "rex")
  echo a.name

  # upcast flowing through a generic (`seq[Animal].add(Dog())`)
  var pack: seq[Animal] = @[]
  pack.add(Dog(name: "fido"))
  pack.add(a)
  echo pack.len
  echo pack[0].name

main()

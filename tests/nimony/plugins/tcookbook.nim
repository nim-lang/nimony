import std / syncio
import deps / cookbook_support

template defineDescribe(T: typedesc) {.plugin: "deps/mcookbook_describe".}
template checked(body: typed) {.plugin: "deps/mcookbook_check".}

type
  Person = object
    name: string
    age: int
  Empty = object
  Odd = object
    result, value: int

defineDescribe(Person)
defineDescribe(Empty)
defineDescribe(Odd)

proc nextValue(calls: var int): int =
  inc calls
  calls

var calls = 0
dbg(nextValue(calls))
echo "calls: ", calls
echo describe(Person(name: "Ann", age: 30))
echo describe(Empty())
echo describe(Odd(result: 1, value: 2))
checked:
  echo "checked"

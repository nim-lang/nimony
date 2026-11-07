import std / syncio

template defineDescribe(T: typedesc) {.plugin: "deps/mcookbook_describe".}

type Person = object
  age: int

proc add(target: var string; value: string) = target = "intercepted"
proc `&`(a, b: string): string = "intercepted"
proc `$`(value: int): string = "intercepted"

defineDescribe(Person)
echo describe(Person(age: 30))

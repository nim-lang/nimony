# issue #1404

type
  Foo* = object
    fooField*: int

  Bar* = object
    barField*: int

const fooField* = 12345

proc barField*(): int = 54321

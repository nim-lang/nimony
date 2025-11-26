
type
  TestTestObj = ref object of RootObj
    id: int

block:
  method foo(x: TestTestObj) =
    discard
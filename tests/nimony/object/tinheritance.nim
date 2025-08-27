import std/assertions

type
  Person = ref object of RootObj
    name*: string
    age: int
  
  Student = ref object of Person
    id: int

let
  student: Student

# object construction:
student = Student(name: "Anton", age: 5, id: 2)
assert student.name == "Anton"



type
  TestTestObj = object of RootObj
    id: int

var x = TestTestObj(id: 12)
assert x.id == 12


proc foo =
  type
    TestTestObj = object of RootObj
      id: int

  var x = TestTestObj(id: 12)
  assert x.id == 12

  block:
    type
      TestTestObj = object of RootObj
        id: int

    var x = TestTestObj(id: 12)
    assert x.id == 12

  block:
    type
      TestTestObj = object
        id: int

    var x = TestTestObj(id: 12)
    assert x.id == 12

foo()

block:
  type
    TestTestObj = object of RootObj
      id: int

  var x = TestTestObj(id: 12)
  assert x.id == 12


block:
  type
    TestTestObj = object of RootObj
      id: int

  var x = TestTestObj(id: 12)
  assert x.id == 12

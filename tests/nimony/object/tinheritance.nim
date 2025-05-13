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
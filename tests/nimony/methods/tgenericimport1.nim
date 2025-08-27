import deps/mgenericinheritance

testFoo(GenericObj[int]())# == "GenericObj int"
testFoo(GenericObj[float]())# == "GenericObj float"

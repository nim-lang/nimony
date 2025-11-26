import std/assertions
import deps/mvariables

assert varInt == 123
assert varAry == [456, 789]
assert varSet == {'v'}

assert letInt == 1234
assert letAry == [5678, 9876]
assert letSet == {'l'}

assert ConstInt == 12345
assert ConstAry == [67890, 54321]
assert ConstSet == {'c'}

changeVars()

assert varInt == -123
assert varAry == [-456, -789]
assert varSet == {'V'}

var
  varInt* = 123
  varAry* = [456, 789]
  varSet* = {'v'}

let
  letInt* = 1234
  letAry* = [5678, 9876]
  letSet* = {'l'}

const
  ConstInt* = 12345
  ConstAry* = [67890, 54321]
  ConstSet* = {'c'}

proc changeVars* =
  varInt = -123
  varAry = [-456, -789]
  varSet = {'V'}

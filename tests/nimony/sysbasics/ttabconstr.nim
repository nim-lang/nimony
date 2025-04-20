proc emptyArray(arr: array[0, int]) = discard
proc tableIntInt(arr: array[1, (int, int)]) = discard
proc tableStrInt(arr: array[2, (string, int)]) = discard
proc tableIntTup(arr: array[3, (int, (int, int))]) = discard

emptyArray {:}
tableIntInt {1: 2}
tableStrInt {"a": 1, "b": 2}
tableIntTup {1: (2, 3), 2: (4, 5), 3: (6, 7)}

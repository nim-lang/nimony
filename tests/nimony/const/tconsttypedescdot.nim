func fourtytwo*(x: typedesc[int]): int = 42

# Must work with built-in types

const builtInAnswer = int.fourtytwo
const builtInAnswer2 = int.fourtytwo * 2

# Must work with objects

type
  ObjectAnswer = object
    value: int

const objectAnswer = ObjectAnswer(value: int.fourtytwo)
const objectAnswer2 = ObjectAnswer(value: int.fourtytwo * 2)

# Must work with distinct types

type
  DistinctAnswer = distinct int

const distinctAnswer = DistinctAnswer(int.fourtytwo)
const distinctAnswer2 = DistinctAnswer(int.fourtytwo * 2)

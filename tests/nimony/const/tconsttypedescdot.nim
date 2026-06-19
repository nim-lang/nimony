# const distinct conversion from a typedesc dot call (e.g. Answer(int.fourtytwo))

type
  Answer = distinct int

func fourtytwo*(x: typedesc[int]): int = 42

const answer1 = Answer(42)
const answer2 = Answer(int.fourtytwo)

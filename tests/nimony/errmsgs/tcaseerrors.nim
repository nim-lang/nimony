let a = "abc"

case a
of "abc", "def": discard
of "ghi", "jkl", a: discard
of "mno", "abc": discard

let b = 123

case b
of 123, 456: discard
of 789, 122..124, b: discard
of 456: discard
else: discard

let c = cstring"abc"

case c
of cstring"abc", cstring"def": discard
of cstring"ghi", cstring"jkl", c: discard
of cstring"mno", cstring"abc": discard

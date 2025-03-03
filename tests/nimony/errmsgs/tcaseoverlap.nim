let a = "abc"

case a
of "abc", "def": discard
of "ghi", "jkl": discard
of "mno", "abc": discard

let b = 123

case b
of 123, 456: discard
of 789, 122..124: discard
of 456: discard
else: discard

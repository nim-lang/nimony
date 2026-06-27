## Regression for https://github.com/nim-lang/nimony/issues/1977
## Anonymous `block:` containing `try/except` must not generate duplicate C labels.

proc test() {.raises.} =
  block:
    try:
      discard
    except ErrorCode:
      discard

try:
  test()
except ErrorCode:
  discard

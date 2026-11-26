# An `except` arm takes one type: `(except .X X)`.

type
  ParseError = ref object of Exception
  OtherError = ref object of Exception

proc parse(s: string): int {.raises: ParseError.} =
  if s.len == 0: raise ParseError(msg: "empty")
  result = s.len

proc main() =
  try:
    discard parse("")
  except ParseError, OtherError:
    discard
  try:
    discard parse("")
  except ParseError, OtherError as e:
    discard
  try:
    discard parse("")
  except ErrorCode, ParseError:
    discard

proc generic[T](x: T) =
  try:
    discard parse("")
  except ParseError, OtherError:
    discard

main()
generic(1)

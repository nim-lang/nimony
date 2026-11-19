# `except A, B:` is one arm per type (#3044 item 5).
import std/syncio
type
  ParseError = ref object of Exception
  OtherError = ref object of Exception

proc parse(s: string): int {.raises: ParseError.} =
  if s.len == 0: raise ParseError(msg: "empty")
  result = s.len

proc other(s: string): int {.raises: OtherError.} =
  if s.len == 0: raise OtherError(msg: "other")
  result = s.len

proc code(s: string): int {.raises.} =
  if s.len == 0: raise Failure
  result = s.len

proc main() =
  try:
    discard parse("")
  except ParseError, OtherError:
    echo "caught 1"
  try:
    discard other("")
  except ParseError, OtherError:
    let x = 3
    echo "caught 2 ", x
  try:
    discard code("")
  except OtherError, ErrorCode:
    echo "caught 3"
  let v = try: other("") except ParseError, OtherError: 7
  echo v

main()

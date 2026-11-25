# Composite formatting for the built-in types.

func collectionToString[T: Stringable](x: openArray[T]; prefix, suffix: string): string =
  ## Shared implementation for the composite `$` overloads. `addQuoted`
  ## supplies the element quoting rules.
  result = prefix
  var first = true
  for value in x:
    if not first:
      result.add ", "
    else:
      first = false
    result.addQuoted value
  result.add suffix

func `$`*[T: Stringable](x: seq[T]): string =
  collectionToString(x, "@[", "]")

func `$`*[I, T: Stringable](x: array[I, T]): string =
  collectionToString(x, "[", "]")

func `$`*[T: Stringable](x: openArray[T]): string =
  collectionToString(x, "[", "]")

func `$`*[T: Ordinal and Stringable](x: set[T]): string =
  result = "{"
  var first = true
  for value in x:
    if not first:
      result.add ", "
    else:
      first = false
    result.addQuoted value
  result.add "}"

func `$`*[T: Stringable, U: Stringable](x: HSlice[T, U]): string =
  result = $x.a
  result.add " .. "
  result.add $x.b

func `$`*[T: tuple](x: T): string =
  result = "("
  var first = true
  var count = 0
  var named = false
  for name, value in fieldPairs(x):
    # `fieldPairs` invents `Field1`, `Field2`, ... for unnamed tuple fields;
    # suppress those so the output matches Nim's tuple syntax.
    let synthetic = name == "Field" & $(count + 1)
    inc count
    if not synthetic:
      named = true
    if not first:
      result.add ", "
    else:
      first = false
    if not synthetic:
      result.add name
      result.add ": "
    result.addQuoted value
  if count == 1 and not named:
    result.add ","
  result.add ")"

# `addFormatted` is the hook behind `&` on tuples. It appends a value's
# user-facing text to `s` without quoting. Types whose `$` is a debug view
# (quotes, type names, field lists) can overload it to format differently in
# `&` while keeping their `$`.

func addFormatted*(s: var string; x: string) {.inline.} =
  ## Appends `x` unchanged, without quotes.
  s.add x

func addFormatted*(s: var string; x: char) {.inline.} =
  ## Appends `x` unchanged, without quotes.
  s.add x

func addFormatted*[T: SomeSignedInt](s: var string; x: T) {.inline.} =
  ## Appends `x` in decimal, directly into `s`.
  s.addInt int64(x)

func addFormatted*[T: SomeUnsignedInt](s: var string; x: T) {.inline.} =
  ## Appends `x` in decimal, directly into `s`.
  s.addInt uint64(x)

func addFormatted*(s: var string; x: float) {.inline.} =
  ## Appends `x` like `$` does, directly into `s`.
  s.addFloat x

func addFormatted*(s: var string; x: float32) {.inline.} =
  ## Appends `x` like `$` does, directly into `s`.
  s.addFloat x

func addFormatted*[T: Stringable](s: var string; x: T) {.inline.} =
  ## Fallback for every other type: appends `$x`.
  s.add $x

template `&`*[T: tuple](x: T): string =
  ## Concatenates the fields of the tuple `x` into a single string. This is
  ## Nimony's replacement for `strformat`:
  ##
  ##   ```nim
  ##   let x = 42
  ##   assert &("x is ", x, "!") == "x is 42!"
  ##   ```
  ##
  ## Contract: each field is appended via `addFormatted(result, field)`.
  ## Strings and chars are added unquoted, numbers are written directly into
  ## the result, and any other `Stringable` type falls back to `$`. Overload
  ## `addFormatted` for a type to control how it appears inside `&`.
  var res = ""
  for value in fields(x):
    res.addFormatted value
  res

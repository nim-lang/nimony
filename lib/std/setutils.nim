func symmetricDifference*[T](x, y: set[T]): set[T] {.magic: "XorSet".}

func `-+-`*[T](x, y: set[T]): set[T] {.magic: "XorSet".}
 
proc toggle*[T: Ordinal](x: var set[T], y: T) {.inline.} =
  ## Toggles the element `y` in the set `x`.
  if x.contains(y):
    x.excl(y)
  else: x.incl(y)

template toggle*[T: Ordinal](x: var set[T], y: set[T]) =
  ## Toggles the set `y` in the set `x`.
  x = x.symmetricDifference(y)

# --- set complement ---

func emptySet*[T: Ordinal](x: typedesc[T]): set[T] {.inline.} =
  ## Returns an empty `set[T]`, same as `default(set[T])`
  result = {}

func fullSet*[T: Ordinal](x: typedesc[T]): set[T] {.inline.} =
  ## Returns a `set[T]` with all elements
  {T.low .. T.high}

func complement*[T: Ordinal](s: set[T]): set[T] {.inline.} =
  ## Returns a `set[T]` with the elements that are not inside
  fullSet(T) - s

func isEmpty*[T: Ordinal](s: set[T]): bool {.inline.} =
  ## Check if the `set[T]` is empty, same as `s == {}`
  s == {}

func isFull*[T: Ordinal](s: set[T]): bool {.inline.} =
  ## Check if the `set[T]` has all the elements,
  ## same as `s == fullSet(set[T])`
  s == fullSet(T)

# --- set element syntax sugar ---

func `[]=`*[T: Ordinal](x: var set[T], element: T, value: bool) {.inline.} =
  ## Syntax sugar for `if value: t.incl(element) else: t.excl(element)`
  if value: x.incl(element) else: x.excl(element)

func `[]`*[T: Ordinal](x: set[T], element: T): bool {.inline.} =
  ## Check if an element is inside the `set[T]`
  ## Syntax sugar of `x.contains(element)`
  x.contains(element)

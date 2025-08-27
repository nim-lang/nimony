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

func fullSet*[T: Ordinal](x: typedesc[T]): set[T] {.inline.} =
  ## Returns a `set[T]` with all elements
  {T.low .. T.high}

func complement*[T: Ordinal](s: set[T]): set[T] {.inline.} =
  ## Returns a `set[T]` with the elements that are not inside
  fullSet(T) - s

# --- set element syntax sugar ---

func `[]=`*[T: Ordinal](x: var set[T], element: T, value: bool) {.inline.} =
  ## Syntax sugar for `if value: t.incl(element) else: t.excl(element)`
  if value: x.incl(element) else: x.excl(element)

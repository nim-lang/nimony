func incl*[T](x: var set[T], y: T) {.magic: "Incl".}

template incl*[T](x: var set[T], y: set[T]) =
  ## Includes the set `y` in the set `x`.
  x = x + y

func excl*[T](x: var set[T], y: T) {.magic: "Excl".}

template excl*[T](x: var set[T], y: set[T]) =
  ## Excludes the set `y` from the set `x`.
  x = x - y

func card*[T](x: set[T]): int {.magic: "Card".}

func len*[T](x: set[T]): int {.magic: "Card".}


func `*`*[T](x, y: set[T]): set[T] {.magic: "MulSet".}

func `+`*[T](x, y: set[T]): set[T] {.magic: "PlusSet".}

func `-`*[T](x, y: set[T]): set[T] {.magic: "MinusSet".}

func contains*[T](x: set[T], y: T): bool {.magic: "InSet".}

proc cardSetImpl(s: ptr UncheckedArray[uint8], len: int): int {.inline.} =
  var i = 0
  result = 0
  var num = 0'u64
  when defined(x86) or defined(amd64):
    while i < len - 8:
      copyMem(addr num, addr s[i], 8)
      inc(result, countBits64(num))
      inc(i, 8)

  while i < len:
    inc(result, countBits32(uint32(s[i])))
    inc(i, 1)

proc cardSet(s: ptr UncheckedArray[uint8], len: int): int {.inline.} =
  result = cardSetImpl(s, len)

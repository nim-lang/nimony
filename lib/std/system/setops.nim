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

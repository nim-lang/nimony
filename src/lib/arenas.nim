type
  StableChunk[T] = object
    next: ptr StableChunk[T]
    len, cap: int
    data: UncheckedArray[T]

  Arena*[T] = object
    head, tail: ptr StableChunk[T]
    len*: int

proc createChunk[T](cap: int): ptr StableChunk[T] =
  result = cast[ptr StableChunk[T]](alloc0(sizeof(StableChunk[T]) + sizeof(T) * cap))
  result.cap = cap

proc createChunkUninit[T](cap: int): ptr StableChunk[T] =
  result = cast[ptr StableChunk[T]](alloc(sizeof(StableChunk[T]) + sizeof(T) * cap))
  result.next = nil
  result.len = 0
  result.cap = cap

proc createArena*[T](cap = 10): Arena[T] =
  var c = createChunk[T](cap)
  Arena[T](head: c, tail: c, len: 0)

proc destroy*[T](s: Arena[T]) =
  var it = s.head
  while it != nil:
    let next = it.next
    dealloc it
    it = next

proc new*[T](s: var Arena[T]): ptr T =
  if s.tail.len >= s.tail.cap:
    let oldTail = s.tail
    let newCap = oldTail.cap div 2 + oldTail.cap
    assert newCap > 2
    s.tail = createChunk[T](newCap)
    oldTail.next = s.tail

  result = addr s.tail.data[s.tail.len]
  inc s.tail.len
  inc s.len

proc newArrayUninit*[T](s: var Arena[T]; len: int): ptr UncheckedArray[T] =
  if s.tail.len + len > s.tail.cap:
    let oldTail = s.tail
    let newCap = max(oldTail.cap div 2 + oldTail.cap, len)
    assert newCap > 2
    s.tail = createChunkUninit[T](newCap)
    oldTail.next = s.tail

  result = addr s.tail.data[s.tail.len]
  inc s.tail.len, len
  inc s.len, len

iterator items*[T](s: Arena[T]): lent T =
  var it = s.head
  while it != nil:
    for i in 0 ..< it.len:
      yield it.data[i]
    it = it.next

when false:
  # unused
  proc nav[T](s: Arena[T]; i: var int): ptr StableChunk[T] =
    var it = s.head
    while it != nil:
      if i < it.len:
        return it
      dec i, it.len
      it = it.next
    return nil

  proc `[]`*[T](s: var Arena[T]; index: int): var T =
    var i = index
    let it = nav(s, i)
    if it != nil:
      return it.data[i]
    else:
      raise newException(IndexDefect, "index out of bounds")

  proc `[]`*[T](s: Arena[T]; index: int): lent T =
    var i = index
    let it = nav(s, i)
    if it != nil:
      return it.data[i]
    else:
      raise newException(IndexDefect, "index out of bounds")

  proc `[]=`*[T](s: var Arena[T]; index: int; elem: sink T) =
    var i = index
    let it = nav(s, i)
    if it != nil:
      it.data[i] = elem
    else:
      raise newException(IndexDefect, "index out of bounds")

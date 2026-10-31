import mcyc4_stream {.cyclic.}

type
  SendEnd = ref object
    w: Writer

  Sender*[T] = object
    e: SendEnd

proc sender*[T](w: sink Writer): Sender[T] =
  Sender[T](e: SendEnd(w: w))

proc newSender*(): Sender[int] = sender[int](newWriter())

type
  Buffer = object
    ## Also exercises the cyclic import's scope: instantiating `seq[char]`
    ## here must not leak a `seq` symbol into the importer.
    data: seq[char]

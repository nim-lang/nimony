import "mcyc3_stream" {.cyclic.}

export mcyc3_stream.Reader, mcyc3_stream.Writer

type
  End = object
    w: Writer
    r: Reader

  Sender*[T] = object
    e: End

proc send*[T](s: Sender[T]; x: sink T): bool =
  discard cProc()
  true

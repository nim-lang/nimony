type
  Option*[T] = object
    case
    of Some:
      val*: T
    of None:
      discard

  Either*[A, B] = object
    case
    of Left:
      a*: A
    of Right:
      b*: B

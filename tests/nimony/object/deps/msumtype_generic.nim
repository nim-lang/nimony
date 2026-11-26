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

  Flip* = object # `Ok`/`Err` in the opposite order of tsumtype_generic's `Res`
    case
    of Err:
      msg*: string
    of Ok:
      n*: int

  Pair*[T] = object
    first*: T
    second*: T

proc getOrDefault*[T](opt: Option[T]; default: T): T =
  case opt
  of Some(val):
    result = val
  of None():
    result = default

proc flipErr*(msg: string): Flip = Err(msg: msg)

import std / syncio

template inspect(body: typed): untyped {.plugin: "deps/moperatorbranchviews".}

inspect:
  proc exercise(start: int): float =
    var x = start
    let y = float(x) + 1.5
    while x < 5:
      x = x + 1
    if x == 5 and y != 0.0:
      if y < 5.0:
        result = y * 2.0
      else:
        result = -y
    elif x <= 0:
      result = 0.0
    else:
      result = float(x) / 2.0
    if result < 0.0:
      result = -result

  echo exercise(3)
  echo exercise(0)
  echo exercise(6)

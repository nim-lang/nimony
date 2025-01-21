{.emit: "int s = 12;".}

proc foo =
  var s = 12
  {.emit: [s, " = ", 3, ";"].}

foo()
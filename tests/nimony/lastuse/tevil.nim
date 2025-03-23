# Carefully crafted, evil test cases



proc use(x: sink string) = discard

proc tevilearlyexit(cond: bool) {.report: "lastuse".} =
  var a = "3"
  use(a)
  #[  ^notlastuse]#
  block lab:
    if cond:
      break lab
    a = "4"
  use(a)
  #[  ^lastuse]#

tevilearlyexit(true)

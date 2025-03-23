


proc use(x: sink string) = discard

proc main(cond: bool) {.report: "lastuse".} =
  var a = "3"
  var b = "33"
  use(a)
  #[  ^notlastuse]#
  if cond:
    use(a)
    #[  ^lastuse]#
  else:
    use(b)
    #[  ^lastuse]#
    use(a)
    #[  ^lastuse]#

main(true)

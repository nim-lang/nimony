

proc use(x: sink string) = discard

proc main(cond: bool) {.report: "lastuse".} =
  var a = "3"

  use(a)
  #[  ^lastuse]#
  if cond:
    a = "5"
  else:
    a = "6"
  use(a)

proc main2(cond: bool) {.report: "lastuse".} =
  var a = "3"

  use(a)
  #[  ^notlastuse]#
  if cond:
    a = "5"
  use(a)

proc main3(cond: bool) {.report: "lastuse".}=
  var a = "3"
  while cond:
    use(a)
    #[  ^notlastuse]#

proc main4(cond: bool) {.report: "lastuse".} =
  var a = "3"
  if cond:
    discard
  else:

    while true:
      use(a)
      #[  ^lastuse]#
      break

main(true)
main2(false)
main3(false)
main4(true)

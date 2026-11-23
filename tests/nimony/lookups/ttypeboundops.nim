import std / syncio
import deps / msandwich2

proc main =
  var s90 = toMyS("abc")
  echo s90

main()

# #33: a routine attached to an argument's type ranks like an imported one:
# `Box[T]` beats the imported `describe[T](x: T)`, and `default` of an `Opt`
# field picks `std/opt`'s template over the `DefaultObj` magic.
import deps / mboundholder

echo describe(holderBox())
echo default(Holder).childIsNone

import std/syncio

type
  Animal* {.inheritable.} = object

method speak*(a: Animal): string = "..."
method move*(a: Animal): string = "crawl"
method identify*(a: Animal): string = "Animal"

proc describe*(a: Animal) =
  echo speak(a), " / ", move(a), " / ", identify(a)

import std / [syncio]

template here(): untyped = instantiationInfo()

template logHere(msg: string) =
  let pos = instantiationInfo()
  echo pos.filename, ":", pos.line, " ", msg

let direct = instantiationInfo()
echo direct.filename, ":", direct.line

let viaTemplate = here()
echo viaTemplate.filename, ":", viaTemplate.line

logHere("first")
logHere("second")

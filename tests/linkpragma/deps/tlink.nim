# `{.link.}` with a constant expression and no extension (`.o` is appended).
# `setup.nim` copies this next to the object it compiled.

import std/syncio

const stem = "ans" & "wer"
{.link: stem.}
{.link: "answer.o".}   # the same file twice is linked once

proc answer(): cint {.importc: "answer".}

echo answer()

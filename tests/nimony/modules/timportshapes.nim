# What a module compiled as an IMPORT looks like to the back ends.
#
# Nothing here is about modules as a language feature: it is about position.
# `hastur native` compiles every test as a MAIN module, so until the Windows
# tree walk began compiling a directory's joined group natively (whose members
# ARE imports) nothing in the suite ever handed the native backend a non-main
# module — and three separate bugs were living in that gap. This test closes it
# for every host: the shapes live in `deps/mimportshapes`, which is only ever
# compiled as an import.
import std / [syncio]
import deps/mimportshapes

echo splitAt("foo.nim/bar.nim")
echo theAnswer()

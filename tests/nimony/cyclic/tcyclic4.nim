# A 3-module `{.cyclic.}` group where the first module lowered needs the RTTI
# `=destroy` of a class declared in the third. The class's owner is lowered
# last, so before the group-wide hook table it had published no hook yet and
# each member forged its own module-suffixed one — none of which was in the
# class's vtable ("method `=destroy_...` not found in class").
import std/syncio
import deps/mcyc4_dns

var b = make()
echo "ok"

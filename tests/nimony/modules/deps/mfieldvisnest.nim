{.feature: "untyped".}

import mfieldvis

# A generic in a THIRD module that expands an `untyped` template of its own
# while operating on `mfieldvis`'s type. Exercises expansion nested inside
# instantiation across three modules at once: the `visOwner` frame walk must
# still attribute each access to the module its code was written in.
template touchPublicImpl*() {.dirty.} =
  x.public = x.public

proc touchPublic*[T](x: var Generic[T]) =
  touchPublicImpl()

# Legitimate: written in `mfieldvis`, called from here.
proc viaOwner*[T](x: Generic[T]): T =
  result = readPrivate(x)

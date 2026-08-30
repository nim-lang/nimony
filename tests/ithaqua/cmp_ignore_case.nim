# `strutils.cmpIgnoreCase`. Was quarantined as a native-backend bug: arkham x64
# aborted with "scalar store rhs Undef" (the register allocator handed the store a
# module-level-symbol Location). Both legs agree again.
import std/[syncio, strutils]
echo cmpIgnoreCase("Hello", "hello") == 0
echo cmpIgnoreCase("apple", "Banana") < 0

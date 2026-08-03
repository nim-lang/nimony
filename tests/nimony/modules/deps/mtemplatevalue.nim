type Handle* = nil pointer

# A *template*, not a `const`: the initializer is a pointer cast, which nimony
# cannot const-evaluate. Mirrors the `template HKEY_LOCAL_MACHINE*: HKEY =
# cast[HKEY](0x80000002'u64)` idiom used cross-module as an argument to an
# importc'd Win32 proc.
template MAGIC_HANDLE*: Handle = cast[Handle](0x80000002'u64)

proc probe*(h: Handle): int32 =
  if h == nil: 0'i32 else: 1'i32

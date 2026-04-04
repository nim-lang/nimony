## Some type definitions for compatibility between different
## backends and platforms.

type
  BiggestInt* = int64
    ## is an alias for the biggest signed integer type the Nim compiler
    ## supports. Currently this is `int64`, but it is platform-dependent
    ## in general.

  BiggestFloat* = float64
    ## is an alias for the biggest floating point type the Nim
    ## compiler supports. Currently this is `float64`, but it is
    ## platform-dependent in general.

  BiggestUInt* = uint64
    ## is an alias for the biggest unsigned integer type the Nim compiler
    ## supports. Currently this is `uint64`, but it is platform-dependent
    ## in general.

when defined(windows):
  type
    clong* {.importc: "long", nodecl.} = int32
      ## This is the same as the type `long` in *C*.
    culong* {.importc: "unsigned long", nodecl.} = uint32
      ## This is the same as the type `unsigned long` in *C*.
else:
  type
    clong* {.importc: "long", nodecl.} = int
      ## This is the same as the type `long` in *C*.
    culong* {.importc: "unsigned long", nodecl.} = uint
      ## This is the same as the type `unsigned long` in *C*.

type # these work for most platforms:
  cchar* {.importc: "char", nodecl.} = char
    ## This is the same as the type `char` in *C*.
  cschar* {.importc: "signed char", nodecl.} = int8
    ## This is the same as the type `signed char` in *C*.
  cshort* {.importc: "short", nodecl.} = int16
    ## This is the same as the type `short` in *C*.
  cint* {.importc: "int", nodecl.} = int32
    ## This is the same as the type `int` in *C*.
  csize_t* {.importc: "size_t", nodecl.} = uint
    ## This is the same as the type `size_t` in *C*.
  clonglong* {.importc: "long long", nodecl.} = int64
    ## This is the same as the type `long long` in *C*.
  cfloat* {.importc: "float", nodecl.} = float32
    ## This is the same as the type `float` in *C*.
  cdouble* {.importc: "double", nodecl.} = float64
    ## This is the same as the type `double` in *C*.
  clongdouble* {.importc: "long double", nodecl.} = BiggestFloat
    ## This is the same as the type `long double` in *C*.
    ## This C type is not supported by Nim's code generator.

  cuchar* {.importc: "unsigned char", nodecl, deprecated: "use `char` or `uint8` instead".} = char
    ## Deprecated: Use `uint8` instead.
  cushort* {.importc: "unsigned short", nodecl.} = uint16
    ## This is the same as the type `unsigned short` in *C*.
  cuint* {.importc: "unsigned int", nodecl.} = uint32
    ## This is the same as the type `unsigned int` in *C*.
  culonglong* {.importc: "unsigned long long", nodecl.} = uint64
    ## This is the same as the type `unsigned long long` in *C*.

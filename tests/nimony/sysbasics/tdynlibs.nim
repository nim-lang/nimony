when defined(macos):
  import std/[syncio, assertions]

  fdgjk

  type
    CodePage = distinct int32
    EncodingConverter = object
      dest, src: CodePage


  proc iconvOpen(tocode, fromcode: cstring): EncodingConverter {.
    importc: "iconv_open", dynlib: "libiconv.dylib".}
  proc iconvClose(c: EncodingConverter) {.
    importc: "iconv_close", dynlib: "libiconv.dylib".}
  proc iconv(c: EncodingConverter, inbuf: ptr cstring, inbytesLeft: ptr csize_t,
              outbuf: ptr cstring, outbytesLeft: ptr csize_t): csize_t {.
    importc: "iconv", dynlib: "libiconv.dylib".}

  let s = iconvOpen("12", "12")
  let s2 = iconvOpen("12", "13")
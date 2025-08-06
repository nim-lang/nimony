when defined(posix):
  import std/[syncio, assertions]

  type
    CodePage = distinct int32
    EncodingConverter = object
      dest, src: CodePage

  when defined(macosx):
    const libiconv = "libiconv.dylib"
  else:
    const libiconv = "(libc.so.6|libiconv.so)"

  proc iconvOpen(tocode, fromcode: cstring): EncodingConverter {.
    importc: "iconv_open", dynlib: libiconv.}
  proc iconvClose(c: EncodingConverter) {.
    importc: "iconv_close", dynlib: libiconv.}
  proc iconv(c: EncodingConverter, inbuf: ptr cstring, inbytesLeft: ptr csize_t,
              outbuf: ptr cstring, outbytesLeft: ptr csize_t): csize_t {.
    importc: "iconv", dynlib: libiconv.}

  let s = iconvOpen("12", "12")
  let s2 = iconvOpen("12", "13")

  iconvClose(s)
  iconvClose(s2)

  proc fox() =
    let s = iconvOpen("12", "12")
    let s2 = iconvOpen("12", "13")
    iconvClose(s)
    iconvClose(s2)

  fox()

  block:
    let s = iconvOpen("12", "12")
    let s2 = iconvOpen("12", "13")
    iconvClose(s)
    iconvClose(s2)

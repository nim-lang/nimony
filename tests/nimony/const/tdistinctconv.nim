# issue #975

type
  Utf16Char* = distinct int16

var x = Utf16Char(0xFFFD'i16) # Compiles without errors
const X = Utf16Char(0xFFFD'i16) # Error: cannot evaluate expression at compile time

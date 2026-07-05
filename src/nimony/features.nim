#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.


type
  Feature* = enum ## A feature is a language mode that is purely frontend-related.
                  ## A `CheckMode` is something Hexer needs to know about too.
    InvalidFeature
    ResemChoiceFeature
    UntypedFeature
    CanRaiseFeature
    LenientConvertersFeature
    EarlyMagicsFeature
    AutoClosuresFeature
    LenientNilsFeature
    LenientLetsFeature
    IgnoreStyleFeature
      ## Nim-2-style "style insensitivity": identifier lookup ignores
      ## underscores and ASCII case after the first character. Strictly a
      ## sem-frontend toggle; hexer / lengc / nifmake are unaffected.
    VarToverloadsFeature
      ## Nim-2-style overloading by `var T`: when two routines differ only
      ## in whether a parameter is `var T` or `T`, prefer the `var T`
      ## overload (mirrors `sumGeneric` +1 for `tyVar` in old Nim's
      ## `complexDisambiguation`).
    LenientFloatsFeature
      ## Nim-2-style implicit narrowing of float *values* (named constants and
      ## arbitrary expressions, not just literals) to a smaller float type,
      ## e.g. passing a `float64` constant where a `float32` is expected. Off by
      ## default because the narrowing can silently lose precision; opt in with
      ## `.feature: "lenientFloats".` (v2 implies it).

proc normalizeFeatureName(s: string): string =
  result = newStringOfCap(s.len)
  for ch in s:
    if ch == '_': discard
    elif ch >= 'A' and ch <= 'Z': result.add chr(ord(ch) + (ord('a') - ord('A')))
    else: result.add ch

proc parseFeatures*(s: string): set[Feature] =
  case normalizeFeatureName(s)
  of "resemchoice": {ResemChoiceFeature}
  of "untyped": {UntypedFeature}
  of "canraise": {CanRaiseFeature}
  of "lenientconverters": {LenientConvertersFeature}
  of "earlymagics": {EarlyMagicsFeature}
  of "autoclosures": {AutoClosuresFeature}
  of "lenientnils": {LenientNilsFeature}
  of "lenientlets": {LenientLetsFeature}
  of "ignorestyle": {IgnoreStyleFeature}
  of "vartoverloads": {VarToverloadsFeature}
  of "lenientfloats": {LenientFloatsFeature}
  of "v2": {UntypedFeature, LenientConvertersFeature, EarlyMagicsFeature,
            AutoClosuresFeature, LenientNilsFeature, IgnoreStyleFeature,
            VarToverloadsFeature, LenientFloatsFeature, LenientLetsFeature}
  else: {}

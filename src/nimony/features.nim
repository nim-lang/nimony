#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.


type
  Feature* = enum ## A feature is a language mode that is purely frontend-related.
                  ## A `CheckMode` is something Hexer needs to know about too.
    InvalidFeature
    NotnilFeature

proc parseFeature*(s: string): Feature =
  case s
  of "notnil": NotnilFeature
  else: InvalidFeature



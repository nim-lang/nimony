import std / syncio

type
  MyS* = distinct string

proc write*(f: File; s: MyS) = write f, string(s)

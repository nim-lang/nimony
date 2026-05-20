# Helper module for `tcross_module_ord_case.nim`: exports an enum whose
# values are computed via `ord(ParentEnum)` (mirrors the `nimony_tags.nim`
# pattern) plus a set constant built from some of those fields.

type
  Tags* = enum
    TgZero
    TgA
    TgB
    TgC
    TgD
    TgE

type
  E* = enum
    NoE
    A = (ord(TgA), "a")
    B = (ord(TgB), "b")
    C = (ord(TgC), "c")
    D = (ord(TgD), "d")
    F = (ord(TgE), "f")

const MySet* = {A, C, F}

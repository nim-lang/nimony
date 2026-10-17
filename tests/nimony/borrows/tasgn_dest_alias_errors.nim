# The destination's address is taken before the right-hand side runs, so a
# right-hand side that may grow the seq would store into freed memory (#2638).

type
  Sym = object
    body: int
  P = object
    symbols: seq[Sym]
    n: int

proc grow(p: var P): int =
  p.symbols.add Sym(body: -1)
  p.symbols.add Sym(body: -1)
  result = 42

proc peek(p: P): int = p.symbols.len

proc viaParam(p: var P) =
  p.symbols[0].body = grow(p)

proc nested(p: var P) =
  p.symbols[0].body = 1 + grow(p)

proc fine(p: var P) =
  let value = grow(p)
  p.symbols[0].body = value
  p.n = grow(p)
  p.symbols[0].body = peek(p)

var p = P(symbols: @[Sym(body: -1)])
p.symbols[0].body = grow(p)
viaParam p
nested p
fine p

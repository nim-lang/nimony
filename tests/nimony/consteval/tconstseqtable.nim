import std/assertions

# A const table of seq literals: every `@[...]` used to cost a full native
# sub-compile, so a few hundred entries took minutes. `expreval` now folds
# `@` on a constant array in-process.

type
  Obj = object
    a: int
    b: string

const
  recipes = [
    ("almonds", @["almond"]),
    ("apple bread", @["apple", "wheat"]),
    ("bahn mi", @["tofu", "wheat", "lettuce", "radish"]),
  ]
  objs = @[Obj(a: 1, b: "x"), Obj(a: 2, b: "y")]
  nested = @[@[1], @[2, 3]]
  fromArr = @(default(array[0, int]))

assert recipes.len == 3
assert recipes[0][1].len == 1
assert recipes[0][1][0] == "almond"
assert recipes[1][1][1] == "wheat"
assert recipes[2][1].len == 4
assert recipes[2][1][3] == "radish"

assert objs.len == 2
assert objs[1].a == 2 and objs[1].b == "y"

assert nested.len == 2
assert nested[0][0] == 1
assert nested[1].len == 2 and nested[1][1] == 3

assert fromArr.len == 0

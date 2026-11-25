type
  Res[T, E] = object
    case
    of Ok: v: T
    of Err: e: E

  Flip = object
    case
    of Ok: n: int
    of Bad: msg: string

# The mismatch is reported on the field, as for `let r: Res[int, string] = Err(e: 5)`.
proc wrongField(): Res[int, string] = Res[int, string].Err(e: 5)

# A branch of another sum type is not a branch of this one.
proc otherBranch(): Res[int, string] = Res[int, string].Bad(msg: "x")

# The branch is named, not positional.
proc positional(): Res[int, string] = Res[int, string].Ok(5)

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

# A type argument left as `_` that no field determines is named.
proc undetermined(): Res[int, string] = Res[int, _].Ok(v: 5)

# A field that contradicts a written argument is reported on the field, also
# while another parameter is still open.
proc contradicted(): Res[int, string] = Res[int, _].Ok(v: "x")

# A written type argument is not overridden by the fields.
type
  Both[A, B] = object
    case
    of Pair:
      a: A
      b: B

proc clash(): Both[int, int] = Both[int, _].Pair(a: "x", b: 1)

proc tooMany(): Res[int, string] = Res[_, _, _].Ok(v: 5)

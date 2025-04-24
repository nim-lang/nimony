#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Implements the core of exception handling.
- We transform `result = x` to `result = (Success, x)`, likewise `return`.
- We translate `proc p(params): T {.raises.}` to `proc p(params): (ErrorCode, T)`.
- We transform `let/var local: T = rcall(args)` to `let/var local: (ErrorCode, T) = rcall(args);
  if local[0] != Success: raise (local[0], result)`
  and other usages of `local` to `local[1]`.
- We transform other `rcalls` to `let tmp = rcall(args); if tmp[0] != Success: raise (tmp[0], result)`
- We transform `raise e` to `raise (e, result)`.
- Destroyer's job is to replicate `finally` sections and destructors for every side exit like `raise`,
  `return` and `break`.
- nifcgen's job is to translate `raise` either to a `goto errHandler` or to a `return`, depending on
  whether the `raise` is inside a `try` block or not.
]##




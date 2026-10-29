# nim-lang/nimony#2784: features are module-scoped, so a `.feature` written in
# a routine body is rejected instead of silently changing every routine.
proc h(s: string; i: int): char =
  result = s[i]

proc g(s: string; i: int): char =
  {.feature: "staticContracts".}
  result = ' '

discard h("ab", 0)
discard g("ab", 0)

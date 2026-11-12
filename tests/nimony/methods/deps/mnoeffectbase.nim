type Shape* = ref object of RootObj
method area*(s: Shape): int {.base, noSideEffect.} = 0

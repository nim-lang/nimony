# copied from manual:
proc callme(x, y: int, s: string = "", c: char, b: bool = false) = discard

callme(0, 1, "abc", '\t', true)
callme(y=1, x=0, "abd", '\t')
callme(c='\t', y=1, x=0)
callme 0, 1, "abc", '\t'

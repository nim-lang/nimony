let (a, b) = (1, 2)
let (_, (c, _)) = (1, (2, 3))

# ensure underscores work manually too:
let tup = (1, 2, 3)
let _ = tup[0]
let d = tup[1]
let _ = tup[2]

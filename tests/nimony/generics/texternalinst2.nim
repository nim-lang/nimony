import deps/[mexternalinst2_1, mexternalinst2_2]

let x1: Inst1 = default(Inst1)
let x2: Inst2 = default(Inst2)
# compare to each other:
let y1: Inst2 = x1
let y2: Inst1 = x2

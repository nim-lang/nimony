# A sum type has no `default`, so it does not satisfy `HasDefault` (#3044 item 18).
type
  Status = object
    case
    of Running:
      discard
    of Exited:
      code: int

var s: seq[Status] = @[]
s.setLen(0)
let x = newSeq[Status](2)

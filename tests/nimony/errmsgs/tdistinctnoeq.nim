# A `distinct` ordinal has no `==` unless it borrows one (#2902).
type
  Day = distinct int

discard Day(1) == Day(2)

## Style-insensitive identifier lookup ({.feature: "ignoreStyle".}).
## The helper module exports `myProcName`; here we call it under spellings
## that differ only in case-after-first-char or underscore placement (Nim 2
## rules — the first character must still match exactly).

{.feature: "ignoreStyle".}

import std / [assertions, syncio]
import deps / mignorestyle

assert myProcName(0) == 1
assert my_proc_name(1) == 2
assert myprocname(2) == 3
assert myPROCNAME(3) == 4
assert my_Proc_Name(4) == 5

# Enum-field lookup goes through the same path.
let s: HttpStatus = http_not_found
assert s == httpNotFound

echo "OK"

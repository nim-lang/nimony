## String implementation.

include memory

type
  StrData = ptr UncheckedArray[char]

const
  EmptyI = 0 # length==0 and not allocated
  LenShift = 1
  IsAllocatedBit = 1

proc len*(s: string): int {.inline, ensures: (0 <= result).} =
  result = s.i shr LenShift

proc high*(s: string): int {.inline.} = len(s)-1

proc cap(s: string): int {.inline.} = allocatedSize(s.a)

template isAllocated(s: string): bool = (s.i and IsAllocatedBit) != 0

proc capacity*(s: string): int =
  result = if isAllocated(s): s.cap else: 0

proc `=wasMoved`*(s: var string) {.exportc: "nimStrWasMoved", inline.} =
  s.i = EmptyI

proc `=destroy`*(s: string) {.exportc: "nimStrDestroy", inline.} =
  if isAllocated(s): dealloc(s.a)

template safeCopyMem(dest: var string; src: string; len, allocated: int) =
  if dest.a != nil:
    copyMem dest.a, src.a, len
    dest.i = (len shl LenShift) or IsAllocatedBit
  else:
    oomHandler allocated
    dest.i = EmptyI

proc `=copy`*(dest: var string; src: string) {.exportc: "nimStrCopy", inline, nodestroy.} =
  if dest.a == src.a:
    return
  let len = src.len
  if isAllocated(dest):
    let cap = dest.cap
    if cap >= len:
      copyMem(dest.a, src.a, len)
      dest.i = (len shl LenShift) or IsAllocatedBit
    else:
      let newCap = max(len, cap + (cap shr 1))
      dealloc dest.a
      dest.a = cast[StrData](alloc(newCap))
      # `alloc` here because `realloc` would copy the memory but that
      # is not required as it is overwritten anyway here:
      safeCopyMem(dest, src, len, newCap)
  elif isAllocated(src):
    dest.a = cast[StrData](alloc(len))
    safeCopyMem(dest, src, len, len)
  else:
    dest = src

proc `=dup`*(s: string): string {.exportc: "nimStrDup", inline, nodestroy.} =
  if isAllocated(s):
    let len = s.len
    result = string(a: cast[StrData](alloc(len)), i: s.i)
    if result.a != nil:
      copyMem result.a, s.a, len
    else:
      oomHandler len
      result.i = EmptyI
  else:
    result = s

proc borrowCStringUnsafe*(s: cstring; len: int): string =
  ## Creates a Nim string from a `cstring` by borrowing the
  ## underlying storage. You have to ensure the `cstring` lives
  ## long enough for this to be safe! If in doubt,
  ## use `fromCString` instead.
  string(a: cast[StrData](s), i: (len shl LenShift))

func strlen(a: cstring): csize_t {.importc: "strlen", header: "<string.h>".}

proc borrowCStringUnsafe*(s: cstring): string =
  ## Creates a Nim string from a `cstring` by borrowing the
  ## underlying storage. You have to ensure the `cstring` lives
  ## long enough for this to be safe! If in doubt,
  ## use `fromCString` instead.
  string(a: cast[StrData](s), i: (int(strlen(s)) shl LenShift))

proc ensureTerminatingZero*(s: var string) =
  let len = s.len
  if isAllocated(s):
    let cap = s.cap
    if cap > len:
      s.a[len] = '\0'
    else:
      let newCap = len+1
      let a = cast[StrData](realloc(s.a, newCap))
      if a != nil:
        a[len] = '\0'
        s.a = a
      else:
        oomHandler newCap
        # ensure zero termination anyway:
        s.a = cast[StrData](cstring"")
        s.i = EmptyI
  else:
    let newCap = len+1
    let a = cast[StrData](alloc(newCap))
    if a != nil:
      copyMem a, s.a, len
      a[len] = '\0'
      s.a = a
      s.i = s.i or IsAllocatedBit
    else:
      oomHandler newCap
      # ensure zero termination anyway:
      s.a = cast[StrData](cstring"")
      s.i = EmptyI

proc toCString*(s: var string): cstring =
  ensureTerminatingZero(s)
  result = cast[cstring](s.a)

proc rawData*(s: string): ptr UncheckedArray[char] {.inline.} =
  result = s.a

proc growImpl(s: var string; newLen: int) =
  let cap = s.cap
  if newLen > cap:
    let newCap = max(newLen, cap + (cap shr 1))
    s.a = cast[StrData](realloc(s.a, newCap))
    if s.a != nil:
      s.i = (newLen shl LenShift) or IsAllocatedBit
    else:
      oomHandler newCap
      s.i = EmptyI

proc makeAllocated(s: var string; newLen: int) =
  let len = s.len
  let newCap = max(newLen, len + (len shr 1))
  let existing = s.a
  s.a = cast[StrData](alloc(newCap))
  if s.a != nil:
    s.i = (newLen shl LenShift) or IsAllocatedBit
    if len > 0:
      copyMem addr(s.a[0]), existing, len
  else:
    oomHandler newCap
    s.i = EmptyI

proc add*(s: var string; part: string) =
  let len = s.len
  let newLen = len + part.len
  if not isAllocated(s):
    makeAllocated s, newLen
  else:
    growImpl s, newLen
  if s.a != nil and part.len > 0:
    copyMem addr(s.a[len]), part.a, part.len

proc add*(s: var string; c: char) =
  let newLen = s.len+1
  if not isAllocated(s):
    makeAllocated s, newLen
  else:
    growImpl s, newLen
  if s.a != nil:
    s.a[newLen-1] = c

proc setLen*(s: var string; newLen: int) =
  let len = s.len
  if newLen < len:
    s.i = newLen shl LenShift or (s.i and IsAllocatedBit)
  elif newLen > len:
    if not isAllocated(s):
      makeAllocated s, newLen
    else:
      growImpl s, newLen

proc shrink*(s: var string; newLen: int) =
  if newLen < 0:
    s.i = s.i and IsAllocatedBit
  elif newLen < s.len:
    s.i = newLen shl LenShift or (s.i and IsAllocatedBit)

proc `[]=`*(s: var string; i: int; c: char) {.requires: (i < len(s) and i >= 0), inline.} =
  s.a[i] = c

proc `[]`*(s: string; i: int): char {.requires: (i < len(s) and i >= 0), inline.} = s.a[i]

proc substr*(s: string; first, last: int): string =
  let len = s.len
  let f = if first >= 0 and first < len: first else: 0
  let l = if last >= 0 and last < len: last+1 else: len
  if l <= f:
    result = string(a: cast[StrData](cstring""), i: EmptyI)
  else:
    let newLen = l - f
    if isAllocated(s):
      result = string(a: cast[StrData](alloc(newLen)), i: (newLen shl LenShift) or IsAllocatedBit)
      if result.a != nil:
        copyMem result.a, addr s.a[f], newLen
      else:
        oomHandler newLen
        result.i = EmptyI
    else:
      # slices into data we don't own anyway can be done without copy:
      result = string(a: cast[StrData](addr s.a[f]), i: newLen shl LenShift)

proc substr*(s: string; first = 0): string =
  result = substr(s, first, high(s))

proc `==`*(a, b: string): bool {.exportc: "nimStrEq", inline.} =
  if a.len == b.len:
    result = cmpMem(a.a, b.a, a.len) == 0
  else:
    result = false

proc nimStrAtLe(s: string; idx: int; ch: char): bool {.exportc: "nimStrAtLe", inline.} =
  result = idx < s.len and s[idx] <= ch

proc cmpStrings(a, b: string): int =
  let alen = a.len
  let blen = b.len
  let minlen = min(alen, blen)
  if minlen > 0:
    result = cmpMem(a.a, b.a, minlen)
    if result == 0:
      result = alen - blen
  else:
    result = alen - blen

proc `<=`*(a, b: string): bool {.inline.} =
  cmpStrings(a, b) <= 0

proc `<`*(a, b: string): bool {.inline.} =
  cmpStrings(a, b) < 0

proc prepareMutation*(s: var string) =
  if not isAllocated(s):
    let len = s.len
    let a = cast[StrData](alloc(len))
    if a != nil:
      copyMem a, s.a, len
      s.i = (len shl LenShift) or IsAllocatedBit
    else:
      oomHandler len
      s.i = EmptyI
    s.a = a # also do this for `a == nil`

proc newString*(len: int): string =
  let a = cast[StrData](alloc(len))
  if a != nil:
    result = string(a: a, i: (len shl LenShift) or IsAllocatedBit)
  else:
    oomHandler len
    result = string(a: nil, i: EmptyI)

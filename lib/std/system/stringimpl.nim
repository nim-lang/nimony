## String implementation.

include memory

type
  StrData = ptr UncheckedArray[char]
  StrImpl* = object
    a: StrData # can be nil for OutOfMem
    i: int

const
  Empti = 1 # length==0 and not allocated; name is NOT a typo
  LenShift = 1
  IsStaticMask = 1

proc len*(s: StrImpl): int {.inline.} =
  result = s.i shr LenShift

proc high*(s: StrImpl): int {.inline.} = len(s)-1

proc cap(s: StrImpl): int {.inline.} = allocatedSize(s.a)

template isAllocated(s: StrImpl): bool = (s.i and IsStaticMask) == 0

proc capacity*(s: StrImpl): int =
  result = if isAllocated(s): s.cap else: 0

proc `=wasMoved`*(s: var StrImpl) {.exportc: "nimStrWasMoved", inline.} =
  s.i = Empti

template safeCopyMem(dest: var StrImpl; src: StrImpl; len, allocated: int) =
  if dest.a != nil:
    copyMem dest.a, src.a, len
    dest.i = len shl LenShift
  else:
    oomHandler allocated
    dest.i = Empti

proc `=destroy`*(s: StrImpl) {.exportc: "nimStrDestroy", inline.} =
  if isAllocated(s): dealloc(s.a)

proc `=copy`*(dest: var StrImpl; src: StrImpl) {.exportc: "nimStrCopy", inline, nodestroy.} =
  if dest.a == src.a:
    return
  let len = src.len
  if isAllocated(dest):
    let cap = dest.cap
    if cap >= len:
      copyMem(dest.a, src.a, len)
      dest.i = len shl LenShift
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

proc `=dup`*(s: StrImpl): StrImpl {.exportc: "nimStrDup", inline, nodestroy.} =
  if isAllocated(s):
    let len = s.len
    result = StrImpl(a: cast[StrData](alloc(len)), i: s.i)
    if result.a != nil:
      copyMem result.a, s.a, len
    else:
      oomHandler len
      result.i = Empti
  else:
    result = s

proc borrowCStringUnsafe*(s: cstring; len: int): StrImpl =
  ## Creates a Nim string from a `cstring` by borrowing the
  ## underlying storage. You have to ensure the `cstring` lives
  ## long enough for this to be safe! If in doubt,
  ## use `fromCString` instead.
  StrImpl(a: cast[StrData](s), i: (len shl LenShift) or IsStaticMask)

proc ensureTerminatingZero*(s: var StrImpl) =
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
        s.i = Empti
  else:
    let newCap = len+1
    let a = cast[StrData](alloc(newCap))
    if a != nil:
      copyMem a, s.a, len
      a[len] = '\0'
      s.a = a
    else:
      oomHandler newCap
      # ensure zero termination anyway:
      s.a = cast[StrData](cstring"")
      s.i = Empti

proc toCString*(s: var StrImpl): cstring =
  ensureTerminatingZero(s)
  result = cast[cstring](s.a)

proc growImpl(s: var StrImpl; newLen: int) =
  let cap = s.cap
  if newLen > cap:
    let newCap = max(newLen, cap + (cap shr 1))
    s.a = cast[StrData](realloc(s.a, newCap))
    if s.a != nil:
      s.i = newLen shl LenShift
    else:
      oomHandler newCap
      s.i = Empti

proc makeAllocated(s: var StrImpl; newLen: int) =
  let len = s.len
  let newCap = max(newLen, len + (len shr 1))
  s.a = cast[StrData](alloc(newCap))
  if s.a != nil:
    s.i = newLen shl LenShift
  else:
    oomHandler newCap
    s.i = Empti

proc add*(s: var StrImpl; part: StrImpl) =
  let len = s.len
  let newLen = len + part.len
  if not isAllocated(s):
    makeAllocated s, newLen
  else:
    growImpl s, newLen
  if s.a != nil:
    copyMem addr(s.a[len]), part.a, part.len

proc add*(s: var StrImpl; c: char) =
  let newLen = s.len+1
  if not isAllocated(s):
    makeAllocated s, newLen
  else:
    growImpl s, newLen
  if s.a != nil:
    s.a[newLen-1] = c

proc setLen*(s: var StrImpl; newLen: int) =
  let len = s.len
  if newLen < len:
    s.i = newLen shl LenShift or (s.i and IsStaticMask)
  elif newLen > len:
    if not isAllocated(s):
      makeAllocated s, newLen
    else:
      growImpl s, newLen

proc shrink*(s: var StrImpl; newLen: int) =
  if newLen < 0:
    s.i = s.i and IsStaticMask
  elif newLen < s.len:
    s.i = newLen shl LenShift or (s.i and IsStaticMask)

proc `[]=`*(s: var StrImpl; i: int; c: char) {.requires: (i < s.len and i >= 0), inline.} =
  s.a[i] = c

proc `[]`*(s: StrImpl; i: int): char {.requires: (i < s.len and i >= 0), inline.} = s.a[i]

proc substr*(s: StrImpl; first, last: int): StrImpl =
  let len = s.len
  let f = if first >= 0 and first < len: first else: 0
  let l = if last >= 0 and last < len: last+1 else: len
  if l <= f:
    result = StrImpl(a: cast[StrData](cstring""), i: Empti)
  else:
    let newLen = l - f
    if isAllocated(s):
      result = StrImpl(a: cast[StrData](alloc(newLen)), i: newLen shl LenShift or (s.i and IsStaticMask))
      if result.a != nil:
        copyMem result.a, addr s.a[f], newLen
      else:
        oomHandler newLen
        result.i = Empti
    else:
      # slices into data we don't own anyway can be done without copy:
      result = StrImpl(a: cast[StrData](addr s.a[f]), i: newLen shl LenShift or (s.i and IsStaticMask))

proc substr*(s: StrImpl; first = 0): StrImpl =
  result = substr(s, first, high(s))

proc `==`*(a, b: StrImpl): bool =
  if a.len == b.len:
    result = cmpMem(a.a, b.a, a.len) == 0
  else:
    result = false

proc cmpStrings(a, b: StrImpl): int =
  let alen = a.len
  let blen = b.len
  let minlen = min(alen, blen)
  if minlen > 0:
    result = cmpMem(a.a, b.a, minlen)
    if result == 0:
      result = alen - blen
  else:
    result = alen - blen

proc `<=`*(a, b: StrImpl): bool {.inline.} =
  cmpStrings(a, b) <= 0

proc `<`*(a, b: StrImpl): bool {.inline.} =
  cmpStrings(a, b) < 0

proc prepareMutation*(s: var StrImpl) =
  if not isAllocated(s):
    let len = s.len
    let a = cast[StrData](alloc(len))
    if a != nil:
      copyMem a, s.a, len
      s.i = len shl LenShift
    else:
      oomHandler len
      s.i = Empti
    s.a = a # also do this for `a == nil`

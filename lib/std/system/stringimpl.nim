## SSO string implementation.
##
## Three tiers:
##   short  (slen <= AlwaysAvail=7):  all data inline in `bytes`
##   medium (8 <= slen <= PayloadSize=14): all data inline in `bytes` + `more` (reinterpreted)
##   long   (slen == HeapSlen=255):   data on heap via `more: ptr LongString`
##   static (slen == StaticSlen=254): data in a static `LongString` (never freed)
##
## `bytes` layout (LE): byte0=slen, bytes1..AlwaysAvail=inline chars, bytes for more ptr reused

const
  HeapSlen   = 255  ## slen sentinel: heap-allocated long string
  StaticSlen = 254  ## slen sentinel: static/literal long string (capImpl=0, never freed)
  AlwaysAvail = sizeof(uint) - 1          ## inline chars that fit alongside slen in `bytes`
  PayloadSize = AlwaysAvail + sizeof(pointer) - 1  ## total inline capacity (short+medium)

const LongStringDataOffset = 3 * sizeof(int)  ## byte offset of LongString.data from start

# ---- atomic helpers (non-atomic for now) ----

func atomicAddFetch(p: var int; v: int): int {.inline.} =
  result = p + v
  p = result

func atomicSubFetch(p: var int; v: int): int {.inline.} =
  result = p - v
  p = result

# ---- low-level byte accessors ----

template ssLen(s: string): int =
  ## Reads slen from byte 0 of `bytes`.
  int(cast[ptr byte](addr s.bytes)[])

template setSSLen(s: var string; v: int) =
  ## Writes slen to byte 0 of `bytes`.
  cast[ptr byte](addr s.bytes)[] = cast[byte](v)

template inlinePtr(s: string): ptr UncheckedArray[char] =
  ## Read-only pointer to the inline char storage (offset +1 from `bytes`).
  cast[ptr UncheckedArray[char]](cast[uint](addr s.bytes) + 1'u)

template inlinePtrV(s: var string): ptr UncheckedArray[char] =
  ## Mutable pointer to the inline char storage (offset +1 from `bytes`).
  cast[ptr UncheckedArray[char]](cast[uint](addr s.bytes) + 1'u)

template inlinePtrAt(s: var string; offset: int): pointer =
  ## Mutable pointer to inline char at `offset` (avoids addr-of-template-index issues).
  cast[pointer](cast[uint](addr s.bytes) + 1'u + uint(offset))

# ---- raw data pointer ----

func rawData(s {.byref.}: string): ptr UncheckedArray[char] {.inline.} =
  ## Returns a pointer into s's char data.
  if ssLen(s) > PayloadSize:
    result = cast[ptr UncheckedArray[char]](addr s.more.data[0])
  else:
    result = inlinePtr(s)

# ---- length ----

func len*(s: string): int {.inline, semantics: "string.len", ensures: (0 <= result).} =
  result = ssLen(s)
  if result > PayloadSize:
    result = s.more.fullLen

func high*(s: string): int {.inline.} = len(s) - 1
func low*(s: string): int {.inline.} = 0

func capacity*(s: string): int =
  let sl = ssLen(s)
  if sl == HeapSlen: s.more.capImpl
  elif sl == StaticSlen: s.more.fullLen
  else: PayloadSize

# ---- read-only raw data API (public, safe for external callers) ----

func readRawData*(s {.byref.}: string; start = 0): ptr UncheckedArray[char]
    {.inline, noSideEffect, raises: [], tags: [].} =
  ## Returns a read-only pointer to s[start].
  ## For inline strings, the pointer is into s's inline storage;
  ## s must remain alive while the pointer is used.
  if ssLen(s) > PayloadSize:
    result = cast[ptr UncheckedArray[char]](cast[uint](addr s.more.data[0]) + uint(start))
  else:
    result = cast[ptr UncheckedArray[char]](cast[uint](inlinePtr(s)) + uint(start))

# ---- lifecycle hooks ----

func `=wasMoved`*(s: var string) {.exportc: "nimStrWasMoved", inline.} =
  s.bytes = 0

func `=destroy`*(s: string) {.exportc: "nimStrDestroy", inline.} =
  let sl = ssLen(s)
  if sl == HeapSlen:
    if atomicSubFetch(s.more.rc, 1) == 0:
      dealloc(s.more)

func `=copy`*(dest: var string; src: string) {.exportc: "nimStrCopy", inline, nodestroy.} =
  let ssrc = ssLen(src)
  if ssrc <= PayloadSize:
    # short/medium: destroy dest heap block if any, then bitcopy
    let sdest = ssLen(dest)
    if sdest == HeapSlen:
      if atomicSubFetch(dest.more.rc, 1) == 0:
        dealloc(dest.more)
    copyMem(addr dest.bytes, addr src.bytes, sizeof(string))
  else:
    # long or static: COW share
    if addr(dest) == addr(src): return
    let sdest = ssLen(dest)
    if sdest == HeapSlen:
      if atomicSubFetch(dest.more.rc, 1) == 0:
        dealloc(dest.more)
    if ssrc == HeapSlen:
      discard atomicAddFetch(src.more.rc, 1)
    copyMem(addr dest.bytes, addr src.bytes, sizeof(string))

func `=dup`*(s: string): string {.exportc: "nimStrDup", inline, nodestroy.} =
  let sl = ssLen(s)
  if sl == HeapSlen:
    discard atomicAddFetch(s.more.rc, 1)
  result = string(bytes: s.bytes, more: s.more)

# ---- cstring length ----

func strlen(a: cstring): csize_t {.importc: "strlen", header: "<string.h>".}

func len*(a: cstring): int {.inline.} =
  if a == nil: 0
  else: a.strlen.int

# ---- cstring view ----

func nimStrToCString*(s {.byref.}: string): cstring {.inline, exportc: "nimStrToCString".} =
  ## Zero-cost cstring view. SSO strings are always null-terminated.
  if ssLen(s) > PayloadSize:
    result = cast[cstring](addr s.more.data[0])
  else:
    result = cast[cstring](inlinePtr(s))

# ---- internal growth helpers ----

func ssResize(old: int): int {.inline.} =
  if old <= 0: 4
  elif old <= 32767: old * 2
  else: old div 2 + old

func ensureUniqueLong(s: var string; oldLen, newLen: int) =
  ## Ensures s.more is a unique writable heap block with capacity >= newLen.
  let sl = ssLen(s)
  let isHeap = sl == HeapSlen
  let cap = if isHeap: s.more.capImpl else: 0
  if isHeap and s.more.rc == 1 and newLen <= cap:
    s.more.fullLen = newLen
  else:
    let newCap = if newLen > cap: max(newLen, ssResize(cap)) else: cap
    let p = cast[ptr LongString](alloc(LongStringDataOffset + newCap + 1))
    if p != nil:
      p.rc = 1
      p.fullLen = newLen
      p.capImpl = newCap
      let old = s.more
      copyMem(addr p.data[0], addr old.data[0], min(oldLen, newCap) + 1)
      if isHeap and atomicSubFetch(old.rc, 1) == 0:
        dealloc(old)
      s.more = p
      setSSLen(s, HeapSlen)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + newCap + 1
      s.bytes = 0

func transitionToLong(s: var string; sl: int; newLen: int) =
  let newCap = max(newLen, ssResize(newLen))
  let p = cast[ptr LongString](alloc(LongStringDataOffset + newCap + 1))
  if p != nil:
    p.rc = 1
    p.fullLen = newLen
    p.capImpl = newCap
    copyMem(addr p.data[0], inlinePtrV(s), sl)
    s.more = p
    setSSLen(s, HeapSlen)
  else:
    {.cast(noSideEffect).}: oomHandler LongStringDataOffset + newCap + 1
    s.bytes = 0

# ---- mutation helpers ----

func prepareMutation*(s: var string) {.inline.} =
  ## Ensures s's data is uniquely owned (not shared with another string or static).
  let sl = ssLen(s)
  if sl == StaticSlen or (sl == HeapSlen and s.more.rc > 1):
    if sl == HeapSlen:
      discard atomicSubFetch(s.more.rc, 1)
    let old = s.more
    let oldLen = old.fullLen
    let p = cast[ptr LongString](alloc(LongStringDataOffset + oldLen + 1))
    if p != nil:
      p.rc = 1
      p.fullLen = oldLen
      p.capImpl = oldLen
      copyMem(addr p.data[0], addr old.data[0], oldLen + 1)
      s.more = p
      setSSLen(s, HeapSlen)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + oldLen + 1
      s.bytes = 0

func prepareMutationAt*(s: var string; i: int): var char {.requires: (i < len(s) and i >= 0), inline.} =
  ## Ensures s is uniquely owned and returns a mutable reference to s[i].
  ## Note: may transition short/medium strings to heap for borrowing.
  prepareMutation(s)
  let sl = ssLen(s)
  if sl <= PayloadSize:
    # Transition inline string to heap so we have a borrowable .data field.
    transitionToLong(s, sl, sl)
  result = s.more.data[i]

# ---- beginStore / endStore for bulk writes ----

func beginStore*(s: var string; ensuredLen: int; start = 0): ptr UncheckedArray[char]
    {.inline, noSideEffect, raises: [], tags: [].} =
  ## Prepares s for a bulk write. s.len must be >= ensuredLen beforehand.
  ## Call endStore(s) afterwards to sync the hot prefix cache.
  if ssLen(s) > PayloadSize:
    ensureUniqueLong(s, s.more.fullLen, s.more.fullLen)
    result = cast[ptr UncheckedArray[char]](cast[uint](addr s.more.data[0]) + uint(start))
  else:
    result = cast[ptr UncheckedArray[char]](cast[uint](inlinePtrV(s)) + uint(start))

func endStore*(s: var string) {.inline, noSideEffect, raises: [], tags: [].} =
  ## Syncs the hot prefix cache after a bulk write via beginStore.
  ## No-op for short/medium strings.
  if ssLen(s) > PayloadSize:
    copyMem(inlinePtrV(s), addr s.more.data[0], AlwaysAvail)

# ---- add char / string ----

func add*(s: var string; c: char) =
  let sl = ssLen(s)
  if sl < PayloadSize:
    let newLen = sl + 1
    inlinePtrV(s)[sl] = c
    inlinePtrV(s)[newLen] = '\0'
    setSSLen(s, newLen)
  elif sl > PayloadSize:
    let l = s.more.fullLen
    if sl == HeapSlen and s.more.rc == 1 and l < s.more.capImpl:
      s.more.data[l] = c
      s.more.data[l + 1] = '\0'
      s.more.fullLen = l + 1
      if l < AlwaysAvail:
        inlinePtrV(s)[l] = c
    else:
      let oldLen = s.more.fullLen
      ensureUniqueLong(s, oldLen, oldLen + 1)
      if ssLen(s) == HeapSlen:
        s.more.data[oldLen] = c
        s.more.data[oldLen + 1] = '\0'
        if oldLen < AlwaysAvail:
          inlinePtrV(s)[oldLen] = c
  else:
    # sl == PayloadSize → transition to long
    transitionToLong(s, sl, sl + 1)
    if ssLen(s) == HeapSlen:
      s.more.data[sl] = c
      s.more.data[sl + 1] = '\0'

func add*(s: var string; part: string) =
  let partLen = part.len
  if partLen == 0: return
  let partData = rawData(part)  # fetch before any mutation (safe: part is local copy)
  let sl = ssLen(s)
  if sl <= PayloadSize:
    let sLen = sl
    let newLen = sLen + partLen
    if newLen <= PayloadSize:
      copyMem(inlinePtrAt(s, sLen), partData, partLen)
      inlinePtrV(s)[newLen] = '\0'
      setSSLen(s, newLen)
    else:
      transitionToLong(s, sLen, newLen)
      if ssLen(s) == HeapSlen:
        copyMem(cast[pointer](cast[uint](addr s.more.data[0]) + uint(sLen)), partData, partLen)
        s.more.data[newLen] = '\0'
        copyMem(inlinePtrV(s), addr s.more.data[0], AlwaysAvail)
  else:
    let sLen = s.more.fullLen
    let newLen = sLen + partLen
    ensureUniqueLong(s, sLen, newLen)
    if ssLen(s) == HeapSlen:
      copyMem(cast[pointer](cast[uint](addr s.more.data[0]) + uint(sLen)), partData, partLen)
      s.more.data[newLen] = '\0'
      if sLen < AlwaysAvail:
        copyMem(inlinePtrV(s), addr s.more.data[0], AlwaysAvail)

# ---- setLen / shrink ----

func setLen*(s: var string; newLen: int) =
  let sl = ssLen(s)
  let curLen = if sl > PayloadSize: s.more.fullLen else: sl
  if newLen == curLen: return
  if newLen <= 0:
    if sl == HeapSlen:
      if atomicSubFetch(s.more.rc, 1) == 0: dealloc(s.more)
    s.bytes = 0
    return
  if sl <= PayloadSize:
    if newLen <= PayloadSize:
      let inl = inlinePtrV(s)
      if newLen > curLen:
        zeroMem(inlinePtrAt(s, curLen), newLen - curLen)
        inl[newLen] = '\0'
        setSSLen(s, newLen)
      else:
        inl[newLen] = '\0'
        setSSLen(s, newLen)
    else:
      transitionToLong(s, curLen, newLen)
      if ssLen(s) == HeapSlen:
        zeroMem(cast[pointer](cast[uint](addr s.more.data[0]) + uint(curLen)), newLen - curLen)
        s.more.data[newLen] = '\0'
  else:
    if newLen <= PayloadSize:
      let old = s.more
      copyMem(inlinePtrV(s), addr old.data[0], newLen)
      inlinePtrV(s)[newLen] = '\0'
      if sl == HeapSlen and atomicSubFetch(old.rc, 1) == 0:
        dealloc(old)
      setSSLen(s, newLen)
    else:
      ensureUniqueLong(s, curLen, newLen)
      if ssLen(s) == HeapSlen:
        if newLen > curLen:
          zeroMem(cast[pointer](cast[uint](addr s.more.data[0]) + uint(curLen)), newLen - curLen)
        s.more.data[newLen] = '\0'

func shrink*(s: var string; newLen: int) =
  if newLen <= 0:
    if ssLen(s) == HeapSlen:
      if atomicSubFetch(s.more.rc, 1) == 0: dealloc(s.more)
    s.bytes = 0
  elif newLen < s.len:
    let sl = ssLen(s)
    if sl <= PayloadSize:
      inlinePtrV(s)[newLen] = '\0'
      setSSLen(s, newLen)
    else:
      ensureUniqueLong(s, s.more.fullLen, newLen)
      if ssLen(s) == HeapSlen:
        s.more.data[newLen] = '\0'

# ---- indexing ----

func `[]`*(s: string; i: int): char {.requires: (i < len(s) and i >= 0), inline.} =
  if ssLen(s) > PayloadSize: s.more.data[i]
  else: inlinePtr(s)[i]

func `[]=`*(s: var string; i: int; c: char) {.requires: (i < len(s) and i >= 0), inline.} =
  prepareMutation(s)
  if ssLen(s) > PayloadSize:
    s.more.data[i] = c
    if i < AlwaysAvail: inlinePtrV(s)[i] = c
  else:
    inlinePtrV(s)[i] = c

# ---- substr / slicing ----

func substr*(s: string; first, last: int): string =
  result = string(bytes: 0'u, more: nil)
  let sLen = s.len
  let f = max(first, 0)
  let l = min(last, sLen - 1) + 1
  if l <= f: return
  let newLen = l - f
  let src = rawData(s)
  if newLen <= PayloadSize:
    setSSLen(result, newLen)
    copyMem(inlinePtrV(result), cast[pointer](cast[uint](src) + uint(f)), newLen)
    inlinePtrV(result)[newLen] = '\0'
  else:
    let p = cast[ptr LongString](alloc(LongStringDataOffset + newLen + 1))
    if p != nil:
      p.rc = 1
      p.fullLen = newLen
      p.capImpl = newLen
      copyMem(addr p.data[0], cast[pointer](cast[uint](src) + uint(f)), newLen)
      p.data[newLen] = '\0'
      result.more = p
      setSSLen(result, HeapSlen)
      copyMem(inlinePtrV(result), addr p.data[0], AlwaysAvail)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + newLen + 1
      result.bytes = 0

func substr*(s: string; first = 0): string =
  result = substr(s, first, high(s))

# ---- comparison ----

func equalStrings(a, b: string): bool {.inline.} =
  let la = a.len
  let lb = b.len
  if la != lb: return false
  if la == 0: return true
  cmpMem(rawData(a), rawData(b), la) == 0

func `==`*(a, b: string): bool {.inline, semantics: "string.==".} =
  equalStrings(a, b)

func nimStrAtLe(s: string; idx: int; ch: char): bool {.inline.} =
  result = idx < s.len and s[idx] <= ch

func cmpStrings(a, b: string): int =
  let la = a.len
  let lb = b.len
  let minLen = min(la, lb)
  if minLen > 0:
    result = cmpMem(rawData(a), rawData(b), minLen)
    if result == 0: result = la - lb
  else:
    result = la - lb

func `<=`*(a, b: string): bool {.inline.} = cmpStrings(a, b) <= 0
func `<`*(a, b: string): bool {.inline.} = cmpStrings(a, b) < 0

# ---- newString / newStringOfCap ----

func newString*(len: int): string =
  result = string(bytes: 0'u, more: nil)
  if len <= 0: return
  if len <= PayloadSize:
    setSSLen(result, len)
    zeroMem(inlinePtrV(result), len + 1)
  else:
    let p = cast[ptr LongString](alloc(LongStringDataOffset + len + 1))
    if p != nil:
      zeroMem(p, LongStringDataOffset + len + 1)
      p.rc = 1
      p.fullLen = len
      p.capImpl = len
      result.more = p
      setSSLen(result, HeapSlen)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + len + 1

func newStringOfCap*(len: int): string =
  ## Returns a new empty string with capacity reserved for `len` chars.
  result = string(bytes: 0'u, more: nil)
  if len <= PayloadSize: return  # inline capacity always available
  let p = cast[ptr LongString](alloc(LongStringDataOffset + len + 1))
  if p != nil:
    p.rc = 1
    p.fullLen = 0
    p.capImpl = len
    p.data[0] = '\0'
    result.more = p
    setSSLen(result, HeapSlen)
  else:
    {.cast(noSideEffect).}: oomHandler LongStringDataOffset + len + 1

# ---- concat / & ----

template concat*(): string {.varargs.} =
  var res = ""
  for s in unpack():
    res.add s
  res

func `&`*(a, b: string): string {.semantics: "string.&".} =
  result = string(bytes: 0'u, more: nil)
  let rlen = a.len + b.len
  if rlen == 0: return
  if rlen <= PayloadSize:
    let al = a.len
    setSSLen(result, rlen)
    if al > 0: copyMem(inlinePtrV(result), rawData(a), al)
    if b.len > 0: copyMem(inlinePtrAt(result, al), rawData(b), b.len)
    inlinePtrV(result)[rlen] = '\0'
  else:
    let p = cast[ptr LongString](alloc(LongStringDataOffset + rlen + 1))
    if p != nil:
      p.rc = 1
      p.fullLen = rlen
      p.capImpl = rlen
      let al = a.len
      if al > 0: copyMem(addr p.data[0], rawData(a), al)
      if b.len > 0:
        copyMem(cast[pointer](cast[uint](addr p.data[0]) + uint(al)), rawData(b), b.len)
      p.data[rlen] = '\0'
      result.more = p
      setSSLen(result, HeapSlen)
      copyMem(inlinePtrV(result), addr p.data[0], AlwaysAvail)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + rlen + 1

func charToString(c: char): string =
  result = string(bytes: 0'u, more: nil)
  setSSLen(result, 1)
  inlinePtrV(result)[0] = c
  inlinePtrV(result)[1] = '\0'

func `&`*(x: string; y: char): string {.inline.} = result = x & charToString(y)
func `&`*(x: char; y: string): string {.inline.} = result = charToString(x) & y

func terminatingZero*(s: string): string =
  result = s & "\0"
  result.shrink s.len

# ---- cstring conversions ----

func borrowCStringUnsafe(s: cstring; l: int): string =
  ## Creates a Nim string from a cstring by copying up to `l` chars.
  result = string(bytes: 0'u, more: nil)
  if l <= 0: return
  if l <= PayloadSize:
    setSSLen(result, l)
    copyMem(inlinePtrV(result), s, l)
    inlinePtrV(result)[l] = '\0'
  else:
    let p = cast[ptr LongString](alloc(LongStringDataOffset + l + 1))
    if p != nil:
      p.rc = 1
      p.fullLen = l
      p.capImpl = l
      copyMem(addr p.data[0], s, l + 1)
      result.more = p
      setSSLen(result, HeapSlen)
      copyMem(inlinePtrV(result), addr p.data[0], AlwaysAvail)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + l + 1

func borrowCStringUnsafe*(s: cstring): string {.exportc: "nimBorrowCStringUnsafe".} =
  borrowCStringUnsafe(s, len(s))

func ensureTerminatingZero*(s: var string) =
  ## SSO strings are always null-terminated. This is a no-op.
  discard

func toCString*(s: var string): cstring =
  ## Returns a null-terminated cstring pointer.
  ## SSO strings are always null-terminated, so this is zero-cost.
  result = cast[cstring](rawData(s))

func fromCString*(s: cstring): string =
  ## Creates a Nim string from a `cstring` by copying the underlying storage.
  result = string(bytes: 0'u, more: nil)
  let l = len(s)
  if l == 0: return
  if l <= PayloadSize:
    setSSLen(result, l)
    copyMem(inlinePtrV(result), s, l)
    inlinePtrV(result)[l] = '\0'
  else:
    let p = cast[ptr LongString](alloc(LongStringDataOffset + l + 1))
    if p != nil:
      p.rc = 1
      p.fullLen = l
      p.capImpl = l
      copyMem(addr p.data[0], s, l + 1)
      result.more = p
      setSSLen(result, HeapSlen)
      copyMem(inlinePtrV(result), addr p.data[0], AlwaysAvail)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + l + 1

template `$`*(x: string): string = x

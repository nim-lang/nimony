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

# ---- low-level byte accessors ----

template ssLenOf(bytes: uint): int =
  ## Extracts slen from an already-loaded `bytes` word. Pure register op (no memory access).
  ## Use when `bytes` is already in a register (e.g. loaded for SWAR comparison).
  ## Assumes little-endian: slen is the low byte of `bytes`.
  int(bytes and 0xFF'u)

template ssLen(s: string): int =
  ## Reads slen via a byte load at offset 0. Keeps slen access visibly distinct
  ## from char writes at offsets 1+ so the C compiler can cache slen across loops.
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
  if ssLen(s) == HeapSlen:
    if arcDec(s.more.rc):
      dealloc(s.more)

func `=copy`*(dest: var string; src: string) {.exportc: "nimStrCopy", inline, nodestroy.} =
  let ssrc = ssLen(src)
  if ssrc <= PayloadSize:
    # short/medium: destroy dest heap block if any, then bitcopy
    let sdest = ssLen(dest)
    if sdest == HeapSlen:
      if arcDec(dest.more.rc):
        dealloc(dest.more)
    copyMem(addr dest.bytes, addr src.bytes, sizeof(string))
  else:
    # long or static: COW share
    if addr(dest) == addr(src): return
    let sdest = ssLen(dest)
    if sdest == HeapSlen:
      if arcDec(dest.more.rc):
        dealloc(dest.more)
    if ssrc == HeapSlen:
      arcInc(src.more.rc)
    copyMem(addr dest.bytes, addr src.bytes, sizeof(string))

func `=dup`*(s: string): string {.exportc: "nimStrDup", inline, nodestroy.} =
  if ssLen(s) == HeapSlen:
    arcInc(s.more.rc)
  result = string(bytes: s.bytes, more: s.more)

# ---- cstring length ----

func strlen(a: cstring): csize_t {.importc: "strlen", header: "<string.h>".}

func len*(a: cstring): int {.inline.} =
  if a == nil: 0
  else: a.strlen.int

# ---- cstring view ----

func nimStrToCString*(s {.byref.}: string): cstring {.inline, exportc: "nimStrToCString".} =
  ## Returns a null-terminated cstring. For heap strings, writes the null
  ## terminator on demand (opt-in). Static/literal strings already have one.
  let sl = ssLen(s)
  if sl > PayloadSize:
    if sl == HeapSlen:
      let p = s.more
      # XXX This is wrong!
      p.data[p.fullLen] = '\0'
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
    # Sync inline cache even on fast path (use oldLen since new data may not exist yet)
    copyMem(inlinePtrV(s), addr s.more.data[0], min(oldLen, AlwaysAvail))
  else:
    let newCap = if newLen > cap: max(newLen, ssResize(cap)) else: cap
    let p = cast[ptr LongString](alloc(LongStringDataOffset + newCap + 1))
    if p != nil:
      p.rc = 1
      p.fullLen = newLen
      p.capImpl = newCap
      let old = s.more
      copyMem(addr p.data[0], addr old.data[0], min(oldLen, newCap))
      if isHeap and arcDec(old.rc):
        dealloc(old)
      s.more = p
      setSSLen(s, HeapSlen)
      # Sync inline cache after creating new block (use oldLen since new data may not exist yet)
      copyMem(inlinePtrV(s), addr p.data[0], min(oldLen, AlwaysAvail))
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
    # Sync inline cache after creating new block
    copyMem(inlinePtrV(s), addr p.data[0], min(sl, AlwaysAvail))
  else:
    {.cast(noSideEffect).}: oomHandler LongStringDataOffset + newCap + 1
    s.bytes = 0

# ---- mutation helpers ----

func prepareMutation*(s: var string) {.inline.} =
  ## Ensures s's data is uniquely owned (not shared with another string or static).
  let sl = ssLen(s)
  if sl == StaticSlen or (sl == HeapSlen and s.more.rc > 1):
    if sl == HeapSlen:
      discard arcDec(s.more.rc)
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
    setSSLen(s, newLen)
  elif sl > PayloadSize:
    let l = s.more.fullLen
    if sl == HeapSlen and s.more.rc == 1 and l < s.more.capImpl:
      s.more.data[l] = c
      s.more.fullLen = l + 1
      if l < AlwaysAvail:
        inlinePtrV(s)[l] = c
    else:
      let oldLen = s.more.fullLen
      ensureUniqueLong(s, oldLen, oldLen + 1)
      if ssLen(s) == HeapSlen:
        s.more.data[oldLen] = c
        if oldLen < AlwaysAvail:
          inlinePtrV(s)[oldLen] = c
  else:
    # sl == PayloadSize → transition to long
    transitionToLong(s, sl, sl + 1)
    if ssLen(s) == HeapSlen:
      s.more.data[sl] = c

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
      setSSLen(s, newLen)
    else:
      transitionToLong(s, sLen, newLen)
      if ssLen(s) == HeapSlen:
        copyMem(cast[pointer](cast[uint](addr s.more.data[0]) + uint(sLen)), partData, partLen)
        copyMem(inlinePtrV(s), addr s.more.data[0], AlwaysAvail)
  else:
    let sLen = s.more.fullLen
    let newLen = sLen + partLen
    ensureUniqueLong(s, sLen, newLen)
    if ssLen(s) == HeapSlen:
      copyMem(cast[pointer](cast[uint](addr s.more.data[0]) + uint(sLen)), partData, partLen)
      if sLen < AlwaysAvail:
        copyMem(inlinePtrV(s), addr s.more.data[0], AlwaysAvail)

# ---- zero-padding helper for inline shrink ----

template zeroSwarPad(s: var string; newLen: int) =
  ## Clears stale char bytes above newLen in `bytes`, sets slen = newLen.
  ## Required for SWAR correctness: bytes above slen must be 0.
  let keepBits = uint((newLen + 1) * 8)
  let mask = if keepBits >= uint(sizeof(uint) * 8): not 0'u
             else: (uint(1) shl keepBits) - 1'u
  s.bytes = (s.bytes and (mask and not 0xFF'u)) or uint(newLen)

# ---- setLen / shrink ----

func shrink*(s: var string; newLen: int) =
  if newLen <= s.len:
    let sl = ssLen(s)
    if sl <= PayloadSize:
      if newLen <= AlwaysAvail:
        zeroSwarPad(s, newLen)  # clear stale bytes for SWAR; sets slen
      else:
        setSSLen(s, newLen)
    else:
      if newLen <= PayloadSize:
        # Transition from long/static to inline
        let old = s.more
        if newLen > 0:
          copyMem(inlinePtrV(s), addr old.data[0], newLen)
        if newLen <= AlwaysAvail:
          zeroSwarPad(s, newLen)
        else:
          setSSLen(s, newLen)
      else:
        # Stays long: ensure unique before mutating
        prepareMutation(s)
        s.more.fullLen = newLen
        copyMem(inlinePtrV(s), addr s.more.data[0], min(newLen, AlwaysAvail))

func setLen*(s: var string; newLen: int) =
  if newLen <= s.len:
    shrink(s, newLen)
    return
  let sl = ssLen(s)
  if sl <= PayloadSize:
    let curLen = s.len
    if newLen <= PayloadSize:
      let inl = inlinePtrV(s)
      if newLen > curLen:
        zeroMem(inlinePtrAt(s, curLen), newLen - curLen)
        setSSLen(s, newLen)
      else:
        if newLen <= AlwaysAvail:
          zeroSwarPad(s, newLen)  # clears stale chars for SWAR; also sets slen
        else:
          setSSLen(s, newLen)
    else:
      transitionToLong(s, curLen, newLen)
      if ssLen(s) == HeapSlen:
        zeroMem(cast[pointer](cast[uint](addr s.more.data[0]) + uint(curLen)), newLen - curLen)
  else:
    if newLen <= PayloadSize:
      let old = s.more
      copyMem(inlinePtrV(s), addr old.data[0], newLen)
      if newLen <= AlwaysAvail:
        zeroSwarPad(s, newLen)  # clear stale prefix bytes for SWAR; sets slen
      else:
        setSSLen(s, newLen)
    else:
      let curLen = s.len
      ensureUniqueLong(s, curLen, newLen)
      if ssLen(s) == HeapSlen:
        if newLen > curLen:
          zeroMem(cast[pointer](cast[uint](addr s.more.data[0]) + uint(curLen)), newLen - curLen)
        copyMem(inlinePtrV(s), addr s.more.data[0], AlwaysAvail)

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
  else:
    let p = cast[ptr LongString](alloc(LongStringDataOffset + newLen + 1))
    if p != nil:
      p.rc = 1
      p.fullLen = newLen
      p.capImpl = newLen
      copyMem(addr p.data[0], cast[pointer](cast[uint](src) + uint(f)), newLen)
      result.more = p
      setSSLen(result, HeapSlen)
      copyMem(inlinePtrV(result), addr p.data[0], AlwaysAvail)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + newLen + 1
      result.bytes = 0

func substr*(s: string; first = 0): string =
  result = substr(s, first, high(s))

# ---- SWAR / comparison helpers ----
# Assumes little-endian 64-bit (the common case; BE support can be added later).

func bswap(x: uint): uint {.importc: "__builtin_bswap64", nodecl, noSideEffect.}

func ctzImpl(x: uint): int {.inline.} =
  func ctz64(x: uint64): int32 {.importc: "__builtin_ctzll", nodecl, noSideEffect.}
  int(ctz64(uint64(x)))

func swarKey(x: uint): uint {.inline.} =
  ## On LE: shift out slen byte, bswap → char[0] lands in MSB.
  ## Integer compare on swarKey results = lexicographic char order.
  result = bswap(x shr 8)

template inlinePtrOf(p: ptr string): ptr UncheckedArray[char] =
  ## Inline char pointer from a ptr string — avoids unsafeAddr dance.
  cast[ptr UncheckedArray[char]](cast[uint](p) + 1'u)

template tailPtrOf(p: ptr string): ptr UncheckedArray[char] =
  ## Pointer to the medium-string tail bytes (chars AlwaysAvail..PayloadSize-1)
  ## which live in the `more` field, at offset AlwaysAvail+1 from struct start.
  cast[ptr UncheckedArray[char]](cast[uint](p) + 1'u + uint(AlwaysAvail))

func cmpInlineBytes(a, b: ptr UncheckedArray[char]; n: int): int {.inline.} =
  result = 0
  for i in 0..<n:
    let ac = a[i]; let bc = b[i]
    if ac < bc: return -1
    if ac > bc: return 1

func cmpShortInline(abytes, bbytes: uint; aslen, bslen: int): int {.inline.} =
  ## Compare two short strings (both slen <= AlwaysAvail) using CTZ to find
  ## the first differing byte without a loop.
  let minLen = min(aslen, bslen)
  if minLen > 0:
    # LE: char bytes are at bits 8..63. Mask to minLen chars, find lowest diff.
    let diffMask = (1'u shl (minLen * 8)) - 1'u
    let diff = ((abytes xor bbytes) shr 8) and diffMask
    if diff != 0:
      let byteShift = (ctzImpl(diff) shr 3) * 8 + 8
      let ac = (abytes shr byteShift) and 0xFF'u
      let bc = (bbytes shr byteShift) and 0xFF'u
      if ac < bc: return -1
      return 1
  aslen - bslen

func cmpStringPtrs(a, b: ptr string): int {.inline.} =
  ## Compare via pointers to avoid struct copies in the hot path.
  let abytes = a.bytes
  let bbytes = b.bytes
  let aslen = ssLenOf(abytes)
  let bslen = ssLenOf(bbytes)
  if aslen <= AlwaysAvail and bslen <= AlwaysAvail:
    return cmpShortInline(abytes, bbytes, aslen, bslen)
  if aslen <= PayloadSize and bslen <= PayloadSize:
    # Both medium: all data is inline, no heap access needed.
    let minLen = min(aslen, bslen)
    let pfxLen = min(minLen, AlwaysAvail)
    result = cmpInlineBytes(inlinePtrOf(a), inlinePtrOf(b), pfxLen)
    if result != 0: return
    if minLen > AlwaysAvail:
      result = cmpInlineBytes(tailPtrOf(a), tailPtrOf(b), minLen - AlwaysAvail)
    if result == 0: result = aslen - bslen
    return
  # At least one long. Hot prefix (bytes 1..AlwaysAvail) mirrors heap data.
  let pfxLen = min(min(aslen, bslen), AlwaysAvail)
  result = cmpInlineBytes(inlinePtrOf(a), inlinePtrOf(b), pfxLen)
  if result != 0: return
  let la = if aslen > PayloadSize: a.more.fullLen else: aslen
  let lb = if bslen > PayloadSize: b.more.fullLen else: bslen
  let minLen = min(la, lb)
  if minLen <= AlwaysAvail:
    result = la - lb
    return
  # Skip the AlwaysAvail prefix (already compared via inline cache above).
  let ap = if aslen > PayloadSize: cast[ptr UncheckedArray[char]](cast[uint](addr a.more.data[0]) + uint(AlwaysAvail))
           else: tailPtrOf(a)
  let bp = if bslen > PayloadSize: cast[ptr UncheckedArray[char]](cast[uint](addr b.more.data[0]) + uint(AlwaysAvail))
           else: tailPtrOf(b)
  result = cmpMem(ap, bp, minLen - AlwaysAvail)
  if result == 0: result = la - lb

# ---- comparison ----

func equalStrings(a, b: string): bool {.inline.} =
  let abytes = a.bytes
  let bbytes = b.bytes
  let aslen = ssLenOf(abytes)
  let bslen = ssLenOf(bbytes)
  if aslen <= AlwaysAvail and bslen <= AlwaysAvail:
    return abytes == bbytes  # SWAR: one word covers slen + all chars
  let la = if aslen > PayloadSize: a.more.fullLen else: aslen
  let lb = if bslen > PayloadSize: b.more.fullLen else: bslen
  if la != lb: return false
  if la == 0: return true
  if aslen <= PayloadSize and bslen <= PayloadSize:
    # Both medium: compare bytes word first (slen + chars 0-6 in one op),
    # then the tail stored in the `more` overlay.
    if abytes != bbytes: return false
    return cmpMem(tailPtrOf(unsafeAddr a), tailPtrOf(unsafeAddr b),
                  la - AlwaysAvail) == 0
  # At least one long: delegate to cmpStringPtrs
  cmpStringPtrs(unsafeAddr a, unsafeAddr b) == 0

func `==`*(a, b: string): bool {.inline, semantics: "string.==".} =
  equalStrings(a, b)

func nimStrAtLe(s: string; idx: int; ch: char): bool {.inline.} =
  result = idx < s.len and s[idx] <= ch

func cmp*(a, b: string): int {.inline.} =
  ## Specialized comparison for strings.
  let abytes = a.bytes
  let bbytes = b.bytes
  let aslen = ssLenOf(abytes)
  let bslen = ssLenOf(bbytes)
  if aslen <= AlwaysAvail and bslen <= AlwaysAvail:
    return cmpShortInline(abytes, bbytes, aslen, bslen)
  cmpStringPtrs(unsafeAddr a, unsafeAddr b)

func `<=`*(a, b: string): bool {.inline.} = cmp(a, b) <= 0
func `<`*(a, b: string): bool {.inline.} = cmp(a, b) < 0

# ---- startsWith ----

func startsWithImpl*(s, prefix: string): bool {.inline.} =
  ## Returns true if `s` begins with `prefix`.
  ##
  ## Always opens with a register-only masked comparison of the first
  ## min(prefix.len, AlwaysAvail) chars — no branch needed to decide whether
  ## the prefix fits in registers, because by SWAR design the `bytes` word
  ## always carries valid (zero-padded) chars for all three tiers.
  let pbytes = prefix.bytes
  let pslen = ssLenOf(pbytes)
  let sbytes = s.bytes
  let sslen = ssLenOf(sbytes)

  # Mask to min(pslen, AlwaysAvail) char-bytes (bits 8..8+n*8-1 of `bytes`).
  # charBits ≤ 56 so the shift never overflows a 64-bit uint.
  let charBits = min(pslen, AlwaysAvail) * 8
  let charMask = ((1'u shl charBits) - 1'u) shl 8
  if (sbytes and charMask) != (pbytes and charMask): return false

  # Resolve actual lengths (register for short/medium; one load for long).
  let sLen = if sslen > PayloadSize: s.more.fullLen else: sslen
  let pLen = if pslen > PayloadSize: prefix.more.fullLen else: pslen
  if sLen < pLen: return false
  if pLen <= AlwaysAvail: return true  # short prefix fully covered above

  # Compare tail bytes (chars AlwaysAvail..pLen-1).
  let sTail =
    if sslen > PayloadSize:
      cast[ptr UncheckedArray[char]](cast[uint](addr s.more.data[0]) + uint(AlwaysAvail))
    else:
      tailPtrOf(unsafeAddr s)
  let pTail =
    if pslen > PayloadSize:
      cast[ptr UncheckedArray[char]](cast[uint](addr prefix.more.data[0]) + uint(AlwaysAvail))
    else:
      tailPtrOf(unsafeAddr prefix)
  cmpMem(sTail, pTail, pLen - AlwaysAvail) == 0

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
    zeroMem(p, LongStringDataOffset + len + 1)
    p.rc = 1
    p.fullLen = 0
    p.capImpl = len
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
      result.more = p
      setSSLen(result, HeapSlen)
      copyMem(inlinePtrV(result), addr p.data[0], AlwaysAvail)
    else:
      {.cast(noSideEffect).}: oomHandler LongStringDataOffset + rlen + 1

func charToString(c: char): string =
  result = string(bytes: 0'u, more: nil)
  setSSLen(result, 1)
  inlinePtrV(result)[0] = c

func `&`*(x: string; y: char): string {.inline.} = result = x & charToString(y)
func `&`*(x: char; y: string): string {.inline.} = result = charToString(x) & y

func terminatingZero*(s: string): string =
  result = s & "\0"
  result.shrink s.len

# ---- cstring conversions ----

func borrowCStringUnsafe*(s: cstring; l: int): string =
  ## Creates a Nim string from a cstring by copying up to `l` chars.
  result = string(bytes: 0'u, more: nil)
  if l <= 0: return
  if l <= PayloadSize:
    setSSLen(result, l)
    copyMem(inlinePtrV(result), s, l)
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
  ## Ensures the string ends with a null terminator for C compatibility.
  ## Inline/medium strings always maintain a zero; heap strings do not unless
  ## this (or toCString) is called explicitly.
  ## Static strings already have a terminator from their C string literal.
  if s.len == 0 or s[s.len-1] != '\0':
    let oldLen = s.len
    setLen(s, oldLen + 1)
    s[oldLen] = '\0'

func toCString*(s: var string): cstring =
  ## Returns a null-terminated cstring pointer.
  ensureTerminatingZero(s)
  result = cast[cstring](rawData(s))

func fromCString*(s: cstring): string =
  ## Creates a Nim string from a `cstring` by copying the underlying storage.
  result = string(bytes: 0'u, more: nil)
  let l = len(s)
  if l == 0: return
  if l <= PayloadSize:
    setSSLen(result, l)
    copyMem(inlinePtrV(result), s, l)
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

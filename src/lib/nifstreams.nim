## nifstreams — the classic NIF streaming surface, kept ONLY for external
## consumers: the Nim compiler's IC modules (ast2nif / deps / modulegraphs /
## pipelines under `dist/nimony`) import this module and must keep compiling
## unchanged. Nimony's own code imports nifpools (via nifprelude) and must
## never import this file.
##
## Everything here is an honest adapter, not a fake:
## * Floats get a REAL interning pool: `pool.floats.getOrIncl` returns a
##   `FloatId` index, `floatToken` packs it into a genuine `FloatLit` NifToken
##   (transit-only: it must never enter a TokenBuf, whose float encoding is
##   inline multi-token), and `pool.floats[t.floatId]` decodes it — lossless.
## * `Stream`/`next` wrap the textual nifreader; the unified NifKind has real
##   `ParLe`/`ParRi`/`EofToken` members, so structural scanners (deps.nim)
##   see the exact classic kinds. Ident/StringLit/Symbol payloads are interned
##   into the global `pool`, so `pool.strings[t.litId]` works as before.
##   Number tokens keep their KIND only (a 4-byte token cannot always carry
##   the value); classic scanners never read those payloads.

import std / tables
import nifpools
export nifpools

from nifreader import Reader, ExpandedToken, decodeStr

# ── Classic interned float literals (ast2nif) ────────────────────────────

type
  FloatId* = distinct uint32   ## 1-based index into the global float pool
  FloatPool* = object
    values: seq[float64]
    lookup: Table[uint64, uint32]   # bit pattern -> 1-based id

func `==`*(a, b: FloatId): bool {.borrow.}

var globalFloats*: FloatPool

template floats*(p: Pool): var FloatPool = globalFloats

proc getOrIncl*(fp: var FloatPool; v: float64): FloatId =
  let bits = cast[uint64](v)
  let existing = fp.lookup.getOrDefault(bits, 0'u32)
  if existing != 0'u32:
    result = FloatId(existing)
  else:
    fp.values.add v
    let id = uint32(fp.values.len)
    fp.lookup[bits] = id
    result = FloatId(id)

proc `[]`*(fp: FloatPool; id: FloatId): float64 {.inline.} =
  fp.values[int(uint32(id)) - 1]

proc floatToken*(id: FloatId; info: PackedLineInfo): NifToken {.inline.} =
  ## Transit-only token: carries the pool index so the receiver can decode it
  ## via `pool.floats[t.floatId]`. It must never be appended to a TokenBuf
  ## (nifcore stores floats inline as a multi-token encoding); the line info
  ## is dropped like in the other classic token constructors.
  NifToken((uint32(id) shl KindBits) or uint32(FloatLit))

proc floatId*(n: NifToken): FloatId {.inline.} = FloatId(uoperand(n))

# ── Classic streaming text reader (deps.nim) ─────────────────────────────

type
  Stream* = object
    r*: Reader

proc open*(filename: string): Stream =
  Stream(r: nifreader.open(filename))

proc close*(s: var Stream) =
  nifreader.close(s.r)

proc next*(s: var Stream): NifToken =
  ## One classic packed token per call. Pool-referencing kinds are interned
  ## into the global `pool`/`globalTags`, so `.litId`/`.tagId` accessors and
  ## `pool.strings[...]`/`pool.tags[...]` lookups behave exactly as classic
  ## nifstreams did. Kinds without a pool payload come back kind-only.
  var t = default(ExpandedToken)
  nifreader.next(s.r, t)
  case t.tk
  of ParLe:
    result = tagLitToken(registerTag(globalTags, decodeStr(s.r, t)))
  of Ident:
    result = identToken(pool.strings.getOrIncl(decodeStr(s.r, t)))
  of StrLit:
    result = strLitToken(pool.strings.getOrIncl(decodeStr(s.r, t)))
  of Symbol:
    result = symToken(pool.syms.getOrIncl(decodeStr(s.r, t)))
  of SymbolDef:
    result = symdefToken(pool.syms.getOrIncl(decodeStr(s.r, t)))
  else:
    # ParRi/EofToken/DotToken/CharLit/numbers: correct kind, no payload.
    result = NifToken(uint32(t.tk))

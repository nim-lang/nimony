# (c) 2026
#
# A DNS client and a minimal authoritative server on the same ring as the
# sockets they sit on. `getaddrinfo` blocks and io_uring has no resolver, and
# libc's resolver cannot be given a `Deadline` or be cancelled — a pool thread
# parked inside it stays parked. This is the version that carries a `Deadline`:
# one datagram out, one back, bounded by the caller's budget (`doc/internals/
# http.md` item 6).
#
# Client (`Resolver`): reads `/etc/hosts` and `/etc/resolv.conf` at
# construction, then `resolve` answers from `/etc/hosts` when it can and
# otherwise asks the first nameserver over UDP. Only IPv4 is answered today;
# the wire format understands CNAME chains and NXDOMAIN, so a name that needs
# chasing or does not exist is reported, not mistranslated.
#
# Server (`DnsServer`): a name table answered over `recvFrom`/`sendTo` — the
# unconnected socket procs `std/socket` exists for — so a client that was never
# met is answered from the address its datagram came from. This is not a
# forwarder and not a cache; it is the "here is what this box is called"
# table for a machine private enough not to need one, and the hermetic test
# bed the resolver's own behaviour is checked against.

import std/socket
import std/syncio
import std/strtabs
import std/assertions

from std/posix/posix import Sockaddr_storage

const
  MaxPtrLabels = 64
    ## A poisoned compression chain must stop; the bound is generous because a
    ## real response is around a dozen pointers.
  # DNS record types worth naming. Decoding handles A and the name-valued
  # kinds; everything else is still skipped by length, so a response that does
  # not concern us is not read past its rdata.
  RTypeA* = 1'u16
  RTypeNs* = 2'u16
  RTypeCname* = 5'u16
  RTypePtr* = 12'u16

# ------------------------------------------------------------ wire format ---
#
# RFC 1035, the part this module needs: a 12-byte header, length-prefixed
# QNAMEs, and name *compression* — a component whose top two bits are `11` is
# a 14-bit offset back into the message, which is how a response names the
# owner of an answer for (almost) free. Every multi-byte integer on the wire
# is big-endian; the helpers below read and write it as raw bytes so no
# host-endianness switch exists to get wrong.

proc uw16(a, b: char): uint16 {.inline.} =
  (uint16(ord(a)) shl 8) or uint16(ord(b))

proc uw32(a, b, c, d: char): uint32 {.inline.} =
  (uint32(ord(a)) shl 24) or (uint32(ord(b)) shl 16) or
  (uint32(ord(c)) shl 8) or uint32(ord(d))

proc put16(s: var seq[char]; v: uint16) =
  s.add char((v shr 8) and 0xFF)
  s.add char(v and 0xFF)

proc put32(s: var seq[char]; v: uint32) =
  s.add char((v shr 24) and 0xFF)
  s.add char((v shr 16) and 0xFF)
  s.add char((v shr 8) and 0xFF)
  s.add char(v and 0xFF)

proc canon(name: string): string =
  ## Lowercase and drop any trailing dots: the wire treats DNS names as
  ## case-insensitive and a root-relative spelling as equal to an absolute one,
  ## so the table and the queries must agree on one spelling.
  var endPos = name.len
  while endPos > 0 and name[endPos - 1] == '.':
    dec endPos
  result = ""
  for i in 0 ..< endPos:
    let c = name[i]
    result.add(if c in {'A'..'Z'}: char(ord(c) + 32) else: c)

type
  Question* = object
    name*: string          ## dotted, canonical-cased
    rtype*: uint16
    rclass*: uint16

  Answer* = object
    name*: string          ## owner name, dotted and canonical-cased
    rtype*: uint16
    ttl*: uint32
    ip4*: string           ## rtype == RTypeA: the address as dotted quad
    cname*: string         ## rtype == CNAME/PTR: the target, dotted

  Message* = object
    id*: uint16
    flags*: uint16
    questions*: seq[Question]
    answers*: seq[Answer]

proc encodeName(s: var seq[char]; name: string) =
  ## One dotted name as a run of length-prefixed labels, ending in a zero
  ## byte (the root). Queries never compress, so the encode side has nothing
  ## to index: each label carries its own length.
  var start = 0
  while start < name.len:
    var endPos = start
    while endPos < name.len and name[endPos] != '.':
      inc endPos
    let labelLen = endPos - start
    assert labelLen <= 63, "dns: label longer than 63 bytes"
    s.add char(labelLen)
    for i in start ..< endPos:
      s.add name[i]
    start = endPos + 1
  s.add '\0'

proc encodeMessage*(m: Message; s: var seq[char]) =
  ## Encode `m` for the wire, ready for a datagram. A response echoes its
  ## question verbatim and appends answers; the caller decides which of the
  ## two that is entirely by what lives in `questions`/`answers`.
  s = @[]
  s.put16 m.id
  s.put16 m.flags
  s.put16 uint16(m.questions.len)
  s.put16 uint16(m.answers.len)
  s.put16 0   # NSCOUNT
  s.put16 0   # ARCOUNT
  for q in m.questions:
    encodeName(s, q.name)
    s.put16 q.rtype
    s.put16 q.rclass
  for a in m.answers:
    encodeName(s, a.name)
    s.put16 a.rtype
    s.put16 1          # class IN
    s.put32 a.ttl
    case a.rtype
    of RTypeA:
      # four raw octets built from the caller's dotted quad
      var octet = 0
      var dot = 0
      while dot < a.ip4.len:
        let c = a.ip4[dot]
        if c == '.':
          s.add char(octet)
          octet = 0
        elif c in {'0'..'9'}:
          octet = octet * 10 + (ord(c) - ord('0'))
        dot += 1
      s.add char(octet)
    of RTypeCname, RTypePtr:
      var tmp: seq[char]
      tmp.encodeName(a.cname)
      s.put16 uint16(tmp.len)
      for c in tmp: s.add c
    else:
      s.put16 0          # unknown kinds answer nothing

proc decodeName(buf: openArray[char]; pos: var int; name: var string): bool =
  ## One (possibly compressed) name from `buf`, lowercased into `name`.
  ## `pos` ends past the name in the *original* position — two bytes for a
  ## pointer, the full run for a literal run — so the caller can continue past
  ## what it asked for even though the decode itself jumped around the message.
  var p = pos
  var jumped = false
  var hops = 0
  result = true
  while true:
    if p >= buf.len: return false
    let l = ord(buf[p])
    if l == 0:
      if not jumped: pos = p + 1
      return true
    if (l and 0xC0) == 0xC0:
      if p + 1 >= buf.len: return false
      let off = ((l and 0x3F) shl 8) or ord(buf[p + 1])
      if not jumped: pos = p + 2
      jumped = true
      if off >= buf.len or off == p: return false
      inc hops
      if hops > MaxPtrLabels: return false
      p = off
    elif (l and 0xC0) != 0:
      return false      # label type 10/01 does not exist
    else:
      if l > 63 or p + l + 1 > buf.len: return false
      if name.len > 0: name.add '.'
      for i in 1 .. l:
        let c = buf[p + i]
        name.add(if c in {'A'..'Z'}: char(ord(c) + 32) else: c)
      p += l + 1

proc decodeMessage*(buf: openArray[char]; m: var Message): bool =
  ## Parse `buf` into `m`, following compression in every name. `false` for
  ## anything a resolver cannot trust; authority/additional sections are
  ## skipped by their declared lengths, not parsed.
  if buf.len < 12: return false
  m.id = uw16(buf[0], buf[1])
  m.flags = uw16(buf[2], buf[3])
  let qd = int(uw16(buf[4], buf[5]))
  let an = int(uw16(buf[6], buf[7]))
  let ns = int(uw16(buf[8], buf[9]))
  let ar = int(uw16(buf[10], buf[11]))
  var pos = 12
  m.questions = @[]
  for i in 0 ..< qd:
    var q = Question(name: "")
    if not decodeName(buf, pos, q.name): return false
    if pos + 4 > buf.len: return false
    q.rtype = uw16(buf[pos], buf[pos + 1])
    q.rclass = uw16(buf[pos + 2], buf[pos + 3])
    pos += 4
    m.questions.add q
  m.answers = @[]
  var i = 0
  while i < an:
    var a = Answer(name: "")
    if not decodeName(buf, pos, a.name): return false
    if pos + 10 > buf.len: return false
    a.rtype = uw16(buf[pos], buf[pos + 1])
    discard uw16(buf[pos + 2], buf[pos + 3])   # class
    a.ttl = uw32(buf[pos + 4], buf[pos + 5], buf[pos + 6], buf[pos + 7])
    let rdlen = int(uw16(buf[pos + 8], buf[pos + 9]))
    pos += 10
    if pos + rdlen > buf.len: return false
    case a.rtype
    of RTypeA:
      if rdlen == 4:
        a.ip4 = ""
        a.ip4.addInt ord(buf[pos])
        for k in 1 .. 3:
          a.ip4.add '.'
          a.ip4.addInt ord(buf[pos + k])
    of RTypeCname, RTypePtr:
      var target = ""
      var q = pos
      if decodeName(buf, q, target) and q <= pos + rdlen:
        a.cname = target
    else:
      discard
    pos += rdlen
    m.answers.add a
    inc i
  # Authority/additional are not parsed; their names could compress anywhere in
  # the message, and skipping by length is exactly how a decoder stays inside
  # the datagram no matter what they contain.
  result = true
  discard ns
  discard ar

proc isDots4(s: string): bool =
  ## True for a well-formed dotted quad of decimal octets — the one shape
  ## `/etc/hosts`, `/etc/resolv.conf` and this module's own answers may hold.
  if s.len == 0: return false
  var seg = 0       # digits seen in the current octet
  var dots = 0
  var val = 0
  for i in 0 ..< s.len:
    let c = s[i]
    if c in {'0'..'9'}:
      val = val * 10 + (ord(c) - ord('0'))
      if val > 255: return false
      inc seg
      if seg > 3: return false
    elif c == '.':
      if seg == 0: return false
      inc dots
      if dots > 3: return false
      seg = 0
      val = 0
    else:
      return false
  result = dots == 3 and seg > 0

# ------------------------------------------------------------------ client ---

type
  Resolver* = object
    hosts: StringTableRef           ## canonical name -> dotted quad
    servers*: seq[string]           ## nameserver addresses, dotted quads

proc nextToken(s: string; i: var int): string =
  ## The next whitespace-delimited run of `s`, and the index past it. A line
  ## parser's only tokenizer; `#` starts a comment and is the caller's call.
  result = ""
  while i < s.len and (s[i] == ' ' or s[i] == '\t'):
    inc i
  let start = i
  while i < s.len and s[i] notin {' ', '\t', '\n', '\r'}:
    inc i
  for j in start ..< i:
    result.add s[j]

proc readHosts(t: StringTableRef) =
  try:
    let f = syncio.readFile("/etc/hosts")
    var i = 0
    while i < f.len:
      var j = i
      while j < f.len and f[j] != '\n':
        inc j
      var k = i
      var ip = nextToken(f, k)
      if ip.len > 0 and ip[0] != '#' and isDots4(ip):
        var rest = k
        while rest < j:
          var name = nextToken(f, rest)
          if name.len == 0 or name[0] == '#': break
          t[canon(name)] = ip
      i = j + 1
  except ErrorCode:
    discard                       # a machine without the file resolves upstream

proc readResolvConf(r: var Resolver) =
  r.servers = @[]
  try:
    let f = syncio.readFile("/etc/resolv.conf")
    var i = 0
    while i < f.len:
      var j = i
      while j < f.len and f[j] != '\n':
        inc j
      var k = i
      var kw = nextToken(f, k)
      if kw == "nameserver":
        var nsAddr = nextToken(f, k)
        if isDots4(nsAddr) and nsAddr[0] != '#':
          r.servers.add nsAddr
      i = j + 1
  except ErrorCode:
    discard

proc initResolver*(): Resolver =
  ## `std/dns`'s client: `/etc/hosts` wins over the resolver, which asks the
  ## first `nameserver` line of `/etc/resolv.conf`. Files that parse partially
  ## are used partially — a broken `/etc/hosts` does not take the resolver
  ## down with it.
  result.hosts = newStringTable(modeCaseInsensitive)
  readHosts(result.hosts)
  readResolvConf(result)

proc query*(r: var Resolver; host: string; serverIdx: int;
            dl: Deadline): Message {.passive, raises.} =
  ## One question to `r.servers[serverIdx]` and one datagram back, bounded by
  ## `dl`. Parsed but uninterpreted — the caller applies `resolve`'s rules.
  ## Raises `TimeoutError` when `dl` arrives first, `ValueError` if the reply
  ## is not a response to this question, `NameNotFound` for NXDOMAIN.
  var wire: seq[char]
  var q = Message(id: uint16(monoNow() and 0xFFFF), flags: 0x0100'u16)   # RD
  q.questions.add Question(name: canon(host), rtype: RTypeA, rclass: 1'u16)
  encodeMessage(q, wire)
  var sock = openUdp(0, dl)
  udpConnect(sock, r.servers[serverIdx], 53, budget(sock, dl))
  sendDatagram(sock, wire, budget(sock, dl))
  var respBuf = newSeq[char](512)
  let n = recvDatagram(sock, toOpenArray(respBuf, 0, respBuf.len - 1),
                       budget(sock, dl))
  var m = Message(id: 0'u16, flags: 0'u16)
  if not decodeMessage(toOpenArray(respBuf, 0, n - 1), m):
    raise ValueError
  if m.id != q.id or (m.flags and 0x8000) == 0:
    raise ValueError
  let rcode = m.flags and 0x000F
  if rcode != 0:
    raise NameNotFound          # NXDOMAIN and every other refusal
  result = m

proc resolveOne(r: var Resolver; hostIn: string; serverIdx: int;
                dl: Deadline): string {.passive, raises.} =
  ## `resolve`'s per-server half: ask one nameserver, walk the one response —
  ## CNAME owners rewrite the name being looked for until a name is chased
  ## that has no further CNAME — and return the first A that owns it.
  result = ""
  let host = canon(hostIn)
  let m = query(r, host, serverIdx, dl)
  var wanted = host
  for hop in 0 ..< m.answers.len:
    var chased = false
    for a in m.answers:
      if a.rtype == RTypeCname and canon(a.name) == wanted:
        wanted = canon(a.cname)
        chased = true
        break
    if not chased: break
  for a in m.answers:
    if a.rtype == RTypeA and canon(a.name) == wanted and a.ip4.len > 0:
      return a.ip4
  raise NameNotFound

proc resolve*(r: var Resolver; hostIn: string; dl = never): string {.passive, raises.} =
  ## The IPv4 dotted quad for `hostIn`: from `/etc/hosts` when it is listed
  ## there, else from the first nameserver that answers. Follows CNAME chains
  ## inside the one response; raises `ValueError` for a name the resolver
  ## cannot look up at all, `TimeoutError` when the budget runs out,
  ## `NameNotFound` when the name is not known to the resolver or the zone it
  ## asked.
  let host = canon(hostIn)
  if host.len == 0:
    raise ValueError
  if r.hosts != nil and r.hosts.hasKey(host):
    return r.hosts[host]
  if r.servers.len == 0:
    raise ValueError            # no nameserver: this machine resolves nothing
  for serverIdx in 0 ..< r.servers.len:
    try:
      return resolveOne(r, host, serverIdx, dl)
    except ErrorCode as e:
      if e == NameNotFound:
        discard                  # ask the next nameserver the same question
      else:
        raise e
  raise NameNotFound

# ------------------------------------------------------------------ server ---

type
  DnsServer* = object
    sock: UdpSocket
    names: StringTableRef       ## canonical name -> dotted quad

proc newDnsServer*(port: uint16; deadline = never): DnsServer =
  ## A bound, non-blocking UDP socket that answers A-queries from the table
  ## `add` fills. `port` of 0 asks the kernel to pick one (`boundPort`).
  result.sock = openUdp(port, deadline)
  result.names = newStringTable(modeCaseInsensitive)

proc add*(s: var DnsServer; name, ip: string) =
  ## Serve `name` from `ip` (a dotted quad). Questions are matched
  ## case-insensitively and without regard to a trailing dot.
  assert isDots4(ip), "dns: server addresses must be dotted quads"
  s.names[canon(name)] = ip

proc boundPort*(s: var DnsServer): uint16 {.inline.} = boundPort(s.sock)

proc serveOnce*(s: var DnsServer; dl = never) {.passive, raises.} =
  ## One request/response cycle: wait for a query (which can be from anyone —
  ## the socket is unconnected), answer A from the table, or NXDOMAIN when the
  ## name is not there. Send the reply back to the address the query came
  ## from. Raises `TimeoutError` when no query arrives before `dl`.
  var buf = default(array[512, char])
  var peer = PeerAddr(raw: Sockaddr_storage())
  let n = recvFrom(s.sock, buf, peer, dl)
  var q = Message(id: 0'u16, flags: 0'u16)
  if not decodeMessage(toOpenArray(buf, 0, n - 1), q):
    return                        # garbage datagrams are dropped, not answered
  if q.questions.len == 0:
    return
  let theQ = q.questions[0]
  var resp = Message(id: q.id, flags: 0x8180'u16)   # QR|AA|RD|RA
  resp.questions.add theQ
  let name = canon(theQ.name)
  if theQ.rtype == RTypeA and s.names.hasKey(name):
    resp.answers.add Answer(name: theQ.name, rtype: RTypeA, ttl: 60,
                            ip4: s.names[name])
  else:
    resp.flags = (resp.flags and 0xFFF0'u16) or 0x0003'u16   # NXDOMAIN
  var outWire: seq[char]
  encodeMessage(resp, outWire)
  sendTo(s.sock, outWire, peer, budget(s.sock, dl))
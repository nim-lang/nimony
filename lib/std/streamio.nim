# (c) 2026 Andreas Rumpf
#
# Streams over a descriptor, an in-memory buffer or a pipe: the buffered
# `Reader`/`Writer` pair and the constructors that build them. The connection
# constructors live here too, because they are what turns a resolved address
# into a stream, and that is where `std/dns` is used.
#
# The types and the buffered I/O themselves live in `std/private/coreio` so
# that `std/channels` can use them without dragging in `std/dns`: a channel
# is a record pipe, and a record pipe is core I/O, not name resolution.

import std/[ioring, socket, dns, strutils]

import std/private/coreio
export coreio

proc splitAddress(address: string; host: var string; port: var uint16) {.raises.} =
  let colon = address.rfind(':')
  if colon <= 0 or colon == address.len - 1: raise BadOperation
  host = address.substr(0, colon - 1)
  var p = 0
  for i in colon + 1 ..< address.len:
    let ch = address[i]
    if ch notin {'0'..'9'}: raise BadOperation
    p = p * 10 + (ord(ch) - ord('0'))
    if p > 65535: raise BadOperation
  port = uint16(p)

proc isIpLiteral(host: string): bool =
  result = host.len > 0
  for ch in host:
    if ch notin {'0'..'9', '.'}: return false

proc connectCore(address: string; tls: nil TlsEngine; dl: Deadline): Core {.
    passive, raises.} =
  var host = ""
  var port = 0'u16
  splitAddress(address, host, port)
  if dl <= monoNow(): raise TimeoutError
  let ip = if isIpLiteral(host): host else: resolve(host, dl)
  var s = socket.connect(ip, port, dl)
  let c = newCore(ckFd, s.fd)
  s.fd = -1                         # the core owns the descriptor now
  c.isSocket = true
  c.dl = dl
  if tls != nil:
    c.tls = tls
    tlsHandshake(c, tls, host)     # on failure the core closes the socket
  result = c

proc connect*(address: string; tls: nil TlsEngine = nil;
              dl = never): (Reader, Writer) {.
    passive, raises.} =
  ## A connection to `address` (`host:port`); TLS when an engine is given. For
  ## a protocol where the peer speaks first (SMTP, IMAP): the next step is
  ## reading its greeting. `dl` bounds DNS, TCP and TLS setup together; the
  ## returned streams have no standing deadline. Raises `TimeoutError` on expiry.
  let c = connectCore(address, tls, dl)
  c.dl = never
  result = (initReader(c), initWriter(c))

proc connect*(address: string; request: string; tls: nil TlsEngine = nil;
              dl = never): (Reader, Writer) {.
    passive, raises.} =
  ## For a protocol where we speak first (HTTP, Redis): `request` is written
  ## and FLUSHED before this returns, so the next step is reading the reply —
  ## the forgotten flush that hangs both sides cannot be written.
  ## One `dl` covers DNS, TCP, TLS and that flush; the returned streams start
  ## with `never`. A failed request write aborts rather than cleanly finishing.
  let c = connectCore(address, tls, dl)
  var w = initWriter(c)
  try:
    write(w, request)
    flush(w)
  except:
    w.abort()
    raise
  c.dl = never
  result = (initReader(c), w)

proc connectReader*(address: string; tls: nil TlsEngine = nil;
                    dl = never): Reader {.passive, raises.} =
  ## Only the receiving end, e.g. a feed. The sending direction is left alone
  ## (not shut down) and closes with the reader. `dl` bounds construction only;
  ## use `renew` to set a deadline for later I/O.
  let c = connectCore(address, tls, dl)
  c.dl = never
  result = initReader(c)

proc connectWriter*(address: string; tls: nil TlsEngine = nil;
                    dl = never): Writer {.passive, raises.} =
  ## Only the sending end, e.g. shipping logs. `dl` bounds construction only;
  ## use `renew` to set a deadline for later I/O.
  let c = connectCore(address, tls, dl)
  c.dl = never
  result = initWriter(c)

proc detachWriter*(w: var Writer) =
  ## Drops a writer WITHOUT ending its direction: no adapter trailer, no
  ## `close_notify`, no `shutdown`. For a protocol layer that has written
  ## its request and keeps only the reader (`std/httpclient`), where ending
  ## the direction would make many servers drop the connection.
  let c = w.core
  if c != nil:
    w.finished = true
    if c.ring != nil: endWriting(c)     # a pipe has no peer to confuse
    w.core = nil

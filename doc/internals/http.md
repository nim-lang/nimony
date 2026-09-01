# HTTP client and server — design

**Status: in progress.** Implemented: the message layer, which has no IO in it
at all (`httpmsg.nim` with its nifcore prerequisites, `httpparse.nim`,
`httpwire.nim`), the ioring work §7 gates on (deadlines, timers, connect), and
the connection layer `httpconn.nim` — which is where `.passive` meets the ring
and is proven end to end by `tests/nimony/http/tconn.nim`: a real socket, a
server and a client each a passive chain resumed by pool workers, keep-alive
across two requests, a dead peer, and an idle connection expiring on its own
budget. Still design: §2, the event loop itself.

The design rests on three ideas that already exist in this repo:

- **NIF's tag/payload split** (`src/lib/nifcore.nim`) for messages. HTTP verbs
  and header names are a vocabulary that is closed by the time it matters — the
  stdlib's known set plus whatever the application registers, fixed before the
  first connection — which is exactly what a tag space is for.
- **`.passive` procs** ([passive_procs.md](../passive_procs.md)) for the API. No
  callbacks, no futures, no coloring.
- **The relay pattern** (`ioring/core/backend.nim`, and uirelays' `InputRelays`)
  for backend injection — *below* the application, never in the API it sees.

Everything runs on `std/ioring`.


## 1. Messages are NIF trees

A request and a response are the same type: a `TokenBuf` holding one node.

```
(req (GET) "/index.html" (v11)
  (host           "example.com")
  (content-length 42)                                  ; IntLit — parsed once
  (accept         "text/html" "application/xhtml+xml") ; two children
  (connection     (keep-alive))                        ; value is itself a tag
  (x-trace-id     "abc123")                            ; registered by the app
  (xhdr           "X-Weird" "1"))                      ; registered by nobody
```

```
(res 200 (v11)
  (content-type   "text/html")
  (content-length 1234)
  (connection     (keep-alive)))
```

Method, target and version are the first three children by position; headers
follow in any order. There is no `(headers ...)` wrapper — node jumps make
skipping O(1) either way, and the wrapper would only cost a token.

### Tag space vs payload space

This distinction is the whole design, so it is worth stating plainly:

- The **tag space** is a single process-global `TagPool`, filled during init and
  sealed before the first connection is accepted. Known header names, known
  methods, known header *values* and the structural tags live here — and so do
  whatever custom headers the application registers.
- The **payload space** is a `Pool` per message, created with the message and
  destroyed with it.

`createTokenBuf(cap, sharedTags = gHttpTags)` gives exactly this: the tag
namespace is shared, the literal pool is not.

`createTags[E]` cannot be used directly, because it registers `$e` and
`content-length` is not a Nim identifier — a parallel
`const TagNames: array[HttpTag, string]` supplies the wire spellings, which
also makes a NIF dump of a request read as real HTTP. Header names are
canonical lowercase; methods keep their uppercase form, which is
case-sensitive on the wire.

A spelling maps to exactly one id, so a word that names both a header and a
header *value* — `upgrade` is the one in the built-in set — is one tag used in
two positions, and the position says which role it is in. The registration
loop asserts enum/id alignment, which is what caught that collision.

### The pool is sealed before serving

An application's own headers — `X-Request-Id`, a custom auth header, whatever
the proxy in front adds — are usually the ones it indexes on. They are not
unknown to the *application*, only to the stdlib. So the application registers
them during init and they get the same process-stable ids and integer
compares as `Host`:

```nim
let hTraceId = registerHeader("X-Trace-Id")   # init, before the first accept
...
sealHttpTags()
```

Sealing is what keeps interning off the request path. Growing the pool from
incoming bytes would be uniform and convenient, but the pool is process-global
and monotonic, so exhaustion is permanent and shared: one request carrying 511
novel names would fill it for the life of the process, and every custom header
the program met afterwards would fail — everywhere, not just on the connection
that caused it. Sealing moves exhaustion to registration, where it is a startup
error and a programmer mistake: caught at once, harmless to reject.

The HTTP pool deliberately nominates **no `escapeTag`**. With one, ids past 511
stay legal at the cost of a second token and there is no wall to detect, so any
cap would be a number we invented. Without one, 511 is structural, every tag is
exactly one token, and "the pool is full" is a real condition.

Names nobody registered still need a representation: `(xhdr "name" "value")`.
That form is for headers the application provably does not index by constant — a
proxy forwarding what it does not understand — so it is never on a hot path.

### Why this beats a string map

- **Header lookup is an integer compare.** HTTP names are case-insensitive, so
  a `Table[string, string]` design lowercases and hashes on every access.
  `hContentLength` is a `TagId` known at compile time.
- **Values are typed once, at parse time.** `Content-Length` is an `IntLit`;
  `Date` is an epoch `IntLit`. No re-parsing on access.
- **Known values are tags too.** `Connection: keep-alive` parses to
  `(connection (keep-alive))`, so the keep-alive check on every single request
  is an integer compare rather than a case-insensitive string compare. This is
  the same enum/payload idea applied one level down.
- **The application's own headers get all of the above.** A registered custom
  header is a tag like any other, which matters because those are frequently
  the ones a service actually branches on. A design that made them the
  exception would be optimizing the wrong half.
- **Multi-value headers are just multiple children.** No comma-splitting on
  access.
- **One allocation per message**, contiguous and movable.
- **Client and server share the entire message layer** — the same type is the
  parse target and the build source.

It also generalizes: HPACK's static table *is* the known-header enum, and its
dynamic table is a per-connection pool. HTTP/2 is a framing change, not a
message-model change.

### The body is not in the buffer

Bodies stream and can be gigabytes. The message is head metadata only; body
bytes arrive as their own events, borrowed from the connection's read buffer.

### Ownership and recycling

`TokenBuf` is already `=copy {.error.}`, so `HttpMsg` is move-only: a genuine
copy is rejected. (The diagnostic is late and cryptic — `'=dup' is not
available for type <HttpMsg>`, raised by hexer rather than sem, so `nimony
check` alone accepts the copy. Worth improving, but the property holds.)

`HttpMsg` needs its own `=wasMoved` to clear the "owns a buffer" flag along
with the buffer — without it a moved-from message still claims ownership,
which is precisely the question the loop asks. The loop reclaims at the top of
`next`:

```nim
proc next*(s: var HttpLoop; e: var HttpEvent; dl = Deadline.none): bool {.passive.} =
  if e.msg.hasBuf:           # handler never took it → keep the allocation
    s.recycle(move e.msg)
  ...
```

So a handler that ignores `e.msg` gets buffer reuse for free. A handler that
moves the message out — to hand it to a spawned coroutine, say — can donate the
buffer back with `e.msg = move(m)` when it is done, and pays one allocation if
it does not. **The failure mode is a slow path, never a bug.**

This needs one addition to nifcore: `clear` on `BiTable`/`Pool`, emptying the
tables while keeping their capacity. Without it, recycling a buffer means a
fresh `Pool` and loses the point.


## 2. The application pulls events

Modelled on uirelays' `Event` + `pollEvent`/`waitEvent`: a flat struct with a
kind discriminator and payload fields, filled into a caller-owned variable. No
allocation per event, and no callbacks anywhere above the driver line.

```nim
type
  HttpEventKind* = enum
    NoHttpEvent
    ConnectedEvent      ## peer accepted (and, with TLS, handshake done)
    RequestEvent        ## request head parsed and complete
    ResponseEvent       ## client side: response head parsed
    BodyEvent           ## a piece of the body; `final` marks the last
    ClosedEvent         ## peer gone or error; `status` says why
    TimeoutEvent        ## a deadline expired
    ShutdownEvent

  HttpEvent* = object
    kind*: HttpEventKind
    conn*: ConnId
    msg*: HttpMsg                    ## Request/ResponseEvent; move-only
    data*: ptr UncheckedArray[char]  ## BodyEvent; borrowed until the next `next`
    len*: int
    final*: bool
    status*: ErrorCode
```

One enum covers both directions, because a proxy needs both and the vocabulary
is the same.

```nim
proc main() {.passive.} =
  var s = listenHttp(Port(8080), budget = 30.seconds)
  var e: HttpEvent
  while s.next(e, never):
    case e.kind
    of RequestEvent:
      if e.msg.path == "/": discard s.respond(e.conn, 200, "hello\n")
      else:                 discard s.respond(e.conn, 404, "")
    of ClosedEvent: discard
    else: discard
```

`next` is `waitEvent` without a blocked thread and without a callback: it
returns immediately when events are queued, and otherwise does `delay()` +
register-with-ioring + `suspend()`.

This is the same shape one layer down — `waitCompletions` in `std/ioring` *is*
`pollEvent`, and this loop is that one with parsing on top.

### Responding

`respond` is `.passive` and returns an `ErrorCode`
(`lib/std/errorcodes/errorcodes_http.nim` already carries the status↔code
mapping):

```nim
proc respond*(s: var HttpLoop; c: ConnId; status: int;
              body: openArray[char]; dl = Deadline.none): ErrorCode {.passive.}
```

Because it suspends until the write lands, **suspension is the backpressure** —
there is no drain event and no "did I remember to check for room" bug class.
The same holds for a streaming `write(chunk)`.

Passive does not mean *always* suspends. `respond` should attempt a direct
non-blocking `write(2)` first and only go through the ring on `EAGAIN` or a
short write, so a response that fits the socket buffer never parks. The fast
path is only legal when that connection has nothing already queued in the ring,
or it reorders against in-flight writes.

### Two styles, one API

Because `respond` is passive, the inline and the coroutine-per-connection
styles are the same code:

```nim
of RequestEvent: discard s.respond(e.conn, 200, "hi")     # inline
of RequestEvent: spawn handle(s, e.conn, move e.msg)      # escaped
```

Long work — a database query, a call to another service — escapes into a
spawned coroutine and answers later through the identical `respond`. The loop
stays non-blocking and callback-free. Coroutine-per-connection, for anyone who
wants it, is then a thin layer that dispatches events by `ConnId`; the reverse
does not work, which is why the event loop is the primitive.


## 3. Deadlines are part of the model

Nimony targets designs that hold up under hard-realtime constraints, so
deadlines are explicit rather than an optional configuration knob.

**Deadlines, not timeouts.** Absolute instants, not relative durations. A
per-call timeout does not compose: `readHead(1s)`, `readBody(1s)`, `respond(1s)`
is a 3s worst case that grows with however many operations the code happens to
perform, so the request as a whole has no bound anyone can state. One absolute
deadline threaded through the request bounds the total regardless of what
happens inside it.

```nim
type Deadline* = distinct int64      ## absolute monotonic nanoseconds

proc earlier*(a, b: Deadline): Deadline   ## the only combinator
const never*: Deadline
```

`earlier` is the only combinator, and that is the invariant: a sub-operation
can tighten its caller's budget, never widen it.

`never` exists but must be *typed*. The difference between "no deadline because
I decided" and "no deadline because I did not think about it" is whether the
programmer had to write the word. Nothing that can park has a default.

### Where they are required

Explicit at the boundaries that mint work:

- `listenHttp(port, budget)` — the per-request budget for accepted connections.
- `connect(host, port, dl)` and `request(..., dl)` on the client.
- `next(e, dl)` — the loop's own wait. `TimeoutEvent` is how periodic
  housekeeping gets a turn, mirroring uirelays' `waitEvent(e, timeoutMs)`.

`Conn` carries the deadline from there, so `respond(e.conn, ...)` needs no
parameter. Every parking op takes an optional trailing `dl` that *tightens*:
the call applies `earlier(conn.deadline, dl)` internally, so passing a later
instant cannot widen the budget.

A keep-alive connection holds an **idle deadline** (when the next request must
begin); the loop re-arms it to `now() + budget` at each request boundary.

### What expiry does

The parked continuation resumes with `TimeoutError`. A deadline blown while
*writing* kills the connection — HTTP/1.1 framing cannot resynchronize after a
truncated response. A deadline blown while waiting for the next request on an
idle connection just closes it cleanly. Both surface as `ClosedEvent`.

### What this buys

- **The leak class disappears structurally.** Every parked continuation has a
  deadline, so nothing parks forever. "Who wakes a stuck `respond`" stops
  needing a case-by-case answer.
- **The scheduler can use them.** Deadlines visible to the runtime let the
  ready queue be earliest-deadline-first rather than FIFO, on both the ring's
  completion side and the pool's run queue.
- **Admission control becomes possible.** A loop that is behind can refuse a
  connection rather than accept work it cannot finish in budget — but only if
  the budget is visible at accept time.

### Mapping down

The kernel already speaks absolute deadlines. `TIMEOUT_ABS` and `link_timeout`
are in `std/posix/io_uring`, so a `Deadline` reaches the SQE with no per-call
conversion, and `link_timeout` attaches it to the preceding op — "this read,
with this deadline", one submission.

For kqueue and epoll the seam exists and is currently wasted: an idle worker
calls `gReactor(1)` and then, if nothing fired, sleeps another millisecond
(`threadpool.nim:228-238`), so it wakes every 1–2ms with nothing to do. A
per-lane min-heap of deadlines supplies that wait for real, letting a worker
sleep until the earliest deadline. The change that gives us timers also gets
rid of the fixed-granularity idle wakeup.

The ring uses `CLOCK_BOOTTIME` on Linux rather than `CLOCK_MONOTONIC` (which
`std/monotimes` uses): a machine that suspends for an hour should find its
in-flight deadlines blown, not extended. Darwin and the BSDs have no
equivalent, so they get `CLOCK_MONOTONIC`. Never `CLOCK_REALTIME`, or an NTP
step retimes everything in flight.


## 4. Connections and threading

`ConnId` is a dense integer with a generation counter, like ioring's slot
arena — not a pointer. That keeps `HttpEvent` POD and copyable, and makes a
stale reference *detectable* rather than a use-after-free, which matters
because connections die asynchronously and handlers will hold ids across
suspensions.

**One loop per thread, and a connection belongs to its lane for life.** ioring's
slot arenas and submission rings are per-lane and unlocked, and `closeFd`
already documents that ops held by another lane are not cancelled and leak
their slots. Shared-nothing avoids that entirely. Distribution is by
`SO_REUSEPORT`, or by handing a connection off exactly once at accept time.


## 5. Relays sit below the application

Two injection seams, both invisible to the code in §2:

- **`TransportRelays`** — `read`, `write`, `close`, `handshake`. Plaintext is
  the default; a TLS module overrides it and the HTTP layer never learns.
- **`FramingRelays`** — bytes to events. `RequestEvent` and `BodyEvent` mean the
  same thing in HTTP/1.1 and HTTP/2, so h2 is a second framing relay rather
  than a second API.


## 6. Module layout

| Module | Contains |
|---|---|
| `std/http/httpmsg` | tags, `HttpMsg`, builders, accessors. No IO — testable standalone. |
| `std/http/httpparse` | wire → `TokenBuf`, incremental and resumable across reads. **Done** for request and response heads. |
| `std/http/httpwire` | `TokenBuf` → wire bytes. **Done** for request and response heads. |
| `std/http/httpconn` | passive read/write on ioring, buffering, chunked framing, keep-alive. **Done**. |
| `std/httpclient`, `std/httpserver` | the loops of §2. |

Mirrors `std/ioring.nim` plus `std/ioring/`.


## 7. Prerequisites

None of this is reachable until the following exist.

**ioring**

1. ~~`submitConnect`~~ — done. Non-blocking connect through the ring; the
   attempt is started on the polling thread so the fd is watched from the
   moment it is connecting, and `SO_ERROR` on writability distinguishes a
   refusal from a success (both look identical to the poller). Completes with
   the negated errno, so a caller can tell "nobody listening" from "the
   network ate it".
2. ~~**Timers**~~ — done. `Deadline` is absolute, `never` has to be spelled,
   and every op carries one. A per-lane min-heap answers both questions the
   poll loop has — how long it may wait, and what has run out of time — and
   feeds the `poll(timeoutMs)` argument that was previously a hardcoded 0 or
   1. Entries are not removed when an op completes in time; they name the slot
   *generation* they were armed for and are dropped as stale when they surface.
   `submitTimeout` is a pure timer, and reaching its deadline is its success.

   Still to do here: io_uring can attach an absolute `link_timeout` to an
   individual SQE, which is better than one lane-wide bound. The lane-wide
   bound keeps both backends behaving identically in the meantime.
3. Multishot accept. `submitAccept` is oneshot, so a busy listener pays one
   submission per connection; io_uring has `accept_multishot`.
4. DNS. `getaddrinfo` blocks and io_uring has no resolver, so it needs
   offloading to a pool thread.

**nifcore** — done

5. ~~`clear` on `BiTable`/`Pool`~~ — see §1. `BiTable.clear` drops the values
   and blanks the hash index in place, keeping both allocations.
6. ~~`seal` on `TagPool`~~ — plus `tagId`, the lookup that answers `TagId(0)`
   instead of interning, which is what the parser is allowed to call.

**Elsewhere**

7. `std/uri` does not exist.
8. `std/monotimes` needs a `CLOCK_BOOTTIME` path.


### Parsing, in practice

Every proc in `httpparse` is `parse(buf, i) -> int`: read `buf` from `i`, answer
the index just past what was consumed, or `ParseIncomplete` / `ParseBad`. The
head is inspected in place — no slicing, no substrings — so the only bytes
copied are the ones that end up in the message, and they are copied once,
straight from the read buffer into that message's pool. `nifcore` grew an
`openArray` overload of `addStrLit` for that, and `lookupHeader` folds ASCII
case into a stack buffer.

Resumability costs the caller one integer. A head is parsed only when complete,
so `HeadScanner` looks for the blank line that ends it and remembers where it
stopped; appending more bytes and asking again resumes instead of rescanning,
which is what keeps a head arriving one byte at a time from costing O(n²).

The tag lookup does not use the pool's hash table. The vocabulary is sealed and
under 512 entries, so bucketing tags by name length leaves a handful of
candidates that a byte compare settles — no hashing, and nothing that needs a
`string` to ask with.

Serializing is the same shape in reverse: `write(dest, i, …) -> int` answers
the index just past what it wrote, or `WriteFull`. Writers are all-or-nothing,
so a refused call leaves nothing to unpick — the caller retries from the same
`i` with more room, and `headLen` says exactly how much room that is, so the
usual path never retries at all. Nothing is built in a temporary and copied
over: names are borrowed from the tag pool, values from the message's own
pool, and integers are formatted straight into `dest`.

Round-tripping is the property the tests pin down. Parsing our own output must
reproduce the same tree, and serializing that must be byte-identical — which
is what says the tag/payload split lost nothing on the way in.

There is exactly one deliberate exception, and it is on the response side: the
**reason phrase is parsed and discarded**. Nothing reads it — RFC 9110 tells
clients to ignore it and lets a proxy replace it — so keeping it would mean a
payload string on every response for no reader. `httpwire` regenerates a
canonical phrase, so `404 Totally Not Here` comes back as `404 Not Found`. The
tree still round-trips exactly; only that one string does not.

Rejections are deliberate rather than incidental, because HTTP/1.1 cannot
resynchronize and a parser that guesses is how two hops come to disagree about
where a request ends: obsolete line folding, a space between a header name and
its colon, a bare CR inside a value, a non-numeric or overflowing
`Content-Length`, and control characters anywhere in a target or value are all
`ParseBad`. Head size, header count, target length and value length are capped.

### The passive bridge, confirmed

The shape the design assumed does work:

```nim
proc readAsync*(fd: cint; buf: pointer; len: int; dl: Deadline): int {.passive.} =
  result = 0
  let c = delay()
  discard submitRead(fd, buf, len, dl, c, addr result)
  suspend()
```

The coroutine parks, a pool worker resumes it when the ring completes, and
`addr result` — a pointer into the *caller's* heap-allocated frame — is still
valid on the other side. `result` has to be assigned before its address is
taken, since the compiler will not hand out the address of something it cannot
prove is initialised.

The entry point matters, and it is the §8 hazard in practice: a chain is
started with `submit(delay(chain()), lane)` onto the pool, not called from
non-passive code. A regular proc that calls a parking passive proc gets its
`result` written into a frame it has already left.

### A compiler bug this layer had to route around

An `openArray` parameter of a `.passive` proc arrives corrupt: the length
survives, the pointer does not, so the callee reads whatever is at the wrong
address. Reduced:

```nim
proc take(data: openArray[char]) {.passive.} =
  for i in 0..<data.len: s.add data[i]

proc chain() {.passive.} =
  take("aaa"); take("bbb"); take("ccc")   # prints garbage, not aaa/bbb/ccc
```

It happens with or without a suspension point in the callee, and only for
`openArray` — a `string` parameter and a `ptr UncheckedArray[char]` plus
length both come through intact. So `httpconn` takes bodies as `string` and
destination buffers as pointer-plus-capacity, with a note at the top of the
file so nobody changes it back before the CPS transform is fixed.

Found the slow way. A chunked body came out as garbage while the chunk
*sizes* were right, which is what pointed at the parameter rather than the
framing: the lengths were being read correctly from the same object whose
pointer was not.

## 8. Open

- The exact tag vocabulary: which headers are worth a tag, and which header
  values are worth parsing into tags. Currently `Connection`,
  `Transfer-Encoding` and `Content-Encoding` resolve their values.
- Splitting list-valued headers at parse time. The tree already represents
  `(accept "a" "b")` and the serializer writes it back as `a, b`, but the
  parser stores one line as one value — splitting only some headers on commas
  (never `Date`, never `Set-Cookie`) is a decision worth making deliberately.
- The `openArray`-in-`.passive` bug above, in the compiler rather than here.
- Structured header values — `Content-Type`'s parameters, `Accept`'s q-values —
  are stored as one string today. They are the natural next thing to give
  sub-structure to, and the tree already has room for it.
- Client-side correlation: a `ReqId` returned by `send`, matched by
  `ResponseEvent`, so pipelined or multiplexed requests can be told apart.
- Whether one `HttpLoop` type serves both directions or client and server get
  their own.
- Whether cancellation stays a runtime concern that ioring implements, or wants
  the language-level support the reserved `CoroutineBase.callee` field was set
  aside for. An HTTP server is the workload that will force the answer.

### A hazard to settle first

In `readAsync(fd, buf, len) {.passive.}`, `addr result` points into the
*caller's* frame. If the caller is passive that frame is heap-allocated and
survives the suspension. If a **regular** proc calls a parking passive proc,
the frame is a stack local, `complete()` returns on park so the stack can
unwind, and the later resume writes into a dead frame.

Either the rule is "everything from the pool entry point down is passive" — in
which case `main` above must be `{.passive.}`, as written — or a regular caller
of a parking passive proc should be rejected outright. This is a question about
the CPS model, not about HTTP, but HTTP is where it bites.

# (c) 2026 Andreas Rumpf
#
# Topic trees: one message, many connections.
#
# A WebSocket has one writer and one place that writes to it — that rule is
# what keeps two threads from framing into one connection at the same time. A
# fan-out, and a chat room is the usual one, therefore needs a place that owns
# the writers, and a `TopicTree` is that place: connections subscribe to
# topics, and a publish writes one message into every member's buffer.
#
#   import std/[httpserver, websocket, topictree]
#
#   var tree = initTopicTree()
#
#   proc handle(c: sink HttpConnection) {.passive.} =
#     if not c.next(): return
#     try:
#       var ws = upgradeWebSocket(c)
#       tree.subscribe("chat", ws)
#       while true:
#         var msg = ""
#         case ws.recv(msg)
#         of mkText: tree.publish("chat", msg)
#         of mkClose: break
#         else: discard
#     except ErrorCode:
#       discard
#     ws.close()
#
# Topics are paths, as in uWebSockets: `"chat"`, `"chat/room1"`,
# `"chat/room1/users"`. Publishing to a topic reaches its subscribers and the
# subscribers of everything below it, so `publish("chat", ...)` is the whole
# room and `publish("chat/room1", ...)` one part of it. The tree is a tree and
# not a list of members to scan: a publish walks the path once and then the
# subscribers that hang off the node it lands on, so its cost is the depth of
# the topic and the size of the subtree, not the size of the tree.
#
# ## What a subscription changes
#
# `subscribe` gives the connection's writer to the tree. From then on the tree
# is the one place that writes to that connection, and the connection's own
# `send`, `flush`, ping answers and `close` go through it: a `ws.send` is
# queued with the tree's next turn, and the tree flushes each member once per
# turn. That is the shape Bun's server has, and it is why a published line
# costs one queue slot and one buffer append per member — not a task per
# delivery, which is what a fan-out costs when every connection writes for
# itself.
#
# ## Shards: one writer chain per worker
#
# The copying is what a fan-out costs, and it is linear in the number of
# members. One chain doing all of it is one core, so the tree spreads its
# members over **shards** — one per pool worker by default — and each shard
# owns the writers of its own members: it does the appends and the flushes for
# them, on its own lane.
#
# The shards share one queue, and it is a *broadcast* queue: a publish is one
# reservation on it — one atomic compare-exchange — and then every shard reads
# the same event and applies it to its own members. What decides that a slot
# may be reused is not the shard that has read it first but the one that reads
# it **last**: each slot counts down the shards that still have to take it,
# and the last one advances the queue's head. A publisher waits only for that
# head, never for the shards to finish their turn.
#
# So a publish does not get more expensive with the number of cores; what grows
# with the cores is the number of appends done in parallel. The fixed cost is
# one slot per shard per publish (one load, one decrement).
#
# A connection belongs to one tree only, because it has one writer. It can
# subscribe to any number of topics in it and leave them individually; the
# subscription ends when the connection does.

import std / [websocket, threadpool, atomics, ticketlocks]

type
  TopicEventKind = enum
    teJoin, tePublish, teSend, teClose, teAddTopic, teDropTopic

  TopicNode = ref object
    ## One segment of a topic path, and what hangs off it.
    segment: string
    parent: nil TopicNode
    children: seq[TopicNode]
    subs: seq[Member]

  Member = ref object
    ## One connection's writer, owned by a shard from `subscribe` on.
    wr: WebSocketWriter
    id: int
    topics: seq[string]
    dirty: bool
      ## Frames were buffered for this member since the last flush.
    gen: int
      ## The last publish it was given. A member subscribed to a topic *and*
      ## one of its parents is reached twice by one publish; the generation is
      ## what keeps that to one delivery.
    gone: bool
      ## Its socket failed: it is out of the tree and out of the shard's
      ## members at the end of the turn, without disturbing the walk that
      ## found it.

  Payload = ref object
    ## One event in flight: written by the publisher, read by every shard, and
    ## freed when the last of them is done with it.
    kind: TopicEventKind
    shard: int
      ## Which shard applies it. A publish is `-1`: every shard has members
      ## that need the line.
    member: nil Member
    id: int
    msg: MsgKind
    code: int
    topic: string
    text: string
      ## `teSend`: the message. Already checked and copied.
    frame: string
      ## `tePublish`: the message already spelled as a frame. A server masks
      ## nothing, so the same bytes go to every member.

  Slot = object
    ## One position of the broadcast queue.
    seq: int
      ## The sequence number published here; `-1` while the slot is unused.
    left: int
      ## Shards that still have to take this event. The one that brings it to
      ## zero advances the queue's head.
    payload: nil Payload

  Hub = ref object
    ## The broadcast queue and what waits on it. Shared by the tree's handle
    ## and its shards; the handle going away is what closes it.
    slots: seq[Slot]
    mask: int
    tail: int
      ## The next sequence number to publish.
    head: int
      ## The first sequence number no shard is finished with.
    consumers: int
    closed: bool
    lock: TicketLock
      ## Guards the waiter lists only, and is never held across a `submit`.
    ready: seq[seq[Continuation]]
      ## Per shard, the parked shard waiting for the next event.
    parked: int
      ## Bit `k` is set while shard `k` is in `ready[k]`. Read without the
      ## lock so that a publish that nobody is waiting for takes no lock at
      ## all.
    senders: seq[Continuation]
      ## Publishers parked because the queue is full.
    waiting: int
      ## How many entries `senders` has, read without the lock.

  Shard = ref object
    ## One writer chain: the members it owns, the tree it keeps for them, and
    ## how far it has read the broadcast queue.
    id: int
    root: TopicNode
    members: seq[Member]
    touched: seq[Member]
      ## The members with something to do at the end of the turn: frames to
      ## flush, or a socket that failed and has to go. A turn with one message
      ## for one member touches one member, not all of them — which is what
      ## makes a tree of thousands of quiet connections cheap.
    read: int
    gen: int
      ## Bumped for every publish; see `Member.gen`.
    lastTopic: string
      ## The topic of the last publish and the node it resolved to: a server
      ## publishes the same topic over and over, and one string compare beats
      ## walking the path.
    lastNode: nil TopicNode

  TopicTree* = object
    ## A tree of topics and the chains that write to the connections in it.
    ## A handle: copies share the tree, and a default-constructed one has no
    ## chain behind it yet — `initTopicTree` makes one.
    t: nil TopicTreeRef

  TopicTreeInner = object of WriterOwner
    hub: Hub
    shards: seq[Shard]
    nextId: int

  TopicTreeRef = ref TopicTreeInner

# ------------------------------------------------------------ the tree --

proc newTopic(segment: string; parent: nil TopicNode): TopicNode =
  TopicNode(segment: segment, parent: parent, children: @[], subs: @[])

proc childFor(t: TopicNode; segment: string): TopicNode =
  ## The child of `t` called `segment`, created if it is not there yet.
  var i = 0
  while i < t.children.len:
    if t.children[i].segment == segment: return t.children[i]
    inc i
  result = newTopic(segment, t)
  t.children.add result

proc nodeFor(t: TopicNode; topic: string): TopicNode =
  ## The node of `topic`, created along the way. `""` is the root itself.
  result = t
  var i = 0
  while i <= topic.len:
    let start = i
    while i < topic.len and topic[i] != '/': inc i
    if i > start:
      result = result.childFor(topic.substr(start, i - 1))
    inc i

proc findNode(t: TopicNode; topic: string): nil TopicNode =
  ## The node of `topic`, or nil when nothing ever subscribed there.
  var cur: nil TopicNode = t
  var i = 0
  while i <= topic.len:
    let here = cur
    if here == nil: return nil
    let start = i
    while i < topic.len and topic[i] != '/': inc i
    if i > start:
      let segment = topic.substr(start, i - 1)
      var k = 0
      var next: nil TopicNode = nil
      while k < here.children.len:
        if here.children[k].segment == segment:
          next = here.children[k]
          break
        inc k
      cur = next
    inc i
  result = cur

proc unlink(n: TopicNode) =
  ## Drops an empty node from its parent, and that parent too when it becomes
  ## empty, up to the root. What a `dropTopic` leaves behind is not kept.
  var cur = n
  while true:
    let p = cur.parent
    if p == nil: return
    if cur.subs.len > 0 or cur.children.len > 0: return
    var i = 0
    while i < p.children.len:
      if p.children[i] == cur:
        p.children.delete i
        break
      inc i
    cur = p

proc dropSub(t: TopicNode; m: Member) =
  var i = 0
  while i < t.subs.len:
    if t.subs[i] == m: t.subs.delete i
    else: inc i
  if t.subs.len == 0: unlink(t)

proc resolve(s: Shard; topic: string): nil TopicNode =
  ## The node of `topic`, through the shard's one-entry cache.
  let cached = s.lastNode
  if cached != nil and s.lastTopic == topic: return cached
  result = findNode(s.root, topic)
  let found = result
  if found != nil:
    s.lastTopic = topic
    s.lastNode = found

proc markDirty(s: Shard; m: Member) =
  if not m.dirty:
    m.dirty = true
    s.touched.add m

proc markGone(s: Shard; m: Member) =
  if not m.gone:
    m.gone = true
    # A member that already had frames buffered is in `touched`; one that did
    # not has to be added for its socket to be closed out this turn.
    if not m.dirty: s.touched.add m

proc deliver(s: Shard; t: TopicNode; frame: string; gen: int) =
  ## One publish to `t` and everything below it, each member once.
  ##
  ## `frame` is the message already spelled as a frame. A server masks
  ## nothing, so the bytes are the same for every member: they are built once
  ## per publish and appended, instead of encoded again for each of the
  ## subscribers.
  var i = 0
  while i < t.subs.len:
    let m = t.subs[i]
    if not m.gone and m.gen != gen:
      m.gen = gen
      try:
        m.wr.bufferFrame(frame)
        markDirty(s, m)
      except ErrorCode:
        # A member whose socket is gone takes the line with it; the rest of
        # the subtree does not care, and the walk does not stop for it.
        m.wr.abort()
        markGone(s, m)
    inc i
  i = 0
  while i < t.children.len:
    deliver(s, t.children[i], frame, gen)
    inc i

proc forget(root: TopicNode; m: Member) =
  ## Takes a member out of every topic it was in.
  var i = 0
  while i < m.topics.len:
    let n = findNode(root, m.topics[i])
    if n != nil: dropSub(n, m)
    inc i

proc findBy(id: int; members: seq[Member]): int =
  ## The index of member `id` in `members`, or `members.len` when it has left.
  result = 0
  while result < members.len and members[result].id != id: inc result

# ------------------------------------------------------- the broadcast queue --

proc hasEvent(hub: Hub; seq: int; order: MemoryOrder): bool =
  ## Is sequence `seq` published? The parking path asks with a sequentially
  ## consistent load: the publisher stores `seq` before it looks at `parked`,
  ## so a shard that is about to park and does not see the event and a
  ## publisher that does not see the parked shard cannot both happen.
  atomicLoad(hub.slots[seq and hub.mask].seq, order) == seq

proc wakeShards(hub: Hub) =
  ## Resumes whatever shard was parked for the event just published. Takes the
  ## lock only when the bitmask says somebody waits, which under load is
  ## nobody: the shards are busy with their own members.
  if atomicLoad(hub.parked, moSequentiallyConsistent) == 0: return
  var ks: seq[Continuation] = @[]
  acquire hub.lock
  let mask = atomicExchange(hub.parked, 0, moSequentiallyConsistent)
  var i = 0
  while i < hub.consumers:
    if (mask and (1 shl i)) != 0 and hub.ready[i].len > 0:
      ks.add hub.ready[i][0]
      hub.ready[i].delete 0
    inc i
  release hub.lock
  for k in ks: submit(k)

proc wakeSenders(hub: Hub) =
  ## Resumes the publishers that parked on a full queue: one slot just became
  ## reusable.
  if atomicLoad(hub.waiting, moSequentiallyConsistent) == 0: return
  var ks: seq[Continuation] = @[]
  acquire hub.lock
  ks = hub.senders
  hub.senders = @[]
  atomicStore(hub.waiting, 0, moRelaxed)
  release hub.lock
  for k in ks: submit(k)

proc parkShard(hub: Hub; s: Shard) {.passive.} =
  ## Waits for the next event.
  ##
  ## The wait is **announced before the queue is looked at again**, not after:
  ## the publisher stores the event and then looks at `parked`, so a shard that
  ## announced first and a publisher that looked after cannot both miss each
  ## other — that would be a lost wakeup, and the shard would sleep with an
  ## event waiting for it. If the look finds work (it was published while the
  ## shard was deciding), the announcement is taken back and the chain simply
  ## continues.
  let k = delay()
  let bit = 1 shl s.id
  var go = false
  acquire hub.lock
  hub.ready[s.id].add k
  discard atomicFetchOr(hub.parked, bit, moSequentiallyConsistent)
  if atomicLoad(hub.closed, moSequentiallyConsistent) or
     hasEvent(hub, s.read, moSequentiallyConsistent):
    hub.ready[s.id].delete(hub.ready[s.id].len - 1)
    discard atomicFetchAnd(hub.parked, not bit, moSequentiallyConsistent)
    go = true
  release hub.lock
  if go: submit(k)
  suspend()

proc parkSender(hub: Hub) {.passive, raises.} =
  ## Waits for the queue to have room, announced before the head is looked at
  ## for the same reason `parkShard` announces before it looks at the queue.
  let k = delay()
  var go = false
  acquire hub.lock
  hub.senders.add k
  discard atomicFetchAdd(hub.waiting, 1, moSequentiallyConsistent)
  if atomicLoad(hub.closed, moSequentiallyConsistent) or
     atomicLoad(hub.tail, moRelaxed) -
       atomicLoad(hub.head, moSequentiallyConsistent) < hub.slots.len:
    hub.senders.delete(hub.senders.len - 1)
    discard atomicFetchSub(hub.waiting, 1, moRelaxed)
    go = true
  release hub.lock
  if go: submit(k)
  suspend()

proc shutdown(hub: Hub) =
  ## Ends the shards: the tree's last handle is gone.
  if atomicExchange(hub.closed, true, moSequentiallyConsistent): return
  var ks: seq[Continuation] = @[]
  acquire hub.lock
  discard atomicExchange(hub.parked, 0, moSequentiallyConsistent)
  var i = 0
  while i < hub.consumers:
    var j = 0
    while j < hub.ready[i].len:
      ks.add hub.ready[i][j]
      inc j
    hub.ready[i] = @[]
    inc i
  i = 0
  while i < hub.senders.len:
    ks.add hub.senders[i]
    inc i
  hub.senders = @[]
  atomicStore(hub.waiting, 0, moRelaxed)
  release hub.lock
  for k in ks: submit(k)

proc emit(t: TopicTreeRef; p: sink Payload) {.passive, raises.} =
  ## One publish, and the only operation a publisher does on the queue that is
  ## shared with every other publisher: one compare-exchange on `tail`. The
  ## slot is then written by this thread alone, and the payload is freed by
  ## whichever shard takes it last.
  let hub = t.hub
  var seq = 0
  while true:
    if atomicLoad(hub.closed, moSequentiallyConsistent):
      raise BadOperation
    seq = atomicLoad(hub.tail, moRelaxed)
    if seq - atomicLoad(hub.head, moAcquire) < hub.slots.len:
      var expected = seq
      if atomicCompareExchange(hub.tail, expected, seq + 1,
                               moAcquireRelease, moRelaxed):
        break
    else:
      parkSender(hub)
  let sl = addr hub.slots[seq and hub.mask]
  sl.payload = p
  atomicStore(sl.left, hub.consumers, moRelaxed)
  atomicStore(sl.seq, seq, moSequentiallyConsistent)
  wakeShards(hub)

# ----------------------------------------------------------- the fan-out --

proc apply(s: Shard; p: Payload) =
  ## Applies one event that is addressed to this shard.
  case p.kind
  of teJoin:
    let m = p.member
    if m != nil:
      s.members.add m
      nodeFor(s.root, p.topic).subs.add m
      s.lastNode = nil
  of teSend:
    let i = findBy(p.id, s.members)
    if i < s.members.len:
      try:
        s.members[i].wr.send(p.msg, p.text, flush = false)
        markDirty(s, s.members[i])
      except ErrorCode:
        s.members[i].wr.abort()
        markGone(s, s.members[i])
  of teAddTopic, teDropTopic:
    let i = findBy(p.id, s.members)
    if i < s.members.len:
      let m = s.members[i]
      s.lastNode = nil
      if p.kind == teAddTopic:
        var seen = false
        var k = 0
        while k < m.topics.len:
          if m.topics[k] == p.topic: seen = true
          inc k
        if not seen:
          m.topics.add p.topic
          nodeFor(s.root, p.topic).subs.add m
      else:
        var k = 0
        while k < m.topics.len:
          if m.topics[k] == p.topic:
            m.topics.delete k
            break
          inc k
        let n = findNode(s.root, p.topic)
        if n != nil: dropSub(n, m)
  of teClose:
    let i = findBy(p.id, s.members)
    if i < s.members.len:
      let m = s.members[i]
      s.members.delete i
      forget(s.root, m)
      s.lastNode = nil
      m.gone = true            # out of `members` already; the flush must skip it
      try:
        m.wr.close(if p.code == 0: 1000 else: p.code)
      except ErrorCode:
        m.wr.abort()
  of tePublish:
    discard # handled by `deliverShard`, which needs the frame

proc deliverShard(s: Shard; p: Payload) =
  inc s.gen
  let n = resolve(s, p.topic)
  if n != nil: deliver(s, n, p.frame, s.gen)

proc drop(s: Shard; m: Member) =
  ## Takes a dead member out of the shard and out of the topic tree.
  let i = findBy(m.id, s.members)
  if i < s.members.len:
    s.members.delete i
    forget(s.root, m)

proc flushShard(s: Shard) =
  ## One flush per member that has frames buffered, and the members whose
  ## socket failed leave the tree here. The quiet members of the tree are not
  ## looked at at all.
  var i = 0
  while i < s.touched.len:
    let m = s.touched[i]
    if m.gone:
      drop(s, m)
    else:
      m.dirty = false
      try:
        m.wr.flush()
      except ErrorCode:
        m.wr.abort()
        m.gone = true
        drop(s, m)
    inc i
  s.touched = @[]

proc shardLoop(hub: Hub; s: Shard) {.passive.} =
  ## One writer chain. It reads the broadcast queue in order, applies what is
  ## addressed to its members, flushes them once per turn, and parks when
  ## there is nothing left to do.
  while true:
    try:
      while hasEvent(hub, s.read, moAcquire):
        let sl = addr hub.slots[s.read and hub.mask]
        let p = sl.payload
        if p != nil:
          if p.kind == tePublish: deliverShard(s, p)
          elif p.shard == s.id: apply(s, p)
        # The last shard to take the event is the one that makes the slot
        # reusable; the publisher waits for this, not for the turn to end.
        if atomicFetchSub(sl.left, 1, moAcquireRelease) == 1:
          atomicStore(hub.head, s.read + 1, moSequentiallyConsistent)
          wakeSenders(hub)
        inc s.read
      flushShard(s)
      if atomicLoad(hub.closed, moSequentiallyConsistent) and
         not hasEvent(hub, s.read, moAcquire):
        return
      if not hasEvent(hub, s.read, moAcquire):
        parkShard(hub, s)
    except ErrorCode:
      discard

# ------------------------------------------------------------- the API --

proc newShard(id: int): Shard =
  Shard(id: id, root: newTopic("", nil), members: @[], touched: @[], read: 0,
        gen: 0, lastTopic: "", lastNode: nil)

proc nextPow2(n: int): int =
  result = 2
  while result < n: result = result shl 1

proc treeLoopStart(hub: Hub; shards: seq[Shard]) =
  ## Starts one chain per shard, each hinted at the worker of its number.
  var i = 0
  while i < shards.len:
    submit(delay(shardLoop(hub, shards[i])), i)
    inc i

proc initTopicTree*(capacity = 4096; shards = 0): TopicTree {.raises.} =
  ## A tree, and the chains that fan out for it. `ioRing` has to be up, like
  ## for any server.
  ##
  ## `shards` is how many writer chains the members are spread over; `0` means
  ## one per pool worker, which is what a server wants — the appends and the
  ## flushes of one room then run on as many cores as the pool has.
  ##
  ## `capacity` is how many events may be in flight in the shared queue before
  ## a publisher parks. It is not a batch size: a shard takes as many events
  ## per turn as are published, so this only bounds how far a publisher may
  ## run ahead of the slowest shard.
  # The parked shards are a bitmask in one `int`, so 64 shards is the ceiling;
  # nothing close to that runs on a machine today.
  let count = min(if shards > 0: shards else: max(1, workerCount), 64)
  let cap = nextPow2(max(2, capacity))
  var hub = Hub(slots: newSeq[Slot](cap), mask: cap - 1, tail: 0, head: 0,
                consumers: count, closed: false, lock: TicketLock(),
                ready: newSeq[seq[Continuation]](count), parked: 0,
                senders: @[], waiting: 0)
  var i = 0
  while i < cap:
    hub.slots[i].seq = -1
    inc i
  var ss: seq[Shard] = @[]
  i = 0
  while i < count:
    ss.add newShard(i)
    inc i
  let t = TopicTreeRef(hub: hub, shards: ss, nextId: 0)
  treeLoopStart(hub, ss)
  result = TopicTree(t: t)

proc `=destroy`(t: TopicTreeInner) =
  ## The last handle is gone: the shards are told to stop and the queue goes
  ## with them.
  let hub = t.hub
  if hub != nil: shutdown(hub)
  `=destroy`(t.hub)
  `=destroy`(t.shards)

proc treeOf(t: TopicTree): TopicTreeRef {.raises.} =
  ## The tree behind the handle. A default-constructed handle has none.
  let x = t.t
  if x == nil: raise BadOperation
  result = x

proc shardCount*(t: TopicTree): int {.raises.} =
  ## How many writer chains this tree runs, and so how many events one publish
  ## costs in queue traffic.
  treeOf(t).shards.len

proc attach(t: TopicTreeRef; ws: var WebSocket; topic: string): int {.
    passive, raises.} =
  ## A member with this connection's writer, in `topic`, and the shard that
  ## takes care of it from now on.
  let id = atomicFetchAdd(t.nextId, 1, moRelaxed)
  let m = Member(wr: ws.takeWriter(t, id), id: id, topics: @[topic],
                 dirty: false, gen: 0, gone: false)
  var p = Payload(kind: teJoin, shard: id mod t.shards.len, member: m, id: id,
                  topic: topic)
  emit(t, p)
  result = id

proc addTopic*(tree: TopicTree; id: int; topic: string) {.passive, raises.} =
  ## Puts member `id` in `topic` as well. The tree applies it in order with
  ## the publishes that follow, so a `subscribe` then a `publish` cannot lose
  ## the connection in between.
  let t = treeOf(tree)
  var p = Payload(kind: teAddTopic, shard: id mod t.shards.len, id: id,
                  topic: topic)
  emit(t, p)

proc dropTopic*(tree: TopicTree; id: int; topic: string) {.passive, raises.} =
  ## Takes member `id` out of `topic`, leaving the connection in the tree.
  let t = treeOf(tree)
  var p = Payload(kind: teDropTopic, shard: id mod t.shards.len, id: id,
                  topic: topic)
  emit(t, p)

proc publish*(tree: TopicTree; topic: string; data: string) {.
    passive, raises.} =
  ## Sends `data` as a text message to every member of `topic` **and of every
  ## topic below it**: `publish("chat", ...)` is the whole tree,
  ## `publish("chat/room1", ...)` one part of it.
  ##
  ## The text is checked as UTF-8 and spelled as a frame once, here; every
  ## shard appends the same bytes to its own members. This parks when the
  ## queue is full, which is the tree's backpressure and the reason a
  ## publisher cannot outrun its members without bound.
  let t = treeOf(tree)
  var p = Payload(kind: tePublish, shard: -1, topic: topic, frame: "")
  spellTextFrame(p.frame, data)
  emit(t, p)

proc subscribe*(tree: TopicTree; topic: string; ws: var WebSocket) {.
    passive, raises.} =
  ## Joins `topic` in this tree — a path like `"chat/room1"`, whose parents are
  ## not implied: a publish to `"chat"` reaches this connection because it is
  ## below `"chat"`, and a publish to `"chat/room1"` because it is that topic.
  ##
  ## The first call gives the connection's writer to the tree, which writes to
  ## it from then on; later calls add topics. A connection belongs to one tree
  ## only, because it has one writer.
  let t = treeOf(tree)
  let owner = ws.writerOwner
  let self: WriterOwner = t
  if owner == nil:
    discard t.attach(ws, topic)
  elif owner != self:
    raise BadOperation
  else:
    tree.addTopic(ws.memberId, topic)

proc unsubscribe*(tree: TopicTree; topic: string; ws: var WebSocket) {.
    passive, raises.} =
  ## Leaves `topic`; the connection stays in the tree for its other topics,
  ## and a publish to a parent of `topic` no longer reaches it. A connection
  ## that is not in this tree is left alone.
  let t = treeOf(tree)
  let self: WriterOwner = t
  if ws.writerOwner != self: return
  tree.dropTopic(ws.memberId, topic)

# ---------------------------------------------- what the websocket routes here --

method enqueueSend(t: TopicTreeRef; id: int; kind: MsgKind; text: string) {.
    passive, raises.} =
  var p = Payload(kind: teSend, shard: id mod t.shards.len, id: id, msg: kind,
                  text: text)
  emit(t, p)

method enqueueClose(t: TopicTreeRef; id: int; code: int) {.passive, raises.} =
  var p = Payload(kind: teClose, shard: id mod t.shards.len, id: id, code: code)
  emit(t, p)

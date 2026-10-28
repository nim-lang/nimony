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
# costs one channel send and one buffer append per member — not a task per
# delivery, which is what a fan-out costs when every connection writes for
# itself.
#
# A connection belongs to one tree only, because it has one writer. It can
# subscribe to any number of topics in it and leave them individually; the
# subscription ends when the connection does.

import std / [websocket, channels, threadpool, atomics, ticketlocks]

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
    ## One connection's writer, owned by the tree from `subscribe` on.
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
      ## Its socket failed: it is out of the tree and out of `members` at the
      ## end of the turn, without disturbing the walk that found it.

  TopicEvent = object
    kind: TopicEventKind
    member: nil Member
    id: int
    topic: string
    text: string
    msg: MsgKind
    code: int

  TopicTree* = object
    ## A tree of topics and the chain that writes to the connections in it.
    ## A handle: copies share the tree, and a default-constructed one has no
    ## chain behind it yet — `initTopicTree` makes one.
    t: nil TopicTreeObj

  TopicTreeObj = ref object of WriterOwner
    inbox: Sender[TopicEvent]
    nextId: int
    gen: int
      ## Bumped for every publish; see `Member.gen`.
    root: TopicNode
    lastTopic: string
      ## The topic of the last publish and the node it resolved to: a server
      ## publishes the same topic over and over, and one string compare beats
      ## walking the path.
    lastNode: nil TopicNode

proc treeLoop(t: TopicTreeObj; rx: sink Receiver[TopicEvent]) {.passive.}

proc treeOf(t: TopicTree): TopicTreeObj {.raises.} =
  ## The tree behind the handle. A default-constructed handle has none.
  let x = t.t
  if x == nil: raise BadOperation
  result = x

proc initTopicTree*(capacity = 4096): TopicTree {.raises.} =
  ## A tree, and the chain that fans out for it. `ioRing` has to be up, like
  ## for any server.
  ##
  ## `capacity` is how many publishes may be queued before a publisher parks.
  ## It is also the batch the tree drains per turn, and so the number of
  ## frames that share one flush per member: too small and a fan-out pays a
  ## syscall per frame, too large and a slow peer's frames sit in the queue.
  var (rx, tx) = channel[TopicEvent](capacity)
  let t = TopicTreeObj(inbox: tx, nextId: 0, gen: 0, root: newTopic("", nil),
                       lastTopic: "", lastNode: nil)
  submit(delay(treeLoop(t, rx)), -1)
  result = TopicTree(t: t)

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

# ----------------------------------------------------------- the fan-out --

proc resolve(t: TopicTreeObj; topic: string): nil TopicNode =
  ## The node of `topic`, through the one-entry cache.
  let cached = t.lastNode
  if cached != nil and t.lastTopic == topic: return cached
  result = findNode(t.root, topic)
  let found = result
  if found != nil:
    t.lastTopic = topic
    t.lastNode = found

proc deliver(t: TopicNode; frame: string; gen: int) =
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
        m.dirty = true
      except ErrorCode:
        # A member whose socket is gone takes the line with it; the rest of
        # the subtree does not care, and the walk does not stop for it.
        m.wr.abort()
        m.gone = true
    inc i
  i = 0
  while i < t.children.len:
    deliver(t.children[i], frame, gen)
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

proc treeLoop(t: TopicTreeObj; rx: sink Receiver[TopicEvent]) {.passive.} =
  ## The one chain that writes to a tree's sockets. Everything it writes is
  ## buffered per member with `flush = false`; the flush happens when the queue
  ## runs dry, so one write carries a whole turn's frames.
  let root = t.root
  var members: seq[Member] = @[]
  var frame = ""
  var e = TopicEvent()
  while true:
    try:
      if not rx.tryRecv(e):
        var i = 0
        while i < members.len:
          let m = members[i]
          if m.gone:
            forget(root, m)
            members.delete i
          else:
            if m.dirty:
              m.dirty = false
              try:
                m.wr.flush()
                inc i
              except ErrorCode:
                m.wr.abort()
                m.gone = true
                forget(root, m)
                members.delete i
            else:
              inc i
        if not rx.recv(e): return
      case e.kind
      of teJoin:
        let m = e.member
        if m != nil:
          members.add m
          nodeFor(root, e.topic).subs.add m
          t.lastNode = nil
      of tePublish:
        inc t.gen
        let n = resolve(t, e.topic)
        if n != nil:
          # The frame is spelled once: a server masks nothing, so every member
          # gets the same bytes. The text was checked when it was published.
          spellTextFrame(frame, e.text)
          deliver(n, frame, t.gen)
      of teSend:
        let i = findBy(e.id, members)
        if i < members.len:
          try:
            members[i].wr.send(e.msg, e.text, flush = false)
            members[i].dirty = true
          except ErrorCode:
            members[i].wr.abort()
            members[i].gone = true
      of teAddTopic, teDropTopic:
        let i = findBy(e.id, members)
        if i < members.len:
          let m = members[i]
          t.lastNode = nil
          if e.kind == teAddTopic:
            var seen = false
            var k = 0
            while k < m.topics.len:
              if m.topics[k] == e.topic: seen = true
              inc k
            if not seen:
              m.topics.add e.topic
              nodeFor(root, e.topic).subs.add m
          else:
            var k = 0
            while k < m.topics.len:
              if m.topics[k] == e.topic:
                m.topics.delete k
                break
              inc k
            let n = findNode(root, e.topic)
            if n != nil: dropSub(n, m)
      of teClose:
        let i = findBy(e.id, members)
        if i < members.len:
          let m = members[i]
          members.delete i
          forget(root, m)
          t.lastNode = nil
          try:
            m.wr.close(if e.code == 0: 1000 else: e.code)
          except ErrorCode:
            m.wr.abort()
    except ErrorCode:
      discard

# ------------------------------------------------------------- the API --

proc attach(t: TopicTreeObj; ws: var WebSocket; topic: string): int {.
    passive, raises.} =
  ## A member with this connection's writer, in `topic`. The tree writes to it
  ## from now on.
  let id = atomicFetchAdd(t.nextId, 1, moRelaxed)
  let m = Member(wr: ws.takeWriter(t, id), id: id, topics: @[topic],
                 dirty: false, gen: 0, gone: false)
  discard t.inbox.send TopicEvent(kind: teJoin, member: m, id: id, topic: topic)
  result = id

proc addTopic*(tree: TopicTree; id: int; topic: string) {.passive, raises.} =
  ## Puts member `id` in `topic` as well. The tree applies it in order with
  ## the publishes that follow, so a `subscribe` then a `publish` cannot lose
  ## the connection in between.
  let t = treeOf(tree)
  discard t.inbox.send TopicEvent(kind: teAddTopic, id: id, topic: topic)

proc dropTopic*(tree: TopicTree; id: int; topic: string) {.passive, raises.} =
  ## Takes member `id` out of `topic`, leaving the connection in the tree.
  let t = treeOf(tree)
  discard t.inbox.send TopicEvent(kind: teDropTopic, id: id, topic: topic)

proc publish*(tree: TopicTree; topic: string; data: string) {.passive, raises.} =
  ## Sends `data` as a text message to every member of `topic` **and of every
  ## topic below it**: `publish("chat", ...)` is the whole tree,
  ## `publish("chat/room1", ...)` one part of it.
  ##
  ## The tree does the writing; this parks when its queue is full, which is
  ## the tree's backpressure and the reason a publisher cannot outrun its
  ## members without bound.
  let t = treeOf(tree)
  discard t.inbox.send TopicEvent(kind: tePublish, topic: topic, text: data)

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

method enqueueSend(t: TopicTreeObj; id: int; kind: MsgKind; text: string) {.
    passive, raises.} =
  discard t.inbox.send TopicEvent(kind: teSend, id: id, msg: kind, text: text)

method enqueueClose(t: TopicTreeObj; id: int; code: int) {.passive, raises.} =
  discard t.inbox.send TopicEvent(kind: teClose, id: id, code: code)

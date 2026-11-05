# `std/httpclient`'s `httpRequest` against an in-process `std/httpserver`:
# the status code and reason of every response (non-2xx included, with its
# body), redirects (absolute path, relative, absolute URL, 303 and 307 after
# a POST, the loop limit, `maxRedirects = 0`, no `Location`), credentials
# dropped across origins, `HEAD`, and a deadline that expires while waiting
# for the response head but does not limit the body.
#
# Redirect rules follow RFC 9110 §15.4.

import std / [httpserver, httpclient, ioring, threadpool, atomics, assertions,
              syncio, strutils]

let tags = newHttpTags()

var serverDone: int = 0
var clientDone: int = 0
var gPort: int = 0
var listening: int = 0
var stopping: int = 0
var log = ""

proc awaitFlag(flag: var int) =
  const BudgetMs = 20_000
  let start = monoNow()
  while atomicLoad(flag, moAcquire) == 0:
    if millisUntil(monoNow(), start) > BudgetMs: break
  assert atomicLoad(flag, moAcquire) == 1, "a passive chain never finished"
  atomicStore(flag, 0, moRelease)

proc url(path: string): string = "http://127.0.0.1:" & $gPort & path

proc handle(c: sink HttpConnection) {.passive.} =
  while c.next():
    try:
      let p = c.path
      if p == "/":
        c.respond(200, "hello\n", "text/plain")
      elif p.startsWith("/code/"):
        let code = parseInt(p.substr(6))
        c.respond(code, "status " & $code & "\n", "text/plain")
      elif p == "/target":
        c.respond(200, "target\n", "text/plain")
      elif p == "/moved":
        c.redirect("/target", 301)
      elif p == "/rel/a/b":
        c.redirect("../c?q=1", 302)
      elif p == "/rel/c":
        c.respond(200, "rel c " & c.query & "\n", "text/plain")
      elif p == "/abs":
        c.redirect(url("/target"), 302)
      elif p == "/see-other":
        discard c.readBody()
        c.redirect("/echo", 303)
      elif p == "/temp":
        discard c.readBody()
        c.redirect("/echo", 307)
      elif p == "/echo":
        let body = c.readBody()
        c.respond(200, tags.name(c.meth) & " body=" & body & " type=" &
                  c.requestHeader("Content-Type") & "\n", "text/plain")
      elif p == "/loop":
        c.redirect("/loop", 302)
      elif p == "/nolocation":
        c.respond(302, "", "text/plain")
      elif p == "/cross":
        c.redirect("http://localhost:" & $gPort & "/auth", 302)
      elif p == "/same":
        c.redirect("/auth", 302)
      elif p == "/auth":
        c.respond(200, "auth=" & c.requestHeader("Authorization") & " cookie=" &
                  c.requestHeader("Cookie") & "\n", "text/plain")
      elif p == "/slow":
        # never answer (parked, not sleeping): the client's deadline must end it
        var (r, w) = c.takeStreams()
        r.renew(afterMs(20_000))
        discard r.readAll()
      else:
        c.respond(404, "no\n", "text/plain")
    except ErrorCode:
      c.close()

proc serverChain() {.passive.} =
  var s = listenHttp(0'u16, tags)
  s.serverName = "t"
  gPort = int(boundPort(s.fd))
  atomicStore(listening, 1, moRelease)
  while true:
    let c = s.accept()
    if c.isClosed: break
    submit(delay(handle(c)), -1)
    # `stopping` is set before the client's last request
    if atomicLoad(stopping, moAcquire) == 1: break
  s.close()
  atomicStore(serverDone, 1, moRelease)

proc show(tag: string; r: var HttpResponse) {.passive, raises.} =
  let body = r.body.readAll()
  log.add tag & " " & $r.code & " " & r.reason & " " &
          r.url.substr(url("").len) & " [" & body.strip() & "]\n"

proc clientChain() {.passive.} =
  try:
    # every status arrives as a number, with its body
    for code in [403, 401, 404, 500]:
      var r = httpRequest("GET", url("/code/" & $code))
      show("[1." & $code & "]", r)

    var r2 = httpRequest("GET", url("/moved"))
    show("[2] 301 absolute path:", r2)
    var r3 = httpRequest("GET", url("/rel/a/b"))
    show("[3] 302 relative:", r3)
    var r4 = httpRequest("GET", url("/abs"))
    show("[4] 302 absolute URL:", r4)

    var form = HttpHeaders("")
    form.add "Content-Type", "application/x-www-form-urlencoded"
    var r5 = httpRequest("POST", url("/see-other"), "x=1", form)
    show("[5] POST 303:", r5)
    var r6 = httpRequest("POST", url("/temp"), "x=1", form)
    show("[6] POST 307:", r6)

    var r7 = httpRequest("GET", url("/loop"), maxRedirects = 3)
    show("[7] loop, limit 3:", r7)
    var r8 = httpRequest("GET", url("/moved"), maxRedirects = 0)
    log.add "[8] maxRedirects 0: " & $r8.code & " Location " &
            r8.headers.get("Location") & "\n"
    var r9 = httpRequest("GET", url("/nolocation"))
    show("[9] no Location:", r9)

    var r10 = httpRequest("HEAD", url("/"))
    show("[10] HEAD:", r10)

    # the deadline bounds the head; the body starts with `never`
    var r11b = httpRequest("GET", url("/"), dl = afterMs(5000))
    let unbounded = r11b.body.deadline == never
    log.add "[11] body deadline never: " & $unbounded & " [" &
            r11b.body.readAll().strip() & "]\n"

    try:
      var r11 = httpRequest("GET", url("/slow"), dl = afterMs(300))
      log.add "[11] no error: " & $r11.code & "\n"
    except ErrorCode as e:
      log.add "[11] deadline: " & $e & "\n"

    var cred = HttpHeaders("")
    cred.add "Authorization", "Bearer t"
    cred.add "Cookie", "s=1"
    var r12 = httpRequest("GET", url("/same"), headers = cred)
    show("[12] same origin:", r12)
    var r13 = httpRequest("GET", url("/cross"), headers = cred)
    log.add "[13] cross origin: " & $r13.code & " " &
            r13.body.readAll().strip() & "\n"

    # the raising API is unchanged, and takes a deadline too
    try:
      var g = httpGet(url("/code/403"), dl = afterMs(5000))
      log.add "[14] no error: " & g.readAll() & "\n"
    except ErrorCode as e:
      log.add "[14] httpGet 403: " & $e & "\n"
    try:
      discard httpRequest("GE T", url("/"))
      log.add "[15] no error\n"
    except ErrorCode as e:
      log.add "[15] bad method: " & $e & "\n"
  except ErrorCode as e:
    log.add "CLIENT FAILED: " & $e & "\n"
  # the sentinel: the server stops after accepting it
  atomicStore(stopping, 1, moRelease)
  try:
    var last = httpRequest("GET", url("/"))
    discard last.body.readAll()
  except ErrorCode as e:
    log.add "SENTINEL FAILED: " & $e & "\n"
  atomicStore(clientDone, 1, moRelease)

proc main =
  initIoRing()
  submit(delay(serverChain()), 0)
  let start = monoNow()
  while atomicLoad(listening, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 5000: break
  assert atomicLoad(listening, moAcquire) == 1, "the listener never came up"
  submit(delay(clientChain()), 1)
  awaitFlag(clientDone)
  awaitFlag(serverDone)
  stdout.write log

main()

{.feature: "assumeSync".}  # test program: globals shared freely

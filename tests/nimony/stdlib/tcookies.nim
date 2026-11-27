import std/[assertions, cookies, options, strtabs, strutils, syncio, times]

proc main() {.raises.} =
  block: # date formatting and the Time/DateTime overloads
    let expire = fromUnix(0) + initDuration(seconds = 1)
    let expected = "Set-Cookie: test=value; Expires=Thu, 01 Jan 1970 00:00:01 GMT"
    assert setCookie("test", "value", expire) == expected
    assert setCookie("test", "value", expire.local) == expected
    assert setCookie("test", "value", expire.utc) == expected
    assert setCookie("test", "value", fromUnix(0).utc) ==
      "Set-Cookie: test=value; Expires=Thu, 01 Jan 1970 00:00:00 GMT"

  block: # parsing Cookie headers
    let table = parseCookies("uid=1; kp=2")
    assert table["uid"] == "1"
    assert table["kp"] == "2"
    assert table["UID"] == "1" # cookie names are case insensitive

    let spaced = parseCookies("  a=1;\tb=; c=3")
    assert spaced["a"] == "1"
    assert spaced["b"] == ""
    assert spaced["c"] == "3"
    assert parseCookies("").len == 0
    assert parseCookies("novalue").len == 0
    assert parseCookies("a=1; a=2")["a"] == "2"

  block: # Set-Cookie attributes
    assert setCookie("k", "v") == "Set-Cookie: k=v"
    assert setCookie("k", "v", noName = true) == "k=v"
    assert setCookie("k", "v", domain = "example.com", path = "/app",
      expires = "Wed, 21 Oct 2015 07:28:00 GMT", secure = true,
      httpOnly = true, maxAge = some(3600), sameSite = SameSite.Lax) ==
      "Set-Cookie: k=v; Domain=example.com; Path=/app; " &
      "Expires=Wed, 21 Oct 2015 07:28:00 GMT; Secure; HttpOnly; " &
      "Max-Age=3600; SameSite=Lax"
    assert setCookie("k", "v", secure = true,
      sameSite = SameSite.None).contains("; SameSite=None")
    assert setCookie("k", "v", sameSite = SameSite.Strict).contains(
      "; SameSite=Strict")
    assert not setCookie("k", "v").contains("SameSite")
    assert $SameSite.Strict == "Strict"

try:
  main()
except:
  quit "tcookies: unexpected exception"

echo "cookies: OK"

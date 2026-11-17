import std/[assertions, mimetypes, syncio]

block:
  var m = newMimetypes()
  assert m.getMimetype("mp4") == "video/mp4"
  assert m.getMimetype(".mp4") == "video/mp4"
  assert m.getMimetype("MP4") == "video/mp4"
  assert m.getMimetype(".MP4") == "video/mp4"
  assert m.getMimetype("json") == "application/json"
  assert m.getMimetype("nim") == "text/nim"
  assert m.getMimetype("nimble") == "text/nimble"
  assert m.getMimetype("nimf") == "text/nim"
  assert m.getMimetype("nims") == "text/nim"

  assert m.getExt("application/json") == "json"
  assert m.getExt("text/html") == "html"
  assert m.getExt("TEXT/HTML") == "html"
  assert m.getExt("text/plain") == "txt"

  assert m.getMimetype("") == "text/plain"
  assert m.getMimetype("INVALID") == "text/plain"
  assert m.getMimetype("invalid", "application/octet-stream") ==
    "application/octet-stream"
  assert m.getExt("") == "txt"
  assert m.getExt("INVALID/NONEXISTENT") == "txt"
  assert m.getExt("invalid/nonexistent", "bin") == "bin"

block:
  var a = newMimetypes()
  var b = newMimetypes()
  a.register(ext = "fakext", mimetype = "text/fakelang")
  assert a.getMimetype("fakext") == "text/fakelang"
  assert a.getMimetype("FaKeXT") == "text/fakelang"
  assert a.getExt("TEXT/FAKELANG") == "fakext"
  assert b.getMimetype("fakext") == "text/plain"

  a.register(ext = "fakext", mimetype = "application/x-fake")
  assert a.getMimetype("fakext") == "application/x-fake"

block:
  let empty: MimeDB = MimeDB()
  assert empty.getMimetype("txt") == "text/plain"
  assert empty.getExt("text/plain") == "txt"

assert mimes.len > 500
assert mimes[0][0] == "ez"
assert mimes[0][1] == "application/andrew-inset"

echo "mimetypes: ok"

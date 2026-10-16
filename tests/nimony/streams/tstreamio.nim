import std/[syncio, streamio, paths, dirs]
import std/compress/gzip

proc main() {.raises.} =
  # a string as a Reader
  var r = openReader("first\r\nsecond\nlast")
  echo r.readLine()
  echo r.readLine()
  echo r.readLine()
  echo r.readLine().len         # past the end

  # an in-memory Writer
  var w = openWriter()
  w.write "abc"
  w.write "def"
  echo extract(w)

  # gzip both ways through the adapter slot
  var text = ""
  for i in 0 ..< 200: text.add "line " & $i & "\n"
  var gw = openWriter()
  gw.setAdapter gzipAdapter(fmtGzip)
  gw.write text
  let gz = extract(gw)
  echo gz.len < text.len, " ", uncompress(gz) == text
  var gr = openReader(gz)
  gr.setAdapter gzipAdapter(fmtDetect)
  echo gr.readAll() == text

  # a truncated stream is an error, not a short read
  var cut = openReader(gz.substr(0, gz.len div 2))
  cut.setAdapter gzipAdapter(fmtDetect)
  try:
    discard cut.readAll()
    echo "no error?"
  except ErrorCode as e:
    echo "truncated: ", e

  # files: write, append, read back, copy into memory
  let p = path"tstreamio_tmp.txt"
  var fw = openWriter(p)
  fw.write "one\n"
  fw.close()
  var fa = openAppending(p)
  fa.write "two\n"
  fa.close()
  var fr = openReader(p)
  var mem = openWriter()
  echo copy(mem, fr)
  fr.close()                    # Windows cannot delete a file that is open
  echo extract(mem)

  # a gzip file written through a destructor, no explicit close
  let pz = path"tstreamio_tmp.gz"
  block:
    var zw = openWriter(pz)
    zw.setAdapter gzipAdapter(fmtGzip)
    zw.write text
  var zr = openReader(pz)
  zr.setAdapter gzipAdapter(fmtDetect)
  echo zr.readAll() == text
  zr.close()

  # direction is checked
  try:
    gr.setAdapter gzipAdapter(fmtDetect)
    var bad = openWriter()
    bad.setAdapter gzipAdapter(fmtDetect)
    echo "no error?"
  except ErrorCode as e:
    echo "write with fmtDetect: ", e
  removeFile(p)
  removeFile(pz)

try:
  main()
except ErrorCode as e:
  echo "FAILED: ", e

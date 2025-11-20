import std/[syncio, streams, assertions]


block tstreams:
  var outp = newFileStream(stdout)
  try:
    write(outp, "Hello! What is your name?\n")
  except:
    discard

block tstreams3:
  try:
    var fs = openFileStream("shouldneverexist.txt")
  except:
    echo "threw exception"

# block:
#   var a = newStringStream "hehohihahuhyh"
#   a.readDataStrImpl = nil

#   var buffer = "_ooo_"

#   try:
#     assert a.readDataStr(buffer, 1..3) == 3
#   except:
#     assert false

#   echo buffer

# block:
#   try:
#     var ss = newStringStream("The quick brown fox jumped over the lazy dog.\nThe lazy dog ran")
#     assert(ss.getPosition == 0)
#     assert(ss.peekStr(5) == "The q")
#     assert(ss.getPosition == 0) # haven't moved
#     assert(ss.readStr(5) == "The q")
#     assert(ss.getPosition == 5) # did move
#     assert(ss.peekLine() == "uick brown fox jumped over the lazy dog.")
#     assert(ss.getPosition == 5) # haven't moved
#     var str = newString(100)
#     assert(ss.peekLine(str))
#     assert(str == "uick brown fox jumped over the lazy dog.")
#     assert(ss.getPosition == 5) # haven't moved
#     # bug #19707 - Ensure we dont error with writing over literals on arc/orc
#     ss.setPosition(0)
#     ss.write("hello")
#     ss.setPosition(0)
#     assert(ss.peekStr(5) == "hello")
#   except:
#     discard

proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

proc print(s: cstring) =
  printf("%s\n", s)

proc print(n: int) =
  printf("%ld\n", n)

proc print(s1, s2: cstring) =
  printf("%s: %s\n", s1, s2)

proc print(s1: cstring, n: int) =
  printf("%s: %ld\n", s1, n)

proc print(s1: cstring, b: bool) =
  let bs = if b: cstring"true" else: cstring"false"
  printf("%s: %s\n", s1, bs)

type Obj = object
  a, b: int
  c: cstring

var o = Obj(a: 1, b: 2, c: "xyz")
for f in fields(o):
  print f

for name, f in fieldPairs(o):
  print name, f

var o2 = Obj(a: 3, b: 4, c: "uvw")
for f1, f2 in fields(o, o2):
  print "o", f1
  print "o2", f2

for name, f1, f2 in fieldPairs(o, o2):
  var name1 = "o " & name
  print toCString(name1), f1
  var name2 = "o2 " & name
  print toCString(name2), f2

# tuple:
var tup = (x: cstring"abc", y: 123, z: true)
for name, f in fieldPairs(tup):
  print name, f

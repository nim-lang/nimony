# An undeclared call in the `for` head must report the undeclared identifier
# instead of the implicit-iterator fallback's "cannot call expression of type
# auto" (nim-lang/nimony#2553).

for line in split(""):
  discard

for line in "".split():
  discard

for line in split "":
  discard

for line in nosuch(1)(2):
  discard

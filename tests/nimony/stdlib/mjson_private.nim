# For tjson: an object whose fields are private to this module.
type Secret* = object
  id*: int
  hidden: string

proc initSecret*(id: int; hidden: string): Secret = Secret(id: id, hidden: hidden)
proc hidden*(s: Secret): string = s.hidden

# A `static` parameter only restricts the call site: the routine is not
# specialized per value, so the body cannot fold it. The diagnostic points at
# the generic value parameter form, which is the compile-time value.

proc pick(backward: static[bool]): string =
  when backward: "back" else: "fwd"

discard pick(true)

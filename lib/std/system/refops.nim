# Manual reference counting, on top of whatever the selected memory management
# strategy (`include "$MM"`) defines `=dup` and `=destroy` to do. Strategy
# independent, hence its own module rather than a copy in every strategy.

proc GC_ref*[T](x: ref T) {.nodestroy, inline.} =
  ## Manually increments the reference count of `x`. Pairs with `GC_unref`.
  ## For raw `alloc`'d memory that holds `ref` fields ARC cannot trace, this
  ## keeps the referenced object alive past the lifetime of the `ref` variable.
  ## `nodestroy` is essential: it suppresses the destructor the compiler would
  ## otherwise inject on the `=dup` result, so the extra reference deliberately
  ## leaks (no-op when `x` is nil — the `=dup` hook guards it).
  discard `=dup`(x)

proc GC_unref*[T](x: ref T) {.inline.} =
  ## Manually decrements the reference count of `x`, freeing it at zero.
  ## Inverse of `GC_ref`. `x` is a borrowed (non-`sink`) parameter, so the
  ## only effect is the `=destroy` hook's `arcDec` (nil-safe).
  `=destroy`(x)

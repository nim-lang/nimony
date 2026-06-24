import std/assertions

block: # int clamp
  assert clamp(5, 1, 10) == 5
  assert clamp(-3, 1, 10) == 1
  assert clamp(99, 1, 10) == 10
  assert clamp(1, 1, 10) == 1    # lower boundary
  assert clamp(10, 1, 10) == 10  # upper boundary

block: # float clamp
  assert clamp(1.4, 0.0, 1.0) == 1.0
  assert clamp(0.5, 0.0, 1.0) == 0.5
  assert clamp(-0.2, 0.0, 1.0) == 0.0

block: # method-call syntax and a non-default integer width
  assert 200'i32.clamp(0'i32, 255'i32) == 200'i32
  assert 300'i32.clamp(0'i32, 255'i32) == 255'i32

block: # works on any Orderable, e.g. char (Ordinal -> Orderable)
  assert clamp('m', 'a', 'z') == 'm'
  assert clamp('A', 'a', 'z') == 'a'

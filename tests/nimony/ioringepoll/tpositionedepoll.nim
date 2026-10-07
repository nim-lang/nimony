# Run the shared positioned-file assertions through epoll, not io_uring.
# nimony.args selects the backend for this program and all its imports.
# .nojoin keeps that backend selection isolated from other ring tests.
when defined(linux):
  when not defined(nimIoringNoUring):
    {.error: "this regression must use the epoll backend".}
  import ../threads/tpositioned

## Regression test: syncio's `readLine` under -d:useLibc (the `fgets` path)
## gives the same lines as the native branch: no NUL terminator or stack bytes
## from `fgets`, NUL bytes in the input kept.

include "../stdlib/treadline.nim"

# v2 plugin

This is the code for the `v2` plugin that provides an import mechanism of Nim 2.0 code into Nimony.

Usage:

```nim
import (std / encodings) {.plugin: "v2".}
```

This currently requires a "Nim devel" compiler to work.

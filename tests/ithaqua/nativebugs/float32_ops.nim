# native: prints 0.0 and -1.5881868e-23. wasm: 1.5 and 0.3 (correct).
import std/syncio
echo float32(1.5)
echo $(0.1'f32 + 0.2'f32)

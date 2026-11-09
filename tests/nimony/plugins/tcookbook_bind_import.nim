import std / syncio
import deps / cookbook_binding

proc label(value: string): string =
  "caller: " & value

echo label("example")
echo boundLabel("example")

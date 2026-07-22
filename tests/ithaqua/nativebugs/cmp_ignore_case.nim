# native: compile fails "arkham x64n: scalar store rhs Undef". wasm: true / true.
import std/[syncio, strutils]
echo cmpIgnoreCase("Hello", "hello") == 0
echo cmpIgnoreCase("apple", "Banana") < 0

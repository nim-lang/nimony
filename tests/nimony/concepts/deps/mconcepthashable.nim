## Exposes a `Hashable`-constrained generic without re-exporting `std/hashes`,
## so the importer resolves `hash` purely through the concept's own module.

import std/hashes

proc sameHash*[T: Hashable](x, y: T): bool = hash(x) == hash(y)

# Generic code that never imports the container modules.
func same*[T: Equatable](x, y: T): bool = x == y

template tsame*(x, y: untyped): bool = x == y

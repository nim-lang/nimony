type Foo = distinct int
func `!=`(a, b: Foo): bool = int(a) != int(b)

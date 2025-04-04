proc internalTypeName*[T](x: typedesc[T]): string {.magic: "InternalTypeName", noSideEffect.}

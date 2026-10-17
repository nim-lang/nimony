# Imported by `bodies.nim` for the `generic-body` and `folded-body` phases of
# hastur's incremental-build regression (`hastur incremental`). Both bodies
# are edited and restored in place by `src/hastur/incrementaltests.nim`, so
# keep them as they are: the point of the phases is that neither body is
# compiled where it is written. `tag` is instantiated into the importer's
# `.s.nif`, `folded` is run at the importer's compile time.

proc tag*[T](x: T): int =
  result = 1

proc folded*(): int =
  result = 7

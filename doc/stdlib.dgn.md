# Standard library

## Interfacing with the operating system

To be written.

## Collections

### Hashes

Computing a hash value is a prerequisite for using the value as a key in a table datastructure. A table is general (key, value)-store.

@../lib/std/hashes.nim

####Hash

This integer type is used to hold hash values. A hash is also called a "checksum". Two hashes can be combined into one new hash by the `!&` operator. The result after one or many combinations should be finished with the `!$` operator.

####Hashable

A concept that describes what a type must fullfill in order to be considered hashable.


####hash

The overloaded `hash` operation returns the hash of a value.

####nextTry

To iterate over a search space of possible hash values, `nextTry` can be used.

@../lib/std/tables.nim


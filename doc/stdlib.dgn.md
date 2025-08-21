# Standard library

## Interfacing with the operating system

To be written.

## String handling

### strutils

@../lib/std/strutils.nim

#### Character sets

The module defines several sets for common character categories:

| Name | Description |
|------|-------------|
| `Whitespace` | Space, tab, vertical tab, carriage return, new line, form feed |
| `Letters` | All ASCII letters (a-z, A-Z) |
| `UppercaseLetters` | Uppercase ASCII letters (A-Z) |
| `LowercaseLetters` | Lowercase ASCII letters (a-z) |
| `PunctuationChars` | All ASCII punctuation characters |
| `Digits` | Numeric digits (0-9) |
| `HexDigits` | Hexadecimal digits (0-9, A-F, a-f) |
| `IdentChars` | Characters valid in identifiers (a-z, A-Z, 0-9, _) |
| `IdentStartChars` | Characters valid at the start of identifiers (a-z, A-Z, _) |
| `Newlines` | Newline characters (\r, \n) |
| `PrintableChars` | All printable ASCII characters |
| `AllChars` | All possible characters (0x00-0xFF) |

(These are all `const` sets of type `set[char]`.)

#### Character classification

Functions to check character properties:

####isAlphaAscii

Checks if character is alphabetical (a-z, A-Z)

####isAlphaNumeric

Checks if character is alphanumeric (a-z, A-Z, 0-9)

####isDigit
Checks if character is a digit (0-9)

####isLowerAscii

Checks if character is lowercase

####isUpperAscii

Checks if character is uppercase

####allCharsInSet

Returns true if every character in a string is in a given set

####isEmptyOrWhitespace

Checks if string is empty or consists entirely of whitespace

####startsWith

Checks if string starts with a prefix

####endsWith

Checks if string ends with a suffix

####continuesWith

Checks if string continues with a substring at a given position

####toLowerAscii

Converts character or string to lowercase

####toUpperAscii

Converts character or string to uppercase

####capitalizeAscii

Converts first character of string to uppercase

####cmpIgnoreCase

Case-insensitive string comparison

####cmpIgnoreStyle

Style-insensitive string comparison (ignores case and underscores)

####find

Searches for a character in a string within a range

####replace

Replaces characters in a string

####escape

Escapes a string with control characters and special sequences.

####unescape

Unescapes a previously escaped string.

####FloatFormatMode

An enum that specifies the format of floating point numbers.

####formatFloat

Formats floating point numbers as strings.

####formatBiggestFloat

Formats largest floating point numbers as strings.


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

### Tables

@../lib/std/tables.nim

####Keyable

A concept that describes what a type must fulfill in order to be used as a key in a table. A keyable type must have:
- An equality operator `==` that compares two values of the type
- A `hash` procedure that computes a hash value for the type

####Table

A generic hash table that stores key-value pairs. The table uses a hybrid approach:
- For small tables (≤ 4 entries), it uses linear search
- For larger tables, it uses a hash table with open addressing

####contains

Checks if a key exists in the table.

####hasKey

Alias for `contains`.

####getOrDefault

Retrieves a value for a key, returning the default value if the key is not found.

####getOrQuit

Retrieves a value for a key, quits the program if the key is not found.

####[]

Retrieves a value for a key. Raises `KeyError` if the key is not found.

####[]=

Sets a value for a key. If the key already exists, the value is updated; otherwise, a new entry is added.

####mgetOrPut

Retrieves a value for a key, or adds a new entry with the default value if the key doesn't exist. Returns a mutable reference to the value.

####len

Returns the number of key-value pairs in the table.

####pairs

Iterates over all key-value pairs in the table.

####mpairs

Iterates over all key-value pairs in the table, providing mutable access to values.

####initTable

Creates a new empty table.


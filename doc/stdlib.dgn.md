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

### unicode

@../lib/std/unicode.nim

####Rune

A distinct type that can hold a single Unicode code point. A Rune may be composed with other Runes to form a character on the screen.

####runeLen

Returns the number of runes in a string (not bytes).

####runeLenAt

Returns the number of bytes the rune starting at a given position takes.

####fastRuneAt

A template that efficiently extracts a rune from a string at a given byte position.

####runeAt

Returns the rune at a specific byte index in a string.

####validateUtf8

Returns the position of the invalid byte if the string does not hold valid UTF-8 data, otherwise returns -1.

####toUTF8

Converts a rune into its UTF-8 representation.

####add

Adds a rune to a string.

####runeOffset

Returns the byte position of a rune at a given rune position.

####runeReverseOffset

Returns the byte offset of a rune counting from the end of the string.

####runeAtPos

Returns the rune at a given rune position (not byte position).

####runeStrAtPos

Returns the rune at a given position as a UTF-8 string.

####runeSubStr

Returns a UTF-8 substring starting at a code point position with a specified number of code points.

####toLower

Converts a rune to lowercase. Works for any Unicode rune.

####toUpper

Converts a rune to uppercase. Works for any Unicode rune.

####toTitle

Converts a rune to title case.

####isLower

Returns true if a rune is lowercase.

####isUpper

Returns true if a rune is uppercase.

####isAlpha

Returns true if a rune is an alphabetic character (a letter).

####isTitle

Returns true if a rune is a Unicode titlecase code point.

####isWhiteSpace

Returns true if a rune is a Unicode whitespace code point.

####isCombining

Returns true if a rune is a Unicode combining code unit.

####isSpace

Returns true if a string contains only whitespace runes.

####toUpper

Converts a string to uppercase runes.

####toLower

Converts a string to lowercase runes.

####swapCase

Swaps the case of runes in a string.

####capitalize

Converts the first character of a string to uppercase.

####translate

Translates words in a string using a replacement procedure.

####title

Converts a string to a Unicode title (capitalizes first character of each word).

####runes

An iterator that yields runes from a string.

####utf8

An iterator that yields UTF-8 strings from a string.

####toRunes

Converts a string to a sequence of runes.

####cmpRunesIgnoreCase

Compares two UTF-8 strings case-insensitively.

####reversed

Returns the reverse of a string, interpreting it as runes.

####graphemeLen

Returns the number of bytes belonging to a byte index, including following combining code units.

####lastRune

Returns the last rune in a string and its length in bytes.

####size

Returns the number of bytes a rune takes.

####split

Splits a Unicode string into substrings using separators.

####splitWhitespace

Splits a Unicode string at whitespace runes.

####strip

Strips leading or trailing runes from a string.

####repeat

Returns a string of repeated runes.

####align

Right-aligns a Unicode string with padding.

####alignLeft

Left-aligns a Unicode string with padding.


### encodings

@../lib/std/encodings.nim

####EncodingConverter

A type that can convert between two character encodings. On Windows, this uses the Windows API; on other platforms, it uses the iconv library.

####getCurrentEncoding

Retrieves the current system encoding. On Unix systems, always returns "UTF-8". On Windows, returns either the UI code page or console code page depending on the parameter.

####open

Opens a converter that can convert from one encoding to another. Raises an error if the requested conversion cannot be fulfilled.

####close

Frees the resources held by an encoding converter.

####convert

Converts a string from one encoding to another.

####nameToCodePage

Converts an encoding name to a Windows code page number. Windows specific.

####codePageToName

Converts a Windows code page number to an encoding name. Windows specific.


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


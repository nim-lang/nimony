# Standard library

## system

@../lib/std/system.nim

The system module defines fundamental types and type classes:

### Numbers

####int
Word-sized signed integer type.

####int8
8-bit signed integer type.

####int16
16-bit signed integer type.

####int32
32-bit signed integer type.

####int64
64-bit signed integer type.

####Natural
Natural number type.

####Positive
Positive number type.

####uint
Word-sized unsigned integer type.

####uint8
8-bit unsigned integer type.

####uint16
16-bit unsigned integer type.

####uint32
32-bit unsigned integer type.

####uint64
64-bit unsigned integer type.

####float
Floating point number type.

####float32
32-bit floating point number type.

####float64
64-bit floating point number type.

### Other basic types

####char
8-bit character type.

####cstring
C-compatible string type.

####pointer
Pointer type.

####bool
Boolean type (false, true).

####string
Built-in string type.

####seq
Built-in sequence type.

####array
Built-in array type.

####tuple
Built-in tuple type.


### Type classes

####SomeSignedInt
All signed integer types.

####SomeUnsignedInt
All unsigned integer types.

####SomeInteger
All integer types.

####SomeFloat
All floating point types.

####SomeNumber
All numeric types.


### Arithmetic operators

####inc
In-place increment.

####dec
In-place decrement.

####succ
Successor function.

####pred
Predecessor function.

####+
Addition.

####-
Subtraction.

####*
Multiplication.

####div
Integer division.

####mod
Modulus for integers.

####shl
Bit shift left.

####shr
Bit shift right.

####ashr
Arithmetic shift right.

####and
Logical and bitwise AND.

####or
Logical or bitwise OR.

####xor
Logical and bitwise XOR.

####not
Logical and bitwise NOT.

### Comparison operators

####==
Equality

####!=
Inequality. This is a generic template that means you **should not** implement it for your own types. Implement only `==` instead.

####>
Greater than. This is a generic template that means you **should not** implement it for your own types. Implement only `<` instead.

####=>
Greater than or equal to. This is a generic template that means you **should not** implement it for your own types. Implement only `>=` instead.

####<
Less than.

####<=
Less than or equal to.

####is

Returns true if the object is of the given type. This is a compile-time check.

####isnot

The negation of `is`. This is a compile-time check.

####of

Returns true if the object is of the given subtype. This is a runtime check.


####contains

Set membership check.

####in

Set membership check. `a in b` is equivalent to `b contains a`.

####notin

The negation of `in`.


### Moves

####move
Move operation.

####ensureMove
Ensure move optimization.

### Compile-time utilities

####defined
Check if symbol is defined.

####declared
Check if symbol is declared.

####compiles
Check if code compiles.

####astToStr
Convert AST to string.

####isMainModule
True if this is the main module.

### Memory management

####sizeof
Get size of type.

####addr
Get address of variable.

####swap
Swap two values. This does not use and `=sink` or `=copy` or `=dup` hooks! It always swaps the values' bits. This is in line with the design of Nim's move semantics.

####allocFixed
Allocate fixed size memory.

####deallocFixed
Deallocate fixed size memory.

####copyMem
Copy memory.

####cmpMem
Compare memory.

####zeroMem
Zero memory.

####continueAfterOutOfMem
The default implementation of the "out of memory" handler. The standard library tries to handle this case gracefully.

####threadOutOfMem
Returns if the current thread ran out of memory.

####setOomHandler
Set out of memory handler. Applications can use this to handle the "out of memory" case differently: For many applications the cheapest solution is to just exit the program.


### Miscellaneous operations

####default

Returns the default value for a type.

###typeof

Returns the type of an expression

### String and sequence operations

####len
Length of a string/seq/array/etc.

####high

Highest index of a string/seq/array/etc.

####low

Lowest index of a string/seq/array/etc.

####capacity

Capacity of a string/seq/array/etc.

####add

Adds a value to a string/seq. Often also called an "append" operation.

####$
String conversion operator.

####toOpenArray

Converts a string/seq/array/etc to an open array.


####rawData

Provides an unchecked view into the underlying data of a string/seq. The data is guaranteed to be sequential in memory.

####borrowCStringUnsafe

Borrows a C string from a Nim string. This is unsafe because the Nim string may be moved or deallocated while the C string is still in use.


####setLen

Sets the length of a string/seq/array/etc. Prefer to use `grow` or `shrink` instead as they are more efficient and clearer.

####grow

Grows a `seq` by a given number of elements. The elements are default initialized.

####growUnsafe

Grows a `seq` by a given number of elements. The elements are not  initialized. Use with care!

####shrink

Shrinks a string/seq/array/etc.

####toCString

Converts a Nim string to a C string.

####prepareMutation

Prepares a string for mutation. String literals are "copy on write", so you need to call `prepareMutation` before modifying strings via `addr`.

####prepareMutationAt

Prepares the given string for mutation and returns an addressable
reference to the character at index `i`.

####&

Concatenates two strings. Prefer to use `concat` instead as it is much more efficient.

####concat

Concatenates many strings.


####substr

Returns a substring of a string.

####addEscapedChar
Add escaped character to string.

####addQuoted
Add quoted value to string.

####items

Yields all the elements of a string/seq/array/etc.

####newSeq

Creates a new `seq` with a given length. The elements are default initialized.

####newSeqOf

Creates a new `seq` with a given length. The elements are initialized with a given value.

####newSeqUninit

Creates a new `seq` with a given length. The elements are not initialized. Use with care!

####@

Creates a new `seq` from an `array`. Typically used with an array constructor, for example: `@[1, 2, 3]`.


####newString

Creates a new `string` with a given length. The elements are not initialized. Use with care!

####del

Deletes an element from a `seq` by swapping it with its last element and then setting the internal length field. Thus it is O(1) but it does change the order of the elements.

####pop

Pops the last element from a `seq` and returns it.

### Iterators

####..
Inclusive range iterator.

####..<
Exclusive range iterator.

####countdown
Countdown iterator.

####fields
Iterate over object/tuple fields.

####fieldPairs
Iterate over field name-value pairs.

####unpack

Used to iterate over varargs parameters.

### Coroutines

####Continuation
Continuation representation.

####ContinuationProc
Continuation procedure type.

####CoroutineBase
Base coroutine type.

####Scheduler
Scheduler function type.

####afterYield
Get next continuation after yield.

####delay
Delay execution and return continuation.

####advance
Single step through continuations.

####Inf

Positive infinity floating point value.

####NaN

Not a Number floating point value.

####ord

Converts a value to its ordinal value.

####abs

Returns the absolute value. That is the value of its number ignoring its sign.

####chr

Converts an integer to a character.

####isNil

Checks if a value is nil.


## Input / Output

### syncio

@../lib/std/syncio.nim

####echo
Prints arguments to stdout followed by a newline.

####File

The type representing a file handle, which is a pointer to a C FILE structure.

####FileMode

An enum that specifies the file mode when opening a file.

####stdin

Standard input file handle.

####stdout

Standard output file handle.

####stderr

Standard error file handle.

####open
Opens a file with specified mode and buffer size.

####close
Closes a file handle.

####write
Writes data to a file (overloaded for different types).

####writeLine
Writes a string followed by a newline to a file.

####readLine
Reads a line from a file into a string.

####addReadLine
Appends a line from a file to a string.

####writeBuffer
Writes raw bytes to a file.

####readBuffer
Reads raw bytes from a file.

####flushFile
Flushes file buffers to disk.

####failed
Checks if a file operation has failed.

####quit
Exits the program with a value or message.

####tryWriteFile
Attempts to write content to a file, returns success status.

## memfiles

@../lib/std/memfiles.nim

####MemFile

A type that represents a memory mapped file. Contains:
- `mem` - A pointer to the memory mapped file contents
- `size` - The size of the memory mapped file
- Platform-specific fields for Windows and POSIX systems

####open

Opens a memory mapped file with various options:
- `filename` - The file to map
- `mode` - File access mode (read/write)
- `mappedSize` - Size of the mapped region (-1 for entire file)
- `offset` - Starting offset (must be multiple of page size)
- `newFileSize` - Initial file size for new files
- `allowRemap` - Whether to keep file handles open for remapping
- `mapFlags` - Platform-specific mapping flags

####close

Closes a memory mapped file and writes any changes back to the filesystem if the file was opened with write access.


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

####isSpaceAscii

Checks if character is whitespace

####isLowerAscii

Checks if character is lowercase

####isUpperAscii

Checks if character is uppercase

####allCharsInSet

Returns true if every character in a string is in a given set

####isEmptyOrWhitespace

Checks if string is empty or consists entirely of whitespace

####split

Splits a string into substring using a group of separators

####delete

Deletes characters in a string within a range

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

####normalize

Normalizes a string

####cmpIgnoreCase

Case-insensitive string comparison

####cmpIgnoreStyle

Style-insensitive string comparison (ignores case and underscores)

####find

Searches for a character, a set of chars or a substring in a string within a range

####replace

Replaces characters or substrings in a string

####replaceWord

Replaces every words in a string

####multiReplace

Replaces multiple substrings or characters in a string

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

####%

Interpolates a format string with values.

####format

Interpolates a format string with values.

####strip

Strips leading or trailing characters.

####trimZeros

Trim trailing zeros from a formatted floating point in a string.

####formatSize

Rounds and formats bytes.

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


### wordwrap

@../lib/std/wordwrap.nim

####wrapWords

Word wraps a Unicode string to fit within a specified line width. Parameters:
- `s` - The string to wrap
- `maxLineWidth` - Maximum width of each line (default: 80)
- `splitLongWords` - Whether to split words longer than maxLineWidth (default: true)
- `seps` - Set of characters considered as separators (default: Whitespace)
- `newLine` - String to use for line breaks (default: "\n")

The function handles Unicode graphemes correctly and can either split long words or keep them intact depending on the `splitLongWords` parameter.


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

####hashIgnoreStyle

Efficient hashing of strings; style is ignored.

####hashIgnoreCase

Efficient hashing of strings; case is ignored.

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


### Sets

@../lib/std/sets.nim

####HashSet

A generic hash set data structure that stores unique values. Implemented using a hash table where values are mapped to boolean true.

####initHashSet

Creates a new empty hash set.

####incl

Adds an element to the hash set.

####excl

Removes an element from the hash set.

####contains

Checks if an element is present in the hash set.

####containsOrIncl

Checks if an element is present in the hash set, and if not, adds it. Returns true if the element was already present.

####items

An iterator that yields all elements in the hash set.


## Generic Operating System Services

### pathnorm

@../lib/std/pathnorm.nim

####PathIter

An object used to iterate over path components.

####hasNext

Checks whether the `PathIter` has another path component in the given input string.

####next

Advances a `PathIter` and returns a `(start, end)` pair representing the bounds of the next path component within the provided string.

####addNormalizePath

Low-level procedure that appends a normalized representation of `x` to the `result` string and updates the `state`.

####normalizePath

Returns a normalized path string. It collapses repeated separators, resolves `.` and `..` where possible, and returns `.` for an empty normalized path. The optional `dirSep` parameter controls the separator used in the result.

### envvars

@../lib/std/envvars.nim

Provides utilities for reading and modifying process environment variables.

####getEnv

Returns the value of the environment variable named `key`.

If the variable does not exist the `default` argument (empty string by default) is returned. Use `existsEnv` when you need to distinguish between a missing variable and an empty value.

####existsEnv

Returns true if the environment variable named `key` exists, false otherwise.

####putEnv

Sets the environment variable `key` to `val` for the current process.
On error an `OSError` is raised.

####delEnv

Removes the environment variable named `key` from the current process environment. If the variable is not present the call is a no-op. On error an `OSError` is raised.

####envPairs

Iterator yielding all environment variable pairs as tuples `(key, value)`.


### paths

@../lib/std/paths.nim

This module implements path handling.

####Path
A path object that holds the path data.

####path
Creates a new path from a string.

####`$`
String conversion for Path.

####hash
Computes hash for a Path. On a case-sensitive filesystem this is done case-sensitively otherwise case-insensitively.

####`==`
Compares two paths. On a case-sensitive filesystem this is done case-sensitively otherwise case-insensitively.

####add
Adds a path to another path.

####`/`
Joins two directory names to one. Returns normalized path concatenation of `head` and `tail`, preserving whether or not `tail` has a trailing slash (or, if tail if empty, whether head has one).

####splitPath
Splits a directory into `(head, tail)` tuple, so that `head / tail == path` (except for edge cases like "/usr").

####splitFile
Splits a filename into `(dir, name, extension)` tuple. `dir` does not end in DirSep unless it's `/`. `extension` includes the leading dot. If `path` has no extension, `ext` is the empty string. If `path` has no directory component, `dir` is the empty string. If `path` has no filename component, `name` and `ext` are empty strings.

####isAbsolute
Checks whether a given `path` is absolute. On Windows, network paths are considered absolute too.

####relativePath
Converts `path` to a path relative to `base`. The `sep` (default: DirSep) is used for the path normalizations, this can be useful to ensure the relative path only contains `'/'` so that it can be used for URL constructions. On Windows, if a root of `path` and a root of `base` are different, returns `path` as is because it is impossible to make a relative path. That means an absolute path can be returned.

####isRelativeTo
Returns true if `path` is relative to `base`.

####parentDir
Returns the parent directory of `path`. This is similar to `splitPath(path).head` when `path` doesn't end in a dir separator, but also takes care of path normalizations.

####tailDir
Returns the tail part of `path`.

####isRootDir
Checks whether a given `path` is a root directory.

####parentDirs
Iterator that walks over all parent directories of a given `path`. If `fromRoot` is true (default: false), the traversal will start from the file system root directory. If `inclusive` is true (default), the original argument will be included in the traversal. Relative paths won't be expanded by this iterator. Instead, it will traverse only the directories appearing in the relative path.

####`/../`
The same as `parentDir(head) / tail`, unless there is no parent directory. Then `head / tail` is performed instead.

####extractFilename
Extracts the filename of a given `path`. This is the same as `name & ext` from splitFile.

####lastPathPart
Like extractFilename, but ignores trailing dir separator; aka: baseName in some other languages.

####changeFileExt
Changes the file extension to `ext`. If the `filename` has no extension, `ext` will be added. If `ext` == "" then any extension is removed. `Ext` should be given without the leading `'.'`, because some filesystems may use a different character.

####addFileExt
Adds the file extension `ext` to `filename`, unless `filename` already has an extension. `Ext` should be given without the leading `'.'`, because some filesystems may use a different character.

####unixToNativePath
Converts an UNIX-like path to a native one. On an UNIX system this does nothing. Else it converts `'/'`, `'.'`, `'..'` to the appropriate things. On systems with a concept of "drives", `drive` is used to determine which drive label to use during absolute path conversion. `drive` defaults to the drive of the current working directory, and is ignored on systems that do not have a concept of "drives".

####getCurrentDir
Returns the current working directory i.e. where the built binary is run. The path returned by this proc is determined at run time.

####normalizeExe
Normalize executable name. On Windows this proc will check if an `.exe` extension needs to be added. On other platforms it does nothing.

####normalizePath
Normalize a path. Consecutive directory separators are collapsed, including directory separators at the end of the path.

####normalizePathEnd
Normalize path so that it maintains a trailing separator or not depending on the value of the `trailingSep` parameter.

####absolutePath
Returns the absolute path of `path`, rooted at `root` (which must be absolute; default: current directory). If `path` is absolute, return it, ignoring `root`.

####expandTilde
Expands `~` or a path starting with `~/` to a full path, replacing `~` with getHomeDir() (otherwise returns `path` unmodified). Windows: this is still supported despite the Windows platform not having this convention.


### dirs

@../lib/std/dirs.nim

This module implements operations for creating, removing, and iterating over directories.


####tryCreateFinalDir

Tries to create the final directory in a path. In other words, it tries to create a single new directory, not a nested one. It returns the OS's error code making it easy to distinguish between "could not create" and "already exists".

####createDir

Creates a new directory `dir`. If the directory already exists, no error is raised. This can be used to create a nested directory structure directly.

####tryRemoveFinalDir

Tries to remove the final directory in a path. In other words, it tries to remove a single directory, not a nested one. It returns the OS's error code making it easy to distinguish between "could not remove" and "does not exist".

####removeDir

Removes the directory `dir`. If the directory does not exist, no error is raised.


####tryRemoveFile

Tries to remove the file. It returns the OS's error code making it easy to distinguish between "could not remove" and "does not exist".

####removeFile

Removes the file `file`. If the file did not exist, no error is raised.


####walkDir

Walks over all entries in the directory `dir`.

Yields tuples of `(kind, path)` where `kind` is one of:
- `pcFile` - regular file
- `pcDir` - directory
- `pcLinkToFile` - symbolic link to a file
- `pcLinkToDir` - symbolic link to a directory

If `relative` is true, yields relative paths (just the filename/dirname), otherwise yields full paths.

If `checkDir` is true, raises an error if `dir` doesn't exist or isn't a directory.

Special directories "." and ".." are skipped.


####getCurrentDir
Returns the current working directory as a `Path`. Raises an error if unable to retrieve the current directory.

####setCurrentDir
Sets the current working directory to `dir`. Raises an error if the directory does not exist or lacks permissions.



### appdirs

@../lib/std/appdirs.nim

This module implements helpers for determining special directories used by apps.

####getHomeDir
Returns the home directory of the current user as a Path. Used for expanding `~` in user configuration files.

####getDataDir
Returns the data directory of the current user for applications. On non-Windows OSs, this follows the XDG Base Directory spec and uses the `XDG_DATA_HOME` environment variable if set, otherwise defaults to `~/.local/share` or `~/Library/Application Support` on macOS.

####getConfigDir
Returns the config directory of the current user for applications. On non-Windows OSs, this follows the XDG Base Directory spec and uses the `XDG_CONFIG_HOME` environment variable if set, otherwise defaults to `~/.config/`. The returned string always ends with an OS-dependent trailing slash.

####getCacheDir
Returns the cache directory of the current user for applications. Uses platform-specific environment variables:
- Windows: `LOCALAPPDATA`
- macOS: `XDG_CACHE_HOME` or `HOME/Library/Caches`
- Other: `XDG_CACHE_HOME` or `HOME/.cache`

####getCacheDir(app: Path)
Returns the cache directory for a specific application. On Windows, this is `getCacheDir() / app / "cache"`; on other platforms, `getCacheDir() / app`.

####getTempDir
Returns the temporary directory for the current user. On Windows, uses `GetTempPath`. On POSIX, checks `TMPDIR`, `TEMP`, `TMP`, and `TEMPDIR` environment variables, defaulting to `/tmp` if none are set. The implementation can be overridden with `-d:tempDir=mytempname` at compile time. Does not check if the returned path exists.



### Threads

@../lib/std/rawthreads.nim

####getThreadId

Gets the ID of the currently running thread. This uses a cached value to avoid performing a syscall. Do not assume any value range for thread IDs!

####RawThread

A type that represents a raw thread.

####create

Creates a new thread. Runs `fn(arg)` on the newly created thread. `stackSize` is the size of the stack for the thread, if its default value 0 is used, a system specific stack size is used (typically 2MB).

`pinnedToCpu` is the CPU number to pin the thread to. If it is not -1, it is attempted to run the thread on the given CPU number. If the attempt fails, the thread will run on a different CPU. The standard library offers no way to check if the thread is actually pinned to the given CPU. Also, some platforms like OSX fundamentaly do not support pinning.


####join

Joins a thread. Waits for the thread to finish.

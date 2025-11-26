# Nifmake - Nimony Build System

Nifmake is a make-like tool used by Nimony to implement parallel and incremental compilation. It can either run a dependency graph directly or translate it to a Makefile.


## Concepts

There are **two distinct roles of Inputs and Outputs**:

1. Inputs/Outputs for DAG Construction (Dependency Knowledge). These are derived from the `(do ...)` statements.
2. Inputs/Outputs as Command String Substitutions. These are specified in the `(cmd ...)` template using special tags: `(input)` and `( output)` with optional indexing.

**DON'T CONFUSE:**
`(input "file.nim")` in a `(do ...)` block → Adds "file.nim" to the node's dependency list
`(input)` in a `(cmd ...)` template → Placeholder that will be replaced with actual filenames during execution


## Usage

### Basic Usage

Create a build description file (e.g., `build.nif`):

```nif
(.nif24)
(stmts
  (cmd :nifler "bin/nifler" "--portablePaths --deps parse" (input) (output))

  (do nifler
    (input "src/main.nim")
    (output "nimcache/main.1.nif")
  )
)
```

Run the build:

```bash
nifmake run build.nif
```

### Parallel Builds

Enable parallel execution with the `-j` flag:

```bash
nifmake -j run build.nif
```

### Makefile Generation

Generate a Makefile from your build description:

```bash
nifmake makefile build.nif
```

### Command System

NIF supports reusable command definitions that can be used in multiple build rules. A command consists of:

- A name
- A sequence of tokens that can be:
  - String literals
  - Special tags: `(input)`, `(output)`, `(args)`, `(argsext)`

Example command definition:

```nif
(cmd :compile "nim" (args) "c" "--out:" (output) (input))
```

This command can be used in build rules:

```nif
(do compile
  (input "src/main.nim")
  (output "bin/main")
)
```

Inside a command the `input` and `output` tags take optional indexes `from` and `until` and an optional prefix and an optional suffix:

`(input N M)` refers to the input file(s) from index `N` to index `M`. If the indexes are left out the first input is used. An index can be negative, if so it indexes from the end, `-1` is the last entry. This means that `(input +0 -1)` covers all input files. Before the `N` there can also be a string literal which is then used as a prefix: `(input "--file:" 0 -1)` produces `--file:<input 1> --file:<input 2>...`.

After the indexes a suffix can be written: `(input 0 -1 ".nim")` produces `<input 1>.nim <input 2>.nim ...`.

`(output)`: Refers to the output file(s) from index `N` to index `M`. The indexing and the prefixes and suffixes work just like it does for `(input)`.

`(args)` stands for all arguments that are passed to the command via `(args)`. These are separated by spaces.

`(argsext)` is the file extension that the tool supports so that additional command line arguments can be provided to the tool as long as the `--base` command line option of `nifmake` is used. `(argsext)` defaults to `.args` but for linkers is typically set to `.linker.args`.


## Build File Format

Build files use the NIF syntax:

```nif
(.nif24)
(stmts
  (cmd :command_template "bin/tool" (args) (input +0 +1) (output))
  (do command_template
    (input "input_file1")
    (input "input_file2")
    (input "included_file")
    (output "output_file")
    (args "lfoobar")
    (args "-funsigned-char")
  )
  (cmd :another_command "bin/toolab" (input) (output))
  (do another_command
    (input "dependency")
    (output "another_output")
  )
)
```

### Commands

Each `do` statement defines a build rule:
- First argument: command name
- `input`: input file dependencies
- `output`: output file(s)


## Example

```nif
(.nif24)
(stmts
  (cmd :nifler "nifler" "--portablePaths --deps parse" (input) (output))
  (cmd :nimsem "nimsem" (input) (output))
  (cmd :hexer "hexer" (input) (output))
  (cmd :nifc "nifc" (input) (output))
  (cmd :gcc "-o" (output) (input) (argsext ".linker.args"))

  (do nifler
    (input "src/main.nim")
    (output "nimcache/main.1.nif")
  )
  (do nimsem
    (input "nimcache/main.1.nif")
    (output "nimcache/main.2.nif")
  )
  (do hexer
    (input "nimcache/main.2.nif")
    (output "nimcache/main.c.nif")
  )
  (do nifc
    (input "nimcache/main.c.nif")
    (output "nimcache/main.c")
  )
  (do gcc
    (input "nimcache/main.c")
    (output "nimcache/main")
  )
)
```

This creates a build pipeline: `.nim` → `.1.nif` → `.2.nif` → `.c.nif` → `.c` → executable


## Implementation

The tool implements the algorithm described in the [Tup build system paper](https://gittup.org/tup/build_system_rules_and_algorithms.pdf):

1. **Build partial DAG**: Add changed files to dependency graph
2. **Add dependencies**: Recursively add dependencies for each node
3. **Topological sort**: Order nodes for execution
4. **Execute**: Run commands in dependency order

Key features:
- Cycle detection during topological sort
- Timestamp-based incremental builds
- Variable expansion in command templates

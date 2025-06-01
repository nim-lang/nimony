# Nifmake - Nimony Build System

Nifmake is a make-like tool used by Nimony to implement parallel and incremental compilation. It can either run a dependency graph directly or translate it to a Makefile.

## Features

- **Incremental builds**: Only rebuilds files when dependencies change
- **Dependency tracking**: Automatically tracks file dependencies
- **Makefile generation**: Can generate standard Makefiles
- **Variable expansion**: Supports `$input`, `$output`, `$inputs`, `$outputs` variables
- **Cycle detection**: Detects circular dependencies in build graphs

## Usage

```bash
# Run a build directly
nifmake run build.nif

# Generate a Makefile
nifmake makefile build.nif

# Generate a Makefile with custom name
nifmake --makefile build.mk makefile build.nif

# Mark files as changed for incremental builds
nifmake --changed src/main.nim run build.nif

# Enable parallel builds (future enhancement)
nifmake --parallel run build.nif
```

## Build File Format

Build files use the NIF (Nim Intermediate Format) syntax:

```nif
(.nif24)
(stmts
  (do "command_template"
    (import "input_file1")
    (import "input_file2")
    (incl "included_file")
    (result "output_file")
  )
  (do "another_command"
    (import "dependency")
    (result "another_output")
  )
)
```

### Commands

Each `do` statement defines a build rule:
- First argument: command template with variable placeholders
- `import`: input file dependencies
- `incl`: additional input file dependencies
- `result`: output file(s)

### Variable Expansion

Command templates support these variables:
- `$input`: First input file (quoted for shell)
- `$output`: First output file (quoted for shell)
- `$inputs`: All input files (space-separated, quoted)
- `$outputs`: All output files (space-separated, quoted)

## Example

```nif
(.nif24)
(stmts
  (do "nifler $input $output"
    (import "src/main.nim")
    (result "nimcache/main.1.nif")
  )
  (do "nimsem $input $output"
    (import "nimcache/main.1.nif")
    (result "nimcache/main.2.nif")
  )
  (do "hexer $input $output"
    (import "nimcache/main.2.nif")
    (result "nimcache/main.c.nif")
  )
  (do "nifc $input $output"
    (import "nimcache/main.c.nif")
    (result "nimcache/main.c")
  )
  (do "gcc -o $output $input"
    (import "nimcache/main.c")
    (result "nimcache/main")
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
- Cross-platform shell command execution
# cobol-repl

Interactive REPL (Read-Eval-Print Loop) for exploring COBOL code.

## Features

- **Interactive parsing**: Parse COBOL code directly in the REPL
- **Program management**: Load, list, and inspect multiple COBOL programs
- **AST exploration**: View full Abstract Syntax Trees
- **Token inspection**: Tokenize code and inspect tokens
- **Command history**: Persistent command history

## Usage

```bash
cargo run --bin cobol-repl
```

### Commands

- `help` - Show help message
- `parse <code>` - Parse COBOL code
- `load <file>` - Load COBOL program from file
- `list` - List all loaded programs
- `show <name>` - Show program structure
- `ast <name>` - Show full AST for program
- `tokens <code>` - Tokenize COBOL code
- `clear` - Clear all loaded programs
- `exit` / `quit` - Exit REPL

### Example Session

```
cobol> IDENTIFICATION DIVISION. PROGRAM-ID. HELLO. PROCEDURE DIVISION. DISPLAY "Hello". STOP RUN.
✓ Parsed successfully! Program ID: HELLO
  Loaded as 'HELLO' (use 'show HELLO' to inspect)

cobol> show HELLO
Program: HELLO
  Identification Division:
    Program ID: Some("HELLO")
  Procedure Division:
    Statements: 2

cobol> tokens DISPLAY "Hello"
Tokens (3 total):
  0: Identifier("DISPLAY") at line 1:1
  1: StringLiteral("Hello") at line 1:9
  2: Period at line 1:16
```

## Status

✅ Basic functionality implemented


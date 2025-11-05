# cobol-parser

Parser for COBOL source code that converts tokens into an Abstract Syntax Tree (AST). This crate is part of the [Cobalt](https://github.com/MarsZDF/cobalt) COBOL tooling ecosystem.

## Features

- ✅ Recursive descent parser for COBOL programs
- ✅ Parses all four divisions (Identification, Environment, Data, Procedure)
- ✅ Handles statements, expressions, and data definitions
- ✅ Comprehensive error reporting with location information
- ✅ Support for basic COBOL constructs

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
cobol-parser = { path = "../cobol-parser" }
cobol-lexer = { path = "../cobol-lexer" }
cobol-ast = { path = "../cobol-ast" }
```

## Quick Start

```rust
use cobol_parser::parse_source;
use cobol_lexer::Format;

let source = r#"
   IDENTIFICATION DIVISION.
   PROGRAM-ID. HELLO-WORLD.
   PROCEDURE DIVISION.
       DISPLAY "Hello, World!".
       STOP RUN.
"#;

let program = parse_source(source, Format::FreeFormat).unwrap();
println!("Parsed program: {:?}", program);
```

## Usage

### Parsing from Tokens

```rust
use cobol_parser::parse;
use cobol_lexer::{tokenize, Format};

let tokens = tokenize(source, Format::FreeFormat).unwrap();
let program = parse(tokens).unwrap();
```

### Parsing Directly from Source

```rust
use cobol_parser::parse_source;
use cobol_lexer::Format;

let program = parse_source(source, Format::FreeFormat)?;
```

### Using the Parser Directly

```rust
use cobol_parser::Parser;
use cobol_lexer::{tokenize, Format};

let tokens = tokenize(source, Format::FreeFormat).unwrap();
let mut parser = Parser::new(tokens);
let program = parser.parse_program().unwrap();
```

## Supported COBOL Features

### Divisions
- ✅ IDENTIFICATION DIVISION
- ✅ ENVIRONMENT DIVISION (basic support)
- ✅ DATA DIVISION (WORKING-STORAGE SECTION)
- ✅ PROCEDURE DIVISION

### Data Definitions
- ✅ Level numbers (01-49, 66, 77, 88)
- ✅ PICTURE clauses
- ✅ VALUE clauses

### Statements
- ✅ DISPLAY
- ✅ ACCEPT
- ✅ MOVE (basic)
- ✅ COMPUTE (basic)
- ✅ IF (basic)
- ✅ STOP RUN
- ✅ EXIT
- ✅ RETURN (basic)

## Error Handling

The parser provides detailed error messages:

```rust
use cobol_parser::{parse_source, ParseError};
use cobol_lexer::Format;

match parse_source(source, Format::FreeFormat) {
    Ok(program) => {
        // Use program
    }
    Err(ParseError::UnexpectedToken { expected, found }) => {
        eprintln!("Expected {:?}, found '{}' at line {}", 
                 expected, found.lexeme, found.line);
    }
    Err(e) => {
        eprintln!("Parse error: {}", e);
    }
}
```

## Status

🚧 **Early Development** - Basic parsing is implemented. More COBOL features are being added incrementally.

## Architecture

This parser uses a recursive descent parsing algorithm, which:
- Processes tokens sequentially
- Builds AST nodes as it encounters COBOL constructs
- Provides detailed error messages at each step
- Preserves source location information throughout

## Related Projects

- `cobol-lexer` - Tokenizes COBOL source code
- `cobol-ast` - AST data structures
- `cobol-analyzer-*` - Analysis modules (coming soon)

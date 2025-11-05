# cobol-lexer

A fast, modular lexer for COBOL source code supporting both fixed-format and free-format. This crate is part of the [Cobalt](https://github.com/MarsZDF/cobalt) COBOL tooling ecosystem.

## Features

- ✅ Tokenizes COBOL source code into a stream of tokens
- ✅ Supports both fixed-format (column-based) and free-format COBOL
- ✅ Zero-copy where possible (uses references)
- ✅ Comprehensive error reporting with line and column information
- ✅ Case-insensitive keyword recognition
- ✅ Handles COBOL-specific constructs (level numbers, PICTURE clauses, etc.)
- ✅ Well-tested with comprehensive unit and integration tests
- ✅ Zero dependencies (pure Rust implementation)

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
cobol-lexer = { path = "../cobol-lexer" }
```

Or from crates.io (when published):

```toml
[dependencies]
cobol-lexer = "0.1"
```

## Quick Start

```rust
use cobol_lexer::{tokenize, Format};

let source = r#"
   IDENTIFICATION DIVISION.
   PROGRAM-ID. HELLO-WORLD.
   PROCEDURE DIVISION.
       DISPLAY "Hello, World!".
       STOP RUN.
"#;

let tokens = tokenize(source, Format::FreeFormat).unwrap();
for token in tokens {
    println!("{:?}", token);
}
```

## Usage

### Basic Tokenization

```rust
use cobol_lexer::{tokenize, Format};

let tokens = tokenize(source_code, Format::FreeFormat)?;
```

### Format Detection

```rust
use cobol_lexer::{detect_format, Format};

let format = detect_format(source_code);
let tokens = tokenize(source_code, format)?;
```

### Working with Tokens

```rust
use cobol_lexer::{tokenize, Format, TokenType};

let tokens = tokenize(source, Format::FreeFormat)?;

for token in &tokens {
    // Check token type
    match &token.token_type {
        TokenType::Identifier(name) => {
            println!("Found identifier: {}", name);
        }
        TokenType::StringLiteral(value) => {
            println!("Found string: {}", value);
        }
        TokenType::LevelNumber(level) => {
            println!("Found level number: {}", level);
        }
        _ => {}
    }
    
    // Access position information
    println!("Token at line {}, column {}", token.line, token.column);
    println!("Byte range: {}..{}", token.start, token.end);
}
```

## Token Types

The lexer recognizes the following token types:

### Keywords
- Division keywords: `IDENTIFICATION`, `DATA`, `ENVIRONMENT`, `PROCEDURE`
- Section keywords: `DIVISION`, `SECTION`, `WORKING-STORAGE`, etc.
- Statement keywords: `IF`, `ELSE`, `PERFORM`, `MOVE`, `COMPUTE`, `CALL`, `DISPLAY`, etc.
- Data definition: `PICTURE`, `PIC`, `VALUE`, `OCCURS`, `REDEFINES`

### Literals
- String literals: `"text"` or `'text'`
- Numeric literals: `123`, `456.789`, `-42`, `+100`

### Identifiers
- Variable names, paragraph names, etc. (case-insensitive)

### Operators
- Arithmetic: `+`, `-`, `*`, `/`, `**`
- Comparison: `=`, `<>`, `<`, `>`, `<=`, `>=`

### Special COBOL Constructs
- Level numbers: `01`-`49`, `66`, `77`, `88`

### Punctuation
- Period (`.`), comma (`,`), semicolon (`;`), colon (`:`), parentheses, etc.

### Comments
- Free-format: `*> comment`
- Fixed-format: `*` in column 7

## COBOL Format Support

### Free-Format
Free-format COBOL has no column restrictions. This is fully supported.

### Fixed-Format
Fixed-format COBOL follows these rules:
- Columns 1-6: Sequence numbers (optional)
- Column 7: Indicator area (`*` for comments, `-` for continuation, space/`D` for code)
- Columns 8-72: Code area
- Columns 73-80: Program identification (typically ignored)

Fixed-format support is currently in development (currently falls back to free-format).

## Error Handling

The lexer returns `Result<Vec<Token>, LexError>` where `LexError` provides detailed information:

```rust
use cobol_lexer::{tokenize, Format, LexError};

match tokenize(source, Format::FreeFormat) {
    Ok(tokens) => {
        // Process tokens
    }
    Err(LexError::UnterminatedString { line, column }) => {
        eprintln!("Unterminated string at line {}, column {}", line, column);
    }
    Err(LexError::UnexpectedChar { char, line, column }) => {
        eprintln!("Unexpected character '{}' at line {}, column {}", char, line, column);
    }
    // ... other error types
}
```

## Examples

Run the example:

```bash
cargo run --example basic_tokenize
```

## Testing

Run all tests:

```bash
cargo test
```

Run integration tests:

```bash
cargo test --test integration_tests
```

## Benchmarks

Run benchmarks (requires nightly Rust or criterion feature):

```bash
cargo bench
```

## Architecture

This crate is designed to be:
- **Small and focused**: Only lexical analysis, nothing else
- **Fast**: Should handle millions of lines efficiently
- **Accurate**: Preserves all information from source
- **Extensible**: Easy to add new token types

The lexer is designed to be consumed by parsers in other crates (e.g., `cobol-parser`).

## Contributing

Contributions are welcome! This is part of the Cobalt ecosystem. Please see the main repository for contributing guidelines.

## License

Licensed under the MIT License. See [LICENSE](LICENSE) or http://opensource.org/licenses/MIT for details.

## Status

🚧 **Early Development** - Free-format COBOL is fully supported. Fixed-format support is in progress.

## Roadmap

- [x] Free-format COBOL lexer
- [ ] Complete fixed-format COBOL lexer
- [ ] Line continuation handling
- [ ] More comprehensive keyword support
- [ ] Performance optimizations
- [ ] More detailed error messages

## Related Projects

- `cobol-ast` - AST data structures (coming soon)
- `cobol-parser` - Parser that consumes tokens from this lexer (coming soon)
- `cobol-analyzer-*` - Various analysis modules (coming soon)

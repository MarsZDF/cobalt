# Cobalt

A modular, open-source COBOL tooling ecosystem in Rust. Cobalt provides a collection of small, composable libraries that form the foundation for COBOL analysis, refactoring, and modernization tools.

## 🎯 Goals

- **Modular**: Small, focused crates that work together
- **Fast**: Built with Rust for performance
- **Composable**: Use what you need, combine as needed
- **Open Source**: MIT/Apache-2.0 licensed

## 📦 Crates

### cobol-lexer
Fast, modular lexer for COBOL source code supporting both fixed-format and free-format.

**Status**: ✅ Ready for use

[📖 Documentation](cobol-lexer/README.md)

### cobol-ast
Abstract Syntax Tree (AST) data structures for COBOL programs.

**Status**: ✅ Core structures defined

[📖 Documentation](cobol-ast/README.md)

### cobol-parser
Parses tokens into AST.

**Status**: ✅ Basic parsing implemented

[📖 Documentation](cobol-parser/README.md)

### cobol-migration-analyzer
Migration assessment tool for COBOL to cloud/microservices transformation.

**Status**: ✅ Implemented

### cobol-doc-gen
Documentation generator for COBOL programs - converts COBOL to readable specifications.

**Status**: ✅ Implemented

### cobol-analyzer-*
Additional analysis modules (coming soon).

**Status**: 🚧 Planned

## 🚀 Quick Start

### Using cobol-lexer

```toml
[dependencies]
cobol-lexer = { path = "./cobol-lexer" }
```

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
```

### Using cobol-ast

```toml
[dependencies]
cobol-ast = { path = "./cobol-ast" }
```

```rust
use cobol_ast::{Program, Statement, Span, Spanned};
// Build or manipulate AST nodes
```

## 🏗️ Architecture

```
┌─────────────┐
│ COBOL Source│
└──────┬──────┘
       │
       v
┌─────────────┐
│ cobol-lexer │ Tokenizes source code
└──────┬──────┘
       │
       v
┌─────────────┐
│cobol-parser │ Parses tokens into AST
└──────┬──────┘
       │
       v
┌─────────────┐
│  cobol-ast  │ AST data structures
└──────┬──────┘
       │
       v
┌─────────────┐
│  Analyzers  │ Static analysis, refactoring, etc.
└─────────────┘
```

## 🧪 Development

### Prerequisites

- Rust (stable, beta, or nightly)
- Cargo

### Building

```bash
# Build all crates
cd cobol-lexer && cargo build
cd cobol-ast && cargo build
```

### Testing

```bash
# Run all tests
cd cobol-lexer && cargo test
cd cobol-ast && cargo test
```

### CI/CD

We use GitHub Actions for continuous integration:

- ✅ Tests on stable, beta, and nightly Rust
- ✅ Linting with clippy and rustfmt
- ✅ Builds on Linux, Windows, and macOS
- ✅ Builds examples and documentation

## 📝 Contributing

Contributions are welcome! This project follows standard Rust conventions:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Ensure all tests pass and clippy is happy
6. Submit a pull request

## 📄 License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## 🗺️ Roadmap

- [x] cobol-lexer - Free-format COBOL lexer
- [ ] cobol-lexer - Fixed-format COBOL lexer
- [x] cobol-ast - Core AST structures
- [x] cobol-parser - Basic parser implementation
- [ ] cobol-parser - Full COBOL grammar support
- [ ] cobol-analyzer-* - Analysis modules
- [ ] Language server support
- [ ] Formatter
- [ ] Refactoring tools

## 🙏 Acknowledgments

This project aims to modernize COBOL tooling using Rust's excellent performance and safety guarantees.

# Cobalt

A modular, open-source COBOL tooling ecosystem in Rust. Cobalt provides a collection of small, composable libraries that form the foundation for COBOL analysis, refactoring, and modernization tools.

## 🎯 Goals

- **Modular**: Small, focused crates that work together
- **Fast**: Built with Rust for performance
- **Composable**: Use what you need, combine as needed
- **Open Source**: MIT/Apache-2.0 licensed
- **Production Ready**: Comprehensive error handling and testing

## 📦 Crates

### cobol-lexer
Fast, modular lexer for COBOL source code supporting both fixed-format and free-format COBOL.

**Features**:
- ✅ Free-format COBOL lexing
- ✅ Fixed-format COBOL lexing with column-based parsing
- ✅ Case-insensitive keyword recognition
- ✅ Comprehensive token types (keywords, identifiers, literals, operators, punctuation)
- ✅ Source location tracking (line, column, span)
- ✅ Error reporting with precise location information
- ✅ Supports continuation lines and comment handling

**Status**: ✅ Complete

[📖 Documentation](cobol-lexer/README.md) | [Examples](cobol-lexer/examples/)

### cobol-ast
Abstract Syntax Tree (AST) data structures for COBOL programs.

**Features**:
- ✅ Complete AST representation of all four COBOL divisions
- ✅ Data Division structures (data items, PICTURE clauses, OCCURS, etc.)
- ✅ Procedure Division statements (DISPLAY, MOVE, COMPUTE, IF, PERFORM, etc.)
- ✅ Expression trees
- ✅ Source span tracking for all nodes
- ✅ Visitor pattern for AST traversal
- ✅ Optional serialization support (serde)

**Status**: ✅ Core structures defined

[📖 Documentation](cobol-ast/README.md)

### cobol-parser
Recursive descent parser that converts tokens into a structured AST.

**Features**:
- ✅ Parses all four COBOL divisions (Identification, Environment, Data, Procedure)
- ✅ Data item definitions with PICTURE, VALUE, OCCURS clauses
- ✅ Comprehensive statement support (DISPLAY, ACCEPT, MOVE, COMPUTE, IF, EVALUATE, PERFORM, etc.)
- ✅ File operations (OPEN, CLOSE, READ, WRITE, REWRITE, DELETE)
- ✅ String manipulation (STRING, UNSTRING)
- ✅ Table operations (SEARCH, SORT)
- ✅ Complex data structures (OCCURS DEPENDING ON, REDEFINES)
- ✅ Subprogram support (CALL, LINKAGE SECTION)
- ✅ Error recovery and detailed error messages
- ✅ Handles whitespace and comments gracefully

**Status**: ✅ Comprehensive parsing implemented

[📖 Documentation](cobol-parser/README.md) | [Examples](cobol-parser/examples/)

### cobol-migration-analyzer
CLI tool for assessing COBOL systems for cloud migration and microservices transformation.

**Features**:
- ✅ Cloud readiness analysis with detailed scoring
- ✅ Microservices decomposition recommendations
- ✅ Effort estimation with resource requirements
- ✅ Technical debt assessment using real AST analysis
- ✅ Multiple cloud platform support (AWS, Azure, GCP, Hybrid, Kubernetes)
- ✅ Migration strategy recommendations (Lift-and-shift, Replatform, Refactor, Rebuild, Replace)
- ✅ Real COBOL parsing integration (no mock data)
- ✅ Executive summary generation
- ✅ Comprehensive risk assessment

**Status**: ✅ Production ready

**Usage**:
```bash
cargo run --bin cobol-migrate -- \
  --input program.cbl \
  --platform aws \
  --strategy replatform \
  --output report.json
```

### cobol-doc-gen
CLI tool that generates human-readable documentation from COBOL programs.

**Features**:
- ✅ Extracts program structure and logic from real COBOL AST
- ✅ Generates documentation in multiple formats (HTML, Markdown, JSON)
- ✅ Comprehensive complexity metrics (cyclomatic complexity, nesting depth, maintainability index)
- ✅ Cross-references and variable usage tracking
- ✅ Paragraph and section flow analysis
- ✅ PERFORM call analysis and call graphs
- ✅ Technical debt calculation
- ✅ Real COBOL parsing integration (no mock data)
- ✅ Customizable templates with security validation

**Status**: ✅ Production ready

**Usage**:
```bash
cargo run --bin cobol-doc -- \
  --input program.cbl \
  --format html \
  --output docs/ \
  --include-source \
  --include-metrics
```

## 🚀 Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/MarsZDF/cobalt.git
cd cobalt

# Build all crates
cargo build --all
```

### Using the Lexer

```rust
use cobol_lexer::{tokenize, Format};

let source = r#"
   IDENTIFICATION DIVISION.
   PROGRAM-ID. HELLO-WORLD.
   PROCEDURE DIVISION.
       DISPLAY "Hello, World!".
       STOP RUN.
"#;

let tokens = tokenize(source, Format::FreeFormat)?;
for token in tokens {
    println!("{:?} at line {}", token.token_type, token.line);
}
```

### Using the Parser

```rust
use cobol_parser::parse_source;
use cobol_ast::Program;

let source = r#"
   IDENTIFICATION DIVISION.
   PROGRAM-ID. HELLO-WORLD.
   PROCEDURE DIVISION.
       DISPLAY "Hello, World!".
       STOP RUN.
"#;

let program: Program = parse_source(source)?;
println!("Program ID: {:?}", program.identification.program_id);
```

### Complete Pipeline Example

```rust
use cobol_lexer::{tokenize, Format};
use cobol_parser::parse;
use cobol_ast::{Program, Visitor};

let source = "/* your COBOL code */";

// Step 1: Tokenize
let tokens = tokenize(source, Format::FreeFormat)?;

// Step 2: Parse
let program: Program = parse(&tokens)?;

// Step 3: Analyze (using visitor pattern)
struct MyVisitor;
impl Visitor for MyVisitor {
    // Implement visitor methods
}
```

## 🏗️ Architecture

```
┌─────────────────────┐
│   COBOL Source      │
│  (.cbl, .cob, etc.) │
└──────────┬──────────┘
           │
           v
┌─────────────────────┐
│   cobol-lexer       │ Tokenizes source code
│                     │ (free-format ✅, fixed-format ✅)
└──────────┬──────────┘
           │
           v
┌─────────────────────┐
│  cobol-parser       │ Parses tokens into AST
│                     │ (comprehensive COBOL support ✅)
└──────────┬──────────┘
           │
           v
┌─────────────────────┐
│    cobol-ast        │ AST data structures
│                     │ (with visitor pattern ✅)
└──────────┬──────────┘
           │
           ├──────────────────┬──────────────────┐
           │                  │                  │
           v                  v                  v
┌─────────────────┐ ┌──────────────────┐ ┌──────────────┐
│ cobol-migration │ │   cobol-doc-gen  │ │  cobol-      │
│   -analyzer ✅  │ │       ✅         │ │  formatter   │
└─────────────────┘ └──────────────────┘ └──────────────┘
```

## 🧪 Development

### Prerequisites

- Rust 1.70+ (stable, beta, or nightly)
- Cargo (comes with Rust)

### Building

```bash
# Build all crates
cargo build --all

# Build a specific crate
cd cobol-lexer && cargo build

# Build with optimizations
cargo build --all --release
```

### Testing

```bash
# Run all tests
cargo test --all

# Run tests for a specific crate
cd cobol-lexer && cargo test

# Run with output
cargo test --all -- --nocapture
```

### Running Examples

```bash
# Run lexer example
cd cobol-lexer && cargo run --example basic_tokenize

# Run parser example
cd cobol-parser && cargo run --example basic_parse
```

### Running CLI Tools

```bash
# Run migration analyzer
cargo run --bin cobol-migrate -- --help

# Run documentation generator
cargo run --bin cobol-doc -- --help
```

### Linting and Formatting

```bash
# Format code
cargo fmt --all

# Run clippy
cargo clippy --all -- -D warnings
```

### Benchmarks

```bash
cd cobol-lexer && cargo bench
```

## 🔧 Workspace Structure

```
cobalt/
├── Cargo.toml              # Workspace configuration
├── README.md               # This file
├── .github/
│   └── workflows/
│       └── ci.yml          # CI/CD pipeline
├── cobol-lexer/            # Lexer crate
│   ├── src/
│   ├── tests/
│   ├── examples/
│   └── benches/
├── cobol-ast/              # AST crate
│   ├── src/
│   └── tests/
├── cobol-parser/           # Parser crate
│   ├── src/
│   ├── tests/
│   └── examples/
├── cobol-migration-analyzer/  # Migration tool
│   └── src/
└── cobol-doc-gen/          # Documentation generator
    └── src/
```

## 🚦 CI/CD

We use GitHub Actions for continuous integration:

- ✅ Tests on stable, beta, and nightly Rust
- ✅ Tests on Linux, Windows, and macOS
- ✅ Linting with clippy and rustfmt
- ✅ Builds examples and documentation
- ✅ All crates tested in the pipeline

See [\`.github/workflows/ci.yml\`](.github/workflows/ci.yml) for details.

## 📝 Contributing

Contributions are welcome! This project follows standard Rust conventions:

1. Fork the repository
2. Create a feature branch (\`git checkout -b feature/amazing-feature\`)
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass (\`cargo test --all\`)
6. Run clippy and fix warnings (\`cargo clippy --all\`)
7. Format code (\`cargo fmt --all\`)
8. Update documentation as needed
9. Submit a pull request

### Development Guidelines

- Follow Rust naming conventions
- Write comprehensive tests
- Document public APIs with rustdoc
- Handle errors explicitly (use \`Result\` types)
- Keep crates focused and modular
- Use workspace dependencies where appropriate

## 🗺️ Roadmap

### Completed ✅
- [x] cobol-lexer - Complete lexer with free-format and fixed-format support
- [x] cobol-ast - Comprehensive AST structures for all COBOL constructs
- [x] cobol-parser - Full COBOL grammar support (EVALUATE, PERFORM VARYING, file I/O, string operations, etc.)
- [x] cobol-migration-analyzer - Production-ready migration assessment tool with real AST integration
- [x] cobol-doc-gen - Complete documentation generator with complexity metrics and cross-references
- [x] Security hardening - Fixed 47 unsafe operations across all crates
- [x] Parser integration - Real COBOL parsing in all analysis tools
- [x] Workspace setup and CI/CD

### In Progress 🚧
- [ ] Comprehensive testing framework with real COBOL programs
- [ ] Performance benchmarking and optimization
- [ ] Enhanced error messages and recovery strategies

### Planned 📋
- [ ] cobol-formatter - Code formatter for COBOL
- [ ] cobol-analyzer-* - Additional analysis modules
  - [ ] Code quality metrics
  - [ ] Dead code detection
  - [ ] Security vulnerability scanner
  - [ ] Complexity analyzer
- [ ] Language server support (LSP)
- [ ] Refactoring tools
- [ ] REPL for COBOL exploration
- [ ] COBOL to Rust transpiler (experimental)

## 🤝 Acknowledgments

This project aims to modernize COBOL tooling using Rust's excellent performance and safety guarantees. Special thanks to:

- The Rust community for excellent tooling and documentation
- COBOL maintainers for keeping legacy systems running
- Contributors and users of this project

## 📚 Additional Resources

- [COBOL Language Reference](https://www.ibm.com/docs/en/cobol-zos)
- [Rust Book](https://doc.rust-lang.org/book/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

## 💬 Community

- Issues: [GitHub Issues](https://github.com/MarsZDF/cobalt/issues)
- Discussions: [GitHub Discussions](https://github.com/MarsZDF/cobalt/discussions)
- Pull Requests: [GitHub Pull Requests](https://github.com/MarsZDF/cobalt/pulls)

---

**Built with ❤️ in Rust**

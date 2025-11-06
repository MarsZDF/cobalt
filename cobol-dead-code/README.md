# cobol-dead-code

Dead code detector for COBOL programs. Identifies unused variables, unused paragraphs/sections, and unreachable code.

## Features

- ✅ Control Flow Graph (CFG) construction
- ✅ Reachability analysis
- ✅ Unused variable detection
- ✅ Unused paragraph/section detection
- ✅ Unreachable statement detection
- ✅ JSON and text output formats

## Installation

```bash
cargo install --path cobol-dead-code
```

Or use it as a library in your Rust project:

```toml
[dependencies]
cobol-dead-code = { path = "../cobol-dead-code" }
```

## Usage

### Command Line

Analyze a COBOL file:

```bash
cobol-dead-code input.cbl
```

Analyze multiple files:

```bash
cobol-dead-code *.cbl
```

Output in JSON format:

```bash
cobol-dead-code --format json input.cbl
```

Show only unused variables:

```bash
cobol-dead-code --variables-only input.cbl
```

Show only unused procedures:

```bash
cobol-dead-code --procedures-only input.cbl
```

Show only unreachable code:

```bash
cobol-dead-code --unreachable-only input.cbl
```

### Library Usage

```rust
use cobol_dead_code::analyze_program;
use cobol_parser::parse_source;
use cobol_lexer::detect_format;

let source = r#"
   IDENTIFICATION DIVISION.
   PROGRAM-ID. TEST.
   DATA DIVISION.
   WORKING-STORAGE SECTION.
   01 UNUSED-VAR PIC 9(5).
   PROCEDURE DIVISION.
       DISPLAY "Hello".
       STOP RUN.
"#;

let format = detect_format(source);
let program = parse_source(source, format)?;
let report = analyze_program(program.node);

println!("Unused variables: {}", report.unused_variables.len());
for var in &report.unused_variables {
    println!("  - {} (level {})", var.name, var.level);
}
```

## Output

The tool reports:

- **Unused Variables**: Data items defined but never referenced
- **Unused Paragraphs**: Paragraphs defined but never called (via PERFORM or GO TO)
- **Unused Sections**: Sections defined but never referenced
- **Unreachable Statements**: Code that cannot be reached from entry points

## Limitations

- Dynamic calls (PERFORM with variable paragraph names) may not be fully detected
- ALTER statements create complex control flow that may not be fully analyzed
- String-based paragraph references require exact name matching
- Some false positives may occur due to COBOL's dynamic nature

## Examples

See the `examples/` directory for more usage examples.

## Contributing

Contributions are welcome! Please see the main Cobalt repository for contribution guidelines.

## License

MIT License - see LICENSE file for details.


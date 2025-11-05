# cobol-linter

Static analysis linter for COBOL code, similar to `clippy` for Rust.

## Features

- **Program structure checks**: Validates COBOL program structure
- **Code quality metrics**: Detects code smells and complexity issues
- **Best practices**: Enforces COBOL coding standards
- **Multiple output formats**: Text and JSON output
- **Severity levels**: Error, Warning, and Info levels

## Usage

```bash
# Lint a single file
cargo run --bin cobol-lint -- program.cbl

# Lint multiple files
cargo run --bin cobol-lint -- program1.cbl program2.cbl

# JSON output
cargo run --bin cobol-lint -- --format json program.cbl

# Only show warnings and errors
cargo run --bin cobol-lint -- --severity warning program.cbl
```

## Lint Rules

### Structure Checks
- `missing-program-id` - Program should have a PROGRAM-ID
- `missing-author` - Consider adding AUTHOR information
- `missing-environment-division` - Program should have Environment Division
- `missing-data-division` - Program should have Data Division

### Code Quality
- `long-identifier-name` - Identifiers longer than 30 characters
- `large-procedure-division` - Procedure Division with too many statements
- `high-complexity` - High nesting depth detected
- `deeply-nested-code` - Deeply nested code blocks
- `stop-run-not-last` - STOP RUN should typically be the last statement
- `stop-run-in-nested-context` - STOP RUN inside nested block

## Output Format

### Text Output
```
program.cbl:5:10: WARNING [missing-author] Consider adding AUTHOR information
program.cbl:10:1: WARNING [large-procedure-division] Procedure Division has 150 statements (consider splitting)

Summary: 0 errors, 2 warnings, 0 info
```

### JSON Output
```json
[
  {
    "severity": "Warning",
    "rule": "missing-author",
    "message": "Consider adding AUTHOR information",
    "line": 5,
    "column": 10,
    "file": "program.cbl"
  }
]
```

## Status

✅ Basic linting rules implemented


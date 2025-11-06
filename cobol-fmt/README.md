# cobol-fmt

Auto-formatter for COBOL source code (like `rustfmt` or `black`).

## Features

- ✅ Configurable indentation (spaces/tabs, width)
- ✅ Keyword case normalization (UPPER, lower, preserve)
- ✅ Identifier case normalization
- ✅ Spacing around operators
- ✅ Column alignment for data items
- ✅ Line length enforcement
- ✅ Comment preservation
- ✅ Division and section formatting
- ✅ Level number formatting

## Installation

```bash
cargo install --path cobol-fmt
```

Or use it as a library in your Rust project:

```toml
[dependencies]
cobol-fmt = { path = "../cobol-fmt" }
```

## Usage

### Command Line

Format a file:

```bash
cobol-fmt input.cbl
```

Format and write back to file:

```bash
cobol-fmt --write input.cbl
```

Check if files are formatted (useful for CI):

```bash
cobol-fmt --check *.cbl
```

Format with traditional COBOL style:

```bash
cobol-fmt --traditional input.cbl
```

Format with modern style:

```bash
cobol-fmt --modern input.cbl
```

Format with fixed-format style:

```bash
cobol-fmt --fixed-format input.cbl
```

Custom options:

```bash
cobol-fmt --indent-width 4 --keyword-case upper --max-line-length 80 input.cbl
```

### Library Usage

```rust
use cobol_fmt::{format_source, FormatConfig};
use cobol_lexer::detect_format;

let source = r#"
   IDENTIFICATION DIVISION.
   PROGRAM-ID. HELLO-WORLD.
   PROCEDURE DIVISION.
       DISPLAY "Hello, World!".
       STOP RUN.
"#;

let format = detect_format(source);
let config = FormatConfig::traditional();
let formatted = format_source(source, format, config)?;
println!("{}", formatted);
```

## Configuration

### FormatConfig Options

- `indent_width`: Number of spaces per indentation level (default: 4)
- `use_spaces`: Use spaces instead of tabs (default: true)
- `keyword_case`: Keyword case convention - `Upper`, `Lower`, or `Preserve` (default: `Upper`)
- `identifier_case`: Identifier case convention - `Upper`, `Lower`, or `Preserve` (default: `Preserve`)
- `space_around_operators`: Add spaces around operators (default: true)
- `align_data_items`: Align data items by level number (default: true)
- `max_line_length`: Maximum line length before wrapping (default: 80)
- `preserve_continuations`: Preserve existing line continuations (default: true)
- `format_picture`: Format PICTURE clauses (default: true)
- `indent_procedure`: Indent PROCEDURE DIVISION statements (default: true)

### Preset Styles

**Traditional Style:**
- 4-space indent
- Uppercase keywords and identifiers
- 80-character line length

**Modern Style:**
- 2-space indent
- Lowercase keywords and identifiers
- 132-character line length

**Fixed-Format Style:**
- 4-space indent
- Uppercase keywords
- 72-character line length (traditional fixed-format)

## Examples

See the `examples/` directory for more usage examples.

## Limitations

- Fixed-format COBOL support is limited (free-format is fully supported)
- Some complex formatting scenarios may require manual adjustment
- Comment preservation is best-effort
- PICTURE clause formatting is basic

## Contributing

Contributions are welcome! Please see the main Cobalt repository for contribution guidelines.

## License

MIT License - see LICENSE file for details.


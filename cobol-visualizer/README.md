# cobol-visualizer

Generate visualizations for COBOL programs (AST diagrams, control flow graphs, etc.).

## Features

- **AST visualization**: Generate Abstract Syntax Tree diagrams
- **Control flow graphs**: Visualize program execution flow
- **Multiple formats**: SVG, Mermaid, and Graphviz DOT formats
- **Dependency graphs**: (Planned) Visualize program dependencies

## Usage

```bash
# Generate AST diagram (SVG)
cargo run --bin cobol-visualizer -- program.cbl

# Generate control flow diagram
cargo run --bin cobol-visualizer -- --type flow program.cbl

# Generate Mermaid diagram
cargo run --bin cobol-visualizer -- --format mermaid program.cbl

# Generate Graphviz DOT format
cargo run --bin cobol-visualizer -- --format dot --output program.dot program.cbl
```

## Visualization Types

### AST (Abstract Syntax Tree)
Shows the hierarchical structure of the COBOL program.

```bash
cargo run --bin cobol-visualizer -- --type ast program.cbl
```

### Flow (Control Flow)
Shows the execution flow of statements in the Procedure Division.

```bash
cargo run --bin cobol-visualizer -- --type flow program.cbl
```

### Dependencies
(Planned) Shows dependencies between programs and modules.

## Output Formats

### SVG
Raster graphics format, viewable in browsers and image viewers.

### Mermaid
Text-based diagram format, can be rendered in Markdown, GitHub, etc.

### Graphviz DOT
Graph description language, can be rendered with Graphviz tools.

## Examples

Generate SVG AST diagram:
```bash
cargo run --bin cobol-visualizer -- program.cbl
# Output: program.svg
```

Generate Mermaid flow diagram:
```bash
cargo run --bin cobol-visualizer -- --type flow --format mermaid program.cbl
# Output: program.mermaid
```

## Status

✅ Basic visualization implemented (AST and Flow diagrams)


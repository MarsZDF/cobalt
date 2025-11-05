# cobol-ast

Abstract Syntax Tree (AST) data structures for COBOL programs. This crate is part of the [Cobalt](https://github.com/MarsZDF/cobalt) COBOL tooling ecosystem.

## Features

- Complete AST representation of COBOL programs
- All four divisions (Identification, Environment, Data, Procedure)
- Data definitions with PICTURE clauses, OCCURS, REDEFINES, etc.
- Procedure division statements (PERFORM, IF, MOVE, COMPUTE, etc.)
- Expression trees for arithmetic, logical, and string operations
- Source location information (spans) for all nodes
- Visitor pattern for AST traversal
- Optional serialization support (serde feature)

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
cobol-ast = { path = "../cobol-ast" }
```

## Usage

### Building an AST Programmatically

```rust
use cobol_ast::{Program, IdentificationDivision, ProcedureDivision, Statement, Span, Spanned};
use cobol_ast::statement::{DisplayStatement, DisplayOperand};
use cobol_ast::literal::Literal;

let program = Program {
    identification: Spanned::new(
        IdentificationDivision {
            program_id: Some("HELLO".to_string()),
            author: None,
            installation: None,
            date_written: None,
            date_compiled: None,
            security: None,
            remarks: None,
        },
        Span::new(1, 1, 3, 20, 0, 100),
    ),
    environment: None,
    data: None,
    procedure: Spanned::new(
        ProcedureDivision {
            using: None,
            returning: None,
            sections: vec![],
            paragraphs: vec![],
            statements: vec![
                Spanned::new(
                    Statement::Display(Spanned::new(
                        DisplayStatement {
                            operands: vec![
                                Spanned::new(
                                    DisplayOperand::Literal(Literal::String("Hello, World!".to_string())),
                                    Span::new(5, 15, 5, 29, 200, 214),
                                ),
                            ],
                        },
                        Span::new(5, 1, 5, 30, 195, 215),
                    )),
                    Span::new(5, 1, 5, 30, 195, 215),
                ),
            ],
        },
        Span::new(4, 1, 6, 10, 150, 220),
    ),
};
```

## AST Structure

### Program Structure

- `Program` - Top-level node containing all divisions
- `IdentificationDivision` - Program identification
- `EnvironmentDivision` - Configuration and I/O
- `DataDivision` - Data definitions
- `ProcedureDivision` - Executable code

### Data Definitions

- `DataItem` - Data items with level numbers, PICTURE, VALUE, etc.
- `Picture` - PICTURE clause strings
- `OccursClause` - Array/table definitions
- `Usage` - USAGE clause types

### Statements

- `Statement` - Enum of all statement types
- `DisplayStatement`, `MoveStatement`, `ComputeStatement`, etc.
- `IfStatement`, `PerformStatement`, `CallStatement`, etc.

### Expressions

- `Expression` - Expression trees
- `BinaryOp` - Binary operators (arithmetic, comparison, logical)
- `UnaryOp` - Unary operators

## Visitor Pattern

Use the `Visitor` trait to traverse the AST:

```rust
use cobol_ast::{Visitor, Program, Statement, Expression, DataItem, Spanned};

struct MyVisitor;

impl Visitor<()> for MyVisitor {
    fn visit_program(&mut self, program: &Spanned<Program>) {
        // Visit program
    }
    
    fn visit_statement(&mut self, statement: &Spanned<Statement>) {
        // Visit statements
    }
    
    fn visit_expression(&mut self, expression: &Spanned<Expression>) {
        // Visit expressions
    }
    
    fn visit_data_item(&mut self, data_item: &Spanned<DataItem>) {
        // Visit data items
    }
}
```

## Serialization

Enable the `serialize` feature for serde support:

```toml
[dependencies]
cobol-ast = { path = "../cobol-ast", features = ["serialize"] }
```

Then serialize/deserialize AST nodes:

```rust
use cobol_ast::Program;
// ... use serde_json or other serialization formats
```

## Status

🚧 **Early Development** - Core AST structures are defined and ready for use by parsers.

## Related Projects

- `cobol-lexer` - Tokenizes COBOL source code
- `cobol-parser` - Parses tokens into AST (coming soon)
- `cobol-analyzer-*` - Various analysis modules (coming soon)

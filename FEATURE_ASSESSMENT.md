# Feature Integration Difficulty Assessment

This document assesses the difficulty of integrating four new features into the Cobalt project.

## 1. Inline Validation and Linting ⭐⭐ (Easy - 2-3 days)

### Current State
- ✅ `cobol-linter` crate already exists with a `LintRule` trait
- ✅ Basic rules implemented: `StopRunRule`, `NamingConventionRule`, `ComplexityRule`, `UnusedVariableRule`
- ✅ Infrastructure for adding new rules is in place
- ✅ AST visitor pattern available for traversal

### What Needs to Be Added

#### Deprecated Syntax Detection
- **Difficulty**: Easy
- **Implementation**: Add rules checking for:
  - Deprecated COBOL keywords (e.g., `ALTER`, `GO TO`)
  - Old-style file handling (pre-2002)
  - Legacy data types
- **Effort**: 1 day
- **Location**: `cobol-linter/src/lib.rs` - add new `LintRule` implementations

#### Y2K-Style Date Issues
- **Difficulty**: Easy-Medium
- **Implementation**: Pattern matching for:
  - 2-digit year formats in date literals
  - Date arithmetic with 2-digit years
  - `PIC 9(6)` date formats (YYMMDD)
- **Effort**: 1-2 days
- **Location**: New rule in `cobol-linter/src/lib.rs`

#### COBOL 2014 Compliance
- **Difficulty**: Medium
- **Implementation**: 
  - Check for features not in COBOL 2014 standard
  - Flag vendor-specific extensions
  - Verify standard compliance
- **Effort**: 2-3 days (requires COBOL 2014 spec knowledge)
- **Location**: New compliance checker module

#### Error Recovery Integration
- **Difficulty**: Easy
- **Implementation**: 
  - Hook into parser's error recovery
  - Emit lint warnings during parsing
  - Suggest fixes for common errors
- **Effort**: 1 day
- **Location**: `cobol-parser/src/parser.rs` + `cobol-linter`

### Total Effort: 2-3 days

### Example Implementation
```rust
struct DeprecatedSyntaxRule;

impl LintRule for DeprecatedSyntaxRule {
    fn name(&self) -> &str { "deprecated-syntax" }
    
    fn check(&self, program: &Program) -> Vec<LintIssue> {
        // Check for ALTER, GO TO, etc.
    }
}

struct Y2KDateRule;

impl LintRule for Y2KDateRule {
    fn name(&self) -> &str { "y2k-date-format" }
    
    fn check(&self, program: &Program) -> Vec<LintIssue> {
        // Check for 2-digit year patterns
    }
}
```

---

## 2. Security and Compliance Scanner ⭐⭐⭐ (Medium - 5-7 days)

### Current State
- ✅ Basic security utilities in `cobol-migration-analyzer/src/security.rs` (file path validation)
- ✅ No COBOL-specific security scanning yet
- ✅ AST and parser available for analysis

### What Needs to Be Added

#### OWASP-Style Security Checks
- **Difficulty**: Medium-Hard
- **Implementation**: Detect:
  - **Buffer Overflows**: String operations without bounds checking
    - `STRING`, `UNSTRING` without proper size validation
    - `MOVE` operations that could overflow
  - **SQL Injection**: Embedded SQL with user input
    - `EXEC SQL` statements with concatenated strings
    - Dynamic SQL construction
  - **Path Traversal**: File operations with user input
    - `OPEN`, `READ`, `WRITE` with unvalidated paths
  - **Insecure Random**: Weak random number generation
  - **Hardcoded Secrets**: Passwords, keys in source code
- **Effort**: 3-4 days
- **Location**: New `cobol-security-scanner` crate or extend `cobol-linter`

#### PCI-DSS Compliance
- **Difficulty**: Medium
- **Implementation**: Check for:
  - Credit card data handling (PAN, CVV)
  - Encryption requirements
  - Access control violations
  - Audit logging requirements
- **Effort**: 2 days
- **Location**: New compliance module

#### GDPR Compliance
- **Difficulty**: Medium
- **Implementation**: Check for:
  - Personal data handling (PII detection)
  - Data retention policies
  - Right to deletion implementation
  - Data encryption requirements
- **Effort**: 2 days
- **Location**: New compliance module

#### Report Generation
- **Difficulty**: Easy
- **Implementation**: 
  - JSON/XML/HTML report formats
  - Compliance checklist generation
  - Risk scoring
- **Effort**: 1 day
- **Location**: New report generator module

### Total Effort: 5-7 days

### Example Implementation
```rust
// New crate: cobol-security-scanner
pub struct SecurityScanner {
    rules: Vec<Box<dyn SecurityRule>>,
}

pub trait SecurityRule {
    fn name(&self) -> &str;
    fn check(&self, program: &Program) -> Vec<SecurityIssue>;
}

struct BufferOverflowRule;
struct SQLInjectionRule;
struct HardcodedSecretRule;
```

---

## 3. cobol-fmt ⭐⭐ (Easy - 2-3 days)

### Current State
- ✅ Just built `ferrum-fmt` for FORTRAN (very similar!)
- ✅ Can reuse most of the architecture
- ✅ COBOL lexer and parser available

### What Needs to Be Done

#### Core Formatter
- **Difficulty**: Easy (we have a working reference!)
- **Implementation**: 
  - Adapt `ferrum-fmt` architecture for COBOL
  - Handle COBOL-specific formatting:
    - Division headers (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
    - Level numbers (01-49, 66, 77, 88)
    - PICTURE clauses alignment
    - Period terminators
    - Fixed-format column rules (if supporting fixed-format)
- **Effort**: 2 days
- **Location**: New `cobol-fmt` crate

#### COBOL-Specific Formatting Rules
- **Difficulty**: Easy-Medium
- **Implementation**:
  - Indentation for divisions and sections
  - Alignment of data items by level number
  - PICTURE clause formatting
  - Procedure division statement alignment
  - Comment preservation (`*` in column 7 for fixed-format)
- **Effort**: 1 day
- **Location**: `cobol-fmt/src/formatter.rs`

#### CLI Tool
- **Difficulty**: Easy
- **Implementation**: Similar to `ferrum-fmt` CLI
- **Effort**: 0.5 days (mostly copy-paste and adapt)

### Total Effort: 2-3 days

### Advantages
- ✅ Can copy 80% of `ferrum-fmt` code
- ✅ COBOL and FORTRAN have similar formatting needs
- ✅ Lexer/parser already exist

### Example Structure
```rust
// cobol-fmt/src/lib.rs (similar to ferrum-fmt)
pub fn format_source(source: &str, format: Format, config: FormatConfig) -> Result<String> {
    let tokens = tokenize(source, format)?;
    let ast = parse_source(source, format).ok();
    let formatter = Formatter::new(config);
    formatter.format(&tokens, ast.as_ref())
}
```

---

## 4. Dead Code Detector ⭐⭐⭐⭐ (Hard - 7-10 days)

### Current State
- ✅ AST available for analysis
- ✅ Visitor pattern for traversal
- ✅ Basic unused variable detection skeleton exists (not implemented)
- ❌ No control flow analysis yet

### What Needs to Be Built

#### Control Flow Graph (CFG) Construction
- **Difficulty**: Hard
- **Implementation**: 
  - Build CFG from COBOL statements
  - Handle COBOL-specific constructs:
    - `PERFORM` statements (various forms)
    - `GO TO` statements (unconditional jumps)
    - `ALTER` statements (dynamic GO TO)
    - Paragraph and section calls
    - `EXIT` statements
  - Handle nested control structures (IF, EVALUATE, etc.)
- **Effort**: 3-4 days
- **Location**: New `cobol-analyzer` crate or extend existing

#### Reachability Analysis
- **Difficulty**: Medium-Hard
- **Implementation**:
  - Mark entry points (PROGRAM-ID, paragraphs called from outside)
  - Traverse CFG to find unreachable code
  - Handle COBOL's paragraph-based structure
  - Detect unused paragraphs and sections
- **Effort**: 2-3 days

#### Unused Variable Detection
- **Difficulty**: Medium
- **Implementation**:
  - Track variable definitions and uses
  - Handle COBOL scoping rules
  - Detect unused data items
  - Handle REDEFINES and OCCURS
- **Effort**: 2 days

#### Unused Procedure Detection
- **Difficulty**: Medium
- **Implementation**:
  - Track paragraph/section definitions
  - Find all call sites (PERFORM, GO TO)
  - Mark unused procedures
- **Effort**: 1-2 days

### Total Effort: 7-10 days

### Challenges
- COBOL's paragraph-based structure makes CFG complex
- `PERFORM` can call paragraphs by name (string-based)
- `GO TO` and `ALTER` create complex control flow
- Need to handle both static and dynamic calls

### Example Implementation
```rust
// New crate: cobol-dead-code
pub struct DeadCodeDetector {
    cfg: ControlFlowGraph,
    entry_points: HashSet<String>,
}

impl DeadCodeDetector {
    pub fn analyze(&self, program: &Program) -> DeadCodeReport {
        // Build CFG
        // Find entry points
        // Mark reachable code
        // Report unreachable code
    }
}
```

---

## Summary Table

| Feature | Difficulty | Effort | Dependencies | Priority |
|---------|-----------|--------|--------------|----------|
| **Inline Validation/Linting** | ⭐⭐ Easy | 2-3 days | cobol-linter (exists) | High |
| **Security Scanner** | ⭐⭐⭐ Medium | 5-7 days | New crate or extend linter | High |
| **cobol-fmt** | ⭐⭐ Easy | 2-3 days | Copy from ferrum-fmt | Medium |
| **Dead Code Detector** | ⭐⭐⭐⭐ Hard | 7-10 days | New CFG analysis | Low |

## Recommended Implementation Order

1. **cobol-fmt** (2-3 days) - Quick win, high value, easy to implement
2. **Inline Validation/Linting** (2-3 days) - Extends existing work, high value
3. **Security Scanner** (5-7 days) - Important for production use
4. **Dead Code Detector** (7-10 days) - Most complex, can be done later

## Total Estimated Effort

- **Quick wins (fmt + linting)**: 4-6 days
- **Full feature set**: 16-23 days (~3-4 weeks)

## Notes

- All features can be built incrementally
- Each can be a separate crate or integrated into existing ones
- Tests and documentation will add ~20% to each estimate
- CI/CD integration is already set up, so that's minimal overhead


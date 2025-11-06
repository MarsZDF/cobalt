# Cobalt Project TODO

This document tracks planned features and improvements for the Cobalt project.

## 🔄 In Progress

- [ ] **cobol-fmt** - Auto-formatter for COBOL source code
- [ ] **Enhanced Inline Validation and Linting** - Extended linting rules

## 📋 Planned Features

### Security and Compliance Scanner
- **Priority**: High
- **Estimated Effort**: 5-7 days
- **Status**: Planned

**Features to implement:**
- [ ] OWASP-style security checks
  - [ ] Buffer overflow detection (STRING, UNSTRING, MOVE operations)
  - [ ] SQL injection detection (EXEC SQL with user input)
  - [ ] Path traversal detection (file operations with unvalidated paths)
  - [ ] Insecure random number generation
  - [ ] Hardcoded secrets detection (passwords, keys)
- [ ] PCI-DSS compliance checks
  - [ ] Credit card data handling (PAN, CVV)
  - [ ] Encryption requirements
  - [ ] Access control violations
  - [ ] Audit logging requirements
- [ ] GDPR compliance checks
  - [ ] Personal data handling (PII detection)
  - [ ] Data retention policies
  - [ ] Right to deletion implementation
  - [ ] Data encryption requirements
- [ ] Report generation
  - [ ] JSON/XML/HTML report formats
  - [ ] Compliance checklist generation
  - [ ] Risk scoring

**Implementation Notes:**
- Create new `cobol-security-scanner` crate or extend `cobol-linter`
- Use AST visitor pattern for analysis
- Integrate with existing parser and lexer

---

### Dead Code Detector
- **Priority**: Medium
- **Estimated Effort**: 7-10 days
- **Status**: Planned

**Features to implement:**
- [ ] Control Flow Graph (CFG) construction
  - [ ] Handle COBOL-specific constructs (PERFORM, GO TO, ALTER)
  - [ ] Paragraph and section call tracking
  - [ ] Nested control structures (IF, EVALUATE)
- [ ] Reachability analysis
  - [ ] Mark entry points (PROGRAM-ID, external calls)
  - [ ] Traverse CFG to find unreachable code
  - [ ] Detect unused paragraphs and sections
- [ ] Unused variable detection
  - [ ] Track variable definitions and uses
  - [ ] Handle COBOL scoping rules
  - [ ] Detect unused data items
  - [ ] Handle REDEFINES and OCCURS
- [ ] Unused procedure detection
  - [ ] Track paragraph/section definitions
  - [ ] Find all call sites (PERFORM, GO TO)
  - [ ] Mark unused procedures

**Implementation Notes:**
- Create new `cobol-dead-code` crate
- Build CFG from AST
- Handle COBOL's paragraph-based structure
- Consider both static and dynamic calls

**Challenges:**
- COBOL's paragraph-based structure makes CFG complex
- `PERFORM` can call paragraphs by name (string-based)
- `GO TO` and `ALTER` create complex control flow
- Need to handle both static and dynamic calls

---

## ✅ Completed

- [x] cobol-lexer - Tokenizer for COBOL source code
- [x] cobol-ast - AST data structures
- [x] cobol-parser - Recursive descent parser
- [x] cobol-repl - Interactive REPL
- [x] cobol-linter - Basic linting rules
- [x] cobol-visualizer - AST visualization tool
- [x] cobol-migration-analyzer - Migration assessment
- [x] cobol-doc-gen - Documentation generator
- [x] Workspace setup and CI/CD

---

## 📝 Notes

- All features can be built incrementally
- Each can be a separate crate or integrated into existing ones
- Tests and documentation will add ~20% to each estimate
- CI/CD integration is already set up

---

**Last Updated**: 2024-12-19


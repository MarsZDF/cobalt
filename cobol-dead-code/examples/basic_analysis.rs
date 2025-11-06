use cobol_dead_code::analyze_program;
use cobol_lexer::detect_format;
use cobol_parser::parse_source;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXAMPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USED-VAR PIC 9(5) VALUE 10.
       01 UNUSED-VAR PIC 9(5) VALUE 20.
       01 ANOTHER-UNUSED PIC X(10) VALUE "test".
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY USED-VAR.
           PERFORM HELPER-PARAGRAPH.
           STOP RUN.
       HELPER-PARAGRAPH.
           DISPLAY "Helper".
       UNUSED-PARAGRAPH.
           DISPLAY "Never called".
"#;

    println!("Analyzing COBOL program for dead code...\n");

    let format = detect_format(source);
    let program = parse_source(source, format)?;
    let report = analyze_program(program.node);

    println!("📊 Dead Code Analysis Results\n");
    println!("{}", "=".repeat(60));

    println!("\nUnused Variables: {}", report.unused_variables.len());
    for var in &report.unused_variables {
        println!("  - {} (level {}) - {}", var.name, var.level, var.location);
    }

    println!("\nUnused Paragraphs: {}", report.unused_paragraphs.len());
    for para in &report.unused_paragraphs {
        println!("  - {}", para);
    }

    println!("\nUnused Sections: {}", report.unused_sections.len());
    for section in &report.unused_sections {
        println!("  - {}", section);
    }

    println!(
        "\nUnreachable Statements: {}",
        report.unreachable_statements.len()
    );
    for stmt in &report.unreachable_statements {
        println!("  - {} - {}", stmt.statement_type, stmt.location);
    }

    println!("\n{}", "=".repeat(60));

    Ok(())
}

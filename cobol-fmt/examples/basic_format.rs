use cobol_fmt::{format_source, FormatConfig};
use cobol_lexer::detect_format;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X PIC 9(5) VALUE 10.
       01 WS-Y PIC 9(5) VALUE 20.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
"#;

    println!("Original code:");
    println!("{}", source);
    println!("\n{}", "=".repeat(50));
    println!("\nFormatted with default config:\n");

    let format = detect_format(source);
    let formatted = format_source(source, format, FormatConfig::default())?;
    println!("{}", formatted);

    println!("\n{}", "=".repeat(50));
    println!("\nFormatted with traditional style:\n");

    let formatted_traditional = format_source(source, format, FormatConfig::traditional())?;
    println!("{}", formatted_traditional);

    println!("\n{}", "=".repeat(50));
    println!("\nFormatted with modern style:\n");

    let formatted_modern = format_source(source, format, FormatConfig::modern())?;
    println!("{}", formatted_modern);

    Ok(())
}

use cobol_lexer::Format;
use cobol_parser::parse_source;

fn main() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
    "#;

    println!("Parsing COBOL source...\n");
    println!("Source:\n{}", source);

    match parse_source(source, Format::FreeFormat) {
        Ok(program) => {
            println!("\n✅ Parsing successful!\n");
            println!(
                "Program ID: {:?}",
                program.node.identification.node.program_id
            );
            println!(
                "\nNumber of statements: {}",
                program.node.procedure.node.statements.len()
            );

            for (i, statement) in program.node.procedure.node.statements.iter().enumerate() {
                println!("\nStatement {}: {:?}", i + 1, statement.node);
            }
        }
        Err(e) => {
            eprintln!("\n❌ Parsing failed: {}", e);
        }
    }
}

use cobol_lexer::{tokenize, Format};

fn main() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
    "#;

    println!("Source: {:?}", source);
    match tokenize(source, Format::FreeFormat) {
        Ok(tokens) => {
            println!("Tokens count: {}", tokens.len());
            for (i, token) in tokens.iter().enumerate() {
                println!("Token {}: {:?}", i, token);
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}
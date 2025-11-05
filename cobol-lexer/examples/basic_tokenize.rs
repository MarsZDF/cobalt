use cobol_lexer::{tokenize, detect_format, Format, TokenType};

fn main() {
    // Example COBOL program
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
    "#;

    // Auto-detect format
    let format = detect_format(source);
    println!("Detected format: {:?}\n", format);

    // Tokenize the source
    match tokenize(source, Format::FreeFormat) {
        Ok(tokens) => {
            println!("Tokenized {} tokens:\n", tokens.len());
            
            for (i, token) in tokens.iter().enumerate() {
                // Skip trivial tokens (whitespace, comments) for cleaner output
                if token.is_trivial() {
                    continue;
                }
                
                println!(
                    "Token {}: {:?} at line {}, column {}",
                    i + 1, token.token_type, token.line, token.column
                );
                println!("  Lexeme: {:?}", token.lexeme);
                
                // Show some specific token type examples
                match &token.token_type {
                    TokenType::StringLiteral(s) => {
                        println!("  String value: {:?}", s);
                    }
                    TokenType::NumericLiteral(n) => {
                        println!("  Numeric value: {:?}", n);
                    }
                    TokenType::LevelNumber(lvl) => {
                        println!("  Level number: {}", lvl);
                    }
                    TokenType::Identifier(id) => {
                        println!("  Identifier: {:?}", id);
                    }
                    _ => {}
                }
                println!();
            }
            
            // Count tokens by type
            let keyword_count = tokens.iter().filter(|t| t.is_keyword()).count();
            let identifier_count = tokens.iter()
                .filter(|t| matches!(t.token_type, TokenType::Identifier(_)))
                .count();
            
            println!("\nSummary:");
            println!("  Keywords: {}", keyword_count);
            println!("  Identifiers: {}", identifier_count);
            println!("  Total tokens: {}", tokens.len());
        }
        Err(e) => {
            eprintln!("Error tokenizing: {}", e);
        }
    }
}

use cobol_lexer::{detect_format, tokenize, Format, LexError, TokenType};

#[test]
fn test_hello_world() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
    "#;

    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    // Should have tokens
    assert!(!tokens.is_empty());

    // Should identify keywords
    let keywords: Vec<_> = tokens
        .iter()
        .filter(|t| {
            matches!(
                t.token_type,
                TokenType::Identification | TokenType::Division | TokenType::Display
            )
        })
        .collect();
    assert!(!keywords.is_empty());

    // Should identify string literal
    let string_literal = tokens
        .iter()
        .find(|t| matches!(t.token_type, TokenType::StringLiteral(_)));
    assert!(string_literal.is_some());

    if let Some(token) = string_literal {
        if let TokenType::StringLiteral(s) = &token.token_type {
            assert_eq!(s, "Hello, World!");
        }
    }
}

#[test]
fn test_keywords() {
    let source = "IF PERFORM MOVE COMPUTE CALL DISPLAY";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    let keywords: Vec<_> = tokens.iter().filter(|t| t.is_keyword()).collect();

    assert_eq!(keywords.len(), 6);
}

#[test]
fn test_identifiers() {
    let source = "HELLO-WORLD my-variable COUNTER_123";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    let identifiers: Vec<_> = tokens
        .iter()
        .filter(|t| matches!(t.token_type, TokenType::Identifier(_)))
        .collect();

    assert_eq!(identifiers.len(), 3);
}

#[test]
fn test_numeric_literals() {
    let source = "123 456.789 -42 +100";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    let numbers: Vec<_> = tokens
        .iter()
        .filter(|t| matches!(t.token_type, TokenType::NumericLiteral(_)))
        .collect();

    assert!(!numbers.is_empty());
}

#[test]
fn test_level_numbers() {
    let source = "01 05 10 49 66 77 88";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    let levels: Vec<_> = tokens
        .iter()
        .filter(|t| matches!(t.token_type, TokenType::LevelNumber(_)))
        .collect();

    assert_eq!(levels.len(), 7);

    // Check specific level numbers
    if let TokenType::LevelNumber(1) = levels[0].token_type {
        // OK
    } else {
        panic!("Expected level 01");
    }
}

#[test]
fn test_string_literals() {
    let source = r#"DISPLAY "Hello" DISPLAY 'World'"#;
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    let strings: Vec<_> = tokens
        .iter()
        .filter(|t| matches!(t.token_type, TokenType::StringLiteral(_)))
        .collect();

    assert_eq!(strings.len(), 2);
}

#[test]
fn test_operators() {
    let source = "= <> < > <= >= + - * /";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    let operators: Vec<_> = tokens
        .iter()
        .filter(|t| {
            matches!(
                t.token_type,
                TokenType::Equals
                    | TokenType::NotEquals
                    | TokenType::LessThan
                    | TokenType::GreaterThan
                    | TokenType::LessOrEqual
                    | TokenType::GreaterOrEqual
                    | TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Multiply
                    | TokenType::Divide
            )
        })
        .collect();

    assert_eq!(operators.len(), 10);
}

#[test]
fn test_punctuation() {
    let source = ". , ; : ( )";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    let punct: Vec<_> = tokens
        .iter()
        .filter(|t| {
            matches!(
                t.token_type,
                TokenType::Period
                    | TokenType::Comma
                    | TokenType::Semicolon
                    | TokenType::Colon
                    | TokenType::LeftParen
                    | TokenType::RightParen
            )
        })
        .collect();

    assert_eq!(punct.len(), 6);
}

#[test]
fn test_comments() {
    let source = "*> This is a comment\n*> Another comment";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    let comments: Vec<_> = tokens
        .iter()
        .filter(|t| matches!(t.token_type, TokenType::Comment(_)))
        .collect();

    assert_eq!(comments.len(), 2);
}

#[test]
fn test_unterminated_string() {
    let source = r#"DISPLAY "Hello World"#;
    let result = tokenize(source, Format::FreeFormat);

    assert!(result.is_err());
    assert!(matches!(
        result.unwrap_err(),
        LexError::UnterminatedString { .. }
    ));
}

#[test]
fn test_format_detection_free() {
    let source = "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.";
    assert_eq!(detect_format(source), Format::FreeFormat);
}

#[test]
fn test_format_detection_fixed() {
    let source = "000100 IDENTIFICATION DIVISION.\n000200 PROGRAM-ID. HELLO.";
    assert_eq!(detect_format(source), Format::FixedFormat);
}

#[test]
fn test_case_insensitive_keywords() {
    let sources = vec![
        "IDENTIFICATION",
        "identification",
        "Identification",
        "IDENTIFICATION",
    ];

    for source in sources {
        let tokens = tokenize(source, Format::FreeFormat).unwrap();
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Identification)));
    }
}

#[test]
fn test_program_id_with_period() {
    let source = "PROGRAM-ID. HELLO-WORLD.";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();

    // Should recognize PROGRAM-ID as a keyword even with period
    let program_id = tokens
        .iter()
        .find(|t| matches!(t.token_type, TokenType::ProgramId));

    assert!(program_id.is_some());
}

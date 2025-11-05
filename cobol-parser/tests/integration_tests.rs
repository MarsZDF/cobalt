use cobol_parser::parse_source;
use cobol_lexer::Format;

#[test]
fn test_parse_hello_world() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
    "#;

    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok(), "Parsing failed: {:?}", result.err());
    
    let program = result.unwrap();
    assert_eq!(program.node.identification.node.program_id, Some("HELLO-WORLD".to_string()));
    assert!(program.node.procedure.node.statements.len() > 0);
}

#[test]
fn test_parse_with_data_division() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9(5) VALUE 0.
       PROCEDURE DIVISION.
           DISPLAY COUNTER.
           STOP RUN.
    "#;

    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok(), "Parsing failed: {:?}", result.err());
    
    let program = result.unwrap();
    assert!(program.node.data.is_some());
    
    if let Some(data_div) = &program.node.data {
        if let Some(ws) = &data_div.node.working_storage_section {
            assert!(!ws.node.data_items.is_empty());
        }
    }
}

#[test]
fn test_parse_display_statement() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           DISPLAY "Hello".
           DISPLAY "World".
           STOP RUN.
    "#;

    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok());
    
    let program = result.unwrap();
    let statements = &program.node.procedure.node.statements;
    
    // Should have at least DISPLAY and STOP RUN statements
    assert!(statements.len() >= 2);
}

#[test]
fn test_parse_accept_statement() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           ACCEPT INPUT-VALUE.
           DISPLAY INPUT-VALUE.
           STOP RUN.
    "#;

    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok());
}

#[test]
fn test_parse_stop_run() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           STOP RUN.
    "#;

    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok());
}

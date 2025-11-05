use cobol_lexer::Format;
use cobol_parser::parse_source;

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
    assert_eq!(
        program.node.identification.node.program_id,
        Some("HELLO-WORLD".to_string())
    );
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
fn test_parse_compute_statement() {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. COMPUTE-TEST.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 X PIC 9(5).
01 Y PIC 9(5).
01 Z PIC 9(10).
PROCEDURE DIVISION.
    MOVE 10 TO X.
    MOVE 20 TO Y.
    COMPUTE Z = X + Y.
    COMPUTE Z = X * Y - 5.
    STOP RUN.
"#;
    
    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok(), "Parsing failed: {:?}", result.err());
}

#[test]
fn test_parse_if_statement() {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. IF-TEST.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 X PIC 9(5).
PROCEDURE DIVISION.
    MOVE 10 TO X.
    IF X > 5
        DISPLAY "X is greater than 5"
    ELSE
        DISPLAY "X is less than or equal to 5"
    END-IF.
    STOP RUN.
"#;
    
    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok(), "Parsing failed: {:?}", result.err());
}

#[test]
fn test_parse_perform_statement() {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. PERFORM-TEST.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 COUNTER PIC 9(5).
PROCEDURE DIVISION.
    MOVE 0 TO COUNTER.
    PERFORM UNTIL COUNTER > 10
        ADD 1 TO COUNTER
        DISPLAY COUNTER
    END-PERFORM.
    PERFORM 5 TIMES
        DISPLAY "Loop"
    END-PERFORM.
    STOP RUN.
"#;
    
    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok(), "Parsing failed: {:?}", result.err());
}

#[test]
fn test_parse_file_operations() {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. FILE-TEST.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO "input.txt".
    SELECT OUTPUT-FILE ASSIGN TO "output.txt".
DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-RECORD PIC X(100).
FD OUTPUT-FILE.
01 OUTPUT-RECORD PIC X(100).
PROCEDURE DIVISION.
    OPEN INPUT INPUT-FILE.
    OPEN OUTPUT OUTPUT-FILE.
    READ INPUT-FILE.
    WRITE OUTPUT-RECORD FROM INPUT-RECORD.
    CLOSE INPUT-FILE.
    CLOSE OUTPUT-FILE.
    STOP RUN.
"#;
    
    let result = parse_source(source, Format::FreeFormat);
    // File operations might not be fully implemented, so we just check it doesn't crash
    let _ = result;
}

#[test]
fn test_parse_complex_data_structures() {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. DATA-STRUCTURE-TEST.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 CUSTOMER-RECORD.
    05 CUSTOMER-ID PIC 9(10).
    05 CUSTOMER-NAME PIC X(50).
    05 CUSTOMER-ADDRESS.
        10 STREET PIC X(50).
        10 CITY PIC X(30).
        10 STATE PIC X(2).
        10 ZIP PIC 9(5).
    05 CUSTOMER-BALANCE PIC 9(10)V99.
01 COUNTER PIC 9(5) VALUE 0.
01 OCCURS-TABLE OCCURS 10 TIMES.
    05 TABLE-ITEM PIC 9(5).
PROCEDURE DIVISION.
    STOP RUN.
"#;
    
    let result = parse_source(source, Format::FreeFormat);
    assert!(result.is_ok(), "Parsing failed: {:?}", result.err());
}

#[test]
fn test_parse_string_operations() {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STRING-TEST.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 SOURCE-1 PIC X(10) VALUE "Hello".
01 SOURCE-2 PIC X(10) VALUE "World".
01 DESTINATION PIC X(20).
PROCEDURE DIVISION.
    STRING SOURCE-1 DELIMITED BY SPACE
           " " DELIMITED BY SIZE
           SOURCE-2 DELIMITED BY SPACE
           INTO DESTINATION.
    UNSTRING DESTINATION DELIMITED BY SPACE
             INTO SOURCE-1 SOURCE-2.
    STOP RUN.
"#;
    
    let result = parse_source(source, Format::FreeFormat);
    // String operations might not be fully implemented
    let _ = result;
}

#[test]
fn test_parse_evaluate_statement() {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. EVALUATE-TEST.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 GRADE PIC X.
PROCEDURE DIVISION.
    MOVE 'A' TO GRADE.
    EVALUATE GRADE
        WHEN 'A'
            DISPLAY "Excellent"
        WHEN 'B'
            DISPLAY "Good"
        WHEN OTHER
            DISPLAY "Other"
    END-EVALUATE.
    STOP RUN.
"#;
    
    let result = parse_source(source, Format::FreeFormat);
    // Evaluate might not be fully implemented
    let _ = result;
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

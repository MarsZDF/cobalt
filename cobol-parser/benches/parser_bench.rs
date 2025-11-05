use cobol_lexer::Format;
use cobol_parser::parse_source;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_small_program(c: &mut Criterion) {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
PROCEDURE DIVISION.
    DISPLAY "Hello, World!".
    STOP RUN.
"#;

    c.bench_function("parse small program", |b| {
        b.iter(|| parse_source(black_box(source), Format::FreeFormat))
    });
}

fn bench_medium_program(c: &mut Criterion) {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. CALCULATE.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 COUNTER PIC 9(5).
01 RESULT PIC 9(10).
PROCEDURE DIVISION.
    MOVE 0 TO COUNTER.
    PERFORM UNTIL COUNTER > 100
        COMPUTE RESULT = RESULT + COUNTER
        ADD 1 TO COUNTER
    END-PERFORM.
    DISPLAY "Result: " RESULT.
    STOP RUN.
"#;

    c.bench_function("parse medium program", |b| {
        b.iter(|| parse_source(black_box(source), Format::FreeFormat))
    });
}

fn bench_data_division(c: &mut Criterion) {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. DATA-TEST.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 CUSTOMER-RECORD.
    05 CUSTOMER-ID PIC 9(10).
    05 CUSTOMER-NAME PIC X(50).
    05 CUSTOMER-ADDRESS PIC X(100).
    05 CUSTOMER-BALANCE PIC 9(10)V99.
01 COUNTER PIC 9(5) VALUE 0.
01 FLAG PIC X VALUE 'N'.
PROCEDURE DIVISION.
    STOP RUN.
"#;

    c.bench_function("parse data division", |b| {
        b.iter(|| parse_source(black_box(source), Format::FreeFormat))
    });
}

fn bench_statements(c: &mut Criterion) {
    let source = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. STATEMENTS.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 X PIC 9(5).
01 Y PIC 9(5).
01 Z PIC 9(10).
PROCEDURE DIVISION.
    MOVE 10 TO X.
    MOVE 20 TO Y.
    COMPUTE Z = X + Y.
    IF Z > 25
        DISPLAY "Large value"
    ELSE
        DISPLAY "Small value"
    END-IF.
    PERFORM 10 TIMES
        ADD 1 TO X
    END-PERFORM.
    STOP RUN.
"#;

    c.bench_function("parse statements", |b| {
        b.iter(|| parse_source(black_box(source), Format::FreeFormat))
    });
}

criterion_group!(
    benches,
    bench_small_program,
    bench_medium_program,
    bench_data_division,
    bench_statements
);
criterion_main!(benches);

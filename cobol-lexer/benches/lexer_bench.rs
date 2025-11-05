use criterion::{black_box, criterion_group, criterion_main, Criterion};
use cobol_lexer::{tokenize, Format};

fn bench_small_program(c: &mut Criterion) {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
    "#;

    c.bench_function("tokenize small program", |b| {
        b.iter(|| tokenize(black_box(source), Format::FreeFormat))
    });
}

fn bench_medium_program(c: &mut Criterion) {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(5) VALUE 12345.
       01 NUM2 PIC 9(5) VALUE 67890.
       01 RESULT PIC 9(6).
       PROCEDURE DIVISION.
           COMPUTE RESULT = NUM1 + NUM2
           DISPLAY "Result: " RESULT
           STOP RUN.
    "#;

    c.bench_function("tokenize medium program", |b| {
        b.iter(|| tokenize(black_box(source), Format::FreeFormat))
    });
}

fn bench_keyword_recognition(c: &mut Criterion) {
    let source = "IF PERFORM MOVE COMPUTE CALL DISPLAY ACCEPT STOP RUN EXIT RETURN";

    c.bench_function("tokenize many keywords", |b| {
        b.iter(|| tokenize(black_box(source), Format::FreeFormat))
    });
}

fn bench_string_literals(c: &mut Criterion) {
    let source = r#"DISPLAY "Hello" DISPLAY "World" DISPLAY "COBOL" DISPLAY "Lexer""#;

    c.bench_function("tokenize string literals", |b| {
        b.iter(|| tokenize(black_box(source), Format::FreeFormat))
    });
}

criterion_group!(
    benches,
    bench_small_program,
    bench_medium_program,
    bench_keyword_recognition,
    bench_string_literals
);
criterion_main!(benches);

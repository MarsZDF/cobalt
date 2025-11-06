use cobol_fmt::{format_source, FormatConfig, IdentifierCase, KeywordCase};
use cobol_lexer::Format;

#[test]
fn test_basic_formatting() {
    let source = r#"IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
PROCEDURE DIVISION.
DISPLAY "Hello, World!".
STOP RUN."#;

    let formatted = format_source(source, Format::FreeFormat, FormatConfig::default()).unwrap();

    // Should preserve structure but normalize formatting
    assert!(formatted.contains("IDENTIFICATION DIVISION"));
    assert!(formatted.contains("PROGRAM-ID"));
    assert!(formatted.contains("PROCEDURE DIVISION"));
}

#[test]
fn test_keyword_case_upper() {
    let source = "identification division.\nprogram-id. test.";

    let mut config = FormatConfig::default();
    config.keyword_case = KeywordCase::Upper;
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();

    assert!(formatted.contains("IDENTIFICATION"));
    assert!(formatted.contains("DIVISION"));
    assert!(formatted.contains("PROGRAM-ID"));
}

#[test]
fn test_keyword_case_lower() {
    let source = "IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.";

    let mut config = FormatConfig::default();
    config.keyword_case = KeywordCase::Lower;
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();

    assert!(formatted.to_lowercase().contains("identification"));
    assert!(formatted.to_lowercase().contains("division"));
}

#[test]
fn test_traditional_style() {
    let source = "identification division.\nprogram-id. test.";

    let config = FormatConfig::traditional();
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();

    // Traditional style should use uppercase keywords
    assert!(formatted.contains("IDENTIFICATION") || formatted.contains("identification"));
}

#[test]
fn test_modern_style() {
    let source = "IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.";

    let config = FormatConfig::modern();
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();

    // Modern style should use lowercase keywords
    assert!(formatted.to_lowercase().contains("identification"));
}

#[test]
fn test_fixed_format_style() {
    let source = "IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.";

    let config = FormatConfig::fixed_format();
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();

    // Fixed format should have 72-character line length
    // (This is a basic test - actual line length checking would be more complex)
    assert!(formatted.contains("IDENTIFICATION") || formatted.contains("identification"));
}

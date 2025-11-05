use crate::error::{LexError, LexResult};
use crate::fixed::FixedFormatLexer;
use crate::free::FreeFormatLexer;
use crate::token::Token;

/// COBOL source format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    /// Fixed-format COBOL (columns 1-6 sequence, 7 indicator, 8-72 code)
    FixedFormat,
    
    /// Free-format COBOL (no column restrictions)
    FreeFormat,
}

/// Main lexer interface for COBOL source code.
///
/// # Example
///
/// ```rust
/// use cobol_lexer::{tokenize, Format};
///
/// let source = r#"
/// IDENTIFICATION DIVISION.
/// PROGRAM-ID. HELLO-WORLD.
/// PROCEDURE DIVISION.
///     DISPLAY "Hello, World!".
///     STOP RUN.
/// "#;
///
/// let tokens = tokenize(source, Format::FreeFormat).unwrap();
/// for token in tokens {
///     println!("{:?}", token);
/// }
/// ```
pub fn tokenize(source: &str, format: Format) -> LexResult<Vec<Token>> {
    match format {
        Format::FixedFormat => {
            let mut lexer = FixedFormatLexer::new(source);
            lexer.tokenize()
        }
        Format::FreeFormat => {
            let mut lexer = FreeFormatLexer::new(source);
            lexer.tokenize()
        }
    }
}

/// Auto-detect the format of COBOL source code.
///
/// This is a heuristic that checks for fixed-format indicators:
/// - Presence of sequence numbers in columns 1-6
/// - Indicator characters in column 7 (*, -, D, space)
/// - Code starting at column 8
///
/// If detection is ambiguous, defaults to FreeFormat.
pub fn detect_format(source: &str) -> Format {
    let lines: Vec<&str> = source.lines().collect();
    let mut fixed_format_indicators = 0;
    
    for line in lines.iter().take(10) {
        // Skip empty lines
        if line.trim().is_empty() {
            continue;
        }
        
        // Check for sequence numbers in columns 1-6 (usually numeric)
        if line.len() >= 6 {
            let seq_area = &line[0..6];
            if seq_area.chars().all(|c| c.is_ascii_digit() || c == ' ') {
                fixed_format_indicators += 1;
            }
        }
        
        // Check for indicator in column 7
        if line.len() >= 7 {
            let indicator = line.chars().nth(6).unwrap();
            if indicator == '*' || indicator == '-' || indicator == 'D' || indicator == ' ' {
                fixed_format_indicators += 1;
            }
        }
        
        // Check if code starts at column 8
        if line.len() >= 8 {
            let col8 = line.chars().nth(7).unwrap();
            if !col8.is_whitespace() {
                fixed_format_indicators += 1;
            }
        }
    }
    
    // If we have strong indicators, assume fixed format
    if fixed_format_indicators >= 5 {
        Format::FixedFormat
    } else {
        Format::FreeFormat
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn test_free_format_tokenize() {
        let source = "IDENTIFICATION DIVISION.";
        let tokens = tokenize(source, Format::FreeFormat).unwrap();
        assert!(!tokens.is_empty());
        assert_eq!(tokens[0].token_type, TokenType::Identification);
    }

    #[test]
    fn test_detect_free_format() {
        let source = "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.";
        assert_eq!(detect_format(source), Format::FreeFormat);
    }

    #[test]
    fn test_detect_fixed_format() {
        let source = "000100 IDENTIFICATION DIVISION.\n000200 PROGRAM-ID. HELLO.";
        assert_eq!(detect_format(source), Format::FixedFormat);
    }
}

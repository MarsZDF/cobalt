use crate::error::{LexError, LexResult};
use crate::token::Token;

/// Lexer for fixed-format COBOL.
/// 
/// Fixed-format COBOL has specific column rules:
/// - Columns 1-6: Sequence numbers (optional)
/// - Column 7: Indicator area (* for comments, - for continuation, space/D for code)
/// - Columns 8-72: Code area
/// - Columns 73-80: Program identification (optional, typically ignored)
pub struct FixedFormatLexer<'a> {
    source: &'a str,
    lines: Vec<&'a str>,
    current_line: usize,
}

impl<'a> FixedFormatLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let lines: Vec<&str> = source.lines().collect();
        Self {
            source,
            lines,
            current_line: 0,
        }
    }

    /// Tokenize the entire source.
    pub fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        // TODO: Implement fixed-format lexing
        // For now, use free-format lexer as a fallback
        // This is a temporary workaround until fixed-format is fully implemented
        use crate::free::FreeFormatLexer;
        let mut free_lexer = FreeFormatLexer::new(self.source);
        free_lexer.tokenize()
    }
}

/// Check if a line is a comment line (starts with * in column 7).
fn is_comment_line(line: &str) -> bool {
    let trimmed = line.trim_start();
    trimmed.starts_with('*') && trimmed.len() >= 7 && trimmed.chars().nth(6) == Some('*')
}

/// Check if a line is a continuation line (has - in column 7).
fn is_continuation_line(line: &str) -> bool {
    line.len() >= 7 && line.chars().nth(6) == Some('-')
}

/// Extract the code area from a fixed-format line (columns 8-72).
fn extract_code_area(line: &str) -> &str {
    if line.len() < 8 {
        return "";
    }
    let end = line.len().min(72);
    &line[7..end]
}

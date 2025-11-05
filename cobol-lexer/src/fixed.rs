use crate::error::{LexError, LexResult};
use crate::token::{lookup_keyword, Token, TokenType};
use crate::free::FreeFormatLexer;

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
    position: usize,
    accumulated_line: String,
}

impl<'a> FixedFormatLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let lines: Vec<&str> = source.lines().collect();
        Self {
            source,
            lines,
            current_line: 0,
            position: 0,
            accumulated_line: String::new(),
        }
    }

    /// Tokenize the entire source.
    pub fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        let processed_source = self.preprocess_fixed_format()?;
        let mut free_lexer = FreeFormatLexer::new(&processed_source);
        free_lexer.tokenize()
    }

    /// Preprocess fixed-format COBOL into free-format for tokenization.
    fn preprocess_fixed_format(&mut self) -> LexResult<String> {
        let mut result = String::new();
        let mut continuation_mode = false;
        let mut previous_line_content = String::new();

        for (line_num, line) in self.lines.iter().enumerate() {
            let line_type = self.classify_line(line);
            
            match line_type {
                LineType::Comment => {
                    result.push_str("*> ");
                    if line.len() > 7 {
                        result.push_str(&line[7..]);
                    }
                    result.push('\n');
                    continuation_mode = false;
                }
                LineType::Blank => {
                    result.push('\n');
                    continuation_mode = false;
                }
                LineType::Continuation => {
                    if !continuation_mode {
                        return Err(LexError::InvalidContinuation {
                            line: line_num + 1,
                            column: 7,
                        });
                    }
                    let code_area = extract_code_area(line);
                    result.push_str(code_area);
                }
                LineType::Code => {
                    if continuation_mode {
                        result.push('\n');
                    }
                    let code_area = extract_code_area(line);
                    if !code_area.trim().is_empty() {
                        result.push_str(code_area);
                        continuation_mode = self.line_continues(line);
                        if continuation_mode {
                            result.push(' ');
                        } else {
                            result.push('\n');
                        }
                    } else {
                        result.push('\n');
                        continuation_mode = false;
                    }
                }
                LineType::Debug => {
                    result.push_str("*> DEBUG: ");
                    let code_area = extract_code_area(line);
                    result.push_str(code_area);
                    result.push('\n');
                    continuation_mode = false;
                }
            }
        }

        Ok(result)
    }

    /// Classify a line based on the indicator in column 7.
    fn classify_line(&self, line: &str) -> LineType {
        if line.len() < 7 {
            return LineType::Blank;
        }

        let indicator = line.chars().nth(6).unwrap_or(' ');
        
        match indicator {
            '*' | '/' => LineType::Comment,
            '-' => LineType::Continuation,
            'D' | 'd' => LineType::Debug,
            ' ' => {
                if line.trim().is_empty() {
                    LineType::Blank
                } else {
                    LineType::Code
                }
            }
            _ => LineType::Code,
        }
    }

    /// Check if a line ends with continuation (no period and content continues).
    fn line_continues(&self, line: &str) -> bool {
        let code_area = extract_code_area(line).trim();
        !code_area.is_empty() && !code_area.ends_with('.')
    }
}

#[derive(Debug, PartialEq)]
enum LineType {
    Comment,
    Continuation,
    Code,
    Debug,
    Blank,
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

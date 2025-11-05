use crate::error::{LexError, LexResult};
use crate::token::{lookup_keyword, Token, TokenType};

/// Lexer for free-format COBOL.
pub struct FreeFormatLexer<'a> {
    #[allow(dead_code)]
    source: &'a str,
    chars: std::str::Chars<'a>,
    current_pos: usize,
    line: usize,
    column: usize,
    peeked: Option<char>,
}

impl<'a> FreeFormatLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
            current_pos: 0,
            line: 1,
            column: 1,
            peeked: None,
        }
    }

    /// Get the next character, advancing position.
    fn next_char(&mut self) -> Option<char> {
        if let Some(ch) = self.peeked.take() {
            self.advance_position(ch);
            Some(ch)
        } else {
            let ch = self.chars.next()?;
            self.advance_position(ch);
            Some(ch)
        }
    }

    /// Peek at the next character without advancing.
    fn peek_char(&mut self) -> Option<char> {
        if self.peeked.is_none() {
            self.peeked = self.chars.next();
        }
        self.peeked
    }

    /// Advance position tracking based on character.
    fn advance_position(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += ch.len_utf8();
        }
        self.current_pos += ch.len_utf8();
    }

    /// Skip whitespace including newlines.
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
                self.next_char();
            } else {
                break;
            }
        }
    }

    /// Tokenize the entire source.
    pub fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            self.skip_whitespace();

            if let Some(token) = self.next_token()? {
                if token.token_type == TokenType::Eof {
                    tokens.push(token);
                    break;
                }
                tokens.push(token);
            } else {
                break;
            }
        }

        Ok(tokens)
    }

    /// Get the next token.
    fn next_token(&mut self) -> LexResult<Option<Token>> {
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.current_pos;

        let ch = match self.next_char() {
            Some(ch) => ch,
            None => {
                return Ok(Some(Token::new(
                    TokenType::Eof,
                    String::new(),
                    self.line,
                    self.column,
                    self.current_pos,
                    self.current_pos,
                )));
            }
        };

        let token = match ch {
            // Comments
            '*' if self.peek_char() == Some('>') => {
                self.next_char(); // consume '>'
                self.tokenize_comment(start_line, start_column, start_pos)?
            }
            '*' if start_column == 1 => {
                // Fixed-format style comment in column 1
                self.tokenize_line_comment(start_line, start_column, start_pos)?
            }

            // String literals
            '"' | '\'' => self.tokenize_string(ch, start_line, start_column, start_pos)?,

            // Numbers
            ch if ch.is_ascii_digit() => {
                self.tokenize_number(ch, start_line, start_column, start_pos)?
            }

            // Operators and punctuation
            '=' => self.single_char_token(TokenType::Equals, "=", start_line, start_column, start_pos),
            '<' => {
                if self.peek_char() == Some('>') {
                    self.next_char();
                    self.two_char_token(TokenType::NotEquals, "<>", start_line, start_column, start_pos)
                } else if self.peek_char() == Some('=') {
                    self.next_char();
                    self.two_char_token(TokenType::LessOrEqual, "<=", start_line, start_column, start_pos)
                } else {
                    self.single_char_token(TokenType::LessThan, "<", start_line, start_column, start_pos)
                }
            }
            '>' => {
                if self.peek_char() == Some('=') {
                    self.next_char();
                    self.two_char_token(TokenType::GreaterOrEqual, ">=", start_line, start_column, start_pos)
                } else {
                    self.single_char_token(TokenType::GreaterThan, ">", start_line, start_column, start_pos)
                }
            }
            '+' => self.single_char_token(TokenType::Plus, "+", start_line, start_column, start_pos),
            '-' => self.single_char_token(TokenType::Minus, "-", start_line, start_column, start_pos),
            '*' => {
                if self.peek_char() == Some('*') {
                    self.next_char();
                    self.two_char_token(TokenType::Power, "**", start_line, start_column, start_pos)
                } else {
                    self.single_char_token(TokenType::Multiply, "*", start_line, start_column, start_pos)
                }
            }
            '/' => self.single_char_token(TokenType::Divide, "/", start_line, start_column, start_pos),
            '.' => self.single_char_token(TokenType::Period, ".", start_line, start_column, start_pos),
            ',' => self.single_char_token(TokenType::Comma, ",", start_line, start_column, start_pos),
            ';' => self.single_char_token(TokenType::Semicolon, ";", start_line, start_column, start_pos),
            ':' => self.single_char_token(TokenType::Colon, ":", start_line, start_column, start_pos),
            '(' => self.single_char_token(TokenType::LeftParen, "(", start_line, start_column, start_pos),
            ')' => self.single_char_token(TokenType::RightParen, ")", start_line, start_column, start_pos),
            '[' => self.single_char_token(TokenType::LeftBracket, "[", start_line, start_column, start_pos),
            ']' => self.single_char_token(TokenType::RightBracket, "]", start_line, start_column, start_pos),

            // Identifiers and keywords
            ch if ch.is_ascii_alphabetic() || ch == '_' || ch == '-' => {
                self.tokenize_identifier_or_keyword(ch, start_line, start_column, start_pos)?
            }

            // Whitespace (should be skipped, but we're here as fallback)
            ' ' | '\t' | '\r' => return Ok(None),
            
            // Newlines are handled in skip_whitespace
            '\n' => return Ok(None),

            // Unknown
            ch => {
                return Err(LexError::UnexpectedChar {
                    char: ch,
                    line: start_line,
                    column: start_column,
                });
            }
        };

        Ok(Some(token))
    }

    fn tokenize_comment(&mut self, start_line: usize, start_column: usize, start_pos: usize) -> LexResult<Token> {
        let _comment = String::from("*>");
        let mut content = String::new();

        while let Some(ch) = self.next_char() {
            if ch == '\n' {
                break;
            }
            content.push(ch);
        }

        Ok(Token::new(
            TokenType::Comment(content.clone()),
            format!("*>{}", content),
            start_line,
            start_column,
            start_pos,
            self.current_pos,
        ))
    }

    fn tokenize_line_comment(&mut self, start_line: usize, start_column: usize, start_pos: usize) -> LexResult<Token> {
        let mut content = String::new();

        while let Some(ch) = self.next_char() {
            if ch == '\n' {
                break;
            }
            content.push(ch);
        }

        Ok(Token::new(
            TokenType::Comment(content.clone()),
            format!("*{}", content),
            start_line,
            start_column,
            start_pos,
            self.current_pos,
        ))
    }

    fn tokenize_string(&mut self, quote: char, start_line: usize, start_column: usize, start_pos: usize) -> LexResult<Token> {
        let mut value = String::new();
        let mut lexeme = String::from(quote);
        let mut terminated = false;

        while let Some(ch) = self.next_char() {
            match ch {
                '\\' if self.peek_char() == Some('"') || self.peek_char() == Some('\'') => {
                    // Escaped quote
                    let escaped = match self.next_char() {
                        Some(ch) => ch,
                        None => return Err(LexError::UnterminatedString { line: self.line, column: self.column }),
                    };
                    value.push(escaped);
                    lexeme.push('\\');
                    lexeme.push(escaped);
                }
                ch if ch == quote => {
                    lexeme.push(ch);
                    terminated = true;
                    break;
                }
                '\n' => {
                    return Err(LexError::UnterminatedString {
                        line: start_line,
                        column: start_column,
                    });
                }
                ch => {
                    value.push(ch);
                    lexeme.push(ch);
                }
            }
        }

        if !terminated {
            return Err(LexError::UnterminatedString {
                line: start_line,
                column: start_column,
            });
        }

        Ok(Token::new(
            TokenType::StringLiteral(value),
            lexeme,
            start_line,
            start_column,
            start_pos,
            self.current_pos,
        ))
    }

    fn tokenize_number(&mut self, first: char, start_line: usize, start_column: usize, start_pos: usize) -> LexResult<Token> {
        let mut value = String::from(first);
        let mut has_decimal = false;

        while let Some(ch) = self.peek_char() {
            match ch {
                '.' if !has_decimal => {
                    value.push(ch);
                    has_decimal = true;
                    self.next_char();
                }
                ch if ch.is_ascii_digit() => {
                    value.push(ch);
                    self.next_char();
                }
                _ => break,
            }
        }

        // Check for level numbers (01-49, 66, 77, 88)
        if let Ok(num) = value.parse::<u8>() {
            if (1..=49).contains(&num) || num == 66 || num == 77 || num == 88 {
                return Ok(Token::new(
                    TokenType::LevelNumber(num),
                    value.clone(),
                    start_line,
                    start_column,
                    start_pos,
                    self.current_pos,
                ));
            }
        }

        Ok(Token::new(
            TokenType::NumericLiteral(value.clone()),
            value,
            start_line,
            start_column,
            start_pos,
            self.current_pos,
        ))
    }

    fn tokenize_identifier_or_keyword(&mut self, first: char, start_line: usize, start_column: usize, start_pos: usize) -> LexResult<Token> {
        let mut value = String::from(first);

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                value.push(ch);
                self.next_char();
            } else {
                break;
            }
        }

        // Check if it's a keyword
        if let Some(keyword_type) = lookup_keyword(&value) {
            Ok(Token::new(
                keyword_type,
                value.clone(),
                start_line,
                start_column,
                start_pos,
                self.current_pos,
            ))
        } else {
            Ok(Token::new(
                TokenType::Identifier(value.clone()),
                value,
                start_line,
                start_column,
                start_pos,
                self.current_pos,
            ))
        }
    }

    fn single_char_token(&self, token_type: TokenType, lexeme: &str, line: usize, column: usize, start: usize) -> Token {
        Token::new(
            token_type,
            lexeme.to_string(),
            line,
            column,
            start,
            start + lexeme.len(),
        )
    }

    fn two_char_token(&self, token_type: TokenType, lexeme: &str, line: usize, column: usize, start: usize) -> Token {
        Token::new(
            token_type,
            lexeme.to_string(),
            line,
            column,
            start,
            start + lexeme.len(),
        )
    }
}

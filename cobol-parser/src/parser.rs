use cobol_ast::{Span, Spanned};
use cobol_ast::program::*;
use cobol_ast::statement::*;
use cobol_ast::expression::*;
use cobol_ast::data::*;
use cobol_ast::literal::*;
use cobol_lexer::{Token, TokenType};
use crate::error::{ParseError, ParseResult};

/// Recursive descent parser for COBOL.
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Create a new parser from tokens.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    /// Parse a complete COBOL program.
    pub fn parse_program(&mut self) -> ParseResult<Spanned<Program>> {
        let start_pos = self.current_token().map(|t| (t.line, t.column, t.start));

        let identification = self.parse_identification_division()?;
        let environment = self.optional_parse_environment_division()?;
        let data = self.optional_parse_data_division()?;
        let procedure = self.parse_procedure_division()?;

        let end_pos = self.previous_token().map(|t| (t.line, t.column, t.end));

        let span = if let (Some(start), Some(end)) = (start_pos, end_pos) {
            Span::from_tokens(start, end)
        } else {
            Span::new(1, 1, 1, 1, 0, 0)
        };

        Ok(Spanned::new(
            Program {
                identification,
                environment,
                data,
                procedure,
            },
            span,
        ))
    }

    /// Parse IDENTIFICATION DIVISION.
    fn parse_identification_division(&mut self) -> ParseResult<Spanned<IdentificationDivision>> {
        let start = self.consume_token(TokenType::Identification)?;
        self.consume_token(TokenType::Division)?;
        self.consume_token(TokenType::Period)?;

        let mut program_id = None;

        // Parse PROGRAM-ID if present
        if self.check_token(TokenType::ProgramId) {
            self.advance(); // consume PROGRAM-ID
            if let Some(token) = self.advance() {
                if let TokenType::Identifier(ref name) = token.token_type {
                    program_id = Some(name.clone());
                }
            }
            self.consume_token(TokenType::Period)?;
        }

        // Skip to next division (simplified - would parse other entries)
        while !self.is_at_end()
            && !self.check_token(TokenType::Environment)
            && !self.check_token(TokenType::Data)
            && !self.check_token(TokenType::Procedure)
        {
            self.advance();
        }

        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));
        Ok(Spanned::new(
            IdentificationDivision {
                program_id,
                author: None,
                installation: None,
                date_written: None,
                date_compiled: None,
                security: None,
                remarks: None,
            },
            span,
        ))
    }

    /// Optionally parse ENVIRONMENT DIVISION.
    fn optional_parse_environment_division(
        &mut self,
    ) -> ParseResult<Option<Spanned<EnvironmentDivision>>> {
        if !self.check_token(TokenType::Environment) {
            return Ok(None);
        }

        let start = self.consume_token(TokenType::Environment)?;
        self.consume_token(TokenType::Division)?;
        self.consume_token(TokenType::Period)?;

        // Skip to next division (simplified)
        while !self.is_at_end()
            && !self.check_token(TokenType::Data)
            && !self.check_token(TokenType::Procedure)
        {
            self.advance();
        }

        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));
        Ok(Some(Spanned::new(
            EnvironmentDivision {
                configuration_section: None,
                input_output_section: None,
            },
            span,
        )))
    }

    /// Optionally parse DATA DIVISION.
    fn optional_parse_data_division(&mut self) -> ParseResult<Option<Spanned<DataDivision>>> {
        if !self.check_token(TokenType::Data) {
            return Ok(None);
        }

        let start = self.consume_token(TokenType::Data)?;
        self.consume_token(TokenType::Division)?;
        self.consume_token(TokenType::Period)?;

        let mut working_storage = None;

        // Parse WORKING-STORAGE SECTION if present
        if self.check_token(TokenType::WorkingStorage) {
            let ws_start = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                message: "Expected WORKING-STORAGE token".to_string(),
            })?;
            self.consume_token(TokenType::Section)?;
            self.consume_token(TokenType::Period)?;

            let mut data_items = Vec::new();
            // Parse data items until PROCEDURE DIVISION
            while !self.is_at_end() && !self.check_token(TokenType::Procedure) {
                if self.check_level_number() {
                    if let Ok(item) = self.parse_data_item() {
                        data_items.push(item);
                    } else {
                        self.advance(); // Skip invalid item
                    }
                } else {
                    self.advance();
                }
            }

            let ws_span = self.create_span(&ws_start, &self.previous_token().unwrap_or(&ws_start));
            working_storage = Some(Spanned::new(
                WorkingStorageSection { data_items },
                ws_span,
            ));
        }

        // Skip to PROCEDURE DIVISION
        while !self.is_at_end() && !self.check_token(TokenType::Procedure) {
            self.advance();
        }

        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));
        Ok(Some(Spanned::new(
            DataDivision {
                file_section: None,
                working_storage_section: working_storage,
                local_storage_section: None,
                linkage_section: None,
            },
            span,
        )))
    }

    /// Parse PROCEDURE DIVISION.
    fn parse_procedure_division(&mut self) -> ParseResult<Spanned<ProcedureDivision>> {
        let start = self.consume_token(TokenType::Procedure)?;
        self.consume_token(TokenType::Division)?;
        self.consume_token(TokenType::Period)?;

        let mut statements = Vec::new();

        // Parse statements until end
        while !self.is_at_end() {
            if let Ok(statement) = self.parse_statement() {
                statements.push(statement);
            } else {
                break;
            }
        }

        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));
        Ok(Spanned::new(
            ProcedureDivision {
                using: None,
                returning: None,
                sections: Vec::new(),
                paragraphs: Vec::new(),
                statements,
            },
            span,
        ))
    }

    /// Parse a data item.
    fn parse_data_item(&mut self) -> ParseResult<Spanned<DataItem>> {
        let level_token = self.advance().ok_or_else(|| {
            ParseError::UnexpectedEof {
                expected: vec!["level number".to_string()],
            }
        })?;

        let level = if let TokenType::LevelNumber(level) = level_token.token_type {
            if !((1..=49).contains(&level) || level == 66 || level == 77 || level == 88) {
                return Err(ParseError::InvalidLevelNumber {
                    value: level,
                    token: level_token.clone(),
                });
            }
            level
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: vec!["level number".to_string()],
                found: level_token,
            });
        };

        let name_token = self.advance().ok_or_else(|| {
            ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            }
        })?;

        let name = if let TokenType::Identifier(ref name) = name_token.token_type {
            Spanned::new(name.clone(), self.create_span(&name_token, &name_token))
        } else if let TokenType::Filler = name_token.token_type {
            Spanned::new("FILLER".to_string(), self.create_span(&name_token, &name_token))
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: vec!["identifier".to_string()],
                found: name_token,
            });
        };

        // Parse optional clauses
        let mut picture = None;
        let mut value = None;

        while !self.is_at_end() {
            if self.check_token(TokenType::Pic) || self.check_token(TokenType::Picture) {
                let pic_start = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                    message: "Expected PICTURE token".to_string(),
                })?;
                // Simplified - would parse PICTURE string
                if let Some(pic_token) = self.advance() {
                    if let TokenType::StringLiteral(ref pic_str) = pic_token.token_type {
                        let pic_span = self.create_span(&pic_start, &pic_token);
                        picture = Some(Spanned::new(Picture::new(pic_str.clone()), pic_span));
                    }
                }
            } else if self.check_token(TokenType::Value) {
                let value_start = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                    message: "Expected VALUE token".to_string(),
                })?;
                // Simplified - would parse VALUE clause
                if let Some(value_token) = self.advance() {
                    if let TokenType::StringLiteral(ref lit) = value_token.token_type {
                        let value_span = self.create_span(&value_start, &value_token);
                        value = Some(Spanned::new(
                            InitialValue::Literal(Literal::String(lit.clone())),
                            value_span,
                        ));
                    }
                }
            } else if self.check_token(TokenType::Period) {
                self.advance();
                break;
            } else {
                self.advance(); // Skip other clauses for now
            }
        }

        let span = self.create_span(&level_token, &self.previous_token().unwrap_or(&level_token));
        Ok(Spanned::new(
            DataItem {
                level,
                name,
                picture,
                value,
                occurs: None,
                redefines: None,
                usage: None,
                children: Vec::new(),
            },
            span,
        ))
    }

    /// Parse a statement.
    fn parse_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let token = self.peek().ok_or_else(|| ParseError::UnexpectedEof {
            expected: vec!["statement".to_string()],
        })?;

        match token.token_type {
            TokenType::Display => self.parse_display_statement(),
            TokenType::Accept => self.parse_accept_statement(),
            TokenType::Move => self.parse_move_statement(),
            TokenType::Compute => self.parse_compute_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::Stop => self.parse_stop_statement(),
            TokenType::Exit => self.parse_exit_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => Err(ParseError::UnexpectedToken {
                expected: vec!["DISPLAY, ACCEPT, MOVE, COMPUTE, IF, STOP".to_string()],
                found: token.clone(),
            }),
        }
    }

    /// Parse DISPLAY statement.
    fn parse_display_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.consume_token(TokenType::Display)?;
        let mut operands = Vec::new();

        while !self.is_at_end() && !self.check_token(TokenType::Period) {
            if let Some(token) = self.advance() {
                match token.token_type {
                    TokenType::StringLiteral(ref s) => {
                        let span = self.create_span(&token, &token);
                        operands.push(Spanned::new(
                            DisplayOperand::Literal(Literal::String(s.clone())),
                            span,
                        ));
                    }
                    TokenType::Identifier(ref id) => {
                        let span = self.create_span(&token, &token);
                        operands.push(Spanned::new(
                            DisplayOperand::Identifier(id.clone()),
                            span,
                        ));
                    }
                    _ => break,
                }
            }
        }

        self.consume_token(TokenType::Period)?;
        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));

        Ok(Spanned::new(
            Statement::Display(Spanned::new(
                DisplayStatement { operands },
                span.clone(),
            )),
            span,
        ))
    }

    /// Parse ACCEPT statement.
    fn parse_accept_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.consume_token(TokenType::Accept)?;
        let id_token = self.advance().ok_or_else(|| {
            ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            }
        })?;

        let identifier = if let TokenType::Identifier(ref id) = id_token.token_type {
            id.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: vec!["identifier".to_string()],
                found: id_token,
            });
        };

        self.consume_token(TokenType::Period)?;
        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));

        Ok(Spanned::new(
            Statement::Accept(Spanned::new(
                AcceptStatement {
                    identifier,
                    from: None,
                },
                span.clone(),
            )),
            span,
        ))
    }

    /// Parse MOVE statement.
    fn parse_move_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.consume_token(TokenType::Move)?;
        // Simplified - would parse full MOVE statement
        self.consume_token(TokenType::Period)?;
        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));

        Ok(Spanned::new(
            Statement::Move(Spanned::new(
                MoveStatement {
                    from: Spanned::new(MoveSource::Literal(Literal::String(String::new())), span.clone()),
                    to: Vec::new(),
                },
                span.clone(),
            )),
            span,
        ))
    }

    /// Parse COMPUTE statement.
    fn parse_compute_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.consume_token(TokenType::Compute)?;
        // Simplified - would parse full COMPUTE statement
        self.consume_token(TokenType::Period)?;
        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));

        Ok(Spanned::new(
            Statement::Compute(Spanned::new(
                ComputeStatement {
                    targets: Vec::new(),
                    expression: Spanned::new(
                        Expression::Literal(Literal::String(String::new())),
                        span.clone(),
                    ),
                },
                span.clone(),
            )),
            span,
        ))
    }

    /// Parse IF statement.
    fn parse_if_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.consume_token(TokenType::If)?;
        // Simplified - would parse full IF statement
        self.consume_token(TokenType::Period)?;
        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));

        Ok(Spanned::new(
            Statement::If(Spanned::new(
                IfStatement {
                    condition: Spanned::new(
                        Expression::Literal(Literal::Boolean(true)),
                        span.clone(),
                    ),
                    then_statements: Vec::new(),
                    else_statements: None,
                },
                span.clone(),
            )),
            span,
        ))
    }

    /// Parse STOP statement.
    fn parse_stop_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.consume_token(TokenType::Stop)?;
        let stop_type = if self.check_token(TokenType::Run) {
            self.advance();
            StopStatement::Run
        } else {
            StopStatement::Literal(None)
        };
        self.consume_token(TokenType::Period)?;
        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));

        Ok(Spanned::new(
            Statement::Stop(Spanned::new(stop_type, span.clone())),
            span,
        ))
    }

    /// Parse EXIT statement.
    fn parse_exit_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.consume_token(TokenType::Exit)?;
        self.consume_token(TokenType::Period)?;
        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));

        Ok(Spanned::new(
            Statement::Exit(Spanned::new(ExitStatement::Program, span.clone())),
            span,
        ))
    }

    /// Parse RETURN statement.
    fn parse_return_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.consume_token(TokenType::Return)?;
        // Simplified
        self.consume_token(TokenType::Period)?;
        let span = self.create_span(&start, &self.previous_token().unwrap_or(&start));

        Ok(Spanned::new(
            Statement::Return(Spanned::new(
                ReturnStatement {
                    file_name: String::new(),
                    into: None,
                    at_end: None,
                    not_at_end: None,
                },
                span.clone(),
            )),
            span,
        ))
    }

    // Helper methods

    fn check_token(&self, token_type: TokenType) -> bool {
        if let Some(token) = self.peek() {
            token.token_type == token_type
        } else {
            false
        }
    }

    fn check_level_number(&self) -> bool {
        if let Some(token) = self.peek() {
            matches!(token.token_type, TokenType::LevelNumber(_))
        } else {
            false
        }
    }

    fn consume_token(&mut self, token_type: TokenType) -> ParseResult<Token> {
        if self.check_token(token_type) {
            self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                message: format!("Expected {:?} token", token_type),
            })
        } else {
            let found = self.peek().cloned().unwrap_or_else(|| Token::new(
                TokenType::Eof,
                String::new(),
                0,
                0,
                0,
                0,
            ));
            Err(ParseError::UnexpectedToken {
                expected: vec![format!("{:?}", token_type)],
                found,
            })
        }
    }

    fn advance(&mut self) -> Option<Token> {
        // Skip trivial tokens (whitespace, comments)
        while let Some(token) = self.tokens.get(self.current) {
            if token.is_trivial() || token.token_type == TokenType::Eof {
                self.current += 1;
                if token.token_type == TokenType::Eof {
                    return None;
                }
            } else {
                self.current += 1;
                return Some(token.clone());
            }
        }
        None
    }

    fn peek(&self) -> Option<&Token> {
        // Skip trivial tokens (whitespace, comments)
        let mut idx = self.current;
        while let Some(token) = self.tokens.get(idx) {
            if !token.is_trivial() && token.token_type != TokenType::Eof {
                return Some(token);
            }
            idx += 1;
        }
        None
    }
    
    fn peek_raw(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn current_token(&self) -> Option<&Token> {
        // Return first non-trivial token from current position
        let mut idx = self.current;
        while let Some(token) = self.tokens.get(idx) {
            if !token.is_trivial() {
                return Some(token);
            }
            idx += 1;
        }
        None
    }

    fn previous_token(&self) -> Option<&Token> {
        // Return last non-trivial token before current
        if self.current == 0 {
            return None;
        }
        let mut idx = self.current - 1;
        loop {
            if let Some(token) = self.tokens.get(idx) {
                if !token.is_trivial() {
                    return Some(token);
                }
                if idx == 0 {
                    break;
                }
                idx -= 1;
            } else {
                break;
            }
        }
        None
    }

    fn is_at_end(&self) -> bool {
        if let Some(token) = self.peek_raw() {
            token.token_type == TokenType::Eof
        } else {
            true
        }
    }

    fn create_span(&self, start: &Token, end: &Token) -> Span {
        Span::new(
            start.line,
            start.column,
            end.line,
            end.column,
            start.start,
            end.end,
        )
    }
}

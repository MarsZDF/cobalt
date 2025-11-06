use cobol_lexer::{Token, TokenType};
use cobol_ast::Program;
use crate::config::{FormatConfig, KeywordCase, IdentifierCase};

/// Formatter for COBOL source code.
pub struct Formatter {
    config: FormatConfig,
}

impl Formatter {
    pub fn new(config: FormatConfig) -> Self {
        Self { config }
    }
    
    /// Format COBOL source code from tokens.
    /// Uses AST for indentation depth calculation.
    pub fn format(&self, tokens: &[Token], ast: Option<&cobol_ast::Spanned<Program>>) -> String {
        let mut output = String::new();
        let mut current_line = String::new();
        let mut current_indent = 0;
        let mut in_data_division = false;
        
        // Track indentation depth from AST if available
        let indent_map = if let Some(spanned_prog) = ast {
            self.compute_indentation(&spanned_prog.node, tokens)
        } else {
            self.compute_indentation_from_tokens(tokens)
        };
        
        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];
            
            // Skip EOF
            if matches!(token.token_type, TokenType::Eof) {
                break;
            }
            
            // Handle comments - preserve as-is
            if matches!(token.token_type, TokenType::Comment(_)) {
                if !current_line.trim().is_empty() {
                    output.push_str(&current_line);
                    output.push('\n');
                    current_line.clear();
                }
                output.push_str(&token.lexeme);
                output.push('\n');
                i += 1;
                continue;
            }
            
            // Skip whitespace - we'll add our own
            if matches!(token.token_type, TokenType::Whitespace(_)) {
                i += 1;
                continue;
            }
            
            // Handle division headers
            if self.is_division_keyword(&token.token_type) {
                if !current_line.trim().is_empty() {
                    output.push_str(&current_line);
                    output.push('\n');
                    current_line.clear();
                }
                current_indent = 0;
                if matches!(token.token_type, TokenType::Data) {
                    in_data_division = true;
                } else if matches!(token.token_type, TokenType::Procedure) {
                    in_data_division = false;
                }
            }
            
            // Handle level numbers in DATA DIVISION
            if in_data_division {
                if let TokenType::LevelNumber(level) = token.token_type {
                    if !current_line.trim().is_empty() {
                        output.push_str(&current_line);
                        output.push('\n');
                        current_line.clear();
                    }
                    // Level numbers: 01-49, 66, 77, 88
                    // Format: level number at column 8, then data item
                    current_line = format!("{:6} {}", level, self.indent_string(1));
                    i += 1;
                    continue;
                }
            }
            
            // Get indentation for this token
            let token_indent = indent_map.get(&i).copied().unwrap_or(current_indent);
            
            // Start new line if needed
            if current_line.trim().is_empty() {
                current_indent = token_indent;
                current_line = self.indent_string(current_indent);
            }
            
            // Format the token
            let formatted = self.format_token(token);
            
            // Check if we need to wrap the line
            let line_length = current_line.len();
            if line_length + formatted.len() > self.config.max_line_length && !current_line.trim().is_empty() {
                output.push_str(&current_line.trim_end());
                output.push('\n');
                current_line = self.indent_string(current_indent + 1);
            }
            
            // Add spacing before token if needed
            if !current_line.trim().is_empty() {
                if self.needs_space_before(tokens.get(i.saturating_sub(1)), token) {
                    current_line.push(' ');
                }
            }
            
            current_line.push_str(&formatted);
            
            // Check if this token ends a statement (period in COBOL)
            if matches!(token.token_type, TokenType::Period) {
                output.push_str(&current_line);
                output.push('\n');
                current_line.clear();
            }
            
            i += 1;
        }
        
        // Add remaining line
        if !current_line.trim().is_empty() {
            output.push_str(&current_line);
            output.push('\n');
        }
        
        output
    }
    
    fn format_token(&self, token: &Token) -> String {
        match &token.token_type {
            TokenType::Identifier(ref name) => {
                match self.config.identifier_case {
                    IdentifierCase::Upper => name.to_uppercase(),
                    IdentifierCase::Lower => name.to_lowercase(),
                    IdentifierCase::Preserve => name.clone(),
                }
            }
            keyword_type if self.is_keyword(keyword_type) => {
                let keyword = self.keyword_to_string(keyword_type);
                match self.config.keyword_case {
                    KeywordCase::Upper => keyword.to_uppercase(),
                    KeywordCase::Lower => keyword.to_lowercase(),
                    KeywordCase::Preserve => token.lexeme.clone(),
                }
            }
            TokenType::LevelNumber(level) => format!("{:02}", level),
            TokenType::StringLiteral(ref s) | TokenType::NumericLiteral(ref s) => s.clone(),
            TokenType::Period => ".".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::LeftParen => "(".to_string(),
            TokenType::RightParen => ")".to_string(),
            TokenType::Plus => "+".to_string(),
            TokenType::Minus => "-".to_string(),
            TokenType::Multiply => "*".to_string(),
            TokenType::Divide => "/".to_string(),
            TokenType::Equals => "=".to_string(),
            TokenType::GreaterThan => ">".to_string(),
            TokenType::LessThan => "<".to_string(),
            TokenType::GreaterOrEqual => ">=".to_string(),
            TokenType::LessOrEqual => "<=".to_string(),
            TokenType::NotEquals => "<>".to_string(),
            _ => token.lexeme.clone(),
        }
    }
    
    fn is_keyword(&self, token_type: &TokenType) -> bool {
        matches!(token_type,
            TokenType::Division | TokenType::Section | TokenType::Procedure |
            TokenType::Identification | TokenType::Data | TokenType::Environment |
            TokenType::WorkingStorage | TokenType::LocalStorage | TokenType::Linkage |
            TokenType::File | TokenType::ProgramId | TokenType::Perform |
            TokenType::If | TokenType::Else | TokenType::EndIf |
            TokenType::Move | TokenType::Compute | TokenType::Call |
            TokenType::Copy | TokenType::Display | TokenType::Accept |
            TokenType::Stop | TokenType::Run | TokenType::GoTo |
            TokenType::GoBack | TokenType::Exit | TokenType::Return |
            TokenType::Picture | TokenType::Pic | TokenType::Value |
            TokenType::Occurs | TokenType::Redefines | TokenType::Add |
            TokenType::Subtract | TokenType::Evaluate | TokenType::When |
            TokenType::EndEvaluate
        )
    }
    
    fn keyword_to_string(&self, token_type: &TokenType) -> &str {
        match token_type {
            TokenType::Division => "DIVISION",
            TokenType::Section => "SECTION",
            TokenType::Procedure => "PROCEDURE",
            TokenType::Identification => "IDENTIFICATION",
            TokenType::Data => "DATA",
            TokenType::Environment => "ENVIRONMENT",
            TokenType::WorkingStorage => "WORKING-STORAGE",
            TokenType::LocalStorage => "LOCAL-STORAGE",
            TokenType::Linkage => "LINKAGE",
            TokenType::File => "FILE",
            TokenType::ProgramId => "PROGRAM-ID",
            TokenType::Perform => "PERFORM",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::EndIf => "END-IF",
            TokenType::Move => "MOVE",
            TokenType::Compute => "COMPUTE",
            TokenType::Call => "CALL",
            TokenType::Copy => "COPY",
            TokenType::Display => "DISPLAY",
            TokenType::Accept => "ACCEPT",
            TokenType::Stop => "STOP",
            TokenType::Run => "RUN",
            TokenType::GoTo => "GO TO",
            TokenType::GoBack => "GOBACK",
            TokenType::Exit => "EXIT",
            TokenType::Return => "RETURN",
            TokenType::Picture => "PICTURE",
            TokenType::Pic => "PIC",
            TokenType::Value => "VALUE",
            TokenType::Occurs => "OCCURS",
            TokenType::Redefines => "REDEFINES",
            TokenType::Add => "ADD",
            TokenType::Subtract => "SUBTRACT",
            TokenType::Evaluate => "EVALUATE",
            TokenType::When => "WHEN",
            TokenType::EndEvaluate => "END-EVALUATE",
            _ => "",
        }
    }
    
    fn is_division_keyword(&self, token_type: &TokenType) -> bool {
        matches!(token_type,
            TokenType::Identification | TokenType::Environment |
            TokenType::Data | TokenType::Procedure
        )
    }
    
    fn needs_space_before(&self, prev: Option<&Token>, current: &Token) -> bool {
        if prev.is_none() {
            return false;
        }
        let prev_type = &prev.unwrap().token_type;
        
        // No space after opening paren, before closing paren
        if matches!(prev_type, TokenType::LeftParen) || matches!(current.token_type, TokenType::RightParen) {
            return false;
        }
        
        // No space before comma, semicolon, period
        if matches!(current.token_type, TokenType::Comma | TokenType::Semicolon | TokenType::Period) {
            return false;
        }
        
        // Space after comma, semicolon
        if matches!(prev_type, TokenType::Comma | TokenType::Semicolon) {
            return true;
        }
        
        // Space around operators if configured
        if self.config.space_around_operators {
            if self.is_operator(prev_type) || self.is_operator(&current.token_type) {
                return true;
            }
        }
        
        // Default: space between different token types
        true
    }
    
    fn is_operator(&self, token_type: &TokenType) -> bool {
        matches!(token_type,
            TokenType::Plus | TokenType::Minus | TokenType::Multiply |
            TokenType::Divide | TokenType::Equals | TokenType::NotEquals |
            TokenType::LessThan | TokenType::LessOrEqual |
            TokenType::GreaterThan | TokenType::GreaterOrEqual
        )
    }
    
    fn indent_string(&self, level: usize) -> String {
        if self.config.use_spaces {
            " ".repeat(level * self.config.indent_width)
        } else {
            "\t".repeat(level)
        }
    }
    
    fn compute_indentation(&self, _program: &Program, _tokens: &[Token]) -> std::collections::HashMap<usize, usize> {
        // TODO: Use AST to compute accurate indentation
        // For now, fall back to token-based heuristics
        self.compute_indentation_from_tokens(_tokens)
    }
    
    fn compute_indentation_from_tokens(&self, tokens: &[Token]) -> std::collections::HashMap<usize, usize> {
        let mut indent_map = std::collections::HashMap::new();
        let mut indent_level: usize = 0;
        let mut in_procedure_division = false;
        
        for (i, token) in tokens.iter().enumerate() {
            // Start PROCEDURE DIVISION
            if matches!(token.token_type, TokenType::Procedure) {
                in_procedure_division = true;
                indent_level = if self.config.indent_procedure { 1 } else { 0 };
            }
            
            // Increase indent after certain keywords in PROCEDURE DIVISION
            if in_procedure_division {
                if matches!(token.token_type,
                    TokenType::If | TokenType::Perform | TokenType::Evaluate
                ) {
                    indent_map.insert(i, indent_level);
                    indent_level += 1;
                } else if matches!(token.token_type,
                    TokenType::EndIf | TokenType::EndPerform | TokenType::EndEvaluate
                ) {
                    indent_level = indent_level.saturating_sub(1);
                    indent_map.insert(i, indent_level);
                } else if matches!(token.token_type, TokenType::Else | TokenType::When) {
                    indent_map.insert(i, indent_level.saturating_sub(1));
                } else {
                    indent_map.insert(i, indent_level);
                }
            } else {
                indent_map.insert(i, 0);
            }
        }
        
        indent_map
    }
}


/// Configuration for COBOL code formatting.
#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// Number of spaces per indentation level (default: 4)
    pub indent_width: usize,
    
    /// Use spaces instead of tabs (default: true)
    pub use_spaces: bool,
    
    /// Keyword case convention: "upper", "lower", or "preserve" (default: "upper")
    pub keyword_case: KeywordCase,
    
    /// Identifier case convention: "upper", "lower", or "preserve" (default: "preserve")
    pub identifier_case: IdentifierCase,
    
    /// Spacing around operators (default: true)
    pub space_around_operators: bool,
    
    /// Align data items by level number (default: true)
    pub align_data_items: bool,
    
    /// Maximum line length before wrapping (default: 80 for fixed-format, 132 for free-format)
    pub max_line_length: usize,
    
    /// Preserve existing line continuations (default: true)
    pub preserve_continuations: bool,
    
    /// Format PICTURE clauses (default: true)
    pub format_picture: bool,
    
    /// Indent PROCEDURE DIVISION statements (default: true)
    pub indent_procedure: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeywordCase {
    Upper,
    Lower,
    Preserve,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentifierCase {
    Upper,
    Lower,
    Preserve,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent_width: 4,
            use_spaces: true,
            keyword_case: KeywordCase::Upper, // COBOL traditionally uses uppercase
            identifier_case: IdentifierCase::Preserve,
            space_around_operators: true,
            align_data_items: true,
            max_line_length: 80, // COBOL traditional line length
            preserve_continuations: true,
            format_picture: true,
            indent_procedure: true,
        }
    }
}

impl FormatConfig {
    /// Create a new config with traditional COBOL style (uppercase keywords, 4-space indent).
    pub fn traditional() -> Self {
        Self {
            indent_width: 4,
            use_spaces: true,
            keyword_case: KeywordCase::Upper,
            identifier_case: IdentifierCase::Upper,
            space_around_operators: true,
            align_data_items: true,
            max_line_length: 80,
            preserve_continuations: true,
            format_picture: true,
            indent_procedure: true,
        }
    }
    
    /// Create a new config with modern COBOL style (lowercase keywords, 2-space indent).
    pub fn modern() -> Self {
        Self {
            indent_width: 2,
            use_spaces: true,
            keyword_case: KeywordCase::Lower,
            identifier_case: IdentifierCase::Lower,
            space_around_operators: true,
            align_data_items: true,
            max_line_length: 132,
            preserve_continuations: true,
            format_picture: true,
            indent_procedure: true,
        }
    }
    
    /// Create a config for fixed-format COBOL (72-character line length).
    pub fn fixed_format() -> Self {
        Self {
            max_line_length: 72,
            ..Self::traditional()
        }
    }
}


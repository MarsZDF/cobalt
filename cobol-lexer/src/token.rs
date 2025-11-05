/// Token types that can appear in COBOL source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Keywords
    Division,
    Section,
    Procedure,
    Identification,
    Data,
    Environment,
    WorkingStorage,
    LocalStorage,
    Linkage,
    File,
    ProgramId,
    Perform,
    If,
    Else,
    EndIf,
    Move,
    Compute,
    Call,
    Copy,
    Display,
    Accept,
    Stop,
    Run,
    GoTo,
    GoBack,
    Exit,
    Return,
    Using,
    Giving,
    Until,
    Varying,
    From,
    To,
    By,
    With,
    After,
    Before,
    Times,
    Thru,
    ThruEnd,
    EndPerform,
    
    // Data definition keywords
    Picture,
    Pic,
    Value,
    Occurs,
    Redefines,
    Indexed,
    Key,
    Is,
    External,
    Global,
    Filler,
    Justified,
    Just,
    Right,
    Left,
    Blank,
    Zero,
    
    // Arithmetic statements
    Add,
    Subtract,
    MultiplyOp,  // Different from Multiply token type above
    DivideOp,    // Different from Divide token type above
    On,
    Size,
    Error,
    Not,  // NOT operator/keyword
    
    // Control flow
    Evaluate,
    When,
    WhenOther,
    EndEvaluate,
    Continue,
    Next,
    Sentence,
    
    // File operations
    Open,
    Close,
    Read,
    Write,
    Rewrite,
    Delete,
    Start,
    Unlock,
    Input,
    Output,
    InputOutput,
    Extend,
    
    // String operations
    StringStmt,  // STRING statement (different from String literal)
    Unstring,
    Inspect,
    Tallying,
    Replacing,
    Converting,
    All,
    Leading,
    First,
    Initial,
    
    // Other statements
    Initialize,
    Set,
    Search,
    Sort,
    Merge,
    Release,
    Generate,
    Transform,
    Validate,
    
    // Conditions and qualifiers
    Equal,
    Greater,
    Less,
    Than,
    Negative,
    Positive,
    Space,
    HighValue,
    LowValue,
    Quote,
    
    // Logical operators (already have these as operators, but also as keywords)
    AndOp,
    OrOp,
    
    // Other keywords
    Of,
    In,
    Then,
    End,
    EndProgram,
    EndProgramId,
    EndData,
    EndEnvironment,
    EndProcedure,
    
    // Operators
    Equals,           // =
    NotEquals,        // <> or !=
    LessThan,         // <
    GreaterThan,      // >
    LessOrEqual,      // <=
    GreaterOrEqual,   // >=
    Plus,             // +
    Minus,            // -
    Multiply,         // *
    Divide,           // /
    Power,            // **
    
    // Punctuation
    Period,           // .
    Comma,            // ,
    Semicolon,        // ;
    Colon,            // :
    LeftParen,        // (
    RightParen,       // )
    LeftBracket,      // [
    RightBracket,     // ]
    
    // Literals
    StringLiteral(String),    // "text" or 'text'
    NumericLiteral(String),   // 123, 123.45, -123, +123
    
    // Identifiers
    Identifier(String),
    
    // Special COBOL constructs
    LevelNumber(u8),          // 01-49, 66, 77, 88
    
    // Comments
    Comment(String),
    
    // Whitespace (optionally preserved)
    Whitespace(String),
    
    // End of file
    Eof,
    
    // Unknown/error token
    Unknown(char),
}

/// A token with position information for error reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// The type of token
    pub token_type: TokenType,
    
    /// The actual text that was tokenized
    pub lexeme: String,
    
    /// Line number (1-indexed)
    pub line: usize,
    
    /// Column number (1-indexed)
    pub column: usize,
    
    /// Byte offset in the source string (0-indexed)
    pub start: usize,
    
    /// Byte offset of the end of the token (0-indexed)
    pub end: usize,
}

impl Token {
    /// Create a new token with the given information.
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        column: usize,
        start: usize,
        end: usize,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            line,
            column,
            start,
            end,
        }
    }
    
    /// Get the span as a tuple (start, end).
    pub fn span(&self) -> (usize, usize) {
        (self.start, self.end)
    }
    
    /// Check if this token is a keyword.
    pub fn is_keyword(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Division
                | TokenType::Section
                | TokenType::Procedure
                | TokenType::Identification
                | TokenType::Data
                | TokenType::Environment
                | TokenType::WorkingStorage
                | TokenType::Perform
                | TokenType::If
                | TokenType::Else
                | TokenType::Move
                | TokenType::Compute
                | TokenType::Call
                | TokenType::Display
        )
    }
    
    /// Check if this token is whitespace or a comment.
    pub fn is_trivial(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Whitespace(_) | TokenType::Comment(_)
        )
    }
}

/// COBOL keywords mapped to their token types.
pub const KEYWORDS: &[(&str, TokenType)] = &[
    ("DIVISION", TokenType::Division),
    ("SECTION", TokenType::Section),
    ("PROCEDURE", TokenType::Procedure),
    ("IDENTIFICATION", TokenType::Identification),
    ("DATA", TokenType::Data),
    ("ENVIRONMENT", TokenType::Environment),
    ("WORKING-STORAGE", TokenType::WorkingStorage),
    ("LOCAL-STORAGE", TokenType::LocalStorage),
    ("LINKAGE", TokenType::Linkage),
    ("FILE", TokenType::File),
    ("PROGRAM-ID", TokenType::ProgramId),
    ("PROGRAM-ID.", TokenType::ProgramId),
    ("PERFORM", TokenType::Perform),
    ("IF", TokenType::If),
    ("ELSE", TokenType::Else),
    ("END-IF", TokenType::EndIf),
    ("ENDIF", TokenType::EndIf),
    ("MOVE", TokenType::Move),
    ("COMPUTE", TokenType::Compute),
    ("CALL", TokenType::Call),
    ("COPY", TokenType::Copy),
    ("DISPLAY", TokenType::Display),
    ("ACCEPT", TokenType::Accept),
    ("STOP", TokenType::Stop),
    ("RUN", TokenType::Run),
    ("STOP-RUN", TokenType::Stop),
    ("GO", TokenType::GoTo),
    ("GOTO", TokenType::GoTo),
    ("GO-BACK", TokenType::GoBack),
    ("GOBACK", TokenType::GoBack),
    ("EXIT", TokenType::Exit),
    ("RETURN", TokenType::Return),
    ("USING", TokenType::Using),
    ("GIVING", TokenType::Giving),
    ("UNTIL", TokenType::Until),
    ("VARYING", TokenType::Varying),
    ("FROM", TokenType::From),
    ("TO", TokenType::To),
    ("BY", TokenType::By),
    ("WITH", TokenType::With),
    ("AFTER", TokenType::After),
    ("BEFORE", TokenType::Before),
    ("TIMES", TokenType::Times),
    ("THROUGH", TokenType::Thru),
    ("THRU", TokenType::Thru),
    ("END-PERFORM", TokenType::EndPerform),
    ("ENDPERFORM", TokenType::EndPerform),
    ("PICTURE", TokenType::Picture),
    ("PIC", TokenType::Pic),
    ("VALUE", TokenType::Value),
    ("OCCURS", TokenType::Occurs),
    ("REDEFINES", TokenType::Redefines),
    ("INDEXED", TokenType::Indexed),
    ("KEY", TokenType::Key),
    ("IS", TokenType::Is),
    ("EXTERNAL", TokenType::External),
    ("GLOBAL", TokenType::Global),
    ("FILLER", TokenType::Filler),
    ("JUSTIFIED", TokenType::Justified),
    ("JUST", TokenType::Just),
    ("RIGHT", TokenType::Right),
    ("LEFT", TokenType::Left),
    ("BLANK", TokenType::Blank),
    ("ZERO", TokenType::Zero),
    ("ZEROS", TokenType::Zero),
    ("ZEROES", TokenType::Zero),
    ("ADD", TokenType::Add),
    ("SUBTRACT", TokenType::Subtract),
    ("MULTIPLY", TokenType::MultiplyOp),
    ("DIVIDE", TokenType::DivideOp),
    ("ON", TokenType::On),
    ("SIZE", TokenType::Size),
    ("ERROR", TokenType::Error),
    ("NOT", TokenType::Not),
    ("EVALUATE", TokenType::Evaluate),
    ("WHEN", TokenType::When),
    ("WHEN-OTHER", TokenType::WhenOther),
    ("WHENOTHER", TokenType::WhenOther),
    ("END-EVALUATE", TokenType::EndEvaluate),
    ("ENDEVALUATE", TokenType::EndEvaluate),
    ("CONTINUE", TokenType::Continue),
    ("NEXT", TokenType::Next),
    ("SENTENCE", TokenType::Sentence),
    ("OPEN", TokenType::Open),
    ("CLOSE", TokenType::Close),
    ("READ", TokenType::Read),
    ("WRITE", TokenType::Write),
    ("REWRITE", TokenType::Rewrite),
    ("DELETE", TokenType::Delete),
    ("START", TokenType::Start),
    ("UNLOCK", TokenType::Unlock),
    ("STRING", TokenType::StringStmt),
    ("UNSTRING", TokenType::Unstring),
    ("INSPECT", TokenType::Inspect),
    ("TALLYING", TokenType::Tallying),
    ("REPLACING", TokenType::Replacing),
    ("CONVERTING", TokenType::Converting),
    ("ALL", TokenType::All),
    ("LEADING", TokenType::Leading),
    ("FIRST", TokenType::First),
    ("INITIAL", TokenType::Initial),
    ("INITIALIZE", TokenType::Initialize),
    ("SET", TokenType::Set),
    ("SEARCH", TokenType::Search),
    ("SORT", TokenType::Sort),
    ("MERGE", TokenType::Merge),
    ("RELEASE", TokenType::Release),
    ("GENERATE", TokenType::Generate),
    ("TRANSFORM", TokenType::Transform),
    ("VALIDATE", TokenType::Validate),
    ("EQUAL", TokenType::Equal),
    ("GREATER", TokenType::Greater),
    ("LESS", TokenType::Less),
    ("THAN", TokenType::Than),
    ("NEGATIVE", TokenType::Negative),
    ("POSITIVE", TokenType::Positive),
    ("SPACE", TokenType::Space),
    ("SPACES", TokenType::Space),
    ("HIGH-VALUE", TokenType::HighValue),
    ("HIGH-VALUES", TokenType::HighValue),
    ("HIGHVALUE", TokenType::HighValue),
    ("HIGHVALUES", TokenType::HighValue),
    ("LOW-VALUE", TokenType::LowValue),
    ("LOW-VALUES", TokenType::LowValue),
    ("LOWVALUE", TokenType::LowValue),
    ("LOWVALUES", TokenType::LowValue),
    ("QUOTE", TokenType::Quote),
    ("QUOTES", TokenType::Quote),
    ("AND", TokenType::AndOp),
    ("OR", TokenType::OrOp),
    ("OF", TokenType::Of),
    ("IN", TokenType::In),
    ("THEN", TokenType::Then),
    ("END", TokenType::End),
];

/// Look up a keyword by name (case-insensitive).
pub fn lookup_keyword(name: &str) -> Option<TokenType> {
    let upper = name.to_uppercase();
    KEYWORDS
        .iter()
        .find(|(kw, _)| kw.eq_ignore_ascii_case(&upper))
        .map(|(_, token_type)| token_type.clone())
}

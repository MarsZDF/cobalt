use crate::span::Spanned;
use crate::expression::Expression;
use crate::literal::Literal;

/// Procedure division statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum Statement {
    Display(Spanned<DisplayStatement>),
    Accept(Spanned<AcceptStatement>),
    Move(Spanned<MoveStatement>),
    Compute(Spanned<ComputeStatement>),
    If(Spanned<IfStatement>),
    Evaluate(Spanned<EvaluateStatement>),
    Perform(Box<Spanned<PerformStatement>>),
    Call(Spanned<CallStatement>),
    GoTo(Spanned<GoToStatement>),
    GoBack(Spanned<GoBackStatement>),
    Exit(Spanned<ExitStatement>),
    Stop(Spanned<StopStatement>),
    Return(Spanned<ReturnStatement>),
    Paragraph(Spanned<ParagraphStatement>),
    Section(Spanned<SectionStatement>),
    // String manipulation
    String(Spanned<StringStatement>),
    Unstring(Spanned<UnstringStatement>),
    // Table handling
    Search(Spanned<SearchStatement>),
    Sort(Spanned<SortStatement>),
    // File operations
    Open(Spanned<OpenStatement>),
    Close(Spanned<CloseStatement>),
    Read(Spanned<ReadStatement>),
    Write(Spanned<WriteStatement>),
    Rewrite(Spanned<RewriteStatement>),
    Delete(Spanned<DeleteStatement>),
}

/// DISPLAY statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct DisplayStatement {
    pub operands: Vec<Spanned<DisplayOperand>>,
}

/// Operand in DISPLAY statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum DisplayOperand {
    Literal(Literal),
    Identifier(String),
}

/// ACCEPT statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct AcceptStatement {
    pub identifier: String,
    pub from: Option<String>, // FROM mnemonic-name
}

/// MOVE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct MoveStatement {
    pub from: Spanned<MoveSource>,
    pub to: Vec<Spanned<String>>, // TO identifier...
}

/// Source in MOVE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum MoveSource {
    Literal(Literal),
    Identifier(String),
    FigurativeConstant(crate::data::FigurativeConstant),
}

/// COMPUTE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct ComputeStatement {
    pub targets: Vec<Spanned<String>>, // identifier = ... or identifier ROUNDED = ...
    pub expression: Spanned<Expression>,
}

/// IF statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct IfStatement {
    pub condition: Spanned<Expression>,
    pub then_statements: Vec<Spanned<Statement>>,
    pub else_statements: Option<Vec<Spanned<Statement>>>,
}

/// PERFORM statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum PerformStatement {
    /// PERFORM paragraph-name [THROUGH paragraph-name]
    Simple {
        paragraph: String,
        through: Option<String>,
    },
    /// PERFORM procedure-name [THROUGH procedure-name] n TIMES
    Times {
        paragraph: String,
        through: Option<String>,
        count: Spanned<Expression>,
    },
    /// PERFORM procedure-name [THROUGH procedure-name] UNTIL condition
    Until {
        paragraph: String,
        through: Option<String>,
        condition: Spanned<Expression>,
    },
    /// PERFORM procedure-name [THROUGH procedure-name] VARYING ... FROM ... BY ...
    Varying {
        paragraph: String,
        through: Option<String>,
        varying: Spanned<PerformVaryingClause>,
    },
    /// PERFORM ... END-PERFORM (inline)
    Inline {
        statements: Vec<Spanned<Statement>>,
        times: Option<Spanned<Expression>>,
        until: Option<Spanned<Expression>>,
        varying: Option<Spanned<PerformVaryingClause>>,
    },
}

/// PERFORM VARYING clause.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct PerformVaryingClause {
    pub identifier: String,
    pub from: Spanned<Expression>,
    pub by: Option<Spanned<Expression>>,
    pub until: Option<Spanned<Expression>>,
}

/// CALL statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct CallStatement {
    pub program_name: String,
    pub using: Option<Vec<Spanned<String>>>,
    pub returning: Option<String>,
}

/// GO TO statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct GoToStatement {
    pub paragraph: Option<String>,
    pub depending_on: Option<String>,
}

/// GO BACK statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct GoBackStatement {
    // GO BACK has no operands
}

/// EXIT statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum ExitStatement {
    Program,
    Paragraph,
    Section,
    Perform,
}

/// STOP statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum StopStatement {
    Run, // STOP RUN
    Literal(Option<Literal>), // STOP literal
}

/// RETURN statement (for file processing).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct ReturnStatement {
    pub file_name: String,
    pub into: Option<String>,
    pub at_end: Option<Vec<Spanned<Statement>>>,
    pub not_at_end: Option<Vec<Spanned<Statement>>>,
}

/// Paragraph statement (declaration).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct ParagraphStatement {
    pub name: String,
}

/// Section statement (declaration).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct SectionStatement {
    pub name: String,
}

/// EVALUATE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct EvaluateStatement {
    pub selection_subjects: Vec<Spanned<Expression>>,
    pub when_clauses: Vec<WhenClause>,
    pub when_other: Option<Vec<Spanned<Statement>>>,
}

/// WHEN clause in EVALUATE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct WhenClause {
    pub conditions: Vec<WhenCondition>,
    pub statements: Vec<Spanned<Statement>>,
}

/// Condition in WHEN clause.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum WhenCondition {
    Value(Spanned<Expression>),
    Range { from: Spanned<Expression>, to: Spanned<Expression> },
    Any, // ANY keyword
    True, // TRUE keyword  
    False, // FALSE keyword
}

/// STRING statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct StringStatement {
    pub sources: Vec<StringSource>,
    pub destination: Spanned<Expression>,
    pub pointer: Option<Spanned<Expression>>,
    pub on_overflow: Option<Vec<Spanned<Statement>>>,
    pub not_on_overflow: Option<Vec<Spanned<Statement>>>,
}

/// Source operand in STRING statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct StringSource {
    pub operand: Spanned<Expression>,
    pub delimiter: Option<Spanned<Expression>>,
}

/// UNSTRING statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct UnstringStatement {
    pub source: Spanned<Expression>,
    pub delimiters: Vec<Spanned<Expression>>,
    pub destinations: Vec<UnstringDestination>,
    pub pointer: Option<Spanned<Expression>>,
    pub tallying: Option<Spanned<Expression>>,
    pub on_overflow: Option<Vec<Spanned<Statement>>>,
    pub not_on_overflow: Option<Vec<Spanned<Statement>>>,
}

/// Destination operand in UNSTRING statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct UnstringDestination {
    pub destination: Spanned<Expression>,
    pub delimiter_in: Option<Spanned<Expression>>,
    pub count_in: Option<Spanned<Expression>>,
}

/// SEARCH statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct SearchStatement {
    pub table_name: String,
    pub varying: Option<String>,
    pub at_end: Option<Vec<Spanned<Statement>>>,
    pub when_clauses: Vec<SearchWhenClause>,
}

/// WHEN clause in SEARCH statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct SearchWhenClause {
    pub condition: Spanned<Expression>,
    pub statements: Vec<Spanned<Statement>>,
}

/// SORT statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct SortStatement {
    pub file_name: String,
    pub keys: Vec<SortKey>,
    pub input_procedure: Option<String>,
    pub using_files: Vec<String>,
    pub output_procedure: Option<String>,
    pub giving_files: Vec<String>,
}

/// Sort key specification.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct SortKey {
    pub direction: SortDirection,
    pub field: String,
}

/// Sort direction.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum SortDirection {
    Ascending,
    Descending,
}

/// OPEN statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct OpenStatement {
    pub files: Vec<OpenFile>,
}

/// File specification in OPEN statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct OpenFile {
    pub mode: OpenMode,
    pub file_name: String,
}

/// File open mode.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum OpenMode {
    Input,
    Output,
    InputOutput,
    Extend,
}

/// CLOSE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct CloseStatement {
    pub files: Vec<CloseFile>,
}

/// File specification in CLOSE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct CloseFile {
    pub file_name: String,
    pub disposition: Option<CloseDisposition>,
}

/// Close disposition.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum CloseDisposition {
    Reel,
    Unit,
    NoRewind,
    Lock,
}

/// READ statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct ReadStatement {
    pub file_name: String,
    pub record_name: Option<String>,
    pub into: Option<Spanned<Expression>>,
    pub key: Option<Spanned<Expression>>,
    pub at_end: Option<Vec<Spanned<Statement>>>,
    pub not_at_end: Option<Vec<Spanned<Statement>>>,
    pub invalid_key: Option<Vec<Spanned<Statement>>>,
    pub not_invalid_key: Option<Vec<Spanned<Statement>>>,
}

/// WRITE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct WriteStatement {
    pub record_name: String,
    pub from: Option<Spanned<Expression>>,
    pub advancing: Option<AdvancingClause>,
    pub at_eop: Option<Vec<Spanned<Statement>>>,
    pub not_at_eop: Option<Vec<Spanned<Statement>>>,
    pub invalid_key: Option<Vec<Spanned<Statement>>>,
    pub not_invalid_key: Option<Vec<Spanned<Statement>>>,
}

/// ADVANCING clause in WRITE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum AdvancingClause {
    Page,
    Lines(Spanned<Expression>),
    MnemonicName(String),
}

/// REWRITE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct RewriteStatement {
    pub record_name: String,
    pub from: Option<Spanned<Expression>>,
    pub invalid_key: Option<Vec<Spanned<Statement>>>,
    pub not_invalid_key: Option<Vec<Spanned<Statement>>>,
}

/// DELETE statement.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct DeleteStatement {
    pub file_name: String,
    pub record: Option<String>,
    pub invalid_key: Option<Vec<Spanned<Statement>>>,
    pub not_invalid_key: Option<Vec<Spanned<Statement>>>,
}

use crate::span::{Span, Spanned};
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
    Perform(Spanned<PerformStatement>),
    Call(Spanned<CallStatement>),
    GoTo(Spanned<GoToStatement>),
    GoBack(Spanned<GoBackStatement>),
    Exit(Spanned<ExitStatement>),
    Stop(Spanned<StopStatement>),
    Return(Spanned<ReturnStatement>),
    Paragraph(Spanned<ParagraphStatement>),
    Section(Spanned<SectionStatement>),
}

/// DISPLAY statement.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct DisplayStatement {
    pub operands: Vec<Spanned<DisplayOperand>>,
}

/// Operand in DISPLAY statement.
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct MoveStatement {
    pub from: Spanned<MoveSource>,
    pub to: Vec<Spanned<String>>, // TO identifier...
}

/// Source in MOVE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum StopStatement {
    Run, // STOP RUN
    Literal(Option<Literal>), // STOP literal
}

/// RETURN statement (for file processing).
#[derive(Debug, Clone, PartialEq, Eq)]
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
